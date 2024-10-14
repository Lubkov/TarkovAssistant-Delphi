unit App.Service;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils, System.Rtti,
  System.TypInfo, System.SysConst, System.JSON, Generics.Collections,
  App.SQLite.Connection, ME.DB.Profile, ME.DB.Options, ME.DB.QuestTracker,
  ME.DB.Marker, ME.DB.Quest;

type
  TAppService = class(TComponent)
  private
    FDBConnection: TSQLiteConnection;
    FOptions: TOptions;
    FProfile: TProfile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadParams;
    procedure SaveParams;
    procedure LoadProfile;
    procedure SaveProfile;
    procedure SaveQuestTracker(const Root: TJSONObject; Items: TList<TQuestTracker>);
    procedure LoadQuestTracker(const Source: TJSONValue; Items: TList<TQuestTracker>);

    procedure LoadDataFromDB;
    procedure ConnectToDB;

    function GetQuestState(const Marker: TDBMarker): TQuestTracker;
    function IsQuestFinished(const Quest: TDBQuest): Boolean;

    property DBConnection: TSQLiteConnection read FDBConnection;
    property Options: TOptions read FOptions;
    property Profile: TProfile read FProfile;
  end;

var
  AppService: TAppService;

implementation

uses
  App.Constants, Map.Data.Service, ME.Service.Resource, ME.Service.Map,
  ME.Service.Layer, ME.Service.Quest, ME.Service.Marker, ME.Service.QuestItem,
  ME.Service.Profile, ME.Service.QuestTracker, ME.Service.Options;

{ TAppService }

constructor TAppService.Create(AOwner: TComponent);
begin
  inherited;

  DataService := TDataService.Create;
  FOptions := TOptions.Create;
  FProfile := TProfile.Create;

  // DB layer
  FDBConnection := TSQLiteConnection.Create(Self);
  ResourceService := TResourceService.Create(DBConnection.Connection);
  MapService := TMapService.Create(DBConnection.Connection);
  LayerService := TLayerService.Create(DBConnection.Connection);
  QuestService := TQuestService.Create(DBConnection.Connection);
  MarkerService := TMarkerService.Create(DBConnection.Connection);
  QuestItemService := TQuestItemService.Create(DBConnection.Connection);
  ProfileService := TProfileService.Create(DBConnection.Connection);
  QuestTrackerService := TQuestTrackerService.Create(DBConnection.Connection);
  OptionsService := TOptionsService.Create(DBConnection.Connection);
end;

destructor TAppService.Destroy;
begin
  DataService.Free;
  FOptions.Free;
  FProfile.Free;

  ResourceService.Free;
  MapService.Free;
  FDBConnection.Free;
  LayerService.Free;
  QuestService.Free;
  MarkerService.Free;
  QuestItemService.Free;
  ProfileService.Free;
  QuestTrackerService.Free;
  OptionsService.Free;

  inherited;
end;

procedure TAppService.LoadParams;
const
  ConfigFile = 'config.json';
var
  Data: TStrings;
  FileName: string;
  Root: TJSONValue;
begin
  AppParams.Load;
  ConnectToDB;
//  OptionsService.Load(FOptions);

  FileName := TPath.Combine(AppParams.Path, ConfigFile);
  Data := TStringList.Create;
  try
    Data.LoadFromFile(FileName, TEncoding.UTF8);

    Root := TJSONObject.ParseJSONValue(Data.Text);
    try
      if not (Root is TJSONObject) then
        Exit;

      FOptions.Assign(Root);
    finally
      Root.Free;
    end;
  finally
    Data.Free;
  end;

  LoadProfile;
end;

procedure TAppService.SaveParams;
const
  ConfigFile = 'config.json';
var
  Data: TStrings;
  FileName: string;
  JSONObject: TJSONObject;
begin
  FileName := TPath.Combine(AppParams.Path, ConfigFile);
  Data := TStringList.Create;
  try
    JSONObject := TJSONObject.Create;
    try
      FOptions.AssignTo(JSONObject);
      Data.Text := JSONObject.ToJSON;
    finally
      JSONObject.Free;
    end;

    Data.SaveToFile(FileName, TEncoding.UTF8);
  finally
    Data.Free;
  end;

  if not SameText(FOptions.Profile, FProfile.Name) then
    LoadProfile;
end;

procedure TAppService.LoadProfile;
const
  ProfileName = 'Profiles\stalker.json';
var
  FileName: string;
  Data: TStrings;
  Root: TJSONValue;
begin
//  if FOptions.Profile <> '' then
//    if ProfileService.GetByName(FOptions.Profile, FProfile) then
//      QuestTrackerService.GetProfileProgress(FProfile.QuestTrackers)
//    else
//      FProfile.Clear
//  else
//    FProfile.Clear;

  FProfile.Clear;
  FileName := TPath.Combine(AppParams.DataPath, ProfileName);
  if (FOptions.Profile = '') or not FileExists(FileName) then
    Exit;

  Data := TStringList.Create;
  try
    Data.LoadFromFile(FileName, TEncoding.UTF8);

    Root := TJSONObject.ParseJSONValue(Data.Text);
    try
      if not (Root is TJSONObject) then
        Exit;

      FProfile.Assign(Root);
      LoadQuestTracker(Root.FindValue('items'), FProfile.QuestTrackers);
    finally
      Root.Free;
    end;
  finally
    Data.Free;
  end;
end;

procedure TAppService.SaveProfile;
const
  ProfileName = 'Profiles\stalker.json';
var
  Data: TStrings;
  FileName: string;
  JSONObject: TJSONObject;
begin
  FileName := TPath.Combine(AppParams.DataPath, ProfileName);

  Data := TStringList.Create;
  try
    JSONObject := TJSONObject.Create;
    try
      FProfile.AssignTo(JSONObject);
      SaveQuestTracker(JSONObject, FProfile.QuestTrackers);

      Data.Text := JSONObject.ToJSON;
    finally
      JSONObject.Free;
    end;

    Data.SaveToFile(FileName, TEncoding.UTF8);
  finally
    Data.Free;
  end;
end;

procedure TAppService.SaveQuestTracker(const Root: TJSONObject; Items: TList<TQuestTracker>);
var
  QuestTracker: TQuestTracker;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for QuestTracker in Items do begin
    JSONObject := TJSONObject.Create;
    try
      QuestTracker.AssignTo(JSONObject);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;

  Root.AddPair('items', JSONItems);
end;

procedure TAppService.LoadQuestTracker(const Source: TJSONValue; Items: TList<TQuestTracker>);
var
  i: Integer;
  List: TJSONArray;
  JSONObject: TJSONValue;
  QuestTracker: TQuestTracker;
begin
  if not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    JSONObject := TJSONObject(List.Items[i]);

    QuestTracker := TQuestTracker.Create;
    try
      QuestTracker.Assign(JSONObject);
    finally
      Items.Add(QuestTracker);
    end;
  end;
end;

procedure TAppService.LoadDataFromDB;
begin
  DataService.Load;
end;

procedure TAppService.ConnectToDB;
const
  Database = 'data.db';
var
  FileName: string;
begin
  FileName := TPath.Combine(AppParams.DataPath, Database);
  FDBConnection.Disconnect;
  FDBConnection.Database := FileName;
  FDBConnection.Connect;
end;

function TAppService.GetQuestState(const Marker: TDBMarker): TQuestTracker;
begin
  Result := Profile.GetQuestState(Marker.ID);
  if Result = nil then begin
    Result := TQuestTracker.Create;
    try
      Result.MarkerID := Marker.ID;
      Result.QuestID := Marker.QuestID;
      Result.ProfileID := Profile.ID;
      Result.Finished := False;

      QuestTrackerService.Save(Result);
    except
      Result.Free;
      raise;
    end;
    Profile.AddQuestState(Result);
  end;
end;

function TAppService.IsQuestFinished(const Quest: TDBQuest): Boolean;
var
  Marker: TDBMarker;
  Finished: Boolean;
begin
  for Marker in Quest.Markers do begin
    Finished := Profile.IsQuestPartFinished(Marker.ID);
    if not Finished then
      Exit(False);
  end;

  Result := True;
end;

end.
