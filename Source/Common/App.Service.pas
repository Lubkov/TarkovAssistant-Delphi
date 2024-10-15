unit App.Service;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils, System.Rtti,
  System.TypInfo, System.SysConst, System.JSON, Generics.Collections,
  App.SQLite.Connection, ME.Profile, ME.Options, ME.QuestTracker,
  ME.DB.Marker, ME.DB.Quest;

type
  TAppService = class(TComponent)
  private
    FDBConnection: TSQLiteConnection;
    FOptions: TOptions;
    FProfile: TProfile;

    function GetIsLogIn: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadParams;
    procedure SaveParams;
    procedure LoadProfile;
    procedure SaveProfile;

    procedure LoadDataFromDB;
    procedure ConnectToDB;

    function GetQuestState(const Marker: TDBMarker): TQuestTracker;
    function IsQuestFinished(const Quest: TDBQuest): Boolean;

    function GetQuestSelected(const Quest: TDBQuest): Boolean;
    procedure SetQuestSelected(const Quest: TDBQuest; const Value: Boolean);

    property DBConnection: TSQLiteConnection read FDBConnection;
    property Options: TOptions read FOptions;
    property Profile: TProfile read FProfile;
    property IsLogIn: Boolean read GetIsLogIn;
  end;

var
  AppService: TAppService;

implementation

uses
  Map.Data.Service, ME.Service.Resource, ME.Service.Map, ME.Service.Layer,
  ME.Service.Quest, ME.Service.Marker, ME.Service.QuestItem, ME.Service.Profile,
  ME.Service.QuestTracker;

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

  ProfileService := TProfileService.Create;
  QuestTrackerService := TQuestTrackerService.Create;
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

  inherited;
end;

function TAppService.GetIsLogIn: Boolean;
begin
  Result := FOptions.Profile <> '';
end;

procedure TAppService.LoadParams;
const
  ConfigFile = 'config.json';
var
  Data: TStrings;
  FileName: string;
  Root: TJSONValue;
begin
  FileName := TPath.Combine(TOptions.Path, ConfigFile);
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
  ConnectToDB;
end;

procedure TAppService.SaveParams;
const
  ConfigFile = 'config.json';
var
  Data: TStrings;
  FileName: string;
  JSONObject: TJSONObject;
begin
  FileName := TPath.Combine(TOptions.Path, ConfigFile);
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
begin
  ProfileService.Load(FOptions.Profile, FProfile);
end;

procedure TAppService.SaveProfile;
begin
  ProfileService.Save(Profile);
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
  FileName := TPath.Combine(FOptions.DataPath, Database);
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
      Result.Finished := False;
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

function TAppService.GetQuestSelected(const Quest: TDBQuest): Boolean;
var
  Marker: TDBMarker;
begin
  if not AppService.IsLogIn then
    Exit(False);

  for Marker in Quest.Markers do
    if Profile.IsMarkerSelected(Marker.ID) then
      Exit(True);

  Result := False;
end;

procedure TAppService.SetQuestSelected(const Quest: TDBQuest; const Value: Boolean);
var
  Marker: TDBMarker;
begin
  if not AppService.IsLogIn then
    Exit;

  for Marker in Quest.Markers do
    Profile.SetMarkerSelected(Marker.ID, Value);

  SaveProfile;
end;

end.
