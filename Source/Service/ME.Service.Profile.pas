unit ME.Service.Profile;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Rtti, System.TypInfo,
  System.SysConst, System.JSON, ME.Profile;

type
  TProfileService = class(TObject)
  private
    function GetProfileFolder: string;
    function GetProfileFileName(const ProfileName: string): string;
  public
    procedure GetAll(const Names: TStrings);
    function Load(const ProfileName: string; const Profile: TProfile): Boolean;
    procedure Save(const Profile: TProfile);
    procedure Remove(const ProfileName: string); overload;
    procedure Remove(const Profile: TProfile); overload;
  end;

var
  ProfileService: TProfileService;

implementation

uses
  App.Service, ME.Service.QuestTracker;

{ TProfileService }

function TProfileService.GetProfileFolder: string;
const
  FolderName = 'Profiles';
begin
 Result := TPath.Combine(AppService.Options.DataPath, FolderName);
end;

function TProfileService.GetProfileFileName(const ProfileName: string): string;
begin
  Result := TPath.Combine(GetProfileFolder, ProfileName + '.json');
end;

procedure TProfileService.GetAll(const Names: TStrings);
var
  FileName: string;
begin
  Names.BeginUpdate;
  try
    Names.Clear;

    for FileName in TDirectory.GetFiles(GetProfileFolder, '*.json', TSearchOption.soTopDirectoryOnly) do
      Names.Add(TPath.GetFileNameWithoutExtension(FileName));
  finally
    Names.EndUpdate;
  end;
end;

function TProfileService.Load(const ProfileName: string; const Profile: TProfile): Boolean;
var
  FileName: string;
  Data: TStrings;
  Root: TJSONValue;
begin
  Profile.Clear;

  FileName := GetProfileFileName(ProfileName);
  if (ProfileName = '') or not FileExists(FileName) then
    Exit(False);

  Data := TStringList.Create;
  try
    Data.LoadFromFile(FileName, TEncoding.UTF8);

    Root := TJSONObject.ParseJSONValue(Data.Text);
    try
      if not (Root is TJSONObject) then
        Exit(False);

      Profile.Assign(Root);
      QuestTrackerService.Load(Root.FindValue('markers'), Profile.QuestTrackers);
      Result := True;
    finally
      Root.Free;
    end;
  finally
    Data.Free;
  end;
end;

procedure TProfileService.Save(const Profile: TProfile);
var
  Data: TStrings;
  FileName: string;
  JSONObject: TJSONObject;
begin
  FileName := GetProfileFileName(Profile.Name);

  Data := TStringList.Create;
  try
    JSONObject := TJSONObject.Create;
    try
      Profile.AssignTo(JSONObject);
      QuestTrackerService.Save(JSONObject, Profile.QuestTrackers);

      Data.Text := JSONObject.ToJSON;
    finally
      JSONObject.Free;
    end;

    Data.SaveToFile(FileName, TEncoding.UTF8);
  finally
    Data.Free;
  end;
end;

procedure TProfileService.Remove(const ProfileName: string);
var
  FileName: string;
begin
  FileName := GetProfileFileName(ProfileName);
  if (ProfileName = '') or not FileExists(FileName) then
    Exit;

  TFile.Delete(FileName);
end;

procedure TProfileService.Remove(const Profile: TProfile);
begin
  Remove(Profile.Name);
end;

end.
