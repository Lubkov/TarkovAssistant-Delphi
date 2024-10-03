unit ME.DB.Options;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  Data.DB, ME.DB.Entity;

type
  TOptions = class(TEntity)
  private
    FDataPath: string;
    FSreenshotPath: string;
    FTrackLocation: Boolean;
    FProfile: string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;
    procedure Assign(const Source: TJSONValue); overload;
    procedure AssignTo(const Dest: TJSONObject);

    function IsEqual(const Source: TOptions): Boolean;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property DataPath: string read FDataPath write FDataPath;
    property SreenshotPath: string read FSreenshotPath write FSreenshotPath;
    property TrackLocation: Boolean read FTrackLocation write FTrackLocation;
    property Profile: string read FProfile write FProfile;
  end;

implementation

{ TOptions }

constructor TOptions.Create;
begin
  inherited;

  FDataPath := '';
  FSreenshotPath := '';
  FTrackLocation := True;
  FProfile := '';
end;

destructor TOptions.Destroy;
begin

  inherited;
end;

procedure TOptions.Assign(const Source: TEntity);
begin
  inherited;

  FDataPath := TOptions(Source).DataPath;
  FSreenshotPath := TOptions(Source).SreenshotPath;
  FTrackLocation := TOptions(Source).TrackLocation;
  FProfile := TOptions(Source).Profile;
end;

procedure TOptions.Assign(const DataSet: TDataSet);
begin
  inherited;

  FDataPath := DataSet.FieldByName('DataPath').AsString;
  FSreenshotPath := DataSet.FieldByName('SreenshotPath').AsString;
  FTrackLocation := DataSet.FieldByName('TrackLocation').AsInteger = 1;
  FProfile := DataSet.FieldByName('Profile').Value;
end;

procedure TOptions.Assign(const Source: TJSONValue);
begin
  FDataPath := Source.GetValue<string>('data_path');
  FSreenshotPath := Source.GetValue<string>('sreenshot_path');
  FTrackLocation := Source.GetValue<Boolean>('track_location');
  FProfile := Source.GetValue<string>('profile');
end;

procedure TOptions.AssignTo(const Dest: TJSONObject);
begin
  Dest.AddPair('data_path', DataPath);
  Dest.AddPair('sreenshot_path', SreenshotPath);
  Dest.AddPair('track_location', TrackLocation);
  Dest.AddPair('profile', Profile);
end;

function TOptions.IsEqual(const Source: TOptions): Boolean;
begin
  Result := SameText(DataPath, Source.DataPath) and
            SameText(SreenshotPath, Source.SreenshotPath) and
            (TrackLocation = Source.TrackLocation) and
            SameText(Profile, Source.Profile);
end;

class function TOptions.EntityName: string;
begin
  Result := 'Options';
end;

class function TOptions.FieldList: string;
begin
  Result := 'ID, DataPath, SreenshotPath, TrackLocation, Profile';
end;

end.
