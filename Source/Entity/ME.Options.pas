unit ME.Options;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  Data.DB, App.Entity;

type
  TOptions = class(TEntity)
  private
    class var
      FAppPath: string;
  private
    FDataPath: string;
    FSreenshotPath: string;
    FTrackLocation: Boolean;
    FProfile: string;
  protected
    function GetIsNewInstance: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const Source: TJSONValue); overload; override;
    procedure AssignTo(const Dest: TJSONObject); override;

    function IsEqual(const Source: TOptions): Boolean;

    class property Path: string read FAppPath;
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

function TOptions.GetIsNewInstance: Boolean;
begin
  Result := FDataPath = '';
end;

procedure TOptions.Assign(const Source: TEntity);
var
  Options: TOptions;
begin
  inherited;

  Options := TOptions(Source);

  FDataPath := Options.DataPath;
  FSreenshotPath := Options.SreenshotPath;
  FTrackLocation := Options.TrackLocation;
  FProfile := Options.Profile;
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

initialization
  TOptions.FAppPath := IncludeTrailingPathDelimiter(System.SysUtils.GetCurrentDir);

end.
