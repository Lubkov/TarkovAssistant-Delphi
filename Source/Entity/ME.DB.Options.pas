unit ME.DB.Options;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TOptions = class(TEntity)
  private
    FProfileID: Variant;
    FDataPath: string;
    FSreenshotPath: string;
    FTrackLocation: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property ProfileID: Variant read FProfileID write FProfileID;
    property DataPath: string read FDataPath write FDataPath;
    property SreenshotPath: string read FSreenshotPath write FSreenshotPath;
    property TrackLocation: Boolean read FTrackLocation write FTrackLocation;
  end;

implementation

{ TOptions }

constructor TOptions.Create;
begin
  inherited;

  FProfileID := Null;
  FDataPath := '';
  FSreenshotPath := '';
  FTrackLocation := True;
end;

destructor TOptions.Destroy;
begin

  inherited;
end;

procedure TOptions.Assign(const Source: TEntity);
begin
  inherited;

  FProfileID := TOptions(Source).ProfileID;
  FDataPath := TOptions(Source).DataPath;
  FSreenshotPath := TOptions(Source).SreenshotPath;
  FTrackLocation := TOptions(Source).TrackLocation;
end;

procedure TOptions.Assign(const DataSet: TDataSet);
begin
  inherited;

  FProfileID := DataSet.FieldByName('ProfileID').Value;
  FDataPath := DataSet.FieldByName('DataPath').AsString;
  FSreenshotPath := DataSet.FieldByName('SreenshotPath').AsString;
  FTrackLocation := DataSet.FieldByName('TrackLocation').AsBoolean;
end;

class function TOptions.EntityName: string;
begin
  Result := 'Options';
end;

class function TOptions.FieldList: string;
begin
  Result := 'ID, ProfileID, DataPath, SreenshotPath, TrackLocation';
end;

end.
