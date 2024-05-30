unit ME.MapLevel;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics,
  Data.DB, ME.DB.Entity;

const
  MainMapLevelIndex = 0;

type
  TMapLevel = class(TEntity)
  private
    FMapID: Variant;
    FLevel: Integer;
    FName: string;
    FPicture: TBitmap;

    function GetIsMainLevel: Boolean;
    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property MapID: Variant read FMapID write FMapID;
    property Level: Integer read FLevel write FLevel;
    property Name: string read FName write FName;
    property Picture: TBitmap read FPicture write SetPicture;
    property IsMainLevel: Boolean read GetIsMainLevel;
  end;

implementation

{ TMapLevel }

constructor TMapLevel.Create;
begin
  inherited;

  FMapID := Null;
  FLevel := -1;
  FName := '';
  FPicture := TBitmap.Create;
end;

destructor TMapLevel.Destroy;
begin
  FreeAndNil(FPicture);

  inherited;
end;

function TMapLevel.GetIsMainLevel: Boolean;
begin
  Result := Level = MainMapLevelIndex;
end;

procedure TMapLevel.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

procedure TMapLevel.Assign(const Source: TEntity);
begin
  inherited;

  FMapID := TMapLevel(Source).MapID;
  FLevel := TMapLevel(Source).Level;
  FName := TMapLevel(Source).Name;
  Picture := TMapLevel(Source).Picture;
end;

procedure TMapLevel.Assign(const DataSet: TDataSet);
begin
  inherited;

  FMapID := DataSet.FieldByName('MapID').Value;
  FLevel := DataSet.FieldByName('Level').AsInteger;
  FName := DataSet.FieldByName('Name').AsString;

  if DataSet.FindField('Picture') <> nil then
    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TMapLevel.EntityName: string;
begin
  Result := 'Map';
end;

class function TMapLevel.FieldList: string;
begin
  Result := 'ID, MapID, Level, Name';
end;

end.
