unit ME.DB.Layer;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics,
  Data.DB, ME.DB.Entity;

const
  MainLayerIndex = 0;

type
  TDBLayer = class(TEntity)
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

{ TDBLayer }

constructor TDBLayer.Create;
begin
  inherited;

  FMapID := Null;
  FLevel := -1;
  FName := '';
  FPicture := TBitmap.Create;
end;

destructor TDBLayer.Destroy;
begin
  FreeAndNil(FPicture);

  inherited;
end;

function TDBLayer.GetIsMainLevel: Boolean;
begin
  Result := Level = MainLayerIndex;
end;

procedure TDBLayer.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

procedure TDBLayer.Assign(const Source: TEntity);
begin
  inherited;

  FMapID := TDBLayer(Source).MapID;
  FLevel := TDBLayer(Source).Level;
  FName := TDBLayer(Source).Name;
  Picture := TDBLayer(Source).Picture;
end;

procedure TDBLayer.Assign(const DataSet: TDataSet);
begin
  inherited;

  FMapID := DataSet.FieldByName('MapID').Value;
  FLevel := DataSet.FieldByName('Level').AsInteger;
  FName := DataSet.FieldByName('Name').AsString;

  if DataSet.FindField('Picture') <> nil then
    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TDBLayer.EntityName: string;
begin
  Result := 'Layer';
end;

class function TDBLayer.FieldList: string;
begin
  Result := 'ID, MapID, Level, "Name"';
end;

end.
