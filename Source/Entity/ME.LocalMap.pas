unit ME.LocalMap;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics,
  Data.DB, ME.DB.Entity, ME.Point, ME.MapLevel, Generics.Collections;

type
  TLocalMap = class(TEntity)
  private
    FName: string;
    FLeft: TPoint;
    FRight: TPoint;
    FPicture: TBitmap;
    FLevels: TList<TMapLevel>;

    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    procedure ClearLevelList;

    property Name: string read FName write FName;
    property Left: TPoint read FLeft write FLeft;
    property Right: TPoint read FRight write FRight;
    property Picture: TBitmap read FPicture write SetPicture;
    property Levels: TList<TMapLevel> read FLevels;
  end;

implementation

{ TLocalMap }

constructor TLocalMap.Create;
begin
  inherited;

  FName := '';
  FLeft := TPoint.Create;
  FRight := TPoint.Create;
  FPicture := TBitmap.Create;
  FLevels := TList<TMapLevel>.Create;
end;

destructor TLocalMap.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  FPicture.Free;

  ClearLevelList;
  FLevels.Free;

  inherited;
end;

procedure TLocalMap.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

procedure TLocalMap.Assign(const Source: TEntity);
begin
  inherited;

  FName := TLocalMap(Source).Name;
  FLeft.Assign(TLocalMap(Source).Left);
  FRight.Assign(TLocalMap(Source).Right);
  Picture := TLocalMap(Source).Picture;
end;

procedure TLocalMap.Assign(const DataSet: TDataSet);
begin
  inherited;

  FName := DataSet.FieldByName('Name').AsString;
  FLeft.ID := DataSet.FieldByName('LeftID').Value;
  FLeft.X := DataSet.FieldByName('X1').AsInteger;
  FLeft.Y := DataSet.FieldByName('Y1').AsInteger;
  FRight.ID := DataSet.FieldByName('RightID').Value;
  FRight.X := DataSet.FieldByName('X2').AsInteger;
  FRight.Y := DataSet.FieldByName('Y2').AsInteger;

  if DataSet.FindField('Picture') <> nil then
    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TLocalMap.EntityName: string;
begin
  Result := 'LocalMap';
end;

class function TLocalMap.FieldList: string;
begin
  Result := 'ID, Name, LeftID, RightID';
end;

procedure TLocalMap.ClearLevelList;
var
  i: Integer;
begin
  for i := 0 to FLevels.Count - 1 do
    FLevels[i].Free;

  FLevels.Clear;
end;

end.
