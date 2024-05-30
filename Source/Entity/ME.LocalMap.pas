unit ME.LocalMap;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics, Generics.Collections,
  Data.DB, ME.DB.Entity, ME.MapLevel, ME.Marker;

type
  TLocalMap = class(TEntity)
  private
    FName: string;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FPicture: TBitmap;
    FLevels: TList<TMapLevel>;
    FTags: TList<TMarker>;

    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    procedure ClearLevelList;
    procedure ClearTagList;

    property Name: string read FName write FName;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
    property Picture: TBitmap read FPicture write SetPicture;
    property Levels: TList<TMapLevel> read FLevels;
    property Tags: TList<TMarker> read FTags;
  end;

implementation

{ TLocalMap }

constructor TLocalMap.Create;
begin
  inherited;

  FName := '';
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
  FPicture := TBitmap.Create;
  FLevels := TList<TMapLevel>.Create;
  FTags := TList<TMarker>.Create;
end;

destructor TLocalMap.Destroy;
begin
  FPicture.Free;

  ClearLevelList;
  FLevels.Free;

  ClearTagList;
  FTags.Free;

  inherited;
end;

procedure TLocalMap.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

procedure TLocalMap.Assign(const Source: TEntity);
var
  LocalMap: TLocalMap;
begin
  inherited;

  LocalMap := TLocalMap(Source);
  FName := LocalMap.Name;
  FLeft := LocalMap.Left;
  FTop := LocalMap.Top;
  FRight := LocalMap.Right;
  FBottom := LocalMap.Bottom;
  Picture := TLocalMap(Source).Picture;
end;

procedure TLocalMap.Assign(const DataSet: TDataSet);
begin
  inherited;

  FName := DataSet.FieldByName('Name').AsString;
  FLeft := DataSet.FieldByName('Left').AsInteger;
  FTop := DataSet.FieldByName('Top').AsInteger;
  FRight := DataSet.FieldByName('Right').AsInteger;
  FBottom := DataSet.FieldByName('Bottom').AsInteger;

  if DataSet.FindField('Picture') <> nil then
    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TLocalMap.EntityName: string;
begin
  Result := 'LocalMap';
end;

class function TLocalMap.FieldList: string;
begin
  Result := 'ID, Name, "Left", "Top", "Right", "Bottom"';
end;

procedure TLocalMap.ClearLevelList;
var
  i: Integer;
begin
  for i := 0 to FLevels.Count - 1 do
    FLevels[i].Free;

  FLevels.Clear;
end;

procedure TLocalMap.ClearTagList;
var
  i: Integer;
begin
  for i := 0 to FTags.Count - 1 do
    FTags[i].Free;

  FTags.Clear;
end;

end.
