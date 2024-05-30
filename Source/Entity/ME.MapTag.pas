unit ME.MapTag;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TTagKind = (tkPMCExtraction, tkScavExtraction, tkCoopExtraction);

  TMapTag = class(TEntity)
  private
    FMapID: Variant;
    FName: string;
    FKind: TTagKind;
    FLeft: Integer;
    FTop: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;
    class function KindToStr(Value: TTagKind): string;

    property MapID: Variant read FMapID write FMapID;
    property Name: string read FName write FName;
    property Kind: TTagKind read FKind write FKind;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
  end;

implementation

{ TMapTag }

constructor TMapTag.Create;
begin
  inherited;

  FMapID := Null;
  FName := '';
  FKind := tkPMCExtraction;
  FLeft := 0;
  FTop := 0;
end;

destructor TMapTag.Destroy;
begin

  inherited;
end;

procedure TMapTag.Assign(const Source: TEntity);
begin
  inherited;

  MapID := TMapTag(Source).MapID;
  Name := TMapTag(Source).Name;
  Kind := TMapTag(Source).Kind;
  Left := TMapTag(Source).Left;
  Top := TMapTag(Source).Top;
end;

procedure TMapTag.Assign(const DataSet: TDataSet);
begin
  inherited;

  MapID := DataSet.FieldByName('MapID').Value;
  Name := DataSet.FieldByName('Name').AsString;
  Kind := TTagKind(DataSet.FieldByName('Kind').AsInteger);
  Left := DataSet.FieldByName('Left').AsInteger;
  Top := DataSet.FieldByName('Top').AsInteger;
end;

class function TMapTag.EntityName: string;
begin
  Result := 'MapTag';
end;

class function TMapTag.FieldList: string;
begin
  Result := 'ID, "MapID", "Name", "Kind", "Left", "Top"';
end;

class function TMapTag.KindToStr(Value: TTagKind): string;
begin
  case Value of
    tkPMCExtraction:
      Result := 'Выход ЧВК';
    tkScavExtraction:
      Result := 'Выход дикого';
    tkCoopExtraction:
      Result := 'Совм. выход';
  else
    Result := '';
  end;
end;

end.
