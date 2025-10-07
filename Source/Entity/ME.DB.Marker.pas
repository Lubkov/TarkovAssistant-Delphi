unit ME.DB.Marker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections,
  Data.DB, App.Entity, ME.DB.Entity, ME.DB.Resource, ME.DB.QuestItem;

type
  TMarkerKind = (PMCExtraction, ScavExtraction, CoopExtraction, Quest, TransitExtraction);
  TMarkerKindSet = set of TMarkerKind;

  TDBMarker = class(TDBEntity)
  private
    FMapID: Variant;
    FQuestID: Variant;
    FDescription: string;
    FKind: TMarkerKind;
    FLeft: Integer;
    FTop: Integer;
    FImages: TList<TDBResource>;
    FItems: TList<TDBResource>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;
    class function KindToStr(Value: TMarkerKind): string;
    class function KindToInt(const Value: TMarkerKind): Integer;
    class function IntToKind(const Value: Integer): TMarkerKind;

    property MapID: Variant read FMapID write FMapID;
    property QuestID: Variant read FQuestID write FQuestID;
    property Description: string read FDescription write FDescription;
    property Kind: TMarkerKind read FKind write FKind;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Images: TList<TDBResource> read FImages;
    property Items: TList<TDBResource> read FItems;
  end;

implementation

{ TDBMarker }

constructor TDBMarker.Create;
begin
  inherited;

  FMapID := Null;
  FQuestID := Null;
  FDescription := '';
  FKind := TMarkerKind.PMCExtraction;
  FLeft := 0;
  FTop := 0;
  FImages := TObjectList<TDBResource>.Create;
  FItems := TObjectList<TDBResource>.Create;
end;

destructor TDBMarker.Destroy;
begin
  FImages.Free;
  FItems.Free;

  inherited;
end;

procedure TDBMarker.Assign(const Source: TEntity);
var
  Marker: TDBMarker;
begin
  inherited;

  Marker := TDBMarker(Source);

  MapID := Marker.MapID;
  QuestID := Marker.QuestID;
  Description := Marker.Description;
  Kind := Marker.Kind;
  Left := Marker.Left;
  Top := Marker.Top;
end;

procedure TDBMarker.Assign(const DataSet: TDataSet);
begin
  inherited;

  MapID := DataSet.FieldByName('MapID').Value;
  QuestID := DataSet.FieldByName('QuestID').Value;
  Description := DataSet.FieldByName('Description').AsString;
  Kind := IntToKind(DataSet.FieldByName('Kind').AsInteger);
  Left := DataSet.FieldByName('Left').AsInteger;
  Top := DataSet.FieldByName('Top').AsInteger;
end;

class function TDBMarker.EntityName: string;
begin
  Result := 'Marker';
end;

class function TDBMarker.FieldList: string;
begin
  Result := 'ID, "MapID", "QuestID", "Description", "Kind", "Left", "Top"';
end;

class function TDBMarker.KindToStr(Value: TMarkerKind): string;
begin
  case Value of
    TMarkerKind.PMCExtraction:
      Result := 'Выход ЧВК';
    TMarkerKind.ScavExtraction:
      Result := 'Выход дикого';
    TMarkerKind.CoopExtraction:
      Result := 'Совм. выход';
    TMarkerKind.TransitExtraction:
      Result := 'Переход';
  else
    Result := '';
  end;
end;

class function TDBMarker.KindToInt(const Value: TMarkerKind): Integer;
begin
  Result := Ord(Value) + 1;
end;

class function TDBMarker.IntToKind(const Value: Integer): TMarkerKind;
begin
  Result := TMarkerKind(Value - 1);
end;

end.
