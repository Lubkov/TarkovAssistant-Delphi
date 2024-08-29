unit ME.DB.Marker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections,
  Data.DB, ME.DB.Entity, ME.DB.Resource, ME.DB.QuestItem;

type
  TMarkerKind = (PMCExtraction, ScavExtraction, CoopExtraction, Quest);
  TMarkerKindSet = set of TMarkerKind;

  TDBMarker = class(TEntity)
  private
    FMapID: Variant;
    FQuestID: Variant;
    FCaption: string;
    FKind: TMarkerKind;
    FLeft: Integer;
    FTop: Integer;
    FImages: TObjectList<TDBResource>;
    FItems: TObjectList<TDBResource>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;
    class function KindToStr(Value: TMarkerKind): string;

    property MapID: Variant read FMapID write FMapID;
    property QuestID: Variant read FQuestID write FQuestID;
    property Caption: string read FCaption write FCaption;
    property Kind: TMarkerKind read FKind write FKind;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Images: TObjectList<TDBResource> read FImages;
    property Items: TObjectList<TDBResource> read FItems;
  end;

implementation

{ TDBMarker }

constructor TDBMarker.Create;
begin
  inherited;

  FMapID := Null;
  FQuestID := Null;
  FCaption := '';
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
begin
  inherited;

  MapID := TDBMarker(Source).MapID;
  QuestID := TDBMarker(Source).QuestID;
  Caption := TDBMarker(Source).Caption;
  Kind := TDBMarker(Source).Kind;
  Left := TDBMarker(Source).Left;
  Top := TDBMarker(Source).Top;
end;

procedure TDBMarker.Assign(const DataSet: TDataSet);
begin
  inherited;

  MapID := DataSet.FieldByName('MapID').Value;
  QuestID := DataSet.FieldByName('QuestID').Value;
  Caption := DataSet.FieldByName('Caption').AsString;
  Kind := TMarkerKind(DataSet.FieldByName('Kind').AsInteger);
  Left := DataSet.FieldByName('Left').AsInteger;
  Top := DataSet.FieldByName('Top').AsInteger;
end;

class function TDBMarker.EntityName: string;
begin
  Result := 'Marker';
end;

class function TDBMarker.FieldList: string;
begin
  Result := 'ID, "MapID", "QuestID", "Caption", "Kind", "Left", "Top"';
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
  else
    Result := '';
  end;
end;

end.
