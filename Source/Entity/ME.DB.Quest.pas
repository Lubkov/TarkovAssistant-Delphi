unit ME.DB.Quest;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections,
  Data.DB, ME.DB.Entity, ME.DB.Marker;

type
  TTrader = (None, Prapor, Therapist, Skier, Peacemaker, Mechanic, Ragman, Jaeger, Fence, Lightkeeper);
  TQuestChangedEvent = procedure(const QuestID: Variant) of object;

  TDBQuest = class(TEntity)
  private
    FMapID: Variant;
    FName: string;
    FTrader: TTrader;
    FMarkers: TList<TDBMarker>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    class function TraderToStr(Value: TTrader): string; static;

    property MapID: Variant read FMapID write FMapID;
    property Name: string read FName write FName;
    property Trader: TTrader read FTrader write FTrader;
    property Markers: TList<TDBMarker> read FMarkers;
  end;

implementation

{ TDBQuest }

constructor TDBQuest.Create;
begin
  inherited;

  FMapID := Null;
  FName := '';
  FMarkers := TObjectList<TDBMarker>.Create;
end;

destructor TDBQuest.Destroy;
begin
  FMarkers.Free;

  inherited;
end;

procedure TDBQuest.Assign(const Source: TEntity);
begin
  inherited;

  FMapID := TDBQuest(Source).MapID;
  FName := TDBQuest(Source).Name;
  FTrader := TDBQuest(Source).Trader;
end;

procedure TDBQuest.Assign(const DataSet: TDataSet);
begin
  inherited;

  FMapID := DataSet.FieldByName('MapID').Value;
  FName := DataSet.FieldByName('Name').AsString;
  FTrader := TTrader(DataSet.FieldByName('Trader').AsInteger);
end;

class function TDBQuest.EntityName: string;
begin
  Result := 'Quest';
end;

class function TDBQuest.FieldList: string;
begin
  Result := 'ID, MapID, Name, Trader';
end;

class function TDBQuest.TraderToStr(Value: TTrader): string;
begin
  case Value of
    TTrader.Prapor:
      Result := 'Прапор';
    TTrader.Therapist:
      Result := 'Терапевт';
    TTrader.Skier:
      Result := 'Лыжник';
    TTrader.Peacemaker:
      Result := 'Миротворец';
    TTrader.Mechanic:
      Result := 'Механик';
    TTrader.Ragman:
      Result := 'Барахольщик';
    TTrader.Jaeger:
      Result := 'Егерь';
    TTrader.Fence:
      Result := 'Скупщик';
    TTrader.Lightkeeper:
      Result := 'Смотритель';
  else
    Result := '';
  end;
end;

end.
