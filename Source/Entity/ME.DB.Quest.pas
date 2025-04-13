unit ME.DB.Quest;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections,
  Data.DB, App.Entity, ME.DB.Entity, ME.DB.Marker, ME.Trader;

type
  TQuestChangedEvent = procedure(const QuestID, MapID: Variant) of object;

  TDBQuest = class(TDBEntity)
  private
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

    property Name: string read FName write FName;
    property Trader: TTrader read FTrader write FTrader;
    property Markers: TList<TDBMarker> read FMarkers;
  end;

implementation

{ TDBQuest }

constructor TDBQuest.Create;
begin
  inherited;

  FName := '';
  FMarkers := TObjectList<TDBMarker>.Create;
end;

destructor TDBQuest.Destroy;
begin
  FMarkers.Free;

  inherited;
end;

procedure TDBQuest.Assign(const Source: TEntity);
var
  Quest: TDBQuest;
begin
  inherited;

  Quest := TDBQuest(Source);

  FName := Quest.Name;
  FTrader := Quest.Trader;
end;

procedure TDBQuest.Assign(const DataSet: TDataSet);
begin
  inherited;

  FName := DataSet.FieldByName('Name').AsString;
  FTrader := TTrader(DataSet.FieldByName('Trader').AsInteger);
end;

class function TDBQuest.EntityName: string;
begin
  Result := 'Quest';
end;

class function TDBQuest.FieldList: string;
begin
  Result := 'ID, Name, Trader';
end;

end.
