unit ME.DB.QuestItem;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, App.Entity, ME.DB.Entity;

type
  TDBQuestItem = class(TDBEntity)
  private
    FResourceID: Variant;
    FMarkerID: Variant;
    FAmount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property ResourceID: Variant read FResourceID write FResourceID;
    property MarkerID: Variant read FMarkerID write FMarkerID;
    property Amount: Integer read FAmount write FAmount;
  end;

implementation

{ TDBQuestItem }

constructor TDBQuestItem.Create;
begin
  inherited;

  FResourceID := Null;
  FMarkerID := Null;
  FAmount := 1;
end;

destructor TDBQuestItem.Destroy;
begin

  inherited;
end;

procedure TDBQuestItem.Assign(const Source: TEntity);
var
  QuestItem: TDBQuestItem;
begin
  inherited;

  QuestItem := TDBQuestItem(Source);

  FResourceID := QuestItem.ResourceID;
  FMarkerID := QuestItem.MarkerID;
  FAmount := QuestItem.Amount;
end;

procedure TDBQuestItem.Assign(const DataSet: TDataSet);
begin
  FResourceID := DataSet.FieldByName('ResourceID').Value;
  FMarkerID := DataSet.FieldByName('MarkerID').Value;
  FAmount := DataSet.FieldByName('Amount').AsInteger;
end;

class function TDBQuestItem.EntityName: string;
begin
  Result := 'Picture';
end;

class function TDBQuestItem.FieldList: string;
begin
  Result := 'ResourceID, MarkerID, Amount';
end;

end.
