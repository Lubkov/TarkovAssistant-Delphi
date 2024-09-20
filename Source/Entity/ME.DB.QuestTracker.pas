unit ME.DB.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TQuestStatus = (qsNone, qsNew, qsStarted, qsFinished);

  TQuestTracker = class(TEntity)
  private
    FQuestID: Variant;
    FMarkerID: Variant;
    FStatus: TQuestStatus;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property QuestID: Variant read FQuestID write FQuestID;
    property MarkerID: Variant read FMarkerID write FMarkerID;
    property Status: TQuestStatus read FStatus write FStatus;
  end;

implementation

{ TQuestTracker }

constructor TQuestTracker.Create;
begin
  inherited;

  FQuestID := Null;
  FMarkerID := Null;
  FStatus := TQuestStatus.qsNone;
end;

destructor TQuestTracker.Destroy;
begin

  inherited;
end;

procedure TQuestTracker.Assign(const Source: TEntity);
begin

end;

procedure TQuestTracker.Assign(const DataSet: TDataSet);
begin

end;

class function TQuestTracker.EntityName: string;
begin

end;

class function TQuestTracker.FieldList: string;
begin

end;

function TQuestTracker.write: Variant;
begin

end;

end.
