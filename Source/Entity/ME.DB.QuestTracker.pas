unit ME.DB.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TQuestStatus = (qsNone, qsNew, qsStarted, qsFinished);

  TQuestTracker = class(TEntity)
  private
    FProfileID: Variant;
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

    property ProfileID: Variant read FProfileID write FProfileID;
    property QuestID: Variant read FQuestID write FQuestID;
    property MarkerID: Variant read FMarkerID write FMarkerID;
    property Status: TQuestStatus read FStatus write FStatus;
  end;

implementation

{ TQuestTracker }

constructor TQuestTracker.Create;
begin
  inherited;

  FProfileID := Null;
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
  inherited;

  FProfileID := TQuestTracker(Source).ProfileID;
  FQuestID := TQuestTracker(Source).QuestID;
  FMarkerID := TQuestTracker(Source).MarkerID;
  FStatus := TQuestTracker(Source).Status;
end;

procedure TQuestTracker.Assign(const DataSet: TDataSet);
begin
  inherited;

  ProfileID := DataSet.FieldByName('ProfileID').Value;
  FQuestID := DataSet.FieldByName('QuestID').Value;
  FMarkerID := DataSet.FieldByName('MarkerID').Value;
  FStatus := TQuestStatus(DataSet.FieldByName('Status').AsInteger);
end;

class function TQuestTracker.EntityName: string;
begin
  Result := 'QuestTracker';
end;

class function TQuestTracker.FieldList: string;
begin
 Result := 'ID, ProfileID, QuestID, MarkerID, Status';
end;

end.
