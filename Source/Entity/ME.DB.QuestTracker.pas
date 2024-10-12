unit ME.DB.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  System.Rtti, System.TypInfo, Data.DB, ME.DB.Entity;

type
  TQuestTracker = class(TEntity)
  private
    FProfileID: Variant;
    FQuestID: Variant;
    FMarkerID: Variant;
    FFinished: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;
    procedure Assign(const Source: TJSONValue); overload; override;
    procedure AssignTo(const Dest: TJSONObject); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property ProfileID: Variant read FProfileID write FProfileID;
    property QuestID: Variant read FQuestID write FQuestID;
    property MarkerID: Variant read FMarkerID write FMarkerID;
    property Finished: Boolean read FFinished write FFinished;
  end;

implementation

{ TQuestTracker }

constructor TQuestTracker.Create;
begin
  inherited;

  FProfileID := Null;
  FQuestID := Null;
  FMarkerID := Null;
  Finished := False;
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
  FFinished := TQuestTracker(Source).Finished;
end;

procedure TQuestTracker.Assign(const DataSet: TDataSet);
begin
  inherited;

  ProfileID := DataSet.FieldByName('ProfileID').Value;
  FQuestID := DataSet.FieldByName('QuestID').Value;
  FMarkerID := DataSet.FieldByName('MarkerID').Value;
  FFinished := DataSet.FieldByName('Finished').AsInteger = 1;
end;

procedure TQuestTracker.Assign(const Source: TJSONValue);
begin
  FMarkerID := Source.GetValue<Integer>('id');
  FFinished := Source.GetValue<Boolean>('finished');
end;

procedure TQuestTracker.AssignTo(const Dest: TJSONObject);
begin
  Dest.AddPair('id', Integer(FMarkerID));
  Dest.AddPair('finished', FFinished);
end;

class function TQuestTracker.EntityName: string;
begin
  Result := 'QuestTracker';
end;

class function TQuestTracker.FieldList: string;
begin
 Result := 'ID, ProfileID, QuestID, MarkerID, Finished';
end;

end.
