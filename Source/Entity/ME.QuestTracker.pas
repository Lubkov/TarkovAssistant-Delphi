unit ME.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  System.Rtti, System.TypInfo, System.Generics.Defaults, App.Entity;

type
  TQuestTracker = class(TEntity)
  private
    FMarkerID: Integer;
    FFinished: Boolean;
    FSeleced: Boolean;
  protected
    function GetIsNewInstance: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const Source: TJSONValue); overload; override;
    procedure AssignTo(const Dest: TJSONObject); overload; override;

    property MarkerID: Integer read FMarkerID write FMarkerID;
    property Finished: Boolean read FFinished write FFinished;
    property Seleced: Boolean read FSeleced write FSeleced;
  end;

  TQuestTrackerComparer = class(TComparer<TQuestTracker>)
  public
    function Compare(const Left, Right: TQuestTracker): Integer; override;
  end;

implementation

{ TQuestTracker }

constructor TQuestTracker.Create;
begin
  inherited;

  FMarkerID := -1;
  FFinished := False;
  FSeleced := False;
end;

destructor TQuestTracker.Destroy;
begin

  inherited;
end;

function TQuestTracker.GetIsNewInstance: Boolean;
begin
  Result := FMarkerID <= 0;
end;

procedure TQuestTracker.Assign(const Source: TEntity);
var
  QuestTracker: TQuestTracker;
begin
  QuestTracker := TQuestTracker(Source);

  FMarkerID := QuestTracker.MarkerID;
  FFinished := QuestTracker.Finished;
  FSeleced := QuestTracker.Seleced;
end;

procedure TQuestTracker.Assign(const Source: TJSONValue);
begin
  FMarkerID := Source.GetValue<Integer>('id');
  FFinished := Source.GetValue<Boolean>('finished');
  FSeleced := Source.GetValue<Boolean>('seleced');
end;

procedure TQuestTracker.AssignTo(const Dest: TJSONObject);
begin
  Dest.AddPair('id', FMarkerID);
  Dest.AddPair('finished', FFinished);
  Dest.AddPair('seleced', FSeleced);
end;

{ TQuestTrackerComparer }

function TQuestTrackerComparer.Compare(const Left, Right: TQuestTracker): Integer;
begin
  if Left.MarkerID = Right.MarkerID then
    Result := 0
  else
  if Left.MarkerID < Right.MarkerID then
    Result := -1
  else
    Result := 1;
end;

end.
