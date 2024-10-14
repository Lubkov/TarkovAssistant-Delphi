unit ME.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  System.Rtti, System.TypInfo, App.Entity;

type
  TQuestTracker = class(TEntity)
  private
    FMarkerID: Variant;
    FFinished: Boolean;
  protected
    function GetIsNewInstance: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const Source: TJSONValue); overload; override;
    procedure AssignTo(const Dest: TJSONObject); overload; override;

    property MarkerID: Variant read FMarkerID write FMarkerID;
    property Finished: Boolean read FFinished write FFinished;
  end;

implementation

uses
  ME.DB.Utils;

{ TQuestTracker }

constructor TQuestTracker.Create;
begin
  inherited;

  FMarkerID := Null;
  Finished := False;
end;

destructor TQuestTracker.Destroy;
begin

  inherited;
end;

function TQuestTracker.GetIsNewInstance: Boolean;
begin
  Result := IsNullID(FMarkerID);
end;

procedure TQuestTracker.Assign(const Source: TEntity);
var
  QuestTracker: TQuestTracker;
begin
  QuestTracker := TQuestTracker(Source);

  FMarkerID := QuestTracker.MarkerID;
  FFinished := QuestTracker.Finished;
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

end.
