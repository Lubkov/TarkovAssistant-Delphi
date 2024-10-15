unit ME.Profile;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  System.Rtti, System.TypInfo, System.Generics.Defaults, Generics.Collections,
  App.Entity, ME.QuestTracker;

type
  TPMCType = (pmcBear, pmcUsec);

  TProfile = class(TEntity)
  private
    FName: string;
    FKind: TPMCType;
    FQuestTrackers: TList<TQuestTracker>;
    FQuestTrackerComparer: TQuestTrackerComparer;
  protected
    function GetIsNewInstance: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const Source: TJSONValue); overload; override;
    procedure AssignTo(const Dest: TJSONObject); overload; override;
    procedure Clear;

    procedure AddQuestState(const Value: TQuestTracker);
    function GetQuestState(const MarkerID: Variant): TQuestTracker;
    function IsQuestPartFinished(const MarkerID: Variant): Boolean;
    function IsMarkerSelected(const MarkerID: Variant): Boolean;
    procedure SetMarkerSelected(const MarkerID: Variant; const Value: Boolean);

    property Name: string read FName write FName;
    property Kind: TPMCType read FKind write FKind;
    property QuestTrackers: TList<TQuestTracker> read FQuestTrackers;
  end;

implementation

{ TProfile }

constructor TProfile.Create;
begin
  inherited;

  FQuestTrackerComparer := TQuestTrackerComparer.Create;
  FQuestTrackers := TObjectList<TQuestTracker>.Create;
  Clear;
end;

destructor TProfile.Destroy;
begin
  FQuestTrackers.Free;
  FQuestTrackerComparer.Free;

  inherited;
end;

function TProfile.GetIsNewInstance: Boolean;
begin
  Result := FName = '';
end;

procedure TProfile.Assign(const Source: TEntity);
var
  Profile: TProfile;
begin
  inherited;

  Profile := TProfile(Source);

  FName := Profile.Name;
  FKind := Profile.Kind;
end;

procedure TProfile.Assign(const Source: TJSONValue);
begin
  inherited;

  FName := Source.GetValue<string>('name');
  FKind := TRttiEnumerationType.GetValue<TPMCType>(Source.GetValue<string>('kind'));
end;

procedure TProfile.AssignTo(const Dest: TJSONObject);
begin
  inherited;

  Dest.AddPair('name', FName);
  Dest.AddPair('kind', TRttiEnumerationType.GetName<TPMCType>(FKind));
end;

procedure TProfile.Clear;
begin
  FName := '';
  FKind := TPMCType.pmcBear;
  FQuestTrackers.Clear;
end;

procedure TProfile.AddQuestState(const Value: TQuestTracker);
begin
  QuestTrackers.Add(Value);
end;

function TProfile.GetQuestState(const MarkerID: Variant): TQuestTracker;
var
  Item: TQuestTracker;
begin
//   FQuestTrackers.Sort;

  for Item in QuestTrackers do
    if Item.MarkerID = MarkerID then
      Exit(Item);

  Result := nil;
end;

function TProfile.IsQuestPartFinished(const MarkerID: Variant): Boolean;
var
  Item: TQuestTracker;
begin
  if IsNewInstance then
    Exit(False);

  Item := GetQuestState(MarkerID);
  if Item <> nil then
    Result := Item.Finished
  else
    Result := False;
end;

function TProfile.IsMarkerSelected(const MarkerID: Variant): Boolean;
var
  Item: TQuestTracker;
begin
  if IsNewInstance then
    Exit(False);

  Item := GetQuestState(MarkerID);
  if Item <> nil then
    Result := Item.Seleced
  else
    Result := False;
end;

procedure TProfile.SetMarkerSelected(const MarkerID: Variant; const Value: Boolean);
var
  Item: TQuestTracker;
begin
  if IsNewInstance then
    Exit;

  Item := GetQuestState(MarkerID);
  if Item = nil then begin
    Item := TQuestTracker.Create;
    try
      Item.MarkerID := MarkerID;
      Item.Finished := False;
    finally
      AddQuestState(Item);
    end;
  end;

  Item.Seleced := Value;
end;

end.
