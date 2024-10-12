unit ME.DB.Profile;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.SysConst, System.JSON,
  System.Rtti, System.TypInfo, Generics.Collections, Data.DB,
  ME.DB.Entity, ME.DB.QuestTracker;

type
  TPMCType = (pmcBear, pmcUsec);

  TProfile = class(TEntity)
  private
    FName: string;
    FKind: TPMCType;
    FQuestTrackers: TList<TQuestTracker>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;
    procedure Assign(const Source: TJSONValue); overload; override;
    procedure AssignTo(const Dest: TJSONObject); overload; override;
    procedure Clear;

    procedure AddQuestState(const Value: TQuestTracker);
    function GetQuestState(const MarkerID: Variant): TQuestTracker;
    function IsQuestPartFinished(const MarkerID: Variant): Boolean;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property Name: string read FName write FName;
    property Kind: TPMCType read FKind write FKind;
    property QuestTrackers: TList<TQuestTracker> read FQuestTrackers;
  end;

implementation

{ TProfile }

constructor TProfile.Create;
begin
  inherited;

  FQuestTrackers := TObjectList<TQuestTracker>.Create;
  Clear;
end;

destructor TProfile.Destroy;
begin
  FQuestTrackers.Free;

  inherited;
end;

procedure TProfile.Assign(const Source: TEntity);
begin
  inherited;

  FName := TProfile(Source).Name;
  FKind := TProfile(Source).Kind;
end;

procedure TProfile.Assign(const DataSet: TDataSet);
begin
  inherited;

  FName := DataSet.FieldByName('Name').AsString;
  FKind := TPMCType(DataSet.FieldByName('Kind').AsInteger);
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
  ID := Null;
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
  for Item in QuestTrackers do
    if Item.MarkerID = MarkerID then
      Exit(Item);

  Result := nil;
end;

function TProfile.IsQuestPartFinished(const MarkerID: Variant): Boolean;
var
  Item: TQuestTracker;
begin
  Item := GetQuestState(MarkerID);
  if Item <> nil then
    Result := Item.Finished
  else
    Result := False;
end;

class function TProfile.EntityName: string;
begin
  Result := 'Profile';
end;

class function TProfile.FieldList: string;
begin
  Result := 'ID, Name, Kind';
end;

end.
