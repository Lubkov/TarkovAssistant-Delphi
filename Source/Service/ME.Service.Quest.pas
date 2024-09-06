unit ME.Service.Quest;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections, Data.DB,
  ME.DB.Entity, ME.DB.DAO, ME.DB.Service, ME.DB.Quest, ME.DB.Marker, ME.DAO.Quest;

type
  TQuestService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure Insert(const Entity: TEntity); override;
    procedure Remove(const ID: Variant); override;

    procedure LoadQuests(const MapID: Variant; const Items: TList<TDBQuest>);
    procedure LoadMarkers(const MapID, QuestID: Variant; const Items: TList<TDBMarker>);
  end;

var
  QuestService: TQuestService;

implementation

uses
  ME.Service.Marker;

{ TQuestService }

function TQuestService.GetDAOClass: TDAOClass;
begin
  Result := TQuestDAO;
end;

procedure TQuestService.Insert(const Entity: TEntity);
var
  Quest: TDBQuest;
//  Marker: TDBMarker;
  TranStarted: Boolean;
begin
  Quest := TDBQuest(Entity);
  TranStarted := InTransaction;

  if not TranStarted then
    StartTransaction;
  try
    DAO.Insert(Quest);

//    for Marker in Quest.Markers do begin
//      Marker.MapID := Quest.MapID;
//      Marker.QuestID := Quest.ID;
//      MarkerService.Insert(Marker);
//    end;

    if not TranStarted then
      CommitTransaction;
  except
    if not TranStarted then
      RollbackTransaction;
    raise;
  end;
end;

procedure TQuestService.Remove(const ID: Variant);
var
  Quest: TDBQuest;
  Marker: TDBMarker;
  TranStarted: Boolean;
begin
  TranStarted := InTransaction;
  if not TranStarted then
    StartTransaction;
  try
    Quest := TDBQuest.Create;
    try
      LoadMarkers(Null, ID, Quest.Markers);
      for Marker in Quest.Markers do
        MarkerService.Remove(Marker);
    finally
      Quest.Free;
    end;

    inherited;

    if not TranStarted then
      CommitTransaction;
  except
    if not TranStarted then
      RollbackTransaction;
    raise;
  end;
end;

procedure TQuestService.LoadQuests(const MapID: Variant; const Items: TList<TDBQuest>);
//var
//  Quest: TDBQuest;
begin
  TQuestDAO(DAO).LoadQuests(MapID, Items);

//  for Quest in Items do
//    LoadMarkers(Quest.MapID, Quest.ID, Quest.Markers);
end;

procedure TQuestService.LoadMarkers(const MapID, QuestID: Variant; const Items: TList<TDBMarker>);
begin
  MarkerService.LoadQuestMarkers(MapID, QuestID, Items);
end;

end.
