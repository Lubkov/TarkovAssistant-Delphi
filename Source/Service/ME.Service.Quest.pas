unit ME.Service.Quest;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, Data.DB, ME.DB.Entity,
  ME.DB.DAO, ME.DB.Service, ME.DB.Quest, ME.DB.Point, ME.DAO.Quest;

type
  TQuestService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure LoadQuests(const MapID: Variant; const Items: TList<TQuest>);
    procedure LoadParts(const QuestID: Variant; const Items: TList<TPoint>);
  end;

var
  QuestService: TQuestService;

implementation

uses
  ME.Service.Point;

{ TQuestService }

function TQuestService.GetDAOClass: TDAOClass;
begin
  Result := TQuestDAO;
end;

procedure TQuestService.LoadQuests(const MapID: Variant; const Items: TList<TQuest>);
begin
  TQuestDAO(DAO).LoadQuests(MapID, Items);
end;

procedure TQuestService.LoadParts(const QuestID: Variant; const Items: TList<TPoint>);
begin
  PointService.LoadQuestParts(QuestID, Items);
end;

end.
