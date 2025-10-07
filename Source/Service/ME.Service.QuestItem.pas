unit ME.Service.QuestItem;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.QuestItem, ME.DAO.QuestItem;

type
  TQuestItemService = class(TServiceCommon)
  private
    function GetQuestItemDAO: TQuestItemDAO;
  protected
    function GetDAOClass: TDAOClass; override;
  public
    function GetAt(ResourceID, MarkerID: Variant; const Entity: TDBEntity): Boolean; overload;
    procedure Remove(const ResourceID, MarkerID: Variant); overload;
    procedure Save(const ResourceID, MarkerID: Variant; Amount: Integer); overload;
    procedure Save(const ResourceID, MarkerID: Variant); overload;

    property QuestItemDAO: TQuestItemDAO read GetQuestItemDAO;
  end;

var
  QuestItemService: TQuestItemService;

implementation

{ TQuestItemService }

function TQuestItemService.GetQuestItemDAO: TQuestItemDAO;
begin
  Result := TQuestItemDAO(DAO);
end;

function TQuestItemService.GetDAOClass: TDAOClass;
begin
  Result := TQuestItemDAO;
end;

function TQuestItemService.GetAt(ResourceID, MarkerID: Variant; const Entity: TDBEntity): Boolean;
begin
  Result := QuestItemDAO.GetAt(ResourceID, MarkerID, Entity);
end;

procedure TQuestItemService.Remove(const ResourceID, MarkerID: Variant);
begin
  QuestItemDAO.Remove(ResourceID, MarkerID);
end;

procedure TQuestItemService.Save(const ResourceID, MarkerID: Variant; Amount: Integer);
begin
  QuestItemDAO.Save(ResourceID, MarkerID, Amount);
end;

procedure TQuestItemService.Save(const ResourceID, MarkerID: Variant);
begin
  Save(ResourceID, MarkerID, 1);
end;

end.
