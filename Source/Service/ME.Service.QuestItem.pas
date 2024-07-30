unit ME.Service.QuestItem;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.QuestItem, ME.DAO.QuestItem;

type
  TQuestItemService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
  end;

var
  QuestItemService: TQuestItemService;

implementation

{ TQuestItemService }

function TQuestItemService.GetDAOClass: TDAOClass;
begin
  Result := TQuestItemDAO;
end;

end.
