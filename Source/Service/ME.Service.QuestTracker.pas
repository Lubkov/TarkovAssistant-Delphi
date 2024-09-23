unit ME.Service.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.QuestTracker, ME.DAO.QuestTracker;

type
  TQuestTrackerService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
  end;

var
  QuestTrackerService: TQuestTrackerService;

implementation

{ TQuestTrackerService }

function TQuestTrackerService.GetDAOClass: TDAOClass;
begin
  Result := TQuestTrackerDAO;
end;

end.
