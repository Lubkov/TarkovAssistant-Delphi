unit ME.Service.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, Data.DB, ME.DB.Entity,
  ME.DB.DAO, ME.DB.Service, ME.DB.QuestTracker, ME.DAO.QuestTracker;

type
  TQuestTrackerService = class(TServiceCommon)
  private
    function GetQuestTrackerDAO: TQuestTrackerDAO;
  protected
    function GetDAOClass: TDAOClass; override;
  public
    function GetMarkerState(MarkerID: Integer; const Entity: TEntity): Boolean;
    procedure GetProfileProgress(const Items: TList<TQuestTracker>);

    property QuestTrackerDAO: TQuestTrackerDAO read GetQuestTrackerDAO;
  end;

var
  QuestTrackerService: TQuestTrackerService;

implementation

uses
  App.Service;

{ TQuestTrackerService }

function TQuestTrackerService.GetQuestTrackerDAO: TQuestTrackerDAO;
begin
  Result := TQuestTrackerDAO(DAO);
end;

function TQuestTrackerService.GetDAOClass: TDAOClass;
begin
  Result := TQuestTrackerDAO;
end;

function TQuestTrackerService.GetMarkerState(MarkerID: Integer; const Entity: TEntity): Boolean;
begin
  Result := QuestTrackerDAO.GetMarkerState(MarkerID, AppService.Profile.ID, Entity);
end;

procedure TQuestTrackerService.GetProfileProgress(const Items: TList<TQuestTracker>);
begin
  Items.Clear;
  QuestTrackerDAO.GetProfileProgress(AppService.Profile.ID, Items);
end;

end.
