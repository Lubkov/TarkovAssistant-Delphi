unit ME.Service.Point;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, Data.DB, ME.DB.Entity,
  ME.DB.DAO, ME.DB.Service, ME.DB.Point, ME.DAO.Point;

type
  TPointService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure LoadQuestParts(const QuestID: Variant; const Items: TList<TPoint>);
  end;

var
  PointService: TPointService;

implementation

{ TPointService }

function TPointService.GetDAOClass: TDAOClass;
begin
  Result := TPointDAO;
end;

procedure TPointService.LoadQuestParts(const QuestID: Variant; const Items: TList<TPoint>);
begin
  TPointDAO(DAO).LoadQuestParts(QuestID, Items);
end;

end.
