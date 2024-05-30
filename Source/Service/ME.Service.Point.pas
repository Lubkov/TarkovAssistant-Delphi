unit ME.Service.Point;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Point, ME.DAO.Point;

type
  TPointService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
  end;

var
  PointService: TPointService;

implementation

{ TPointService }

function TPointService.GetDAOClass: TDAOClass;
begin
  Result := TPointDAO;
end;

end.
