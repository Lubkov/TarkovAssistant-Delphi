unit ME.PointService;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.Point, ME.PointDAO;

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
