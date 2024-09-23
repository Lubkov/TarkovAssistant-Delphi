unit ME.Service.Options;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Options, ME.DAO.Options;

type
  TOptionsService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
  end;

var
  OptionsService: TOptionsService;

implementation

{ TOptionsService }

function TOptionsService.GetDAOClass: TDAOClass;
begin
  Result := TOptionsDAO;
end;

end.
