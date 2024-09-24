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
    function Load(const Entity: TEntity): Boolean;
  end;

var
  OptionsService: TOptionsService;

implementation

{ TOptionsService }

function TOptionsService.GetDAOClass: TDAOClass;
begin
  Result := TOptionsDAO;
end;

function TOptionsService.Load(const Entity: TEntity): Boolean;
begin
  Result := TOptionsDAO(DAO).GetFirst(Entity);
end;

end.
