unit ME.Service.Resource;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Resource, ME.DAO.Resource;

type
  TResourceService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure SavePicture(const Entity: TEntity);
  end;

var
  ResourceService: TResourceService;

implementation

{ TResourceService }

function TResourceService.GetDAOClass: TDAOClass;
begin
  Result := TResourceDAO;
end;

procedure TResourceService.SavePicture(const Entity: TEntity);
begin
  TResourceDAO(DAO).SavePicture(Entity);
end;

end.
