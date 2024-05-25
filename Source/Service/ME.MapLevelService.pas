unit ME.MapLevelService;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.MapLevel, ME.MapLevelDAO, Generics.Collections;

type
  TMapLevelService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure SavePicture(const Entity: TEntity);
  end;

var
  MapLevelService: TMapLevelService;

implementation

{ TMapLevelService }

function TMapLevelService.GetDAOClass: TDAOClass;
begin
  Result := TMapLevelDAO;
end;

procedure TMapLevelService.SavePicture(const Entity: TEntity);
begin
  TMapLevelDAO(DAO).SavePicture(Entity);
end;

end.
