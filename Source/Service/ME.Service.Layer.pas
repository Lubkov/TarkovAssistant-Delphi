unit ME.Service.Layer;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Layer, ME.DAO.Layer, Generics.Collections;

type
  TLayerService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure SavePicture(const Entity: TEntity);
  end;

var
  LayerService: TLayerService;

implementation

{ TLayerService }

function TLayerService.GetDAOClass: TDAOClass;
begin
  Result := TLayerDAO;
end;

procedure TLayerService.SavePicture(const Entity: TEntity);
begin
  TLayerDAO(DAO).SavePicture(Entity);
end;

end.
