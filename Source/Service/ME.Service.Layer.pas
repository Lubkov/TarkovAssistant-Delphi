unit ME.Service.Layer;

interface

uses
  System.SysUtils, System.Classes, FMX.Graphics, Data.DB, ME.DB.Entity, ME.DB.DAO,
  ME.DB.Service, ME.DB.Layer, ME.DAO.Layer, Generics.Collections;

type
  TLayerService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure LoadPicture(const LayerID: Variant; const Picture: TBitmap);
    procedure GetMapLayers(const MapID: Variant; const Items: TList<TDBLayer>);
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

procedure TLayerService.LoadPicture(const LayerID: Variant; const Picture: TBitmap);
begin
  TLayerDAO(DAO).LoadPicture(LayerID, Picture);
end;

procedure TLayerService.GetMapLayers(const MapID: Variant; const Items: TList<TDBLayer>);
begin
  TLayerDAO(DAO).GetMapLayers(MapID, TList<TEntity>(Items));
end;

procedure TLayerService.SavePicture(const Entity: TEntity);
begin
  TLayerDAO(DAO).SavePicture(Entity);
end;

end.
