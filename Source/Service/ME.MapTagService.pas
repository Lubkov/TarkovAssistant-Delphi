unit ME.MapTagService;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections,
  Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service, ME.MapTag, ME.MapTagDAO;

type
  TMapTagService = class(TServiceCommon)
  private
    function GetMapTagDAO: TMapTagDAO;
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure GetMapTags(const MapID: Variant; const Items: TList<TMapTag>);

    property MapTagDAO: TMapTagDAO read GetMapTagDAO;
  end;

var
  MapTagService: TMapTagService;

implementation

uses
  ME.PointService;

{ TMapTagService }

function TMapTagService.GetMapTagDAO: TMapTagDAO;
begin
  Result := TMapTagDAO(DAO);
end;

function TMapTagService.GetDAOClass: TDAOClass;
begin
  Result := TMapTagDAO;
end;

procedure TMapTagService.GetMapTags(const MapID: Variant; const Items: TList<TMapTag>);
begin
  MapTagDAO.GetMapTags(MapID, Items);
end;

end.
