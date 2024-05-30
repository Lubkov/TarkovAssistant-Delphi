unit ME.Service.Map;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Map, ME.MapLevel, ME.DB.Marker, ME.DAO.Map;

type
  TMapService = class(TServiceCommon)
  private
    function GetMapDAO: TMapDAO;
  protected
    function GetDAOClass: TDAOClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
    procedure Remove(const ID: Variant); override;

    procedure LoadMapLevels(const Map: TMap; LoadPicture: Boolean);
    procedure LoadMarkers(const Map: TMap);

    property MapDAO: TMapDAO read GetMapDAO;
  end;

var
  MapService: TMapService;

implementation

uses
  ME.DB.Utils, ME.PointService, ME.MapLevelService, ME.Service.Marker;

{ TMapService }

function TMapService.GetMapDAO: TMapDAO;
begin
  Result := TMapDAO(DAO);
end;

function TMapService.GetDAOClass: TDAOClass;
begin
  Result := TMapDAO;
end;

function TMapService.GetAt(ID: Integer; const Entity: TEntity): Boolean;
begin
  Result := inherited GetAt(ID, Entity);
end;

procedure TMapService.Insert(const Entity: TEntity);
var
  Map: TMap;
  Level: TMapLevel;
  Marker: TMarker;
begin
  Map := TMap(Entity);

  StartTransaction;
  try
    DAO.Insert(Map);

    for Level in Map.Levels do begin
      Level.MapID := Map.ID;
      MapLevelService.Insert(Level);
    end;

    for Marker in Map.Tags do begin
      Marker.MapID := Map.ID;
      MarkerService.Insert(Marker);
    end;

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TMapService.Update(const Entity: TEntity);
var
  Map: TMap;
begin
  Map := TMap(Entity);

  StartTransaction;
  try
    DAO.Update(Map);

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TMapService.Remove(const ID: Variant);
begin
  StartTransaction;
  try
    MapDAO.RemoveMapLevels(ID);
    DAO.Remove(ID);

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TMapService.LoadMapLevels(const Map: TMap; LoadPicture: Boolean);
begin
  MapDAO.LoadMapLevels(Map, LoadPicture);
end;

procedure TMapService.LoadMarkers(const Map: TMap);
begin
  MarkerService.LoadMarkers(Map.ID, Map.Tags);
end;

end.

