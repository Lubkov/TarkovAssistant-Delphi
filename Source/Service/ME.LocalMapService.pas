unit ME.LocalMapService;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.LocalMap, ME.MapLevel, ME.MapTag, ME.LocalMapDAO;

type
  TLocalMapService = class(TServiceCommon)
  private
    function GetLocalMapDAO: TLocalMapDAO;

    procedure InternalSave(const Entity: TEntity);
  protected
    function GetDAOClass: TDAOClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
    procedure Remove(const ID: Variant); override;

    procedure LoadMapLevels(const Entity: TEntity; LoadPicture: Boolean);
    procedure LoadMapTags(const Entity: TEntity);

    property LocalMapDAO: TLocalMapDAO read GetLocalMapDAO;
  end;

var
  LocalMapService: TLocalMapService;

implementation

uses
  ME.DB.Utils, ME.PointService, ME.MapLevelService, ME.MapTagService;

{ TLocalMapService }

function TLocalMapService.GetLocalMapDAO: TLocalMapDAO;
begin
  Result := TLocalMapDAO(DAO);
end;

function TLocalMapService.GetDAOClass: TDAOClass;
begin
  Result := TLocalMapDAO;
end;

procedure TLocalMapService.InternalSave(const Entity: TEntity);
var
  LocalMap: TLocalMap;
  Level: TMapLevel;
  Tag: TMapTag;
begin
  LocalMap := TLocalMap(Entity);

  StartTransaction;
  try
    PointService.Save(LocalMap.Left);
    PointService.Save(LocalMap.Right);

    if LocalMap.IsNewInstance then
      DAO.Insert(LocalMap)
    else
      DAO.Update(LocalMap);

    for Level in LocalMap.Levels do begin
      Level.MapID := LocalMap.ID;
      MapLevelService.Save(Level);
    end;

    for Tag in LocalMap.Tags do begin
      Tag.MapID := LocalMap.ID;
      MapTagService.Save(Tag);
    end;

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

function TLocalMapService.GetAt(ID: Integer; const Entity: TEntity): Boolean;
begin
  Result := inherited GetAt(ID, Entity);
  if Result then
    LoadMapLevels(Entity, False);
end;

procedure TLocalMapService.Insert(const Entity: TEntity);
begin
  InternalSave(Entity);
end;

procedure TLocalMapService.Update(const Entity: TEntity);
begin
  InternalSave(Entity);
end;

procedure TLocalMapService.Remove(const ID: Variant);
begin
  StartTransaction;
  try
    LocalMapDAO.RemoveMapLevels(ID);
    DAO.Remove(ID);

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TLocalMapService.LoadMapLevels(const Entity: TEntity; LoadPicture: Boolean);
begin
  LocalMapDAO.LoadMapLevels(Entity, LoadPicture);
end;

procedure TLocalMapService.LoadMapTags(const Entity: TEntity);
var
  LocalMap: TLocalMap;
begin
  LocalMap := TLocalMap(Entity);

  MapTagService.GetMapTags(Entity.ID, LocalMap.Tags);
end;

end.

