unit ME.LocalMapService;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.LocalMap, ME.MapLevel, ME.LocalMapDAO;

type
  TLocalMapService = class(TServiceCommon)
  private
    function GetLocalMapDAO: TLocalMapDAO;
  protected
    function GetDAOClass: TDAOClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
    procedure Remove(const ID: Variant); override;

    procedure LoadMapLevels(const Entity: TEntity; LoadPicture: Boolean);

    property LocalMapDAO: TLocalMapDAO read GetLocalMapDAO;
  end;

var
  LocalMapService: TLocalMapService;

implementation

uses
  ME.PointService, ME.MapLevelService;

{ TLocalMapService }

function TLocalMapService.GetLocalMapDAO: TLocalMapDAO;
begin
  Result := TLocalMapDAO(DAO);
end;

function TLocalMapService.GetDAOClass: TDAOClass;
begin
  Result := TLocalMapDAO;
end;

function TLocalMapService.GetAt(ID: Integer; const Entity: TEntity): Boolean;
begin
  Result := inherited GetAt(ID, Entity);
  if Result then
    LoadMapLevels(Entity, False);
end;

procedure TLocalMapService.Insert(const Entity: TEntity);
var
  LocalMap: TLocalMap;
  Level: TMapLevel;
begin
  LocalMap := TLocalMap(Entity);

  StartTransaction;
  try
    PointService.Insert(LocalMap.Left);
    PointService.Insert(LocalMap.Right);
    DAO.Insert(LocalMap);

    for Level in LocalMap.Levels do begin
      Level.MapID := LocalMap.ID;
      MapLevelService.Insert(Level);
    end;

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TLocalMapService.Update(const Entity: TEntity);
var
  LocalMap: TLocalMap;
  Level: TMapLevel;
begin
  LocalMap := TLocalMap(Entity);

  StartTransaction;
  try
    PointService.Update(LocalMap.Left);
    PointService.Update(LocalMap.Right);
    DAO.Update(LocalMap);

    for Level in LocalMap.Levels do
      MapLevelService.Update(Level);

    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
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

end.

