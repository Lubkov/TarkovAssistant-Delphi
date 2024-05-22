unit ME.LocalMapDAO;

interface

uses
  System.SysUtils, System.Classes, Data.DB, MemDS, DBAccess, Uni,
  ME.DB.Entity, ME.DB.DAO, ME.Point, ME.LocalMap, ME.MapLevel;

type
  TLocalMapDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;

    procedure LoadMapLevels(const Entity: TEntity; LoadPicture: Boolean);
    procedure RemoveMapLevels(const MapID: Variant);
  end;

implementation

{ TLocalMapDAO }

function TLocalMapDAO.EntityClass: TEntityClass;
begin
  Result := TLocalMap;
end;

function TLocalMapDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := ' SELECT ' +
                      '     m.ID as ID, ' +
                      '     m.Name as Name, ' +
                      '     m.LeftID as LeftID, ' +
                      '     p1.X as X1, ' +
                      '     p1.Y as Y1, ' +
                      '     m.RightID as RightID, ' +
                      '     p2.X as X2, ' +
                      '     p2.Y as Y2 ' +
                      ' FROM LocalMap m ' +
                      '   INNER JOIN Point p1 ON (p1.ID = m.LeftID) ' +
                      '             AND (m.ID = :ID) ' +
                      '   INNER JOIN Point p2 ON p2.ID = m.RightID ';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TLocalMapDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  LocalMap: TLocalMap;
begin
  LocalMap := TLocalMap(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO LocalMap (Name, LeftID, RightID) ' +
      ' VALUES (:Name, :LeftID, :RightID) ';
    Query.ParamByName('Name').AsString := LocalMap.Name;
    Query.ParamByName('LeftID').AsInteger := LocalMap.Left.ID;
    Query.ParamByName('RightID').AsInteger := LocalMap.Right.ID;
    Query.Execute;
    LocalMap.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TLocalMapDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  LocalMap: TLocalMap;
begin
  LocalMap := TLocalMap(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' UPDATE LocalMap ' +
      ' SET ' +
      '   Name = :Name, ' +
      '   LeftID = :LeftID, ' +
      '   RightID = :RightID ' +
      ' WHERE ID = :ID ';
    Query.ParamByName('ID').Value := LocalMap.ID;
    Query.ParamByName('Name').AsString := LocalMap.Name;
    Query.ParamByName('LeftID').AsInteger := LocalMap.Left.ID;
    Query.ParamByName('RightID').AsInteger := LocalMap.Right.ID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TLocalMapDAO.LoadMapLevels(const Entity: TEntity; LoadPicture: Boolean);
const
  PictureFileName = 'Picture';
var
  Query: TUniQuery;
  LocalMap: TLocalMap;
  Level: TMapLevel;
  FieldNames: string;
begin
  LocalMap := TLocalMap(Entity);
  FieldNames := 'ID, MapID, Level';
  if LoadPicture then
    FieldNames := FieldNames + ', ' + PictureFileName;

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + FieldNames + ' FROM MapLevel WHERE MapID = :MapID';
    Query.ParamByName('MapID').Value := Entity.ID;
    Query.Open;

    LocalMap.ClearLevelList;
    while not Query.Eof do begin
      Level := TMapLevel.Create;
      try
        Level.Assign(Query);
      finally
        LocalMap.Levels.Add(Level);
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TLocalMapDAO.RemoveMapLevels(const MapID: Variant);
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM ' + TMapLevel.EntityName + ' WHERE MapID = :MapID';
    Query.ParamByName('MapID').Value := MapID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
