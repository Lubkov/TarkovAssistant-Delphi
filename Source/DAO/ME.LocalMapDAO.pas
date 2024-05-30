unit ME.LocalMapDAO;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections, Data.DB,
  MemDS, DBAccess, Uni, ME.DB.Entity, ME.DB.DAO, ME.Point, ME.LocalMap, ME.MapLevel;

type
  TLocalMapDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure GetAll(const Items: TList<TEntity>); override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;

    procedure LoadMapLevels(const Entity: TEntity; LoadPicture: Boolean);
    procedure RemoveMapLevels(const MapID: Variant);
  end;

implementation

const
  SqlSelectCommandText =
    ' SELECT ' +
    '     m.ID as ID, ' +
    '     m.Name as Name, ' +
    '     m.Left as Left, ' +
    '     m.Top as Top, ' +
    '     m.Right as Right, ' +
    '     m.Bottom as Bottom, ' +
    '     m.Picture as Picture ' +
    ' FROM LocalMap m ' +
    ' %s ';

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
    Query.SQL.Text := Format(SqlSelectCommandText, [' WHERE (m.ID = :ID) ']);
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TLocalMapDAO.GetAll(const Items: TList<TEntity>);
var
  Query: TUniQuery;
  Entity: TEntity;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, ['']);
    Query.Open;

    while not Query.Eof do begin
      Entity := EntityClass.Create;
      try
        Entity.Assign(Query);
      finally
        Items.Add(Entity);
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TLocalMapDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  LocalMap: TLocalMap;
  Param: TParam;
begin
  LocalMap := TLocalMap(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO LocalMap (Name, Left, Top, Right, Bottom, Picture) ' +
      ' VALUES (:Name, :Left, :Top, :Right, :Bottom, :Picture) ';
    Query.ParamByName('Name').AsString := LocalMap.Name;
    Query.ParamByName('Left').AsInteger := LocalMap.Left;
    Query.ParamByName('Top').AsInteger := LocalMap.Top;
    Query.ParamByName('Right').AsInteger := LocalMap.Right;
    Query.ParamByName('Bottom').AsInteger := LocalMap.Bottom;
    Param := Query.ParamByName('Picture');
    LocalMap.AssignPictureTo(LocalMap.Picture, Param);

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
      '   Left = :Left, ' +
      '   Top = :Top, ' +
      '   Right = :Right, ' +
      '   Bottom = :Bottom, ' +
      '   Picture = :Picture ' +
      ' WHERE ID = :ID ';
    Query.ParamByName('ID').Value := LocalMap.ID;
    Query.ParamByName('Name').AsString := LocalMap.Name;
    Query.ParamByName('Left').AsInteger := LocalMap.Left;
    Query.ParamByName('Top').AsInteger := LocalMap.Top;
    Query.ParamByName('Right').AsInteger := LocalMap.Right;
    Query.ParamByName('Bottom').AsInteger := LocalMap.Bottom;
    LocalMap.AssignPictureTo(LocalMap.Picture, Query.ParamByName('Picture'));
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
  FieldNames := TMapLevel.FieldList;
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
