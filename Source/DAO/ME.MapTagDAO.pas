unit ME.MapTagDAO;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections, Data.DB,
  MemDS, DBAccess, Uni, ME.DB.Entity, ME.DB.DAO, ME.Point, ME.MapTag;

type
  TMapTagDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure GetAll(const Items: TList<TEntity>); override;
    procedure GetMapTags(const MapID: Variant; const Items: TList<TMapTag>);
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
  end;

implementation

const
  SqlSelectCommandText =
    ' SELECT ' +
    '     t.ID as ID, ' +
    '     t.MapID as MapID, ' +
    '     t.Name as Name, ' +
    '     t.Kind as Kind, ' +
    '     p.ID as Position, ' +
    '     p.X as X1, ' +
    '     p.Y as Y1 ' +
    ' FROM MapTag t ' +
    '   INNER JOIN Point p ON (p.ID = t.Position) ' +
    '             %s ';

{ TMapTagDAO }

function TMapTagDAO.EntityClass: TEntityClass;
begin
  Result := TMapTag;
end;

function TMapTagDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [' AND (t.ID = :ID) ']);
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TMapTagDAO.GetAll(const Items: TList<TEntity>);
const
  Filter = '';
var
  Query: TUniQuery;
  Entity: TEntity;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [Filter]);
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

procedure TMapTagDAO.GetMapTags(const MapID: Variant; const Items: TList<TMapTag>);
const
  Filter = ' AND (t.MapID = :MapID) ';
var
  Query: TUniQuery;
  Entity: TMapTag;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [Filter]);
    Query.ParamByName('MapID').Value := MapID;
    Query.Open;

    while not Query.Eof do begin
      Entity := TMapTag.Create;
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
procedure TMapTagDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  MapTag: TMapTag;
begin
  MapTag := TMapTag(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO MapTag (MapID, Name, Kind, Position) ' +
      ' VALUES (:Name, :Kind, :Position) ';
    Query.ParamByName('MapID').Value := MapTag.MapID;
    Query.ParamByName('Name').AsString := MapTag.Name;
    Query.ParamByName('Kind').AsInteger := Ord(MapTag.Kind);
    Query.ParamByName('Position').AsInteger := MapTag.Position.ID;
    Query.Execute;
    MapTag.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TMapTagDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  MapTag: TMapTag;
begin
  MapTag := TMapTag(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' UPDATE MapTag ' +
      ' SET ' +
      '   MapID = :MapID, ' +
      '   Name = :Name, ' +
      '   Kind = :Kind, ' +
      '   Position = :Position ' +
      ' WHERE ID = :ID ';
    Query.ParamByName('ID').Value := MapTag.ID;
    Query.ParamByName('MapID').Value := MapTag.MapID;
    Query.ParamByName('Name').AsString := MapTag.Name;
    Query.ParamByName('Kind').AsInteger := Ord(MapTag.Kind);
    Query.ParamByName('Position').AsInteger := MapTag.Position.ID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
