unit ME.PointDAO;

interface

uses
  System.SysUtils, System.Classes, ME.DB.Entity, ME.DB.DAO, ME.Point,
  Data.DB, MemDS, DBAccess, Uni;

type
  TPointDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
  end;

implementation

{ TPointDAO }

function TPointDAO.EntityClass: TEntityClass;
begin
  Result := TPoint;
end;

function TPointDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, X, Y FROM Point WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TPointDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Point: TPoint;
begin
  Point := TPoint(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Point (X, Y) VALUES (:X, :Y)';
    Query.ParamByName('X').AsInteger := Point.X;
    Query.ParamByName('Y').AsInteger := Point.Y;
    Query.Execute;
    Point.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TPointDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  Point: TPoint;
begin
  Point := TPoint(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE Point SET X = :X, Y = :Y WHERE ID = :ID';
    Query.ParamByName('ID').Value := Point.ID;
    Query.ParamByName('X').AsInteger := Point.X;
    Query.ParamByName('Y').AsInteger := Point.Y;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
