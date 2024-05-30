unit ME.DAO.Point;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, Data.DB, MemDS, DBAccess, Uni,
  ME.DB.Entity, ME.DB.DAO, ME.DB.Point;

type
  TPointDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;

    procedure LoadQuestParts(const QuestID: Variant; const Items: TList<TPoint>);
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
    Query.SQL.Text := 'SELECT ID, QuestID, X, Y FROM Point WHERE ID = :ID';
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
    Query.SQL.Text := 'INSERT INTO Point (QuestID, X, Y) VALUES (:QuestID, :X, :Y)';
    Query.ParamByName('QuestID').Value := Point.QuestID;
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
    Query.SQL.Text := 'UPDATE Point SET QuestID = :QuestID, X = :X, Y = :Y WHERE ID = :ID';
    Query.ParamByName('ID').Value := Point.ID;
    Query.ParamByName('QuestID').Value := Point.QuestID;
    Query.ParamByName('X').AsInteger := Point.X;
    Query.ParamByName('Y').AsInteger := Point.Y;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TPointDAO.LoadQuestParts(const QuestID: Variant; const Items: TList<TPoint>);
var
  Query: TUniQuery;
  Point: TPoint;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, QuestID, X, Y FROM Point WHERE QuestID = :QuestID';
    Query.ParamByName('QuestID').Value := QuestID;
    Query.Open;

    while not Query.Eof do begin
      Point := TPoint.Create;
      try
        Point.Assign(Query);
      finally
        Items.Add(Point);
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
