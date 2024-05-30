unit ME.DAO.Quest;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections,  Data.DB, MemDS, DBAccess,
  Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Quest;

type
  TQuestDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure LoadQuests(const MapID: Variant; const Items: TList<TQuest>);
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
  end;

implementation

{ TQuestDAO }

function TQuestDAO.EntityClass: TEntityClass;
begin
  Result := TQuest;
end;

function TQuestDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, MapID, Name FROM Quest WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TQuestDAO.LoadQuests(const MapID: Variant; const Items: TList<TQuest>);
var
  Query: TUniQuery;
  Quest: TQuest;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, MapID, Name FROM Quest WHERE MapID = :MapID';
    Query.ParamByName('MapID').Value := MapID;
    Query.Open;

    while not Query.Eof do begin
      Quest := TQuest.Create;
      try
        Quest.Assign(Query);
      finally
        Items.Add(Quest);
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TQuestDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Quest: TQuest;
begin
  Quest := TQuest(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Quest (MapID, Name) VALUES (:MapID, :Name)';
    Query.ParamByName('MapID').Value := Quest.MapID;
    Query.ParamByName('Name').AsString := Quest.Name;
    Query.Execute;
    Quest.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TQuestDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  Quest: TQuest;
begin
  Quest := TQuest(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE Quest SET MapID = :MapID, Name = :Name WHERE ID = :ID';
    Query.ParamByName('ID').Value := Quest.ID;
    Query.ParamByName('MapID').Value := Quest.MapID;
    Query.ParamByName('Name').AsString := Quest.Name;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
