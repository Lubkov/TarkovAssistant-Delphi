unit ME.DAO.Quest;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections,  Data.DB, MemDS, DBAccess,
  Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Quest;

type
  TQuestDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TDBEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TDBEntity): Boolean; override;
    procedure LoadQuests(const MapID: Variant; const Items: TList<TDBQuest>);
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
  end;

implementation

{ TQuestDAO }

function TQuestDAO.EntityClass: TDBEntityClass;
begin
  Result := TDBQuest;
end;

function TQuestDAO.GetAt(ID: Integer; const Entity: TDBEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, MapID, Name, Trader FROM Quest WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TQuestDAO.LoadQuests(const MapID: Variant; const Items: TList<TDBQuest>);
var
  Query: TUniQuery;
  Quest: TDBQuest;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, MapID, Name, Trader FROM Quest WHERE MapID = :MapID ORDER BY Name';
    Query.ParamByName('MapID').Value := MapID;
    Query.Open;

    while not Query.Eof do begin
      Quest := TDBQuest.Create;
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

procedure TQuestDAO.Insert(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Quest: TDBQuest;
begin
  Quest := TDBQuest(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Quest (MapID, Name, Trader) VALUES (:MapID, :Name, :Trader)';
    Query.ParamByName('MapID').Value := Quest.MapID;
    Query.ParamByName('Name').AsString := Quest.Name;
    Query.ParamByName('Trader').AsInteger := Ord(Quest.Trader);
    Query.Execute;
    Quest.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TQuestDAO.Update(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Quest: TDBQuest;
begin
  Quest := TDBQuest(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE Quest SET MapID = :MapID, Name = :Name, Trader = :Trader WHERE ID = :ID';
    Query.ParamByName('ID').Value := Quest.ID;
    Query.ParamByName('MapID').Value := Quest.MapID;
    Query.ParamByName('Name').AsString := Quest.Name;
    Query.ParamByName('Trader').AsInteger := Ord(Quest.Trader);
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
