unit ME.DAO.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections,  Data.DB, MemDS, DBAccess,
  Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.QuestTracker;

type
  TQuestTrackerDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
  end;


implementation

{ TQuestTrackerDAO }

function TQuestTrackerDAO.EntityClass: TEntityClass;
begin
  Result := TQuestTracker;
end;

function TQuestTrackerDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + EntityClass.FieldList + ' FROM ' + EntityClass.EntityName + ' WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TQuestTrackerDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Tracker: TQuestTracker;
begin
  Tracker := TQuestTracker(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Profile (QuestID, MarkerID, Status) VALUES (:QuestID, :MarkerID, :Status)';
    Query.ParamByName('QuestID').Value := Tracker.QuestID;
    Query.ParamByName('MarkerID').Value := Tracker.MarkerID;
    Query.ParamByName('Status').AsInteger := Ord(Tracker.Status);
    Query.Execute;
    Tracker.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TQuestTrackerDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  Tracker: TQuestTracker;
begin
  Tracker := TQuestTracker(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE Profile ' +
      'SET QuestID = :QuestID, ' +
      '    MarkerID = :MarkerID, ' +
      '    Status = :Status ' +
      'WHERE ID = :ID';
    Query.ParamByName('ID').Value := Tracker.ID;
    Query.ParamByName('QuestID').Value := Tracker.QuestID;
    Query.ParamByName('MarkerID').Value := Tracker.MarkerID;
    Query.ParamByName('Status').AsInteger := Ord(Tracker.Status);
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
