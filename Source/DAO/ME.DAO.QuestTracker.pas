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
    function GetMarkerState(MarkerID, ProfileID: Variant; const Entity: TEntity): Boolean;
    procedure GetProfileProgress(ProfileID: Variant; const Items: TList<TQuestTracker>);
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

function TQuestTrackerDAO.GetMarkerState(MarkerID, ProfileID: Variant; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' SELECT ' + EntityClass.FieldList +
      ' FROM ' + EntityClass.EntityName +
      ' WHERE (MarkerID = :MarkerID) ' +
      '      AND (ProfileID = :ProfileID) ';
    Query.ParamByName('MarkerID').Value := MarkerID;
    Query.ParamByName('ProfileID').Value := ProfileID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TQuestTrackerDAO.GetProfileProgress(ProfileID: Variant; const Items: TList<TQuestTracker>);
var
  Query: TUniQuery;
  Entity: TQuestTracker;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' SELECT ' + EntityClass.FieldList +
      ' FROM ' + EntityClass.EntityName +
      ' WHERE (ProfileID = :ProfileID) ';
    Query.ParamByName('ProfileID').Value := ProfileID;
    Query.Open;

    while not Query.Eof do begin
      Entity := TQuestTracker.Create;
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

procedure TQuestTrackerDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Tracker: TQuestTracker;
begin
  Tracker := TQuestTracker(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO QuestTracker (QuestID, MarkerID, ProfileID, Finished) VALUES (:QuestID, :MarkerID, :ProfileID, :Finished)';
    Query.ParamByName('QuestID').Value := Tracker.QuestID;
    Query.ParamByName('MarkerID').Value := Tracker.MarkerID;
    Query.ParamByName('ProfileID').Value := Tracker.ProfileID;
    Query.ParamByName('Finished').AsInteger := Integer(Tracker.Finished);
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
      'UPDATE QuestTracker ' +
      'SET QuestID = :QuestID, ' +
      '    MarkerID = :MarkerID, ' +
      '    ProfileID = :ProfileID, ' +
      '    Finished = :Finished ' +
      'WHERE ID = :ID';
    Query.ParamByName('ID').Value := Tracker.ID;
    Query.ParamByName('QuestID').Value := Tracker.QuestID;
    Query.ParamByName('MarkerID').Value := Tracker.MarkerID;
    Query.ParamByName('ProfileID').Value := Tracker.ProfileID;
    Query.ParamByName('Finished').AsInteger := Integer(Tracker.Finished);
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
