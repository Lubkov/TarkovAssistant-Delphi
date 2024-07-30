unit ME.DAO.QuestItem;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, MemDS, DBAccess, Uni,
  ME.DB.Entity, ME.DB.DAO, ME.DB.QuestItem;

type
  TQuestItemDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
  end;

implementation

{ TQuestItemDAO }

function TQuestItemDAO.EntityClass: TEntityClass;
begin
  Result := TDBQuestItem;
end;

function TQuestItemDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + TDBQuestItem.FieldList + ' FROM ' + TDBQuestItem.EntityName + ' WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TQuestItemDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  QuestItem: TDBQuestItem;
begin
  QuestItem := TDBQuestItem(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO ' + TDBQuestItem.EntityName +
      '   (ResourceID, MarkerID) ' +
      ' VALUES ' +
      '   (:ResourceID, :MarkerID) ';
    Query.ParamByName('ResourceID').Value := QuestItem.ResourceID;
    Query.ParamByName('MarkerID').Value := QuestItem.MarkerID;
    Query.Execute;
    QuestItem.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TQuestItemDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  QuestItem: TDBQuestItem;
begin
  QuestItem := TDBQuestItem(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' UPDATE ' + TDBQuestItem.EntityName +
      ' SET ' +
      '    ResourceID = :ResourceID, ' +
      '    MarkerID = :MarkerID ' +
      'WHERE ID = :ID';
    Query.ParamByName('ID').Value := QuestItem.ID;
    Query.ParamByName('ResourceID').Value := QuestItem.ResourceID;
    Query.ParamByName('MarkerID').Value := QuestItem.MarkerID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
