unit ME.DAO.QuestItem;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, MemDS, DBAccess, Uni,
  ME.DB.Entity, ME.DB.DAO, ME.DB.QuestItem;

type
  TQuestItemDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TDBEntityClass; override;
  public
    function GetAt(ResourceID, MarkerID: Integer; const Entity: TDBEntity): Boolean; overload;
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
    procedure Remove(const ResourceID, MarkerID: Variant); overload;
    procedure Save(const ResourceID, MarkerID: Variant; Amount: Integer);
  end;

implementation

{ TQuestItemDAO }

function TQuestItemDAO.EntityClass: TDBEntityClass;
begin
  Result := TDBQuestItem;
end;

function TQuestItemDAO.GetAt(ResourceID, MarkerID: Integer; const Entity: TDBEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' SELECT ' + TDBQuestItem.FieldList +
      ' FROM ' + TDBQuestItem.EntityName +
      ' WHERE (ResourceID = :ResourceID) AND (MarkerID = :MarkerID)';
    Query.ParamByName('ResourceID').Value := ResourceID;
    Query.ParamByName('MarkerID').Value := MarkerID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TQuestItemDAO.Insert(const Entity: TDBEntity);
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
      '   (ResourceID, MarkerID, Amount) ' +
      ' VALUES ' +
      '   (:ResourceID, :MarkerID, :Amount) ';
    Query.ParamByName('ResourceID').Value := QuestItem.ResourceID;
    Query.ParamByName('MarkerID').Value := QuestItem.MarkerID;
    Query.ParamByName('Amount').Value := QuestItem.Amount;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TQuestItemDAO.Update(const Entity: TDBEntity);
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
      '    Amount = :Amount ' +
      ' WHERE (ResourceID = :ResourceID) AND (MarkerID = :MarkerID)';
    Query.ParamByName('ResourceID').Value := QuestItem.ResourceID;
    Query.ParamByName('MarkerID').Value := QuestItem.MarkerID;
    Query.ParamByName('Amount').Value := QuestItem.Amount;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TQuestItemDAO.Remove(const ResourceID, MarkerID: Variant);
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' DELETE FROM ' + EntityClass.EntityName +
      ' WHERE (ResourceID = :ResourceID) AND (MarkerID = :MarkerID)';
    Query.ParamByName('ResourceID').Value := ResourceID;
    Query.ParamByName('MarkerID').Value := MarkerID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TQuestItemDAO.Save(const ResourceID, MarkerID: Variant; Amount: Integer);
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO ' + TDBQuestItem.EntityName +
      '     (ResourceID, MarkerID, Amount) ' +
      ' VALUES (:ResourceID, :MarkerID, :Amount) ' +
      ' ON CONFLICT(ResourceID, MarkerID) ' +
      ' DO UPDATE SET ' +
      '   Amount = excluded.Amount;';
    Query.ParamByName('ResourceID').Value := ResourceID;
    Query.ParamByName('MarkerID').Value := MarkerID;
    Query.ParamByName('Amount').Value := Amount;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
