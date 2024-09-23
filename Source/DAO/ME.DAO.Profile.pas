unit ME.DAO.Profile;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections,  Data.DB, MemDS, DBAccess,
  Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Profile;

type
  TProfileDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
  end;


implementation

{ TProfileDAO }

function TProfileDAO.EntityClass: TEntityClass;
begin
  Result := TProfile;
end;

function TProfileDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
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

procedure TProfileDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Profile: TProfile;
begin
  Profile := TProfile(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Profile (Name) VALUES (:Name)';
    Query.ParamByName('Name').Value := Profile.Name;
    Query.Execute;
    Profile.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TProfileDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  Profile: TProfile;
begin
  Profile := TProfile(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE Profile SET Name = :Name WHERE ID = :ID';
    Query.ParamByName('ID').Value := Profile.ID;
    Query.ParamByName('Name').AsString := Profile.Name;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
