unit ME.DAO.Profile;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections,  Data.DB, MemDS, DBAccess,
  Uni, ME.DB.Entity, ME.DB.DAO, ME.Profile;

type
  TProfileDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TDBEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TDBEntity): Boolean; override;
    function GetAtName(const Name: string; const Entity: TDBEntity): Boolean;
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
  end;


implementation

{ TProfileDAO }

function TProfileDAO.EntityClass: TDBEntityClass;
begin
  Result := TProfile;
end;

function TProfileDAO.GetAt(ID: Integer; const Entity: TDBEntity): Boolean;
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

function TProfileDAO.GetAtName(const Name: string; const Entity: TDBEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + EntityClass.FieldList + ' FROM ' + EntityClass.EntityName + ' WHERE Name = :Name';
    Query.ParamByName('Name').AsString := Name;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TProfileDAO.Insert(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Profile: TProfile;
begin
  Profile := TProfile(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Profile (Name, Kind) VALUES (:Name, :Kind)';
    Query.ParamByName('Name').AsString := Profile.Name;
    Query.ParamByName('Kind').AsInteger := Ord(Profile.Kind);
    Query.Execute;
    Profile.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TProfileDAO.Update(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Profile: TProfile;
begin
  Profile := TProfile(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE Profile SET Name = :Name, Kind = :Kind WHERE ID = :ID';
    Query.ParamByName('ID').Value := Profile.ID;
    Query.ParamByName('Name').AsString := Profile.Name;
    Query.ParamByName('Kind').AsInteger := Ord(Profile.Kind);
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
