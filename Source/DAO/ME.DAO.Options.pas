unit ME.DAO.Options;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections,  Data.DB, MemDS, DBAccess,
  Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Options;

type
  TOptionsDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TDBEntityClass; override;
  public
    function GetFirst(const Entity: TDBEntity): Boolean;
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
  end;


implementation

{ TOptionsDAO }

function TOptionsDAO.EntityClass: TDBEntityClass;
begin
  Result := TOptions;
end;

function TOptionsDAO.GetFirst(const Entity: TDBEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + EntityClass.FieldList + ' FROM ' + EntityClass.EntityName + ' ORDER BY ROWID ASC LIMIT 1';
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TOptionsDAO.Insert(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Options: TOptions;
begin
  Options := TOptions(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'INSERT INTO ' + EntityClass.EntityName +
      '  (Profile, DataPath, SreenshotPath, TrackLocation) ' +
      'VALUES ' +
      '  (:ProfileID, :DataPath, :SreenshotPath, :TrackLocation)';
    Query.ParamByName('Profile').AsString := Options.Profile;
    Query.ParamByName('DataPath').AsString := Options.DataPath;
    Query.ParamByName('SreenshotPath').AsString := Options.SreenshotPath;
    Query.ParamByName('TrackLocation').AsBoolean := Boolean(Options.TrackLocation);
    Query.Execute;
    Options.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TOptionsDAO.Update(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Profile: TOptions;
begin
  Profile := TOptions(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE ' + EntityClass.EntityName +
      'SET ' +
      '  Profile = :Profile, ' +
      '  DataPath = :DataPath, ' +
      '  SreenshotPath = :SreenshotPath, ' +
      '  TrackLocation = :TrackLocation ' +
      'WHERE ID = :ID';
    Query.ParamByName('ID').Value := Profile.ID;
    Query.ParamByName('Profile').AsString := Profile.Profile;
    Query.ParamByName('DataPath').AsString := Profile.DataPath;
    Query.ParamByName('SreenshotPath').AsString := Profile.SreenshotPath;
    Query.ParamByName('TrackLocation').AsBoolean := Profile.TrackLocation;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
