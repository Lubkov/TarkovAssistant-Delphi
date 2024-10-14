unit ME.DB.DAO;

interface

uses
  System.Classes, System.SysUtils, System.Variants, Generics.Collections,
  Data.DB, MemDS, DBAccess, Uni, App.Entity, ME.DB.Entity;

type
  TDAOClass = class of TDAOCommon;

  TDAOCommon = class(TObject)
  private
    FConnection: TCustomConnection;

    function GetConnection: TUniConnection;
  protected
    function EntityClass: TDBEntityClass; virtual; abstract;
    function GetSqlSelectCommandText: string; virtual; abstract;
  public
    constructor Create(const Connection: TCustomConnection);

    function GetNewInstance: TDBEntity; virtual; //CreateInstance
    function GetAt(ID: Integer; const Entity: TDBEntity): Boolean; virtual;
    procedure GetAll(const Items: TList<TDBEntity>); virtual; abstract;
    procedure Insert(const Entity: TDBEntity); virtual; abstract;
    procedure Update(const Entity: TDBEntity); virtual; abstract;
    procedure Remove(const ID: Variant);
    procedure TruncateTable;
    function RecordCount: Int64;

    property Connection: TUniConnection read GetConnection;
  end;

implementation

{ TDAOCommon }

constructor TDAOCommon.Create(const Connection: TCustomConnection);
begin
  inherited Create;

  FConnection := Connection;
end;

function TDAOCommon.GetConnection: TUniConnection;
begin
  Result := TUniConnection(FConnection);
end;

function TDAOCommon.GetNewInstance: TDBEntity;
begin
  Result := EntityClass.Create;
end;

function TDAOCommon.GetAt(ID: Integer; const Entity: TDBEntity): Boolean;
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

procedure TDAOCommon.Remove(const ID: Variant);
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM ' + EntityClass.EntityName + ' WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TDAOCommon.TruncateTable;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM ' + EntityClass.EntityName;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

function TDAOCommon.RecordCount: Int64;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT Count(*) RecCount FROM ' + EntityClass.EntityName;
    Query.Open;

    Result := Query.Fields[0].AsLargeInt;
  finally
    Query.Free;
  end;
end;

end.
