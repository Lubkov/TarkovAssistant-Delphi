unit ME.DB.Service;

interface

uses
  System.Classes, System.SysUtils, System.Variants, Generics.Collections, Data.DB,
  MemDS, DBAccess, Uni, ME.DB.Entity, ME.DB.DAO;

type
  TServiceCommon = class(TObject)
  private
    FConnection: TCustomConnection;
    FDAO: TDAOCommon;

    function GetConnection: TUniConnection;
    function GetInTransaction: Boolean;
  protected
    function GetDAOClass: TDAOClass; virtual; abstract;
  public
    constructor Create(const Connection: TCustomConnection);
    destructor Destroy; override;

    function GetAt(ID: Integer; const Entity: TDBEntity): Boolean; virtual;
    procedure GetAll(const Items: TList<TDBEntity>); virtual;
    procedure Insert(const Entity: TDBEntity); virtual;
    procedure Update(const Entity: TDBEntity); virtual;
    procedure Save(const Entity: TDBEntity); virtual;
    procedure Remove(const ID: Variant); overload; virtual;
    procedure Remove(const Entity: TDBEntity); overload; virtual;
    procedure RemoveAll; virtual;
    function RecordCount: Int64; virtual;

    function GetNewInstance: TDBEntity; virtual;

    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    property Connection: TUniConnection read GetConnection;
    property DAO: TDAOCommon read FDAO;
    property InTransaction: Boolean read GetInTransaction;
  end;

implementation

uses
  ME.DB.Utils;

{ TServiceCommon }

constructor TServiceCommon.Create(const Connection: TCustomConnection);
begin
  inherited Create;

  FConnection := Connection;
  FDAO := GetDAOClass.Create(Connection);
end;

destructor TServiceCommon.Destroy;
begin
  FreeAndNil(FDAO);

  inherited;
end;

function TServiceCommon.GetConnection: TUniConnection;
begin
  Result := TUniConnection(FConnection);
end;

function TServiceCommon.GetInTransaction: Boolean;
begin
  Result := Connection.InTransaction;
end;

function TServiceCommon.GetAt(ID: Integer; const Entity: TDBEntity): Boolean;
begin
  Result := FDAO.GetAt(ID, Entity);
end;

procedure TServiceCommon.GetAll(const Items: TList<TDBEntity>);
begin
  FDAO.GetAll(Items);
end;

procedure TServiceCommon.Insert(const Entity: TDBEntity);
begin
  FDAO.Insert(Entity);
end;

procedure TServiceCommon.Update(const Entity: TDBEntity);
begin
  FDAO.Update(Entity);
end;

procedure TServiceCommon.Save(const Entity: TDBEntity);
begin
  if IsNullID(Entity.ID) then
    Insert(Entity)
  else
    Update(Entity);
end;

procedure TServiceCommon.Remove(const ID: Variant);
begin
  FDAO.Remove(ID);
end;

procedure TServiceCommon.Remove(const Entity: TDBEntity);
begin
  Remove(Entity.ID);
end;

procedure TServiceCommon.RemoveAll;
begin
  FDAO.TruncateTable;
end;

function TServiceCommon.RecordCount: Int64;
begin
  Result := FDAO.RecordCount;
end;

function TServiceCommon.GetNewInstance: TDBEntity;
begin
  Result := FDAO.GetNewInstance;
end;

procedure TServiceCommon.StartTransaction;
begin
  if not Connection.InTransaction then
    Connection.StartTransaction;
end;

procedure TServiceCommon.CommitTransaction;
begin
  if Connection.InTransaction then
    Connection.Commit;
end;

procedure TServiceCommon.RollbackTransaction;
begin
  if Connection.InTransaction then
    Connection.Rollback;
end;

end.
