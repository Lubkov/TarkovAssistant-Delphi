unit ME.MapLevelDAO;

interface

uses
  System.SysUtils, System.Classes, ME.DB.Entity, ME.DB.DAO, ME.MapLevel,
  Data.DB, MemDS, DBAccess, Uni;

type
  TMapLevelDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
    procedure LoadPicture(const Entity: TEntity);
    procedure SavePicture(const Entity: TEntity);
  end;

implementation

{ TMapLevelDAO }

function TMapLevelDAO.EntityClass: TEntityClass;
begin
  Result := TMapLevel;
end;

function TMapLevelDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, MapID, Level FROM MapLevel WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TMapLevelDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  MapLevel: TMapLevel;
begin
  MapLevel := TMapLevel(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO MapLevel (MapID, Level) VALUES (:MapID, :Level)';
    Query.ParamByName('MapID').Value := MapLevel.MapID;
    Query.ParamByName('Level').AsInteger := MapLevel.Level;
    Query.Execute;
    MapLevel.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TMapLevelDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  MapLevel: TMapLevel;
begin
  MapLevel := TMapLevel(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE MapLevel SET MapID = :MapID, Level = :Level WHERE ID = :ID';
    Query.ParamByName('ID').Value := MapLevel.ID;
    Query.ParamByName('MapID').AsInteger := MapLevel.MapID;
    Query.ParamByName('Level').AsInteger := MapLevel.Level;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TMapLevelDAO.LoadPicture(const Entity: TEntity);
var
  Query: TUniQuery;
  MapLevel: TMapLevel;
  Stream: TMemoryStream;
begin
  MapLevel := TMapLevel(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, Picture FROM MapLevel WHERE ID = :ID';
    Query.ParamByName('ID').Value := MapLevel.ID;
    Query.Open;

    Stream := TMemoryStream.Create;
    try
      TBlobField(Query.FieldByName('Picture')).SaveToStream(Stream);
      Stream.Position := 0;
      MapLevel.Picture.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    Query.Free;
  end;
end;

procedure TMapLevelDAO.SavePicture(const Entity: TEntity);
var
  Query: TUniQuery;
  MapLevel: TMapLevel;
  Stream: TMemoryStream;
begin
  MapLevel := TMapLevel(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE MapLevel SET Picture = :Picture WHERE ID = :ID';
    Query.ParamByName('ID').Value := MapLevel.ID;

    Stream := TMemoryStream.Create;
    try
      MapLevel.Picture.SaveToStream(Stream);
      Stream.Position := 0;
      Query.ParamByName('Picture').LoadFromStream(Stream, ftBlob);
    finally
      Stream.Free;
    end;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
