unit ME.DAO.Layer;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, MemDS, DBAccess, Uni,
  ME.DB.Entity, ME.DB.DAO, ME.DB.Layer;

type
  TLayerDAO = class(TDAOCommon)
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

{ TLayerDAO }

function TLayerDAO.EntityClass: TEntityClass;
begin
  Result := TLayer;
end;

function TLayerDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + TLayer.FieldList + ' FROM Layer WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Layer: TLayer;
begin
  Layer := TLayer(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Layer (MapID, Level, Name) VALUES (:MapID, :Level, :Name)';
    Query.ParamByName('MapID').Value := Layer.MapID;
    Query.ParamByName('Level').AsInteger := Layer.Level;
    Query.ParamByName('Name').AsString := Layer.Name;
    Query.Execute;
    Layer.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  Layer: TLayer;
begin
  Layer := TLayer(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE Layer SET ' +
      '    MapID = :MapID, ' +
      '    Level = :Level, ' +
      '    Name = :Name ' +
      'WHERE ID = :ID';
    Query.ParamByName('ID').Value := Layer.ID;
    Query.ParamByName('MapID').AsInteger := Layer.MapID;
    Query.ParamByName('Level').AsInteger := Layer.Level;
    Query.ParamByName('Name').AsString := Layer.Name;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.LoadPicture(const Entity: TEntity);
var
  Query: TUniQuery;
  Layer: TLayer;
  Stream: TMemoryStream;
begin
  Layer := TLayer(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, Picture FROM Layer WHERE ID = :ID';
    Query.ParamByName('ID').Value := Layer.ID;
    Query.Open;

    Stream := TMemoryStream.Create;
    try
      TBlobField(Query.FieldByName('Picture')).SaveToStream(Stream);
      Stream.Position := 0;
      Layer.Picture.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.SavePicture(const Entity: TEntity);
var
  Query: TUniQuery;
  Layer: TLayer;
  Stream: TMemoryStream;
begin
  Layer := TLayer(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE Layer SET Picture = :Picture WHERE ID = :ID';
    Query.ParamByName('ID').Value := Layer.ID;

    if Layer.Picture.IsEmpty then
      Query.ParamByName('Picture').Value := Null
    else begin
      Stream := TMemoryStream.Create;
      try
        Layer.Picture.SaveToStream(Stream);
        Stream.Position := 0;
        Query.ParamByName('Picture').LoadFromStream(Stream, ftBlob);
      finally
        Stream.Free;
      end;
    end;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
