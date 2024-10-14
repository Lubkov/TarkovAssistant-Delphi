unit ME.DAO.Layer;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections,
  FMX.Graphics, Data.DB, MemDS, DBAccess, Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Layer;

type
  TLayerDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TDBEntityClass; override;
    function GetSqlSelectCommandText: string; override;
  public
    function GetAt(ID: Integer; const Entity: TDBEntity): Boolean; override;
    procedure GetMapLayers(const MapID: Variant; const Items: TList<TDBEntity>; LoadPicture: Boolean);
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
    procedure LoadPicture(const LayerID: Variant; const Picture: TBitmap); overload;
    procedure LoadPicture(const Entity: TDBEntity); overload;
    procedure SavePicture(const Entity: TDBEntity);
  end;

implementation

uses
  ME.DB.Utils;

{ TLayerDAO }

function TLayerDAO.EntityClass: TDBEntityClass;
begin
  Result := TDBLayer;
end;

function TLayerDAO.GetSqlSelectCommandText: string;
begin
  Result := 'SELECT ' + TDBLayer.FieldList + ' FROM Layer %s';
end;

function TLayerDAO.GetAt(ID: Integer; const Entity: TDBEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + TDBLayer.FieldList + ' FROM Layer WHERE ID = :ID';
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.GetMapLayers(const MapID: Variant; const Items: TList<TDBEntity>; LoadPicture: Boolean);
var
  Query: TUniQuery;
  Entity: TDBEntity;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ' + TDBLayer.FieldList + ', Picture FROM Layer WHERE MapID = :MapID';
    Query.ParamByName('MapID').Value := MapID;
    Query.Open;

    while not Query.Eof do begin
      Entity := EntityClass.Create;
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

procedure TLayerDAO.Insert(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Layer: TDBLayer;
  Param: TParam;
begin
  Layer := TDBLayer(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'INSERT INTO Layer (MapID, Level, Name, Picture) VALUES (:MapID, :Level, :Name, :Picture)';
    Query.ParamByName('MapID').Value := Layer.MapID;
    Query.ParamByName('Level').AsInteger := Layer.Level;
    Query.ParamByName('Name').AsString := Layer.Name;
    Param := Query.ParamByName('Picture');
    Layer.AssignPictureTo(Layer.Picture, Param);
    Query.Execute;
    Layer.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.Update(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Layer: TDBLayer;
begin
  Layer := TDBLayer(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      'UPDATE Layer SET ' +
      '    MapID = :MapID, ' +
      '    Level = :Level, ' +
      '    Name = :Name, ' +
      '    Picture = :Picture ' +
      'WHERE ID = :ID';
    Query.ParamByName('ID').Value := Layer.ID;
    Query.ParamByName('MapID').AsInteger := Layer.MapID;
    Query.ParamByName('Level').AsInteger := Layer.Level;
    Query.ParamByName('Name').AsString := Layer.Name;
    Layer.AssignPictureTo(Layer.Picture, Query.ParamByName('Picture'));
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.LoadPicture(const LayerID: Variant; const Picture: TBitmap);
var
  Query: TUniQuery;
  Stream: TMemoryStream;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT ID, Picture FROM Layer WHERE ID = :ID';
    Query.ParamByName('ID').Value := LayerID;
    Query.Open;

    Stream := TMemoryStream.Create;
    try
      TBlobField(Query.FieldByName('Picture')).SaveToStream(Stream);
      Stream.Position := 0;
      Picture.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    Query.Free;
  end;
end;

procedure TLayerDAO.LoadPicture(const Entity: TDBEntity);
var
  Layer: TDBLayer;
begin
  Layer := TDBLayer(Entity);
  LoadPicture(Layer.ID, Layer.Picture);
end;

procedure TLayerDAO.SavePicture(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Layer: TDBLayer;
  Stream: TMemoryStream;
begin
  Layer := TDBLayer(Entity);

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
