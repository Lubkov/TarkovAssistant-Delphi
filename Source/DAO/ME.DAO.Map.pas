unit ME.DAO.Map;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections, Data.DB,
  MemDS, DBAccess, Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Map, ME.DB.Layer;

type
  TMapDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TEntity): Boolean; override;
    procedure GetAll(const Items: TList<TEntity>); override;
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;

    procedure LoadLayers(const Entity: TEntity; LoadPicture: Boolean);
    procedure RemoveLayers(const MapID: Variant);
  end;

implementation

const
  SqlSelectCommandText =
    ' SELECT ' +
    '     m.ID as ID, ' +
    '     m.Caption as Caption, ' +
    '     m.Left as Left, ' +
    '     m.Top as Top, ' +
    '     m.Right as Right, ' +
    '     m.Bottom as Bottom, ' +
    '     m.Picture as Picture ' +
    ' FROM Map m ' +
    ' %s ';

{ TMapDAO }

function TMapDAO.EntityClass: TEntityClass;
begin
  Result := TDBMap;
end;

function TMapDAO.GetAt(ID: Integer; const Entity: TEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [' WHERE (m.ID = :ID) ']);
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TMapDAO.GetAll(const Items: TList<TEntity>);
var
  Query: TUniQuery;
  Entity: TEntity;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, ['']);
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

procedure TMapDAO.Insert(const Entity: TEntity);
var
  Query: TUniQuery;
  Map: TDBMap;
  Param: TParam;
begin
  Map := TDBMap(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO ' + TDBMap.EntityName +
      '   (Caption, Left, Top, Right, Bottom, Picture) ' +
      ' VALUES (:Caption, :Left, :Top, :Right, :Bottom, :Picture) ';
    Query.ParamByName('Caption').AsString := Map.Caption;
    Query.ParamByName('Left').AsInteger := Map.Left;
    Query.ParamByName('Top').AsInteger := Map.Top;
    Query.ParamByName('Right').AsInteger := Map.Right;
    Query.ParamByName('Bottom').AsInteger := Map.Bottom;
    Param := Query.ParamByName('Picture');
    Map.AssignPictureTo(Map.Picture, Param);

    Query.Execute;
    Map.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TMapDAO.Update(const Entity: TEntity);
var
  Query: TUniQuery;
  Map: TDBMap;
begin
  Map := TDBMap(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' UPDATE ' + TDBMap.EntityName +
      ' SET ' +
      '   Caption = :Caption, ' +
      '   Left = :Left, ' +
      '   Top = :Top, ' +
      '   Right = :Right, ' +
      '   Bottom = :Bottom, ' +
      '   Picture = :Picture ' +
      ' WHERE ID = :ID ';
    Query.ParamByName('ID').Value := Map.ID;
    Query.ParamByName('Caption').AsString := Map.Caption;
    Query.ParamByName('Left').AsInteger := Map.Left;
    Query.ParamByName('Top').AsInteger := Map.Top;
    Query.ParamByName('Right').AsInteger := Map.Right;
    Query.ParamByName('Bottom').AsInteger := Map.Bottom;
    Map.AssignPictureTo(Map.Picture, Query.ParamByName('Picture'));
    Query.Execute;
  finally
    Query.Free;
  end;
end;

procedure TMapDAO.LoadLayers(const Entity: TEntity; LoadPicture: Boolean);
const
  PictureFileName = 'Picture';
//var
//  Query: TUniQuery;
//  Map: TDBMap;
//  Level: TDBLayer;
//  FieldNames: string;
begin
//  Map := TDBMap(Entity);
//  FieldNames := TDBLayer.FieldList;
//  if LoadPicture then
//    FieldNames := FieldNames + ', ' + PictureFileName;
//
//  Query := TUniQuery.Create(nil);
//  try
//    Query.Connection := Connection;
//    Query.SQL.Text := 'SELECT ' + FieldNames + ' FROM ' + TDBLayer.EntityName + ' WHERE MapID = :MapID';
//    Query.ParamByName('MapID').Value := Entity.ID;
//    Query.Open;
//
//    Map.ClearLevelList;
//    while not Query.Eof do begin
//      Level := TDBLayer.Create;
//      try
//        Level.Assign(Query);
//      finally
//        Map.Layers.Add(Level);
//      end;
//
//      Query.Next;
//    end;
//  finally
//    Query.Free;
//  end;
end;

procedure TMapDAO.RemoveLayers(const MapID: Variant);
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM ' + TDBLayer.EntityName + ' WHERE MapID = :MapID';
    Query.ParamByName('MapID').Value := MapID;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
