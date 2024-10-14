unit ME.DAO.Marker;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Generics.Collections, Data.DB,
  MemDS, DBAccess, Uni, ME.DB.Entity, ME.DB.DAO, ME.DB.Marker;

type
  TMarkerDAO = class(TDAOCommon)
  private
  protected
    function EntityClass: TDBEntityClass; override;
  public
    function GetAt(ID: Integer; const Entity: TDBEntity): Boolean; override;
    procedure GetAll(const Items: TList<TDBEntity>); override;
    procedure LoadMarkers(const MapID, QuestID: Variant; const Items: TList<TDBMarker>);
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
  end;

implementation

uses
  ME.DB.Utils;

const
  SqlSelectCommandText =
    ' SELECT ' +
    '     t.ID as ID, ' +
    '     t.MapID as MapID, ' +
    '     t.QuestID as QuestID, ' +
    '     t.Caption as Caption, ' +
    '     t.Kind as Kind, ' +
    '     t.Left as Left, ' +
    '     t.Top as Top ' +
    ' FROM Marker t ' +
    ' %s ';

{ TMarkerDAO }

function TMarkerDAO.EntityClass: TDBEntityClass;
begin
  Result := TDBMarker;
end;

function TMarkerDAO.GetAt(ID: Integer; const Entity: TDBEntity): Boolean;
var
  Query: TUniQuery;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [' WHERE (t.ID = :ID) ']);
    Query.ParamByName('ID').Value := ID;
    Query.Open;

    Result := not Query.Eof;
    if Result then
      Entity.Assign(Query);
  finally
    Query.Free;
  end;
end;

procedure TMarkerDAO.GetAll(const Items: TList<TDBEntity>);
const
  Filter = '';
var
  Query: TUniQuery;
  Entity: TDBEntity;
begin
  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [Filter]);
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

procedure TMarkerDAO.LoadMarkers(const MapID, QuestID: Variant; const Items: TList<TDBMarker>);
//const
//  Filter = ' WHERE (t.MapID = :MapID) ';
var
  Query: TUniQuery;
  Entity: TDBMarker;
  Filter: string;
begin
  if IsNullID(MapID) then
    Filter := ' WHERE (1 = 1) '
  else
    Filter := ' WHERE (t.MapID = :MapID) ';

  if IsNullID(QuestID) then
    Filter := Filter + ' AND (t.QuestID IS NULL) '
  else
    Filter := Filter + ' AND (t.QuestID = :QuestID) ';

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := Format(SqlSelectCommandText, [Filter]);
    if Query.FindParam('MapID') <> nil then
      Query.ParamByName('MapID').Value := MapID;

    if Query.FindParam('QuestID') <> nil then
      Query.ParamByName('QuestID').Value := QuestID;

    Query.Open;

    while not Query.Eof do begin
      Entity := TDBMarker.Create;
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

procedure TMarkerDAO.Insert(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Marker: TDBMarker;
begin
  Marker := TDBMarker(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' INSERT INTO Marker (MapID, QuestID, Caption, Kind, Left, Top) ' +
      ' VALUES (:MapID, :QuestID, :Caption, :Kind, :Left, :Top) ';
    Query.ParamByName('MapID').Value := Marker.MapID;
    Query.ParamByName('QuestID').Value := Marker.QuestID;
    Query.ParamByName('Caption').AsString := Marker.Caption;
    Query.ParamByName('Kind').AsInteger := Ord(Marker.Kind);
    Query.ParamByName('Left').AsInteger := Marker.Left;
    Query.ParamByName('Top').AsInteger := Marker.Top;
    Query.Execute;
    Marker.ID := Query.LastInsertId;
  finally
    Query.Free;
  end;
end;

procedure TMarkerDAO.Update(const Entity: TDBEntity);
var
  Query: TUniQuery;
  Marker: TDBMarker;
begin
  Marker := TDBMarker(Entity);

  Query := TUniQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text :=
      ' UPDATE Marker ' +
      ' SET ' +
      '   MapID = :MapID, ' +
      '   QuestID = :QuestID, ' +
      '   Caption = :Caption, ' +
      '   Kind = :Kind, ' +
      '   Left = :Left, ' +
      '   Top = :Top ' +
      ' WHERE ID = :ID ';
    Query.ParamByName('ID').Value := Marker.ID;
    Query.ParamByName('MapID').Value := Marker.MapID;
    Query.ParamByName('QuestID').Value := Marker.QuestID;
    Query.ParamByName('Caption').AsString := Marker.Caption;
    Query.ParamByName('Kind').AsInteger := Ord(Marker.Kind);
    Query.ParamByName('Left').AsInteger := Marker.Left;
    Query.ParamByName('Top').AsInteger := Marker.Top;
    Query.Execute;
  finally
    Query.Free;
  end;
end;

end.
