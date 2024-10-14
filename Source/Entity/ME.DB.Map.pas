unit ME.DB.Map;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics, Generics.Collections,
  Data.DB, App.Entity, ME.DB.Entity, ME.DB.Layer, ME.DB.Marker, ME.DB.Quest;

type
  TDBMap = class;
  TDBMapChangedEvent = procedure(const MapID: Variant) of object;

  TDBMap = class(TDBEntity)
  private
    FCaption: string;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FPicture: TBitmap;
    FLayers: TList<TDBLayer>;
    FMarkers: TList<TDBMarker>;
    FQuests: TList<TDBQuest>;

    procedure SetPicture(const Value: TBitmap);
    function GetMainLayer: TDBLayer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property Caption: string read FCaption write FCaption;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
    property Picture: TBitmap read FPicture write SetPicture;
    property Layers: TList<TDBLayer> read FLayers;
    property Markers: TList<TDBMarker> read FMarkers;
    property Quests: TList<TDBQuest> read FQuests;
    property MainLayer: TDBLayer read GetMainLayer;
  end;

implementation

{ TDBMap }

constructor TDBMap.Create;
begin
  inherited;

  FCaption := '';
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
  FPicture := TBitmap.Create;
  FLayers := TObjectList<TDBLayer>.Create;
  FMarkers := TObjectList<TDBMarker>.Create;
  FQuests := TObjectList<TDBQuest>.Create;
end;

destructor TDBMap.Destroy;
begin
  FPicture.Free;
  FLayers.Free;
  FQuests.Free;
  FMarkers.Free;

  inherited;
end;

procedure TDBMap.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

function TDBMap.GetMainLayer: TDBLayer;
var
  Layer: TDBLayer;
begin
  Result := nil;
  for Layer in Layers do
    if Layer.IsMainLevel then begin
      Result := Layer;
      Exit;
    end;
end;

procedure TDBMap.Assign(const Source: TEntity);
var
  Map: TDBMap;
begin
  inherited;

  Map := TDBMap(Source);

  FCaption := Map.Caption;
  FLeft := Map.Left;
  FTop := Map.Top;
  FRight := Map.Right;
  FBottom := Map.Bottom;
  Picture := TDBMap(Source).Picture;
end;

procedure TDBMap.Assign(const DataSet: TDataSet);
begin
  inherited;

  FCaption := DataSet.FieldByName('Caption').AsString;
  FLeft := DataSet.FieldByName('Left').AsInteger;
  FTop := DataSet.FieldByName('Top').AsInteger;
  FRight := DataSet.FieldByName('Right').AsInteger;
  FBottom := DataSet.FieldByName('Bottom').AsInteger;

  if DataSet.FindField('Picture') <> nil then
    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TDBMap.EntityName: string;
begin
  Result := 'Map';
end;

class function TDBMap.FieldList: string;
begin
  Result := 'ID, "Caption", "Left", "Top", "Right", "Bottom", "Picture"';
end;

end.
