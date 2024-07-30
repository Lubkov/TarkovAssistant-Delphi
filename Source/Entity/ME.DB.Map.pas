unit ME.DB.Map;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics, Generics.Collections,
  Data.DB, ME.DB.Entity, ME.DB.Layer, ME.DB.Marker, ME.DB.Quest;

type
  TDBMap = class;
  TDBMapChangedEvent = procedure(const MapID: Variant) of object;

  TDBMap = class(TEntity)
  private
    FCaption: string;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FPicture: TBitmap;
    FLayers: TList<TDBLayer>;
//    FTags: TList<TMarker>;
//    FQuests: TList<TQuest>;

    procedure SetPicture(const Value: TBitmap);
//    function GetMainLayer: TLayer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

//    procedure ClearLevelList;
//    procedure ClearTagList;
//    procedure ClearQuestList;

    property Caption: string read FCaption write FCaption;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
    property Picture: TBitmap read FPicture write SetPicture;
    property Layers: TList<TDBLayer> read FLayers;
//    property Tags: TList<TMarker> read FTags;
//    property Quests: TList<TQuest> read FQuests;
//    property MainLayer: TLayer read GetMainLayer;
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
//  FTags := TList<TMarker>.Create;
//  FQuests := TList<TQuest>.Create;
end;

destructor TDBMap.Destroy;
begin
  FPicture.Free;
  FLayers.Free;

//  ClearTagList;
//  FTags.Free;
//
//  ClearQuestList;
//  FQuests.Free;

  inherited;
end;

procedure TDBMap.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

//function TDBMap.GetMainLayer: TLayer;
//var
//  Layer: TLayer;
//begin
//  Result := nil;
//  for Layer in Layers do
//    if Layer.IsMainLevel then begin
//      Result := Layer;
//      Exit;
//    end;
//end;

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

//procedure TDBMap.ClearLevelList;
//var
//  i: Integer;
//begin
//  for i := 0 to FLayers.Count - 1 do
//    FLayers[i].Free;
//
//  FLayers.Clear;
//end;
//
//procedure TDBMap.ClearTagList;
//var
//  i: Integer;
//begin
//  for i := 0 to FTags.Count - 1 do
//    FTags[i].Free;
//
//  FTags.Clear;
//end;
//
//procedure TDBMap.ClearQuestList;
//var
//  i: Integer;
//begin
//  for i := 0 to FQuests.Count - 1 do
//    FQuests[i].Free;
//
//  FQuests.Clear;
//end;

end.
