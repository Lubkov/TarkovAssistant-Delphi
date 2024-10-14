unit Map.Data.Service;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.IOUtils, FMX.Graphics,
  Generics.Collections, ME.DB.Entity, ME.DB.Map, ME.DB.Layer, ME.DB.Resource;

type
  TDataService = class
  private
    FItems: TList<TDBMap>;

    function GetCount: Integer;
    function GetMapItem(Index: Integer): TDBMap;
    procedure SetMapItem(Index: Integer; const Value: TDBMap);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;

    function GetSourceFileName(const Source: TDBEntity): string;
    procedure LoadImage(const Source: TDBResource; const Dest: TBitmap);
    procedure SaveImage(const Source: TDBEntity; const Dest: TBitmap);
    procedure DeleteImage(const Source: TDBEntity);

    property Items: TList<TDBMap> read FItems;
    property Count: Integer read GetCount;
    property Map[Index: Integer]: TDBMap read GetMapItem write SetMapItem;
  end;

var
  DataService: TDataService;

implementation

uses
  App.Constants, ME.Service.Map, ME.Service.Marker, ME.Service.Layer,
  ME.Service.Quest, ME.Service.Resource;

{ TDataService }

constructor TDataService.Create;
begin
  inherited;

  FItems := TObjectList<TDBMap>.Create;
end;

destructor TDataService.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TDataService.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDataService.GetMapItem(Index: Integer): TDBMap;
begin
  Result := Items[Index];
end;

procedure TDataService.SetMapItem(Index: Integer; const Value: TDBMap);
begin
  Items[Index] := Value;
end;

procedure TDataService.Load;
var
  Map: TDBMap;
begin
  MapService.GetAll(TList<TDBEntity>(FItems));

  for Map in FItems do begin
    MapService.LoadLayers(Map, False);
    MarkerService.LoadMarkers(Map.ID, Map.Markers);
    QuestService.LoadQuests(Map.ID, Map.Quests, True);
  end;
end;

function TDataService.GetSourceFileName(const Source: TDBEntity): string;
//var
//  Folder, Ext: string;
begin
//  if Source is TDBMap then begin
//    Folder := 'Maps';
//    Ext := 'jpg';
//  end
//  else
//  if Source is TLayer then begin
//    Folder := 'Levels';
//    Ext := 'png';
//  end
//  else
//  if Source is TQuestItem then begin
//    Folder := 'Items';
//    Ext := 'png';
//  end
//  else
//  if Source is TResource then begin
//    Folder := 'Markers';
//    Ext := 'jpg';
//  end;
//
//  Result := TPath.Combine(AppParams.DataPath, TPath.Combine(Folder, Source.ID + '.' + Ext));
end;

procedure TDataService.LoadImage(const Source: TDBResource; const Dest: TBitmap);
//var
//  FileName: string;
begin
  ResourceService.LoadPicture(Source, Dest);

//  FileName := GetSourceFileName(Source);
//
//  if FileExists(FileName) then
//    Dest.LoadFromFile(FileName)
//  else
//    Dest.Assign(nil);
end;

procedure TDataService.SaveImage(const Source: TDBEntity; const Dest: TBitmap);
var
  FileName: string;
begin
  FileName := GetSourceFileName(Source);

  if Dest.IsEmpty then
    DeleteImage(Source)
  else
    Dest.SaveToFile(FileName);
end;

procedure TDataService.DeleteImage(const Source: TDBEntity);
var
  FileName: string;
begin
  FileName := GetSourceFileName(Source);

  if FileExists(FileName) then
    TFile.Delete(FileName);
end;

end.
