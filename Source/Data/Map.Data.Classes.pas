unit Map.Data.Classes;

interface

uses
  System.SysUtils, System.Classes, System.Variants, System.Rtti, System.TypInfo,
  System.SysConst, System.JSON, Generics.Collections, Map.Data.Types,
  ME.DB.Map, ME.DB.Layer, ME.DB.Quest, ME.DB.Marker, ME.DB.Resource,
  ME.DB.QuestItem;

type
  TJSONDataImport = class(TObject)
  private
    class procedure LoadLayers(const Source: TJSONValue; Items: TList<TLayer>);
    class procedure LoadMarkers(const Source: TJSONValue; Items: TList<TMarker>);
    class procedure LoadQuestItems(const Source: TJSONValue; Items: TList<TQuestItem>);
    class procedure LoadMarkerImages(const Source: TJSONValue; Items: TObjectList<TResource>);
    class procedure LoadQuests(const Source: TJSONValue; Items: TList<TQuest>);
  public
    class procedure Load(const Data: string; Items: TList<TMap>);
    class procedure LoadFromFile(const FileName: string; Items: TList<TMap>);
  end;

  TJSONDataExport = class(TObject)
  private
    class procedure SaveLayers(const Root: TJSONObject; Items: TList<TLayer>);
    class procedure SaveMarkers(const Root: TJSONObject; Items: TList<TMarker>);
    class procedure SaveQuestItems(const Root: TJSONObject; Items: TList<TQuestItem>);
    class procedure SaveMarkerImages(const Root: TJSONObject; Items: TObjectList<TResource>);
    class procedure SaveQuests(const Root: TJSONObject; Items: TList<TQuest>);
  public
    class function Save(Items: TList<TMap>): string;
    class procedure SaveToFile(const FileName: string; Items: TList<TMap>);
  end;

  TDBDataImport = class
  private
    class procedure SaveLayers(const MapID: Variant; const Items: TList<TLayer>);
    class procedure SaveQuests(const MapID: Variant; const Items: TList<TQuest>);
    class procedure SaveMarkers(const MapID, QuestID: Variant; const Items: TList<TMarker>);
    class function SaveResource(const MarkerID: Variant; const Source: TResource): Variant;
    class procedure SaveResources(const MarkerID: Variant; const Items: TList<TResource>);
    class procedure SaveQuestItems(const MarkerID: Variant; const Items: TList<TQuestItem>);
  public
    class procedure Load(const Items: TList<TMap>);
  end;

implementation

uses
  App.Service, Map.Data.Service, ME.Service.Map, ME.Service.Layer, ME.Service.Quest, 
  ME.Service.Marker, ME.Service.Resource, ME.Service.QuestItem;

class procedure TJSONDataImport.LoadLayers(const Source: TJSONValue; Items: TList<TLayer>);
var
  i: Integer;
  List: TJSONArray;
  Layer: TLayer;
begin
  if not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    Layer := TLayer.Create;
    try
      Layer.Assign(TJSONObject(List.Items[i]));
    finally
      Items.Add(Layer);
    end;
  end;
end;

class procedure TJSONDataImport.LoadMarkers(const Source: TJSONValue; Items: TList<TMarker>);
var
  i: Integer;
  List: TJSONArray;
  JSONObject: TJSONValue;
  Marker: TMarker;
begin
  if not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    JSONObject := TJSONObject(List.Items[i]);

    Marker := TMarker.Create;
    try
      Marker.Assign(JSONObject);
      LoadQuestItems(JSONObject.FindValue('items'), Marker.Items);
      LoadMarkerImages(JSONObject.FindValue('images'), Marker.Images);
    finally
      Items.Add(Marker);
    end;
  end;
end;

class procedure TJSONDataImport.LoadQuestItems(const Source: TJSONValue; Items: TList<TQuestItem>);
var
  i: Integer;
  List: TJSONArray;
  JSONObject: TJSONValue;
  QuestItem: TQuestItem;
begin
  if (Source = nil) or not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    JSONObject := TJSONObject(List.Items[i]);

    QuestItem := TQuestItem.Create;
    try
      QuestItem.Assign(JSONObject);
    finally
      Items.Add(QuestItem);
    end;
  end;
end;

class procedure TJSONDataImport.LoadMarkerImages(const Source: TJSONValue; Items: TObjectList<TResource>);
var
  i: Integer;
  List: TJSONArray;
  JSONObject: TJSONValue;
  Image: TResource;
begin
  if not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    JSONObject := TJSONObject(List.Items[i]);

    Image := TResource.Create;
    try
      Image.Assign(JSONObject);
    finally
      Items.Add(Image);
    end;
  end;
end;

class procedure TJSONDataImport.LoadQuests(const Source: TJSONValue; Items: TList<TQuest>);
var
  i: Integer;
  List: TJSONArray;
  JSONObject: TJSONValue;
  Quest: TQuest;
begin
  if not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    JSONObject := TJSONObject(List.Items[i]);

    Quest := TQuest.Create;
    try
      Quest.Assign(JSONObject);
      LoadMarkers(JSONObject.FindValue('markers'), Quest.Markers);
    finally
      Items.Add(Quest);
    end;
  end;
end;

class procedure TJSONDataImport.Load(const Data: string; Items: TList<TMap>);
var
  Root: TJSONArray;
  JSONObject: TJSONValue;
  Map: TMap;
  i: Integer;
begin
  JSONObject := TJSONObject.ParseJSONValue(Data);
  if not (JSONObject is TJSONArray) then
    Exit;

  Root := TJSONArray(JSONObject);
  try
    for i := 0 to Root.Count - 1 do begin
      JSONObject := TJSONObject(Root.Items[i]);

      Map := TMap.Create;
      try
        Map.Assign(JSONObject);
        LoadLayers(JSONObject.FindValue('layers'), Map.Layers);
        LoadMarkers(JSONObject.FindValue('markers'), Map.Markers);
        LoadQuests(JSONObject.FindValue('quests'), Map.Quests);
      finally
        Items.Add(Map);
      end;
    end;
  finally
    Root.Free;
  end;
end;

class procedure TJSONDataImport.LoadFromFile(const FileName: string; Items: TList<TMap>);
var
  Data: TStrings;
begin
  Data := TStringList.Create;
  try
    Data.LoadFromFile(FileName, TEncoding.UTF8);
    Load(Data.Text, Items);
  finally
    Data.Free;
  end;
end;

{ TJSONDataExport }

class procedure TJSONDataExport.SaveLayers(const Root: TJSONObject; Items: TList<TLayer>);
var
  Layer: TLayer;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for Layer in Items do begin
    JSONObject := TJSONObject.Create;
    try
      Layer.AssignTo(JSONObject);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;
  Root.AddPair('layers', JSONItems);
end;

class procedure TJSONDataExport.SaveMarkers(const Root: TJSONObject; Items: TList<TMarker>);
var
  Marker: TMarker;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for Marker in Items do begin
    JSONObject := TJSONObject.Create;
    try
      Marker.AssignTo(JSONObject);

      SaveQuestItems(JSONObject, Marker.Items);
      SaveMarkerImages(JSONObject, Marker.Images);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;
  Root.AddPair('markers', JSONItems);
end;

class procedure TJSONDataExport.SaveQuestItems(const Root: TJSONObject; Items: TList<TQuestItem>);
var
  QuestItem: TQuestItem;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for QuestItem in Items do begin
    JSONObject := TJSONObject.Create;
    try
      QuestItem.AssignTo(JSONObject);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;
  Root.AddPair('items', JSONItems);
end;

class procedure TJSONDataExport.SaveMarkerImages(const Root: TJSONObject; Items: TObjectList<TResource>);
var
  Image: TResource;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for Image in Items do begin
    JSONObject := TJSONObject.Create;
    try
      Image.AssignTo(JSONObject);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;
  Root.AddPair('images', JSONItems);
end;

class procedure TJSONDataExport.SaveQuests(const Root: TJSONObject; Items: TList<TQuest>);
var
  Quest: TQuest;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for Quest in Items do begin
    JSONObject := TJSONObject.Create;
    try
      Quest.AssignTo(JSONObject);
      SaveMarkers(JSONObject, Quest.Markers);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;
  Root.AddPair('quests', JSONItems);
end;

class function TJSONDataExport.Save(Items: TList<TMap>): string;
var
  Root: TJSONArray;
  JSONObject: TJSONObject;
  Map: TMap;
begin
  Root := TJSONArray.Create;
  try
    for Map in Items do begin
      JSONObject := TJSONObject.Create;
      try
        Map.AssignTo(JSONObject);
        SaveLayers(JSONObject, Map.Layers);
        SaveMarkers(JSONObject, Map.Markers);
        SaveQuests(JSONObject, Map.Quests);
      finally
        Root.Add(JSONObject);
      end;
    end;

    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

class procedure TJSONDataExport.SaveToFile(const FileName: string; Items: TList<TMap>);
var
  Data: TStrings;
begin
  Data := TStringList.Create;
  try
    Data.Text := Save(Items);
    Data.SaveToFile(FileName, TEncoding.UTF8)
  finally
    Data.Free;
  end;
end;

{ TDBDataImport }

class procedure TDBDataImport.Load(const Items: TList<TMap>);
var
  Source: TMap;
  Map: TDBMap;
begin
  MapService.StartTransaction;
  try
    for Source in Items do begin
      Map := TDBMap.Create;
      try
        Map.Caption := Source.Caption;
        Map.Left := Source.Left;
        Map.Top := Source.Top;
        Map.Right := Source.Right;
        Map.Bottom := Source.Bottom;
        DataService.LoadImage(Source, Map.Picture);
    
        MapService.Save(Map);
        SaveLayers(Map.ID, Source.Layers);
        SaveMarkers(Map.ID, Null, Source.Markers);
        SaveQuests(Map.ID, Source.Quests);
      finally
        Map.Free;
      end;           
    end;

    MapService.CommitTransaction;
  except
    MapService.RollbackTransaction;
    raise;
  end;
end;

class procedure TDBDataImport.SaveLayers(const MapID: Variant; const Items: TList<TLayer>);
var
  Source: TLayer;
  Layer: TDBLayer;
begin
  for Source in Items do begin
    Layer := TDBLayer.Create;
    try
      Layer.MapID := MapID;
      Layer.Level := Source.Level;
      Layer.Name := Source.Caption;
      DataService.LoadImage(Source, Layer.Picture);

      LayerService.Save(Layer);
    finally
      Layer.Free;
    end;
  end;
end;

class procedure TDBDataImport.SaveQuests(const MapID: Variant; const Items: TList<TQuest>);
var
  Source: TQuest;
  Quest: TDBQuest;
begin
  for Source in Items do begin
    Quest := TDBQuest.Create;
    try
      Quest.MapID := MapID;
      Quest.Name := Source.Caption;
      Quest.Trader := TTrader(Ord(Source.Trader));

      QuestService.Save(Quest);
      SaveMarkers(MapID, Quest.ID, Source.Markers);
    finally
      Quest.Free;
    end;
  end;
end;

class procedure TDBDataImport.SaveMarkers(const MapID, QuestID: Variant; const Items: TList<TMarker>);
var
  Source: TMarker;
  Marker: TDBMarker;
begin
  for Source in Items do begin
    Marker := TDBMarker.Create;
    try
      Marker.MapID := MapID;
      Marker.QuestID := QuestID;
      Marker.Caption := Source.Caption;
      Marker.Kind := TMarkerKind(Source.Kind);
      Marker.Left := Source.Left;
      Marker.Top := Source.Top;

      MarkerService.Save(Marker);

      SaveResources(Marker.ID, Source.Images);
      SaveQuestItems(Marker.ID, Source.Items);
    finally
      Marker.Free;
    end;
  end;
end;

class function TDBDataImport.SaveResource(const MarkerID: Variant; const Source: TResource): Variant;
var
  Resource: TDBResource;
begin
  Resource := TDBResource.Create;
  try
    Resource.MarkerID := MarkerID;
    if VarIsNull(MarkerID) or VarIsEmpty(MarkerID) then
      Resource.Kind := TResourceKind.QuestItem
    else
      Resource.Kind := TResourceKind.Screenshot;

    Resource.Description := Source.Description;
    DataService.LoadImage(Source, Resource.Picture);

    ResourceService.Save(Resource);
    Result := Resource.ID;
  finally
    Resource.Free;
  end;
end;

class procedure TDBDataImport.SaveResources(const MarkerID: Variant; const Items: TList<TResource>);
var
  Resource: TResource;
begin
  for Resource in Items do
    SaveResource(MarkerID, Resource);
end;

class procedure TDBDataImport.SaveQuestItems(const MarkerID: Variant; const Items: TList<TQuestItem>);
var
  Resource: TResource;
  ResourceID: Variant;
  QuestItem: TDBQuestItem;
begin
  for Resource in Items do begin
    ResourceID := SaveResource(Null, Resource);

    QuestItem := TDBQuestItem.Create;
    try
      QuestItem.ResourceID := ResourceID;
      QuestItem.MarkerID := MarkerID;

      QuestItemService.Save(QuestItem);
    finally
      QuestItem.Free;
    end;
  end;
end;

end.
