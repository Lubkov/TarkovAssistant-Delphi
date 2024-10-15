unit ME.Service.QuestTracker;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Rtti, System.TypInfo,
  System.SysConst, System.JSON, Generics.Collections, ME.QuestTracker;

type
  TQuestTrackerService = class(TObject)
  private
  public
    procedure Load(const Source: TJSONValue; Items: TList<TQuestTracker>);
    procedure Save(const Root: TJSONObject; Items: TList<TQuestTracker>);
  end;

var
  QuestTrackerService: TQuestTrackerService;

implementation

uses
  App.Service;

{ TQuestTrackerService }

procedure TQuestTrackerService.Load(const Source: TJSONValue; Items: TList<TQuestTracker>);
var
  i: Integer;
  List: TJSONArray;
  JSONObject: TJSONValue;
  QuestTracker: TQuestTracker;
begin
  if not (Source is TJSONArray) then
    Exit;

  List := TJSONArray(Source);
  for i := 0 to List.Count - 1 do begin
    JSONObject := TJSONObject(List.Items[i]);

    QuestTracker := TQuestTracker.Create;
    try
      QuestTracker.Assign(JSONObject);
    finally
      Items.Add(QuestTracker);
    end;
  end;
end;

procedure TQuestTrackerService.Save(const Root: TJSONObject; Items: TList<TQuestTracker>);
var
  QuestTracker: TQuestTracker;
  JSONItems: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONItems := TJSONArray.Create;
  for QuestTracker in Items do begin
    JSONObject := TJSONObject.Create;
    try
      QuestTracker.AssignTo(JSONObject);
    finally
      JSONItems.Add(JSONObject);
    end;
  end;

  Root.AddPair('markers', JSONItems);
end;

end.
