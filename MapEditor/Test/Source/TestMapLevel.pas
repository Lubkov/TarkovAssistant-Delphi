unit TestMapLevel;

interface

uses
  System.Classes, System.SysUtils, System.Variants, TestFramework, Data.DB,
  ME.DB.Entity, ME.Point, ME.LocalMap, ME.MapLevel;

type
  TTestMapLevel = class(TTestCase)
  private
    procedure InsertLocalMap(const LocalMap: TLocalMap);
    procedure InsertMapLevel(const MapID: Variant; const MapLevel: TMapLevel);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertMapLevel;
    procedure TestUpdateMapLevel;
    procedure TestRemoveMapLevel;
  end;

implementation

uses
  TestUtils, TestData, ME.DB.Utils, ME.AppService, ME.LocalMapService,
  ME.PointService, ME.MapLevelService;

{ TTestMapLevel }

procedure TTestMapLevel.SetUp;
begin
  inherited;

  AppService := TMEService.Create(nil);
  AppService.Connect;
end;

procedure TTestMapLevel.TearDown;
begin
  inherited;

  FreeAndNil(AppService);
end;

procedure TTestMapLevel.InsertLocalMap(const LocalMap: TLocalMap);
begin
  InterchangeMap.AssignTo(LocalMap);
  LocalMapService.Insert(LocalMap);
  CheckFalse(IsNullID(LocalMap.ID));
end;

procedure TTestMapLevel.InsertMapLevel(const MapID: Variant; const MapLevel: TMapLevel);
begin
  MapLevel.MapID := MapID;
  MapLevel.Level := MainMapLevelIndex;
  MapLevelService.Insert(MapLevel);
  CheckFalse(IsNullID(MapLevel.ID));
end;

procedure TTestMapLevel.TestInsertMapLevel;
var
  LocalMap: TLocalMap;
  Expected: TMapLevel;
  Actual: TMapLevel;
begin
  LocalMap := TLocalMap.Create;
  try
    InsertLocalMap(LocalMap);

    Expected := TMapLevel.Create;
    try
      InsertMapLevel(LocalMap.ID, Expected);
      // add new map
      InsertLocalMap(LocalMap);

      Expected.MapID := LocalMap.ID;
      Expected.Level := Expected.Level + 1;
      MapLevelService.Update(Expected);

      Actual := TMapLevel.Create;
      try
        MapLevelService.GetAt(Expected.ID, Actual);
        CheckTrue(CompareMapLevel(Expected, Actual));
      finally
        Actual.Free;
      end;
    finally
      Expected.Free;
    end;
  finally
    LocalMap.Free;
  end;
end;

procedure TTestMapLevel.TestUpdateMapLevel;
var
  LocalMap: TLocalMap;
  Expected: TMapLevel;
  Actual: TMapLevel;
begin
  LocalMap := TLocalMap.Create;
  try
    InsertLocalMap(LocalMap);

    Expected := TMapLevel.Create;
    try
      InsertMapLevel(LocalMap.ID, Expected);

      Actual := TMapLevel.Create;
      try
        MapLevelService.GetAt(Expected.ID, Actual);
        CheckTrue(CompareMapLevel(Expected, Actual));
      finally
        Actual.Free;
      end;
    finally
      Expected.Free;
    end;
  finally
    LocalMap.Free;
  end;
end;

procedure TTestMapLevel.TestRemoveMapLevel;
var
  LocalMap: TLocalMap;
  Expected: TMapLevel;
  Actual: TMapLevel;
begin
  LocalMap := TLocalMap.Create;
  try
    InsertLocalMap(LocalMap);

    Expected := TMapLevel.Create;
    try
      InsertMapLevel(LocalMap.ID, Expected);
      MapLevelService.Remove(Expected);

      Actual := TMapLevel.Create;
      try
        MapLevelService.GetAt(Expected.ID, Actual);
        CheckTrue(IsNullID(Actual.ID));
      finally
        Actual.Free;
      end;
    finally
      Expected.Free;
    end;
  finally
    LocalMap.Free;
  end;
end;

initialization
  RegisterTest(TTestMapLevel.Suite);

end.
