unit TestLocalMap;

interface

uses
  System.Classes, System.SysUtils, System.Variants, TestFramework, Data.DB,
  ME.DB.Entity, ME.Point, ME.LocalMap, ME.MapLevel;

type
  TTestLocalMap = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertLocalMap;
    procedure TestUpdateLocalMap;
    procedure TestRemoveLocalMap;
    procedure TestRemoveAllLocalMap;
    procedure TestSavePictureMap;
  end;

implementation

uses
  TestUtils, TestData, ME.DB.Utils, ME.AppService, ME.PointService,
  ME.LocalMapService, ME.MapLevelService;

{ TTestLocalMap }

procedure TTestLocalMap.SetUp;
begin
  inherited;

  Randomize;
  AppService := TMEService.Create(nil);
  AppService.Connect;
end;

procedure TTestLocalMap.TearDown;
begin
  inherited;

  FreeAndNil(AppService);
end;

procedure TTestLocalMap.TestInsertLocalMap;
var
  Expected, Actual: TLocalMap;
begin
  Expected := TLocalMap.Create;
  try
    WoodsMap.AssignTo(Expected);
    LocalMapService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    Actual := TLocalMap.Create;
    try
      LocalMapService.GetAt(Expected.ID, Actual);
      CheckTrue(CompareLocalMap(Expected, Actual));
    finally
      Actual.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TTestLocalMap.TestUpdateLocalMap;
var
  Expected: TLocalMap;
  Actual: TLocalMap;
begin
  Expected := TLocalMap.Create;
  try
    WoodsMap.AssignTo(Expected);
    LocalMapService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    CustomsMap.AssignTo(Expected);
    LocalMapService.Update(Expected);

    Actual := TLocalMap.Create;
    try
      LocalMapService.GetAt(Expected.ID, Actual);
      CheckTrue(CompareLocalMap(Expected, Actual));
    finally
      Actual.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TTestLocalMap.TestRemoveLocalMap;
var
  Expected: TLocalMap;
  Actual: TLocalMap;
begin
  Expected := TLocalMap.Create;
  try
    InterchangeMap.AssignTo(Expected);
//    Expected.Name := 'Interchange';
//    Expected.Left.SetBounds(533, -477);
//    Expected.Right.SetBounds(-367, 423);
    LocalMapService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    LocalMapService.Remove(Expected);

    Actual := TLocalMap.Create;
    try
      LocalMapService.GetAt(Expected.ID, Actual);
      CheckTrue(IsNullID(Actual.ID));

      PointService.GetAt(Expected.Left.ID, Actual.Left);
      CheckTrue(IsNullID(Actual.Left.ID));

      PointService.GetAt(Expected.Right.ID, Actual.Right);
      CheckTrue(IsNullID(Actual.Right.ID));
    finally
      Actual.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TTestLocalMap.TestRemoveAllLocalMap;
const
  Count = 10;
var
  LocalMap: TLocalMap;
  i: Integer;
begin
  LocalMap := TLocalMap.Create;
  try
    for i := 0 to Count - 1 do begin
      LocalMap.Name := 'Map #' + IntToStr(i);
      LocalMap.Left.SetBounds(Random(1000), Random(1000));
      LocalMap.Right.SetBounds(Random(1000), Random(1000));
      LocalMapService.Insert(LocalMap);
      CheckFalse(IsNullID(LocalMap.ID));
    end;

    LocalMapService.RemoveAll;
    CheckTrue(LocalMapService.RecordCount = 0);
  finally
    LocalMap.Free;
  end;
end;

procedure TTestLocalMap.TestSavePictureMap;
var
  Expected, Actual: TLocalMap;
  Level: TMapLevel;
begin
  Expected := TLocalMap.Create;
  try
    FactoryMap.AssignTo(Expected);
    LocalMapService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    for Level in Expected.Levels do
      MapLevelService.SavePicture(Level);

//    Actual := TLocalMap.Create;
//    try
//      LocalMapService.GetAt(Expected.ID, Actual);
//      CheckTrue(CompareLocalMap(Expected, Actual));
//    finally
//      Actual.Free;
//    end;
  finally
    Expected.Free;
  end;
end;

initialization
  RegisterTest(TTestLocalMap.Suite);

end.
