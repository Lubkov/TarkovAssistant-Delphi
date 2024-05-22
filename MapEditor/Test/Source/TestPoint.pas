unit TestPoint;

interface

uses
  System.Classes, System.SysUtils, System.Variants, TestFramework, Data.DB,
  ME.DB.Entity, ME.Point, ME.PointService, ME.Connection, ME.DB.Utils;

type
  TTestPoint = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertPoint;
    procedure TestUpdatePoint;
    procedure TestRemovePoint;
    procedure TestRemoveAllPoint;
  end;

implementation

uses
  TestUtils, TestData, ME.AppService;

procedure TTestPoint.SetUp;
begin
  inherited;

  Randomize;
  AppService := TMEService.Create(nil);
  AppService.Connect;
end;

procedure TTestPoint.TearDown;
begin
  inherited;

  FreeAndNil(AppService);
end;

procedure TTestPoint.TestInsertPoint;
var
  Expected: TPoint;
  Actual: TPoint;
begin
  Expected := TPoint.Create;
  try
    PointTestData[0].AssignTo(Expected);
    PointService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    Actual := TPoint.Create;
    try
      PointService.GetAt(Expected.ID, Actual);
      CheckTrue(ComparePoint(Expected, Actual));
    finally
      Actual.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TTestPoint.TestUpdatePoint;
var
  Expected: TPoint;
  Actual: TPoint;
begin
  Expected := TPoint.Create;
  try
    PointTestData[0].AssignTo(Expected);
    PointService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    PointTestData[1].AssignTo(Expected);
    PointService.Update(Expected);

    Actual := TPoint.Create;
    try
      PointService.GetAt(Expected.ID, Actual);
      CheckTrue(ComparePoint(Expected, Actual));
    finally
      Actual.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TTestPoint.TestRemovePoint;
var
  Expected: TPoint;
  Actual: TPoint;
begin
  Expected := TPoint.Create;
  try
    PointTestData[2].AssignTo(Expected);
    PointService.Insert(Expected);
    CheckFalse(IsNullID(Expected.ID));

    PointService.Remove(Expected);

    Actual := TPoint.Create;
    try
      PointService.GetAt(Expected.ID, Actual);
      CheckTrue(IsNullID(Actual.ID));
    finally
      Actual.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TTestPoint.TestRemoveAllPoint;
var
  Point: TPoint;
  i: Integer;
begin
  Point := TPoint.Create;
  try
    for i := 0 to Length(PointTestData) - 1 do begin
      PointTestData[i].AssignTo(Point);
      PointService.Insert(Point);
      CheckFalse(IsNullID(Point.ID));
    end;

    PointService.RemoveAll;
    CheckTrue(PointService.RecordCount = 0);
  finally
    Point.Free;
  end;
end;

initialization
  RegisterTest(TTestPoint.Suite);

end.

