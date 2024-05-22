unit TestData;

interface

uses
  System.Classes, System.SysUtils, System.Variants, Vcl.Graphics, Vcl.Imaging.jpeg,
  ME.Point, ME.MapLevel, ME.LocalMap;

const
  NullValue = 'NullValue';

type
  TMEPoint = record
  public
    ID: Variant;
    X: Integer;
    Y: Integer;
  public
    constructor Create(X, Y: Integer);

    procedure AssignTo(Source: TPoint);
  end;

  TMEMapLevel = record
  public
    ID: Variant;
    MapID: Variant;
    Level: Integer;
    Picture: TJPEGImage;
  public
    constructor Create(MapID: Variant; Level: Integer);

    procedure AssignTo(Source: TMapLevel);
  end;

  TMELocalMap = record
    ID: Variant;
    Name: string;
    Left: TMEPoint;
    Right: TMEPoint;
    Levels: array of TMEMapLevel;

    constructor Create(const Name: string; Left, Right: TMEPoint);

    procedure AssignTo(Source: TLocalMap);
  end;

var
  PointTestData: array of TMEPoint;
  MapLevelTestData: array of TMEMapLevel;
  MapTestData: array of TMELocalMap;

  WoodsMap: TMELocalMap;
  CustomsMap: TMELocalMap;
  InterchangeMap: TMELocalMap;
  FactoryMap: TMELocalMap;

implementation

{ TMEPoint }

constructor TMEPoint.Create(X, Y: Integer);
begin
  ID := NullValue;
  Self.X := X;
  Self.Y := Y;
end;

procedure TMEPoint.AssignTo(Source: TPoint);
begin
  if not SameText(VarToStr(ID), NullValue) then
    Source.ID := ID;

  Source.SetBounds(X, Y);
end;

{ TMEMapLevel }

constructor TMEMapLevel.Create(MapID: Variant; Level: Integer);
begin
  Self.ID := NullValue;
  Self.MapID := NullValue;
  Self.Level := Level;
  Self.Picture := TJPEGImage.Create;
end;

procedure TMEMapLevel.AssignTo(Source: TMapLevel);
begin
  if not SameText(VarToStr(ID), NullValue) then
    Source.ID := ID;

 if not SameText(VarToStr(MapID), NullValue) then
   Source.MapID := MapID;

  Source.Level := Level;
  Source.Picture := Picture;
end;

{ TMELocalMap }

constructor TMELocalMap.Create(const Name: string; Left, Right: TMEPoint);
begin
  ID := NullValue;
  Self.Name := Name;
  Self.Left.ID := Left.ID;
  Self.Left.X := Left.X;
  Self.Left.Y := Left.Y;
  Self.Right.ID := Right.ID;
  Self.Right.X := Right.X;
  Self.Right.Y := Right.Y;
end;

procedure TMELocalMap.AssignTo(Source: TLocalMap);
var
  Level: TMapLevel;
  i: Integer;
begin
  if not SameText(VarToStr(ID), NullValue) then
    Source.ID := ID;

  Source.Name := Name;
  Left.AssignTo(Source.Left);
  Right.AssignTo(Source.Right);

  Source.ClearLevelList;
  for i := 0 to Length(Levels) - 1 do begin
    Level := TMapLevel.Create;
    try
      Levels[i].AssignTo(Level);
      Level.MapID := Source.ID;
    finally
      Source.Levels.Add(Level);
    end;
  end;
end;

{ init/done test data }

procedure InitTestData;
begin
  SetLength(PointTestData, 5);
  PointTestData[0] := TMEPoint.Create(500, -920);
  PointTestData[1] := TMEPoint.Create(-700, 480);
  PointTestData[2] := TMEPoint.Create(800, -300);
  PointTestData[3] := TMEPoint.Create(-400, 300);
  PointTestData[4] := TMEPoint.Create(533, -477);

  SetLength(MapLevelTestData, 2);
  MapLevelTestData[0] := TMEMapLevel.Create(NullValue, 0);
  MapLevelTestData[1] := TMEMapLevel.Create(NullValue, 1);

  WoodsMap := TMELocalMap.Create('Woods', TMEPoint.Create(500, -920), TMEPoint.Create(-700, 480));
  SetLength(WoodsMap.Levels, 1);
  WoodsMap.Levels[0] := TMEMapLevel.Create(NullValue, 0);
  WoodsMap.Levels[0].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Woods.jpg');

  CustomsMap := TMELocalMap.Create('Customs', TMEPoint.Create(800, -300), TMEPoint.Create(-400, 300));
  SetLength(CustomsMap.Levels, 1);
  CustomsMap.Levels[0] := TMEMapLevel.Create(NullValue, 0);
  CustomsMap.Levels[0].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Ñustoms.jpg');

  InterchangeMap := TMELocalMap.Create('Interchange', TMEPoint.Create(533, -477), TMEPoint.Create(-367, 423));
  SetLength(InterchangeMap.Levels, 1);
  InterchangeMap.Levels[0] := TMEMapLevel.Create(NullValue, 0);
  InterchangeMap.Levels[0].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Interchange_main.jpg');

  FactoryMap := TMELocalMap.Create('Factory', TMEPoint.Create(333, -329), TMEPoint.Create(-257, 359));
  SetLength(FactoryMap.Levels, 4);
  FactoryMap.Levels[0] := TMEMapLevel.Create(NullValue, -1);
  FactoryMap.Levels[0].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Factory_basement.jpg');
  FactoryMap.Levels[1] := TMEMapLevel.Create(NullValue, 0);
  FactoryMap.Levels[1].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Factory_main.jpg');
  FactoryMap.Levels[2] := TMEMapLevel.Create(NullValue, 1);
  FactoryMap.Levels[2].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Factory_level2.jpg');
  FactoryMap.Levels[3] := TMEMapLevel.Create(NullValue, 2);
  FactoryMap.Levels[3].Picture.LoadFromFile('e:\Projects\Delphi\EscapeFromTarkov\Maps\Origin\Factory_level3.jpg');

  SetLength(MapTestData, 4);
  MapTestData[0] := WoodsMap;
  MapTestData[1] := CustomsMap;
  MapTestData[2] := InterchangeMap;
  MapTestData[3] := FactoryMap;
end;

procedure DoneTestData;
var
  LocalMap: TMELocalMap;
  MapLevel: TMEMapLevel;
begin
  for MapLevel in MapLevelTestData do
    MapLevel.Picture.Free;

  for LocalMap in MapTestData do
    for MapLevel in LocalMap.Levels do
      MapLevel.Picture.Free;
end;

initialization
  InitTestData;

finalization
  DoneTestData;

end.
