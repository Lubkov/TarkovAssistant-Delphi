unit TestUtils;

interface

uses
  System.Classes, System.SysUtils, System.Variants, Data.DB,
  ME.DB.Entity, ME.DB.Utils, ME.Point, ME.LocalMap, ME.MapLevel;

  function ComparePoint(const Expected, Actual: TPoint): Boolean;
  function CompareMapLevel(const Expected, Actual: TMapLevel): Boolean;
  function CompareLocalMap(const Expected, Actual: TLocalMap): Boolean;

implementation

function ComparePoint(const Expected, Actual: TPoint): Boolean;
begin
  Result := (Expected.ID = Actual.ID) and
            (Expected.X = Actual.X) and
            (Expected.Y = Actual.Y);
end;

function CompareMapLevel(const Expected, Actual: TMapLevel): Boolean;
begin
  Result := (Expected.ID = Actual.ID) and
            (Expected.MapID = Actual.MapID) and
            (Expected.Level = Actual.Level);
end;

function CompareLocalMap(const Expected, Actual: TLocalMap): Boolean;
begin
  Result := (Expected.ID = Actual.ID) and
            ComparePoint(Expected.Left, Actual.Left) and
            ComparePoint(Expected.Right, Actual.Right);
end;

end.
