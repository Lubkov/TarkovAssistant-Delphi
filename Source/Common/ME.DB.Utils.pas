unit ME.DB.Utils;

interface

uses
  System.SysUtils, System.Variants, Data.DB;

  function IsNullID(const Value: Variant): Boolean;
  function IsNull(const Value, ReplaceValue: Variant): Variant;
  function IfNull(const Value, NullValue: Variant): Variant;

implementation

function IsNullID(const Value: Variant): Boolean;
begin
  Result := VarIsEmpty(Value) or VarIsNull(Value) or
            (VarToStr(Value) = '') or (VarToStr(Value) = '0');
end;

function IsNull(const Value, ReplaceValue: Variant): Variant;
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    Result := ReplaceValue
  else
    Result := Value;
end;

function IfNull(const Value, NullValue: Variant): Variant;
begin
  if (Value = NullValue) then
    Result := Null
  else
    Result := Value;
end;

end.
