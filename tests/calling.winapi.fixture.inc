{ Preserve the complete initialized procedural variable and its next sibling.
  The modifier is part of the type; the equals sign begins the initializer. }
var
  Direct: function(AValue: LongInt): LongInt; winapi = nil;
  AfterValue: LongInt;

{ This ordinary implementation owns its convention separately from its result
  type and still owns its body. The native compiler determines the actual ABI. }
function IncrementApi(AValue: LongInt): LongInt; winapi;
begin
  Result := AValue + 1;
end;
