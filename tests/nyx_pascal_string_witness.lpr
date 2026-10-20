{ Independent native compiler witness. Its output is data for semantic tests;
  the portable resolver never links or invokes these authored UI expressions.
  Deliberate implicit-conversion warnings remain visible in its separate log. }
program nyx_pascal_string_witness;
{$mode delphi}{$H+}
uses SysUtils, Classes, strings.expressions.fixture;

var LText: AnsiString; LOutput: TFileStream;

procedure Store(const APath: String; const AText: AnsiString);
begin
  LOutput := TFileStream.Create(APath,fmCreate);
  try

    if Length(AText) > 0 then LOutput.WriteBuffer(AText[1],Length(AText));
  finally LOutput.Free; end;
end;

begin

  if ParamCount <> 1 then raise Exception.Create('Supply the owned witness output path');
  LText := NativeStringExpressions;

  if NativeStringExpressionCalls <> 108 then raise Exception.Create('The complete native witness did not execute');
  Store(ParamStr(1),LText);
  Store(ParamStr(1)+'.capacity',NativeStringCapacities);
  WriteLn('PASS native string witness / 108 expressions');
end.
