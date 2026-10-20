{ Independent whole native overload witness. The semantic tests receive only
  exact output data; their resolver never executes these operator bodies. }
program nyx_pascal_pointer_witness;
{$mode delphi}{$H+}
uses SysUtils, Classes, pointers.identity.fixture;

var LText: AnsiString; LOutput: TFileStream;
begin

  if ParamCount <> 1 then raise Exception.Create('Supply the owned witness output path');
  LText := NativePointerIdentity;

  if NativePointerCalls <> 93 then raise Exception.Create('The whole native witness did not execute');
  LOutput := TFileStream.Create(ParamStr(1),fmCreate);
  try

    if Length(LText) > 0 then LOutput.WriteBuffer(LText[1],Length(LText));
  finally LOutput.Free; end;
  WriteLn('PASS native pointer witness / 93 overloads');
end.
