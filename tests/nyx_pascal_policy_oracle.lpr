{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
program nyx_pascal_policy_oracle;
{$mode delphi}{$H+}{$codepage utf8}
{$push}
{$modeswitch nestedprocvars}
{$pop}

{ Independent handwritten input for the actual configured Win32/i386 compiler.
  No Nyx-produced projection supplies layout, calling or restore evidence. }
uses SysUtils;

{$R-}{$Q-}{$H+}{$macro on}{$scopedenums off}
{$define RETAINED_VALUE:=11}
{$packrecords 1}{$packenum 1}{$calling cdecl}
type
  { This grammar remains enabled after POP because the mode switch is global. }
  TNestedCallback = procedure is nested;
  { Concrete compiler layout before changing local policies. }
  TBefore = record
    Prefix: Byte;
    Value: LongWord;
  end;
  { Enumeration storage before the settings frame. }
  TBeforeEnum = (BeforeValue);
  { A compiler-checked cdecl signature, independent of the ambient default. }
  TCdeclProc = procedure; cdecl;
  { A compiler-checked stdcall signature for the nested default. }
  TStdCallProc = procedure; stdcall;
{$push}
{$R+}{$Q+}{$H-}{$inline on}{$packrecords 8}{$packenum 4}{$calling stdcall}
{$scopedenums on}
{$define RETAINED_VALUE:=13}
type
  { Local frame changes this concrete compiler layout. }
  TInside = record
    Prefix: Byte;
    Value: LongWord;
  end;
  { Local minimum enumeration width is four bytes. }
  TInsideEnum = (InsideValue);

procedure Inside;
begin
end;

{$macro off}
{$pop}
{$ifopt R+}{$fatal Range checks were not restored}{$endif}
{$ifopt Q+}{$fatal Overflow checks were not restored}{$endif}
{$ifopt H-}{$fatal String defaults were not restored}{$endif}
type
  { Concrete layout after POP must match the first declaration. }
  TAfter = record
    Prefix: Byte;
    Value: LongWord;
  end;
  { Enum packing and unscoped-name policy after the frame. }
  TAfterEnum = (AfterValue);

procedure After;
begin
end;

var
  LChecks: Integer;
  LCdecl: TCdeclProc;
  LStdCall: TStdCallProc;
  LEnum: TAfterEnum;

procedure Check(ACondition: Boolean);
begin
  Inc(LChecks);

  if not ACondition then
  begin
    raise Exception.Create('Actual compiler policy mismatch at check ' + IntToStr(LChecks));
  end;
end;

begin
  Check(SizeOf(TNestedCallback) > 0);
  Check(SizeOf(TBefore) = 5);
  Check(SizeOf(TInside) = 8);
  Check(SizeOf(TAfter) = 5);
  Check(SizeOf(TBeforeEnum) = 1);
  Check(SizeOf(TInsideEnum) = 4);
  Check(SizeOf(TAfterEnum) = 1);
  LEnum := AfterValue;
  Check(LEnum = AfterValue);
  LCdecl := @After;
  LStdCall := @Inside;
  LCdecl;
  LStdCall;
  { Macro enablement is global in this compiler and survives the local POP.
    The replacement table also survives; re-enable it explicitly for the check. }
  {$macro on}
  Check(RETAINED_VALUE = 13);
  WriteLn('PASS ', LChecks, ' independent compiler local-policy checks');
end.
