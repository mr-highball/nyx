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
program nyx_pascal_mode_oracle;
{$codepage utf8}

{ Independent native compiler evidence for the source-mode facts represented by
  translation. Each compiler job selects one source dialect; FPC permits only one
  source MODE directive per module. The default selection is Delphi. No resolver
  output is compiled as a substitute for these exact handwritten bytes. }
{ Diagnostic 2005 reports intentional nested same-delimiter comments. Suppress
  only that expected warning while these lexical probes are scanned, including
  inactive branches which the compiler still reads under the active mode. }
{$warn 2005 off}
{$ifdef NYX_ORACLE_FPC}
{$mode fpc}
{$ifopt H+}{$fatal FPC mode must default to short strings}{$endif}
{ outer { inner } still inside the comment }
{$modeswitch out}
{$else}
{$ifdef NYX_ORACLE_OBJFPC}
{$mode objfpc}
{$ifopt H+}{$fatal ObjFPC mode must default to short strings}{$endif}
{$ifndef FPC_OBJFPC}{$fatal ObjFPC mode define is missing}{$endif}
{$ifdef FPC_DELPHI}{$fatal Stale Delphi mode define}{$endif}
{ outer { inner } still inside the comment }
{$else}
{$ifdef NYX_ORACLE_UNICODE}
{$mode delphiunicode}
{$ifopt H-}{$fatal DelphiUnicode must default to long strings}{$endif}
{$ifndef FPC_DELPHI}{$fatal DelphiUnicode uses the Delphi mode define}{$endif}
{$ifdef FPC_OBJFPC}{$fatal Stale ObjFPC mode define}{$endif}
{ outer { inner } // }
{$else}
{$ifdef NYX_ORACLE_TP}
{$mode tp}
{$ifopt H+}{$fatal TP mode must default to short strings}{$endif}
{$ifndef FPC_TP}{$fatal TP mode define is missing}{$endif}
{$ifdef FPC_DELPHI}{$fatal Stale DelphiUnicode mode define}{$endif}
{ outer { inner } // }
{$modeswitch out}
{$else}
{$mode delphi}
{$ifopt H-}{$fatal Delphi mode must default to long strings}{$endif}
{$ifndef FPC_DELPHI}{$fatal Delphi mode define is missing}{$endif}
{$ifdef FPC_TP}{$fatal Stale TP mode define}{$endif}
{ outer { inner } // }
{$endif}
{$endif}
{$endif}
{$endif}
{$warn 2005 on}

uses TypInfo;

{ OUT is native to ObjFPC/Delphi or enabled explicitly in FPC/TP above. }
procedure Produce(out AValue: Integer);
begin
  AValue := 1;
end;

var
  LValue: Integer;
begin
  Produce(LValue);

  if LValue <> 1 then
  begin
    Halt(1);
  end;
  {$ifdef NYX_ORACLE_UNICODE}

  if PTypeInfo(TypeInfo(String))^.Kind <> tkUString then
  {$else}
  {$ifopt H+}

  if PTypeInfo(TypeInfo(String))^.Kind <> tkAString then
  {$else}

  if PTypeInfo(TypeInfo(String))^.Kind <> tkSString then
  {$endif}
  {$endif}
  begin
    Halt(1);
  end;
  WriteLn('PASS native mode/comment/OUT/default oracle');
end.
