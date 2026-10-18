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
program nyx_pascal_warning_oracle;
{$mode delphi}{$H+}{$codepage utf8}

{ Message 5066 is the configured FPC's deprecated-symbol-with-message ID, pinned
  from its original English message source. This is independent compiler evidence;
  the Nyx selector contract does not expand named aliases to guessed message IDs.
  The pinned compiler saves pending message requests at PUSH. Its already-applied
  OFF request is absent from that pending chain, so this POP restores defaults:
  the unmodified normal example emits three marked-symbol warnings, not two. }
function LegacyValue: Integer; deprecated 'Use CurrentValue';
begin
  Result := 21;
end;

var
  LValue: Integer;

{$ifdef NYX_WARNING_UNKNOWN}
{$WARN NYX_FUTURE_WARNING OFF}
{$endif}

begin
  {$WARN SYMBOL_DEPRECATED OFF}
  LValue := LegacyValue;
  {$PUSH}
  {$ifdef NYX_WARNING_AS_ERROR}
  {$WARN 5066 ERROR}
  {$else}
  {$WARN 5066 ON}
  {$endif}
  LValue := LValue + LegacyValue;
  {$POP}
  LValue := LValue + LegacyValue;
  {$ifndef NYX_WARNING_AS_ERROR}
  {$WARN SYMBOL_DEPRECATED ON}
  LValue := LValue + LegacyValue;
  {$endif}
  WriteLn('PASS native warning-policy oracle / value=', LValue);
end.
