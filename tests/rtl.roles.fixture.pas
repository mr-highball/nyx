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
unit rtl.roles.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

{ Native ABI qualification uses RawByteString deliberately. This callable
  RTL-only helper is not a portable application-text contract or an RTL export. }
procedure nyx_rtl_fixture_copy(out AResult: RawByteString; const AText: RawByteString);
  rtlproc;
{ Compiler helpers compile but are hidden from ordinary source lookup. Keep
  their body/signature evidence separate from the callable RTL-only routine. }
procedure nyx_compiler_fixture_copy(out AResult: RawByteString; const AText: RawByteString);
  rtlproc; compilerproc;
{ RTL role alone still owns an ordinary implementation body and signature. }
function RTLFixtureValue(AValue: LongInt): LongInt; rtlproc;
{ Execute real calls across the compiled helper and ordinary following routine. }
function RunRTLProcedureFixture: LongInt;

implementation

procedure nyx_rtl_fixture_copy(out AResult: RawByteString; const AText: RawByteString);
begin
  AResult := AText;
end;

procedure nyx_compiler_fixture_copy(out AResult: RawByteString; const AText: RawByteString);
begin
  AResult := AText;
end;

function RTLFixtureValue(AValue: LongInt): LongInt;
begin
  Result := AValue + 36;
end;

function RunRTLProcedureFixture: LongInt;
var
  LBytes: RawByteString;
begin
  nyx_rtl_fixture_copy(LBytes, 'Pascal');
  Result := RTLFixtureValue(Length(LBytes));
end;

end.
