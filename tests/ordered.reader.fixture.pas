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
unit ordered.reader.fixture;
{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch nestedcomments-}

{$push}{$H-}{$macro on}
{$define SharedByte:=Byte}
{$if not declared(TFuture)}
  {$modeswitch nestedcomments}
{$endif}

interface

function RunOrderedReaderFixture: Integer;

{$include ordered.reader.fixture.inc}
type
  { The second textual visit can see this declaration; the first cannot. }
  TFuture = SharedByte;
{$include ordered.reader.fixture.inc}
{$pop}
{ This deliberate nesting proves the module switch survived POP. Suppress only
  FPC's documented comment-level warning 2005 around that compiler oracle. }
{$push}{$warn 2005 off}
const
  { A real token boundary lets FPC apply the queued message setting before the
    intentional nested comment is scanned. The executed result uses this value. }
  CReaderAnswer = 42;
{ outer { nested } module setting survives local POP }
{$pop}
{$ifopt H-}{$fatal Local string policy was not restored}{$endif}
{$ifndef SAW_BEFORE}{$fatal The first include lost its earlier scope}{$endif}
{$ifndef SAW_AFTER}{$fatal The second include lost its later scope}{$endif}
{$ifndef SAVED_POLICY}{$fatal The included local policy was not retained}{$endif}

implementation

function RunOrderedReaderFixture: Integer;
begin
  Result := CReaderAnswer;
end;

end.
