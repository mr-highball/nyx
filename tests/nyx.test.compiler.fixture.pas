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

unit nyx.test.compiler.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text;

type
  { Closed physical-process roles in the maintained compiler family fixture.
    A published PID marker proves entry in that actual child, not job admission. }
  TNyxCompilerFixtureRole = (cfrCompiler, cfrHelper, cfrGrandchild);

{ Stable marker filename used for diagnostic role identity. Invalid ordinals
  refuse; the caller owns the returned text and no process/file is retained. }
function NyxCompilerFixtureMarkerName(ARole: TNyxCompilerFixtureRole): TNyxText;
{ Resolves the marker within a private fixture-status child of the supplied
  invocation directory. This performs no I/O. The fixture atomically publishes
  there, since owned source invocations pin their root against directory writers.
  A reader still needs the actual PID/handle and terminal join evidence. }
function NyxCompilerFixtureMarkerPath(const ADirectory: TNyxText;
  ARole: TNyxCompilerFixtureRole): TNyxText;

implementation

uses SysUtils;

function NyxCompilerFixtureMarkerName(ARole: TNyxCompilerFixtureRole): TNyxText;
begin
  { The integer boundary also rejects explicit unchecked enum casts; a closed
    enum case would let FPC fold the defensive else into unreachable code. }
  case Ord(ARole) of
    Ord(cfrCompiler):
    begin
      Result := 'compiler.ready';
    end;
    Ord(cfrHelper):
    begin
      Result := 'helper.ready';
    end;
    Ord(cfrGrandchild):
    begin
      Result := 'grandchild.ready';
    end;
  else
    raise EArgumentException.Create('Unknown compiler fixture process role');
  end;
end;

function NyxCompilerFixtureMarkerPath(const ADirectory: TNyxText;
  ARole: TNyxCompilerFixtureRole): TNyxText;
const
  CStatusDirectory: TNyxText = 'fixture-status';
begin
  Result := ADirectory;

  if (Result <> '') and (Result[Length(Result)] <> PathDelim) and
    (Result[Length(Result)] <> '/') then
  begin
    Result := Result + PathDelim;
  end;
  Result := Result + CStatusDirectory + PathDelim + NyxCompilerFixtureMarkerName(ARole);
end;

end.
