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
unit condition.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

{ Dedicated scalar-coordinate qualification: 🌟 𐐷. }
{$if declared(TSeen)}
  {$fatal A future declaration leaked into an earlier scope}
{$endif}
{$i condition.fixture.inc}

type
  { This name becomes visible only after the first physical include visit. }
  TSeen = Byte;

{$i condition.fixture.inc}
{$if not (defined(SAW_BEFORE) and defined(SAW_AFTER))}
  {$fatal Repeated include scope was not preserved}
{$endif}

type
  { Source-local alias deliberately differs from System and implicit ObjPas. }
  Integer = Byte;

{$if (sizeof(Integer)=1) and (sizeof(System.Integer)=2)}
const CRootCorrect = True;
{$else}
const CRootCorrect = False;
{$endif}

{ Verify nested lexical shadowing through an actual compiled application call. }
function NestedCorrect: Boolean;

implementation

function NestedCorrect: Boolean;
type
  { The local routine's alias overrides the containing unit's byte alias. }
  Integer = QWord;
begin

  {$if declared(TSeen) and (sizeof(Integer)=8)}
  Result := True;
  {$else}
  Result := False;
  {$endif}
end;

end.
