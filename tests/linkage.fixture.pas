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
unit linkage.fixture;

{$mode delphi}{$H+}{$codepage utf8}
{$ifndef PAS2JS}{$modeswitch cvar}{$endif}

interface

{$ifndef PAS2JS}
{$push}
{$MINENUMSIZE 1   retain this explanatory text }
{$endif}
type
  { Native compiler admission qualifies the one-byte minimum request, while
    browser compilation keeps the same portable declared choices. }
  TSmallChoice = (scFirst, scSecond);
{$ifndef PAS2JS}
{$PACKENUM 4   retain another layout explanation }
{$endif}
  { Independent enum storage must not inherit the preceding one-byte request. }
  TWideChoice = (wcFirst, wcSecond);
{$ifndef PAS2JS}
{$PACKRECORDS 1   the value ends before this comment }
{$endif}
  { Native size verifies the actual compiler's selected packing, not a resolver
    ABI calculation. The record owns only its two scalar fields. }
  TPackedValue = record
    Marker: Byte;
    Value: LongInt;
  end;
{$ifndef PAS2JS}
{$PACKSET 1   preserve this set-packing explanation }
{$endif}
  { Small ordinal set for compiler-backed packing admission. }
  TChoiceSet = set of 0..7;
{$ifndef PAS2JS}
{$pop}
{$endif}

type
  { Compiler-produced native measurements, independent of resolver layout
    inference. Browser compilation returns the zeroed value without an ABI claim. }
  TCompilerLayoutSizes = record
    SmallEnumBytes: Integer;
    WideEnumBytes: Integer;
    PackedRecordBytes: Integer;
    PackedSetBytes: Integer;
  end;

var
  SharedValue: LongInt;
  {$ifndef PAS2JS}public name 'nyx_linkage_fixture_shared';{$endif}
  FollowingValue: LongInt;

{ Calls real linked routines after a public variable declaration. This exact
  whole source is also fed to ordinary translation, syntax and name binding. }
function CompilerLinkageValue: Integer;
{ Read actual native SizeOf results through an ordinary function boundary. }
function CompilerLayoutSizes: TCompilerLayoutSizes;

implementation

{ The native symbol modifier belongs to this routine, not the unit's visibility. }
function LinkedValue: Integer;
{$ifndef PAS2JS}public name 'nyx_linkage_fixture_value';{$endif}
begin
  Result := SharedValue + FollowingValue;
end;

{ A second linked body must remain a sibling of the preceding routine. }
function AddedValue: Integer;
{$ifndef PAS2JS}public name 'nyx_linkage_fixture_added';{$endif}
begin
  Result := 1;
end;

function CompilerLinkageValue: Integer;
begin
  SharedValue := 20;
  FollowingValue := 21;
  Result := LinkedValue + AddedValue;
end;

function CompilerLayoutSizes: TCompilerLayoutSizes;
begin
  Result := Default(TCompilerLayoutSizes);
  {$ifndef PAS2JS}
  Result.SmallEnumBytes := SizeOf(TSmallChoice);
  Result.WideEnumBytes := SizeOf(TWideChoice);
  Result.PackedRecordBytes := SizeOf(TPackedValue);
  Result.PackedSetBytes := SizeOf(TChoiceSet);
  {$endif}
end;

end.
