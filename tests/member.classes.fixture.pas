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
unit member.classes.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Value returned by a class property, independent of the owning class's lifetime. }
  TCounterState = record
    { Detached numeric payload returned by the class property. }
    Count: LongInt;
    { Return a zero value without allocating a record/class owner. }
    class function GetEmpty: TCounterState; static;
    { Read-only class-level value, independent of a record instance. }
    class property Empty: TCounterState read GetEmpty;
  end;
  { Class storage/property fixture. No instances or managed resources are owned. }
  TCounterSettings = class
  private
    class var
      FFirst, FSecond: LongInt;
      FThird: LongInt;
    { Combine every grouped/continued class field into a detached result. }
    class function GetState: TCounterState; static;
    { Replace the first contribution; retain the other two class fields. }
    class procedure SetLimit(AValue: LongInt); static;
  public
    {$i member.classes.fixture.inc}
  public
    { Instance storage remains separate from the preceding class properties. }
    InstanceValue: LongInt;
    { Ordinary instance access must refuse when queried through the type. }
    property InstanceOnly: LongInt read InstanceValue write InstanceValue;
    { Seed both supplementary class fields without constructing an instance. }
    class procedure Initialize;
  end;

{ Execute compiled class properties and all continued class-var declarations. }
function ClassMemberValue: LongInt;

implementation

class function TCounterState.GetEmpty: TCounterState;
begin
  Result.Count := 0;
end;

class function TCounterSettings.GetState: TCounterState;
begin
  Result.Count := FFirst + FSecond + FThird;
end;

class procedure TCounterSettings.SetLimit(AValue: LongInt);
begin
  FFirst := AValue;
end;

class procedure TCounterSettings.Initialize;
begin
  FSecond := 1;
  FThird := 1;
end;

function ClassMemberValue: LongInt;
begin
  TCounterSettings.Initialize;
  TCounterSettings.Limit := 40;
  Result := TCounterSettings.State.Count + TCounterState.Empty.Count;
end;

end.
