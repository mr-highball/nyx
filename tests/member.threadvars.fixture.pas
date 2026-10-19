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
unit member.threadvars.fixture;
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Compiler-owned thread-storage oracle. Continued class threadvars remain
    separate from shared class storage; no instance is required to access them. }
  TThreadLocalSettings = class
  public
    class var
      Shared: LongInt;
    {$i member.threadvars.fixture.inc}
  const
    Offset = 2;
  public
    InstanceValue: LongInt;
    class function ReadValue: LongInt; static;
  end;

threadvar
  GlobalFirst, GlobalSecond: LongInt;
  GlobalThird: LongInt;
var
  Ordinary: LongInt;

{ Join one native worker and verify both threadvar families' isolation, shared
  class storage and the constant following the included member section. }
function ThreadStorageValue: LongInt;

implementation

uses Classes;

type
  { Independent compiler/runtime fixture, not a Nyx scheduler replacement. It
    owns scalar observations only; its caller always joins before freeing it. }
  TThreadProbe = class(TThread)
  protected
    procedure Execute; override;
  public
    Initial, LocalTotal, GlobalTotal: LongInt;
  end;

class function TThreadLocalSettings.ReadValue: LongInt;
begin
  Result := LocalFirst + LocalSecond + LocalThird + Offset;
end;

procedure TThreadProbe.Execute;
begin
  Initial := TThreadLocalSettings.LocalFirst + GlobalFirst;
  TThreadLocalSettings.LocalFirst := 20;
  TThreadLocalSettings.LocalSecond := 10;
  TThreadLocalSettings.LocalThird := 10;
  GlobalFirst := 11;
  GlobalSecond := 13;
  GlobalThird := 18;
  LocalTotal := TThreadLocalSettings.ReadValue;
  GlobalTotal := GlobalFirst + GlobalSecond + GlobalThird;
  TThreadLocalSettings.Shared := GlobalTotal;
end;

function ThreadStorageValue: LongInt;
var
  LWorker: TThreadProbe;
begin
  TThreadLocalSettings.LocalFirst := 7;
  TThreadLocalSettings.LocalSecond := 0;
  TThreadLocalSettings.LocalThird := 0;
  TThreadLocalSettings.Shared := 0;
  GlobalFirst := 5;
  GlobalSecond := 0;
  GlobalThird := 0;
  LWorker := TThreadProbe.Create(True);
  try
    LWorker.FreeOnTerminate := False;
    LWorker.Start;
    LWorker.WaitFor;
    Result := -1;

    if (LWorker.FatalException = nil) and (LWorker.Initial = 0) and
      (LWorker.LocalTotal = 42) and (LWorker.GlobalTotal = 42) and
      (TThreadLocalSettings.LocalFirst = 7) and (GlobalFirst = 5) and
      (TThreadLocalSettings.Shared = 42) then
    begin
      Result := 42;
    end;
  finally
    LWorker.Free;
  end;
end;

end.
