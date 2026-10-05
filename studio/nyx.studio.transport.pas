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

unit nyx.studio.transport;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Immutable admitted transport limits. A deadline covers the entire request,
    including partial headers/body and upload; receiving bytes never resets it.
    Adapters snapshot the policy, so later fluent changes affect only new hosts.
    This is local transport configuration, not portable design or server undo. }
  TNyxTransportLimits = record
  private
    FDeadlineMS: Integer;
  public
    property DeadlineMS: Integer read FDeadlineMS;
  end;

  { Reference-counted fluent policy, independent of DOM, sockets and LCL.
    WholeRequest accepts 1..120000 milliseconds. Zero/unbounded waits refuse.
    The default is fifteen seconds; downloads may explicitly choose another
    admitted bound. Cancellation prevents delivery, not prior server admission. }
  INyxTransportPolicy = interface
    ['{6D5A4DC1-F7AE-4C4E-B7A7-FF84056963F6}']
    function WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
    function Snapshot: TNyxTransportLimits;
  end;

function NewNyxTransportPolicy: INyxTransportPolicy;
{ Re-admit snapshots from alternative policy implementations and default
  records before allocating transport/timer work. Never turn zero into an
  unbounded native wait or browser XHR timeout. }
procedure ValidateNyxTransportLimits(const ALimits: TNyxTransportLimits);

implementation

uses
  SysUtils;

type
  TTransportPolicy = class(TInterfacedObject, INyxTransportPolicy)
  private
    FLimits: TNyxTransportLimits;
  public
    constructor Create;
    function WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
    function Snapshot: TNyxTransportLimits;
  end;

constructor TTransportPolicy.Create;
begin
  inherited Create;
  FLimits.FDeadlineMS := 15000;
end;

function TTransportPolicy.WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
begin

  if (AMilliseconds < 1) or (AMilliseconds > 120000) then
  begin
    raise Exception.Create('Transport deadline requires 1..120000 milliseconds');
  end;
  FLimits.FDeadlineMS := AMilliseconds;
  Result := Self;
end;

function TTransportPolicy.Snapshot: TNyxTransportLimits;
begin
  Result := FLimits;
end;

function NewNyxTransportPolicy: INyxTransportPolicy;
begin
  Result := TTransportPolicy.Create;
end;

procedure ValidateNyxTransportLimits(const ALimits: TNyxTransportLimits);
begin

  if (ALimits.DeadlineMS < 1) or (ALimits.DeadlineMS > 120000) then
  begin
    raise Exception.Create('Transport requires an admitted whole-request deadline');
  end;
end;

end.
