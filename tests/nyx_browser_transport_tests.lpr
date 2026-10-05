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

program nyx_browser_transport_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.studio.transport,
  nyx.studio.exchange.browser;

type
  { External policy implementations must also return an admitted snapshot. }
  TEmptyPolicy = class(TInterfacedObject, INyxTransportPolicy)
    function WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
    function Snapshot: TNyxTransportLimits;
  end;

  { Test packets select a closed fixture mode at the explicit wire boundary.
    The raw Pascal peer holds real HTTP requests; no Studio project is attached. }
  TProbeMode = (pmReply, pmSilent, pmHeaderTrickle, pmBodyTrickle);

  TTransportJourney = class
  private
    FExchange: TNyxBrowserEditorExchange;
    FMode: TProbeMode;
    FStarted: Double;
    FCallbacks: Integer;
    FChecks: Integer;
    FCanceled: Boolean;
    FPolicy: INyxTransportPolicy;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Failed(const AMessage: TNyxText);
    procedure StartMode;
    procedure Next;
    procedure Reply(AStatus: Integer; const AText: TNyxText);
    procedure Cancel;
    procedure CanceledDone;
    procedure Forbidden(AStatus: Integer; const AText: TNyxText);
    procedure TickForbidden;
    procedure Finish;
  public
    procedure Run;
  end;

var
  GJourney: TTransportJourney;

function TEmptyPolicy.WholeRequest(AMilliseconds: Integer): INyxTransportPolicy;
begin
  Result := Self;
end;

function TEmptyPolicy.Snapshot: TNyxTransportLimits;
begin
  Result := Default(TNyxTransportLimits);
end;

procedure TTransportJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(FChecks);
end;

procedure TTransportJourney.Failed(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-nyx-transport-error', AMessage);
  WriteLn('FAIL ', AMessage);
  FreeAndNil(FExchange);
end;

procedure TTransportJourney.Run;
var
  LSnapshot: TNyxTransportLimits;
  LRefused: Boolean;
begin
  try
    FPolicy := NewNyxTransportPolicy.WholeRequest(220);
    LSnapshot := FPolicy.Snapshot;
    FPolicy.WholeRequest(1000);
    Check(LSnapshot.DeadlineMS = 220, 'Captured deadline retains immutable value');
    LRefused := False;
    try
      FPolicy.WholeRequest(0);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Unbounded browser deadline refuses');
    LRefused := False;
    try
      FExchange := TNyxBrowserEditorExchange.Create(TEmptyPolicy.Create);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Alternative unset policy refuses before browser transport work');
    FPolicy.WholeRequest(220);
    FExchange := TNyxBrowserEditorExchange.Create(FPolicy);
    { Mutation after construction cannot silently extend this adapter's bound. }
    FPolicy.WholeRequest(1000);
    FMode := pmReply;
    StartMode;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure TTransportJourney.StartMode;
var
  LBefore: Integer;
begin
  LBefore := FCallbacks;
  FStarted := window.performance.now;
  FExchange.Post(True, '', NyxObject([NyxField('case', NyxData(Ord(FMode)))]).ToJSON, Reply);
  Check(FCallbacks = LBefore, 'Browser Post must not deliver inline');
end;

procedure TTransportJourney.Reply(AStatus: Integer; const AText: TNyxText);
var
  LElapsed: Double;
begin
  try
    Inc(FCallbacks);
    Check(FCallbacks = Ord(FMode) + 1, 'One terminal callback per actual browser request');
    LElapsed := window.performance.now - FStarted;
    document.body.setAttribute('data-nyx-transport-elapsed-' + IntToStr(Ord(FMode)),
      FloatToStr(LElapsed));

    if FMode = pmReply then
    begin
      Check((AStatus = 200) and (AText = '{"ok":true}'), 'Actual XHR preserves exact successful bytes');
    end
    else
    begin
      Check((AStatus = 0) and (Pos('local work is retained', AText) > 0),
        'Stalled XHR returns bounded failure help');
      Check((LElapsed >= 170) and (LElapsed < 1200), 'Byte progress cannot reset captured whole-request timeout');
    end;
    window.setTimeout(@Next, 80);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure TTransportJourney.Next;
begin
  try

    if FMode <> pmBodyTrickle then
    begin
      FMode := Succ(FMode);
      StartMode;
      Exit;
    end;
    FreeAndNil(FExchange);
    FExchange := TNyxBrowserEditorExchange.Create(NewNyxTransportPolicy.WholeRequest(15000));
    FCanceled := True;
    FExchange.Post(True, '', NyxObject([NyxField('case', NyxData(Ord(pmSilent)))]).ToJSON, Forbidden);
    FExchange.Schedule(200, TickForbidden);
    window.setTimeout(@Cancel, 80);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure TTransportJourney.Forbidden(AStatus: Integer; const AText: TNyxText);
begin
  Failed('Canceled/destroyed browser receiver was notified');
end;

procedure TTransportJourney.TickForbidden;
begin
  Failed('Destroyed scheduled receiver was notified');
end;

procedure TTransportJourney.Cancel;
begin
  FExchange.CancelRequest;
  FExchange.CancelTick;
  FreeAndNil(FExchange);
  window.setTimeout(@CanceledDone, 300);
end;

procedure TTransportJourney.CanceledDone;
begin
  try
    Check(FCanceled and (FCallbacks = 4), 'Canceled request/tick never adds a terminal callback');
    { A fresh adapter uses the same peer after a canceled stalled request. }
    FExchange := TNyxBrowserEditorExchange.Create(NewNyxTransportPolicy.WholeRequest(1000));
    FExchange.Post(True, '', NyxObject([NyxField('case', NyxData(0))]).ToJSON,
      procedure(AStatus: Integer; const AText: TNyxText)
      begin
        try
          Check((AStatus = 200) and (AText = '{"ok":true}'), 'Actual browser transport works after cancellation');
          Finish;
        except
          on LException: Exception do
          begin
            Failed(LException.Message);
          end;
        end;
      end);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure TTransportJourney.Finish;
var
  LStop: TJSXMLHttpRequest;
begin
  FreeAndNil(FExchange);
  document.body.setAttribute('data-nyx-transport-checks', IntToStr(FChecks));
  document.body.setAttribute('data-nyx-transport-ready', 'passed');
  WriteLn('PASS ', FChecks, ' actual browser transport deadline checks');
  { Stop only this qualification peer after publishing the terminal DOM marker. }
  LStop := TJSXMLHttpRequest.new;
  LStop.open('GET', '/done', True);
  LStop.send;
end;

begin
  GJourney := TTransportJourney.Create;
  GJourney.Run;
end.
