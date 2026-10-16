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

program nyx_canvas_observation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.publication, nyx.canvas.observers
  {$ifdef PAS2JS}, Web{$endif};

type
  { Owned test receiver records delivered revisions and may revoke an independent
    token, add the next receiver, close its borrowed hub or deliberately fail. }
  TObserver = class(TInterfacedObject, INyxCanvasPresentationObserver)
  public
    Calls: Integer;
    Revision: Integer;
    Hub: TNyxCanvasObservers;
    CancelNext: INyxCancellationScope;
    Added: INyxCanvasPresentationObserver;
    AddedScope: INyxCancellationScope;
    Fail: Boolean;
    FailEmpty: Boolean;
    Closing: Boolean;
    procedure Presented(ARevision: Integer);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Canvas observation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.Presented(ARevision: Integer);
begin
  Inc(Calls);
  Revision := ARevision;

  if CancelNext <> nil then
  begin
    CancelNext.Cancel;
  end;

  if (Added <> nil) and (AddedScope = nil) then
  begin
    AddedScope := Hub.Subscribe(Added);
  end;

  if Closing then
  begin
    Hub.Close;
  end;

  if Fail then
  begin
    raise Exception.Create('Deliberate presentation receiver failure');
  end;

  if FailEmpty then
  begin
    raise Exception.Create('');
  end;
end;

procedure Run;
var
  LHub: TNyxCanvasObservers;
  LFirst: TObserver;
  LSecond: TObserver;
  LThird: TObserver;
  LFirstLease: INyxCanvasPresentationObserver;
  LSecondLease: INyxCanvasPresentationObserver;
  LThirdLease: INyxCanvasPresentationObserver;
  LFirstScope: INyxCancellationScope;
  LSecondScope: INyxCancellationScope;
  LScopes: array of INyxCancellationScope;
  LFailed: Boolean;
  LIndex: Integer;
begin
  LHub := TNyxCanvasObservers.Create;
  LFirst := TObserver.Create;
  LFirstLease := LFirst;
  LFirst.Hub := LHub;
  LSecond := TObserver.Create;
  LSecondLease := LSecond;
  LThird := TObserver.Create;
  LThirdLease := LThird;
  try
    LFirstScope := LHub.Subscribe(LFirstLease);
    LSecondScope := LHub.Subscribe(LSecondLease);
    Check((LFirst.Calls = 0) and (LSecond.Calls = 0),
      'subscription does not call borrowed application receivers');
    LHub.Notify(7);
    Check((LFirst.Calls = 1) and (LFirst.Revision = 7) and (LSecond.Calls = 1),
      'the actual completed frame revision reaches each observer');
    LFirst.CancelNext := LSecondScope;
    LFirst.Added := LThirdLease;
    LHub.Notify(8);
    Check((LFirst.Calls = 2) and (LSecond.Calls = 1) and LSecondScope.Cancelled,
      'cancellation suppresses a later receiver in the same snapshot');
    Check(LThird.Calls = 0, 'new observers wait for the next frame snapshot');
    LHub.Notify(9);
    Check((LThird.Calls = 1) and (LThird.Revision = 9),
      'a retained addition receives a subsequent completed frame');
    LFirst.Fail := True;
    LFailed := False;
    try
      LHub.Notify(10);
    except
      on E: ENyxPublicationNotification do
      begin
        LFailed := Pos('Deliberate', E.Message) > 0;
      end;
    end;
    Check(LFailed and (LThird.Calls = 2),
      'committed notification failure still delivers every later eligible receiver');
    LFirst.Fail := False;
    LFirst.FailEmpty := True;
    LFailed := False;
    try
      LHub.Notify(11);
    except
      on E: ENyxPublicationNotification do
      begin
        LFailed := E.HasReceiverMessage and (E.ReceiverMessage = '');
      end;
    end;
    Check(LFailed and (LThird.Calls = 3),
      'an empty receiver exception still reports committed failure and preserves later delivery');
    LFirst.FailEmpty := False;
    LFirst.Closing := True;
    LHub.Notify(12);
    Check(LFirstScope.Cancelled and LFirst.AddedScope.Cancelled and (LThird.Calls = 3),
      'closing during notification revokes the current and remaining receivers');
    LHub.Notify(13);
    Check(LFirst.Calls = 6, 'closed hubs deliver no later frame');
    LFailed := False;
    try
      LHub.Subscribe(LSecondLease);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'closed hubs refuse new receiver ownership');
    LHub.Free;
    LHub := nil;
    LFirstScope.Cancel;
    LSecondScope.Cancel;
    Check(LFirstScope.Cancelled and LSecondScope.Cancelled,
      'retained cancellation tokens safely outlive the canvas hub');
    LHub := TNyxCanvasObservers.Create;
    SetLength(LScopes, 256);
    for LIndex := 0 to High(LScopes) do
    begin
      LScopes[LIndex] := LHub.Subscribe(LSecondLease);
    end;
    LFailed := False;
    try
      LHub.Subscribe(LSecondLease);
    except
      on E: ENyxSchedule do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'observer admission has an explicit active-slot budget');
    LScopes[0].Cancel;
    LFirstScope := LHub.Subscribe(LSecondLease);
    Check(not LFirstScope.Cancelled,
      'retiring an observer reclaims capacity without replacing the canvas');
  finally
    LHub.Free;
    LFirst.CancelNext := nil;
    LFirst.AddedScope := nil;
    LFirst.Added := nil;
    LScopes := nil;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-observation', 'passed');
    document.body.setAttribute('data-canvas-observation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' canvas presentation observation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-observation', 'failed');
      document.body.setAttribute('data-canvas-observation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
