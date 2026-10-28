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
program nyx_viewport_context_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.scheduler, nyx.viewport.context{$ifdef PAS2JS}, Web{$endif};

type
  { Caller-owned synchronous receiver. Its tokens cancel before destruction;
    optional actions exercise real reentrant source/registration retirement. }
  TReceiver = class
  public
    Calls: Integer;
    Last: TNyxViewportContextSnapshot;
    Owner: INyxViewportContextOwner;
    Other: INyxViewportSubscription;
    NextWidth: Double;
    RetireSource: Boolean;
    Fail: Boolean;
    procedure Changed(const AViewport: TNyxViewportContextSnapshot);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TReceiver.Changed(const AViewport: TNyxViewportContextSnapshot);
var
  LWidth: Double;
begin
  Inc(Calls);
  Last := AViewport;

  if Other <> nil then
  begin
    Other.Cancel;
  end;

  if NextWidth > 0 then
  begin
    LWidth := NextWidth;
    NextWidth := 0;
    Owner.Publish(LWidth, AViewport.Height);
  end;

  if RetireSource then
  begin
    Owner.Retire;
  end;

  if Fail then
  begin
    raise Exception.Create('Expected viewport observer refusal');
  end;
end;

procedure Run;
var
  LScheduler: INyxScheduler;
  LOwner: INyxViewportContextOwner;
  LContext: INyxViewportContext;
  LFirst: TReceiver;
  LSecond: TReceiver;
  LFirstToken: INyxViewportSubscription;
  LSecondToken: INyxViewportSubscription;
  LSnapshot: TNyxViewportContextSnapshot;
  LRefused: Boolean;
begin
  LScheduler := NewNyxScheduler;
  LFirst := TReceiver.Create;
  LSecond := TReceiver.Create;
  try
    LOwner := NewNyxViewportContext(LScheduler);
    LContext := LOwner.Context;
    Check(LContext.Active and not LContext.Snapshot.Defined,
      'a new scope is live without inventing an initial zero viewport');
    LFirstToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LFirst.Changed);
    LSecondToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LSecond.Changed);
    Check((LFirst.Calls = 0) and (LSecond.Calls = 0),
      'subscriptions do not call borrowed receivers during construction');
    LOwner.Publish(0, 0);
    Check(LContext.Snapshot.Defined and (LContext.Snapshot.Revision = 1),
      'an actual zero viewport differs from an unknown viewport');
    Check((LFirst.Calls = 1) and (LSecond.Calls = 1),
      'independent subscribers both receive the admitted observation');
    LOwner.Publish(0, 0);
    Check((LFirst.Calls = 1) and (LContext.Snapshot.Revision = 1),
      'equal extents do not spend revisions or notify');
    LOwner.Publish(1240.25, 820.5);
    Check((LFirst.Last.Width = 1240.25) and (LSecond.Last.Height = 820.5),
      'fractional logical dimensions survive without allocation rounding');
    LSnapshot := LContext.Snapshot;
    LRefused := False;
    try
      LOwner.Publish(-1, 100);
    except
      on ENyxViewportContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LContext.Snapshot.SameExtent(LSnapshot) and
      (LContext.Snapshot.Revision = LSnapshot.Revision),
      'invalid observation preserves the admitted snapshot and revision');
    LFirstToken := nil;
    LOwner.Publish(390, 844);
    Check((LFirst.Calls = 2) and (LSecond.Calls = 3),
      'releasing a public subscription cancels only its borrowed receiver');
    LOwner := nil;
    Check(not LContext.Active and not LSecondToken.Active and
      (LContext.Snapshot.Width = 390),
      'reader/token leases cannot keep a retired writer or receiver active');
    LSecondToken.Cancel;
    LSecondToken.Cancel;
    LRefused := False;
    try
      LFirstToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LFirst.Changed);
    except
      on ENyxViewportContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired source refuses a new borrowed receiver');
    LContext := nil;
    LSecondToken := nil;

    LOwner := NewNyxViewportContext(LScheduler);
    LContext := LOwner.Context;
    LFirst.Calls := 0;
    LSecond.Calls := 0;
    LFirstToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LFirst.Changed);
    LSecondToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LSecond.Changed);
    LFirst.Other := LSecondToken;
    LOwner.Publish(1280, 900);
    Check((LFirst.Calls = 1) and (LSecond.Calls = 0) and not LSecondToken.Active,
      'canceling a later receiver suppresses its already captured delivery');
    LFirst.Other := nil;
    LFirstToken := nil;
    LSecondToken := nil;
    LOwner := nil;
    LContext := nil;

    LOwner := NewNyxViewportContext(LScheduler);
    LContext := LOwner.Context;
    LFirst.Calls := 0;
    LSecond.Calls := 0;
    LFirst.Owner := LOwner;
    LFirst.NextWidth := 390;
    LFirstToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LFirst.Changed);
    LSecondToken := LContext.Subscribe({$ifdef PAS2JS}@{$endif}LSecond.Changed);
    LOwner.Publish(1280, 900);
    Check((LFirst.Calls = 2) and (LSecond.Calls = 1) and
      (LSecond.Last.Width = 390) and (LSecond.Last.Revision = 2),
      'reentrant publication never delivers an older viewport after its successor');
    LFirst.Fail := True;
    LRefused := False;
    try
      LOwner.Publish(640, 900);
    except
      on LException: Exception do
      begin
        LRefused := LException.Message = 'Expected viewport observer refusal';
      end;
    end;
    Check(LRefused and (LSecond.Last.Width = 640) and
      (LContext.Snapshot.Revision = 3),
      'observer failure reports after other current receivers and retains admission');
    LFirst.Fail := False;
    LFirst.RetireSource := True;
    LOwner.Publish(800, 900);
    Check(not LContext.Active and not LFirstToken.Active and
      not LSecondToken.Active and (LSecond.Last.Width = 640),
      'source retirement during delivery cancels later borrowed receivers');
    LRefused := False;
    try
      LOwner.Publish(900, 900);
    except
      on ENyxViewportContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired writer refuses publication');
  finally
    LFirstToken := nil;
    LSecondToken := nil;
    LFirst.Other := nil;
    LFirst.Owner := nil;
    LOwner := nil;
    LContext := nil;
    LFirst.Free;
    LSecond.Free;
    LScheduler := nil;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' viewport context checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'PASS ' + IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'FAIL ' + LException.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
