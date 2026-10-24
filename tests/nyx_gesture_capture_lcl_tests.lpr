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
program nyx_gesture_capture_lcl_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Interfaces, Forms, Controls, ExtCtrls, LMessages, LCLType,
  nyx.text, nyx.types, nyx.events, nyx.observation, nyx.gestures.lcl;

type
  { Detached flags model source admission without retaining a widget or model.
    Epoch is monotonic even when readiness returns to its original value. }
  TCaptureInput = class(TInterfacedObject, INyxLCLCaptureInput)
  public
    Current: Boolean;
    Ready: Boolean;
    Epoch: Integer;
    function GetCurrent: Boolean;
    function GetReady: Boolean;
    function GetEpoch: Integer;
  end;

  { Owns the fixture's weak native sinks. The chained procedure can invalidate
    the original press during native cancellation, before user observation.
    These are actual LCL messages, not an OS cursor or hardware-input claim. }
  TCaptureProbe = class
  private
    FChecks: Integer;
    FCancels: Integer;
    FInvalidateEpoch: Boolean;
    FInvalidateRevision: Boolean;
    FPrevious: TWndMethod;
    FEvents: INyxEvents;
    FFrame: INyxNativeGestureFrame;
    FPublication: INyxObservationPublication;
    FInput: TCaptureInput;
    procedure Check(ACondition: Boolean; const AMessage: TNyxText);
    procedure PreviousMessage(var AMessage: TLMessage);
    procedure Observed(const AOrigin: TNyxText; ATrigger: TNyxTrigger);
  public
    procedure Run;
  end;

function TCaptureInput.GetCurrent: Boolean;
begin
  Result := Current;
end;

function TCaptureInput.GetReady: Boolean;
begin
  Result := Ready;
end;

function TCaptureInput.GetEpoch: Integer;
begin
  Result := Epoch;
end;

procedure TCaptureProbe.Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(String(AMessage));
  end;
  Inc(FChecks);
end;

procedure TCaptureProbe.PreviousMessage(var AMessage: TLMessage);
begin
  FPrevious(AMessage);

  if AMessage.Msg = LM_CANCELMODE then
  begin

    if FInvalidateEpoch then
    begin
      Inc(FInput.Epoch);
    end;

    if FInvalidateRevision then
    begin
      FEvents.CancelPending;
      FPublication.PublishRevision(FEvents.ViewRevision);
    end;
  end;
end;

procedure TCaptureProbe.Observed(const AOrigin: TNyxText; ATrigger: TNyxTrigger);
begin

  if ATrigger = ntPointerCancel then
  begin
    Check((AOrigin = 'capture-source') and FFrame.Dispatching,
      'Cancellation preserves its logical origin and physical lifetime frame');
    Inc(FCancels);
  end;
end;

procedure TCaptureProbe.Run;
var
  LForm: TForm;
  LPane: TPanel;
  LObserver: INyxLCLCaptureObserver;
  LScoped: INyxLCLScopedCaptureObserver;
  LInputLease: INyxLCLCaptureInput;
  LCase: Integer;
  LExpected: Integer;
begin
  LForm := TForm.CreateNew(nil);
  try
    LPane := TPanel.Create(LForm);
    LPane.Parent := LForm;
    LPane.SetBounds(0, 0, 100, 100);
    LPane.HandleNeeded;
    for LCase := 0 to 10 do
    begin
      FCancels := 0;
      FInvalidateEpoch := False;
      FInvalidateRevision := False;
      FEvents := NewNyxEvents;
      FFrame := NewNyxNativeGestureFrame;
      FInput := TCaptureInput.Create;
      LInputLease := FInput;
      FInput.Current := True;
      FInput.Ready := LCase <> 5;
      FInput.Epoch := 1;
      FPrevious := LPane.WindowProc;
      LPane.WindowProc := PreviousMessage;
      LObserver := NewNyxLCLCaptureObserver(FFrame);
      try
        Check(Supports(LObserver, INyxObservationPublication, FPublication),
          'Capture observer exposes flags-only revision publication');
        Check(Supports(LObserver, INyxLCLScopedCaptureObserver, LScoped),
          'Capture observer supports optional per-source admission');

        if LCase = 10 then
        begin
          LObserver.Add('capture-source', LPane);
        end
        else
        begin
          LScoped.AddScoped('capture-source', LPane, LInputLease);
        end;
        LObserver.Activate(FEvents, Observed);
        LPane.Perform(LM_LBUTTONDOWN, MK_LBUTTON, 0);
        LExpected := 0;
        case LCase of
          0:
            begin
              LExpected := 1;
            end;
          1:
            begin
              FInput.Ready := False;
              Inc(FInput.Epoch);
              FInput.Ready := True;
            end;
          2, 10:
            begin
              FEvents.CancelPending;
              FPublication.PublishRevision(FEvents.ViewRevision);
            end;
          3:
            begin
              FInput.Current := False;
            end;
          4:
            begin
              FInput.Ready := False;
            end;
          5:
            begin
              FInput.Ready := True;
            end;
          6:
            begin
              FInvalidateEpoch := True;
            end;
          7:
            begin
              FInvalidateRevision := True;
            end;
          8:
            begin
              LObserver.Disconnect;
            end;
          9:
            begin
              Inc(FInput.Epoch);
              LPane.Perform(LM_LBUTTONDOWN, MK_LBUTTON, 0);
              LExpected := 1;
            end;
        end;
        LPane.Perform(LM_CANCELMODE, 0, 0);
        Check(FCancels = LExpected,
          'Only an original admitted press can cancel; case ' + TNyxText(IntToStr(LCase)));
        LPane.Perform(LM_CANCELMODE, 0, 0);
        Check(FCancels = LExpected, 'Repeated cancellation has no original press to replay');
      finally
        LObserver.Disconnect;
        LScoped := nil;
        FPublication := nil;
        LObserver := nil;
        LPane.WindowProc := FPrevious;
        FPrevious := nil;
        ReleaseNyxLCLPointer(LPane);
        FEvents.Close;
        FEvents := nil;
        FFrame := nil;
        LInputLease := nil;
        FInput := nil;
      end;
    end;
    WriteLn('PASS ', FChecks, ' native capture epoch/revision checks');
  finally
    LForm.Free;
  end;
end;

var
  GProbe: TCaptureProbe;
begin
  Application.Initialize;
  GProbe := TCaptureProbe.Create;
  try
    try
      GProbe.Run;
    except
      on E: Exception do
      begin
        WriteLn('FAIL ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    GProbe.Free;
  end;
end.
