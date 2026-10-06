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

unit nyx.test.resize.input;

{$mode delphi}{$H+}{$codepage utf8}

interface

function RunNyxResizeInputChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.data, nyx.behavior, nyx.events, nyx.gestures,
  nyx.scheduler, nyx.designer.resize;

type
  TResizeHost = class
    Size: TNyxResizeSize;
    Last: TNyxResizeSize;
    Allowed: Boolean;
    Commits: Integer;
    Cancels: Integer;
    Previews: Integer;
    function Capture(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
      out APolicy: TNyxResizePolicy): Boolean;
    procedure Feedback(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
      const ASize: TNyxResizeSize);
  end;

function TResizeHost.Capture(AAxis: TNyxResizeAxis; out ASize: TNyxResizeSize;
  out APolicy: TNyxResizePolicy): Boolean;
begin
  Result := Allowed;
  ASize := Size;
  APolicy := NyxResizePolicy;
end;

procedure TResizeHost.Feedback(AAxis: TNyxResizeAxis; APhase: TNyxResizePhase;
  const ASize: TNyxResizeSize);
begin
  Last := ASize;
  case APhase of
    nrpPreview: Inc(Previews);
    nrpCancel: Inc(Cancels);
    nrpCommit:
      begin
        Inc(Commits);
        Size := ASize;
      end;
  end;
end;

function RunNyxResizeInputChecks: Integer;
var
  LHost: TResizeHost;
  LHandle: TNyxResizeHandle;
  LEvents: INyxEvents;
  LChecks: Integer;
  LResult: TNyxGestureResult;
  LBaseline: TNyxResizeSize;
  LCommits: Integer;
  LConsumed: Boolean;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Resize input: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure Tickets(const AValues: TNyxExecutions);
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to High(AValues) do
    begin

      if AValues[LIndex].Failure <> '' then
      begin
        raise Exception.Create('Resize callback failed: ' + AValues[LIndex].Failure);
      end;
    end;
  end;

  function Pointer(ATrigger: TNyxTrigger; AID: Integer; AX, AY: Double;
    AModifiers: TNyxKeyModifiers = []): TNyxGestureResult;
  var
    LInfo: TNyxEventInfo;
    LDecision: INyxGestureDecision;
  begin
    LInfo := Default(TNyxEventInfo);
    LInfo.Trigger := ATrigger;
    LInfo.Name := NyxEvent(NyxTriggerName(ATrigger));
    LInfo.Value := NyxNull;
    LInfo.OriginID := 'grip';
    LInfo.SourceID := 'grip';
    LInfo.HasPointer := True;
    LInfo.Pointer.Kind := npiTouch;
    LInfo.Pointer.ID := AID;
    LInfo.Pointer.Primary := True;
    LInfo.Pointer.Button := npbPrimary;
    LInfo.Pointer.Buttons := [npbPrimary];
    LInfo.Pointer.HasPosition := True;
    LInfo.Pointer.X := AX;
    LInfo.Pointer.Y := AY;
    LInfo.Pointer.Modifiers := AModifiers;
    LDecision := NewNyxGestureDecision([ngcCapturePointer, ngcReleasePointer]);
    try
      Tickets(LEvents.DispatchGesture(LInfo, 'grip', 'grip', LDecision));
    finally
      Result := LDecision.Seal;
    end;
  end;

  procedure Key(AKey: TNyxKey; AModifiers: TNyxKeyModifiers = []);
  var
    LInfo: TNyxEventInfo;
  begin
    LInfo := Default(TNyxEventInfo);
    LInfo.Trigger := ntKeyDown;
    LInfo.Name := NyxEvent(NyxTriggerName(ntKeyDown));
    LInfo.Value := NyxNull;
    LInfo.SourceID := 'grip';
    LInfo.OriginID := 'grip';
    LInfo.HasKeyboard := True;
    LInfo.Keyboard := NyxKeyStroke(AKey, AModifiers);
    Tickets(LEvents.DispatchInput(LInfo, 'grip', 'grip', LConsumed));
  end;

begin
  LChecks := 0;
  LHost := TResizeHost.Create;
  LEvents := NewNyxEvents;
  LHandle := nil;
  try
    LHost.Allowed := True;
    LHost.Size := NyxResizeSize(203, 121);
    LHandle := TNyxResizeHandle.Create(LEvents, NyxControl('grip'), nraBoth,
      LHost.Capture, LHost.Feedback);
    LResult := Pointer(ntPointerDown, 7, 5, 5);
    Check(LResult.PointerRequest = nprCapture, 'matching primary input requests capture');
    Pointer(ntPointerUp, 7, 5, 5);
    Check((LHost.Commits = 0) and not LHandle.Dragging,
      'tap is a no-op even when original geometry is off grid');
    Pointer(ntPointerDown, 7, 5, 5);
    Pointer(ntPointerCancel, 8, 5, 5);
    Check(LHandle.Dragging, 'another pointer cancellation cannot retire the captured gesture');
    Pointer(ntPointerMove, 8, 18, 26);
    Check(LHost.Last.SameSize(LHost.Size), 'a second pointer cannot move the owned gesture');
    Pointer(ntPointerMove, 7, 18, 26);
    Check(LHost.Last.SameSize(NyxResizeSize(216, 144)), 'preview uses captured start, never accumulated deltas');
    Pointer(ntPointerMove, 7, 18, 26);
    Check(LHost.Commits = 0, 'repeated preview is not an authoring transaction');
    LResult := Pointer(ntPointerUp, 7, 18, 26);
    Check((LResult.PointerRequest = nprRelease) and (LHost.Commits = 1) and
      LHost.Size.SameSize(NyxResizeSize(216, 144)), 'release makes one complete snapped commit');
    Pointer(ntPointerCaptureLost, 7, 18, 26);
    Check(LHost.Commits = 1, 'normal capture loss after release cannot duplicate or cancel the commit');
    LBaseline := LHost.Size;
    Pointer(ntPointerDown, 7, 0, 0);
    Pointer(ntPointerMove, 7, 200, 200);
    Key(nkEscapeKey);
    Check(LConsumed and not LHandle.Dragging, 'Escape consumes and permanently cancels the active gesture');
    Pointer(ntPointerUp, 7, 200, 200);
    Check(LHost.Size.SameSize(LBaseline) and (LHost.Commits = 1), 'release cannot revive canceled geometry');
    Pointer(ntPointerDown, 7, 0, 0);
    Pointer(ntPointerMove, 7, 13, 21, [nmAlt]);
    Check(LHost.Last.SameSize(NyxResizeSize(229, 165)), 'Alt preview bypasses the grid');
    Pointer(ntPointerUp, 7, 13, 21, [nmAlt]);
    Check(LHost.Size.SameSize(NyxResizeSize(229, 165)), 'Alt remains effective through release');
    Key(nkRightKey);
    Check(LConsumed and LHost.Size.SameSize(NyxResizeSize(240, 165)),
      'keyboard adjusts the intended dimension and preserves the other off-grid dimension');
    Key(nkDownKey, [nmShift]);
    Check(LConsumed and LHost.Size.SameSize(NyxResizeSize(240, 248)), 'Shift uses the larger typed keyboard step');
    LCommits := LHost.Commits;
    Key(nkRightKey, [nmControl]);
    Check(not LConsumed and (LHost.Commits = LCommits), 'application shortcuts are not stolen');
    LHost.Allowed := False;
    LResult := Pointer(ntPointerDown, 7, 0, 0);
    Check(not LHandle.Dragging and (LResult.PointerRequest = nprUnchanged),
      'host refusal cannot capture an invalid editor');
    Key(nkRightKey);
    Check(LConsumed and (LHost.Commits = LCommits),
      'recognized grip shortcut cannot scroll the host while authoring is refused');
    LHost.Allowed := True;
    Pointer(ntPointerDown, 7, 0, 0);
    Pointer(ntPointerCancel, 7, 10, 10);
    Check(not LHandle.Dragging and (LHost.Commits = LCommits), 'platform cancellation is terminal');
    Pointer(ntPointerDown, 7, 0, 0);
    LEvents.CancelPending;
    Pointer(ntPointerMove, 7, 100, 100);
    Check(not LHandle.Dragging and (LHost.Commits = LCommits), 'retired mount cancels before accepting deltas');
    FreeAndNil(LHandle);
    Key(nkRightKey);
    Check(not LConsumed and (LHost.Commits = LCommits), 'retained router has no dangling handle receiver');
  finally
    LHandle.Free;
    LEvents := nil;
    LHost.Free;
  end;
  Result := LChecks;
end;

end.
