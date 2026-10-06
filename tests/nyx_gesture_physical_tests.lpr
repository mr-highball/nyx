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
program nyx_gesture_physical_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.gestures,
  nyx.gestures.browser, nyx.render.browser, nyx.behavior, nyx.events, nyx.scheduler;

type
  { All semantic assertions live in Pascal, reached by real host input. The CDP
    driver only locates public adapter identities and supplies physical input. }
  TPhysicalProbe = class(TNyxEventCallback)
    Captured: Integer;
    Lost: Integer;
    OutsideMoves: Integer;
    TouchMoves: Integer;
    Canceled: Integer;
    Started: Integer;
    Hovered: Integer;
    Dropped: Integer;
    Ended: Integer;
    Renderer: TNyxBrowserRenderer;
    procedure Require(ACondition: Boolean; const AReason: String);
    procedure Publish;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

const
  CText: TNyxText = 'Physical transfer / 🌙 漢字';
  CTriggers: array[0..9] of TNyxTrigger = (ntPointerDown, ntPointerUp,
    ntPointerMove, ntPointerCapture, ntPointerCaptureLost, ntDragStart,
    ntDragOver, ntDrop, ntDragEnd, ntPointerCancel);

var
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;
  GProbe: TPhysicalProbe;
  GProbeOwner: INyxEventCallback;
  GSubscriptions: array of INyxEventSubscription;

procedure TPhysicalProbe.Require(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    document.body.setAttribute('data-physical-gestures', 'failed');
    document.body.setAttribute('data-physical-error', AReason);
    raise ENyxGesture.Create(AReason);
  end;
end;

procedure TPhysicalProbe.Publish;
begin
  document.body.setAttribute('data-capture-count', IntToStr(Captured));
  document.body.setAttribute('data-loss-count', IntToStr(Lost));
  document.body.setAttribute('data-outside-count', IntToStr(OutsideMoves));
  document.body.setAttribute('data-touch-move-count', IntToStr(TouchMoves));
  document.body.setAttribute('data-cancel-count', IntToStr(Canceled));
  document.body.setAttribute('data-drag-start-count', IntToStr(Started));
  document.body.setAttribute('data-hover-count', IntToStr(Hovered));
  document.body.setAttribute('data-drop-count', IntToStr(Dropped));
  document.body.setAttribute('data-drag-end-count', IntToStr(Ended));

  if (Captured >= 1) and (Lost = Captured) and (OutsideMoves > 0) and
    (Started = 1) and (Hovered > 0) and (Dropped = 1) and (Ended = 1) then
  begin
    document.body.setAttribute('data-physical-gestures', 'passed');
  end;

  if (Captured = 2) and (Lost = 2) and (Canceled = 1) and (TouchMoves > 0) then
  begin
    document.body.setAttribute('data-touch-gestures', 'passed');
  end;
end;

procedure TPhysicalProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LResponse: INyxGestureResponse;
  LSource: TJSHTMLElement;
begin
  document.body.setAttribute('data-probe-trigger', NyxTriggerName(AEvent.Trigger));
  LResponse := NyxGestureResponse(AExecution);
  case AEvent.Trigger of
    ntPointerDown:
      begin
        Require(LResponse.CanRequest(ngcCapturePointer), 'Physical down lacks capture authority');
        LResponse.CapturePointer;
      end;
    ntPointerUp:
      begin
        Require(LResponse.CanRequest(ngcReleasePointer), 'Physical up lost capture ownership');
        LResponse.ReleasePointer;
      end;
    ntPointerMove:
      begin
        LSource := Renderer.ElementFor('capture-button');

        if AEvent.Pointer.X > LSource.getBoundingClientRect.width then
        begin
          Require(TNyxGestureElement(LSource).hasPointerCapture(AEvent.Pointer.ID),
            'Outside pointer move arrived without actual capture');
          Inc(OutsideMoves);

          if AEvent.Pointer.Kind = npiTouch then
          begin
            Inc(TouchMoves);
          end;
        end;
      end;
    ntPointerCapture:
      begin
        Inc(Captured);
        Require(not LResponse.CanRequest(ngcCapturePointer),
          'Capture observation invented response authority');
      end;
    ntPointerCaptureLost:
      begin
        Inc(Lost);
      end;
    ntPointerCancel:
      begin
        Require(AEvent.HasPointer and (AEvent.Pointer.Kind = npiTouch) and
          not LResponse.CanRequest(ngcReleasePointer),
          'Physical touch cancellation invented a response or lost pointer kind');
        Inc(Canceled);
      end;
    ntDragStart:
      begin
        Inc(Started);
        Require(AEvent.HasDrag and AEvent.Drag.CanRespond,
          'Real dragstart lacks a synchronous offer window');
        LResponse.OfferDrag(NyxTransferText(CText), [ndoCopy, ndoMove]);
      end;
    ntDragOver:
      begin
        Inc(Hovered);
        Require(AEvent.HasDrag and not AEvent.Drag.Transfer.Readable and
          AEvent.Drag.Transfer.HasFormat(NyxTextTransferFormat) and
          (AEvent.Drag.Allowed = [ndoCopy, ndoMove]),
          'Real hover exposes data or loses allowed operations');
        LResponse.AcceptDrop(ndoCopy);
      end;
    ntDrop:
      begin
        Inc(Dropped);
        Require(AEvent.HasDrag and AEvent.Drag.Transfer.Readable and
          (AEvent.Drag.Transfer.TextFor(NyxTextTransferFormat) = CText),
          'Real drop loses owned Unicode payload');
        LResponse.AcceptDrop(ndoCopy);
      end;
    ntDragEnd:
      begin
        Inc(Ended);
        Require(AEvent.HasDrag and (AEvent.Drag.Operation = ndoCopy) and
          not AEvent.Drag.Transfer.Readable, 'Real dragend loses final host operation');
      end;
    else
    begin
      { This probe observes only the explicit physical registrations below.
        Other families do not request capture or transfer authority. }
    end;
  end;
  Publish;
end;

function HostPointer(AEvent: TJSPointerEvent): Boolean;
begin
  document.body.setAttribute('data-host-pointer', AEvent._type);
  document.body.setAttribute('data-host-subscribed', BoolToStr(
    GRenderer.Events.HasSubscribers(ntPointerDown), True));
  document.body.setAttribute('data-host-default', BoolToStr(AEvent.defaultPrevented, True));
  document.body.setAttribute('data-host-gesture-error', GRenderer.LastGestureError);
  document.body.setAttribute('data-host-pending-capture', BoolToStr(
    TNyxGestureElement(GRenderer.ElementFor('capture-button')).hasPointerCapture(AEvent.pointerId), True));

  if GSubscriptions[0].LastExecution <> nil then
  begin
    document.body.setAttribute('data-host-callback-failure', GSubscriptions[0].LastExecution.Failure);
    document.body.setAttribute('data-host-callback-status',
      IntToStr(Ord(GSubscriptions[0].LastExecution.Status)));
  end;
  Result := True;
end;

procedure Prepare;
var
  LPage: INyxPage;
  LCapture: INyxButton;
  LTransfer: INyxButton;
  LTarget: INyxCard;
  LIndex: Integer;
  LID: TNyxText;
begin
  GDocument := TNyxDocument.Create;
  LPage := NewNyxPage('physical-page');
  LPage.Configure.Width(500).Padding(20).Gap(24);
  LCapture := NewNyxButton('capture-button').WithText('Capture pointer');
  LCapture.Configure.Width(200).Height(60).TouchBehavior(ntbNone);
  LTransfer := NewNyxButton('transfer-button').WithText('Transfer owned text');
  LTransfer.Configure.Width(200).Height(60).DragSource(True);
  LTarget := NewNyxCard('drop-card').WithText('Copy destination');
  LTarget.Configure.Width(300).Height(140).DropTarget(True);
  LPage.Add(LCapture).Add(LTransfer).Add(LTarget);
  GDocument.AddPage(LPage.Node);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.Render(GDocument, GDocument.Pages[0], TJSHTMLElement(document.body));
  GProbe := TPhysicalProbe.Create;
  GProbe.Renderer := GRenderer;
  GProbeOwner := GProbe;
  SetLength(GSubscriptions, Length(CTriggers));
  for LIndex := 0 to High(CTriggers) do
  begin
    LID := 'capture-button';

    if CTriggers[LIndex] in [ntDragStart, ntDragEnd] then
    begin
      LID := 'transfer-button';
    end
    else if CTriggers[LIndex] in [ntDragOver, ntDrop] then
    begin
      LID := 'drop-card';
    end;
    GSubscriptions[LIndex] := GRenderer.Events.On(NyxControlEvents(LID),
      CTriggers[LIndex]).Subscribe(GProbe);
  end;
  document.body.setAttribute('data-physical-gestures', 'ready');
  GRenderer.ElementFor('capture-button').addEventListener('pointerdown', @HostPointer);
  GRenderer.ElementFor('capture-button').addEventListener('pointerup', @HostPointer);
  GProbe.Publish;
end;

begin
  try
    Prepare;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-physical-gestures', 'failed');
      document.body.setAttribute('data-physical-error', LException.Message);
    end;
  end;
end.
