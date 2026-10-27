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

unit nyx.canvas.observers;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.scheduler, nyx.canvas.types, nyx.canvas.geometry;

type
  { Optional UI-thread observation of the mounted control-scaling policy.
    Standard views snapshot it at construction; changing a saved definition
    never mutates a retained view. Retirement refuses access. This copied enum
    retains no workspace/renderer and grants no camera or document authority. }
  INyxCanvasScalingView = interface(IInterface)
    ['{F330FE0C-230D-47DF-BBF4-D6F561986F54}']
    function GetControlScaling: TNyxCanvasControlScaling;
    property ControlScaling: TNyxCanvasControlScaling read GetControlScaling;
  end;

  { Optional UI-only actual-control geometry in viewport logical pixels. False
    leaves bounds undefined when the exact item/control is absent, hidden or
    outside the allocated surface. Returned geometry owns no widget and grants
    no mutation authority; route operations still require their own revisions. }
  INyxCanvasControlGeometry = interface(IInterface)
    ['{9877F9FB-D992-4701-8489-B051D45688B7}']
    function TryControlBounds(const AItem: TNyxCanvasItemRef;
      const AControl: TNyxControlRef; out ABounds: TNyxCanvasBounds): Boolean;
  end;

  { UI-only notification after the camera and actual item frames are installed.
    The observer must not retain the view it observes. It may read the complete
    frame or queue UI work; synchronous presentation writes remain reserved.
    A notification grants no document, runtime or widget authority. }
  INyxCanvasPresentationObserver = interface(IInterface)
    ['{E1273123-4816-47C8-9009-27250AFD1524}']
    procedure Presented(ARevision: Integer);
  end;

  { Optional presentation subscription leaves original workspace interfaces
    unchanged. The returned cancellation scope revokes only this observation;
    a caller cancels it before retiring its borrowed receiver. Initial delivery
    is explicit: subscribing does not call the observer. New observers start on
    the next notification snapshot. All operations/final releases belong to UI. }
  INyxCanvasPresentationObservation = interface(IInterface)
    ['{4F054105-330C-412F-8961-98151CF2216A}']
    function ObservePresentation(const AObserver: INyxCanvasPresentationObserver): INyxCancellationScope;
  end;

  { View-owned shared notification hub. It retains observer slots, never the
    canvas/view itself. Close revokes every slot; an independently retained scope
    safely outlives the hub. At most 256 active observers bound a UI frame's
    callback work; overflow refuses subscription before changing membership.
    Notification failures report committed presentation
    through ENyxPublicationNotification after all eligible observers were called. }
  TNyxCanvasObservers = class
  private
    FSlots: array of TObject;
    FLeases: array of INyxCancellationScope;
    FClosed: Boolean;
    FNotifying: Boolean;
    procedure Prune;
  public
    destructor Destroy; override;
    function Subscribe(const AObserver: INyxCanvasPresentationObserver): INyxCancellationScope;
    procedure Notify(ARevision: Integer);
    procedure Close;
  end;

implementation

uses nyx.publication;

type
  { One independently revocable observer lease. Cancelling releases its managed
    observer immediately; it retains neither the hub nor its owning canvas. }
  TPresentationSlot = class(TInterfacedObject, INyxCancellationScope)
  public
    Observer: INyxCanvasPresentationObserver;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

procedure TPresentationSlot.Cancel;
begin
  Observer := nil;
end;

function TPresentationSlot.GetCancelled: Boolean;
begin
  Result := Observer = nil;
end;

destructor TNyxCanvasObservers.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TNyxCanvasObservers.Close;
var
  LIndex: Integer;
begin
  FClosed := True;
  for LIndex := 0 to High(FLeases) do
  begin
    FLeases[LIndex].Cancel;
  end;
  FSlots := nil;
  FLeases := nil;
end;

procedure TNyxCanvasObservers.Prune;
var
  LIndex: Integer;
  LCount: Integer;
begin
  LCount := 0;
  for LIndex := 0 to High(FLeases) do
  begin

    if not FLeases[LIndex].Cancelled then
    begin
      FSlots[LCount] := FSlots[LIndex];
      FLeases[LCount] := FLeases[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(FSlots, LCount);
  SetLength(FLeases, LCount);
end;

function TNyxCanvasObservers.Subscribe(
  const AObserver: INyxCanvasPresentationObserver): INyxCancellationScope;
var
  LSlot: TPresentationSlot;
  LCount: Integer;
begin

  if FClosed or (AObserver = nil) then
  begin
    raise ENyxSchedule.Create('A live canvas and managed presentation observer are required');
  end;
  Prune;

  if Length(FSlots) = 256 then
  begin
    raise ENyxSchedule.Create('A canvas admits at most 256 active presentation observers');
  end;
  LSlot := TPresentationSlot.Create;
  Result := LSlot;
  LSlot.Observer := AObserver;
  LCount := Length(FSlots);
  SetLength(FSlots, LCount + 1);
  SetLength(FLeases, LCount + 1);
  FSlots[LCount] := LSlot;
  FLeases[LCount] := Result;
end;

procedure TNyxCanvasObservers.Notify(ARevision: Integer);
var
  LSlots: array of TObject;
  LLeases: array of INyxCancellationScope;
  LObserver: INyxCanvasPresentationObserver;
  LIndex: Integer;
  LFailure: TNyxText;
  LFailed: Boolean;
begin

  if FClosed then
  begin
    Exit;
  end;

  if FNotifying then
  begin
    raise ENyxSchedule.Create('Canvas presentation observers cannot recursively notify');
  end;
  Prune;
  SetLength(LSlots, Length(FSlots));
  SetLength(LLeases, Length(FLeases));
  for LIndex := 0 to High(FSlots) do
  begin
    LSlots[LIndex] := FSlots[LIndex];
    LLeases[LIndex] := FLeases[LIndex];
  end;
  FNotifying := True;
  LFailure := '';
  LFailed := False;
  try
    for LIndex := 0 to High(LSlots) do
    begin

      if FClosed or LLeases[LIndex].Cancelled then
      begin
        Continue;
      end;
      LObserver := TPresentationSlot(LSlots[LIndex]).Observer;
      try
        LObserver.Presented(ARevision);
      except
        on E: Exception do
        begin

          if not LFailed then
          begin
            LFailure := E.Message;
          end;
          LFailed := True;
        end;
      end;
    end;
  finally
    FNotifying := False;
  end;

  if LFailed then
  begin
    raise ENyxPublicationNotification.CreateReceiverFailure(LFailure);
  end;
end;

end.
