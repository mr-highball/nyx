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

unit nyx.canvas.navigation.sources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.scheduler, nyx.canvas.geometry, nyx.canvas.navigation;

type
  { Copied runtime-only overlay contribution. Arrays contain inert geometry;
    sources return no view, callback or execution authority in this frame. }
  TNyxCanvasNavigationFrame = record
    Items: TNyxCanvasNavigationItems;
    Occluders: TNyxCanvasNavigationOccluders;
  end;

  { UI-owned overlay inspection. Implementations must not retain their host
    canvas through this subscription. Capture may return empty when hidden or
    retired; the hub copies/admit values before entering another source. }
  INyxCanvasNavigationSource = interface(IInterface)
    ['{73DAB41D-8E4D-46D6-A65F-413E3AC2BC01}']
    function Capture(const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
  end;

  { Optional live view registration, preserving the original workspace ABI.
    Cancellation revokes only this contribution; a retained scope safely
    outlives the view. New registrations begin at the next capture window. }
  INyxCanvasNavigationContributions = interface(IInterface)
    ['{73DAB41D-8E4D-46D6-A65F-413E3AC2BC02}']
    function ObserveNavigation(const ASource: INyxCanvasNavigationSource): INyxCancellationScope;
    { Fit the current scene and copied overlays through ordinary presentation.
      A direct caller chooses its preferences; the old Fit command retains its
      original behavior when every overlay contribution is empty. }
    function FitNavigation(const AOptions: TNyxCanvasNavigationOptions): TNyxCanvasNavigationResult;
  end;

  { View-owned bounded hub retains source slots, never a canvas/view. All
    operations and destruction belong to its immutable scheduler's UI. Close
    revokes receivers; the owner destroys the hub only after active calls return.
    At most 32 sources and shared geometry budgets bound a capture. }
  TNyxCanvasNavigationSources = class
  private
    FScheduler: INyxScheduler;
    FSlots: array of TObject;
    FLeases: array of INyxCancellationScope;
    FClosed: Boolean;
    FCollecting: Boolean;
    procedure Prune;
  public
    constructor Create(const AScheduler: INyxScheduler);
    destructor Destroy; override;
    function Subscribe(const ASource: INyxCanvasNavigationSource): INyxCancellationScope;
    function Capture(const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
    procedure Close;
  end;

implementation

type
  { Independently revocable lease owns one managed source and the scheduler
    identity used to reject workers before reading that source. No hub/view
    backreference survives cancellation or host retirement. }
  TNavigationSlot = class(TInterfacedObject, INyxCancellationScope)
  public
    Scheduler: INyxScheduler;
    Source: INyxCanvasNavigationSource;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

procedure TNavigationSlot.Cancel;
begin
  Scheduler.RequireUI;
  Source := nil;
end;

function TNavigationSlot.GetCancelled: Boolean;
begin
  Scheduler.RequireUI;
  Result := Source = nil;
end;

constructor TNyxCanvasNavigationSources.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;

  if AScheduler = nil then
  begin
    raise ENyxSchedule.Create('Navigation contributions require their UI scheduler');
  end;
  AScheduler.RequireUI;
  FScheduler := AScheduler;
end;

destructor TNyxCanvasNavigationSources.Destroy;
begin

  if FScheduler <> nil then
  begin
    Close;
  end;
  inherited Destroy;
end;

procedure TNyxCanvasNavigationSources.Close;
var
  LIndex: Integer;
begin
  FScheduler.RequireUI;
  FClosed := True;
  for LIndex := 0 to High(FLeases) do
  begin
    FLeases[LIndex].Cancel;
  end;
end;

procedure TNyxCanvasNavigationSources.Prune;
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

function TNyxCanvasNavigationSources.Subscribe(
  const ASource: INyxCanvasNavigationSource): INyxCancellationScope;
var
  LSource: INyxCanvasNavigationSource;
  LSlot: TNavigationSlot;
begin
  FScheduler.RequireUI;
  LSource := ASource;

  if FClosed or (LSource = nil) then
  begin
    raise ENyxSchedule.Create('Navigation subscription requires an active hub and receiver');
  end;

  if not FCollecting then
  begin
    Prune;
  end;

  if Length(FLeases) >= 32 then
  begin
    raise ENyxSchedule.Create('Navigation source capacity is exhausted');
  end;
  LSlot := TNavigationSlot.Create;
  Result := LSlot;
  LSlot.Scheduler := FScheduler;
  LSlot.Source := LSource;
  SetLength(FSlots, Length(FSlots) + 1);
  SetLength(FLeases, Length(FLeases) + 1);
  FSlots[High(FSlots)] := LSlot;
  FLeases[High(FLeases)] := Result;
end;

function TNyxCanvasNavigationSources.Capture(
  const ACamera: TNyxCanvasCamera): TNyxCanvasNavigationFrame;
var
  LSlot: TNavigationSlot;
  LSource: INyxCanvasNavigationSource;
  LFrame: TNyxCanvasNavigationFrame;
  LItem: TNyxCanvasNavigationItem;
  LBounds: TNyxCanvasBounds;
  LIndex: Integer;
  LEntry: Integer;
  LCount: Integer;
  LBase: Integer;
begin
  FScheduler.RequireUI;

  if FClosed or FCollecting or not ACamera.Defined then
  begin
    raise ENyxSchedule.Create('Navigation capture requires an active nonrecursive UI frame');
  end;
  Result := Default(TNyxCanvasNavigationFrame);
  LCount := Length(FSlots);
  FCollecting := True;
  try
    for LIndex := 0 to LCount - 1 do
    begin
      LSlot := TNavigationSlot(FSlots[LIndex]);
      LSource := LSlot.Source;

      if LSource = nil then
      begin
        Continue;
      end;
      LFrame := LSource.Capture(ACamera);

      if FClosed then
      begin
        raise ENyxSchedule.Create('Navigation host retired during capture');
      end;

      if LSlot.Source = nil then
      begin
        Continue;
      end;

      if (Length(LFrame.Items) > MaximumNyxCanvasNavigationItems - Length(Result.Items)) or
        (Length(LFrame.Occluders) > MaximumNyxCanvasNavigationOccluders - Length(Result.Occluders)) then
      begin
        raise ENyxSchedule.Create('Navigation contributions exceed the shared geometry budget');
      end;
      LBase := Length(Result.Items);
      SetLength(Result.Items, LBase + Length(LFrame.Items));
      for LEntry := 0 to High(LFrame.Items) do
      begin
        LItem := LFrame.Items[LEntry];
        Result.Items[LBase + LEntry] := NyxCanvasNavigationItem(LItem.Bounds, LItem.Scaling, LItem.Independent);
      end;
      LBase := Length(Result.Occluders);
      SetLength(Result.Occluders, LBase + Length(LFrame.Occluders));
      for LEntry := 0 to High(LFrame.Occluders) do
      begin
        LBounds := LFrame.Occluders[LEntry];

        if not LBounds.Defined then
        begin
          raise ENyxSchedule.Create('Navigation contributions contain an undefined occluder');
        end;
        Result.Occluders[LBase + LEntry] := NyxCanvasBounds(
          LBounds.Left, LBounds.Top, LBounds.Width, LBounds.Height);
      end;
    end;
  finally
    FCollecting := False;
  end;
end;

end.
