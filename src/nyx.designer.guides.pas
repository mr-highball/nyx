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
unit nyx.designer.guides;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types;

const
  { One gesture holds a bounded copied snapshot, never an entire design tree. }
  NyxMaximumGuidePeers = 256;

type
  TNyxGuideAxis = (ngaWidth, ngaHeight);
  TNyxGuideKind = (ngkNone, ngkEqualSize, ngkEdge, ngkCenter);

  { Outer face in its immediate layout's logical coordinate plane. Finite signed
    positions allow scrolling; sizes are nonnegative. Default is undefined. }
  TNyxGuideBox = record
  private
    FLeft: Double;
    FTop: Double;
    FWidth: Double;
    FHeight: Double;
    FDefined: Boolean;
    function GetRight: Double;
    function GetBottom: Double;
  public
    function SameBox(const AOther: TNyxGuideBox): Boolean;
    property Defined: Boolean read FDefined;
    property Left: Double read FLeft;
    property Top: Double read FTop;
    property Width: Double read FWidth;
    property Height: Double read FHeight;
    property Right: Double read GetRight;
    property Bottom: Double read GetBottom;
  end;

  { Immutable explanation of one snapped dimension. Geometry and reference IDs
    are copied, with no widget/model ownership. Segment returns one alignment
    line or two equal-size measurement bars in the captured parent plane. }
  TNyxAlignmentGuide = record
  private
    FKind: TNyxGuideKind;
    FAxis: TNyxGuideAxis;
    FReference: TNyxControlRef;
    FOwner: TNyxGuideBox;
    FPeer: TNyxGuideBox;
    FCoordinate: Double;
    FSize: Integer;
  public
    function Segment(AIndex, AWidth, AHeight: Integer): TNyxGuideBox;
    function Caption: TNyxText;
    property Kind: TNyxGuideKind read FKind;
    property Axis: TNyxGuideAxis read FAxis;
    property Reference: TNyxControlRef read FReference;
    property OwnerBox: TNyxGuideBox read FOwner;
    property Size: Integer read FSize;
  end;

  { Captured visible sibling geometry. Peer allocates an independent array before
    appending; fluent copies never mutate a retained baseline. Positions may be
    enabled only when resizing keeps the owner's origin stable (absolute layout).
    Flow layouts still offer matching widths/heights. Default disables guides. }
  TNyxAlignmentContext = record
  private
    FOwner: TNyxGuideBox;
    FParent: TNyxGuideBox;
    FParentID: TNyxControlRef;
    FPeers: array of TNyxAlignmentGuide;
    FPositions: Boolean;
    FTolerance: Integer;
    function GetDefined: Boolean;
    function GetPeerCount: Integer;
  public
    function Peer(const AControl: TNyxControlRef;
      const ABox: TNyxGuideBox): TNyxAlignmentContext;
    function Positions(AEnabled: Boolean): TNyxAlignmentContext;
    function Tolerance(APixels: Integer): TNyxAlignmentContext;
    function SameContext(const AOther: TNyxAlignmentContext): Boolean;
    { Nearest eligible integer dimension within tolerance wins; ties prefer
      equal size, edge, center, then stable snapshot order. Bounds are checked
      before choosing, so an invalid nearest target cannot hide a valid one.
      No match returns the rounded desired size; policy owns grid/clamping. }
    function Snap(AAxis: TNyxGuideAxis; ADesired: Double; AMinimum, AMaximum: Integer;
      out AGuide: TNyxAlignmentGuide): Integer;
    property OwnerBox: TNyxGuideBox read FOwner;
    property Defined: Boolean read GetDefined;
    property PeerCount: Integer read GetPeerCount;
  end;

function NyxGuideBox(ALeft, ATop, AWidth, AHeight: Double): TNyxGuideBox;
{ Parent may be undefined, disabling layout edges/centers. The owner must be
  defined. All values belong to a single stable logical parent plane. }
function NyxAlignmentContext(const AOwner, AParent: TNyxGuideBox;
  const AParentID: TNyxControlRef): TNyxAlignmentContext;

implementation

uses SysUtils, Math, nyx.layout.constraints;

function NyxGuideBox(ALeft, ATop, AWidth, AHeight: Double): TNyxGuideBox;
begin

  if IsNan(ALeft) or IsInfinite(ALeft) or IsNan(ATop) or IsInfinite(ATop) or
    IsNan(AWidth) or IsInfinite(AWidth) or IsNan(AHeight) or IsInfinite(AHeight) or
    (AWidth < 0) or (AHeight < 0) or IsInfinite(ALeft + AWidth) or
    IsInfinite(ATop + AHeight) then
  begin
    raise EArgumentException.Create('Alignment geometry must be finite and nonnegative in size');
  end;
  Result := Default(TNyxGuideBox);
  Result.FLeft := ALeft;
  Result.FTop := ATop;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
  Result.FDefined := True;
end;

function TNyxGuideBox.GetRight: Double;
begin
  Result := FLeft + FWidth;
end;

function TNyxGuideBox.GetBottom: Double;
begin
  Result := FTop + FHeight;
end;

function TNyxGuideBox.SameBox(const AOther: TNyxGuideBox): Boolean;
begin
  Result := (FDefined = AOther.FDefined) and (FLeft = AOther.FLeft) and
    (FTop = AOther.FTop) and (FWidth = AOther.FWidth) and (FHeight = AOther.FHeight);
end;

function NyxAlignmentContext(const AOwner, AParent: TNyxGuideBox;
  const AParentID: TNyxControlRef): TNyxAlignmentContext;
begin

  if not AOwner.Defined or (AParent.Defined and (AParentID.ID = '')) then
  begin
    raise EArgumentException.Create('Alignment context requires exact owned geometry');
  end;
  Result := Default(TNyxAlignmentContext);
  Result.FOwner := AOwner;
  Result.FParent := AParent;
  Result.FParentID := AParentID;
  Result.FTolerance := 6;
end;

function TNyxAlignmentContext.Peer(const AControl: TNyxControlRef;
  const ABox: TNyxGuideBox): TNyxAlignmentContext;
var
  LIndex: Integer;
begin

  if not FOwner.Defined or not ABox.Defined or (AControl.ID = '') or
    (Length(FPeers) >= NyxMaximumGuidePeers) then
  begin
    raise EArgumentException.Create('Alignment peer requires exact geometry within the snapshot budget');
  end;
  for LIndex := 0 to High(FPeers) do
  begin

    if FPeers[LIndex].Reference.ID = AControl.ID then
    begin
      raise EArgumentException.Create('Alignment peer identity must be unique');
    end;
  end;
  Result := Self;
  Result.FPeers := nil;
  SetLength(Result.FPeers, Length(FPeers) + 1);
  { pas2js/FPC array semantics differ: allocate explicitly before copying. }
  for LIndex := 0 to High(FPeers) do
  begin
    Result.FPeers[LIndex] := FPeers[LIndex];
  end;
  Result.FPeers[High(Result.FPeers)] := Default(TNyxAlignmentGuide);
  Result.FPeers[High(Result.FPeers)].FReference := AControl;
  Result.FPeers[High(Result.FPeers)].FPeer := ABox;
end;

function TNyxAlignmentContext.GetDefined: Boolean;
begin
  Result := FOwner.Defined;
end;

function TNyxAlignmentContext.GetPeerCount: Integer;
begin
  Result := Length(FPeers);
end;

function TNyxAlignmentContext.Positions(AEnabled: Boolean): TNyxAlignmentContext;
begin
  Result := Self;
  Result.FPositions := AEnabled;
end;

function TNyxAlignmentContext.Tolerance(APixels: Integer): TNyxAlignmentContext;
begin

  if (APixels < 0) or (APixels > 64) then
  begin
    raise EArgumentException.Create('Alignment tolerance must be between zero and 64 logical pixels');
  end;
  Result := Self;
  Result.FTolerance := APixels;
end;

function TNyxAlignmentContext.SameContext(const AOther: TNyxAlignmentContext): Boolean;
var
  LIndex: Integer;
begin
  Result := FOwner.SameBox(AOther.FOwner) and FParent.SameBox(AOther.FParent) and
    (FParentID.ID = AOther.FParentID.ID) and (FPositions = AOther.FPositions) and
    (FTolerance = AOther.FTolerance) and (Length(FPeers) = Length(AOther.FPeers));

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FPeers) do
  begin

    if (FPeers[LIndex].Reference.ID <> AOther.FPeers[LIndex].Reference.ID) or
      not FPeers[LIndex].FPeer.SameBox(AOther.FPeers[LIndex].FPeer) then
    begin
      Exit(False);
    end;
  end;
end;

function TNyxAlignmentContext.Snap(AAxis: TNyxGuideAxis; ADesired: Double;
  AMinimum, AMaximum: Integer; out AGuide: TNyxAlignmentGuide): Integer;
var
  LDistance: Double;
  LIndex: Integer;

  procedure Consider(AKind: TNyxGuideKind; const AReference: TNyxControlRef;
    const APeer: TNyxGuideBox; ASize, ACoordinate: Double);
  var
    LSize: Integer;
    LDelta: Double;
  begin

    if (ASize < 0) or (ASize > MaximumNyxLayoutBound) then
    begin
      Exit;
    end;
    LSize := Integer(Floor(ASize + 0.5));
    LDelta := Abs(LSize - ADesired);

    if (LSize < AMinimum) or (LSize > AMaximum) or (LDelta > FTolerance) or
      (LDelta > LDistance) or ((LDelta = LDistance) and (AGuide.Kind <> ngkNone) and
      (Ord(AKind) >= Ord(AGuide.Kind))) then
    begin
      Exit;
    end;
    Result := LSize;
    LDistance := LDelta;
    AGuide.FKind := AKind;
    AGuide.FAxis := AAxis;
    AGuide.FReference := AReference;
    AGuide.FOwner := FOwner;
    AGuide.FPeer := APeer;
    AGuide.FCoordinate := ACoordinate;
    AGuide.FSize := LSize;
  end;

  procedure PositionsFor(const AReference: TNyxControlRef; const APeer: TNyxGuideBox);
  begin

    if AAxis = ngaWidth then
    begin
      Consider(ngkEdge, AReference, APeer, APeer.Left - FOwner.Left, APeer.Left);
      Consider(ngkEdge, AReference, APeer, APeer.Right - FOwner.Left, APeer.Right);
      Consider(ngkCenter, AReference, APeer,
        2 * (APeer.Left + APeer.Width / 2 - FOwner.Left), APeer.Left + APeer.Width / 2);
    end
    else
    begin
      Consider(ngkEdge, AReference, APeer, APeer.Top - FOwner.Top, APeer.Top);
      Consider(ngkEdge, AReference, APeer, APeer.Bottom - FOwner.Top, APeer.Bottom);
      Consider(ngkCenter, AReference, APeer,
        2 * (APeer.Top + APeer.Height / 2 - FOwner.Top), APeer.Top + APeer.Height / 2);
    end;
  end;

begin
  AGuide := Default(TNyxAlignmentGuide);

  if IsNan(ADesired) or IsInfinite(ADesired) or (ADesired < 0) or
    (ADesired > MaximumNyxLayoutBound) or (AMinimum < 0) or (AMaximum < AMinimum) or
    (AMaximum > MaximumNyxLayoutBound) or (Ord(AAxis) < Ord(Low(TNyxGuideAxis))) or
    (Ord(AAxis) > Ord(High(TNyxGuideAxis))) then
  begin
    raise EArgumentException.Create('Alignment requires valid dimension, axis and bounds');
  end;
  Result := Integer(Floor(ADesired + 0.5));
  LDistance := FTolerance + 1;

  if not FOwner.Defined then
  begin
    Exit;
  end;

  if FPositions and FParent.Defined then
  begin
    PositionsFor(FParentID, FParent);
  end;
  for LIndex := 0 to High(FPeers) do
  begin

    if AAxis = ngaWidth then
    begin
      Consider(ngkEqualSize, FPeers[LIndex].Reference, FPeers[LIndex].FPeer,
        FPeers[LIndex].FPeer.Width, 0);
    end
    else
    begin
      Consider(ngkEqualSize, FPeers[LIndex].Reference, FPeers[LIndex].FPeer,
        FPeers[LIndex].FPeer.Height, 0);
    end;

    if FPositions then
    begin
      PositionsFor(FPeers[LIndex].Reference, FPeers[LIndex].FPeer);
    end;
  end;
end;

function TNyxAlignmentGuide.Segment(AIndex, AWidth, AHeight: Integer): TNyxGuideBox;
var
  LStart: Double;
  LEnd: Double;
  LBox: TNyxGuideBox;
begin
  Result := Default(TNyxGuideBox);

  if (AWidth < 0) or (AHeight < 0) or (AWidth > MaximumNyxLayoutBound) or
    (AHeight > MaximumNyxLayoutBound) then
  begin
    raise EArgumentException.Create('Guide presentation requires valid proposed dimensions');
  end;

  if (FKind = ngkNone) or (AIndex < 0) or (AIndex > 1) then
  begin
    Exit;
  end;

  if FKind = ngkEqualSize then
  begin
    LBox := FPeer;

    if AIndex = 0 then
    begin
      LBox := NyxGuideBox(FOwner.Left, FOwner.Top, AWidth, AHeight);
    end;

    if FAxis = ngaWidth then
    begin
      Result := NyxGuideBox(LBox.Left, LBox.Top - 6, LBox.Width, 1);
    end
    else
    begin
      Result := NyxGuideBox(LBox.Left - 6, LBox.Top, 1, LBox.Height);
    end;
    Exit;
  end;

  if AIndex <> 0 then
  begin
    Exit;
  end;

  if FAxis = ngaWidth then
  begin
    LStart := Min(FOwner.Top, FPeer.Top) - 6;
    LEnd := Max(FOwner.Top + AHeight, FPeer.Bottom) + 6;
    Result := NyxGuideBox(FCoordinate, LStart, 1, LEnd - LStart);
  end
  else
  begin
    LStart := Min(FOwner.Left, FPeer.Left) - 6;
    LEnd := Max(FOwner.Left + AWidth, FPeer.Right) + 6;
    Result := NyxGuideBox(LStart, FCoordinate, LEnd - LStart, 1);
  end;
end;

function TNyxAlignmentGuide.Caption: TNyxText;
const
  CAxes: array[TNyxGuideAxis] of TNyxText = ('Width', 'Height');
  CKinds: array[TNyxGuideKind] of TNyxText = ('', ' matches ', ' aligns with ', ' centers on ');
begin

  if FKind = ngkNone then
  begin
    Exit('');
  end;
  Result := CAxes[FAxis] + CKinds[FKind] + FReference.ID;
end;

end.
