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
unit nyx.designer.placement;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.designer.guides;

type
  TNyxPlacementEdge = (npeInside, npeBefore, npeAfter);
  TNyxPlacementAxis = (npaUnknown, npaHorizontal, npaVertical);

  { Copied physical face and logical outer size. Face is in the adapter's screen
    plane; input is expressed in local logical pixels. Parent/axis describe the
    actual realized flow, never a guessed catalog default or authored override.
    Unknown means no automatic sibling placement. No widget/tree is retained. }
  TNyxDropFrame = record
  private
    FFace: TNyxGuideBox;
    FWidth: Double;
    FHeight: Double;
    FParent: TNyxControlRef;
    FAxis: TNyxPlacementAxis;
    FContainer: Boolean;
    function GetDefined: Boolean;
  public
    function SameFrame(const AOther: TNyxDropFrame): Boolean;
    property Defined: Boolean read GetDefined;
    property Face: TNyxGuideBox read FFace;
    property Width: Double read FWidth;
    property Height: Double read FHeight;
    property Parent: TNyxControlRef read FParent;
    property Axis: TNyxPlacementAxis read FAxis;
    property Container: Boolean read FContainer;
  end;

  { Immutable fluent choice. Automatic row/column placement uses the leading or
    trailing edge band on containers, and the corresponding half on leaves.
    A container's middle accepts Inside. Root containers accept Inside only.
    Absolute/grid/unknown parent axes offer Inside on containers and refuse
    automatic sibling placement. Explicit choices remain independently available.
    Bands are logical pixels, capped to a quarter of the current face extent. }
  TNyxDropPolicy = record
  private
    FAutomatic: Boolean;
    FPlacement: TNyxPlacementEdge;
    FEdgeBand: Integer;
  public
    function Automatic(AEnabled: Boolean = True): TNyxDropPolicy;
    function Explicit(APlacement: TNyxPlacementEdge): TNyxDropPolicy;
    function EdgeBand(APixels: Integer): TNyxDropPolicy;
    { Undefined/outside/nonfinite geometry refuses. A true result is still only
      presentation: the host must validate ownership and admit its command. }
    function Resolve(const AFrame: TNyxDropFrame; AX, AY: Double;
      out APlacement: TNyxPlacementEdge): Boolean;
    property IsAutomatic: Boolean read FAutomatic;
  end;

  { Copied transient marker. Origin is the exact runtime primitive identity,
    rather than its editable owner or a reusable definition's unqualified ID.
    Default clears paint. No selection, document or mutation capability exists. }
  TNyxDropPreview = record
  private
    FOrigin: TNyxControlRef;
    FFrame: TNyxDropFrame;
    FPlacement: TNyxPlacementEdge;
    function GetActive: Boolean;
  public
    { Four outline strips for Inside/unknown axis; one insertion strip for
      Before/After on a known axis. Undefined segments are intentionally inert. }
    function Segment(AIndex: Integer): TNyxGuideBox;
    { English presentation text; never parsed to recover mutation intent. }
    function Caption: TNyxText;
    property Active: Boolean read GetActive;
    property Origin: TNyxControlRef read FOrigin;
    property Frame: TNyxDropFrame read FFrame;
    property Placement: TNyxPlacementEdge read FPlacement;
  end;

{ Positive finite geometry and a closed axis are required. Parent may be absent
  only for a root; its unknown axis makes that fact explicit. }
function NyxDropFrame(const AFace: TNyxGuideBox; AWidth, AHeight: Double;
  const AParent: TNyxControlRef; AAxis: TNyxPlacementAxis;
  AContainer: Boolean): TNyxDropFrame;
{ Ordinary defaults retain explicit Inside; automatic is an explicit opt-in. }
function NyxDropPolicy: TNyxDropPolicy;
{ Requires exact identity, a defined frame and a closed placement edge. }
function NyxDropPreview(const AOrigin: TNyxControlRef; const AFrame: TNyxDropFrame;
  APlacement: TNyxPlacementEdge): TNyxDropPreview;

implementation

uses SysUtils, Math;

function Finite(AValue: Double): Boolean;
begin
  Result := not IsNan(AValue) and not IsInfinite(AValue);
end;

function NyxDropFrame(const AFace: TNyxGuideBox; AWidth, AHeight: Double;
  const AParent: TNyxControlRef; AAxis: TNyxPlacementAxis;
  AContainer: Boolean): TNyxDropFrame;
begin

  if not AFace.Defined or (AFace.Width <= 0) or (AFace.Height <= 0) or
    not Finite(AWidth) or not Finite(AHeight) or (AWidth <= 0) or (AHeight <= 0) or
    (Ord(AAxis) < Ord(Low(TNyxPlacementAxis))) or
    (Ord(AAxis) > Ord(High(TNyxPlacementAxis))) or
    ((AParent.ID = '') and (AAxis <> npaUnknown)) then
  begin
    raise EArgumentException.Create('Drop geometry requires a positive copied face and exact parent axis');
  end;
  Result := Default(TNyxDropFrame);
  Result.FFace := AFace;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
  Result.FParent := AParent;
  Result.FAxis := AAxis;
  Result.FContainer := AContainer;
end;

function TNyxDropFrame.SameFrame(const AOther: TNyxDropFrame): Boolean;
begin
  Result := FFace.SameBox(AOther.FFace) and (FWidth = AOther.FWidth) and
    (FHeight = AOther.FHeight) and (FParent.ID = AOther.FParent.ID) and
    (FAxis = AOther.FAxis) and (FContainer = AOther.FContainer);
end;

function TNyxDropFrame.GetDefined: Boolean;
begin
  Result := FFace.Defined;
end;

function NyxDropPolicy: TNyxDropPolicy;
begin
  Result := Default(TNyxDropPolicy);
  Result.FEdgeBand := 24;
end;

function TNyxDropPolicy.Automatic(AEnabled: Boolean): TNyxDropPolicy;
begin
  Result := Self;
  Result.FAutomatic := AEnabled;

  if Result.FEdgeBand = 0 then
  begin
    Result.FEdgeBand := 24;
  end;
end;

function TNyxDropPolicy.Explicit(APlacement: TNyxPlacementEdge): TNyxDropPolicy;
begin

  if (Ord(APlacement) < Ord(Low(TNyxPlacementEdge))) or
    (Ord(APlacement) > Ord(High(TNyxPlacementEdge))) then
  begin
    raise EArgumentException.Create('Drop placement requires Inside, Before or After');
  end;
  Result := Self;
  Result.FAutomatic := False;
  Result.FPlacement := APlacement;
end;

function TNyxDropPolicy.EdgeBand(APixels: Integer): TNyxDropPolicy;
begin

  if (APixels < 1) or (APixels > 128) then
  begin
    raise EArgumentException.Create('Drop edge band must be 1..128 logical pixels');
  end;
  Result := Self;
  Result.FEdgeBand := APixels;
end;

function TNyxDropPolicy.Resolve(const AFrame: TNyxDropFrame; AX, AY: Double;
  out APlacement: TNyxPlacementEdge): Boolean;
var
  LPosition: Double;
  LExtent: Double;
  LBand: Double;
begin
  Result := False;
  APlacement := FPlacement;

  if not AFrame.Defined or not Finite(AX) or not Finite(AY) or
    (AX < 0) or (AY < 0) or (AX > AFrame.Width) or (AY > AFrame.Height) then
  begin
    Exit;
  end;

  if not FAutomatic then
  begin
    Exit(((FPlacement = npeInside) and AFrame.Container) or
      ((FPlacement <> npeInside) and (AFrame.Parent.ID <> '')));
  end;

  if (AFrame.Parent.ID = '') or (AFrame.Axis = npaUnknown) then
  begin
    APlacement := npeInside;
    Exit(AFrame.Container);
  end;
  LPosition := AY;
  LExtent := AFrame.Height;

  if AFrame.Axis = npaHorizontal then
  begin
    LPosition := AX;
    LExtent := AFrame.Width;
  end;

  if AFrame.Container then
  begin
    LBand := Min(FEdgeBand, LExtent / 4);
    APlacement := npeInside;

    if LPosition < LBand then
    begin
      APlacement := npeBefore;
    end
    else if LPosition >= LExtent - LBand then
    begin
      APlacement := npeAfter;
    end;
  end
  else if LPosition < LExtent / 2 then
  begin
    APlacement := npeBefore;
  end
  else
  begin
    APlacement := npeAfter;
  end;
  Result := True;
end;

function NyxDropPreview(const AOrigin: TNyxControlRef; const AFrame: TNyxDropFrame;
  APlacement: TNyxPlacementEdge): TNyxDropPreview;
begin

  if (AOrigin.ID = '') or not AFrame.Defined or
    (Ord(APlacement) < Ord(Low(TNyxPlacementEdge))) or
    (Ord(APlacement) > Ord(High(TNyxPlacementEdge))) then
  begin
    raise EArgumentException.Create('Drop preview requires exact runtime identity, geometry and placement');
  end;
  Result := Default(TNyxDropPreview);
  Result.FOrigin := AOrigin;
  Result.FFrame := AFrame;
  Result.FPlacement := APlacement;
end;

function TNyxDropPreview.GetActive: Boolean;
begin
  Result := (FOrigin.ID <> '') and FFrame.Defined;
end;

function TNyxDropPreview.Caption: TNyxText;
begin
  Result := '';

  if not Active then
  begin
    Exit;
  end;
  case FPlacement of
    npeInside: Result := 'Drop inside';
    npeBefore: Result := 'Drop before';
    npeAfter: Result := 'Drop after';
  end;
  Result := Result + TNyxText(' · ') + FOrigin.ID;
end;

function TNyxDropPreview.Segment(AIndex: Integer): TNyxGuideBox;
var
  LPosition: Double;
begin
  Result := Default(TNyxGuideBox);

  if not Active or (AIndex < 0) or (AIndex > 3) then
  begin
    Exit;
  end;

  if (FPlacement <> npeInside) and (FFrame.Axis <> npaUnknown) then
  begin

    if AIndex <> 0 then
    begin
      Exit;
    end;
    LPosition := -2;

    if FFrame.Axis = npaHorizontal then
    begin

      if FPlacement = npeAfter then
      begin
        LPosition := FFrame.Width;
      end;
      Exit(NyxGuideBox(LPosition, -2, 3, FFrame.Height + 4));
    end;

    if FPlacement = npeAfter then
    begin
      LPosition := FFrame.Height;
    end;
    Exit(NyxGuideBox(-2, LPosition, FFrame.Width + 4, 3));
  end;
  case AIndex of
    0: Result := NyxGuideBox(-2, -2, FFrame.Width + 4, 2);
    1: Result := NyxGuideBox(-2, FFrame.Height, FFrame.Width + 4, 2);
    2: Result := NyxGuideBox(-2, 0, 2, FFrame.Height);
    3: Result := NyxGuideBox(FFrame.Width, 0, 2, FFrame.Height);
  end;
end;

end.
