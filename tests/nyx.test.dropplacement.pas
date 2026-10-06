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
unit nyx.test.dropplacement;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Numerical/ownership boundary only. Actual layout, paint and publication are
  qualified by the unchanged semantic-source browser/Win32 Studio consumers. }
function RunNyxDropPolicyChecks: Integer;

implementation

uses SysUtils, Math, nyx.text, nyx.types, nyx.designer.guides, nyx.designer.placement;

function RunNyxDropPolicyChecks: Integer;
var
  LChecks: Integer;
  LFrame: TNyxDropFrame;
  LRoot: TNyxDropFrame;
  LLeaf: TNyxDropFrame;
  LPolicy: TNyxDropPolicy;
  LExplicit: TNyxDropPolicy;
  LEdge: TNyxPlacementEdge;
  LPreview: TNyxDropPreview;
  LRefused: Boolean;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Drop policy: ' + AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LFrame := NyxDropFrame(NyxGuideBox(20, 30, 240, 180), 240, 180,
    NyxControl('workspace'), npaHorizontal, True);
  LExplicit := NyxDropPolicy;
  LPolicy := LExplicit.Automatic.EdgeBand(10);
  Check(not LExplicit.IsAutomatic and LPolicy.IsAutomatic, 'fluent configuration preserves its baseline');
  Check(LExplicit.Resolve(LFrame, 120, 90, LEdge) and (LEdge = npeInside), 'ordinary default retains explicit inside');
  Check(LPolicy.Resolve(LFrame, 9, 90, LEdge) and (LEdge = npeBefore), 'leading row edge offers before');
  Check(LPolicy.Resolve(LFrame, 10, 90, LEdge) and (LEdge = npeInside), 'exact leading boundary belongs to inside');
  Check(LPolicy.Resolve(LFrame, 230, 90, LEdge) and (LEdge = npeAfter), 'exact trailing boundary belongs to after');
  Check(LPolicy.Resolve(LFrame, 120, 2, LEdge) and (LEdge = npeInside), 'cross-axis edges do not change row intent');
  LFrame := NyxDropFrame(LFrame.Face, 240, 180, LFrame.Parent, npaVertical, True);
  Check(LPolicy.Resolve(LFrame, 120, 1, LEdge) and (LEdge = npeBefore), 'column uses its vertical leading edge');
  LLeaf := NyxDropFrame(LFrame.Face, 240, 180, LFrame.Parent, npaVertical, False);
  Check(LPolicy.Resolve(LLeaf, 120, 89, LEdge) and (LEdge = npeBefore), 'leaf leading half offers before');
  Check(LPolicy.Resolve(LLeaf, 120, 90, LEdge) and (LEdge = npeAfter), 'leaf middle belongs to trailing half');
  Check(not LExplicit.Resolve(LLeaf, 120, 90, LEdge), 'inside never treats a leaf as a container');
  LRoot := NyxDropFrame(LFrame.Face, 240, 180, Default(TNyxControlRef), npaUnknown, True);
  Check(LPolicy.Resolve(LRoot, 0, 0, LEdge) and (LEdge = npeInside), 'root has no sibling insertion');
  Check(not LExplicit.Explicit(npeBefore).Resolve(LRoot, 0, 0, LEdge), 'explicit before root refuses');
  LLeaf := NyxDropFrame(LFrame.Face, 240, 180, LFrame.Parent, npaUnknown, False);
  Check(not LPolicy.Resolve(LLeaf, 1, 1, LEdge), 'unknown flow never guesses an automatic axis');
  Check(LExplicit.Explicit(npeAfter).Resolve(LLeaf, 1, 1, LEdge), 'explicit sibling intent remains available');
  Check(not LPolicy.Resolve(LFrame, -1, 1, LEdge) and
    not LPolicy.Resolve(LFrame, 1, 181, LEdge), 'outside geometry refuses');
  Check(not LPolicy.Resolve(Default(TNyxDropFrame), 0, 0, LEdge) and
    not LPolicy.Resolve(LFrame, Infinity, 1, LEdge), 'undefined and nonfinite input refuse');
  LPreview := NyxDropPreview(NyxControl(TNyxText('source-🙂')), LFrame, npeBefore);
  Check((LPreview.Segment(0).Height = 3) and not LPreview.Segment(1).Defined,
    'known vertical insertion paints one inert strip');
  Check(LPreview.Caption = TNyxText('Drop before · source-🙂'), 'caption retains exact supplementary identity');
  LPreview := NyxDropPreview(NyxControl('target'), LFrame, npeInside);
  Check(LPreview.Segment(3).Defined and not LPreview.Segment(4).Defined,
    'inside paints a bounded four-strip outline');
  Check(not Default(TNyxDropPreview).Active, 'default explicitly clears presentation');
  LRefused := False;
  try
    LPolicy.EdgeBand(129);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unbounded edge policies refuse');
  LRefused := False;
  try
    NyxDropFrame(LFrame.Face, 0, 180, LFrame.Parent, npaVertical, True);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'empty logical allocation cannot masquerade as a target');
  Result := LChecks;
end;

end.
