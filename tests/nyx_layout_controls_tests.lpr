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
program nyx_layout_controls_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.layout.flow,
  nyx.layout.policy, nyx.controls, nyx.codec, nyx.source,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  {$else}TRenderer = TNyxLCLRenderer;
  TFace = TControl;{$endif}

var
  GDocument: TNyxDocument;
  GRenderer: TRenderer;
  GChecks: Integer;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;
  GFrame: TJSHTMLIFrameElement;
  {$else}GHost: TForm;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Node(const AID: TNyxText): TNyxNode;
begin
  Result := GRenderer.Root.Find(AID);
  Check(Result <> nil, 'MCP companion contains ' + AID);
end;

function Face(const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}Result := GRenderer.ElementFor(AID, niRuntime);
  {$else}Result := GRenderer.ControlFor(AID, niRuntime);{$endif}
end;

function Width(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}Result := Round(Face(AID).offsetWidth);
  {$else}Result := Face(AID).Width;{$endif}
end;

function Height(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}Result := Round(Face(AID).offsetHeight);
  {$else}Result := Face(AID).Height;{$endif}
end;

function Left(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}Result := Round(Face(AID).getBoundingClientRect.left -
    Face(Node(AID).Parent.ID).getBoundingClientRect.left);
  {$else}Result := Face(AID).Left;{$endif}
end;

function Top(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}Result := Round(Face(AID).getBoundingClientRect.top -
    Face(Node(AID).Parent.ID).getBoundingClientRect.top);
  {$else}Result := Face(AID).Top;{$endif}
end;

{ Native integer bounds and browser fractional CSS allocation can differ by one
  rounded logical pixel. Check measured geometry, never just authored styles. }
procedure Near(AActual, AExpected: Integer; const AReason: TNyxText);
begin
  Check(Abs(AActual - AExpected) <= 1, AReason + ': ' + IntToStr(AActual) +
    ' expected ' + IntToStr(AExpected));
end;

procedure Arithmetic;
var
  LItems: TNyxFlowItems;
  LSizes: TNyxFlowSizes;
  LWidth: Integer;
begin
  SetLength(LItems, 4);
  LItems[0].Visible := True;
  LItems[0].NaturalSize := 50;
  LItems[1].Visible := True;
  LItems[1].Weight := 1;
  LItems[2].Visible := True;
  LItems[2].Weight := 2;
  LItems[3].Visible := False;
  LItems[3].NaturalSize := High(Integer);
  LItems[3].Weight := High(Integer);
  LSizes := NyxFlowSizes(300, 7, LItems);
  Check((LSizes[0] = 50) and (LSizes[1] = 78) and (LSizes[2] = 158) and
    (LSizes[3] = 0), 'known allocation excludes hidden size, weight and gap');
  for LWidth := 64 to 1064 do
  begin
    LSizes := NyxFlowSizes(LWidth, 7, LItems);
    Check(LSizes[0] + LSizes[1] + LSizes[2] + 14 = LWidth,
      'every odd/even available pixel is assigned');
    Check(Abs(LSizes[2] - 2 * LSizes[1]) <= 2, 'allocation retains the 1:2 ratio');
  end;
  LItems[1].Weight := 100000;
  LItems[2].Weight := 200000;
  LSizes := NyxFlowSizes(High(Integer), 7, LItems);
  Check((LSizes[1] = 715827861) and (LSizes[2] = 1431655722),
    'large weighted products never overflow native Integer');
  LItems[0].NaturalSize := High(Integer);
  LSizes := NyxFlowSizes(1, 7, LItems);
  Check((LSizes[0] = High(Integer)) and (LSizes[1] = 0) and (LSizes[2] = 0),
    'fixed overflow never becomes negative flexible geometry');
  Check((LItems[3].Weight = High(Integer)) and not LItems[3].Visible,
    'the allocator retains its borrowed baseline');
  SetLength(LItems, 0);
  LSizes := NyxFlowSizes(-1, -1, LItems);
  Check(Length(LSizes) = 0, 'empty/negative available space has a fresh empty result');
end;

procedure Review;
var
  LRow: TNyxNode;
  LColumn: TNyxNode;
  LNatural: TNyxNode;
  LFirst: TNyxNode;
  LSecond: TNyxNode;
  LFixed: TNyxNode;
  LFace: TFace;
  LMemoFace: TFace;
  LListFace: TFace;
  LSplitFace: TFace;
  LIndex: Integer;
  LVisible: Boolean;
  {$ifdef PAS2JS}LMemo: TJSHTMLTextAreaElement;
  LFirstItem: TJSNode;
  {$else}LMemo: TMemo;{$endif}
begin
  GRenderer.Render(GDocument, GDocument.Find('layout-review'), GHost);
  LRow := Node('layout-weighted-row');
  LColumn := Node('layout-weighted-column');
  LNatural := Node('layout-natural-column');
  LFirst := Node('layout-first-column');
  LSecond := Node('layout-second-column');
  LFixed := Node('layout-fixed-label');
  LFace := Face(LRow.ID);
  LMemoFace := Face('layout-focused-memo');
  LListFace := Face('layout-retained-list');
  LSplitFace := Face('layout-column-second');
  {$ifdef PAS2JS}LFirstItem := LListFace.children[0];
  Check(LListFace.children.length = 3, 'literal list starts with all three rows');
  {$else}Check(TListBox(LListFace).Items.Count = 3, 'literal list starts with all three rows');
  TListBox(LListFace).ItemIndex := 1;{$endif}
  Near(Width(LFirst.ID), 73, 'row allocates first weight after fixed width and gaps');
  Near(Width(LSecond.ID), 147, 'row allocates the second weight');
  Near(Left(LFirst.ID), 80, 'fixed width cannot overlap a following flexible child');
  Near(Left(LSecond.ID), 163, 'weighted row offsets follow actual widths');
  Near(Height('layout-focused-memo'), 144, 'nested authored-height column allocates its memo');
  Near(Height('layout-retained-list'), 144, 'nested column allocates its literal collection');
  Near(Height('layout-column-first'), 60, 'explicit column height allocates first weight');
  Near(Height('layout-column-second'), 120, 'explicit column height allocates split descendant');
  Near(Top('layout-column-first'), 60, 'column reserves fixed header and one gap');
  Near(Top('layout-column-second'), 130, 'column follows allocated preceding height');
  Near(Width('layout-nested-first'), 97, 'allocated row recursively distributes width');
  Near(Width('layout-nested-second'), 193, 'nested row retains second weight');
  Near(Height(LNatural.ID), 94, 'hidden middle has neither natural height nor gap');
  Near(Top('layout-natural-last'), 48, 'visible successor occupies the next flow slot');
  Near(Height('layout-hidden-grid'), 56, 'grid measurement excludes hidden track entries');
  Near(Top('layout-grid-last'), 10, 'grid skips the hidden entry before assigning tracks');
  {$ifdef PAS2JS}LMemo := TJSHTMLTextAreaElement(GRenderer.InputFor('layout-focused-memo'));
  LMemo.focus;
  LMemo.value := 'A retained draft 🌙';
  LMemo.selectionStart := 3;
  LMemo.selectionEnd := 7;
  {$else}LMemo := TMemo(GRenderer.InputFor('layout-focused-memo'));
  LMemo.SetFocus;
  LMemo.Text := 'A retained draft 🌙';
  LMemo.SelStart := 3;
  LMemo.SelLength := 4;{$endif}
  for LIndex := 0 to 4 do
  begin
    LRow.Configure.Width(320 + LIndex);
    LColumn.Configure.Height(260 + LIndex);
    GRenderer.Sync;
    Near(Width(LFirst.ID) + Width(LSecond.ID), 220 + LIndex,
      'resize preserves the complete available horizontal space');
    Near(Height('layout-column-first') + Height('layout-column-second'),
      180 + LIndex, 'resize preserves complete vertical proportional space');
    Check((Face(LRow.ID) = LFace) and (Face('layout-focused-memo') = LMemoFace) and
      (Face('layout-retained-list') = LListFace) and
      (Face('layout-column-second') = LSplitFace), 'resize retains all host identities');
    Check((Height('layout-split-first') >= 0) and (Height('layout-split-second') >= 0) and
      (Height('layout-split-first') <= Height('layout-column-second')) and
      (Height('layout-split-second') <= Height('layout-column-second')),
      'split descendants stay inside their allocated host');
    {$ifdef PAS2JS}Check((LListFace.children.length = 3) and
      (LListFace.children[0] = LFirstItem), 'layout retains actual literal row identities');
    {$else}Check((TListBox(LListFace).Items.Count = 3) and
      (TListBox(LListFace).ItemIndex = 1), 'layout retains actual native collection selection');{$endif}
  end;
  LRow.Configure.Width(320);
  LColumn.Configure.Height(260);
  for LIndex := 0 to 2 do
  begin
    LVisible := LIndex <> 1;
    LSecond.Configure.Visible(LVisible);
    GRenderer.Sync;

    if LVisible then
    begin
      Near(Width(LFirst.ID), 73, 'show restores weighted width');
    end
    else
    begin
      Near(Width(LFirst.ID), 230, 'hide gives the surviving weight all remaining space');
    end;
    {$ifdef PAS2JS}
    Check(document.activeElement = LMemo, 'unrelated visibility retains actual focus');
    Check((LMemo.value = 'A retained draft 🌙') and (LMemo.selectionStart = 3) and
      (LMemo.selectionEnd = 7), 'resize/visibility retain browser draft and selection');
    {$else}
    Check(GHost.ActiveControl = LMemo, 'unrelated visibility retains native focus');
    Check((TNyxText(LMemo.Text) = TNyxText('A retained draft 🌙')) and (LMemo.SelStart = 3) and
      (LMemo.SelLength = 4), 'resize/visibility retain native draft and selection');
    {$endif}
  end;
  for LIndex := 0 to 2 do
  begin
    LNatural.Children[LIndex].Configure.Visible(False);
  end;
  GRenderer.Sync;
  Near(Height(LNatural.ID), 20, 'all hidden children leave padding alone');
  LNatural.Children[1].Configure.Visible(True);
  GRenderer.Sync;
  Near(Height(LNatural.ID), 110, 'sole visible middle has no leading/trailing gap');
  Near(Top('layout-natural-hidden'), 10, 'sole visible child begins at padding');
  LNatural.Children[0].Configure.Visible(True);
  LNatural.Children[2].Configure.Visible(True);
  GRenderer.Sync;
  Near(Height(LNatural.ID), 194, 'show restores every natural size and two gaps');
  LColumn.Children[2].Configure.Visible(False);
  GRenderer.Sync;
  Near(Height('layout-column-first'), 190, 'hidden last column child releases weight and gap');
  LColumn.Children[2].Configure.Visible(True);
  LColumn.Children[0].Configure.Visible(False);
  GRenderer.Sync;
  Near(Height('layout-column-first'), 76, 'hidden fixed first child releases its size and gap');
  Near(Height('layout-column-second'), 154, 'remaining nested split receives proportional space');
  LColumn.Children[0].Configure.Visible(True);
  GRenderer.Sync;
  LFirst.Configure.Flex(0).Width(90);
  LSecond.Configure.Clear(atFlex).Width(130);
  GRenderer.Sync;
  Near(Width(LFirst.ID), 90, 'zero weight restores explicit width');
  Near(Width(LSecond.ID), 130, 'cleared weight restores explicit width');
  Near(Left(LSecond.ID), 180, 'cleared weights retain fixed-width flow');
  {$ifdef PAS2JS}
  Check(not Face(LFirst.ID).classList.contains('nyx-flex') and
    not Face(LSecond.ID).classList.contains('nyx-flex'), 'clearing restores natural minimum sizing');
  Check(Face(LSecond.ID).style.getPropertyValue('flex') = '', 'clear removes the inline weight');
  {$endif}
  LFirst.Configure.Clear(atWidth).Flex(100000);
  LSecond.Configure.Clear(atWidth).Flex(100000);
  LFixed.Configure.Visible(False);
  GRenderer.Sync;
  Near(Width(LFirst.ID), 145, 'large weights and hidden first child distribute safely');
  Near(Width(LSecond.ID), 145, 'remaining weights consume the only visible gap');
  LFixed.Configure.Visible(True);
  LFirst.Configure.Flex(1);
  LSecond.Configure.Flex(2);
  LNatural.Children[1].Configure.Visible(False);
  GRenderer.Sync;
  Near(Width(LFirst.ID), 73, 'restored authored review retains original proportional layout');
  Near(Height(LNatural.ID), 94, 'restored authored review retains original hidden flow');
  {$ifdef PAS2JS}GHost.style.setProperty('width', '390px');
  {$else}GHost.ClientWidth := 390;{$endif}
  GRenderer.Sync;
  Near(Width(LRow.ID), 320, 'host resizing retains the authored inner layout');
  Check(Face('layout-focused-memo') = LMemoFace, 'host resizing retains the actual editor');
  {$ifdef PAS2JS}Check(document.activeElement = LMemo, 'host resizing retains browser focus');
  {$else}Check(GHost.ActiveControl = LMemo, 'host resizing retains native focus');{$endif}
  {$ifdef PAS2JS}GHost.style.setProperty('width', '360px');
  {$else}GHost.ClientWidth := 360;{$endif}
  GRenderer.Sync;
  Near(Width(LRow.ID), Min(320, Width(GRenderer.Root.ID) - 48),
    'explicit width is capped by actual available parent content');
  Near(Width(LFirst.ID) + Width(LSecond.ID), Width(LRow.ID) - 100,
    'capped row redistributes its actual remaining width');
  Check(Face('layout-focused-memo') = LMemoFace, 'narrow allocation retains the editor');
end;

{$ifdef NYX_LAYOUT_POLICY}
{ This consumer must run the unchanged MCP companion containing policy-review.
  Native/widget metrics can differ, so fixed policy geometry has exact expected
  bounds; natural captions are checked for actual measurement and non-overlap. }
procedure Policies;
var
  LRow: TNyxNode;
  LFirst: TNyxNode;
  LSecond: TNyxNode;
  LThird: TNyxNode;
  LMemoFace: TFace;
  LPolicy: TNyxLayoutPolicy;
  LChanged: TNyxLayoutPolicy;
  LControl: INyxRow;
  LConfiguration: INyxConfiguration;
  LOwned: TNyxDocument;
  LDecoded: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LRejected: Boolean;
  LItems: TNyxFlowItems;
  LSizes: TNyxFlowSizes;
  LPositions: TNyxFlowSizes;
  LLines: TNyxFlowLines;
  LJustification: TNyxJustification;
  LCross: TNyxCrossAlignment;
  LExpectedFirst: Integer;
  LExpectedSecond: Integer;
  LFree: Integer;
  {$ifdef PAS2JS}LMemo: TJSHTMLTextAreaElement;
  {$else}LMemo: TMemo;{$endif}
begin
  LPolicy := TNyxLayoutPolicy.Row.Wrap(nfwWrap).Align(ncaEnd).Justify(njCenter);
  LChanged := LPolicy.Wrap(nfwNoWrap).Align(ncaStart);
  Check((LPolicy.Wrapping = nfwWrap) and (LPolicy.Alignment = ncaEnd) and
    (LChanged.Wrapping = nfwNoWrap), 'fluent policy values retain independent baselines');
  LControl := NewNyxRow('public-policy');
  LConfiguration := LControl.Configure.Layout(LPolicy).WidthSizing(nsContent);
  LControl := nil;
  Check(LConfiguration.Done.Node.Prop('flow-wrap') = 'wrap',
    'managed policy configuration retains its specialized control');
  LConfiguration := nil;
  LOwned := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LOwned.AddPage(TNyxNode.Create(nkRow, 'public-policy').Configure.Layout(LPolicy)
      .WidthSizing(nsContent).HeightSizing(nsFill).Done);
    LOwned.Pages[0].Configure.ForPlatform(npfNativeLCL).Wrap(nfwNoWrap).Done;
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LOwned));
    try
      Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(LOwned),
        'all policy choices and platform overrides round-trip exactly');
    finally
      LDecoded.Free;
    end;
    LSource := LWorkspace.Render(LOwned);
    LWorkspace.Accept(LOwned, LSource);
    Check((Pos('.Wrap(nfwWrap)', LSource) > 0) and
      (Pos('.HeightSizing(nsFill)', LSource) > 0), 'generated configuration uses Pascal enums');
    LSource := StringReplace(LSource, 'Layout(nlRow)',
      'Layout(TNyxLayoutPolicy.Row.Wrap(nfwWrap).Align(ncaEnd).Justify(njCenter))', []);
    LDecoded := LWorkspace.Candidate(LOwned, LSource);
    try
      Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(LOwned),
        'Studio source admission evaluates the fluent value policy without executing code');
    finally
      LDecoded.Free;
    end;
    LRejected := False;
    try
      LDecoded := LWorkspace.Candidate(LOwned, StringReplace(LSource,
        'Wrap(nfwWrap)', 'Wrap(ncaStart)', []));
      LDecoded.Free;
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Studio rejects a cross-alignment enum as a wrap argument');
  finally
    LWorkspace.Free;
    LOwned.Free;
  end;
  SetLength(LItems, 3);
  LItems[0].Visible := True;
  LItems[0].NaturalSize := 80;
  LItems[1].Visible := False;
  LItems[1].NaturalSize := 1000;
  LItems[2].Visible := True;
  LItems[2].NaturalSize := 80;
  LSizes := NyxFlowSizes(300, 10, LItems);
  LPositions := NyxFlowPositions(300, 10, LItems, LSizes, njSpaceBetween);
  Check((LPositions[0] = 0) and (LPositions[1] = 0) and (LPositions[2] = 220),
    'justification reserves hidden-free gaps and puts endpoints at known positions');
  LLines := NyxFlowLines(150, 10, LItems, True);
  Check((Length(LLines) = 2) and (LLines[0].Last = 0) and (LLines[1].First = 2),
    'wrapped windows preserve visible order around hidden entries');
  {$ifdef PAS2JS}GHost.style.setProperty('width', '300px');
  GHost.style.setProperty('height', '700px');
  {$else}GHost.ClientWidth := 300;
  GHost.ClientHeight := 700;{$endif}
  GRenderer.Render(GDocument, GDocument.Find('policy-review'), GHost);
  GRenderer.Sync;
  {$ifdef PAS2JS}Near(Height('policy-review'), Round(GHost.clientHeight), 'root fills definite browser host');
  {$else}Near(Height('policy-review'), GHost.ClientHeight, 'root fills definite native host');{$endif}
  Near(Left('policy-small'), 0, 'space-between starts at the leading edge');
  Near(Left('policy-tall'), Width('policy-alignment') - 80, 'space-between reaches the trailing edge');
  Near(Top('policy-small'), 60, 'row end alignment uses the definite cross height');
  Near(Top('policy-tall'), 40, 'different child height shares the same ending edge');
  Near(Top('policy-wrap-first'), 0, 'first natural wrap line begins at zero');
  Near(Top('policy-wrap-second'), 0, 'two fixed widths fit the first line');
  Near(Top('policy-wrap-third'), 50, 'third item starts after the 40-pixel line plus gap');
  Near(Height('policy-wrap-first'), 40, 'stretch shares the first line height');
  Near(Height('policy-wrap'), 80, 'wrapped container measures both natural lines');
  Near(Top('policy-after-text'), Top('policy-narrow-text') + Height('policy-narrow-text') + 10,
    'parent measures caption at the actual authored width');
  Check(Height('policy-narrow-text') > 40, 'narrow caption actually wraps through the target font');
  Check(Width('policy-long-action') > Width('policy-short-action'),
    'native/browser action widths reflect their different captions');
  Near(Left('policy-long-action'), Width('policy-short-action') + 10,
    'natural row children neither overlap nor reserve equal cells');
  Near(Width('policy-spacer'), Width('policy-spacer-row') - 140,
    'implicit spacer shares the remaining width on both targets');
  Near(Left('policy-spacer-end'), Width('policy-spacer-row') - 60,
    'implicit spacer pushes the final caption to the trailing edge');
  Near(Height('policy-retained-memo'), Height('policy-fill-body') - 30,
    'root height reaches the nested weighted editor');
  LMemoFace := Face('policy-retained-memo');
  {$ifdef PAS2JS}LMemo := TJSHTMLTextAreaElement(LMemoFace.querySelector('textarea'));
  LMemo.value := 'Keep this draft 🌙';
  LMemo.focus;
  LMemo.selectionStart := 5;
  LMemo.selectionEnd := 9;
  {$else}LMemo := TMemo(GRenderer.InputFor('policy-retained-memo', niRuntime));
  LMemo.Text := 'Keep this draft 🌙';
  LMemo.SetFocus;
  LMemo.SelStart := 5;
  LMemo.SelLength := 4;{$endif}
  LRow := Node('policy-wrap');
  LFirst := Node('policy-wrap-first');
  LSecond := Node('policy-wrap-second');
  LThird := Node('policy-wrap-third');
  LRow.Configure.Wrap(nfwNoWrap).Done;
  GRenderer.Sync;
  Near(Top(LThird.ID), 0, 'NoWrap preserves a single overflowing line');
  Near(Left(LThird.ID), 260, 'NoWrap retains all fixed main-axis widths');
  LRow.Configure.Wrap(nfwWrap).Align(ncaEnd).Done;
  LFirst.Configure.Visible(False).Done;
  GRenderer.Sync;
  Near(Left(LSecond.ID), 0, 'hiding first wrap entry retains source order');
  Near(Left(LThird.ID), 130, 'showing only two entries fits one line');
  Near(Top(LThird.ID), 10, 'end alignment uses the taller remaining sibling');
  LFirst.Configure.Visible(True).Done;
  LRow.Configure.Clear(atFlowWrap).Align(ncaStart).Done;
  Node('policy-alignment').Configure.Justify(njCenter).Align(ncaCenter).Done;
  GRenderer.Sync;
  Near(Left('policy-small'), (Width('policy-alignment') - 170) div 2,
    'center justification groups actual fixed widths and gap');
  Near(Top('policy-small'), 30, 'center cross alignment uses actual child height');
  for LJustification := Low(TNyxJustification) to High(TNyxJustification) do
  begin
    Node('policy-alignment').Configure.Justify(LJustification).Done;
    GRenderer.Sync;
    LFree := Width('policy-alignment') - 170;
    LExpectedFirst := 0;
    LExpectedSecond := 90;
    case LJustification of
      njStart:
        begin
          { The initial expected bounds already describe start justification. }
        end;
      njCenter:
        begin
          LExpectedFirst := LFree div 2;
          LExpectedSecond := LExpectedFirst + 90;
        end;
      njEnd:
        begin
          LExpectedFirst := LFree;
          LExpectedSecond := LFree + 90;
        end;
      njSpaceBetween: LExpectedSecond := LFree + 90;
      njSpaceAround:
        begin
          LExpectedFirst := LFree div 4;
          LExpectedSecond := Trunc(LFree * 0.75) + 90;
        end;
      njSpaceEvenly:
        begin
          LExpectedFirst := LFree div 3;
          LExpectedSecond := Trunc(LFree * (2 / 3)) + 90;
        end;
    end;
    Near(Left('policy-small'), LExpectedFirst, 'first bound for ' + NyxJustificationName(LJustification));
    Near(Left('policy-tall'), LExpectedSecond, 'second bound for ' + NyxJustificationName(LJustification));
  end;
  for LCross := ncaStart to ncaEnd do
  begin
    Node('policy-alignment').Configure.Align(LCross).Done;
    GRenderer.Sync;
    LExpectedFirst := 0;

    if LCross = ncaCenter then
    begin
      LExpectedFirst := 30;
    end
    else if LCross = ncaEnd then
    begin
      LExpectedFirst := 60;
    end;
    Near(Top('policy-small'), LExpectedFirst, 'cross bound for ' + NyxCrossAlignmentName(LCross));
  end;
  LRow.Configure.Wrap(nfwWrap).Align(ncaStretch).Done;
  LFirst.Configure.HeightSizing(nsContent).Done;
  GRenderer.Sync;
  Near(Height(LFirst.ID), 20, 'Content height remains intrinsic under parent stretch');
  LFirst.Configure.Clear(atHeightSizing).Done;
  LFirst.Configure.Align(ncaEnd).Done;
  Node('policy-wrap-first-caption').Configure.WidthSizing(nsContent).Done;
  GRenderer.Sync;
  Near(Left('policy-wrap-first-caption'), Width(LFirst.ID) - Width('policy-wrap-first-caption'),
    'column cross alignment positions a content-sized child');
  Node('policy-natural-actions').Configure.WidthSizing(nsContent).Done;
  GRenderer.Sync;
  Near(Width('policy-natural-actions'), Width('policy-short-action') +
    Width('policy-long-action') + 10, 'Content width measures a nested natural row');
  Node('policy-alignment').Configure.Width(100).Justify(njCenter).Done;
  GRenderer.Sync;
  Near(Left('policy-small'), 0, 'overflow center keeps leading content reachable');
  Node('policy-alignment').Configure.Clear(atWidth).Done;
  LFirst.Configure.Visible(False).Done;
  LSecond.Configure.Visible(False).Done;
  LThird.Configure.Visible(False).Done;
  GRenderer.Sync;
  Near(Height(LRow.ID), 0, 'all-hidden wrap children reserve no line or gap');
  LFirst.Configure.Visible(True).Done;
  LSecond.Configure.Visible(True).Done;
  LThird.Configure.Visible(True).Done;
  Node('policy-spacer').Configure.Flex(0).Done;
  GRenderer.Sync;
  Near(Width('policy-spacer'), 0, 'explicit zero opts out of implicit spacer weight');
  Node('policy-spacer').Configure.Clear(atFlex).Done;
  Node('policy-narrow-text').Configure.WidthSizing(nsFill).Done;
  GRenderer.Sync;
  Near(Width('policy-narrow-text'), Width('policy-review') - 20,
    'Fill overrides retained authored width');
  Node('policy-narrow-text').Configure.Clear(atWidthSizing).Done;
  GRenderer.Sync;
  Near(Width('policy-narrow-text'), 80, 'clearing sizing restores the retained pixel metric');
  Near(Width('policy-spacer'), Width('policy-spacer-row') - 140,
    'clearing spacer weight restores its shared default');
  {$ifdef PAS2JS}GHost.style.setProperty('height', '760px');
  {$else}GHost.ClientHeight := 760;{$endif}
  GRenderer.Sync;
  Near(Height('policy-review'), 760, 'host height resize reaches the mounted root');
  Near(Height('policy-retained-memo'), Height('policy-fill-body') - 30,
    'resized height reaches the same nested editor');
  Node('policy-review').Configure.HeightSizing(nsContent).Done;
  GRenderer.Sync;
  Check(Height('policy-review') < 760, 'Content height restores natural root measurement');
  Node('policy-review').Configure.HeightSizing(nsFill).Done;
  GRenderer.Sync;
  Check(Face('policy-retained-memo') = LMemoFace, 'all policy updates retain the mounted editor');
  {$ifdef PAS2JS}Check((document.activeElement = LMemo) and (LMemo.value = 'Keep this draft 🌙') and
    (LMemo.selectionStart = 5) and (LMemo.selectionEnd = 9), 'layout policies retain browser draft/focus/selection');
  {$else}Check((GHost.ActiveControl = LMemo) and
    (TNyxText(LMemo.Text) = TNyxText('Keep this draft 🌙')) and
    (LMemo.SelStart = 5) and (LMemo.SelLength = 4), 'layout policies retain native draft/focus/selection');{$endif}
end;
{$endif}

procedure Run;
begin
  GDocument := nil;
  GRenderer := nil;
  try
    {$ifndef PAS2JS}Application.Initialize;{$else}

    if window.location.search = '?frame=1' then
    begin
      Check(window.innerWidth = 390, 'phone run has an actual 390-pixel viewport');
    end;
    {$endif}
    Arithmetic;
    GDocument := BuildNyxDocument;
    GRenderer := TRenderer.Create;
    {$ifdef PAS2JS}GHost := TJSHTMLElement(document.getElementById('layout-host'));
    {$else}GHost := TForm.CreateNew(nil);
    GHost.SetBounds(0, 0, 1000, 1000);
    GHost.Show;{$endif}
    try
      Review;
      {$ifdef NYX_LAYOUT_POLICY}Policies;{$endif}
      {$ifdef PAS2JS}document.body.setAttribute('data-layout-tests', 'passed');
      document.body.setAttribute('data-layout-checks', IntToStr(GChecks));
      {$else}WriteLn('PASS ', GChecks, ' layout control and arithmetic checks');{$endif}
    finally
      { The browser keeps its successful live view until page teardown so the
        selective capture shows the measured controls, rather than an empty
        host left by fixture cleanup. Native qualification releases everything
        immediately and checks heap ownership separately. }
      {$ifndef PAS2JS}
      GRenderer.Free;
      GRenderer := nil;
      GHost.Free;
      GDocument.Free;
      GDocument := nil;
      {$endif}
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}document.body.setAttribute('data-layout-tests', 'failed');
      document.body.setAttribute('data-layout-error', LException.Message);
      {$else}WriteLn(StdErr, 'FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      GRenderer.Free;
      GDocument.Free;
      ExitCode := 1;{$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
{ Execute the complete shared consumer in a true narrow viewport. The parent
  forwards only terminal evidence and does not inject fixture behavior. }
procedure CheckFrame;
var
  LBody: TJSElement;
  LResult: String;
begin
  LBody := GFrame.contentDocument.body;
  LResult := LBody.getAttribute('data-layout-tests');

  if (LResult = 'passed') or (LResult = 'failed') then
  begin
    document.body.setAttribute('data-layout-tests', LResult);
    document.body.setAttribute('data-layout-checks', LBody.getAttribute('data-layout-checks'));
    document.body.setAttribute('data-layout-error', LBody.getAttribute('data-layout-error'));
  end
  else
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}

  if window.location.search = '?host=1' then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.setProperty('width', '390px');
    GFrame.style.setProperty('height', '844px');
    GFrame.style.setProperty('border', '0');
    GFrame.src := 'layout.html?frame=1';
    document.body.appendChild(GFrame);
    window.setTimeout(@CheckFrame, 25);
  end
  else
  begin
    Run;
  end;
  {$else}Run;{$endif}
end.
