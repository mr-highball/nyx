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
    Check((TNyxText(LMemo.Text) = 'A retained draft 🌙') and (LMemo.SelStart = 3) and
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
