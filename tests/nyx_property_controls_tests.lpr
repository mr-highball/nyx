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

program nyx_property_controls_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.data, nyx.types, nyx.model, nyx.schema, nyx.catalog,
  nyx.behavior, nyx.contract, nyx.state, nyx.generated.view, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.mount, nyx.test.collections.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, ComCtrls, Grids, ExtCtrls,
  Graphics, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  {$else}TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  {$endif}
  { The observer owns counts only. A silent Sync must not publish programmatic
    caption/row/format/range changes as physical value edits. }
  TObserver = class
  public
    Changes: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

var
  GDocument: TNyxDocument;
  GCatalog: TNyxCatalog;
  GRenderer: TRenderer;
  GObserver: TObserver;
  GChecks: Integer;
  GFaces: Integer;
  GKinds: Integer;
  GContext: TNyxText;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;
  GFrame: TJSHTMLIFrameElement;
  GAwaitCapture: Boolean;
  GCaptureStarted: Double;
  {$else}GHost: TForm;
  GFirstPicture: TNyxText;
  GSecondPicture: TNyxText;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(GContext + ': ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntChange then
  begin
    Inc(Changes);
  end;
end;

function Face(ANode: TNyxNode): TFace;
begin
  {$ifdef PAS2JS}Result := GRenderer.ElementFor(ANode.ID, niRuntime);
  {$else}Result := GRenderer.ControlFor(ANode.ID, niRuntime);{$endif}
end;

procedure SilentSync;
var
  LChanges: Integer;
begin
  LChanges := GObserver.Changes;
  GRenderer.Sync;
  Check(GObserver.Changes = LChanges, 'Sync does not publish a user value edit');
end;

procedure CheckLiteral(ANode: TNyxNode; AKind: TNyxKind);
const
  CRows: TNyxText = 'Alpha 🌙' + #10 + '"Quoted"' + #10 + 'Last';
  CTable: TNyxText = 'First' + #9 + '"Second"' + #9 + 'Last' + #10 +
    'One 🌙' + #9 + '"Two"' + #9 + #10 + 'Only';
var
  LFace: TFace;
  LFocus: TFace;
  LRows: TNyxText;
  LFirstText: TNyxText;
  LSelectedValue: TNyxText;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LMount: INyxCollectionMount;
  LSpec: TNyxCollectionViewSpec;
  {$ifdef PAS2JS}LContent: TJSHTMLElement;
  LFirst: TJSHTMLElement;
  {$else}LTreeFirst: TTreeNode;
  LGrid: TStringGrid;{$endif}
begin
  LFace := Face(ANode);
  LRows := CRows;
  LFirstText := 'Alpha 🌙';
  LSelectedValue := ANode.Prop('value');

  if (AKind = nkSelect) and (LSelectedValue <> '') then
  begin
    { A compound can declare a constrained scalar domain. Its current value
      remains admitted; an Items test must not replace that domain/value. }
    LFirstText := LSelectedValue;
    LRows := LFirstText + #10 + '"Quoted"' + #10 + 'Last';
  end;

  if AKind = nkTable then
  begin
    LRows := CTable;
  end;
  ANode.Configure.Items(LRows);

  SilentSync;
  Check(Face(ANode) = LFace, 'literal update retains the outer face');
  {$ifdef PAS2JS}
  LContent := LFace;

  if AKind = nkSelect then
  begin
    LContent := GRenderer.InputFor(ANode.ID);
  end;
  Check(LContent.children.length = 3, 'three exact literal rows');
  LFirst := TJSHTMLElement(LContent.children[0]);

  if AKind = nkTable then
  begin
    Check(LFirst.children.length = 3, 'table header has three cells');
    Check(LFirst.children[1].textContent = '"Second"', 'header quotes are literal');
    Check(TJSHTMLElement(LContent.children[1]).children.length = 3, 'trailing empty cell retained');
    Check(TJSHTMLElement(LContent.children[1]).children[0].textContent = TNyxText('One 🌙'),
      'table supplementary text retained');
    Check(TJSHTMLElement(LContent.children[1]).children[1].textContent = '"Two"', 'cell quotes retained');
    Check(TJSHTMLElement(LContent.children[1]).children[2].textContent = '', 'trailing cell is empty');
    Check(TJSHTMLElement(LContent.children[2]).children.length = 1, 'ragged row remains literal');
  end
  else
  begin
    Check(LFirst.textContent = LFirstText, 'literal text retained');
    Check(LContent.children[1].textContent = '"Quoted"', 'row quotes retained');
  end;
  {$else}
  LTreeFirst := nil;
  case AKind of
    nkSelect:
      begin
        Check(TComboBox(GRenderer.InputFor(ANode.ID)).Items.Count = 3, 'three choices');
        Check(TNyxText(TComboBox(GRenderer.InputFor(ANode.ID)).Text) = LSelectedValue,
          'choice value reaches actual native text');
      end;
    nkList:
      begin
        Check(TListBox(LFace).Items.Count = 3, 'three native rows');
        Check(TNyxText(TListBox(LFace).Items[0]) = TNyxText('Alpha 🌙'), 'native row Unicode');
        TListBox(LFace).ItemIndex := 0;
      end;
    nkTree:
      begin
        Check(TTreeView(LFace).Items.Count = 3, 'three native leaves');
        LTreeFirst := TTreeView(LFace).Items[0];
        Check(TNyxText(LTreeFirst.Text) = TNyxText('Alpha 🌙'), 'native leaf Unicode');
        TTreeView(LFace).Selected := LTreeFirst;
      end;
    nkTable:
      begin
        LGrid := TStringGrid(LFace);
        Check((LGrid.RowCount = 3) and (LGrid.ColCount = 3), 'exact native literal dimensions');
        Check(LGrid.Cells[1, 0] = '"Second"', 'native header quotes retained');
        Check(TNyxText(LGrid.Cells[0, 1]) = TNyxText('One 🌙'), 'native cell Unicode');
        Check(LGrid.Cells[1, 1] = '"Two"', 'native cell quotes retained');
        Check(LGrid.Cells[2, 1] = '', 'native trailing cell empty');
        Check(LGrid.Cells[1, 2] = '', 'native ragged retained cells cleared');
      end;
    else
      begin
        raise Exception.Create('Literal fixture requires a Select, List, Tree or Table face');
      end;
  end;
  {$endif}
  LFocus := GRenderer.FocusFor(ANode.ID, niRuntime);
  {$ifdef PAS2JS}NyxFocusWithoutScroll(LFocus);
  {$else}

  if (LFocus is TWinControl) and TWinControl(LFocus).CanFocus then
  begin
    TWinControl(LFocus).SetFocus;
  end;
  {$endif}
  ANode.Configure.Hint('Unrelated publication');
  SilentSync;
  Check(Face(ANode) = LFace, 'no-op literal update retains face');
  {$ifdef PAS2JS}
  Check(LContent.children[0] = LFirst, 'no-op retains literal child identity');
  Check(document.activeElement = LFocus, 'no-op retains actual focus');
  {$else}

  if AKind = nkTree then
  begin
    Check(TTreeView(LFace).Selected = LTreeFirst, 'no-op retains native leaf identity');
  end;
  {$endif}
  ANode.Configure.Items('Last' + #10 + 'Alpha 🌙' + #10 + 'Replacement');
  SilentSync;
  {$ifndef PAS2JS}

  if AKind = nkList then
  begin
    Check(TListBox(LFace).ItemIndex = 1, 'changed list keeps surviving selected text');
  end;

  if AKind = nkTree then
  begin
    Check((TTreeView(LFace).Selected <> nil) and
      (TNyxText(TTreeView(LFace).Selected.Text) = TNyxText('Alpha 🌙')),
      'changed leaves keep surviving selected text');
  end;
  {$endif}
  ANode.Configure.Items('');
  SilentSync;
  {$ifdef PAS2JS}Check(LContent.children.length = 0, 'empty Items clears rows');
  {$else}
  case AKind of
    nkSelect: Check(TComboBox(GRenderer.InputFor(ANode.ID)).Items.Count = 0, 'empty choices');
    nkList: Check(TListBox(LFace).Items.Count = 0, 'empty native list');
    nkTree: Check(TTreeView(LFace).Items.Count = 0, 'empty native tree');
    nkTable: Check((TStringGrid(LFace).RowCount = 1) and
      (TStringGrid(LFace).Cells[0, 0] = ''), 'empty native physical sentinel has no data');
  else
    begin
      raise Exception.Create('Literal clear fixture requires a collection face');
    end;
  end;
  {$endif}

  if AKind <> nkSelect then
  begin
    { The same literal face can accept an attachment. Sync must never substitute
      Items for an attachment's structured dataset, including later publications. }
    LStore := CreateNyxViewCollection;
    case AKind of
      nkTable: LSpec := NyxTestTableViewSpec;
      nkTree: LSpec := NyxTestTreeViewSpec;
    else
      LSpec := NyxTestTableViewSpec;
    end;
    case AKind of
      nkTable:
        begin
          LView := NewNyxCollectionView(LStore, LSpec, cpTable);
        end;
      nkTree:
        begin
          LView := NewNyxCollectionView(LStore, LSpec, cpTree);
        end;
    else
      LView := NewNyxCollectionView(LStore, LSpec, cpList);
    end;
    LMount := GRenderer.BindCollection(ANode.ID, LView);
    ANode.Configure.Items('This must not replace the bound dataset');
    SilentSync;
    Check(LMount.Connected, 'literal mutation retains the managed attachment');
    Check(LView.Snapshot.Count > 1, 'bound dataset remains owned');
    {$ifdef PAS2JS}
    Check(Pos('This must not replace', LFace.textContent) = 0, 'bound DOM not clobbered');
    {$else}
    case AKind of
      nkList: Check(TListBox(LFace).Items.IndexOf('This must not replace the bound dataset') < 0,
        'bound native list not clobbered');
      nkTree: Check(TTreeView(LFace).Items.FindNodeWithText('This must not replace the bound dataset') = nil,
        'bound native tree not clobbered');
      nkTable: Check(TStringGrid(LFace).Cells[0, 0] <> 'This must not replace the bound dataset',
        'bound native table not clobbered');
    else
      begin
        raise Exception.Create('Bound literal fixture requires a List, Tree or Table face');
      end;
    end;
    {$endif}
    LMount := nil;
    LView := nil;
    LStore := nil;
  end;
end;

procedure InputFixtureValue(ANode: TNyxNode; AType: TNyxInputType);
var
  LDomain: TNyxValueDomain;
begin
  { Explicit recipe domains remain authoritative when changing format. A
    numeric keyboard hint cannot silently change an authored Text state family,
    and an integer stepper must retain its integer value through every format. }
  LDomain := NyxNodeValueDomain(ANode);
  case LDomain.Kind of
    nskInteger: ANode.Configure.Value(12);
    nskNumber: ANode.Configure.Value(Double(12));
    nskBoolean: ANode.Configure.Value(True);
  else

    if AType = niNumber then
    begin
      ANode.Configure.Value(TNyxText('12'));
    end
    else
    begin
      ANode.Configure.Value(TNyxText('Typed 🌙'));
    end;
  end;
end;

procedure Visit(ANode: TNyxNode);
var
  LIndex: Integer;
  LKind: TNyxKind;
  LType: TNyxInputType;
  LFace: TFace;
  LProperties: TNyxPropertyInfos;
  LFound: set of TNyxAttribute;
  LAttribute: TNyxAttribute;
  {$ifdef PAS2JS}LInput: TJSHTMLInputElement;
  {$else}LEdit: TEdit;{$endif}
begin
  GContext := ANode.Kind + ' / ' + ANode.ID;
  Inc(GFaces);
  LProperties := NyxProperties(ANode);
  LFound := [];
  for LIndex := 0 to High(LProperties) do
  begin
    Check(LProperties[LIndex].Support.Defined, 'published property has an explicit support snapshot');

    if TryNyxAttribute(LProperties[LIndex].Key, LAttribute) then
    begin
      Include(LFound, LAttribute);
    end;
  end;
  Check(LFound = [Low(TNyxAttribute)..High(TNyxAttribute)], 'all 47 typed attributes remain queryable');
  LFace := Face(ANode);
  ANode.Configure.Hint('Inspect 🌙').AccessibleName('Purpose 🌙');
  SilentSync;
  {$ifdef PAS2JS}
  Check(LFace.title = TNyxText('Inspect 🌙'), 'physical hint updates');
  Check(LFace.getAttribute('aria-label') = TNyxText('Purpose 🌙'), 'physical name updates');
  {$else}
  Check(TNyxText(LFace.Hint) = TNyxText('Inspect 🌙'), 'physical hint updates');
  Check(TNyxText(LFace.AccessibleName) = TNyxText('Purpose 🌙'), 'physical name updates');
  {$endif}

  if TryNyxKind(ANode.ProjectionKind, LKind) then
  begin
    case LKind of
      nkSelect, nkList, nkTable, nkTree: CheckLiteral(ANode, LKind);
      nkInput:
        begin
          for LType := Low(TNyxInputType) to High(TNyxInputType) do
          begin
            ANode.Configure.InputType(LType);

            InputFixtureValue(ANode, LType);
            SilentSync;
            {$ifdef PAS2JS}
            LInput := TJSHTMLInputElement(GRenderer.InputFor(ANode.ID));
            Check(LInput._type = NyxInputTypeName(LType), 'all seven typed browser formats');
            Check((LType <> niNumber) or (LInput.getAttribute('step') = 'any'), 'number permits exact decimals');
            {$else}
            LEdit := TEdit(GRenderer.InputFor(ANode.ID));
            Check((LEdit.PasswordChar <> #0) = (LType = niPassword), 'native masking follows current format');
            {$endif}
          end;
          ANode.Configure.Clear(atInputType);
          InputFixtureValue(ANode, niText);
          SilentSync;
          {$ifdef PAS2JS}
          Check(LInput._type = 'text', 'clearing format restores text');
          Check(not LInput.hasAttribute('step'), 'clearing format withdraws decimal hint');
          {$else}Check(LEdit.PasswordChar = #0, 'clearing format removes masking');{$endif}
          {$ifndef PAS2JS}

          if ANode.ID = 'catalog-input-sample' then
          begin
            ANode.Configure.InputType(niNumber).Value(Double(12));
            SilentSync;
            LEdit.Text := '1.';
            Check((ANode.Prop('value') = '12') and (LEdit.Text = '1.'),
              'changing to Number retains an unfinished numeric draft');
            ANode.Configure.Hint('Keep the draft');
            SilentSync;
            Check(LEdit.Text = '1.', 'unrelated publication retains numeric draft');
            LEdit.Text := '18.5';
            Check(Assigned(LEdit.OnEditingDone), 'live numeric format owns commit boundary');
            LEdit.OnEditingDone(LEdit);
            Check(ANode.Prop('value') = '18.5', 'live numeric commit admits exact decimal');
            ANode.Configure.InputType(niText).Value(TNyxText('Ready'));
            SilentSync;
            LEdit.Text := 'Immediate';
            Check(ANode.Prop('value') = 'Immediate', 'returning to Text restores per-change admission');
          end;
          {$endif}
          {$ifdef PAS2JS}

          if ANode.ID = 'catalog-input-sample' then
          begin
            ANode.Configure.InputType(niNumber).Value(Double(12));
            SilentSync;
            LInput.value := '18.5';
            LInput.dispatchEvent(TJSEvent.new('input'));
            Check(ANode.Prop('value') = '12', 'live numeric format waits for change');
            ANode.Configure.Hint('Keep the draft');
            SilentSync;
            Check(LInput.value = '18.5', 'numeric DOM draft survives unrelated publication');
            LInput.dispatchEvent(TJSEvent.new('change'));
            Check(ANode.Prop('value') = '18.5', 'live numeric change admits exact decimal');
            ANode.Configure.InputType(niText).Value(TNyxText('Ready'));
            SilentSync;
            LInput.value := 'Immediate';
            LInput.dispatchEvent(TJSEvent.new('input'));
            Check(ANode.Prop('value') = 'Immediate', 'returning to Text restores input admission');
          end;
          {$endif}
        end;
      nkCode:
        begin
          ANode.Configure.Text('procedure Craft; 🌙' + #10 + 'begin' + #10 + 'end;');
          SilentSync;
          {$ifdef PAS2JS}Check(LFace.textContent = ANode.Prop('text'), 'code content updates');
          {$else}Check(TNyxText(TMemo(LFace).Lines[0]) = TNyxText('procedure Craft; 🌙'), 'native code content updates');{$endif}
        end;
      nkGroup:
        begin
          ANode.Configure.Text('A useful group 🌙');
          SilentSync;
          {$ifdef PAS2JS}Check(LFace.querySelector('legend').textContent = ANode.Prop('text'), 'group legend updates');
          {$else}Check(TNyxText(TGroupBox(LFace).Caption) = ANode.Prop('text'), 'native group caption updates');{$endif}
        end;
      nkProgress:
        begin
          ANode.Configure.Minimum(20).Maximum(80).Value(50);
          SilentSync;
          {$ifdef PAS2JS}
          Check((TJSHTMLProgressElement(LFace).max = 60) and
            (TJSHTMLProgressElement(LFace).value = 30), 'browser range maps to exact half completion');
          Check((LFace.getAttribute('aria-valuemin') = '20') and
            (LFace.getAttribute('aria-valuemax') = '80') and
            (LFace.getAttribute('aria-valuenow') = '50'), 'progress exposes authored scalar interval');
          {$else}Check((TProgressBar(LFace).Min = 20) and (TProgressBar(LFace).Max = 80) and
            (TProgressBar(LFace).Position = 50), 'native progress uses authored scalar interval');{$endif}
        end;
      nkImage:
        begin
          ANode.Configure.AlternativeText('A quiet image 🌙');
          {$ifdef PAS2JS}ANode.Configure.Source('./first-picture.png');
          {$else}ANode.Configure.Source(GFirstPicture);{$endif}
          SilentSync;
          {$ifdef PAS2JS}
          Check(LFace.getAttribute('src') = './first-picture.png', 'image source updates');
          Check(TJSHTMLImageElement(LFace).alt = TNyxText('A quiet image 🌙'), 'image alternative text updates');
          ANode.Configure.Source('./second-picture.png');
          {$else}
          Check((TImage(LFace).Picture.Width = 3) and (TImage(LFace).Picture.Height = 2), 'first local picture decoded');
          Check(TNyxText(LFace.AccessibleDescription) = TNyxText('A quiet image 🌙'), 'native alternative description');
          ANode.Configure.Source(GSecondPicture);
          {$endif}
          SilentSync;
          {$ifdef PAS2JS}Check(LFace.getAttribute('src') = './second-picture.png', 'changed image source updates');
          {$else}Check((TImage(LFace).Picture.Width = 7) and (TImage(LFace).Picture.Height = 4), 'second local picture decoded');{$endif}
          ANode.Configure.Clear(atSource).Clear(atAlt);
          SilentSync;
          {$ifdef PAS2JS}
          Check(not LFace.hasAttribute('src'), 'cleared source issues no empty URL request');
          Check(TJSHTMLImageElement(LFace).alt = '', 'empty alternative is decorative');
          {$else}
          Check(TImage(LFace).Picture.Graphic = nil, 'cleared source withdraws picture');
          Check(LFace.AccessibleDescription = '', 'cleared alternative description');
          {$endif}
        end;
    else
      begin
        { Other catalog faces have no additional family-specific fixture here.
          Their declared property support, text/name/hint and identity checks
          above remain required; this does not claim unimplemented effects. }
      end;
    end;
  end;
  Check(Face(ANode) = LFace, 'all property updates retain the physical face');
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Visit(ANode.Children[LIndex]);
  end;
end;

procedure LayoutReview;
var
  LLayout: TNyxNode;
  LLeft: TNyxNode;
  LRight: TNyxNode;
  LInputNode: TNyxNode;
  LFace: TFace;
  LFirst: TFace;
  LSecond: TFace;
  LMode: TNyxLayoutMode;
  {$ifdef PAS2JS}LInput: TJSHTMLInputElement;
  {$else}LEdit: TEdit;{$endif}
begin
  GContext := 'MCP-authored layout review';
  GRenderer.Render(GDocument, GDocument.Find('property-review'), GHost);
  LInputNode := GRenderer.Root.Find('property-numeric');
  Check(LInputNode <> nil, 'MCP review includes an initially numeric input');
  LInputNode.Configure.InputType(niText).Value(TNyxText('Ready'));
  SilentSync;
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(GRenderer.InputFor(LInputNode.ID));
  LInput.value := 'Text now';
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  LEdit := TEdit(GRenderer.InputFor(LInputNode.ID));
  LEdit.Text := 'Text now';
  {$endif}
  Check(LInputNode.Prop('value') = 'Text now', 'an initially numeric face admits live text after format change');
  LLayout := GRenderer.Root.Find('property-layout');
  LLeft := LLayout.Children[0];
  LRight := LLayout.Children[1];
  LFace := Face(LLayout);
  LFirst := Face(LLeft);
  LSecond := Face(LRight);
  for LMode := Low(TNyxLayoutMode) to High(TNyxLayoutMode) do
  begin
    LLayout.Configure.Layout(LMode).Columns(2).Padding(16).Gap(12);
    LLeft.Configure.Left(23).Top(31);
    LRight.Configure.Left(137).Top(53);
    SilentSync;
    {$ifdef PAS2JS}

    if LMode = nlAbsolute then
    begin
      Check(LFace.style.getPropertyValue('position') = 'relative', 'absolute host is its containing block');
      Check(LFirst.style.getPropertyValue('position') = 'absolute', 'absolute child is positioned');
      Check((LFirst.offsetLeft = 23) and (LFirst.offsetTop = 31), 'actual absolute coordinates');
    end
    else
    begin
      Check(LFirst.style.getPropertyValue('position') = '', 'flow transition clears absolute position');

      if LMode = nlColumn then
      begin
        Check(LFirst.offsetTop = 16, 'column starts at padding and ignores retained absolute Top');
      end
      else
      begin
        Check(LFirst.offsetLeft = 16, 'row/grid starts at padding and ignores retained absolute Left');
      end;

      if LMode in [nlColumn, nlRow] then
      begin
        Check(LFace.style.getPropertyValue('flex-direction') = NyxLayoutName(LMode), 'live flex direction');
      end;
    end;
    {$else}

    if LMode = nlAbsolute then
    begin
      Check((LFirst.Left = 23) and (LFirst.Top = 31), 'native absolute coordinates');
    end
    else if LMode = nlColumn then
    begin
      Check(LFirst.Top = 16, 'native column ignores retained absolute Top');
      Check(LSecond.Top > LFirst.Top, 'native column stacks children');
    end
    else
    begin
      Check(LFirst.Left = 16, 'native row/grid ignores retained absolute Left');
      Check(LSecond.Left > LFirst.Left, 'native row/grid places children beside one another');
    end;
    {$endif}
    Check((Face(LLayout) = LFace) and (Face(LLeft) = LFirst) and
      (Face(LRight) = LSecond), 'layout transitions retain all control identities');
  end;
  LLayout.Configure.Clear(atLayout).Surface(True);
  SilentSync;
  {$ifndef PAS2JS}
  Check(LFirst.Left = 16, 'cleared native layout restores natural flow geometry');
  {$endif}
  {$ifdef PAS2JS}
  Check(LFace.style.getPropertyValue('flex-direction') = 'row', 'clear returns to natural row layout');
  Check(LFirst.offsetLeft = 16, 'cleared layout restores natural flow geometry');
  Check(LFace.classList.contains('nyx-card'), 'extra surface is applied');
  LLayout.Configure.Surface(False);
  SilentSync;
  Check(not LFace.classList.contains('nyx-card'), 'extra surface is withdrawn');
  {$endif}
end;

{$ifndef PAS2JS}
procedure PreparePictures;
var
  LDirectory: TNyxText;
  LBitmap: TBitmap;
begin

  if ParamCount <> 1 then
  begin
    raise Exception.Create('Supply an owned native picture fixture directory');
  end;
  LDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
  ForceDirectories(LDirectory);
  GFirstPicture := LDirectory + 'first.bmp';
  GSecondPicture := LDirectory + 'second.bmp';
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(3, 2);
    LBitmap.SaveToFile(GFirstPicture);
  finally
    LBitmap.Free;
  end;
  { Saving a lazily backed LCL bitmap can cache its encoded image. Use a fresh
    bitmap for the second independent fixture; a resized cached first stream
    would test identical pictures while claiming different dimensions. }
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(7, 4);
    LBitmap.SaveToFile(GSecondPicture);
  finally
    LBitmap.Free;
  end;
end;
{$endif}

{ Shared retirement order releases renderer callbacks before their observer and
  borrowed document. Browser capture briefly retains the exact live mount, then
  calls this same boundary explicitly; native execution remains synchronous. }
procedure Retire;
begin
  GRenderer.Free;
  GRenderer := nil;
  GObserver.Free;
  GObserver := nil;
  {$ifndef PAS2JS}
  GHost.Free;
  GHost := nil;
  {$endif}
  GCatalog.Free;
  GCatalog := nil;
  GDocument.Free;
  GDocument := nil;
end;

{$ifdef PAS2JS}
{ Read-only physical diagnostics for the final property review, capped at
  sixteen boxes. Viewport and containing widths distinguish real horizontal
  overflow from a visual interpretation without changing styles. The report
  contains dimensions, computed styles and fixture identities, never user text
  or the design model. It is emitted only during explicit live capture. }
function ReviewGeometry: TNyxDataValue;
var
  LBoxes: array of TNyxDataValue;
  LPage: TNyxNode;
  LIndex: Integer;

  function Box(const AName: TNyxText; AElement: TJSHTMLElement): TNyxDataValue;
  var
    LRect: TJSDOMRect;
    LStyle: TJSCSSStyleDeclaration;
  begin
    LRect := AElement.getBoundingClientRect;
    LStyle := window.getComputedStyle(AElement);
    Result := NyxObject([
      NyxField('id', NyxData(AName)), NyxField('left', NyxData(LRect.left)),
      NyxField('right', NyxData(LRect.right)), NyxField('width', NyxData(LRect.width)),
      NyxField('clientWidth', NyxData(AElement.clientWidth)),
      NyxField('scrollWidth', NyxData(AElement.scrollWidth)),
      NyxField('boxSizing', NyxData(LStyle.getPropertyValue('box-sizing'))),
      NyxField('paddingLeft', NyxData(LStyle.getPropertyValue('padding-left'))),
      NyxField('paddingRight', NyxData(LStyle.getPropertyValue('padding-right'))),
      NyxField('overflowX', NyxData(LStyle.getPropertyValue('overflow-x')))]);
  end;

begin
  LPage := GRenderer.Root;
  SetLength(LBoxes, Min(LPage.Count, 11) + 5);
  LBoxes[0] := Box('html', TJSHTMLElement(document.documentElement));
  LBoxes[1] := Box('body', TJSHTMLElement(document.body));
  LBoxes[2] := Box('host', GHost);
  LBoxes[3] := Box(LPage.ID, Face(LPage));
  LBoxes[4] := Box('property-right', Face(LPage.Find('property-right')));
  for LIndex := 5 to High(LBoxes) do
  begin
    LBoxes[LIndex] := Box(LPage.Children[LIndex - 5].ID, Face(LPage.Children[LIndex - 5]));
  end;
  Result := NyxObject([NyxField('innerWidth', NyxData(window.innerWidth)),
    NyxField('innerHeight', NyxData(window.innerHeight)),
    NyxField('boxes', NyxArray(LBoxes))]);
end;

procedure FinishCapture;
var
  LObserved: Boolean;
begin
  LObserved := document.body.getAttribute('data-capture-observed') = 'catalog-properties';

  if not LObserved and (window.performance.now - GCaptureStarted < 30000) then
  begin
    window.setTimeout(@FinishCapture, 25);
    Exit;
  end;
  { A real-clock deadline fails rather than turning an uncaptured/retained mount
    into a successful qualification. Teardown occurs on success and timeout. }
  Retire;
  GAwaitCapture := False;
  document.body.setAttribute('data-property-disposed', 'true');

  if LObserved then
  begin
    document.body.setAttribute('data-property-tests', 'passed');
  end
  else
  begin
    document.body.setAttribute('data-property-tests', 'failed');
    document.body.setAttribute('data-property-error', 'Live catalog capture was not observed before its deadline');
  end;
end;
{$endif}

procedure Run;
var
  LIndex: Integer;
  LPage: TNyxNode;
begin
  GDocument := nil;
  GCatalog := nil;
  GRenderer := nil;
  GObserver := nil;
  GChecks := 0;
  GFaces := 0;
  GKinds := 0;
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    PreparePictures;
    {$else}
    GAwaitCapture := False;
    document.body.setAttribute('data-property-tests', 'running');

    if (window.location.search = '?frame=1') or
      (window.location.search = '?frame=1&capture=1') then
    begin
      Check(window.innerWidth = 390, 'phone fixture has an actual 390-pixel viewport');
    end;
    {$endif}
    GDocument := BuildNyxDocument;
    GCatalog := TNyxCatalog.Create;
    GRenderer := TRenderer.Create;
    GObserver := TObserver.Create;
    GRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}GObserver.Event;
    {$ifdef PAS2JS}GHost := TJSHTMLElement(document.getElementById('property-host'));
    {$else}
    GHost := TForm.CreateNew(nil);
    GHost.SetBounds(0, 0, 1000, 900);
    GHost.Show;
    {$endif}
    try
      for LIndex := 0 to GCatalog.Count - 1 do
      begin
        LPage := GDocument.Find('catalog-' + GCatalog[LIndex].Kind);
        GRenderer.Render(GDocument, LPage, GHost);
        Check(GRenderer.Root.Count = 3, 'unchanged MCP catalog partition');
        Visit(GRenderer.Root.Children[1]);
        Inc(GKinds);
      end;
      LayoutReview;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-property-checks', IntToStr(GChecks));
      document.body.setAttribute('data-property-kinds', IntToStr(GKinds));
      document.body.setAttribute('data-property-faces', IntToStr(GFaces));
      { Capture is explicit so the ordinary/manual harness keeps its existing
        synchronous completion. The maintained driver opts into the live mount. }

      if (window.location.search = '?capture=1') or
        (window.location.search = '?frame=1&capture=1') then
      begin
        GAwaitCapture := True;
        GCaptureStarted := window.performance.now;
        document.body.setAttribute('data-property-geometry', ReviewGeometry.ToJSON);
        document.body.setAttribute('data-capture-checkpoint', 'catalog-properties');
        window.setTimeout(@FinishCapture, 25);
      end
      else
      begin
        document.body.setAttribute('data-property-tests', 'passed');
      end;
      {$else}WriteLn('PASS ', GChecks, ' property checks / ', GKinds, ' kinds / ', GFaces, ' faces');{$endif}
    finally
      {$ifdef PAS2JS}

      if not GAwaitCapture then
      begin
        Retire;
      end;
      {$else}
      Retire;
      {$endif}
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-property-tests', 'failed');
      document.body.setAttribute('data-property-error', LException.Message);
      {$else}
      WriteLn(StdErr, 'FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      GRenderer.Free;
      GObserver.Free;
      GCatalog.Free;
      GDocument.Free;
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
{ The same test program executes inside an exact-width viewport. The outer
  capture process only forwards the child result; it never injects source or
  substitutes a smaller set of assertions for the phone run. }
procedure CheckFrame;
var
  LBody: TJSElement;
  LResult: String;
begin
  LBody := GFrame.contentDocument.body;
  LResult := LBody.getAttribute('data-property-tests');

  if (LResult = 'passed') or (LResult = 'failed') then
  begin
    document.body.setAttribute('data-property-tests', LResult);
    document.body.setAttribute('data-property-checks', LBody.getAttribute('data-property-checks'));
    document.body.setAttribute('data-property-kinds', LBody.getAttribute('data-property-kinds'));
    document.body.setAttribute('data-property-faces', LBody.getAttribute('data-property-faces'));
    document.body.setAttribute('data-property-error', LBody.getAttribute('data-property-error'));
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
    GFrame.src := 'properties.html?frame=1';
    document.body.appendChild(GFrame);
    window.setTimeout(@CheckFrame, 25);
  end
  else
  begin
    Run;
  end;
  {$else}
  Run;
  {$endif}
end.
