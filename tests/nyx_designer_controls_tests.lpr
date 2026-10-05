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

program nyx_designer_controls_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.editing, nyx.events,
  nyx.behavior, nyx.studio.commands,
  nyx.scheduler, nyx.controls, nyx.studio.session, nyx.studio.source,
  nyx.studio.view, nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser, nyx.test.keyboard.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Types, Classes,
    Graphics, IntfGraphics, FPWritePNG, nyx.theme, nyx.widgets.lcl, nyx.render.lcl;{$endif}

type
  { Only the editor commands own document publication. The two actual adapter
    views own realized controls; neither takes ownership of this session. Paint
    never occurs inside a native notification in this boundary fixture. }
  TEditor = class
    Selections: Integer;
    Values: Integer;
    Runtime: Integer;
    CreatorClicks: Integer;
    CallbackError: TNyxText;
    procedure Canvas(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Source(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    {$ifndef PAS2JS}procedure CreatorClick(ASender: TObject);{$endif}
    {$ifndef PAS2JS}procedure NativeFailure(ASender: TObject; AException: Exception);{$endif}
  end;
  TApplicationCallback = class(TNyxEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifndef PAS2JS}
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  {$endif}

var
  GChecks: Integer;
  GApplicationCallbacks: Integer;
  GSession: TNyxStudioSession;
  GCodeDocument: TNyxDocument;
  GEditor: TEditor;
  {$ifdef PAS2JS}
  GCanvas: TNyxBrowserRenderer;
  GCode: TNyxBrowserRenderer;
  GFirst: TJSHTMLElement;
  GSecond: TJSHTMLElement;
  GCodeFirst: TJSHTMLElement;
  GCodeSecond: TJSHTMLElement;
  GFrame: TJSHTMLIFrameElement;
  {$else}
  GCanvas: TNyxLCLRenderer;
  GCode: TNyxLCLRenderer;
  GForm: TForm;
  GFirst: TPanel;
  GSecond: TPanel;
  GCodeFirst: TPanel;
  GCodeSecond: TPanel;
  GTheme: TNyxTheme;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if (GEditor <> nil) and (GEditor.CallbackError <> '') then
  begin
    raise ENyxModel.Create('Designer callback: ' + GEditor.CallbackError);
  end;

  if not ACondition then
  begin
    raise ENyxModel.Create('Designer controls: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TApplicationCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GApplicationCallbacks);
end;

procedure TEditor.Canvas(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  try
    case AEvent.Trigger of
      ntDesignSelect:
        begin
          GSession.Select(ANode.DesignID);
          GCanvas.Select(GSession.SelectedID);
          Inc(Selections);
        end;
      ntDesignValue:
        begin
          GSession.SetCanvasValue(ANode);
          Inc(Values);
        end;
      else
        begin
          Inc(Runtime);
        end;
    end;
  except
    on LException: Exception do
    begin
      CallbackError := LException.Message;
    end;
  end;
end;

procedure TEditor.Source(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  try
    RouteNyxStudioSource(GSession, ANode, AEvent.Trigger);
  except
    on LException: Exception do
    begin
      CallbackError := LException.Message;
    end;
  end;
end;

{$ifndef PAS2JS}
{ Optional visual evidence paints the actual native canvas/source hierarchy.
  It neither changes document state nor uses a browser embedded in the window. }
procedure CaptureNative(AControl: TWinControl; const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LFace: TControl;
  LPoint: TPoint;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  ForceDirectories(ParamStr(1));
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AControl.Width, AControl.Height);
    AControl.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);

    if (AName = 'designer-canvas') or (AName = 'designer-root') then
    begin

      if AName = 'designer-root' then
      begin
        LFace := GCanvas.ControlFor(GCanvas.Root.ID);
        LPoint := AControl.ScreenToClient(LFace.ClientToScreen(Types.Point(1, 60)));
      end
      else
      begin
        LFace := GCanvas.ControlFor('designer-first');
        LPoint := AControl.ScreenToClient(LFace.ClientToScreen(Types.Point(-1, LFace.Height div 2)));
      end;

      Check(ColorToRGB(LBitmap.Canvas.Pixels[LPoint.X, LPoint.Y]) =
        ColorToRGB(NyxLCLColor(GTheme.Accent)),
        'native selection outline is actually painted beside its component at ' +
        IntToStr(LPoint.X) + ',' + IntToStr(LPoint.Y));
    end;
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

{ Report widgetset callback failures to the next ordinary assertion instead of
  opening Lazarus's modal exception dialog and stranding the owned fixture. }
procedure TEditor.NativeFailure(ASender: TObject; AException: Exception);
begin
  CallbackError := AException.ClassName + ': ' + AException.Message;
end;

procedure TEditor.CreatorClick(ASender: TObject);
begin
  Inc(CreatorClicks);
end;

function CreatorButton(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TNyxLCLButton.Create(AOwner);
  TNyxLCLButton(Result).Caption := ANode.Prop('text');
  TNyxLCLButton(Result).OnClick := GEditor.CreatorClick;
end;
{$endif}

{ Locate by the portable authored identity and closed projection kind, without
  guessing a target's encoded reusable runtime ID. The returned node is borrowed
  only while the view remains mounted. }
function Part(ANode: TNyxNode; const AOwner: TNyxText; AKind: TNyxKind): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if (ANode.DesignID = AOwner) and (ANode.ProjectionKind = NyxKindName(AKind)) then
  begin
    Exit(ANode);
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Result := Part(ANode.Children[LIndex], AOwner, AKind);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

procedure Click(ARenderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  const AID: TNyxText);
begin
  {$ifdef PAS2JS}ARenderer.ElementFor(AID).click;
  {$else}TControlAccess(ARenderer.ControlFor(AID)).Click;{$endif}
end;

function PhysicalCode: TNyxText;
begin
  {$ifdef PAS2JS}Result := TJSHTMLTextAreaElement(GCode.InputFor('studio-code')).value;
  {$else}Result := TNyxText(TMemo(GCode.InputFor('studio-code')).Text);{$endif}
end;

function DefinitionSnapshot: TNyxText;
var
  LDocument: TNyxDocument;
begin
  { Compare the whole owned definition through its qualified persistence codec,
    not just the edited field. This detached fixture never replaces a project. }
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('definition-check'));
    LDocument.AddComponent(GSession.Document.Find('designer-reply').Clone);
    Result := TNyxCodec.Encode(LDocument);
  finally
    LDocument.Free;
  end;
end;

procedure Run;
const
  CDraft: TNyxText = '// An independent source draft / 🌙 漢字';
  CReply: TNyxText = 'A customized reply / 🌙 漢字';
  { Initial review copy is English. The edit and pending draft above retain
    independent supplementary/Chinese coverage through both actual adapters. }
  COriginal: TNyxText = 'Your reply starts here.';
var
  LOriginal: TNyxDocument;
  LTemplate: TNyxText;
  LSource: TNyxText;
  LDraft: TNyxText;
  LFirstMemo: TNyxNode;
  LSecondMemo: TNyxNode;
  LSend: TNyxNode;
  LCallback: INyxEventCallback;
  LClick: INyxEventSubscription;
  LText: INyxEventSubscription;
  LFocus: INyxEventSubscription;
  LKey: INyxEventSubscription;
  LSelection: TNyxTextSelection;
  LAfter: TNyxTextSelection;
  LCount: Integer;
  LRejected: Boolean;
  LSuccess: Boolean;
  {$ifdef PAS2JS}
  LCodeInput: TJSHTMLTextAreaElement;
  LCanvasInput: TJSHTMLTextAreaElement;
  LNode: TJSHTMLElement;
  LScroll: Double;
  {$else}
  LCodeInput: TMemo;
  LCanvasInput: TMemo;
  LNode: TControl;
  LScroll: Integer;
  LCanvasHost: TScrollBox;
  LKeyCode: Word;
  {$endif}
begin
  LOriginal := nil;
  GEditor := nil;
  GSession := nil;
  GCanvas := nil;
  GCode := nil;
  GCodeDocument := nil;
  LSuccess := False;
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GForm := TForm.Create(nil);
    GForm.SetBounds(20, 20, 1120, 940);
    GFirst := TPanel.Create(GForm);
    GFirst.Parent := GForm;
    GFirst.SetBounds(0, 0, 540, 420);
    GSecond := TPanel.Create(GForm);
    GSecond.Parent := GForm;
    GSecond.SetBounds(550, 0, 540, 420);
    GCodeFirst := TPanel.Create(GForm);
    GCodeFirst.Parent := GForm;
    GCodeFirst.SetBounds(0, 450, 540, 420);
    GCodeSecond := TPanel.Create(GForm);
    GCodeSecond.Parent := GForm;
    GCodeSecond.SetBounds(550, 450, 540, 420);
    GForm.Show;
    {$else}
    GFirst := TJSHTMLElement(document.createElement('div'));
    GSecond := TJSHTMLElement(document.createElement('div'));
    GCodeFirst := TJSHTMLElement(document.createElement('div'));
    GCodeSecond := TJSHTMLElement(document.createElement('div'));
    GFirst.style.cssText := 'height:420px;max-width:640px;overflow:auto;';
    GSecond.style.cssText := GFirst.style.cssText;
    GCodeFirst.style.cssText := GFirst.style.cssText;
    GCodeSecond.style.cssText := GFirst.style.cssText;
    document.body.appendChild(GFirst);
    document.body.appendChild(GSecond);
    document.body.appendChild(GCodeFirst);
    document.body.appendChild(GCodeSecond);
    {$endif}
    LOriginal := BuildNyxDocument;
    GSession := TNyxStudioSession.Create;
    GSession.Load(TNyxCodec.Encode(LOriginal));
    GSession.Activate('designer-review');
    LTemplate := DefinitionSnapshot;
    LSource := GSession.Source;
    GEditor := TEditor.Create;
    {$ifndef PAS2JS}Application.OnException := GEditor.NativeFailure;{$endif}
    {$ifdef PAS2JS}

    if window.location.search = '?narrow=1' then
    begin
      Check(window.innerWidth = 390, 'narrow companion runs at an actual 390-pixel viewport');
    end;
    {$endif}
    {$ifndef PAS2JS}GTheme := TNyxTheme.Create;{$endif}
    GCanvas := {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif}.Create
      {$ifndef PAS2JS}(GTheme){$endif};
    GCode := {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif}.Create
      {$ifndef PAS2JS}(GTheme){$endif};
    {$ifndef PAS2JS}GCanvas.RegisterFactory(NyxKindName(nkButton), CreatorButton);{$endif}
    GCanvas.OnEvent := {$ifdef PAS2JS}@{$endif}GEditor.Canvas;
    GCode.OnEvent := {$ifdef PAS2JS}@{$endif}GEditor.Source;
    GCanvas.Render(GSession.Document, GSession.ActiveView, GFirst, True);
    LFirstMemo := Part(GCanvas.Root, 'designer-first', nkMemo);
    LSecondMemo := Part(GCanvas.Root, 'designer-second', nkMemo);
    LSend := Part(GCanvas.Root, 'designer-first', nkButton);
    Check((LFirstMemo <> nil) and (LSecondMemo <> nil) and (LSend <> nil),
      'unchanged MCP companion realizes two independent reusable replies');
    LCallback := TApplicationCallback.Create;
    LClick := GCanvas.Events.On(NyxControlEvents(LSend.ID, niRuntime), ntClick).Subscribe(LCallback);
    LText := GCanvas.Events.OnBeforeTextInput(NyxControlEvents(LFirstMemo.ID, niRuntime)).Subscribe(LCallback);
    LFocus := GCanvas.Events.OnAfterEnter(NyxControlEvents(LFirstMemo.ID, niRuntime)).Subscribe(LCallback);
    LKey := GCanvas.Events.OnBeforeKeyDown(NyxControlEvents(LFirstMemo.ID, niRuntime)).Subscribe(LCallback);
    Click(GCanvas, LSend.ID);
    Check((GEditor.Selections = 1) and (GSession.SelectedID = 'designer-first'),
      'native/browser command selects the authored instance');
    {$ifdef PAS2JS}
    Check((GFirst.querySelectorAll('.nyx-selected').length = 1) and
      GCanvas.ElementFor('designer-first', niDesign).classList.contains('nyx-selected'),
      'reusable design selection outlines its outer authored face once');
    {$endif}
    Check((GApplicationCallbacks = 0) and (GEditor.Runtime = 0) and
      (GEditor.CreatorClicks = 0), 'designer selection executes no application/creator callback');
    Check(LFirstMemo.Prop('value') = COriginal,
      'designer selection does not execute the reusable application clear action');
    Check((GSession.Source = LSource) and not GSession.CanUndo,
      'selection creates no generated-source or paired-history edit');
    Click(GCanvas, LFirstMemo.ID);
    {$ifdef PAS2JS}
    LCanvasInput := TJSHTMLTextAreaElement(GCanvas.InputFor(LFirstMemo.ID));
    LCanvasInput.focus;
    LCanvasInput.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'a'));
    LCanvasInput.value := CReply;
    LCanvasInput.dispatchEvent(TJSEvent.new('input'));
    {$else}
    LCanvasInput := TMemo(GCanvas.InputFor(LFirstMemo.ID));
    LCanvasInput.SetFocus;
    LKeyCode := 65;
    TWinControlAccess(TWinControl(LCanvasInput)).KeyDown(LKeyCode, []);
    LCanvasInput.Text := CReply;
    Application.ProcessMessages;
    {$endif}
    Check(GEditor.Values > 0, 'actual memo typing supplies an authored value proposal');
    Check(GSession.CanUndo and (Pos(CReply, GSession.Source) > 0),
      'accepted canvas value reaches typed Pascal and one paired undo boundary');
    Check((GApplicationCallbacks = 0) and (GEditor.Runtime = 0),
      'typing and focus in designer purpose bypass runtime subscribers');
    Check((DefinitionSnapshot = LTemplate) and
      (LSecondMemo.Prop('value') = COriginal), 'instance editing preserves its recipe and sibling');
    GSession.Undo;
    Check((GSession.Source = LSource) and GSession.CanRedo and not GSession.CanUndo,
      'one Undo restores the exact accepted source');
    GSession.Redo;
    Check(Pos(CReply, GSession.Source) > 0, 'paired Redo restores the independent override');

    { Studio consumes its public code-editor component, in a separate retained
      view. Draft typing never replaces the accepted design or Pascal. }
    GCodeDocument := TNyxDocument.Create;
    GCodeDocument.AddPage(NewNyxStudioCodeEditor(GSession.Source));
    GCodeDocument.Pages[0].Configure.Height(800).Done;
    GCode.Render(GCodeDocument, GCodeDocument.Pages[0], GCodeFirst);
    LDraft := GSession.Source + #10 + CDraft;
    {$ifdef PAS2JS}
    LCodeInput := TJSHTMLTextAreaElement(GCode.InputFor('studio-code'));
    LCodeInput.value := LDraft;
    LCodeInput.dispatchEvent(TJSEvent.new('input'));
    LCodeInput.focus;
    {$else}
    LCodeInput := TMemo(GCode.InputFor('studio-code'));
    LCodeInput.Text := LDraft;
    LCodeInput.SetFocus;
    Application.ProcessMessages;
    {$endif}
    Check(GSession.ProjectSnapshot.Pending and (GSession.DraftSource = LDraft),
      'public source editor retains an exact supplementary-Unicode draft');
    LSelection := NyxTextSelection(PhysicalCode, NyxTextScalarCount(PhysicalCode) - 4,
      NyxTextScalarCount(PhysicalCode) - 1);
    GCode.SetTextSelection('studio-code', LSelection);
    {$ifdef PAS2JS}GCodeFirst.scrollTop := 70;
    LScroll := GCodeFirst.scrollTop;
    {$else}TScrollBox(GCode.ControlFor('studio-code').Parent).VertScrollBar.Position := 70;
    LScroll := TScrollBox(GCode.ControlFor('studio-code').Parent).VertScrollBar.Position;{$endif}
    Check(LScroll > 0, 'source view has actual containing scroll before moving');
    GCode.MoveHost(GCodeSecond);
    Check(GCode.InputFor('studio-code') = LCodeInput, 'moving chrome retains the actual code editor object');
    LAfter := GCode.TextSelectionFor('studio-code');
    Check((LAfter.Start = LSelection.Start) and (LAfter.Finish = LSelection.Finish),
      'reparenting retains its Unicode scalar selection');
    {$ifdef PAS2JS}
    Check(document.activeElement = LCodeInput, 'reparenting retains actual browser source focus');
    Check(GCodeSecond.scrollTop = LScroll, 'reparenting retains containing browser scroll');
    GCodeFirst.remove;
    {$else}
    Check(GForm.ActiveControl = LCodeInput, 'reparenting retains actual native source focus');
    Check(TScrollBox(GCode.ControlFor('studio-code').Parent).VertScrollBar.Position = LScroll,
      'reparenting retains containing native scroll');
    FreeAndNil(GCodeFirst);
    {$endif}
    Check(GSession.DraftSource = LDraft, 'freeing the former host preserves the source draft');
    GCanvas.Select('designer-first');
    {$ifdef PAS2JS}Check(document.activeElement = LCodeInput, 'canvas outline never steals source focus');
    {$else}Check(GForm.ActiveControl = LCodeInput, 'native outline never steals source focus');{$endif}

    { Refused reparenting cannot discard the live view or partially move it. }
    LRejected := False;
    try
      GCode.MoveHost(nil);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (GCode.InputFor('studio-code') = LCodeInput),
      'nil host refuses while preserving the mounted editor');
    LRejected := False;
    try
      {$ifdef PAS2JS}GCode.MoveHost(GCode.ElementFor('studio-code'));
      {$else}GCode.MoveHost(TWinControl(GCode.ControlFor('studio-code')));{$endif}
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (GSession.DraftSource = LDraft), 'descendant host refuses without source loss');
    GCode.MoveHost(GCodeSecond);
    Check(GCode.InputFor('studio-code') = LCodeInput, 'same-host move retains its mounted control');
    LRejected := False;
    try
      GCanvas.MoveHost(GCodeSecond);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (GCode.InputFor('studio-code') = LCodeInput),
      'occupied replacement host refuses without displacing either view');

    {$ifdef PAS2JS}
    LNode := GCanvas.ElementFor(LFirstMemo.ID);
    GFirst.scrollTop := 90;
    LScroll := GFirst.scrollTop;
    {$else}
    LNode := GCanvas.ControlFor(LFirstMemo.ID);
    LCanvasHost := TScrollBox(GCanvas.ControlFor(GCanvas.Root.ID).Parent);
    LCanvasHost.VertScrollBar.Position := 90;
    LScroll := LCanvasHost.VertScrollBar.Position;
    {$endif}
    Check(LScroll > 0, 'designer fixture has actual containing scroll');
    GCanvas.MoveHost(GSecond);
    Check({$ifdef PAS2JS}GCanvas.ElementFor{$else}GCanvas.ControlFor{$endif}(LFirstMemo.ID) = LNode,
      'moving the designer retains its reusable memo face');
    {$ifdef PAS2JS}Check(GSecond.scrollTop = LScroll, 'designer containing browser scroll is retained');
    GFirst.remove;
    {$else}Check(LCanvasHost.VertScrollBar.Position = LScroll, 'designer containing native scroll is retained');
    FreeAndNil(GFirst);{$endif}
    Check((GSession.DraftSource = LDraft) and (LSecondMemo.Prop('value') = COriginal),
      'designer move and old-host destruction preserve source and sibling');

    { Imperative subscriptions belong to the subscribing caller. Releasing or
      replacing their interface is not an unsubscribe; retire the designer spies
      explicitly before installing the ordinary runtime comparison. }
    LClick.Cancel;
    LText.Cancel;
    LFocus.Cancel;
    LKey.Cancel;
    LCount := GApplicationCallbacks;
    GCanvas.Render(GSession.Document, GSession.ActiveView, GSecond, False);
    LSend := Part(GCanvas.Root, 'designer-first', nkButton);
    LClick := GCanvas.Events.On(NyxControlEvents(LSend.ID, niRuntime), ntClick).Subscribe(LCallback);
    Click(GCanvas, LSend.ID);
    Check(GApplicationCallbacks = LCount + 1, 'ordinary runtime callback behavior remains active (' +
      IntToStr(GApplicationCallbacks - LCount) + ' invocation)');
    Check(Part(GCanvas.Root, 'designer-first', nkMemo).Prop('value') = '',
      'ordinary runtime dispatch executes the reusable clear action');
    {$ifndef PAS2JS}Check(GEditor.CreatorClicks = 1, 'runtime restores captured creator click behavior');{$endif}
    Check(GSession.DraftSource = LDraft, 'runtime view does not mutate the editor source draft');
    GCanvas.Unmount;
    LRejected := False;
    try
      GCanvas.MoveHost(GSecond);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'an unmounted view cannot move into another host');
    GCanvas.Render(GSession.Document, GSession.ActiveView, GSecond, True);
    GCanvas.Select('designer-first');
    {$ifndef PAS2JS}
    Application.ProcessMessages;
    CaptureNative(GSecond, 'designer-canvas');
    GCanvas.Select('designer-review');
    CaptureNative(GSecond, 'designer-root');
    GCanvas.Select('designer-first');
    CaptureNative(GCodeSecond, 'designer-source');
    {$endif}
    LSuccess := True;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-designer-tests', 'passed');
    document.body.setAttribute('data-designer-checks', IntToStr(GChecks));
    {$else}WriteLn('PASS ', GChecks, ' actual designer and retained source-view checks');{$endif}
  finally
    LClick := nil;
    LText := nil;
    LFocus := nil;
    LKey := nil;
    LCallback := nil;
    {$ifdef PAS2JS}

    if not LSuccess then
    begin
    {$endif}
    { Always release views before their documents/hosts and event receiver. }
    GCanvas.Free;
    GCode.Free;
    {$ifndef PAS2JS}GTheme.Free;{$endif}
    GCodeDocument.Free;
    GSession.Free;
    {$ifndef PAS2JS}Application.OnException := nil;{$endif}
    GEditor.Free;
    LOriginal.Free;
    {$ifndef PAS2JS}GForm.Free;{$endif}
    {$ifdef PAS2JS}end;{$endif}
  end;
end;

{$ifdef PAS2JS}
procedure CheckFrame;
var
  LBody: TJSElement;
  LState: String;
begin
  LBody := GFrame.contentDocument.body;
  LState := LBody.getAttribute('data-designer-tests');

  if (LState = 'passed') or (LState = 'failed') then
  begin
    document.body.setAttribute('data-designer-tests', LState);
    document.body.setAttribute('data-designer-checks', LBody.getAttribute('data-designer-checks'));
    document.body.setAttribute('data-designer-error', LBody.getAttribute('data-designer-error'));
  end
  else
  begin
    window.setTimeout(@CheckFrame, 25);
  end;
end;
{$endif}

begin
  try
    {$ifdef PAS2JS}

    if window.location.search = '?host=1' then
    begin
      GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
      GFrame.style.cssText := 'width:390px;height:940px;border:0;display:block;';
      document.body.appendChild(GFrame);
      GFrame.src := 'designer.html?narrow=1';
      window.setTimeout(@CheckFrame, 25);
    end
    else
    {$endif}
    begin
      Run;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}document.body.setAttribute('data-designer-tests', 'failed');
      document.body.setAttribute('data-designer-error', LException.Message);
      {$else}WriteLn(StdErr, 'FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;{$endif}
    end;
  end;
end.
