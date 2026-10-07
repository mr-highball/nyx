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
program nyx_content_revisions_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.content, nyx.content.editor, nyx.responsive, nyx.presentations, nyx.events, nyx.behavior,
  nyx.data, nyx.codec, nyx.codegen, nyx.schema, nyx.studio.projects, nyx.studio.inspector,
  nyx.studio.sourcejobs, nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, Spin, Graphics, IntfGraphics,
    FPWritePNG, nyx.studio.lcl;{$endif}

const
  CEditor = 'inspector-content';
  { Frozen-parent proof uses the same stable row identity without importing
    a new symbol into its old public library. Normal authoring uses RuleID. }
  CFirstEdit = 'inspector-content-rule-0-edit';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Recipe revision: ' + AReason);
  end;
  Inc(GChecks);
end;

function Choices: INyxContent;
begin
  Result := NewNyxContent;
  Result.WhenViewport(TNyxViewportWidth.Below(640)).Use(NyxComponent('compact-card'));
  Result.WhenPresentation(NyxPresentation('focused')).Use(NyxComponent('reading-card'));
  Result := Result.Done;
end;

{$ifndef NYX_RECIPE_BASELINE}
function Form(const AContent: INyxContent; const AOwner: TNyxControlRef;
  const ADefault: TNyxComponentRef): INyxCard;
begin
  Result := NewNyxContentEditor(CEditor, AOwner, AContent, ADefault,
    [NyxComponent('comfortable-card'), NyxComponent('compact-card'), NyxComponent('reading-card')],
    [NyxPresentation('focused')]);
end;

function Field(const AForm: INyxCard; AField: TNyxContentEditorField): TNyxNode;
begin
  Result := AForm.Node.Find(NyxContentEditorFieldID(CEditor, AField));
end;

procedure Snapshots;
var
  LContent: INyxContent;
  LRevised: INyxContent;
  LForm: INyxCard;
  LFresh: INyxCard;
  LDraft: TNyxContentEditorDraft;
  LCopy: TNyxContentEditorDraft;
  LChange: TNyxContentEditorChange;
  LBefore: TNyxText;
  LRefused: Boolean;
  LWrong: INyxMemo;
  LForged: INyxButton;
  LInteger: INyxSpin;
begin
  { The form must not truncate a valid portable condition at its numeric input
    boundary. Spin admits the complete Integer range; unrelated progress/layout
    limits remain unchanged. These are public fluent setters, not raw props. }
  LInteger := NewNyxSpin('integer-bounds');
  LInteger.Minimum := Low(Integer);
  LInteger.Maximum := High(Integer);
  LInteger.Value := High(Integer);
  Check((LInteger.Minimum = Low(Integer)) and (LInteger.Maximum = High(Integer)) and
    (LInteger.Value = High(Integer)), 'specialized spin preserves exact signed bounds');
  LInteger.Value := Low(Integer);
  ValidateNyxProperties(LInteger.Node);
  Check(LInteger.Value = Low(Integer), 'spin descriptor/value admission retains the negative boundary too');
  LRefused := False;
  try
    NewNyxProgress('bounded-progress').Configure.Maximum(High(Integer)).Done;
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'spin range admission does not loosen unrelated controls');
  LContent := Choices;
  LBefore := LContent.ToData.ToJSON;
  LForm := Form(LContent, NyxControl('workspace'), NyxComponent('comfortable-card'));
  Check(CaptureNyxContentEditorRule(LForm.Node.Find(
    NyxContentEditorRuleID(CEditor, 0, ncraEdit)), LForm.Node, LDraft), 'exact edit row recognized');
  Check(LDraft.Defined and LDraft.Matches(NyxControl('workspace'), LContent,
    NyxComponent('comfortable-card')), 'prefill retains exact owner/default/registry');
  Check(LDraft.Restore(LForm.Node), 'complete local prefill restores');
  Check(Field(LForm, ncfScope).Prop('value') = NyxContentEditorScopeName(ncsViewport),
    'prefill selects the typed viewport scope');
  Check(Field(LForm, ncfWidthMaximum).Prop('value') = '640', 'prefill retains its bound');
  Check(LContent.ToData.ToJSON = LBefore, 'prefill changes no accepted registry');
  Check(not CaptureNyxContentEditor(LForm.Node.Find(
    NyxContentEditorRuleID(CEditor, 0, ncraEdit)), LForm.Node, LChange, LRevised),
    'edit action never becomes a source mutation');
  Field(LForm, ncfWidthMaximum).Configure.Value(720).Done;
  Check(CaptureNyxContentEditor(Field(LForm, ncfApply), LForm.Node, LChange, LRevised),
    'revised condition creates one complete candidate');
  Check((LRevised.Count = 2) and (LRevised.Rule(0).Viewport.WidthMaximum = 720) and
    (LRevised.Rule(1).Scope = ncsPresentation), 'scope replacement preserves count/evaluation order');
  Check(LContent.ToData.ToJSON = LBefore, 'candidate owns independent rule storage');
  Field(LForm, ncfScope).Configure.Value(NyxContentEditorScopeName(ncsPresentation)).Done;
  Field(LForm, ncfPlatform).Configure.Value(NyxContentEditorPlatformName(npfAny)).Done;
  LRefused := False;
  try
    CaptureNyxContentEditor(Field(LForm, ncfApply), LForm.Node, LChange, LRevised);
  except
    on ENyxContent do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LContent.ToData.ToJSON = LBefore), 'colliding scope refuses without merging');
  Field(LForm, ncfWidthMaximum).Configure.Value(900).Done;
  LDraft.Capture(CEditor, LForm.Node);
  LCopy := LDraft;
  Field(LForm, ncfWidthMaximum).Configure.Value(901).Done;
  LDraft.Capture(CEditor, LForm.Node);
  LForm := nil;
  LFresh := Form(LContent, NyxControl('workspace'), NyxComponent('comfortable-card'));
  Check(LCopy.Restore(LFresh.Node) and (Field(LFresh, ncfWidthMaximum).Prop('value') = '900'),
    'record copy survives form retirement independently');
  Check(LDraft.Restore(LFresh.Node) and (Field(LFresh, ncfWidthMaximum).Prop('value') = '901'),
    'later capture cannot change an earlier record copy');
  Check(not LDraft.Restore(nil) and LDraft.Defined, 'absent inspector parks the proposal');
  LFresh := Form(LContent, NyxControl('other'), NyxComponent('comfortable-card'));
  Check(not LDraft.Restore(LFresh.Node) and not LDraft.Defined, 'changed owner retires input');
  Check(Field(LFresh, ncfWidthMaximum).Prop('value') = '640', 'refusal publishes no partial field values');
  LDraft := LCopy;
  LFresh := Form(LContent, NyxControl('workspace'), NyxComponent('reading-card'));
  Check(not LDraft.Restore(LFresh.Node), 'compatible default change retires input');
  LDraft := LCopy;
  LFresh := NewNyxContentEditor(CEditor, NyxControl('workspace'), LContent,
    NyxComponent('comfortable-card'), [NyxComponent('compact-card')], [NyxPresentation('focused')]);
  Check(not LDraft.Restore(LFresh.Node), 'changed recipe choices retire input');
  LDraft := LCopy;
  LFresh := Form(LContent, NyxControl('workspace'), NyxComponent('comfortable-card'));
  LFresh.Node.Remove(Field(LFresh, ncfOrientation));
  LWrong := NewNyxMemo(NyxContentEditorFieldID(CEditor, ncfOrientation));
  LFresh.Add(LWrong);
  Check(not LDraft.Restore(LFresh.Node), 'wrong last field kind refuses before any write');
  Check(Field(LFresh, ncfWidthMaximum).Prop('value') = '640', 'complete shape validation is atomic');
  LDraft := LCopy;
  LFresh := Form(LContent, NyxControl('workspace'), NyxComponent('comfortable-card'));
  LForged := NewNyxButton(NyxContentEditorRuleID(CEditor, 0, ncraEdit));
  LForged.Node.SetProp('nyx.content-editor.edit', '0').SetProp('nyx.content-editor', CEditor);
  LRefused := False;
  try
    CaptureNyxContentEditorRule(LForged.Node, LFresh.Node, LDraft);
  except
    on ENyxContent do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LDraft.Defined, 'same-ID forged button has no prefill authority');
  LDraft := LCopy;
  LDraft.Clear;
  Check(not LDraft.Defined and not LDraft.Restore(LFresh.Node), 'explicit replacement retires matching text');
  LContent.WhenViewport(TNyxViewportCondition.Any.WidthBetween(2147483646, High(Integer)))
    .ForPlatform(npfNativeLCL).Use(NyxComponent('compact-card'));
  LFresh := Form(LContent, NyxControl('workspace'), NyxComponent('comfortable-card'));
  Check(CaptureNyxContentEditorRule(LFresh.Node.Find(
    NyxContentEditorRuleID(CEditor, 2, ncraEdit)), LFresh.Node, LDraft) and LDraft.Restore(LFresh.Node),
    'full positive 32-bit condition can be loaded');
  Check((Field(LFresh, ncfWidthMinimum).Prop('value') = '2147483646') and
    (Field(LFresh, ncfWidthMaximum).Prop('value') = '2147483647'), 'large bounds retain exact text');
  Check(CaptureNyxContentEditor(Field(LFresh, ncfApply), LFresh.Node, LChange, LRevised) and
    (LRevised.ToData.ToJSON = LContent.ToData.ToJSON), 'no-op large condition preserves its descriptor');
end;
{$endif}

{$ifndef PAS2JS}
type
  TControlAccess = class(TControl);

procedure NativeStudio;
var
  LStudio: TNyxNativeStudio;
  LWindow: TForm;
  LDocument: TNyxDocument;
  LOriginal: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSource: TControl;
  LStream: TFileStream;
  LText: TNyxText;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Studio work did not finish: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Click(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
  end;

  function Input(AField: TNyxContentEditorField): TControl;
  begin
    Result := LStudio.ShellView.InputFor(NyxContentEditorFieldID(CEditor, AField));
  end;

  procedure Number(AField: TNyxContentEditorField; AValue: Integer);
  var
    LSpin: TSpinEdit;
  begin
    LSpin := TSpinEdit(Input(AField));
    LSpin.Value := AValue;
    LSpin.OnChange(LSpin);
  end;

  {$ifndef NYX_RECIPE_BASELINE}
  procedure Choice(AField: TNyxContentEditorField; const AValue: TNyxText);
  var
    LSelect: TComboBox;
  begin
    LSelect := TComboBox(Input(AField));
    LSelect.ItemIndex := LSelect.Items.IndexOf(AValue);
    LSelect.OnChange(LSelect);
  end;

  procedure Capture(const AName: TNyxText);
  var
    LBitmap: TBitmap;
    LImage: TLazIntfImage;
    LWriter: TFPWriterPNG;
  begin
    ForceDirectories(ParamStr(1));
    { Reveal the actual numeric fields through the nested native sidebar. A
      shell capture at its top would show only the card heading, and therefore
      provide no evidence of the retained/correctable proposal itself. }
    TScrollBox(LStudio.ShellView.ControlFor('studio-right')).ScrollInView(
      LStudio.ShellView.ControlFor(NyxContentEditorFieldID(CEditor, ncfApply)));
    Ready;
    LBitmap := TBitmap.Create;
    LImage := nil;
    LWriter := nil;
    try
      LBitmap.SetSize(LWindow.ClientWidth, LWindow.ClientHeight);
      LWindow.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LWriter := TFPWriterPNG.Create;
      LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
    finally
      LWriter.Free;
      LImage.Free;
      LBitmap.Free;
    end;
  end;
  {$endif}
begin
  LStudio := nil;
  LWindow := TForm.CreateNew(nil);
  try
    LWindow.ClientWidth := 1280;
    LWindow.ClientHeight := 920;
    LWindow.Show;
    LStudio := TNyxNativeStudio.Create(LWindow,
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    LDocument := BuildNyxDocument;
    try
      { The compiled English seed is the unchanged authenticated export. This
        local typed enrichment adds initial rules only; it is not a claimed
        content mutation through the frozen server's unsupported schema. }
      LDocument.Find('workspace').SetContent(Choices);
      LOriginal := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
      LStudio.LoadProject(LOriginal);
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('workspace');
    LStudio.Run;
    Ready;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Number(ncfWidthMaximum, 900);
    Click('action-code');
    {$ifdef NYX_RECIPE_BASELINE}
    WriteLn('BASELINE / ordinary repaint bound ', TSpinEdit(Input(ncfWidthMaximum)).Value);
    Click(NyxInspectorEventsID);
    Check(LStudio.ShellView.Root.Find(CEditor) = nil, 'frozen events tab removes the form');
    Click(NyxInspectorPropertiesID);
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = 640, 'frozen parent loses the unsubmitted bound');
    Check(LStudio.ShellView.Root.Find(CFirstEdit) = nil, 'frozen parent lacks an edit-row affordance');
    WriteLn('BASELINE GAPS / proposal reset and no existing-choice editing');
    {$else}
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = 900, 'unsubmitted numeric proposal survives shell rebuild');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'draft is outside design/source history');
    LSource := LStudio.CodeView.InputFor('studio-code');
    Click(NyxContentEditorRuleID(CEditor, 0, ncraEdit));
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = 640, 'actual edit button loads its original bound');
    Check(TComboBox(Input(ncfScope)).Text = NyxContentEditorScopeName(ncsViewport), 'actual edit loads its typed scope');
    Check(TWinControl(Input(ncfScope)).Focused, 'edit brings the form field into focus');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'prefill publishes no history');
    Number(ncfWidthMaximum, 720);
    Click(NyxInspectorEventsID);
    Check(LStudio.ShellView.Root.Find(CEditor) = nil, 'events tab parks the recipe form');
    Click(NyxInspectorPropertiesID);
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = 720, 'properties return restores the edited bound');
    LWindow.ClientWidth := 390;
    Ready;
    Click('action-panel-project');
    Click('action-panel-inspector');
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = 720, 'compact panel parking retains the same proposal');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'presentation changes create no paired history');
    Number(ncfWidthMinimum, 800);
    Click(NyxContentEditorFieldID(CEditor, ncfApply));
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'invalid interval refuses atomic source admission');
    Check((TSpinEdit(Input(ncfWidthMinimum)).Value = 800) and
      (TSpinEdit(Input(ncfWidthMaximum)).Value = 720), 'invalid proposal remains available for correction');
    Capture('recipe-correction-compact');
    Number(ncfWidthMinimum, 0);
    Click(NyxContentEditorFieldID(CEditor, ncfApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'corrected physical form reaches paired processor');
    Check((LStudio.Session.Selected.Content.Count = 2) and
      (LStudio.Session.Selected.Content.Rule(0).Viewport.WidthMaximum = 720) and
      (LStudio.Session.Selected.Content.Rule(1).Scope = ncsPresentation),
      'edited condition replaces the original row in its evaluation order');
    Check(LStudio.CodeView.InputFor('studio-code') = LSource, 'accepted edit retains the actual source control');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'one actual Undo restores the exact paired original');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'one actual Redo restores the exact paired revision');
    Click(NyxContentEditorRuleID(CEditor, 0, ncraEdit));
    Choice(ncfScope, NyxContentEditorScopeName(ncsPresentation));
    Click(NyxContentEditorFieldID(CEditor, ncfApply));
    Check((EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter) and
      (Pos('Another choice', LStudio.Status) > 0), 'colliding scope visibly refuses without source/history mutation');
    Check(TComboBox(Input(ncfScope)).Text = NyxContentEditorScopeName(ncsPresentation),
      'collision keeps the proposal for correction');
    Choice(ncfScope, NyxContentEditorScopeName(ncsViewport));
    Number(ncfWidthMaximum, High(Integer));
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = High(Integer), 'native numeric field preserves full positive 32-bit range');
    LWindow.ClientWidth := 1280;
    Ready;
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = High(Integer), 'large proposal survives a compact-to-desktop transition');
    Capture('recipe-revision-desktop');
    LText := LStudio.Session.Source;
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'accepted.pas.txt', fmCreate);
    try
      LStream.WriteBuffer(LText[1], Length(LText));
    finally
      LStream.Free;
    end;
    LStudio.LoadProject(LOriginal);
    LStudio.Session.Select('workspace');
    Ready;
    Check(TSpinEdit(Input(ncfWidthMaximum)).Value = 640, 'explicit project replacement clears even matching form context');
    {$endif}
  finally
    LStudio.Free;
    LWindow.Free;
  end;
end;
{$else}
type
  { Public edit/capture/Sync on actual browser controls. The application logic
    remains Pascal; this compiled consumer requires an admitted HTTP browser. }
  TBrowserForm = class
  public
    Renderer: TNyxBrowserRenderer;
    Draft: TNyxContentEditorDraft;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

procedure TBrowserForm.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and CaptureNyxContentEditorRule(ANode, Renderer.Root, Draft) then
  begin
    Draft.Restore(Renderer.Root);
    Renderer.Sync;
  end;
end;

procedure BrowserControls;
var
  LOwner: TBrowserForm;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LContent: INyxContent;
  LInput: TJSHTMLElement;
begin
  LOwner := TBrowserForm.Create;
  LDocument := TNyxDocument.Create;
  LOwner.Renderer := TNyxBrowserRenderer.Create;
  try
    LContent := Choices;
    LPage := NewNyxPage('form');
    LDocument.AddPage(LPage);
    LPage.Add(Form(LContent, NyxControl('workspace'), NyxComponent('comfortable-card')));
    LOwner.Renderer.OnEvent := @LOwner.Event;
    LOwner.Renderer.Render(LDocument, LDocument.Pages[0], TJSHTMLElement(document.body));
    LOwner.Renderer.ElementFor(NyxContentEditorRuleID(CEditor, 0, ncraEdit)).click;
    LInput := LOwner.Renderer.InputFor(NyxContentEditorFieldID(CEditor, ncfWidthMaximum));
    Check(TJSHTMLInputElement(LInput).value = '640', 'actual browser edit prefills its numeric control');
    TJSHTMLInputElement(LInput).value := '720';
    LInput.dispatchEvent(TJSEvent.new('change'));
    LOwner.Draft.Capture(CEditor, LOwner.Renderer.Root);
    LOwner.Renderer.Render(LDocument, LDocument.Pages[0], TJSHTMLElement(document.body));
    Check(LOwner.Draft.Restore(LOwner.Renderer.Root), 'public browser consumer restores its exact form');
    LOwner.Renderer.Sync;
    Check(TJSHTMLInputElement(LOwner.Renderer.InputFor(
      NyxContentEditorFieldID(CEditor, ncfWidthMaximum))).value = '720', 'browser remount/replay retains its proposal');
  finally
    LOwner.Renderer.Free;
    LPage := nil;
    LDocument.Free;
    LOwner.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    {$ifndef NYX_RECIPE_BASELINE}Snapshots;{$endif}
    {$ifdef PAS2JS}BrowserControls;{$else}NativeStudio;{$endif}
    WriteLn('PASS / recipe revisions / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
