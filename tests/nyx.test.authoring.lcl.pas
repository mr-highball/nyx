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

unit nyx.test.authoring.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxNativeAuthoringJourney: Integer;

implementation

uses
  nyx.behavior,
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  nyx.text,
  nyx.types,
  nyx.state,
  nyx.binding.types,
  nyx.data,
  nyx.contract,
  nyx.schema,
  nyx.model,
  nyx.codec,
  nyx.widgets.lcl,
  nyx.render.lcl,
  nyx.studio.session,
  nyx.source,
  nyx.studio.source,
  nyx.studio.compiler,
  nyx.studio.diagnostics,
  nyx.test.compiler,
  nyx.studio.view,
  nyx.studio.authoring,
  nyx.studio.commands,
  nyx.studio.inspector,
  nyx.studio.palette,
  nyx.catalog.labels,
  nyx.studio.projects,
  nyx.callbacks,
  nyx.test.source.managed,
  nyx.test.source.structural,
  nyx.test.binding;

type
  { Actual LCL events enter the same portable router as the browser controller.
    The fixture rebuilds after event return, outside widget notification lifetime;
    a complete native Studio controller remains a separate product outcome. }
  TNativeAuthoringProbe = class
  public
    Session: TNyxStudioSession;
    Renderer: TNyxLCLRenderer;
    Routed: Integer;
    Error: TNyxText;
    Removal: TNyxCallbackRemoval;
    SourceLine: Integer;
    SourceColumn: Integer;
    CompilerReport: INyxCompilerReport;
    Palette: TNyxStudioPaletteState;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

procedure TNativeAuthoringProbe.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LEffect: TNyxInspectorEffect;
  LRemoval: TNyxCallbackRemoval;
  LDiagnostic: TNyxSourceDiagnostic;
begin
  Error := '';
  try

    if RouteNyxCompilerDiagnostic(Session, ANode, AEvent.Trigger,
      CompilerReport, LDiagnostic) then
    begin
      SourceLine := LDiagnostic.Line;
      SourceColumn := LDiagnostic.Column;
      Inc(Routed);
      Exit;
    end;

    if RouteNyxSourceDiagnostic(Session, ANode, AEvent.Trigger, LDiagnostic) then
    begin
      SourceLine := LDiagnostic.Line;
      SourceColumn := LDiagnostic.Column;
      Inc(Routed);
      Exit;
    end;

    if RouteNyxStudioEvents(Session, ANode, AEvent.Trigger, Removal,
      LEffect, SourceLine, LRemoval) then
    begin
      case LEffect of
        nieRequestRemoval: Removal := LRemoval;
        nieRemoved, nieCancelRemoval: Removal.Pending := False;
      end;
      Inc(Routed);
    end
    else if RouteNyxStudioPalette(ANode, AEvent.Trigger, Palette) or
      RouteNyxStudioSource(Session, ANode, AEvent.Trigger) or
      RouteNyxStudioProperty(Session, ANode, AEvent.Trigger) or
      RouteNyxStudioAuthoring(Session, ANode, AEvent.Trigger, Renderer.Root) then
    begin
      Inc(Routed);
    end;
  except
    on LException: Exception do
    begin
      Error := LException.Message;
    end;
  end;
end;

function RunNyxNativeAuthoringJourney: Integer;
var
  LSession: TNyxStudioSession;
  LFixture: TNyxDocument;
  LShell: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LProbe: TNativeAuthoringProbe;
  LState: TNyxStudioViewState;
  LMemo: TMemo;
  LInput: TEdit;
  LChoice: TComboBox;
  LSpec: TNyxBindingSpec;
  LBefore: TNyxText;
  LCount: Integer;
  LIndex: Integer;
  LPreview: TNyxLCLRenderer;
  LPreviewHost: TForm;
  LPair: TNyxProjectPair;
  LCompilerDocument: TNyxDocument;
  LCompilerSource: TNyxText;
  LCompilerError: ENyxSource;
  LProperties: TNyxPropertyInfos;
  LPropertyIndex: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Native authoring: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Rebuild;
  begin
    LRenderer.Unmount;
    FreeAndNil(LShell);
    LShell := BuildNyxStudioView(LSession, LState, LProbe.CompilerReport);
    LRenderer.Render(LShell, LShell.Pages[0], LHost);
  end;

begin
  Result := 0;
  LCompilerDocument := nil;
  LSession := TNyxStudioSession.Create;
  LFixture := CreateNyxBindingFixture;
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.Create(nil);
  LProbe := TNativeAuthoringProbe.Create;
  LShell := nil;
  try
    LHost.SetBounds(0, 0, 1400, 900);
    LSession.Load(TNyxCodec.Encode(LFixture));
    LSession.Select('reply-memo');
    LState := DefaultNyxStudioViewState;
    LState.StateVisible := True;
    LState.BindingsVisible := True;
    LState.NewStateInput := ssiInteger;
    LState.NewStateValue := '0';
    LProbe.Session := LSession;
    LProbe.Palette := DefaultNyxStudioPaletteState;
    LProbe.Renderer := LRenderer;
    LRenderer.OnEvent := LProbe.Event;
    Rebuild;
    LInput := TEdit(LRenderer.InputFor('inspector-placeholder'));
    Check((Pos('Browser: Available / LCL: Basic support', LInput.Hint) > 0) and
      (Pos('widgetset', LInput.Hint) > 0) and LInput.ShowHint,
      'actual native property field exposes the qualified projection help');
    LBefore := LSession.Source;
    LState.AdvancedProperties := True;
    Rebuild;
    LProperties := NyxProperties(LSession.Selected, LSession.Document);
    LPropertyIndex := 0;
    while (LPropertyIndex < Length(LProperties)) and
      (LProperties[LPropertyIndex].Key <> 'split-position') do
    begin
      Inc(LPropertyIndex);
    end;
    Check((LPropertyIndex < Length(LProperties)) and
      (TLabel(LRenderer.ControlFor('property-support-' +
        IntToStr(LPropertyIndex))).Caption =
        'Pane sizing belongs to the split-view projection.') and
      (LSession.Source = LBefore),
      'native unsupported-property help is visible without changing source');
    LState.AdvancedProperties := False;
    Rebuild;
    LMemo := TMemo(LRenderer.InputFor('state-default-0'));
    Check(TNyxText(LMemo.Text) = TNyxText('Café / 🌙 / 漢字'),
      'saved Unicode defaults reach real native Studio fields');
    LMemo.Text := 'Native saved default / 🌙';
    Check((LProbe.Routed = 1) and
      (LSession.Document.State.Value('🌙/reply').TextValue = TNyxText(LMemo.Text)),
      'native default change enters the portable typed command');
    Rebuild;
    LInput := TEdit(LRenderer.InputFor('state-name-0'));
    LInput.Text := 'reply / 漢字';
    Check(LSession.Document.State.Has('reply / 漢字') and
      (LSession.Document.Find('reply-memo').Bindings[0].StateName = 'reply / 漢字'),
      'native name editor migrates default and references');
    Rebuild;
    TNyxLCLButton(LRenderer.ControlFor('binding-clear')).Click;
    Check(not LSession.Selected.FindBinding(bpValue, LSpec),
      'native binding action creates an independent typed clear');
    LSession.Undo;
    Rebuild;
    LBefore := LSession.Save;
    TNyxLCLButton(LRenderer.ControlFor('state-remove-0')).Click;
    Check((LProbe.Error <> '') and (LSession.Save = LBefore),
      'native used-default removal preserves accepted history');
    LInput := TEdit(LRenderer.InputFor(NyxStudioNewStateNameID));
    LInput.Text := 'counter / 🌙';
    TEdit(LRenderer.InputFor(NyxStudioNewStateValueID)).Text := '42';
    LCount := LSession.Document.State.Count;
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioAddStateID)).Click;
    Check((LProbe.Error = '') and (LSession.Document.State.Count = LCount + 1) and
      (LSession.Document.State.Value('counter / 🌙').IntegerValue = 42),
      'native Add command reads actual Nyx fields into integer state');
    Rebuild;
    LBefore := LSession.Save;
    LInput := TEdit(LRenderer.InputFor('state-default-' + IntToStr(LCount)));
    LInput.Text := '1.5';
    Check((LProbe.Error <> '') and (LSession.Save = LBefore),
      'native fractional integer edit cannot coerce accepted state');
    Rebuild;
    Check(TNyxText(TEdit(LRenderer.InputFor('state-default-' + IntToStr(LCount))).Text) = '42',
      'native rebuilding restores the admitted numeric default');
    LSession.CreateState('payload', TNyxStateValue.FromText(TNyxText('🌙') + NyxScalarText(0) + 'exact'));
    Rebuild;
    LIndex := LSession.Document.State.Count - 1;
    LMemo := TMemo(LRenderer.InputFor('state-default-' + IntToStr(LIndex)));
    Check(Pos(#0, TNyxText(LMemo.Text)) = 0,
      'native NUL default remains editable through escaped widget text');
    LMemo.Text := '"Changed 🌙\u0000exact"';
    Check((LProbe.Error = '') and
      (Pos(#0, LSession.Document.State.Value('payload').TextValue) > 0) and
      (Pos(TNyxText('Changed 🌙'), LSession.Document.State.Value('payload').TextValue) = 1),
      'native escaped editor preserves NUL and supplementary Unicode');
    LState.CodeVisible := True;
    Rebuild;
    LMemo := TMemo(LRenderer.InputFor('studio-code'));
    Check(not LMemo.ReadOnly, 'native Pascal editor uses an editable public Nyx memo');
    { LCL uses platform line endings; portable source reader accepts either. }
    LMemo.Text := StringReplace(LMemo.Text, '''Shared state''',
      '''Native Pascal title''', []);
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error = '') and (LSession.Document.Title = 'Native Pascal title'),
      'native editor/button events share atomic code-to-design commands');
    LSession.Undo;
    Rebuild;
    Check(LSession.Document.Title = 'Shared state',
      'native source edit participates in paired history');
    LMemo := TMemo(LRenderer.InputFor('studio-code'));
    LMemo.Text := StringReplace(LMemo.Text, 'Result.Title :=', 'Result.Caption :=', []);
    LBefore := LSession.Save;
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error <> '') and (LSession.Save = LBefore) and
      (Pos('Result.Caption :=', LSession.DraftSource) > 0),
      'native rejection retains accepted design and editable source buffer');
    TNyxLCLButton(LRenderer.ControlFor('action-reset-source')).Click;
    Rebuild;
    Check(Pos('Result.Caption :=', TNyxText(TMemo(LRenderer.InputFor('studio-code')).Text)) = 0,
      'native Restore accepted restores the public editor content');
    LMemo := TMemo(LRenderer.InputFor('studio-code'));
    LMemo.Text := StringReplace(LMemo.Text, '.SetValue(LQuantityIntegerState, 2)',
      '.SetValue(LQuantityIntegerState, 4)', []);
    LMemo.Text := StringReplace(LMemo.Text, 'NyxTextState(''reply / 漢字'')',
      'NyxTextState(''discussion'')', []);
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error = '') and
      (LSession.Document.State.GetValue(NyxIntegerState('quantity')) = 4) and
      LSession.Document.State.Has('discussion') and
      (LSession.Document.Find('reply-memo').Bindings[0].StateName = 'discussion'),
      'native source edit applies typed defaults and named-reference migration');
    Rebuild;
    Check(TNyxText(TEdit(LRenderer.InputFor('state-default-5')).Text) = '4',
      'accepted source default reaches the actual native state editor');
    LPreview := TNyxLCLRenderer.Create;
    LPreviewHost := nil;
    try
      LPreviewHost := TForm.Create(nil);
      LPreviewHost.SetBounds(0, 0, 1200, 900);
      LPreview.Render(LSession.Document, LSession.Document.Pages[0], LPreviewHost);
      Check((TLabel(LPreview.ControlFor('quantity-caption')).Caption = '4') and
        (TNyxText(TMemo(LPreview.InputFor('reply-memo')).Text) =
        LSession.Document.State.GetValue(NyxTextState('discussion'))),
        'source-edited defaults/bindings drive real native page controls');
    finally
      LPreview.Free;
      LPreviewHost.Free;
    end;
    LSession.Undo;
    Rebuild;
    Check((LSession.Document.State.GetValue(NyxIntegerState('quantity')) = 2) and
      LSession.Document.State.Has('reply / 漢字'),
      'native paired undo restores default/reference contract');
    LMemo := TMemo(LRenderer.InputFor('studio-code'));
    LMemo.Text := StringReplace(LMemo.Text, '.Enabled(LEnabledBooleanState)',
      '.Enabled(NyxTextState(''reply / 漢字''))', []);
    LBefore := LSession.Save;
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((Pos('Boolean binding target', LProbe.Error) > 0) and
      (LSession.Save = LBefore), 'native typed binding failure preserves accepted design');
    TNyxLCLButton(LRenderer.ControlFor('action-reset-source')).Click;
    Rebuild;
    LBefore := LSession.Save;
    LMemo := TMemo(LRenderer.InputFor('studio-code'));
    LMemo.Text := StringReplace(LMemo.Text, '  except',
      '    LReplyMemo.Contract.Value(NyxTextDomain.Choices(['''', ''Native saved default / 🌙'']));' + #10 +
      '    LReplyMemo.Extensions.SetValue(NyxExtension(''code.validation''),' + #10 +
      '      NyxObject([NyxField(''limit'', NyxData(NyxDecimal(''9007199254740993'')))]));' + #10 +
      '  except', []);
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error = '') and
      (LSession.Document.Find('reply-memo').Contract.Snapshot.Field('value').Field('choices').Count = 2) and
      (LSession.Document.Find('reply-memo').Extensions.Value(NyxExtension('code.validation')).
      Field('limit').AsDecimal.Text = '9007199254740993'),
      'native code editor applies exact typed domain and structured extension');
    Rebuild;
    LPreview := TNyxLCLRenderer.Create;
    LPreviewHost := nil;
    try
      LPreviewHost := TForm.Create(nil);
      LPreview.Render(LSession.Document, LSession.Document.Pages[0], LPreviewHost);
      Check(TNyxText(TMemo(LPreview.InputFor('reply-memo')).Text) =
        TNyxText('Native saved default / 🌙'), 'source domain projects into an actual native memo');
    finally
      LPreview.Free;
      LPreviewHost.Free;
    end;
    LMemo := TMemo(LRenderer.InputFor('state-default-0'));
    LMemo.Text := 'Outside choices';
    Check((Pos('choices', LProbe.Error) > 0) and
      (LSession.Document.State.GetValue(NyxTextState('reply / 漢字')) =
      TNyxText('Native saved default / 🌙')),
      'actual native state field obeys source-declared choices');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'native undo restores paired domain/extension document');
    LSession.Redo;
    Check(LSession.Document.Find('reply-memo').Extensions.Has(NyxExtension('code.validation')),
      'native redo restores source-declared structured data');
    LState.InspectorTab := nitEvents;
    LSession.Select('reply-memo');
    Rebuild;
    Check(Pos('description or reply', TNyxText(
      TLabel(LRenderer.ControlFor('selected-component-help')).Caption)) > 0,
      'actual native Events inspector shows the selected component intent');
    TNyxLCLButton(LRenderer.ControlFor('event-after-enter-add')).Click;
    Check((LProbe.Error = '') and (LProbe.SourceLine > 1) and
      (Pos('// TODO: implement TReplyMemoAfterEnter.', LSession.Source) > 0),
      'actual native Add creates a managed Pascal handler and TODO navigation: ' + LProbe.Error);
    Rebuild;
    LHost.Show;
    LRenderer.NavigateCodeLine('studio-code', LProbe.SourceLine);
    Check(TMemo(LRenderer.InputFor('studio-code')).CaretPos.Y = LProbe.SourceLine - 1,
      'native code-editor navigation selects the returned TODO line');
    LHost.Hide;
    TNyxLCLButton(LRenderer.ControlFor('event-after-enter-add')).Click;
    Rebuild;
    Check(Length(NyxAuthoredEvents(LSession.Selected)[0].Callbacks) = 2,
      'actual native Add retains multiple ordered registrations: ' + LProbe.Error);
    TNyxLCLButton(LRenderer.ControlFor('event-after-enter-callback-0-remove')).Click;
    LState.CallbackRemoval := LProbe.Removal;
    Rebuild;
    Check((LShell.Find('event-removal-warning') <> nil) and
      (Length(NyxAuthoredEvents(LSession.Selected)[0].Callbacks) = 2),
      'native removal requires the shared warning first');
    TNyxLCLButton(LRenderer.ControlFor('event-removal-confirm')).Click;
    Check((LProbe.Error = '') and
      (Length(NyxAuthoredEvents(LSession.Selected)[0].Callbacks) = 1) and
      (Pos('procedure TReplyMemoAfterEnter.Invoke', LSession.Source) > 0),
      'actual native confirmation removes one registration and retains its code');
    Rebuild;
    Check((LShell.Find('event-key-down-add') <> nil) and
      (LShell.Find('event-key-up-add') <> nil),
      'native memo Events inspector exposes both keyboard callback families');
    TNyxLCLButton(LRenderer.ControlFor('event-key-down-add')).Click;
    Rebuild;
    Check((LProbe.Error = '') and
      (LShell.Find('event-key-down-count').Prop('text') = '1 registrations') and
      (Pos('// TODO: implement TReplyMemoKeyDown.', LSession.Source) > 0),
      'actual native keyboard Add creates its typed handler and visible registration');
    TNyxLCLButton(LRenderer.ControlFor('event-key-up-add')).Click;
    Check((LProbe.Error = '') and (Pos('.OnKeyUp', LSession.Source) > 0),
      'actual native key-release Add retains independent companion source');
    Rebuild;
    Check((LShell.Find('event-before-key-press-add') <> nil) and
      (LShell.Find('event-before-text-input-add') <> nil) and
      (LShell.Find('event-pointer-down-add') <> nil) and
      (LShell.Find('event-context-menu-add') <> nil),
      'native Inspector exposes modern key, text and pointer families');
    TNyxLCLButton(LRenderer.ControlFor('event-before-key-press-add')).Click;
    Check((LProbe.Error = '') and (Pos('.OnBeforeKeyPress', LSession.Source) > 0),
      'native new phase button authors its typed callback');
    LSession.Undo;
    Rebuild;
    Check(LShell.Find('event-before-key-press-count').Prop('text') = '0 registrations',
      'native new phase registration uses paired history');
    LPair := DecodeNyxProject(EncodeNyxProject(LSession.ProjectSnapshot));
    LSession.SetTitle('Temporary native change');
    LSession.LoadProject(LPair);
    LSession.Select('reply-memo');
    LState.FilesVisible := True;
    LState.ProjectName := 'native-project';
    Rebuild;
    Check((LSession.Document.Title <> 'Temporary native change') and
      (Pos('procedure TReplyMemoKeyDown.Invoke',
      TNyxText(TMemo(LRenderer.InputFor('studio-code')).Text)) > 0),
      'paired reopen reaches the actual native source editor with handwritten handlers');
    Check((TEdit(LRenderer.InputFor('project-file-name')).Text = 'native-project') and
      (LRenderer.ControlFor('action-project-save') is TNyxLCLButton),
      'shared project file controls render through actual LCL inputs/buttons');
    LSession.SetSourceDraft('Retained native draft / 🌙');
    LPair := DecodeNyxProject(EncodeNyxProject(LSession.ProjectSnapshot));
    LSession.LoadProject(LPair);
    Rebuild;
    Check(TNyxText(TMemo(LRenderer.InputFor('studio-code')).Text) =
      TNyxText('Retained native draft / 🌙'), 'paired recovery reaches actual native draft editor');
    Check(LSession.Source = LPair.Source,
      'native draft recovery leaves accepted companion intact');
    LBefore := LSession.Save;
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioPaletteGroupedID)).Click;
    LState.Palette := LProbe.Palette;
    Rebuild;
    Check((LProbe.Error = '') and (LShell.Find('palette-section-inputs') <> nil),
      'real native Grouped button uses the shared presentation router');
    LInput := TEdit(LRenderer.InputFor(NyxStudioPaletteSearchID));
    LInput.Text := 'description reply';
    LState.Palette := LProbe.Palette;
    Rebuild;
    Check((LRenderer.ControlFor('palette-memo') is TNyxLCLButton) and
      (LShell.Find('palette-button') = nil),
      'real native search finds descriptive intent rather than unrelated actions');
    Check(Pos('multiline', LRenderer.ControlFor('palette-memo').Hint) > 0,
      'real native palette exposes the shared creator description as its hint');
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioPaletteDetailsID)).Click;
    LState.Palette := LProbe.Palette;
    Rebuild;
    Check(LShell.Find('palette-description-memo') <> nil,
      'real native Details exposes the same help without requiring hover');
    LChoice := TComboBox(LRenderer.InputFor(NyxStudioPaletteGroupID));
    LChoice.ItemIndex := LChoice.Items.IndexOf(NyxPaletteGroupName(pgNavigation));
    LChoice.OnChange(LChoice);
    LState.Palette := LProbe.Palette;
    Rebuild;
    Check((LProbe.Error = '') and (LShell.Find('palette-empty') <> nil),
      'real native group choice intersects the active search');
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioPaletteResetID)).Click;
    LState.Palette := LProbe.Palette;
    Rebuild;
    Check((LShell.Find('palette-breadcrumbs') <> nil) and
      (LState.Palette.Mode = pmGrouped) and LState.Palette.Details,
      'real native Clear filters restores results and retains presentation');
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioPaletteListID)).Click;
    LState.Palette := LProbe.Palette;
    Rebuild;
    Check((LShell.Find('palette-items-list') <> nil) and
      (LShell.Find('palette-section-inputs') = nil),
      'real native List returns the ungrouped catalog');
    Check((LSession.Save = LBefore) and (LSession.Source = LPair.Source) and
      (LSession.DraftSource = TNyxText('Retained native draft / 🌙')),
      'native discovery preserves the paired project and exact pending draft');
    LSession.DiscardSourceDraft;
    LState.InspectorTab := nitProperties;
    Rebuild;
    LBefore := EditNyxManagedFixture(LSession.Source, 'LReplyMemo', 'LReplyDraftMemo');
    LBefore := EditNyxManagedFixture(LBefore, 'LReplyDraftMemo := NewNyxMemo',
      TNyxText('{ Native reply notes / 🌙 }') + #10 + '    LReplyDraftMemo := NewNyxMemo');
    TMemo(LRenderer.InputFor('studio-code')).Text := String(LBefore);
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error = '') and (Pos('LReplyDraftMemo: INyxMemo;', LSession.Source) > 0),
      'actual native Apply admits deliberate specialized memo naming');
    Rebuild;
    LBefore := LSession.Source;
    TEdit(LRenderer.InputFor('inspector-text')).Text := 'Reply notes';
    Check((LProbe.Error = '') and (LSession.Selected.Prop('text') = 'Reply notes') and
      (Pos('Native reply notes / 🌙', LSession.Source) > 0) and
      (Pos('LReplyDraftMemo: INyxMemo;', LSession.Source) > 0),
      'real native property events retain managed names/comments through the shared router');
    LSession.Undo;
    Rebuild;
    Check((LSession.Source = LBefore) and (TEdit(LRenderer.InputFor('inspector-text')).Text <> 'Reply notes'),
      'native paired undo restores source spelling and its actual field');
    LSession.Redo;
    Rebuild;
    Check(TEdit(LRenderer.InputFor('inspector-text')).Text = 'Reply notes',
      'native paired redo restores the actual visually edited property');
    LPair.Source := LSession.Source;
    LBefore := LPair.Source + #10 + TNyxText('{ 🌙漢字 } ''unfinished');
    TMemo(LRenderer.InputFor('studio-code')).Text := String(LBefore);
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error <> '') and LSession.SourceDiagnostic.Defined and
      (LSession.Source = LPair.Source), 'native failed Apply retains pair and an owned diagnostic');
    Rebuild;
    Check(Pos('Pascal ', TLabel(LRenderer.ControlFor('studio-source-diagnostic-message')).Caption) = 1,
      'native Nyx label exposes the source diagnostic');
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioDiagnosticGoID)).Click;
    LRenderer.NavigateCodeLine('studio-code', LProbe.SourceLine, LProbe.SourceColumn);
    Check((TMemo(LRenderer.InputFor('studio-code')).CaretPos.Y = LProbe.SourceLine - 1) and
      (TMemo(LRenderer.InputFor('studio-code')).CaretPos.X = 9),
      'native navigation resolves the Unicode scalar column to Win32 UTF-16 caret units');
    TNyxLCLButton(LRenderer.ControlFor('action-reset-source')).Click;
    Rebuild;
    Check(not LSession.SourceDiagnostic.Defined and
      (LShell.Find(NyxStudioDiagnosticID) = nil), 'native Restore removes the old error without losing source');
    LBefore := AddNyxStructuralFixture(LPair.Source);
    TMemo(LRenderer.InputFor('studio-code')).Text := String(LBefore);
    TNyxLCLButton(LRenderer.ControlFor('action-apply-source')).Click;
    Check((LProbe.Error = '') and (LSession.Source = LBefore) and
      (LSession.Document.Find('code-reply') <> nil),
      'real native Apply publishes source-created controls and exact companion');
    LPreview := TNyxLCLRenderer.Create;
    LPreviewHost := nil;
    try
      LPreviewHost := TForm.Create(nil);
      LPreview.Render(LSession.Document, LSession.Document.Find('code-notes'), LPreviewHost);
      Check(TNyxText(TMemo(LPreview.InputFor('code-reply')).Text) = 'From crafted source / 🌙',
        'source-created native memo consumes the new typed state default');
      Check((LPreview.ControlFor('code-send') <> nil) and
        (LPreview.ControlFor('code-instance/code-badge') <> nil),
        'source-created native action and reusable badge instantiate real controls');
    finally
      LPreview.Free;
      LPreviewHost.Free;
    end;
    LSession.Select('code-reply');
    Rebuild;
    TEdit(LRenderer.InputFor('inspector-text')).Text := 'Native crafted notes';
    Check((LProbe.Error = '') and (Pos('LReplyEditor: INyxMemo;', LSession.Source) > 0) and
      (Pos('A page written by hand / 🌙 漢字.', LSession.Source) > 0),
      'real native property editing preserves the source-created managed contract');
    LSession.Undo;
    LSession.Undo;
    Rebuild;
    Check((LSession.Source = LPair.Source) and (LSession.Document.Find('code-notes') = nil),
      'native paired undo removes the source-created page and exact companion');
    LSession.Redo;
    Rebuild;
    Check((LSession.Source = LBefore) and (LSession.Document.Find('code-notes') <> nil),
      'native paired redo restores the source-created page and companion');
    LCompilerDocument := CreateNyxCompilerFixture(LCompilerSource);
    LSession.Load(TNyxCodec.Encode(LCompilerDocument));
    LSession.SetSourceDraft(LCompilerSource);
    LSession.ApplySourceDraft;
    LCompilerError := ENyxSource.CreateAt('expected', LCompilerSource,
      Pos('MissingApplicationFunction;', LCompilerSource));
    try
      LProbe.CompilerReport := ReadNyxCompilerReport(LCompilerSource, LCompilerSource,
        'nyx.generated.view.pas', 'nyx.generated.view.pas(' +
        IntToStr(LCompilerError.Line) + ',18) Error: MissingApplicationFunction');
    finally
      LCompilerError.Free;
    end;
    Rebuild;
    Check((LRenderer.ControlFor(NyxStudioCompilerDiagnosticsID) <> nil) and
      TNyxLCLButton(LRenderer.ControlFor(NyxStudioCompilerActionPrefix + '0')).Enabled,
      'shared Nyx compiler diagnostics render a real native location action');
    TNyxLCLButton(LRenderer.ControlFor(NyxStudioCompilerActionPrefix + '0')).Click;
    LRenderer.NavigateCodeLine('studio-code', LProbe.SourceLine, LProbe.SourceColumn);
    LMemo := TMemo(LRenderer.InputFor('studio-code'));
    Check((LProbe.Error = '') and (LProbe.SourceColumn = 11) and
      (LMemo.CaretPos.X = 11) and (LMemo.CaretPos.Y = LProbe.SourceLine - 1),
      'native compiler action focuses the exact supplementary/CJK helper site');
    Check((LSession.Source = LCompilerSource) and (LSession.DraftSource = LCompilerSource),
      'native navigation leaves its accepted pair and editor buffer intact');
    LMemo.Text := String(LCompilerSource + #10 + '{ pending compiler draft }');
    Rebuild;
    Check(not TNyxLCLButton(LRenderer.ControlFor(NyxStudioCompilerActionPrefix + '0')).Enabled and
      (LRenderer.ControlFor('studio-compiler-stale') <> nil),
      'actual native source typing visibly disables stale compiler locations');
    LSession.DiscardSourceDraft;
    LSession.SetTitle('New accepted compiler source');
    Rebuild;
    Check(not TNyxLCLButton(LRenderer.ControlFor(NyxStudioCompilerActionPrefix + '0')).Enabled,
      'native diagnostics cannot navigate after the accepted source changes');
    LSession.Undo;
    Rebuild;
    Check(TNyxLCLButton(LRenderer.ControlFor(NyxStudioCompilerActionPrefix + '0')).Enabled and
      (LSession.Source = LCompilerSource), 'exact native paired undo restores trustworthy location actions');
  finally
    LRenderer.Free;
    LShell.Free;
    LProbe.Free;
    LHost.Free;
    LFixture.Free;
    LCompilerDocument.Free;
    LSession.Free;
  end;
end;

end.
