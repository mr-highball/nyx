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

program nyx_theme_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.json, nyx.colors, nyx.theme,
  nyx.design.tokens, nyx.theme.editor, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.schema, nyx.events, nyx.studio.session,
  nyx.studio.edits, nyx.studio.projects, nyx.studio.commands,
  nyx.studio.presentation, nyx.studio.sourcejobs, nyx.generated.view,
  nyx.studio.agents,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, Spin, Graphics, IntfGraphics,
    FPWritePNG, nyx.studio.lcl, nyx.studio.mcp;{$endif}

const
  CEditor = 'studio-theme-editor';

{$ifdef PAS2JS}
type
  TThemeInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
{$endif}

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Theme authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Shared;
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LTokens: TNyxThemeTokens;
  LCopy: TNyxThemeTokens;
  LForm: INyxCard;
  LFresh: INyxCard;
  LDraft: TNyxThemeEditorDraft;
  LChange: TNyxThemeEditorChange;
  LSession: TNyxStudioSession;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LDecoded: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LPresentation: TNyxStudioPresentation;
  LLoaded: TNyxStudioPresentation;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSource: TNyxText;
  LBadSource: TNyxText;
  LRefused: Boolean;
  LColor: TNyxThemeColor;
  LMetric: TNyxThemeMetric;
  LKind: Integer;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
  LAgentBefore: TNyxDataValue;
  LAgentAfter: TNyxDataValue;
  LObserved: TNyxText;
  {$ifndef PAS2JS}
  LTools: TNyxDataValue;
  LBranches: TNyxDataValue;
  LToolIndex: Integer;
  LBranchIndex: Integer;
  LFoundTheme: Boolean;
  {$endif}
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}

  function Field(ARole: TNyxThemeEditorRole; AOverride: Boolean = False): TNyxNode;
  begin
    Result := LForm.Node.Find(NyxThemeEditorFieldID(CEditor, ARole, AOverride));
  end;

begin
  LDocument := BuildNyxDocument;
  LRead := nil;
  LWorkspace := nil;
  LSession := nil;
  LAgent := nil;
  LSchemas := CaptureNyxSchemas;
  try
    Check(LDocument.Title = 'Theme workshop', 'English seed is the exact semantic export');
    Check(NyxDesignTokens(LDocument).Field('accent').AsText = '#35c3a5',
      'exported semantic tokens reach the independent document');
    LTokens := NyxThemeTokens.Accent(TNyxRGBColor.FromText('#AbCdEf')).FontSize(22).Radius(333);
    LCopy := LTokens;
    LTokens := LTokens.Accent(NyxRGB(10, 20, 30));
    Check(LCopy.ColorValue(ntcAccent).ToText = '#AbCdEf', 'fluent copies retain exact RGB spelling');
    Check(not LCopy.Has(ntcBackground), 'partial declarations inherit absent roles');
    for LColor := Low(TNyxThemeColor) to High(TNyxThemeColor) do
    begin
      LRefused := False;
      try
        LTokens.Color(LColor, NyxNoColor);
      except
        on EArgumentException do
        begin
          LRefused := True;
        end;
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'all semantic colors reject absent values');
    end;
    for LMetric := Low(TNyxThemeMetric) to High(TNyxThemeMetric) do
    begin
      LRefused := False;
      try
        LTokens.Metric(LMetric, -1);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'all metric roles reject negative pixels');
    end;
    for LKind := 0 to 4 do
    begin
      LRefused := False;
      try
        case LKind of
          0: TNyxThemeTokens.FromData(NyxObject([NyxField('unknown', NyxData(1))]));
          1: TNyxThemeTokens.FromData(NyxObject([NyxField('accent', NyxData('red'))]));
          2: TNyxThemeTokens.FromData(NyxObject([NyxField('fontSize', NyxData('16'))]));
          3: TNyxThemeTokens.FromData(NyxObject([NyxField('radius', NyxNull)]));
          4: TNyxThemeTokens.FromData(NyxObject([NyxField('fontSize', NyxData(257))]));
        end;
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'strict token wire boundary refuses unsupported fields/types/ranges');
    end;
    SetNyxThemeTokens(LDocument, LCopy);
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('.Accent(TNyxRGBColor.FromText(''#AbCdEf''))', LSource) > 0) and
      (Pos('.FontSize(22)', LSource) > 0) and
      (Pos('Extensions.SetValue(NyxExtension(''nyx.designTokens'')', LSource) = 0),
      'generated palette uses crafted typed calls and retains imported case');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore, 'typed source reconstructs exact partial theme and complete tree');
    LWorkspace.Free;
    LWorkspace := nil;
    LRead.Free;
    LRead := nil;
    LAgent := TNyxAgentSession.Create(NyxProjectPair(LBefore, LSource));
    LReply := LAgent.Call('nyx_transaction', 'Theme workshop', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('replace-theme-exact')),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('theme')), NyxField('values',
          NyxThemeTokens.Accent(NyxRGB(34, 68, 102)).ToData)])]))]));
    Check(LReply.Field('revision').AsInteger = LAgent.Revision,
      'semantic exact-theme operation publishes a revision');
    LReply := LAgent.Call('nyx_tokens', 'Theme workshop', NyxObject([]));
    Check((LReply.Field('tokens').Field('accent').AsText = '#224466') and
      (LReply.Field('tokens').Field('fontSize').AsInteger = 14),
      'exact semantic replacement retains inheritance rather than merging old metrics');
    LObserved := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
    LAgentBefore := LAgent.Call('nyx_session', 'Theme workshop', NyxObject([]));
    LRefused := False;
    try
      LAgent.Call('nyx_transaction', 'Theme workshop', NyxObject([
        NyxField('expectedRevision', NyxData(LAgent.Revision)),
        NyxField('operationId', NyxData('reject-untyped-theme')),
        NyxField('operations', NyxArray([NyxObject([
          NyxField('op', NyxData('theme')), NyxField('values',
            NyxObject([NyxField('fontSize', NyxData('20'))]))])]))]));
    except
      on ENyxJSON do
      begin
        LRefused := True;
      end;
    end;
    LAgentAfter := LAgent.Call('nyx_session', 'Theme workshop', NyxObject([]));
    Check(LRefused and
      (LAgentBefore.Field('revision').AsInteger = LAgentAfter.Field('revision').AsInteger) and
      (LAgentBefore.Field('canUndo').AsBoolean = LAgentAfter.Field('canUndo').AsBoolean) and
      (LAgentBefore.Field('canRedo').AsBoolean = LAgentAfter.Field('canRedo').AsBoolean) and
      (LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
        .Field('project').AsText = LObserved), 'semantic wrong-type refusal preserves complete pair/history');
    {$ifndef PAS2JS}
    LFoundTheme := False;
    LTools := NyxStudioMCPTools.Field('tools');
    for LToolIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LToolIndex).Field('name').AsText = 'nyx_transaction' then
      begin
        LBranches := LTools.Item(LToolIndex).Field('inputSchema').Field('properties')
          .Field('operations').Field('items').Field('oneOf');
        for LBranchIndex := 0 to LBranches.Count - 1 do
        begin

          if NyxAgentHas(LBranches.Item(LBranchIndex).Field('properties').Field('op'), 'const') and
            (LBranches.Item(LBranchIndex).Field('properties').Field('op')
              .Field('const').AsText = 'theme') then
          begin
            LFoundTheme := LBranches.Item(LBranchIndex).Field('properties').Field('values')
              .Field('oneOf').Item(1).Field('properties').Count = 10;
          end;
        end;
      end;
    end;
    Check(LFoundTheme, 'actual MCP discovery exposes ten closed roles and inherited reset');
    {$endif}
    FreeAndNil(LAgent);
    LForm := NewNyxThemeEditor(CEditor, LDocument);
    Check(Field(ntrRadius).Prop('max') = '1000', 'actual numeric form admits the full public radius');
    Check((Field(ntrAccent, True).Prop('value') = 'true') and
      (Field(ntrBackground, True).Prop('value') = 'false'), 'form distinguishes declaration from inheritance');
    Field(ntrFontSize).Configure.Value(19).Done;
    LDraft.Capture(CEditor, LForm.Node);
    LCopy := NyxDeclaredThemeTokens(LDocument);
    LPresentation := DefaultNyxStudioPresentation;
    LPresentation.ThemeVisible := True;
    LPresentation.ThemeDraft := LDraft;
    LLoaded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPresentation));
    LFresh := NewNyxThemeEditor(CEditor, LDocument);
    Check(LLoaded.ThemeVisible and LLoaded.ThemeDraft.Restore(LFresh.Node) and
      (LFresh.Node.Find(NyxThemeEditorFieldID(CEditor, ntrFontSize)).Prop('value') = '19'),
      'per-workspace preference restores unsubmitted theme independently');
    Check(not LDraft.Restore(nil) and LDraft.Defined, 'parked form retains its draft');
    Check(CaptureNyxThemeEditor(LForm.Node.Find(
      NyxThemeEditorActionID(CEditor, nteApply)), LForm.Node, LChange),
      'explicit Apply captures a typed proposal');
    Check((LChange.Tokens.ToData.Count = 3) and (LChange.Tokens.MetricValue(ntmFontSize) = 19),
      'applying one metric retains the exact inherited roles');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'capturing and prefilling never accepts a design');
    Check(PrepareNyxThemeEditorPreset(LForm.Node.Find(
      NyxThemeEditorActionID(CEditor, nteDark)), LForm.Node, LDraft) and
      LDraft.Restore(LForm.Node), 'dark preset creates a disposable proposal');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'choosing a preset creates no source/history change');
    LSession := TNyxStudioSession.Create;
    LSession.Load(LBefore);
    LSession.Select('home');
    LSession.Select('welcome');
    LForm := NewNyxThemeEditor(CEditor, LSession.Document);
    Field(ntrFontSize).Configure.Value(21).Done;
    Check(CaptureNyxStudioAuthoring(LSession, LForm.Node.Find(
      NyxThemeEditorActionID(CEditor, nteApply)), ntClick, LForm.Node,
      Default(TNyxStudioPendingDesign), LEdit) = sacEdit,
      'ordinary Studio capture routes a closed theme intent');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRequest := LSession.PrepareDesignRequest(LEdit, NyxSchemaRevision);
    LDecoded := ReadNyxStudioDesignRequest(LRequest.ToData);
    Check((LRequest.ToData.Field('version').AsInteger = 13) and
      LRequest.SameRequest(LDecoded), 'worker ticket round-trips exact typed theme intent');
    LPrepared := PrepareNyxStudioDesign(LDecoded, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'independent theme preparation admits typed source');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'ordinary paired processor publishes theme');
    LPrepared := nil;
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(NyxDeclaredThemeTokens(LSession.Document).MetricValue(ntmFontSize) = 21,
      'accepted document contains the explicit new metric');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore, 'one Undo restores exact design/Pascal pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact changed pair');
    LBadSource := StringReplace(LSession.Source, '.FontSize(21)', '.FontSize(''21'')', []);
    LSession.SetSourceDraft(LBadSource);
    LRefused := False;
    try
      LSession.ApplySourceDraft;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Source <> LBadSource), 'source admission rejects a string-valued metric');
    LSession.DiscardSourceDraft;
    LSession.SetSourceDraft(LSession.Source + #10 + '// pending local edits');
    LRefused := False;
    try
      LSession.ApplyPatch(NyxThemePatch(NyxThemePreset(ntpLight)));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'theme mutation respects pending Pascal draft');
    LSession.DiscardSourceDraft;
    LForm := NewNyxThemeEditor(CEditor, LSession.Document);
    LDraft.Capture(CEditor, LForm.Node);
    LSession.ApplyPatch(NyxThemePatch(NyxThemeTokens, True));
    Check(NyxThemeDeclaration(LSession.Document).Kind = ndNull, 'reset removes the local declaration');
    LFresh := NewNyxThemeEditor(CEditor, LSession.Document);
    Check(not LDraft.Restore(LFresh.Node) and not LDraft.Defined, 'changed baseline retires stale proposal');
    LRefused := False;
    try
      CaptureNyxStudioAuthoring(LSession, LForm.Node.Find(
        NyxThemeEditorActionID(CEditor, nteApply)), ntClick, LForm.Node,
        Default(TNyxStudioPendingDesign), LEdit);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'stale form cannot overwrite a changed theme');
    ResetNyxThemeTokens(LDocument);
    SetNyxThemeTokens(LDocument, NyxThemeTokens);
    LSource := StringReplace(TNyxCodegen.Generate(LDocument),
      'NyxThemeTokens);', 'NyxThemePreset(ntpDark));', []);
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check((NyxDeclaredThemeTokens(LRead).ToData.Count = 10) and
      (NyxDeclaredThemeTokens(LRead).ColorValue(ntcBackground).ToText = '#13151e'),
      'handwritten closed preset constructor is admitted through the same fluent grammar');
    LRead.Free;
    LRead := nil;
    LWorkspace.Free;
    LWorkspace := nil;
    SetNyxThemeTokens(LDocument, LCopy);
    {$ifndef PAS2JS}
    { Execute this exact emitted unit separately under both compilers. A reader
      admission alone does not prove that real Pascal construction matches. }

    if ParamCount > 0 then
    begin
      LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.theme');
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
  finally
    LPrepared := nil;
    LForm := nil;
    LFresh := nil;
    LSession.Free;
    LAgent.Free;
    LWorkspace.Free;
    LRead.Free;
    LDocument.Free;
    LSchemas := nil;
  end;
end;

{$ifndef PAS2JS}
type
  TControlAccess = class(TControl);
  TEditAccess = class(TCustomEdit)
  public
    procedure Complete;
  end;

procedure TEditAccess.Complete;
begin
  EditingDone;
end;

procedure NativeStudio;
var
  LStudio: TNyxNativeStudio;
  LWindow: TForm;
  LSeed: TNyxDocument;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LTheme: TNyxTheme;
  LColorBefore: TColor;

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
        raise Exception.Create('Theme Studio did not retire: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Click(const AID: TNyxText);
  var
    LButton: TControl;
  begin
    LButton := LStudio.ShellView.ControlFor(AID);
    Check(LButton <> nil, 'ordinary Nyx command is mounted');
    TControlAccess(LButton).Click;
    Ready;
  end;

  function Input(ARole: TNyxThemeEditorRole): TControl;
  begin
    Result := LStudio.ShellView.InputFor(NyxThemeEditorFieldID(CEditor, ARole));
    Check(Result <> nil, 'ordinary theme field is mounted');
  end;

  procedure Number(ARole: TNyxThemeEditorRole; AValue: Integer);
  var
    LSpin: TSpinEdit;
  begin
    LSpin := TSpinEdit(Input(ARole));
    LSpin.Value := AValue;
    LSpin.OnChange(LSpin);
  end;

  procedure Color(const AText: TNyxText);
  var
    LEdit: TCustomEdit;
  begin
    LEdit := TCustomEdit(Input(ntrAccent));
    LEdit.Text := AText;
    TEditAccess(LEdit).Complete;
    Ready;
  end;

  procedure Capture(const APath: String);
  var
    LBitmap: TBitmap;
    LImage: TLazIntfImage;
  begin
    Ready;
    LWindow.Repaint;
    Application.ProcessMessages;
    LBitmap := TBitmap.Create;
    LImage := nil;
    try
      LBitmap.SetSize(LWindow.ClientWidth, LWindow.ClientHeight);
      LWindow.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LImage.SaveToFile(APath);
    finally
      LImage.Free;
      LBitmap.Free;
    end;
  end;

begin
  LWindow := TForm.CreateNew(nil);
  LStudio := nil;
  LSeed := BuildNyxDocument;
  try
    LWindow.SetBounds(40, 40, 1240, 820);
    LWindow.Show;
    LStudio := TNyxNativeStudio.Create(LWindow, '');
    LPair.Design := TNyxCodec.Encode(LSeed);
    LPair.Source := TNyxCodegen.Generate(LSeed);
    LPair.Draft := '';
    LPair.DraftBase := '';
    LStudio.LoadProject(LPair);
    LStudio.Session.Select('home');
    LStudio.Session.Select('welcome');
    LStudio.Run;
    Ready;
    LColorBefore := TControlAccess(LStudio.ShellView.ControlFor('studio-shell')).Color;
    Click('action-theme-toggle');
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(TCustomEdit(Input(ntrAccent)).Text = '#35c3a5', 'actual RGB field shows semantic seed');
    Color('#123456');
    Number(ntrFontSize, 19);
    Number(ntrRadius, 333);
    Click('action-code');
    Check((TCustomEdit(Input(ntrAccent)).Text = '#123456') and
      (TSpinEdit(Input(ntrRadius)).Value = 333), 'repainting source retains unsubmitted physical form');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'physical proposals remain outside paired history');
    Click('action-theme-toggle');
    Check(LStudio.ShellView.Root.Find(CEditor) = nil, 'collapse parks the form');
    Click('action-theme-toggle');
    Check(TSpinEdit(Input(ntrFontSize)).Value = 19, 'reopening restores parked physical metric');
    Click(NyxThemeEditorActionID(CEditor, nteApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'actual Apply reaches isolated paired admission');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(NyxDeclaredThemeTokens(LStudio.Session.Document).ColorValue(ntcAccent).ToText = '#123456',
      'accepted palette matches physical color editor');
    LTheme := NewNyxDocumentTheme(LStudio.Session.Document);
    try
      Check((TControlAccess(LStudio.CanvasView.ControlFor('home')).Font.Height = -LTheme.FontSize) and
        (TControlAccess(LStudio.CanvasView.ControlFor('continue-button')).Color = RGBToColor(18, 52, 86)),
        'actual canvas consumes the admitted theme');
    finally
      LTheme.Free;
    end;
    Check(TControlAccess(LStudio.ShellView.ControlFor('studio-shell')).Color = LColorBefore,
      'application theme preserves independent Studio chrome palette');

    if ParamCount > 1 then
    begin
      Capture(ParamStr(2));
    end;
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'actual Undo restores complete paired theme');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'actual Redo restores complete changed pair');
    Click(NyxThemeEditorActionID(CEditor, nteLight));
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'preset click changes only a proposal');
    Check(TCustomEdit(Input(ntrBackground)).Text = '#f3f5fa', 'light proposal prefills ordinary RGB control');
    LWindow.ClientWidth := 390;
    Ready;
    Click('action-panel-project');
    Check(TCustomEdit(Input(ntrBackground)).Text = '#f3f5fa', 'compact panel retains theme proposal');

    if ParamCount > 2 then
    begin
      Capture(ParamStr(3));
    end;
    Click(NyxThemeEditorActionID(CEditor, nteReset));
    Check(NyxThemeDeclaration(LStudio.Session.Document).Kind = ndNull, 'actual Reset reveals inherited palette');
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'one Undo of reset restores exact local theme/source');
  finally
    LStudio.Free;
    LSeed.Free;
    LWindow.Free;
  end;
end;
{$else}
procedure BrowserControls;
var
  LSeed: TNyxDocument;
  LShell: TNyxDocument;
  LPage: INyxPage;
  LForm: INyxCard;
  LView: TNyxBrowserRenderer;
  LTheme: TNyxTheme;
  LOptions: TJSObject;
  LInput: TJSHTMLInputElement;
  LChange: TNyxThemeEditorChange;
begin
  LSeed := BuildNyxDocument;
  LShell := TNyxDocument.Create;
  LView := TNyxBrowserRenderer.Create;
  LTheme := TNyxTheme.Create;
  try
    LPage := NewNyxPage('theme-form');
    LShell.AddPage(LPage);
    LForm := NewNyxThemeEditor(CEditor, LSeed);
    LPage.Add(LForm);
    LView.Render(LShell, LShell.Pages[0], TJSHTMLElement(document.body));
    LInput := TJSHTMLInputElement(LView.InputFor(NyxThemeEditorFieldID(CEditor, ntrAccent)));
    Check(LInput.value = '#35c3a5', 'browser RGB editor shows admitted seed');
    LInput.value := '#123456';
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LInput.dispatchEvent(TThemeInputEvent.new('input', LOptions));
    LInput.dispatchEvent(TThemeInputEvent.new('change', LOptions));
    Check(CaptureNyxThemeEditor(LView.Root.Find(
      NyxThemeEditorActionID(CEditor, nteApply)), LView.Root, LChange) and
      (LChange.Tokens.ColorValue(ntcAccent).ToText = '#123456'),
      'browser physical proposal reaches typed reusable capture');
  finally
    LView.Free;
    LTheme.Free;
    LPage := nil;
    LForm := nil;
    LShell.Free;
    LSeed.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Shared;
    {$ifdef PAS2JS}BrowserControls;{$else}NativeStudio;{$endif}
    WriteLn('PASS / theme authoring / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
