{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_studio_chrome_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, Graphics, StdCtrls,
  nyx.text, nyx.types, nyx.colors, nyx.data, nyx.model, nyx.controls,
  nyx.button.presentation, nyx.button.view, nyx.button.editor, nyx.widgets.lcl,
  nyx.codec, nyx.codegen, nyx.design.tokens, nyx.studio.projects,
  nyx.studio.lcl, nyx.studio.view, nyx.studio.glyphs, nyx.studio.chrome,
  nyx.studio.session, nyx.studio.commands, nyx.schema, nyx.resources,
  nyx.disclosure, nyx.studio.presentation, nyx.behavior, nyx.events, nyx.scheduler;

type
  { Invoke the existing native widget entry point, never a Studio mutation.
    The ordinary controller captures and queues the actual button command. }
  TControlAccess = class(TControl);
  { Turn unexpected LCL callback exceptions into an observable test failure. }
  TFailureObserver = class
  public
    Error: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$ifdef NYX_CHROME_LATENCY}
  { Copied command intervals retain no control or Studio owner. Reveal,
    synchronous dispatch and deferred settling are consecutive elapsed times,
    not nested sums. The fixture emits them only after its complete journey. }
  TCommandLatency = record
    ID: TNyxText;
    RevealMilliseconds: QWord;
    DispatchMilliseconds: QWord;
    SettleMilliseconds: QWord;
  end;
  {$endif}

var
  GForm: TForm;
  GStudio: TNyxNativeStudio;
  GFailure: TFailureObserver;
  GChecks: Integer;
  {$ifdef NYX_CHROME_LATENCY}
  GLatencies: array[0..31] of TCommandLatency;
  GLatencyCount: Integer;
  {$endif}

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

procedure Check(ACondition: Boolean; const AMessage: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure AwaitChanges;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Application.ProcessMessages;

    if GFailure.Error <> '' then
    begin
      raise Exception.Create('Studio callback: ' + GFailure.Error);
    end;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Studio chrome did not settle');
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending then
    begin
      Sleep(1);
    end;
  until not GStudio.SourceBusy and not GStudio.PresentationPending;
end;

procedure Click(const AID: TNyxText);
var
  LFace: TControl;
  {$ifdef NYX_CHROME_LATENCY}
  LStarted: QWord;
  LRevealed: QWord;
  LDispatched: QWord;
  {$endif}
begin
  LFace := GStudio.ShellView.ControlFor(AID);
  Check(LFace <> nil, 'A command is mounted: ' + AID);
  {$ifdef NYX_CHROME_LATENCY}
  LStarted := GetTickCount64;
  {$endif}
  GStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);
  Check(LFace.IsVisible and LFace.Enabled, 'A command remains usable: ' + AID);
  {$ifdef NYX_CHROME_LATENCY}
  LRevealed := GetTickCount64;
  {$endif}
  TControlAccess(LFace).Click;
  {$ifdef NYX_CHROME_LATENCY}
  LDispatched := GetTickCount64;
  {$endif}
  AwaitChanges;
  {$ifdef NYX_CHROME_LATENCY}

  if GLatencyCount >= Length(GLatencies) then
  begin
    raise Exception.Create('The bounded native command timing vector is full');
  end;
  GLatencies[GLatencyCount].ID := AID;
  GLatencies[GLatencyCount].RevealMilliseconds := LRevealed - LStarted;
  GLatencies[GLatencyCount].DispatchMilliseconds := LDispatched - LRevealed;
  GLatencies[GLatencyCount].SettleMilliseconds := GetTickCount64 - LDispatched;
  Inc(GLatencyCount);
  {$endif}
end;

{$ifdef NYX_CHROME_LATENCY}
{ Output cannot block a measured command: all command intervals are closed
  before this fixture-only report. No production profiler is enabled. }
procedure PrintLatencies;
var
  LIndex: Integer;
begin
  for LIndex := 0 to GLatencyCount - 1 do
  begin
    WriteLn('LATENCY,', GLatencies[LIndex].ID, ',',
      GLatencies[LIndex].RevealMilliseconds, ',',
      GLatencies[LIndex].DispatchMilliseconds, ',',
      GLatencies[LIndex].SettleMilliseconds);
  end;
end;
{$endif}

procedure Capture(const APath: String);
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
begin
  LBitmap := TBitmap.Create;
  LPNG := TPortableNetworkGraphic.Create;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LPNG.Assign(LBitmap);
    LPNG.SaveToFile(APath);
  finally
    LPNG.Free;
    LBitmap.Free;
  end;
end;

{ Exercise the real queued processor separately from timing-sensitive UI input:
  an unchanged form admits, while a queued or parked stale proposal refuses. }
procedure GuardedStyleCases;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LPage: INyxPage;
  LEditor, LReplacement: INyxColumn;
  LProjection: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LDraft: TNyxButtonEditorDraft;
  LOwner, LBaseline, LBefore: TNyxText;
  LValue: TNyxButtonPresentation;
  LButton: TNyxLCLButton;
  LCaptionWidth, LCaptionHeight, LFallbackWidth, LFallbackHeight: Integer;
  LRefused: Boolean;
  LPreferences, LRestoredPreferences: TNyxStudioPresentation;
begin
  LDocument := TNyxDocument.Create;
  LSession := nil;
  LProjection := nil;
  LButton := nil;
  try
    LPreferences := DefaultNyxStudioPresentation;
    LPreferences.AssetPickerExpanded := True;
    LRestoredPreferences := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPreferences));
    Check(LRestoredPreferences.AssetPickerExpanded,
      'Optional asset-picker disclosure survives copied project presentation');
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxButton('style-target').WithText('Launch'));
    LDocument.AddPage(LPage);
    LSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LSession.Select('style-target');
    LProjection := LSession.SelectedProjection;
    LEditor := NewNyxButtonEditor('guarded-button', LSession.Selected, LProjection);
    LEditor.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befDensity)).Configure.Value('Touch').Done;
    LEditor.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befSize)).Configure.Value('28x').Done;
    LDraft.Capture(LEditor.Node.ID, LEditor.Node);
    LDraft := TNyxButtonEditorDraft.FromData(LDraft.ToData);
    LReplacement := NewNyxButtonEditor('guarded-button', LSession.Selected, LProjection);
    Check(LDraft.Restore(LReplacement.Node.ID, LReplacement.Node),
      'An incomplete numeric draft restores into an editable authoring field');
    ValidateNyxPropertyTree(LReplacement.Node);
    Check(LReplacement.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befSize)).Prop('value') = '28x',
      'Numeric recovery retains the exact unsubmitted text through schema admission');
    LRefused := False;
    try
      CaptureNyxButtonEditor(LReplacement.Node.Find('guarded-button-apply'), LReplacement.Node,
        LOwner, LBaseline, LValue);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.ProjectSnapshot.Design = TNyxCodec.Encode(LDocument)),
      'Malformed draft numbers refuse typed Apply without changing the project');
    LEditor.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befSize)).Configure.Value('28').Done;
    LDraft.Capture(LEditor.Node.ID, LEditor.Node);
    Check(CaptureNyxStudioAuthoring(LSession, LEditor.Node.Find('guarded-button-apply'),
      ntClick, LEditor.Node, Default(TNyxStudioPendingDesign), LEdit) = sacEdit,
      'A complete mounted style form captures ordinary authoring intent');
    Check((LEdit.Action = sdaTransaction) and
      (LEdit.TransactionDesignBaseline = TNyxCodec.Encode(LSession.Document)),
      'Style intent retains the whole design through the source queue');
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'An unchanged style proposal admits in the actual processor');
    LPrepared := nil;
    LSession.SetTitle('A newer accepted design');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'An earlier queued edit refuses stale style without publishing or changing history');
    LProjection.Configure.Text('A different command').Done;
    LReplacement := NewNyxButtonEditor('guarded-button', LSession.Selected, LProjection);
    Check(not LDraft.Restore(LReplacement.Node.ID, LReplacement.Node),
      'A caption change prevents a parked draft from silently restoring');
    LProjection.Configure.Text('Launch').Done;
    LSession.Document.Resources.Define(NyxResourceRef('A new resource'), NyxTextResource('Context changed'));
    FreeAndNil(LProjection);
    LProjection := LSession.SelectedProjection;
    LReplacement := NewNyxButtonEditor('guarded-button', LSession.Selected, LProjection);
    Check(not LDraft.Restore(LReplacement.Node.ID, LReplacement.Node),
      'A resource catalog change prevents stale parked choices');
    LEditor.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befWeight)).Configure.ProjectAs(nkLabel).Done;
    Check(not CaptureNyxButtonEditor(LEditor.Node.Find('guarded-button-apply'), LEditor.Node,
      LOwner, LBaseline, LValue), 'A projected label cannot impersonate an editable style choice');
    LEditor.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befWeight)).Configure.ProjectAs(nkSelect).Done;
    LEditor.Node.Remove(LEditor.Node.Find(NyxButtonEditorFieldID(LEditor.Node.ID, befDensity)));
    LEditor.Add(NewNyxLabel(NyxButtonEditorFieldID(LEditor.Node.ID, befDensity)));
    Check(not CaptureNyxButtonEditor(LEditor.Node.Find('guarded-button-apply'), LEditor.Node,
      LOwner, LBaseline, LValue), 'A same-ID label cannot impersonate the typed style field');
    LButton := TNyxLCLButton.Create(nil);
    LButton.Parent := GForm;
    LButton.Caption := 'A command with a descriptive name';
    LButton.ApplyPresentation(NyxButtonPresentation.FontWeight(nbwRegular), nil);
    Check(not (fsBold in LButton.Font.Style), 'Regular weight takes effect immediately');
    LButton.ApplyPresentation(NyxButtonPresentation.FontWeight(nbwInherit), nil);
    Check(fsBold in LButton.Font.Style, 'Inherit restores the ordinary button weight immediately');
    LButton.GetPreferredSize(LCaptionWidth, LCaptionHeight, True, False);
    LButton.AccessibleName := LButton.Caption;
    LButton.Caption := '';
    LButton.ApplyPresentation(NyxButtonPresentation, nil);
    LButton.GetPreferredSize(LFallbackWidth, LFallbackHeight, True, False);
    Check((LFallbackWidth = LCaptionWidth) and (LFallbackHeight = LCaptionHeight),
      'Image fallback allocates the full accessible command caption');
  finally
    LButton.Free;
    LPrepared := nil;
    LSchemas := nil;
    LEditor := nil;
    LReplacement := nil;
    LProjection.Free;
    LSession.Free;
    LPage := nil;
    LDocument.Free;
  end;
end;

procedure Journey;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LSourceBefore: TNyxText;
  LAtlasBefore: TNyxText;
  LField: TControl;
  LProjection: TNyxNode;
  LButton: TNyxNode;
  LFace: TNyxButtonPresentation;
  LAssets: TNyxNode;
  LAssetHeader: TNyxText;
  LStyleEvents: INyxEvents;
  LStyleScope: INyxCancellationScope;
  LStyleInput: TControl;

  { Keep the old router alive so address reuse cannot fake retention. The
    borrowed input is compared only with a fresh current-owner lookup; its old
    event generation must retire even though the physical field survives. }
  procedure CaptureStyleContinuity;
  begin
    LStyleEvents := GStudio.ShellView.ViewFor('inspector-button').Events;
    LStyleScope := NyxEventGeneration(LStyleEvents);
    LStyleInput := GStudio.ShellView.InputFor(
      NyxButtonEditorFieldID('inspector-button', befDensity));
    Check((LStyleEvents <> nil) and (LStyleScope <> nil) and (LStyleInput <> nil),
      'Capture the actual button-style router, generation and physical input');
  end;

  procedure CheckStyleContinuity;
  begin
    Check((GStudio.ShellView.ViewFor('inspector-button').Events = LStyleEvents) and
      (GStudio.ShellView.InputFor(NyxButtonEditorFieldID('inspector-button', befDensity)) =
        LStyleInput), 'Button style admission retains the SAME Inspector router and input');
    Check(LStyleScope.Cancelled,
      'Retained button-style fields retire their old action generation');
  end;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'A little workspace';
    SetNyxThemeTokens(LDocument, NyxThemeTokens.Accent(TNyxRGBColor.FromText('#0f766e')));
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxHeading('welcome').WithText('Make room for your next idea.'));
    LPage.Add(NewNyxLabel('description').WithText('A calm editor. A canvas full of possibilities.'));
    LPage.Add(NewNyxButton('project-button').Configure.Text('Start something')
      .Variant(nvPrimary).Done);
    LPage.Add(NewNyxCheckbox('project-choice').Configure.Text('Keep me posted').Value(True).Done);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  GStudio.LoadProject(LPair);
  GStudio.Run;
  AwaitChanges;
  Check(GStudio.ProjectSnapshot.Design = LPair.Design, 'Chrome leaves the authored project exact');
  Check(GStudio.ShellView.RootFor('action-actions').Find('studio-workspace-commands') <> nil,
    'Commands are grouped by purpose in the shared Nyx tree');
  Check(GStudio.ShellView.ControlFor('action-undo') is TNyxLCLButton,
    'Glyph commands use the ordinary accessible native button');
  Check(TNyxLCLButton(GStudio.ShellView.ControlFor('action-undo')).GlyphAvailable,
    'The generated resource atlas actually decoded into the native command face');
  LProjection := GStudio.ShellView.RootFor('action-undo').Find('action-undo');
  Check(NyxButtonPresentationOf(LProjection).ContentMode = nbcGlyphOnly,
    'Undo uses a typed glyph-only face');
  Check(LProjection.Prop('text') = 'Undo', 'Undo retains its complete command caption');
  LAtlasBefore := NyxStudioGlyphAtlas.ToWire;
  Check(GStudio.Session.Document.Resources.Count = 0, 'Editor imagery does not leak into the project');
  Check(GStudio.CanvasView.ControlFor('project-button') <> nil, 'The ordinary project canvas remains mounted');
  LField := GStudio.CanvasView.ControlFor('project-choice');
  Check((LField is TCheckBox) and (TControlAccess(LField).Font.Height = -14) and
    (TControlAccess(LField).Font.Name = GForm.Font.Name),
    'An untouched native checkbox keeps the accepted body size and platform family');
  Capture(ParamStr(3) + '-design.png');

  if GStudio.ShellView.RootFor('action-panel-project') <> nil then
  begin
    Click('action-panel-project');
  end;
  LAssets := GStudio.ShellView.RootFor('studio-assets-picker').Find('studio-assets-picker');
  Check(not NyxDisclosureExpanded(LAssets), 'The quick resource picker starts collapsed');
  LField := GStudio.ShellView.InputFor('palette-search');
  Check((LField <> nil) and LField.IsVisible and
    (LField.ClientOrigin.Y - GForm.ClientOrigin.Y + LField.Height <= GForm.ClientHeight),
    'Component search is reachable in the initial project viewport');
  LField := GStudio.ShellView.InputFor('project-title');
  Check(TControlAccess(LField).Font.Name = GForm.Font.Name,
    'Ordinary editor fields inherit the accepted platform UI font family');
  LAssetHeader := LAssets.Part(NyxPart('header')).ID;
  Click(LAssetHeader);
  Check(NyxDisclosureExpanded(GStudio.ShellView.RootFor('studio-assets-picker').Find('studio-assets-picker')),
    'The ordinary disclosure opens the retained quick asset browser');
  GStudio.RequestRefresh(False);
  AwaitChanges;
  Check(NyxDisclosureExpanded(GStudio.ShellView.RootFor('studio-assets-picker').Find('studio-assets-picker')),
    'Asset expansion survives an ordinary shell refresh');
  Click(LAssetHeader);
  Check(not NyxDisclosureExpanded(GStudio.ShellView.RootFor('studio-assets-picker').Find('studio-assets-picker')),
    'The ordinary disclosure closes the quick asset browser');
  Capture(ParamStr(3) + '-project.png');
  GStudio.Session.Select('project-button');
  GStudio.RequestRefresh(False);
  AwaitChanges;

  if GStudio.ShellView.RootFor('action-panel-inspector') <> nil then
  begin
    Click('action-panel-inspector');
  end;
  Check(GStudio.ShellView.RootFor('inspector-button') <> nil, 'Button style has an ordinary typed Inspector form');
  LBefore := GStudio.ProjectSnapshot.Design;
  LSourceBefore := GStudio.ProjectSnapshot.Source;
  LField := GStudio.ShellView.InputFor(NyxButtonEditorFieldID('inspector-button', befDensity));
  Check(LField is TComboBox, 'Density uses a closed choice control');
  TComboBox(LField).ItemIndex := TComboBox(LField).Items.IndexOf('Touch');
  TComboBox(LField).OnChange(LField);
  CaptureStyleContinuity;
  Click('inspector-button-apply');
  CheckStyleContinuity;
  LButton := GStudio.Session.Document.Find('project-button');
  LFace := NyxButtonPresentationOf(LButton);
  Check(LFace.ControlDensity = nbdTouch, 'Apply changes the selected button through the paired source queue');
  Check(Pos('.Density(nbdTouch)', GStudio.ProjectSnapshot.Source) > 0,
    'Generated presentation remains typed Pascal');
  Check(GStudio.ProjectSnapshot.Design <> LBefore, 'One accepted style command changes design');
  Check(NyxStudioGlyphAtlas.ToWire = LAtlasBefore, 'Style edits preserve shared original image bytes');
  CaptureStyleContinuity;
  Click('action-undo');
  CheckStyleContinuity;
  Check((GStudio.ProjectSnapshot.Design = LBefore) and
    (GStudio.ProjectSnapshot.Source = LSourceBefore), 'One Undo restores the exact design/source pair');
  CaptureStyleContinuity;
  Click('action-redo');
  CheckStyleContinuity;
  Check(NyxButtonPresentationOf(GStudio.Session.Document.Find('project-button')).ControlDensity = nbdTouch,
    'Redo restores the same typed style');
  Capture(ParamStr(3) + '-inspector.png');
end;

begin
  try

    if ParamCount <> 3 then
    begin
      raise Exception.Create('Supply a fresh project directory, client width and capture prefix');
    end;
    Application.Initialize;
    GFailure := TFailureObserver.Create;
    Application.OnException := GFailure.Failed;
    GForm := TForm.CreateNew(nil);
    GForm.ClientWidth := StrToInt(ParamStr(2));
    GForm.ClientHeight := 900;
    GForm.Show;
    GuardedStyleCases;
    GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
    Journey;
    {$ifdef NYX_CHROME_LATENCY}
    PrintLatencies;
    {$endif}
    WriteLn('PASS ', GChecks, ' native Studio chrome checks at ', GForm.ClientWidth);
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message, ' / checks=', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  FreeAndNil(GStudio);
  FreeAndNil(GForm);
  while CheckSynchronize(0) do
  begin
    Application.ProcessMessages;
  end;
  Application.OnException := nil;
  FreeAndNil(GFailure);
end.
