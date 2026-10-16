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
program nyx_canvas_palette_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.catalog, nyx.catalog.labels, nyx.canvas.palette, nyx.canvas.types,
  nyx.canvas.declarations, nyx.canvas.workspace, nyx.canvas.editing,
  nyx.codegen, nyx.codec, nyx.state, nyx.schema, nyx.root.types,
  nyx.studio.inspector, nyx.studio.session, nyx.studio.projects,
  nyx.studio.edits, nyx.studio.transactions, nyx.studio.presentation
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas palette: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Send ordinary input to the portable form. This is a model-level consumer;
  actual target widget input remains a separate maintained Studio journey. }
procedure Input(ARoot: TNyxNode; const AField, AValue: TNyxText);
var
  LNode: TNyxNode;
begin
  LNode := ARoot.Find('inspector-canvas-content-' + AField);
  Check(LNode <> nil, 'requested public form field exists: ' + AField);
  LNode.Configure.Value(AValue).Done;
  Check(RouteNyxCanvasItemPalette(LNode, ARoot, ntChange),
    'exact mounted input routes without changing the project: ' + AField);
end;

function Seed: TNyxDocument;
var
  LPage: INyxPage;
  LTools: INyxToolCanvas;
  LCard: INyxCard;
  LLabel: INyxLabel;
begin
  Result := TNyxDocument.Create;
  Result.State.SetValue(NyxTextState('greeting'), 'Welcome');
  LPage := NewNyxPage('home');
  LTools := NewNyxToolCanvas('tools');
  LPage.Add(LTools);
  Result.AddPage(LPage);
  LCard := NewNyxCard('welcome-card');
  LLabel := NewNyxLabel('welcome-label');
  LLabel.Binds.Text(NyxTextState('greeting')).Done;
  LCard.Add(LLabel);
  Result.AddComponent(LCard);
  Result.Validate;
end;

function Inspector(ASession: TNyxStudioSession): INyxColumn;
begin
  Result := NewNyxColumn('qualification-inspector');
  AddNyxCanvasInspector(Result.Node, ASession);
end;

{$ifndef PAS2JS}
{ Export exact admitted Pascal/design only to the explicitly supplied owned
  directory. A separate native/browser compiler consumer executes constructors;
  writing these files alone never proves compiled-source equality. }
procedure ExportPair(const AStage: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure WriteBytes(const AName, AValue: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(AValue) > 0 then
      begin
        LStream.WriteBuffer(AValue[1], Length(AValue));
      end;
    finally
      LStream.Free;
    end;
  end;

begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AStage + PathDelim;
  ForceDirectories(LDirectory);
  WriteBytes('nyx.generated.view.pas', APair.Source);
  WriteBytes('design.nyx', APair.Design);
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LCatalog: TNyxCatalog;
  LPalette: INyxCanvasItemPalette;
  LRoot: INyxColumn;
  LNext: INyxColumn;
  LWorkspace: INyxCanvasWorkspace;
  LContent: INyxNode;
  LOwner: TNyxControlRef;
  LAddition: TNyxCanvasItemAddition;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LRead: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSecond: TNyxProjectPair;
  LDraft: TNyxCanvasPaletteDraft;
  LPresentation: TNyxStudioPresentation;
  LRestored: TNyxStudioPresentation;
  LBasePresentation: TNyxText;
  LPending: TNyxText;
  LScene: TNyxCanvasRef;
  LButton: TNyxNode;
  LFailed: Boolean;
  LInfo: TNyxComponentInfo;
begin
  LDocument := Seed;
  LCatalog := TNyxCatalog.Create;
  LSession := nil;
  try
    Check(TryNyxCanvasPaletteOwner(LDocument, NyxControl('tools'), LOwner),
      'owned Tool Canvas resolves its authored workspace without retargeting templates');
    Check(not TryNyxCanvasPaletteOwner(LDocument, NyxControl('welcome-label'), LOwner),
      'ordinary selected controls do not invent canvas authority');
    Check(TryNyxCanvasPaletteOwner(LDocument, NyxControl('tools'), LOwner),
      'typed owner can be recaptured after unrelated selection');
    LPalette := NewNyxCanvasItemPalette('typed-palette', LDocument, LCatalog, LOwner);
    Check((LPalette.SearchInput <> nil) and (LPalette.ComponentSelect <> nil) and
      (LPalette.GetPlacementInput(ncpfWidth) <> nil) and (LPalette.AddButton <> nil),
      'specialized managed facade exposes exact input/choice/action families');
    LCatalog.Describe(NyxCustomKind(NyxKindName(nkButton)), pgActions,
      [clConfirmation], 'launch rocket', 'Dispatch a selected workflow.');
    LInfo := LCatalog[LCatalog.IndexOf(NyxKindName(nkButton))];
    Check(NyxComponentMatches(LInfo, 'rocket workflow', pgActions),
      'creator aliases and descriptions supply AND search terms within the intent group');
    Check(not NyxComponentMatches(LInfo, 'rocket absent-token', pgActions) and
      not NyxComponentMatches(LInfo, 'rocket', pgInputs),
      'unmatched terms or a different intent group exclude a copied entry');
    FreeAndNil(LCatalog);
    LPalette.SearchInput.Configure.Value('multiline').Done;
    Check(RouteNyxCanvasItemPalette(LPalette.SearchInput.Node, LPalette.Node, ntChange) and
      (LPalette.ComponentSelect.Node.Prop(NyxAttributeName(atItems)) <> ''),
      'copied description/labels still filter after catalog retirement');
    LPalette.Add(NewNyxInput('typed-palette-extra-input').WithText('Creator extension'));
    Check(not RouteNyxCanvasItemPalette(LPalette.Node.Find('typed-palette-extra-input'),
      LPalette.Node, ntChange), 'creator-added fields retain their independent event owner');
    LDraft.Capture('typed-palette', LPalette.Node);
    LDraft := TNyxCanvasPaletteDraft.FromData(LDraft.ToData);
    Check(LDraft.Defined, 'default draft ignores creator-added fields instead of claiming their inputs');
    LDraft.Clear;
    LPalette := nil;

    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LSession := TNyxStudioSession.Create(LBefore);
    LSession.Select('tools');
    LSchemas := CaptureNyxSchemas;
    LFailed := False;
    try
      LAfter := NyxProjectTransaction([
        NyxDesignStep(NyxCreateRoot(NyxReusableRoot('refused-button'), nkButton)),
        NyxDesignStep(NyxCanvasPatch([NyxAttachCanvas(LOwner,
          NyxCanvasRef('Missing scene'))]))]).Candidate(LBefore);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
      not LSession.CanUndo, 'a later invalid scene operation leaves no created root or history prefix');
    LRoot := Inspector(LSession);
    LButton := LRoot.Node.Find('inspector-canvas-content-add');
    Check(LButton <> nil, 'ordinary Tool Canvas Inspector mounts the public content palette');
    Input(LRoot.Node, 'search', 'button');
    Input(LRoot.Node, 'item', 'Send action');
    Input(LRoot.Node, 'width', '320');
    Input(LRoot.Node, 'height', '72');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'search and incomplete authoring fields add no source/history command');

    LDraft.Capture('inspector-canvas-content', LRoot.Node);
    LPresentation := DefaultNyxStudioPresentation;
    LBasePresentation := EncodeNyxStudioPresentation(LPresentation);
    LPresentation.CanvasPaletteDraft := LDraft;
    LRestored := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPresentation));
    Check(LRestored.CanvasPaletteDraft.Defined and
      (TNyxDataValue.ParseJSON(EncodeNyxStudioPresentation(LRestored)).Field('version').AsInteger = 19),
      'optional project presentation retains the exact palette draft');
    LNext := Inspector(LSession);
    Check(LRestored.CanvasPaletteDraft.Restore(LNext.Node) and
      (LNext.Node.Find('inspector-canvas-content-item').Prop(NyxAttributeName(atValue)) = 'Send action'),
      'independent remounted form restores search/choice/name and placement');
    LRestored.CanvasPaletteDraft.Clear;
    LRestored.LogicVisible := False;
    LRestored.ProgramsVisible := False;
    Check(EncodeNyxStudioPresentation(LRestored) = LBasePresentation,
      'retiring the only new draft restores exact legacy presentation bytes');

    Check(CaptureNyxCanvasItemPalette(LButton, LRoot.Node, LSession.Document, LAddition) and
      LAddition.NewControl and LAddition.Attach and (LAddition.Kind.Name = NyxKindName(nkButton)),
      'first-scene proposal is typed control creation plus scene definition/attachment');
    Check(CaptureNyxCanvasInspector(LSession, LButton, LRoot.Node, LEdit),
      'ordinary Inspector delegates the complete insertion to existing source intent');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LRead := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    Check(LRequest.SameRequest(LRead), 'full guarded insertion survives the existing worker protocol');
    LPrepared := PrepareNyxStudioDesign(LRead, LSchemas);
    Check(not LPrepared.Diagnostic.Defined,
      'detached source preparation admits complete first-scene insertion: ' + LPrepared.Diagnostic.Message);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preparation never changes accepted files or history');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'one ordinary paired command publishes root, scene and attachment together');
    LAfter := LSession.ProjectSnapshot;
    Check((LSession.Document.ComponentCount = 2) and
      (LSession.Document.FindComponent(LAddition.Content.Name).Kind = NyxKindName(nkButton)),
      'new control has a distinct owned reusable root');
    LScene := LSession.Document.Find(LAddition.Owner.ID).CanvasScene;
    Check(LScene.Defined and (LSession.Document.Canvases.Definition(LScene).ItemCount = 1),
      'empty Tool Canvas becomes a populated saved scene');
    Check((Pos('INyxButton', LAfter.Source) > 0) and (Pos('NewNyxButton(', LAfter.Source) > 0),
      'generated component source retains specialized Pascal interfaces/factories');
    Check(Pos('LSendActionButton: INyxButton;', LAfter.Source) > 0,
      'default source names reflect the item purpose and control type');
    {$ifndef PAS2JS}ExportPair('new-control', LAfter);{$endif}
    LSession.Undo;
    Check((EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
      not LSession.CanUndo, 'one Undo removes the entire root/scene/attachment group');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores exact typed Pascal/design without another factory call');
    LNext := Inspector(LSession);
    Check(not LDraft.Restore(LNext.Node) and not LDraft.Defined,
      'changed scene membership retires the previous insertion proposal');
    LRoot := LNext;
    Input(LRoot.Node, 'search', 'button');
    Input(LRoot.Node, 'item', 'Send-action');
    Check(CaptureNyxCanvasItemPalette(LRoot.Node.Find('inspector-canvas-content-add'),
      LRoot.Node, LSession.Document, LAddition) and
      (LAddition.Content.Name = 'send-action-2-button'),
      'purpose collisions retain the control type last in the new root name');
    Input(LRoot.Node, 'item', TNyxText('Launch 🌙'));
    Check(CaptureNyxCanvasItemPalette(LRoot.Node.Find('inspector-canvas-content-add'),
      LRoot.Node, LSession.Document, LAddition) and
      (LAddition.Content.Name = TNyxText('launch-🌙-button')),
      'dedicated Unicode item names retain complete scalars through purpose naming');
    Input(LRoot.Node, 'search', 'Saved / welcome-card');
    Input(LRoot.Node, 'item', 'Welcome panel');
    Input(LRoot.Node, 'left', '360');
    Check(CaptureNyxCanvasInspector(LSession, LRoot.Node.Find('inspector-canvas-content-add'),
      LRoot.Node, LEdit), 'saved reusable content uses the same ordinary capture boundary');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined and
      (LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied),
      'saved reusable insertion publishes one complete paired candidate');
    LSecond := LSession.ProjectSnapshot;
    Check((LSession.Document.ComponentCount = 2) and
      (LSession.Document.Canvases.Definition(LScene).ItemCount = 2),
      'reuse inserts an independent item without duplicating the source definition');
    LWorkspace := NewNyxCanvasWorkspace('copied-workspace', LSession.Document, LScene);
    LContent := LWorkspace.Scene.Control(NyxCanvasItem('Welcome panel'));
    LContent.Node.Find('welcome-label').Configure.Text('Independent runtime caption').Done;
    Check(LSession.Document.Find('welcome-label').Prop(NyxAttributeName(atText)) <>
      'Independent runtime caption', 'materialized reusable content retains independent ownership');
    {$ifndef PAS2JS}ExportPair('reuse', LSecond);{$endif}
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'reuse Undo restores the exact preceding pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LSecond),
      'reuse Redo restores the exact complete pair');

    LRoot := Inspector(LSession);
    Input(LRoot.Node, 'width', '-');
    LPending := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      CaptureNyxCanvasInspector(LSession, LRoot.Node.Find('inspector-canvas-content-add'),
        LRoot.Node, LEdit);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPending),
      'incomplete numeric input refuses without touching pair/history');
    Input(LRoot.Node, 'width', '280');
    Input(LRoot.Node, 'item', 'Welcome panel');
    LFailed := False;
    try
      CaptureNyxCanvasInspector(LSession, LRoot.Node.Find('inspector-canvas-content-add'),
        LRoot.Node, LEdit);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPending),
      'duplicate item names refuse the whole insertion');
    Input(LRoot.Node, 'item', 'Pending control');
    Check(CaptureNyxCanvasInspector(LSession, LRoot.Node.Find('inspector-canvas-content-add'),
      LRoot.Node, LEdit), 'a complete proposal remains capturable before draft admission');
    LSession.SetSourceDraft(LSession.Source + #10 + '// Unfinished application Pascal');
    LPending := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPending),
      'pending Pascal refuses root/scene insertion with exact buffer/base retained');
  finally
    LWorkspace := nil;
    LContent := nil;
    LPalette := nil;
    LRoot := nil;
    LNext := nil;
    LPrepared := nil;
    LSession.Free;
    LCatalog.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-palette', 'passed');
    document.body.setAttribute('data-canvas-palette-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' portable canvas palette/source/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-palette', 'failed');
      document.body.setAttribute('data-canvas-palette-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
