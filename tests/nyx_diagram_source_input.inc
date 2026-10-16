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

{ Invoked after the original content journey, using its real retained controls.
  The host owns an independent accepted document, with no project authority. }
type
  { Fixture-owned source host owns its document copy. Inspection returns inert
    snapshots; the global fixture supplies a borrowed view only for deliberate
    reentrant-input qualification. It retains no renderer/editor cycle. }
  TSourceHost = class(TInterfacedObject, INyxDiagramSaveHost,
    INyxDiagramSaveInspection, INyxDiagramSourceInspection)
  public
    Document: TNyxDocument;
    RefuseCurrent: Boolean;
    EditDuringInspection: Boolean;
    destructor Destroy; override;
    procedure Submit(const ARequest: INyxDiagramSaveRequest);
    function Current(const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
    function CurrentSource(const AReference: TNyxDiagramRef): TNyxDiagramSourceSnapshot;
    function IsCurrent(const ASnapshot: TNyxDiagramSourceSnapshot): Boolean;
  end;

  { Cancels during prepared validation, after target staging. It owns only the
    scope; no view/document is retained and Install must remain uncalled. }
  TCancelSource = class(TInterfacedObject, INyxPreparedPublication, INyxPublicationReadiness)
  public
    Scope: INyxCancellationScope;
    Installed: Boolean;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

procedure TCancelSource.Validate;
begin
  Scope.Cancel;
end;

procedure TCancelSource.CheckReady;
begin
  { Cancellation is read by the owning canvas's final validator. }
end;

procedure TCancelSource.Install;
begin
  Installed := True;
end;

procedure TCancelSource.Notify;
begin
end;

procedure TCancelSource.Retire;
begin
end;

destructor TSourceHost.Destroy;
begin
  Document.Free;
  inherited Destroy;
end;

procedure TSourceHost.Submit(const ARequest: INyxDiagramSaveRequest);
begin
  raise ENyxDiagram.Create('Source fixture never publishes project commands');
end;

function TSourceHost.Current(const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
begin
  Result := NyxDiagramSaveTarget(Document, AReference);
end;

function TSourceHost.CurrentSource(const AReference: TNyxDiagramRef): TNyxDiagramSourceSnapshot;
begin
  Result := NyxDiagramSourceSnapshot(Document, AReference);

  if EditDuringInspection then
  begin
    WriteField(ndepContentPadding, '-held during source inspection');
  end;
end;

function TSourceHost.IsCurrent(const ASnapshot: TNyxDiagramSourceSnapshot): Boolean;
begin
  Result := not RefuseCurrent and ASnapshot.Matches(Document);
end;

procedure SourceRefresh; {$ifdef PAS2JS}async;{$endif}
var
  LSource: INyxDiagramSourceView;
  LHost: TSourceHost;
  LHostLease: INyxDiagramSaveHost;
  LDocument: TNyxDocument;
  LGraph: INyxDiagram;
  LRevision: Integer;
  LBefore: TNyxText;
  LState: TNyxText;
  LText: TNyxText;
  LCaret: Integer;
  LForm: TNyxCanvasItemRef;
  LSummary: TNyxCanvasItemRef;
  LField: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LFormerSummary: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LNewSummary: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
  LFailed: Boolean;
  LSnapshot: TNyxDiagramSourceSnapshot;
  LCopy: TNyxDocument;
  LCallback: TReply;
  LSubscription: INyxEventSubscription;
  LReplies: Integer;
  LSourceLayout: INyxCanvasSourceLayoutView;
  LRuntime: INyxCanvasRuntimeView;
  LLayouts: INyxCanvasSceneLayouts;
  LPlan: INyxCanvasLayoutPlan;
  LScope: INyxCancellationScope;
  LCancel: TCancelSource;
  LCancelLease: INyxPreparedPublication;
  LSceneRevision: Integer;
  LPresentation: INyxCanvasLayoutView;
  LPresentationRevision: Integer;
  LFreshMemo: INyxMemo;

  function SummaryCaption: TNyxText;
  var
    LNode: TNyxNode;
  begin
    LNode := FindKind(GCanvas.RendererFor(LSummary).Root, nkLabel);
    {$ifdef PAS2JS}
    Result := GCanvas.RendererFor(LSummary).ElementFor(LNode.ID).textContent;
    {$else}
    Result := TLabel(GCanvas.RendererFor(LSummary).ControlFor(LNode.ID)).Caption;
    {$endif}
  end;

  function CurrentPlacement(const AItem: TNyxCanvasItemRef): TNyxCanvasPlacement;
  var
    LSnapshot: INyxCanvasSnapshot;
    LIndex: Integer;
  begin
    LSnapshot := LRuntime.Scene.Snapshot;
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin

      if LSnapshot.Items[LIndex].Item.Name = AItem.Name then
      begin
        Exit(LSnapshot.Items[LIndex].Placement);
      end;
    end;
    raise ENyxDiagram.Create('Fixture item placement is absent');
  end;

  function CatalogHas(const AName: TNyxText): Boolean;
  begin
    {$ifdef PAS2JS}
    Result := Pos(AName, GTarget.Renderer.InputFor(PartID(ndepContentReference)).textContent) > 0;
    {$else}
    Result := TComboBox(GTarget.Renderer.InputFor(PartID(ndepContentReference))).Items.IndexOf(AName) >= 0;
    {$endif}
  end;
begin
  LDocument := nil;
  LHost := nil;
  Check(Supports(GView, INyxDiagramSourceView, LSource), 'open diagram exposes prepared source replacement');
  LGraph := GView.Diagram.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Summary'), ndsRectangle,
    NyxCanvasBounds(-100, 120, 240, 120)).Captioned('Source changes')
    .WithContent(NyxDiagramComponent(NyxComponent('Summary'))));
  GView.ReplaceDiagram(LGraph, GView.Revision);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  GView.Canvas.Execute(nccFit);
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  LForm := GContent.ContentItem(NyxDiagramVertexRef('Form'));
  LSummary := GContent.ContentItem(NyxDiagramVertexRef('Summary'));
  LField := GCanvas.RendererFor(LForm).InputFor(MemoID);
  LFormerSummary := {$ifdef PAS2JS}GCanvas.RendererFor(LSummary).ElementFor(
    GCanvas.RendererFor(LSummary).Root.ID){$else}GCanvas.RendererFor(LSummary).ControlFor(
    GCanvas.RendererFor(LSummary).Root.ID){$endif};
  TypeMemo;
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  LText := ReadMemo;
  LCaret := MemoCaret;
  LState := GState.GetValue(NyxTextState('reply'));
  LCallback := TReply.Create;
  LCallback.Store := GState;
  LCallback.Scheduler := NewNyxScheduler;
  LSubscription := GCanvas.EventsFor(LForm).On(NyxControlEvents(ButtonID, niRuntime), ntClick)
    .Subscribe(LCallback);
  try
    LHost := TSourceHost.Create;
    LHostLease := LHost;
    LHost.Document := GDocument.Clone;
    LHost.Document.Diagrams.Define(NyxDiagramRef('Content board'), LGraph);
    GView.SavingTo(NyxDiagramSaveTarget(LHost.Document, NyxDiagramRef('Content board')), LHostLease);
    LSnapshot := LHost.CurrentSource(NyxDiagramRef('Content board'));
    LHost.Document.FindComponent('Summary').Find('summary-heading')
      .Configure.Text('Updated from the accepted source').Done;
    LHost.Document.AddComponent(NewNyxColumn('New reusable')
      .Add(NewNyxLabel('new-reusable-heading').WithText('A new choice')));
    LCopy := LSnapshot.OpenDocument;
    try
      Check(LCopy.FindComponent('Summary').Find('summary-heading')
        .Prop(NyxAttributeName(atText)) = 'A different reusable',
        'captured source owns exact independent bytes after its host changes');
    finally
      LCopy.Free;
    end;
    LRevision := GView.Revision;
    GView.ReloadSaved;
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    Check((GView.Revision = LRevision + 1) and not GView.HasUnsavedChanges,
      'ordinary saved reload publishes source and graph as one local generation');
    Check(SummaryCaption = 'Updated from the accepted source',
      'same reusable selector renders its changed accepted definition');
    LNewSummary := {$ifdef PAS2JS}GCanvas.RendererFor(LSummary).ElementFor(
      GCanvas.RendererFor(LSummary).Root.ID){$else}GCanvas.RendererFor(LSummary).ControlFor(
      GCanvas.RendererFor(LSummary).Root.ID){$endif};
    Check(LNewSummary <> LFormerSummary, 'changed reusable receives a deliberately staged replacement');
    Check((GCanvas.RendererFor(LForm).InputFor(MemoID) = LField) and
      (ReadMemo = LText) and (MemoCaret = LCaret) and (GState.GetValue(NyxTextState('reply')) = LState),
      'unrelated reusable retains actual memo, typed value, text and caret');
    Check(LSubscription.Active, 'unchanged content retains its actual callback subscription');
    LReplies := GState.GetValue(NyxIntegerState('replies'));
    {$ifdef PAS2JS}
    GCanvas.RendererFor(LForm).ElementFor(ButtonID).click;
    await(Idle);
    {$else}
    TControlAccess(GCanvas.RendererFor(LForm).ControlFor(ButtonID)).Click;
    Idle;
    {$endif}
    Check(GState.GetValue(NyxIntegerState('replies')) = LReplies + 1,
      'retained ordinary button still executes its existing callback after source publication');
    GView.SelectVertex(NyxDiagramVertexRef('Form'));
    GContent.ShowContentTools(True);
    Check(CatalogHas('New reusable'), 'source reload exposes a newly accepted reusable in the real content picker');

    LBefore := GView.Diagram.ToData.ToJSON;
    LRevision := GView.Revision;
    LHost.RefuseCurrent := True;
    LHost.Document.FindComponent('Summary').Find('summary-heading')
      .Configure.Text('A stale proposal').Done;
    LFailed := False;
    try
      GView.ReloadSaved;
    except
      on ENyxDiagram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GView.Revision = LRevision) and
      (GView.Diagram.ToData.ToJSON = LBefore) and
      (SummaryCaption = 'Updated from the accepted source'),
      'late source refusal preserves graph and both previously accepted contexts');
    Check((GCanvas.RendererFor(LForm).InputFor(MemoID) = LField) and LSubscription.Active,
      'refused source publication retains unchanged controls and callbacks');
    LHost.RefuseCurrent := False;
    LHost.EditDuringInspection := True;
    LFailed := False;
    try
      GView.ReloadSaved;
    except
      on ENyxDiagram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GView.Revision = LRevision) and GView.HasPendingProperties and
      (ReadField(ndepContentPadding) = '-held during source inspection'),
      'input typed during source inspection wins and remains unfinished / refused=' +
      TNyxText(BoolToStr(LFailed, True)) + ' / revision=' + IntToStr(GView.Revision) +
      ' expected=' + IntToStr(LRevision) + ' / pending=' +
      TNyxText(BoolToStr(GView.HasPendingProperties, True)) + ' / field=' + ReadField(ndepContentPadding));
    LHost.EditDuringInspection := False;
    GView.RestoreProperties;

    Check(Supports(GCanvas, INyxCanvasSourceLayoutView, LSourceLayout) and
      Supports(GCanvas, INyxCanvasRuntimeView, LRuntime) and
      Supports(GCanvas, INyxCanvasLayoutView, LPresentation) and
      Supports(LRuntime.Scene, INyxCanvasSceneLayouts, LLayouts),
      'diagram consumes the same public prepared source/membership owner');
    LSceneRevision := LRuntime.Scene.Revision;
    LPresentationRevision := LPresentation.PresentationRevision;
    LPlan := LLayouts.PrepareLayout([NyxCanvasPlace(LForm,
      CurrentPlacement(LForm))], LSceneRevision);
    LScope := NewNyxCancellationScope;
    LCancel := TCancelSource.Create;
    LCancelLease := LCancel;
    LCancel.Scope := LScope;
    Check(not LSourceLayout.ApplySourceLayout(LHost.Document, LPlan, GCanvas.Camera,
      LPresentationRevision, LCancelLease, LScope) and not LCancel.Installed,
      'cancellation during prepared validation installs no source/companion');
    Check((LRuntime.Scene.Revision = LSceneRevision) and not LPlan.Committed and
      (LPresentation.PresentationRevision = LPresentationRevision) and
      (SummaryCaption = 'Updated from the accepted source') and
      (GCanvas.RendererFor(LForm).InputFor(MemoID) = LField),
      'cancelled source retains exact scene/presentation and physical controls');
    LPlan := nil;
    LCancelLease := nil;

    LDocument := LHost.Document.Clone;
    LDocument.RemoveRoot(NyxReusableRoot('Summary'));
    LDocument.AddComponent(NewNyxDrawing('Summary')
      .WithProvider(NyxDrawingProvider('Source fixture missing provider')));
    LDocument.AddComponent(NewNyxColumn('Refused catalog choice'));
    LFailed := False;
    try
      LSource.ReplaceSource(LDocument, LGraph, GView.Revision);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GView.Revision = LRevision) and
      (GView.Diagram.ToData.ToJSON = LBefore) and
      (SummaryCaption = 'Updated from the accepted source') and not CatalogHas('Refused catalog choice'),
      'failed physical source staging publishes neither graph, source nor catalog');
    Check((ReadMemo = LText) and (MemoCaret = LCaret) and LSubscription.Active,
      'failed changed-control staging retains live text, caret and callbacks');
    FreeAndNil(LDocument);
    LDocument := LHost.Document.Clone;
    LDocument.State.SetValue(NyxTextState('New source value'), 'A separate source default');
    LDocument.RemoveRoot(NyxReusableRoot('Summary'));
    LFreshMemo := NewNyxMemo('Summary');
    LFreshMemo.Binds.Value(NyxTextState('New source value'));
    LDocument.AddComponent(LFreshMemo);
    LFailed := False;
    try
      LSource.ReplaceSource(LDocument, LGraph, GView.Revision);
    except
      on Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and not GState.Has('New source value') and
      (GState.GetValue(NyxTextState('reply')) = LState) and
      (GView.Revision = LRevision) and (SummaryCaption = 'Updated from the accepted source'),
      'new source binding cannot silently migrate or overwrite the independent runtime store');
    Check((GCanvas.RendererFor(LForm).InputFor(MemoID) = LField) and LSubscription.Active,
      'incompatible source binding cleans up staged replacements and retains old controls');
    LFreshMemo := nil;
    FreeAndNil(LDocument);
    LHost.Document.FindComponent('Summary').Find('summary-heading')
      .Configure.Text('Second accepted source').Done;
    GView.ReloadSaved;
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    Check((SummaryCaption = 'Second accepted source') and
      (GCanvas.RendererFor(LForm).InputFor(MemoID) = LField),
      'a fresh reload succeeds after refusals without replacing unrelated content');
    GContent.SetContent(NyxDiagramVertexRef('Summary'),
      NyxDiagramComponent(NyxComponent('New reusable')), GView.Revision);
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    Check(SummaryCaption = 'A new choice', 'future content uses the newly installed canvas source context');
    Check(GDocument.FindComponent('Summary').Find('summary-heading')
      .Prop(NyxAttributeName(atText)) = 'A different reusable',
      'source refresh changes no original document or owned defaults');
  finally
    LDocument.Free;
    LSubscription.Cancel;
    LSubscription := nil;
    LHostLease := nil;
    LSource := nil;
  end;
end;
