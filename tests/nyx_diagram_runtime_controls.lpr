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
}program nyx_diagram_runtime_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.controls, nyx.state, nyx.events, nyx.behavior, nyx.callbacks, nyx.scheduler,
  nyx.application.context, nyx.collections.registry, nyx.flow, nyx.logic, nyx.flow.types,
  nyx.flow.routes, nyx.flow.route.model, nyx.view.behavior, nyx.diagram, nyx.diagram.content,
  nyx.diagram.declarations, nyx.diagram.editor, nyx.canvas.geometry,
  nyx.canvas.workspace, nyx.canvas.types, nyx.drawing, nyx.test.flow.routes
  {$ifdef PAS2JS}, JS, Web, nyx.diagram.editor.browser, nyx.canvas.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.diagram.editor.lcl,
    nyx.canvas.lcl{$endif};

type
  { Fixture-owned application authority. State/collections are independent copies;
    the lease exposes only revocable borrowed methods. Views disconnect before
    this owner closes its lease and frees the store. No control retains it. }
  TApplicationOwner = class
  public
    State: TNyxState;
    Collections: INyxCollections;
    Generation: INyxCancellationScope;
    Scheduler: INyxScheduler;
    Lease: INyxApplicationContextLease;
    constructor Create(ADocument: TNyxDocument);
    destructor Destroy; override;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
  end;

  { Count creator reconstruction through the existing injected resolver. Returned
    immutable built-in operations retain no application, document or test owner. }
  TResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Calls: Integer;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

  { Ordinary authored first registration uses the existing weak application
    callback base. The retained context is a revocable port, never its host. }
  TDiagramFirst = class(TNyxApplicationEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Second ordinary registration proves ordered application-store access before
    the authored after-dispatch Logic route samples and publishes its data. }
  TDiagramSecond = class(TNyxApplicationEventCallback)
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  {$ifndef PAS2JS}
  { Borrow only a fixture-owned mounted native control for its installed Click;
    no control or parent window escapes the fixture lifetime. }
  TControlAccess = class(TControl);
  { Test-owned exception sink remains alive through deferred UI retirement. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  { Joined native worker retains only a weak context port and borrows fixture
    globals that outlive WaitFor. Mount must refuse on its independent scheduler
    before entering any caller host/tree/context getter; final release is on UI. }
  TEntryWorker = class(TThread)
  public
    Context: INyxApplicationContext;
    Refusals: Integer;
    UnexpectedView: INyxDiagramEditorView;
    procedure Execute; override;
  end;
  {$endif}

var
  GChecks: Integer;
  GDocument: TNyxDocument;
  GOwner: TApplicationOwner;
  GDescriptor: INyxDiagramEditor;
  GView: INyxDiagramEditorView;
  GContent: INyxDiagramContentView;
  GTarget: {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif};
  GCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  GRuntime: INyxCanvasRuntimeView;
  GResolver: TResolver;
  GResolverLease: INyxLogicResolver;
  GRetained: INyxApplicationContext;
  GOrder: TNyxText;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  {$else}
  GHost: TForm;
  GFailure: TFailure;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GFailure <> nil) and (GFailure.Message <> '') then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('Diagram runtime: ' + AReason);
  end;
  Inc(GChecks);
end;

constructor TApplicationOwner.Create(ADocument: TNyxDocument);
begin
  inherited Create;
  State := ADocument.State.Clone;
  Collections := NewNyxCollections(ADocument.Collections);
  Generation := NewNyxCancellationScope;
  Scheduler := NewNyxScheduler;
  Lease := NewNyxApplicationContext(Scheduler, ResolveState, ResolveCollections, ResolveGeneration);
end;

destructor TApplicationOwner.Destroy;
begin

  if Lease <> nil then
  begin
    Lease.Close;
  end;
  Lease := nil;
  Generation.Cancel;
  State.Free;
  inherited Destroy;
end;

function TApplicationOwner.ResolveState: TNyxState;
begin
  Result := State;
end;

function TApplicationOwner.ResolveCollections: INyxCollections;
begin
  Result := Collections;
end;

function TApplicationOwner.ResolveGeneration: INyxCancellationScope;
begin
  Result := Generation;
end;

function TResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin
  Inc(Calls);
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

procedure TDiagramFirst.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Context.Scheduler.RequireUI;
  GRetained := Context;
  GOrder := GOrder + '1';
  Context.State.SetValue(NyxIntegerState('callback-count'),
    Context.State.GetValue(NyxIntegerState('callback-count')) + 1);
end;

procedure TDiagramSecond.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Context.Scheduler.RequireUI;
  Check(Context.State.GetValue(NyxIntegerState('callback-count')) mod 2 = 1,
    'second authored registration sees the first publication in order');
  GOrder := GOrder + '2';
  Context.State.SetValue(NyxIntegerState('callback-count'),
    Context.State.GetValue(NyxIntegerState('callback-count')) + 1);
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.ClassName + ': ' + AError.Message;
end;

procedure TEntryWorker.Execute;
begin
  try
    UnexpectedView := MountNyxLCLDiagramEditor(GDescriptor, GDocument, GHost,
      nil, nil, ndcmRuntime, Context);
  except
    on ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
end;
{$endif}

function RuntimeDocument: TNyxDocument;
var
  LForm: INyxColumn;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LLabel: INyxLabel;
begin
  Result := RoutedLogicDocument;
  try
    Result.Title := 'Diagram behavior workshop';
    Result.State.SetValue(NyxIntegerState('callback-count'), 0);
    LForm := NewNyxColumn('Profile form');
    LForm.Configure.Padding(12).Gap(8).Done;
    LMemo := NewNyxMemo('profile-source').WithText('Profile JSON');
    LMemo.Configure.Height(80).Done;
    LMemo.Binds.Value(NyxTextState('source'));
    LForm.Add(LMemo);
    LButton := NewNyxButton('profile-run').WithText('Format profile');
    NyxCallbacks(LButton).OnClick.Add(NyxHandler('TDiagramFirst'), NyxCallbackID('first'))
      .Add(NyxHandler('TDiagramSecond'), NyxCallbackID('second'));
    SetNyxFlowRoutes(LButton.Node, NewNyxFlowRoutes.WithRoute(ProfileRoute(nfcmAfterDispatch)));
    LForm.Add(LButton);
    LLabel := NewNyxLabel('profile-summary');
    LLabel.Binds.Text(NyxTextState('summary'));
    LForm.Add(LLabel);
    Result.AddComponent(LForm);
    Result.AddComponent(NewNyxColumn('Plain form').Add(NewNyxButton('plain-button').WithText('Continue')));
    Result.AddComponent(NewNyxDrawing('Unavailable drawing')
      .WithProvider(NyxDrawingProvider('Fixture missing provider')));
    Result.Diagrams.Define(NyxDiagramRef('Behavior board'), NewNyxDiagram.WithVertex(
      NyxDiagramVertex(NyxDiagramVertexRef('Profile'), ndsRectangle,
        NyxCanvasBounds(-170, -140, 340, 280)).Captioned('Live profile')
        .WithContent(NyxDiagramComponent(NyxComponent('Profile form')))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

procedure Idle; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LIndex: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  {$else}
  for LIndex := 1 to 3 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
    Sleep(10);
  end;
  {$endif}
end;

function FirstKind(ARoot: TNyxNode; AKind: TNyxKind): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if ARoot.Kind = NyxKindName(AKind) then
  begin
    Exit(ARoot);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    Result := FirstKind(ARoot.Children[LIndex], AKind);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

procedure BindTarget;
begin
  GView := GTarget;

  if not Supports(GView, INyxDiagramContentView, GContent) then
  begin
    raise Exception.Create('Mounted diagram lacks its optional content capability');
  end;
  GCanvas := GView.Canvas as {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};

  if not Supports(GCanvas, INyxCanvasRuntimeView, GRuntime) then
  begin
    raise Exception.Create('Mounted canvas lacks its optional runtime capability');
  end;
end;

procedure Mount(AMode: TNyxDiagramContentMode; AState: TNyxState;
  const AContext: INyxApplicationContext; const AResolver: INyxLogicResolver);
begin
  GTarget := {$ifdef PAS2JS}MountNyxBrowserDiagramEditor{$else}MountNyxLCLDiagramEditor{$endif}(
    GDescriptor, GDocument, GHost, AState, nil, AMode, AContext, AResolver);
  BindTarget;
end;

procedure RetireView;
begin

  if GView <> nil then
  begin
    GView.Disconnect;
  end;
  GView := nil;
  GTarget := nil;
  GCanvas := nil;
  GRuntime := nil;
  GContent := nil;
end;

procedure ClickContent;
var
  LItem: TNyxCanvasItemRef;
  LNode: TNyxNode;
begin
  LItem := GContent.ContentItem(NyxDiagramVertexRef('Profile'));
  LNode := FirstKind(GCanvas.RendererFor(LItem).Root, nkButton);
  Check(LNode <> nil, 'actual materialized reusable contains its ordinary button');
  {$ifdef PAS2JS}
  GCanvas.RendererFor(LItem).ElementFor(LNode.ID).click;
  {$else}
  TControlAccess(GCanvas.RendererFor(LItem).ControlFor(LNode.ID)).Click;
  {$endif}
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LOther: TApplicationOwner;
  LDead: TApplicationOwner;
  LBehavior: INyxViewBehavior;
  LContext: INyxApplicationContext;
  LGeneration: INyxCancellationScope;
  LItem: TNyxCanvasItemRef;
  LBefore: TNyxText;
  LDocumentBefore: TNyxText;
  LFailed: Boolean;
  LRevision: Integer;
  LResolveCalls: Integer;
  LWindow: INyxDiagramEditorWindow;
  {$ifndef PAS2JS}
  LWorker: TEntryWorker;
  {$endif}
begin
  GDocument := RuntimeDocument;
  LDocumentBefore := TNyxCodec.Encode(GDocument);
  GDescriptor := NewNyxDiagramEditor('runtime-workshop', GDocument, NyxDiagramRef('Behavior board'));
  GOwner := TApplicationOwner.Create(GDocument);
  LOther := TApplicationOwner.Create(GDocument);
  LDead := TApplicationOwner.Create(GDocument);
  try
    LFailed := False;
    try
      Mount(ndcmRuntime, nil, nil, nil);
    except
      on ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (GView = nil), 'runtime behavioral content refuses absent application authority');
    LFailed := False;
    try
      Mount(ndcmRuntime, LOther.State, GOwner.Lease.Context, nil);
    except
      on ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'different supplied UI/behavior stores refuse before mounting');
    LDead.Lease.Close;
    LFailed := False;
    try
      Mount(ndcmRuntime, nil, LDead.Lease.Context, nil);
    except
      on ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'retired parent authority refuses');
    GResolver := TResolver.Create;
    GResolverLease := GResolver;
    LFailed := False;
    try
      Mount(ndcmRuntime, nil, nil, GResolverLease);
    except
      on ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'resolver without application context refuses');
    {$ifndef PAS2JS}
    LWorker := TEntryWorker.Create(True);
    try
      LWorker.Context := GOwner.Lease.Context;
      LWorker.Start;
      LWorker.WaitFor;
      Check((LWorker.Refusals = 1) and (LWorker.UnexpectedView = nil),
        'real worker mount refuses before borrowing target/application authority');
    finally
      LWorker.Free;
    end;
    {$endif}
    Mount(ndcmRuntime, nil, GOwner.Lease.Context, GResolverLease);
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    LItem := GContent.ContentItem(NyxDiagramVertexRef('Profile'));
    LBehavior := GRuntime.BehaviorFor(LItem);
    LContext := LBehavior.Context;
    LGeneration := LContext.Generation;
    Check(LContext.Active and (LContext.State = GOwner.State),
      'ordinary embedded behavior receives exact parent application state');
    ClickContent;
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    Check((GOrder = '12') and (GOwner.State.GetValue(NyxIntegerState('callback-count')) = 2),
      'actual content button dispatches both authored registrations once in order');
    Check((GOwner.State.GetValue(NyxTextState('summary')) =
      'Profile' + #10 + GOwner.State.GetValue(NyxTextState('pretty'))) and
      (TNyxDataValue.ParseJSON(GOwner.State.GetValue(NyxTextState('compact')))
        .Field('name').AsText = 'English demo'), 'authored after-dispatch Logic executes and publishes actual transformed data');
    Check(GResolver.Calls > 0, 'actual graph invocation reaches the injected lazy Logic resolver');
    Check((LOther.State.GetValue(NyxIntegerState('callback-count')) = 0) and
      (LOther.State.GetValue(NyxTextState('summary')) = 'Ready'), 'independent application store remains unchanged');
    LResolveCalls := GResolver.Calls;
    GView.SetGeometry(NyxDiagramVertexRef('Profile'), NyxCanvasBounds(-150, -130, 360, 300), GView.Revision);
    Check((GRuntime.BehaviorFor(LItem) = LBehavior) and (GResolver.Calls = LResolveCalls),
      'geometry retains the same behavior/generation and does not reconstruct Logic');
    LBefore := GView.Diagram.ToData.ToJSON;
    LRevision := GView.Revision;
    LFailed := False;
    try
      GContent.SetContent(NyxDiagramVertexRef('Profile'),
        NyxDiagramComponent(NyxComponent('Unavailable drawing')), LRevision);
    except
      on ENyxDrawing do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and LContext.Active and not LGeneration.Cancelled and
      (GView.Revision = LRevision) and (GView.Diagram.ToData.ToJSON = LBefore),
      'failed target staging preserves accepted behavior authority, generation and graph');
    ClickContent;
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    Check((GOrder = '1212') and (GOwner.State.GetValue(NyxIntegerState('callback-count')) = 4),
      'retained registrations remain single after geometry and failed replacement');
    GContent.SetContent(NyxDiagramVertexRef('Profile'),
      NyxDiagramComponent(NyxComponent('Plain form')), GView.Revision);
    Check(not LContext.Active and LGeneration.Cancelled and GOwner.Lease.Context.Active,
      'replacement revokes old item context/generation without cancelling parent authority');
    LFailed := False;
    try
      Check(GRetained.State = nil, 'revoked callback State must refuse rather than return');
    except
      on ENyxApplicationContext do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'retained authored callback context refuses before reading borrowed state after replacement');
    RetireView;
    Mount(ndcmRuntime, nil, LOther.Lease.Context, GResolverLease);
    ClickContent;
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
    Check((LOther.State.GetValue(NyxIntegerState('callback-count')) = 2) and
      (GOwner.State.GetValue(NyxIntegerState('callback-count')) = 4),
      'a separate runtime mount binds fresh authored instances to its independent store');
    LContext := GRuntime.BehaviorFor(GContent.ContentItem(NyxDiagramVertexRef('Profile'))).Context;
    LOther.Lease.Close;
    Check(not LContext.Active, 'parent revocation reaches the existing embedded context');
    RetireView;
    LWindow := {$ifdef PAS2JS}NewNyxBrowserDiagramEditorWindow{$else}NewNyxLCLDiagramEditorWindow{$endif}(
      'floating-runtime', GDocument, NyxDiagramRef('Behavior board'), GHost, nil,
      nil, ndcmRuntime, GOwner.Lease.Context, GResolverLease);
    try
      GTarget := LWindow.View as {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif};
      BindTarget;
      ClickContent;
      {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
      Check(LWindow.IsOpen and (GOwner.State.GetValue(NyxIntegerState('callback-count')) = 6),
        'public floating runtime forwards the same explicit context/resolver to real authored controls');
      LContext := GRuntime.BehaviorFor(GContent.ContentItem(NyxDiagramVertexRef('Profile'))).Context;
      LWindow.Hide;
      Check(not LWindow.IsOpen and GView.Connected and LContext.Active,
        'floating dismissal retains the mounted runtime lifetime');
    finally
      LWindow.Disconnect;
      RetireView;
      LWindow := nil;
    end;
    Check(not LContext.Active and GOwner.Lease.Context.Active,
      'floating retirement revokes only its content context');
    Mount(ndcmDesignPreview, nil, nil, nil);
    Check(GRuntime.BehaviorFor(GContent.ContentItem(NyxDiagramVertexRef('Profile'))) = nil,
      'explicit design preview mounts behavioral definitions without constructing runtime attachments');
    Check(Pos('Design preview', GTarget.Renderer.Root.Find(
      NyxDiagramEditorPart(GDescriptor.ID, ndepStatus).ID).Prop(NyxAttributeName(atText))) = 1,
      'preview status visibly explains why project actions are paused');
    Check(TNyxCodec.Encode(GDocument) = LDocumentBefore, 'runtime/preview activity leaves exact authored document defaults and declarations unchanged');
    RetireView;
  finally
    RetireView;
    LBehavior := nil;
    LContext := nil;
    GRetained := nil;
    LDead.Free;
    LOther.Free;
  end;
end;

procedure Cleanup;
begin
  RetireView;
  GDescriptor := nil;
  GResolverLease := nil;
  GResolver := nil;
  FreeAndNil(GOwner);
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
    Cleanup;
    document.body.setAttribute('data-diagram-runtime', 'passed');
    document.body.setAttribute('data-diagram-runtime-checks', IntToStr(GChecks));
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-diagram-runtime', 'failed');
      document.body.setAttribute('data-diagram-runtime-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    RegisterNyxCallback(NyxHandler('TDiagramFirst'), TDiagramFirst);
    RegisterNyxCallback(NyxHandler('TDiagramSecond'), TDiagramSecond);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    GHost.style.setProperty('height', '100dvh');
    document.body.appendChild(GHost);
    window.setTimeout(@Start, 100);
    {$else}
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    GHost := TForm.CreateNew(nil);
    GHost.SetBounds(20, 20, 980, 760);

    if ParamStr(2) = '--compact' then
    begin
      GHost.SetBounds(20, 20, 390, 800);
    end;
    GHost.Show;
    try
      Run;
    finally
      Cleanup;
      Idle;
      GHost.Free;
      Application.OnException := nil;
      GFailure.Free;
    end;
    WriteLn('PASS ', GChecks, ' actual diagram runtime checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-runtime', 'failed');
      document.body.setAttribute('data-diagram-runtime-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
