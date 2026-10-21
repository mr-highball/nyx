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
program nyx_view_publication_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data,
  nyx.state, nyx.codec, nyx.content, nyx.presentations, nyx.behavior,
  nyx.events, nyx.scheduler, nyx.publication, nyx.view.publication,
  nyx.resource.context
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Browser renderer is exercised through the same public mount contract. }
  TRenderer = TNyxBrowserRenderer;
  { A borrowed actual browser input or action face. }
  TFace = TJSHTMLElement;
  { The browser host remains owned across every asynchronous observation. }
  THost = TJSHTMLElement;
  {$else}
  { Native renderer supplies actual LCL controls at both admitted widths. }
  TRenderer = TNyxLCLRenderer;
  { A borrowed actual native input or action face. }
  TFace = TControl;
  { The native fixture owns this form until renderer retirement. }
  THost = TForm;
  { Expose the ordinary protected Click slot without replacing Nyx handlers. }
  TControlAccess = class(TControl);
  {$endif}

  { Copied diagnostics contain no document, model, owner or target interfaces. }
  TTrace = record
    Prepared: Integer;
    Validated: Integer;
    Installed: Integer;
    Notified: Integer;
    Retired: Integer;
    Destroyed: Integer;
    Callbacks: Integer;
    Observations: Integer;
    ReentryRefused: Boolean;
    RuntimeID: TNyxText;
  end;

  { Independent fault choices belong only to the public fixture participant. }
  TFault = (vfValidate, vfCancelGeneration, vfReenter, vfNotify, vfRetire,
    vfDispose);
  { A publication can report both committed notification and retirement failure. }
  TFaults = set of TFault;

  { Managed store ownership is separate from renderer and source lifetimes. }
  IStateOwner = interface(IInterface)
    function GetState: TNyxState;
    property State: TNyxState read GetState;
  end;
  { Exact fixture-owned implementation makes Install's lease assignment pure. }
  TStateOwner = class(TInterfacedObject, IStateOwner)
  private
    FState: TNyxState;
  public
    constructor Create(ADocument: TNyxDocument; const AText: TNyxText);
    destructor Destroy; override;
    function GetState: TNyxState;
  end;

  { Forward host declaration is borrowed only through the revocable port. }
  TReview = class;
  { Forward source declaration is borrowed by inactive/accepted dispatch ports. }
  TSource = class;

  { A retained publication never owns the UI host through this weak capability. }
  IReviewPort = interface(IInterface)
    function GetOwner: TReview;
    procedure Revoke;
    property Owner: TReview read GetOwner;
  end;
  { Revocation precedes renderer/host disposal and is a plain owned flag change. }
  TReviewPort = class(TInterfacedObject, IReviewPort)
  private
    FOwner: TReview;
  public
    constructor Create(AOwner: TReview);
    function GetOwner: TReview;
    procedure Revoke;
  end;

  { Managed dispatch gate holds only an exact future token and weak source.
    Staged callbacks are doubly dormant; retirement cannot revive the gate. }
  TCallPort = class(TInterfacedObject)
  public
    Source: TSource;
    Generation: INyxCancellationScope;
    Enabled: Boolean;
    Closed: Boolean;
    procedure Revoke;
  end;

  { Actual input callback leases its source before using the accepted store. }
  TAction = class(TNyxEventCallback)
  public
    Port: TCallPort;
    PortLease: IInterface;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { Ordered observation shares the same revocable accepted-view gate. }
  TActionObserver = class(TNyxEventObserver)
  public
    Port: TCallPort;
    PortLease: IInterface;
    procedure Observe(const AEvent: TNyxEventInfo;
      ACallbackCount: Integer); override;
  end;

  { Independent application work is not owned by any publication source.
    Only event-view notification can cancel its pending execution ticket. }
  TExternalAction = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Named vector owns subscriptions without retaining controls or documents. }
  TTokens = array of INyxEventSubscription;

  { A real application source stages ordinary callbacks and adopts an owned
    store through the participant; it never reaches into renderer internals. }
  TSource = class(TInterfacedObject, INyxViewPublicationSource)
  public
    HostPort: IReviewPort;
    Runtime: IStateOwner;
    Events: INyxEvents;
    Trace: Integer;
    Faults: TFaults;
    Pending: TObject;
    CurrentPort: TCallPort;
    CurrentPortLease: IInterface;
    CurrentTokens: TTokens;
    destructor Destroy; override;
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AView: INyxPreparedEventView): INyxPreparedPublication;
  end;

  { Single-use candidate owns all leases before flags/owner assignment install.
    Former stores/ports survive installation and all displaced cleanup. }
  TPublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FSource: TSource;
    FSourceLease: INyxViewPublicationSource;
    FHostPort: IReviewPort;
    FRuntime: IStateOwner;
    FFormerRuntime: IStateOwner;
    FPort: TCallPort;
    FPortLease: IInterface;
    FFormerPort: TCallPort;
    FFormerPortLease: IInterface;
    FFormerTokens: TTokens;
    FTokens: TTokens;
    FView: INyxPreparedEventView;
    FGeneration: INyxCancellationScope;
    FRevision: Integer;
    FTrace: Integer;
    FFaults: TFaults;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    procedure DoRetire;
  public
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Owns actual target resources and the accepted application store slot.
    Sources retain only Port, so release/disposal creates no renderer cycle. }
  TReview = class
  public
    Document: TNyxDocument;
    Renderer: TRenderer;
    Host: THost;
    Port: IReviewPort;
    Runtime: IStateOwner;
    ActivePort: TCallPort;
    ActivePortLease: IInterface;
    ActiveCallback: INyxEventSubscription;
    ActiveTokens: TTokens;
    ActiveGeneration: INyxCancellationScope;
    Revision: Integer;
    Width: Integer;
    RefuseFactory: Boolean;
    ErrorText: TNyxText;
    constructor Create(AWidth: Integer);
    destructor Destroy; override;
    procedure Mount(const ASource: INyxViewPublicationSource;
      const AOwner: IStateOwner; ADesign: Boolean = False);
    procedure DisposeRenderer;
    procedure TryReenter;
    function ActionID: TNyxText;
    function InputID: TNyxText;
    procedure Click;
    {$ifndef PAS2JS}
    procedure Failed(ASender: TObject; AException: Exception);
    {$endif}
  end;

var
  GChecks: Integer;
  GTraceCount: Integer;
  GTraces: array[0..31] of TTrace;
  GStoresCreated: Integer;
  GStoresDestroyed: Integer;
  GExternalCalls: Integer;
  GReview: TReview;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('View publication: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure CancelTokens(const ATokens: TTokens);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(ATokens) do
  begin
    try
      ATokens[LIndex].Cancel;
    except
      { Disposal attempts every owned token, after the weak port is revoked. }
    end;
  end;
end;

constructor TStateOwner.Create(ADocument: TNyxDocument; const AText: TNyxText);
begin
  inherited Create;
  FState := ADocument.State.Clone;
  FState.SetValue(NyxTextState('draft'), AText);
  Inc(GStoresCreated);
end;

destructor TStateOwner.Destroy;
begin
  FState.Free;
  Inc(GStoresDestroyed);
  inherited Destroy;
end;

function TStateOwner.GetState: TNyxState;
begin
  Result := FState;
end;

constructor TReviewPort.Create(AOwner: TReview);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TReviewPort.GetOwner: TReview;
begin
  Result := FOwner;
end;

procedure TReviewPort.Revoke;
begin
  FOwner := nil;
end;

procedure TCallPort.Revoke;
begin
  Closed := True;
  Enabled := False;
  Source := nil;
end;

procedure TAction.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LSource: TSource;
  LLease: INyxViewPublicationSource;
begin

  if Port.Closed or not Port.Enabled or Port.Generation.Cancelled then
  begin
    Exit;
  end;
  LSource := Port.Source;

  if LSource = nil then
  begin
    Exit;
  end;
  LLease := LSource;
  Inc(GTraces[LSource.Trace].Callbacks);
  LSource.Runtime.State.SetValue(NyxTextState('status'), 'Accepted input');
end;

procedure TActionObserver.Observe(const AEvent: TNyxEventInfo;
  ACallbackCount: Integer);
var
  LSource: TSource;
  LLease: INyxViewPublicationSource;
begin

  if Port.Closed or not Port.Enabled or Port.Generation.Cancelled then
  begin
    Exit;
  end;
  LSource := Port.Source;

  if LSource = nil then
  begin
    Exit;
  end;
  LLease := LSource;
  Check(ACallbackCount = 1, 'prepared observation sees one actual ordinary callback');
  Inc(GTraces[LSource.Trace].Observations);
end;

procedure TExternalAction.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GExternalCalls);
end;

function FindAction(ARoot: TNyxNode): TNyxNode;
var
  LIndex: Integer;
  LFound: TNyxNode;
begin
  Result := nil;

  if (ARoot.Kind = NyxKindName(nkButton)) and
    (ARoot.Prop(NyxAttributeName(atPart)) = NyxPart('submit').Name) then
  begin
    Result := ARoot;
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    LFound := FindAction(ARoot.Children[LIndex]);

    if LFound <> nil then
    begin
      Check(Result = nil, 'fixture has one exact submitted action role');
      Result := LFound;
    end;
  end;
end;

destructor TSource.Destroy;
begin

  if CurrentPort <> nil then
  begin
    CurrentPort.Revoke;
  end;
  CancelTokens(CurrentTokens);
  Inc(GTraces[Trace].Destroyed);
  inherited Destroy;
end;

function TSource.Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
  const AView: INyxPreparedEventView): INyxPreparedPublication;
var
  LHost: TReview;
  LPlan: TPublication;
  LLease: INyxPreparedPublication;
  LAction: TNyxNode;
  LCallback: TAction;
  LObserver: TActionObserver;
  LInfo: TNyxEventInfo;
begin
  LHost := HostPort.Owner;
  Check((LHost <> nil) and (Pending = nil) and (ADocument <> nil) and
    (ARoot <> nil) and ARoot.IsRealized and (AView <> nil),
    'source prepares one exact realized candidate against a live host');
  LPlan := TPublication.Create;
  LLease := LPlan;
  LPlan.FSource := Self;
  LPlan.FSourceLease := Self;
  LPlan.FHostPort := HostPort;
  LPlan.FRuntime := Runtime;
  LPlan.FFormerRuntime := LHost.Runtime;
  LPlan.FFormerPort := LHost.ActivePort;
  LPlan.FFormerPortLease := LHost.ActivePortLease;
  LPlan.FFormerTokens := Copy(LHost.ActiveTokens, 0, Length(LHost.ActiveTokens));
  LPlan.FView := AView;
  LPlan.FGeneration := AView.Generation;
  LPlan.FRevision := LHost.Revision;
  LPlan.FTrace := Trace;
  LPlan.FFaults := Faults;
  LPlan.FPort := TCallPort.Create;
  LPlan.FPortLease := LPlan.FPort;
  LPlan.FPort.Source := Self;
  LPlan.FPort.Generation := LPlan.FGeneration;
  Pending := LPlan;
  Inc(GTraces[Trace].Prepared);

  if Runtime <> nil then
  begin
    LAction := FindAction(ARoot);
    Check(LAction <> nil, 'runtime source finds the actual candidate action');
    GTraces[Trace].RuntimeID := LAction.ID;
    SetLength(LPlan.FTokens, 2);
    LCallback := TAction.Create;
    LCallback.Port := LPlan.FPort;
    LCallback.PortLease := LPlan.FPortLease;
    LPlan.FTokens[0] := AView.PrepareCallback(NyxControlEvents(LAction.ID, niRuntime),
      ntClick, LCallback, NyxEvent(''));
    LObserver := TActionObserver.Create;
    LObserver.Port := LPlan.FPort;
    LObserver.PortLease := LPlan.FPortLease;
    LPlan.FTokens[1] := AView.PrepareObservation(NyxControlEvents(LAction.ID, niRuntime),
      ntClick, LObserver, nopBeforeCallbacks, NyxEvent(''));
    Check(not LPlan.FTokens[0].Active and not LPlan.FTokens[1].Active,
      'callback and observation remain dormant before physical Build');

    if LHost.Renderer.Root = nil then
    begin
      LInfo := Default(TNyxEventInfo);
      LInfo.Trigger := ntClick;
      LInfo.Name := NyxEvent('click');
      LInfo.OriginID := LAction.ID;
      LInfo.SourceID := LAction.ID;
      LInfo.TargetID := LAction.ID;
      LInfo.Value := NyxNull;
      LInfo.Details := NyxNull;
      Events.Dispatch(LInfo, LAction.SourceID, LAction.SourceID);
      Check((GTraces[Trace].Callbacks = 0) and (GTraces[Trace].Observations = 0),
        'old-view input cannot invoke the first dormant source');
    end;
  end;
  Result := LLease;
end;

destructor TPublication.Destroy;
begin
  try
    DoRetire;
  except
    { Explicit retirement reports its fixture failure; destruction never retries. }
  end;
  inherited Destroy;
end;

procedure TPublication.Validate;
var
  LLease: INyxPreparedPublication;
  LHost: TReview;
begin
  LLease := Self;
  LHost := FHostPort.Owner;
  Check(not FValidated and not FRetired and (LHost <> nil),
    'participant validates once while the weak host is live');
  Inc(GTraces[FTrace].Validated);
  Check(LHost.Runtime = FFormerRuntime, 'candidate runtime is not adopted during validation');

  if vfReenter in FFaults then
  begin
    LHost.TryReenter;
    GTraces[FTrace].ReentryRefused := True;
  end;

  if vfValidate in FFaults then
  begin
    raise ENyxModel.Create('Deliberate root participant validation refusal');
  end;

  if vfCancelGeneration in FFaults then
  begin
    FGeneration.Cancel;
  end;
  FValidated := True;
end;

procedure TPublication.CheckReady;
var
  LHost: TReview;
begin
  LHost := FHostPort.Owner;

  if not FValidated or FRetired or FInstalled or (LHost = nil) or
    (LHost.Revision <> FRevision) or (LHost.Runtime <> FFormerRuntime) or
    (FSource.Pending <> Self) or FPort.Closed or FGeneration.Cancelled then
  begin
    raise ENyxModel.Create('Fixture root participant lost its final owned authority');
  end;
end;

procedure TPublication.Install;
var
  LHost: TReview;
begin

  if not FValidated or FInstalled or FRetired then
  begin
    Exit;
  end;
  LHost := FHostPort.Owner;

  if FFormerPort <> nil then
  begin
    FFormerPort.Revoke;
  end;
  LHost.Runtime := FRuntime;
  LHost.ActivePort := FPort;
  LHost.ActivePortLease := FPortLease;
  LHost.ActiveGeneration := FGeneration;
  LHost.ActiveTokens := FTokens;

  if Length(FTokens) > 0 then
  begin
    LHost.ActiveCallback := FTokens[0];
  end
  else
  begin
    LHost.ActiveCallback := nil;
  end;
  Inc(LHost.Revision);
  FSource.CurrentPort := FPort;
  FSource.CurrentPortLease := FPortLease;
  FSource.CurrentTokens := FTokens;
  FPort.Enabled := FRuntime <> nil;
  FInstalled := True;
  Inc(GTraces[FTrace].Installed);
end;

procedure TPublication.Notify;
var
  LHost: TReview;
begin

  if not FInstalled or FNotified then
  begin
    Exit;
  end;
  FNotified := True;
  Inc(GTraces[FTrace].Notified);
  CancelTokens(FFormerTokens);

  if vfDispose in FFaults then
  begin
    LHost := FHostPort.Owner;
    Check(LHost <> nil, 'notification disposal starts from the admitted live receiver');
    LHost.DisposeRenderer;
  end;

  if vfNotify in FFaults then
  begin
    raise ENyxModel.Create('Deliberate committed root notification failure');
  end;
end;

procedure TPublication.DoRetire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if not FInstalled then
  begin
    FPort.Revoke;
    CancelTokens(FTokens);
  end;

  if FSource.Pending = Self then
  begin
    FSource.Pending := nil;
  end;
  Inc(GTraces[FTrace].Retired);
  FSource := nil;
  FSourceLease := nil;
  FView := nil;

  if vfRetire in FFaults then
  begin
    raise ENyxModel.Create('Deliberate root retirement failure');
  end;
end;

procedure TPublication.Retire;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;
  DoRetire;
end;

function Fixture(AContent: Boolean): TNyxDocument;
var
  LPage: INyxPage;
  LRecipe: INyxColumn;
  LInput: INyxInput;
  LButton: INyxButton;
  LInstance: INyxComponent;
  LIndex: Integer;
  LPrefix: TNyxText;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Prepare a useful application view';
    Result.State.SetValue(NyxTextState('draft'), 'Authored placeholder');
    Result.State.SetValue(NyxTextState('status'), 'Waiting for input');
    LPage := NewNyxPage('home');
    Result.AddPage(LPage);
    LPage.Add(NewNyxHeading('heading').WithText('Prepared application ownership'));
    LPage.Add(NewNyxLabel('probe').WithText('Admission keeps the former view useful.'));
    LPage.Add(NewNyxButton('outside-action').WithText('Queue independent application work'));

    if AContent then
    begin
      Result.Presentations.Define(NyxPresentation('focused'),
        TNyxPresentationCondition.Manual);
      for LIndex := 0 to 1 do
      begin
        LPrefix := 'wide';

        if LIndex = 1 then
        begin
          LPrefix := 'compact';
        end;
        LRecipe := NewNyxColumn(LPrefix + '-recipe');
        LInput := NewNyxInput(LPrefix + '-input').WithText('Current draft');
        LInput.Binds.Value(NyxTextState('draft')).Done;
        LRecipe.Add(LInput);
        LButton := NewNyxButton(LPrefix + '-action').WithText('Apply current draft');
        LButton.Configure.PartName(NyxPart('submit')).Done;
        LRecipe.Add(LButton);
        Result.AddComponent(LRecipe);
      end;
      LInstance := NewNyxComponent('workspace');
      LInstance.Content.Use(NyxComponent('wide-recipe'));
      LInstance.Content.WhenPresentation(NyxPresentation('focused')).
        Use(NyxComponent('compact-recipe'));
      LPage.Add(LInstance);
    end
    else
    begin
      LInput := NewNyxInput('input').WithText('Current draft');
      LInput.Binds.Value(NyxTextState('draft')).Done;
      LPage.Add(LInput);
      LButton := NewNyxButton('action').WithText('Apply current draft');
      LButton.Configure.PartName(NyxPart('submit')).Done;
      LPage.Add(LButton);
    end;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function ProbeLabel(ANode: TNyxNode
  {$ifndef PAS2JS}; AOwner: TComponent{$endif}): TFace;
begin

  if GReview.RefuseFactory then
  begin
    raise ENyxModel.Create('Deliberate candidate physical factory refusal');
  end;
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('span'));
  Result.textContent := ANode.Prop(NyxAttributeName(atText));
  {$else}
  Result := TLabel.Create(AOwner);
  TLabel(Result).AutoSize := False;
  TLabel(Result).Caption := String(ANode.Prop(NyxAttributeName(atText)));
  {$endif}
end;

constructor TReview.Create(AWidth: Integer);
begin
  inherited Create;
  Width := AWidth;
  Document := Fixture(False);
  Renderer := TRenderer.Create;
  Port := TReviewPort.Create(Self);
  {$ifdef PAS2JS}
  Host := TJSHTMLElement(Web.document.createElement('div'));
  Host.style.setProperty('width', IntToStr(AWidth) + 'px');
  Host.style.setProperty('height', '700px');
  Web.document.body.appendChild(Host);
  Renderer.RegisterFactory(NyxKindName(nkLabel), @ProbeLabel);
  {$else}
  Host := TForm.Create(nil);
  Host.ClientWidth := AWidth;
  Host.ClientHeight := 700;
  Host.Caption := 'Nyx prepared view publication';
  Host.Show;
  Application.OnException := Failed;
  Renderer.RegisterFactory(NyxKindName(nkLabel), ProbeLabel);
  {$endif}
end;

destructor TReview.Destroy;
begin
  Port.Revoke;
  Renderer.Free;
  ActiveCallback := nil;
  ActiveTokens := nil;
  ActivePort := nil;
  ActivePortLease := nil;
  Runtime := nil;
  Document.Free;
  {$ifdef PAS2JS}Host.remove;
  {$else}
  Application.OnException := nil;
  Host.Free;
  {$endif}
  inherited Destroy;
end;

procedure TReview.Mount(const ASource: INyxViewPublicationSource;
  const AOwner: IStateOwner; ADesign: Boolean);
var
  LState: TNyxState;
begin
  LState := nil;

  if AOwner <> nil then
  begin
    LState := AOwner.State;
  end;
  Renderer.RenderContext(Document, Document.Pages[0], Host, ADesign, LState,
    nil, nil, nil, nrmConfigured, ASource);
end;

procedure TReview.DisposeRenderer;
var
  LRenderer: TRenderer;
begin
  { Revoke the borrowed receiver before any destruction callback. The raw field
    is cleared before Free, so the fixture's later teardown cannot double-free. }
  Port.Revoke;

  if ActivePort <> nil then
  begin
    ActivePort.Revoke;
  end;
  LRenderer := Renderer;
  Renderer := nil;
  LRenderer.Free;
end;

procedure TReview.TryReenter;
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    Mount(nil, Runtime);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'reentrant public RenderContext refuses during admission');
end;

function TReview.ActionID: TNyxText;
var
  LAction: TNyxNode;
begin
  LAction := FindAction(Renderer.Root);
  Check(LAction <> nil, 'accepted view exposes the current action role');
  Result := LAction.ID;
end;

function TReview.InputID: TNyxText;
begin
  Result := 'input';

  if Renderer.Root.Find(Result) <> nil then
  begin
    Exit;
  end;
  Result := NyxQualifiedID('workspace', 'wide-input');

  if Renderer.Root.Find(Result) = nil then
  begin
    Result := NyxQualifiedID('workspace', 'compact-input');
  end;
end;

procedure TReview.Click;
begin
  {$ifdef PAS2JS}Renderer.ElementFor(ActionID, niRuntime).click;
  {$else}TControlAccess(Renderer.ControlFor(ActionID, niRuntime)).Click;{$endif}
end;

{$ifndef PAS2JS}
procedure TReview.Failed(ASender: TObject; AException: Exception);
begin
  ErrorText := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

function NewSource(AReview: TReview; const AOwner: IStateOwner;
  AFaults: TFaults; out ATrace: Integer): INyxViewPublicationSource;
var
  LSource: TSource;
begin
  Check(GTraceCount <= High(GTraces), 'fixture trace budget is explicit');
  ATrace := GTraceCount;
  Inc(GTraceCount);
  LSource := TSource.Create;
  Result := LSource;
  LSource.HostPort := AReview.Port;
  LSource.Runtime := AOwner;
  LSource.Events := AReview.Renderer.Events;
  LSource.Trace := ATrace;
  LSource.Faults := AFaults;
end;

function InputText(AReview: TReview): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AReview.Renderer.InputFor(AReview.InputID)).value;
  {$else}
  Result := TCustomEdit(AReview.Renderer.InputFor(AReview.InputID)).Text;
  {$endif}
end;

procedure WriteInput(AReview: TReview; const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AReview.Renderer.InputFor(AReview.InputID)).value := AText;
  AReview.Renderer.InputFor(AReview.InputID).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AReview.Renderer.InputFor(AReview.InputID)).Text := AText;
  {$endif}
end;

{$ifdef PAS2JS}
function UITurn: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(JSValue(True));
        end, 0);
    end);
end;
{$endif}

{ Await an actual accepted structural recipe, preserving all owners through the
  browser event loop. Native services ordinary LCL and queued UI work. }
procedure AwaitCompact(AReview: TReview); {$ifdef PAS2JS}async;{$endif}
var
  {$ifdef PAS2JS}LStarted: Double;
  {$else}LStarted: QWord;{$endif}
begin
  {$ifdef PAS2JS}LStarted := TJSDate.now;
  {$else}LStarted := GetTickCount64;{$endif}
  while AReview.Renderer.Root.Find(NyxQualifiedID('workspace', 'compact-input')) = nil do
  begin
    {$ifdef PAS2JS}

    if TJSDate.now - LStarted > 10000 then
    {$else}

    if GetTickCount64 - LStarted > 10000 then
    {$endif}
    begin
      raise ENyxModel.Create('Implicit content publication did not settle');
    end;
    {$ifdef PAS2JS}await(TJSPromise.resolve(UITurn));
    {$else}
    Application.ProcessMessages;
    CheckSynchronize(1);

    if AReview.ErrorText <> '' then
    begin
      raise ENyxModel.Create(AReview.ErrorText);
    end;
    {$endif}
  end;
end;

procedure Exercise(AWidth: Integer); {$ifdef PAS2JS}async;{$endif}
var
  LFirst: IStateOwner;
  LNext: IStateOwner;
  LCommitted: IStateOwner;
  LRejectedOwner: IStateOwner;
  LSource: INyxViewPublicationSource;
  LNextSource: INyxViewPublicationSource;
  LCommittedSource: INyxViewPublicationSource;
  LFaultSource: INyxViewPublicationSource;
  LGeneration: INyxCancellationScope;
  LTicket: INyxExecution;
  LExternalToken: INyxEventSubscription;
  LFormerInput: TFace;
  LFormerRoot: TNyxNode;
  LTrace: Integer;
  LNextTrace: Integer;
  LCommittedTrace: Integer;
  LFaultTrace: Integer;
  LRetirementTrace: Integer;
  LContentTrace: Integer;
  LDisposalTrace: Integer;
  LRejected: Boolean;
  LIndex: Integer;
  LPrepared: Integer;
  LCallbacks: Integer;
  LSaved: TNyxText;
begin
  GReview := TReview.Create(AWidth);
  try
    LSaved := TNyxCodec.Encode(GReview.Document);
    LFirst := TStateOwner.Create(GReview.Document, 'First live owner');
    LSource := NewSource(GReview, LFirst, [], LTrace);
    GReview.Mount(LSource, LFirst);
    Check((GReview.Runtime = LFirst) and (GReview.Renderer.State = LFirst.State) and
      (InputText(GReview) = 'First live owner'), 'pure Install adopts the mounted runtime owner');
    Check((GTraces[LTrace].Installed = 1) and
      (GTraces[LTrace].RuntimeID = GReview.ActionID) and
      (GReview.ActiveGeneration = NyxEventGeneration(GReview.Renderer.Events)),
      'plain source-only page uses exact candidate IDs and the accepted future generation');
    LSource := nil;
    Check(GTraces[LTrace].Destroyed = 0, 'accepted renderer retains a caller-released source');
    GReview.Click;
    Check((GTraces[LTrace].Callbacks = 1) and (GTraces[LTrace].Observations = 1) and
      (LFirst.State.GetValue(NyxTextState('status')) = 'Accepted input'),
      'real accepted button runs one prepared callback and observation');
    WriteInput(GReview, 'Preserved actual input');
    Check(LFirst.State.GetValue(NyxTextState('draft')) = 'Preserved actual input',
      'actual input updates only its accepted runtime state');

    for LIndex := 0 to 2 do
    begin
      LFormerRoot := GReview.Renderer.Root;
      LFormerInput := GReview.Renderer.InputFor(GReview.InputID);
      LGeneration := NyxEventGeneration(GReview.Renderer.Events);
      LRejectedOwner := TStateOwner.Create(GReview.Document, 'Refused candidate');
      case LIndex of
        0: LFaultSource := NewSource(GReview, LRejectedOwner, [], LFaultTrace);
        1: LFaultSource := NewSource(GReview, LRejectedOwner, [vfValidate], LFaultTrace);
        2: LFaultSource := NewSource(GReview, LRejectedOwner,
          [vfCancelGeneration], LFaultTrace);
      end;
      GReview.RefuseFactory := LIndex = 0;
      LRejected := False;
      try
        GReview.Mount(LFaultSource, LRejectedOwner);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      GReview.RefuseFactory := False;
      Check(LRejected and (GReview.Renderer.Root = LFormerRoot) and
        (GReview.Renderer.InputFor(GReview.InputID) = LFormerInput),
        'abandoned physical/validator/generation candidate preserves exact old controls');
      Check((GReview.Runtime = LFirst) and (GReview.Renderer.State = LFirst.State) and
        not LGeneration.Cancelled and (InputText(GReview) = 'Preserved actual input'),
        'refusal preserves accepted store, input and event generation');
      Check((GTraces[LFaultTrace].Installed = 0) and
        (GTraces[LFaultTrace].Retired = GTraces[LFaultTrace].Prepared),
        'every discarded root participant receives retirement');
      LFaultSource := nil;
      Check(GTraces[LFaultTrace].Destroyed = 1, 'refused source has no accepted renderer lease');
      LRejectedOwner := nil;
      LCallbacks := GTraces[LTrace].Callbacks;
      GReview.Click;
      Check(GTraces[LTrace].Callbacks = LCallbacks + 1,
        'old actual action remains useful after refused admission');
    end;

    LNext := TStateOwner.Create(GReview.Document, 'Reentrant candidate');
    LNextSource := NewSource(GReview, LNext, [vfReenter], LNextTrace);
    GReview.Mount(LNextSource, LNext);
    Check(GTraces[LNextTrace].ReentryRefused and (GReview.Runtime = LNext) and
      (GTraces[LTrace].Destroyed = 1),
      'reentrant render refuses while accepted publication replaces the former source');
    LFirst := nil;
    LExternalToken := GReview.Renderer.Events.On(
      NyxControlEvents('outside-action', niRuntime), ntClick).
      Policy(neUIQueue).Subscribe(TExternalAction.Create);
    {$ifdef PAS2JS}GReview.Renderer.ElementFor('outside-action').click;
    {$else}TControlAccess(GReview.Renderer.ControlFor('outside-action')).Click;{$endif}
    LTicket := LExternalToken.LastExecution;
    Check((LTicket <> nil) and (LTicket.Status = nesPending),
      'actual queued old callback remains pending before successor admission');
    LGeneration := GReview.ActiveGeneration;
    LCommitted := TStateOwner.Create(GReview.Document, 'Committed despite notification');
    LCommittedSource := NewSource(GReview, LCommitted, [vfNotify, vfRetire], LCommittedTrace);
    LRejected := False;
    try
      GReview.Mount(LCommittedSource, LCommitted);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (GReview.Runtime = LCommitted) and
      (GReview.Renderer.State = LCommitted.State) and
      (InputText(GReview) = 'Committed despite notification'),
      'postcommit cleanup failure retains accepted actual controls and owner');
    Check((GTraces[LCommittedTrace].Notified = 1) and
      (GTraces[LCommittedTrace].Retired = GTraces[LCommittedTrace].Prepared) and
      LGeneration.Cancelled and LTicket.Cancelled and (GExternalCalls = 0),
      'root notification/retirement failure cannot skip old event cleanup');
    LExternalToken.Cancel;
    LExternalToken := nil;
    GReview.Click;
    Check(GTraces[LCommittedTrace].Callbacks = 1,
      'accepted registration remains usable after committed cleanup failure');
    LNextSource := nil;
    LNext := nil;
    LCommittedSource := nil;
    LGeneration := GReview.ActiveGeneration;
    GReview.Mount(nil, LCommitted);
    Check(LGeneration.Cancelled and (GTraces[LCommittedTrace].Destroyed = 1),
      'dropping a source-only runtime attachment advances its event generation');
    GReview.Click;
    Check(GTraces[LCommittedTrace].Callbacks = 1, 'retired source receives no ordinary input');

    LFaultSource := NewSource(GReview, nil, [], LRetirementTrace);
    GReview.Mount(LFaultSource, nil, True);
    LFaultSource := nil;
    Check(GReview.Renderer.DesignMode and (GReview.Runtime = nil) and
      (GTraces[LRetirementTrace].Installed = 1) and
      (GTraces[LRetirementTrace].Destroyed = 1),
      'design transition installs a one-shot retirement source without retaining it');
    Check(TNyxCodec.Encode(GReview.Document) = LSaved,
      'all runtime admission/input/failure cases preserve source defaults');
    LCommitted := nil;

    GReview.Renderer.Unmount;
    FreeAndNil(GReview.Document);
    GReview.Document := Fixture(True);
    LFirst := TStateOwner.Create(GReview.Document, 'Content live owner');
    LSource := NewSource(GReview, LFirst, [], LContentTrace);
    GReview.Mount(LSource, LFirst);
    LSource := nil;
    WriteInput(GReview, 'Draft survives implicit structural remount');
    LGeneration := NyxEventGeneration(GReview.Renderer.Events);
    LPrepared := GTraces[LContentTrace].Prepared;
    GReview.Renderer.Presentations.Select(NyxPresentation('focused'));
    {$ifdef PAS2JS}await(AwaitCompact(GReview));
    {$else}AwaitCompact(GReview);{$endif}
    Check((GTraces[LContentTrace].Prepared > LPrepared) and
      (GTraces[LContentTrace].Destroyed = 0) and LGeneration.Cancelled,
      'real implicit content remount reuses the retained publication source');
    Check((GReview.Runtime = LFirst) and (GReview.Renderer.State = LFirst.State) and
      (InputText(GReview) = 'Draft survives implicit structural remount') and
      (GReview.ActionID = NyxQualifiedID('workspace', 'compact-action')),
      'implicit structural replacement keeps store ownership and binds the new runtime IDs');
    GReview.Click;
    Check((GTraces[LContentTrace].Callbacks = 1) and
      (GTraces[LContentTrace].Observations = 1),
      'replacement action has exactly one accepted callback and observation');
    LTicket := GReview.ActiveCallback.LastExecution;
    FreeAndNil(GReview.Renderer);
    Check((GTraces[LContentTrace].Destroyed = 1) and
      not GReview.ActiveCallback.Active and
      (LFirst.State.GetValue(NyxTextState('draft')) =
        'Draft survives implicit structural remount'),
      'renderer disposal releases source registrations without freeing caller-owned state');

    { This is a distinct normal public mount, not an injected renderer hook.
      Notify frees the renderer before RenderContext returns; afterward only
      copied counters and independently retained managed data are observed. }
    FreeAndNil(GReview);
    GReview := TReview.Create(AWidth);
    LFirst := TStateOwner.Create(GReview.Document, 'Owned beyond notification disposal');
    LSource := NewSource(GReview, LFirst, [vfDispose], LDisposalTrace);
    GReview.Mount(LSource, LFirst);
    Check((GReview.Renderer = nil) and (GReview.Port.Owner = nil) and
      (GReview.Runtime = LFirst),
      'Notify may dispose the renderer after revoking its borrowed host receiver');
    Check((GTraces[LDisposalTrace].Installed = 1) and
      (GTraces[LDisposalTrace].Notified = 1) and
      (GTraces[LDisposalTrace].Retired = GTraces[LDisposalTrace].Prepared) and
      GReview.ActiveGeneration.Cancelled and not GReview.ActiveCallback.Active,
      'disposal during Notify still retires the participant and revokes accepted input');
    Check(LFirst.State.GetValue(NyxTextState('draft')) =
      'Owned beyond notification disposal',
      'accepted caller-owned state survives renderer destruction inside Notify');
    LSource := nil;
    Check(GTraces[LDisposalTrace].Destroyed = 1,
      'disposed renderer and retired preparation release the last source lease');
  finally
    LFaultSource := nil;
    LCommittedSource := nil;
    LNextSource := nil;
    LSource := nil;

    if LExternalToken <> nil then
    begin
      LExternalToken.Cancel;
    end;
    LExternalToken := nil;
    LTicket := nil;
    FreeAndNil(GReview);
    LRejectedOwner := nil;
    LCommitted := nil;
    LNext := nil;
    LFirst := nil;
    {$ifndef PAS2JS}
    for LIndex := 0 to 1023 do
    begin

      if not CheckSynchronize(1) then
      begin
        Break;
      end;
    end;
    {$endif}
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LWidth: Integer;
begin
  try
    LWidth := 1280;
    {$ifndef PAS2JS}
    Application.Initialize;

    if ParamCount > 0 then
    begin
      LWidth := StrToInt(ParamStr(1));
    end;
    {$endif}
    Check(LWidth >= 320, 'native width is an explicit valid fixture input');
    {$ifdef PAS2JS}await(Exercise(LWidth));
    {$else}Exercise(LWidth);{$endif}
    Check(GStoresCreated = GStoresDestroyed,
      'all independent runtime owners retire after target/source disposal');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-view-publication', 'passed');
    document.body.setAttribute('data-view-publication-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' public view-publication checks / width ', LWidth);
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-view-publication', 'failed');
      document.body.setAttribute('data-view-publication-error', E.Message);
      {$else}
      WriteLn('FAIL ', GChecks, ' ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Run;
end.
