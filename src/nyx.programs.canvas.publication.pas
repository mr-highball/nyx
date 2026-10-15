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

unit nyx.programs.canvas.publication;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.programs, nyx.programs.canvas, nyx.programs.execution,
  nyx.canvas.workspace, nyx.application.context, nyx.scheduler;

type
  { UI-only captured delivery authority for a complete program result. It retains
    the canvas view/snapshot and a revocable weak application context, never the
    application or a document backreference. Capture precedes worker execution.
    Every scalar output must map through typed OutputState bindings. Apply admits
    scene, camera and scalar state as one prepared group at the captured revisions
    and page generation. False means cancelled/retired; stale/invalid results
    raise without changing either owner. Notification errors describe committed
    data. Its methods and last release belong to UI; keep a UI owner through
    worker retirement. Computation consumes immutable inputs/snapshot/program
    and never enters this courier. Do not retain it in its own view's callbacks. }
  INyxCanvasProgramDelivery = interface(IInterface)
    ['{E291C5CB-6B87-4E85-A8E6-000000000209}']
    function Apply(const AResult: INyxProgramResult;
      const ACancellation: INyxCancellationScope = nil;
      AMaximumProposals: Integer = 1024): Boolean;
  end;

{ Capture exact active state identity/revision, page generation and canvas
  presentation. Bindings must contain distinct scalar outputs only; existing
  destinations must have their declared types. No program runs or model changes.
  The caller captures ordinary state inputs separately before running its program. }
function CaptureNyxCanvasProgramDelivery(const AView: INyxCanvasWorkspaceView;
  const ASnapshot: INyxCanvasProgramSnapshot; const AContext: INyxApplicationContext;
  const ABindings: INyxProgramStateBindings): INyxCanvasProgramDelivery;

implementation

uses SysUtils, nyx.text, nyx.state, nyx.canvas.geometry, nyx.programs.types,
  nyx.programs.contract, nyx.publication;

type
  { Deferred scalar preparation allows physical target staging to subscribe new
    bindings before state is reserved. Borrowed State is dereferenced only after
    live context/generation checks. Its ordinary preparation owns revocation,
    validation, adoption and notification; this wrapper grants no extra writes. }
  TFrameState = class(TInterfacedObject, INyxPreparedPublication, INyxPublicationReadiness)
  public
    Context: INyxApplicationContext;
    Generation: INyxCancellationScope;
    State: TNyxState;
    Revision: Integer;
    Assignments: array of TNyxStateAssignment;
    Prepared: INyxPreparedPublication;
    Readiness: INyxPublicationReadiness;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Captured immutable mapping/context recipe. Output conversion and geometry
    preparation enter creator getters before the final weak-authority checks.
    Only the separate UI preparation borrows the captured state class. }
  TDelivery = class(TInterfacedObject, INyxCanvasProgramDelivery)
  public
    View: INyxCanvasWorkspaceView;
    LayoutView: INyxCanvasLayoutView;
    Coordinated: INyxCanvasCoordinatedLayoutView;
    Snapshot: INyxCanvasProgramSnapshot;
    Geometry: INyxCanvasProgramFrameSnapshot;
    Context: INyxApplicationContext;
    Generation: INyxCancellationScope;
    Scheduler: INyxScheduler;
    State: TNyxState;
    StateRevision: Integer;
    PresentationRevision: Integer;
    Bindings: array of TNyxProgramStateBinding;
    function Apply(const AResult: INyxProgramResult;
      const ACancellation: INyxCancellationScope;
      AMaximumProposals: Integer): Boolean;
  end;

procedure Require(ACondition: Boolean; AKind: TNyxProgramDiagnosticKind;
  const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxProgram.Create(AKind, Default(TNyxProgramSpan), AMessage);
  end;
end;

function ScalarType(AKind: TNyxStateKind): TNyxProgramType;
begin
  case AKind of
    nskText: Result := NyxProgramType(npkText);
    nskBoolean: Result := NyxProgramType(npkBoolean);
    nskInteger: Result := NyxProgramType(npkInteger);
    nskNumber: Result := NyxProgramType(npkNumber);
  end;
end;

function SameCamera(const ALeft, ARight: TNyxCanvasCamera): Boolean;
begin
  Result := ALeft.Defined and ARight.Defined and
    (ALeft.WorldCenter.X = ARight.WorldCenter.X) and
    (ALeft.WorldCenter.Y = ARight.WorldCenter.Y) and (ALeft.Zoom = ARight.Zoom) and
    (ALeft.ViewportWidth = ARight.ViewportWidth) and
    (ALeft.ViewportHeight = ARight.ViewportHeight) and
    (ALeft.Limits.Minimum = ARight.Limits.Minimum) and
    (ALeft.Limits.Maximum = ARight.Limits.Maximum);
end;

procedure TFrameState.Validate;
var
  LState: TNyxState;
begin
  CheckReady;
  LState := Context.State;
  CheckReady;
  Require(LState = State, npdStale, 'Program frame state owner changed');
  Prepared := LState.PrepareApply(Assignments, Revision);
  Require(Supports(Prepared, INyxPublicationReadiness, Readiness), npdCapability,
    'State preparation requires a final readiness fence');
  Prepared.Validate;
end;

procedure TFrameState.CheckReady;
begin

  if Generation.Cancelled or not Context.Active then
  begin
    raise ENyxCanvasPublicationCancelled.Create('Program frame application/page retired');
  end;

  if Readiness <> nil then
  begin
    Readiness.CheckReady;
  end;
end;

procedure TFrameState.Install;
begin
  Prepared.Install;
end;

procedure TFrameState.Notify;
begin
  Prepared.Notify;
end;

procedure TFrameState.Retire;
begin

  if Prepared <> nil then
  begin
    Prepared.Retire;
  end;
end;

function CaptureNyxCanvasProgramDelivery(const AView: INyxCanvasWorkspaceView;
  const ASnapshot: INyxCanvasProgramSnapshot; const AContext: INyxApplicationContext;
  const ABindings: INyxProgramStateBindings): INyxCanvasProgramDelivery;
var
  LDelivery: TDelivery;
  LBindings: INyxProgramStateBindings;
  LState: TNyxState;
  LBinding: TNyxProgramStateBinding;
  LIndex: Integer;
  LPrevious: Integer;
  LCount: Integer;
begin
  LDelivery := TDelivery.Create;
  Result := LDelivery;
  LDelivery.View := AView;
  LDelivery.Snapshot := ASnapshot;
  LDelivery.Context := AContext;
  LBindings := ABindings;
  Require((LDelivery.View <> nil) and (LDelivery.Snapshot <> nil) and
    (LDelivery.Context <> nil) and (LBindings <> nil), npdType,
    'Program frame capture requires a canvas, snapshot, context and typed outputs');
  LDelivery.Scheduler := LDelivery.Context.Scheduler;
  LDelivery.Scheduler.RequireUI;
  Require(LDelivery.Context.Active and LDelivery.View.Connected, npdStale,
    'Program frame capture requires active owners');
  Require(Supports(LDelivery.View, INyxCanvasLayoutView, LDelivery.LayoutView) and
    Supports(LDelivery.View, INyxCanvasCoordinatedLayoutView, LDelivery.Coordinated) and
    Supports(LDelivery.Snapshot, INyxCanvasProgramFrameSnapshot, LDelivery.Geometry),
    npdCapability, 'Program frame owners do not expose coordinated preparation');
  LState := LDelivery.Context.State;
  Require(LDelivery.Context.Active and (LState <> nil), npdStale,
    'Program frame state retired during initial resolution');
  LDelivery.State := LState;
  LDelivery.StateRevision := LState.Revision;
  LDelivery.Generation := LDelivery.Context.Generation;
  Require(LDelivery.Generation <> nil, npdType, 'Program frame requires a page generation');
  LDelivery.PresentationRevision := LDelivery.LayoutView.PresentationRevision;
  Require(SameCamera(LDelivery.View.Camera, LDelivery.Snapshot.Camera), npdStale,
    'Program frame camera changed since capture');
  LCount := LBindings.Count;
  Require((LCount > 0) and (LCount <= 1024), npdLimit,
    'Program frame requires 1..1024 mapped scalar outputs');
  SetLength(LDelivery.Bindings, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LBinding := LBindings.Binding(LIndex);
    Require((LBinding.Direction = npvOutput) and (LBinding.Name.Name <> ''),
      npdType, 'Program frame bindings must name typed outputs only');
    for LPrevious := 0 to LIndex - 1 do
    begin
      Require(not LDelivery.Bindings[LPrevious].Name.Same(LBinding.Name) and
        (LDelivery.Bindings[LPrevious].StateName <> LBinding.StateName), npdType,
        'Program frame output ports and destinations must be distinct');
    end;
    LDelivery.Bindings[LIndex] := LBinding;
  end;
  { All creator mapping/view getters have completed before borrowing State. }
  Require(LDelivery.Context.Active and not LDelivery.Generation.Cancelled, npdStale,
    'Program frame application retired during capture');
  LState := LDelivery.Context.State;
  Require(LDelivery.Context.Active and not LDelivery.Generation.Cancelled, npdStale,
    'Program frame application retired during state resolution');
  Require((LState = LDelivery.State) and (LState.Revision = LDelivery.StateRevision),
    npdStale, 'Program frame state changed during capture');
  for LIndex := 0 to High(LDelivery.Bindings) do
  begin
    LBinding := LDelivery.Bindings[LIndex];
    Require(LState.Has(LBinding.StateName) and
      (LState.Value(LBinding.StateName).Kind = LBinding.StateKind), npdType,
      'Program frame destination is missing or has another scalar family');
  end;
end;

function TDelivery.Apply(const AResult: INyxProgramResult;
  const ACancellation: INyxCancellationScope; AMaximumProposals: Integer): Boolean;
var
  LKeepAlive: INyxCanvasProgramDelivery;
  LResult: INyxProgramResult;
  LScope: INyxCancellationScope;
  LOutputs: INyxProgramValues;
  LValue: INyxProgramValue;
  LState: TFrameState;
  LStateLease: INyxPreparedPublication;
  LLayout: INyxCanvasProgramLayout;
  LRuntime: INyxCanvasRuntimeView;
  LIndex: Integer;
begin
  LKeepAlive := Self;
  LResult := AResult;
  LScope := ACancellation;
  Scheduler.RequireUI;
  Result := not ((LScope <> nil) and LScope.Cancelled) and
    not Generation.Cancelled and Context.Active and View.Connected;

  if not Result then
  begin
    Exit;
  end;
  Require(LResult <> nil, npdType, 'Program frame delivery requires a complete result');
  Require((LayoutView.PresentationRevision = PresentationRevision) and
    SameCamera(View.Camera, Snapshot.Camera), npdStale, 'Program frame presentation changed');
  LOutputs := LResult.Outputs;
  Require((LOutputs <> nil) and (LOutputs.Count = Length(Bindings)), npdType,
    'Map every scalar program output before coordinated publication');
  LState := TFrameState.Create;
  LStateLease := LState;
  LState.Context := Context;
  LState.Generation := Generation;
  LState.State := State;
  LState.Revision := StateRevision;
  SetLength(LState.Assignments, Length(Bindings));
  try
    for LIndex := 0 to High(Bindings) do
    begin
      LValue := LOutputs.Value(Bindings[LIndex].Name);
      Require((LValue <> nil) and LValue.ValueType.Same(ScalarType(Bindings[LIndex].StateKind)),
        npdType, 'Program frame output changed its declared scalar family');
      LState.Assignments[LIndex] := NyxStateAssign(Bindings[LIndex].StateName, LValue.AsState);
    end;
    Result := not ((LScope <> nil) and LScope.Cancelled) and
      not Generation.Cancelled and Context.Active and View.Connected;

    if not Result then
    begin
      Exit;
    end;
    Require(Supports(View, INyxCanvasRuntimeView, LRuntime), npdCapability,
      'Program frame view does not expose its runtime scene');
    LLayout := Geometry.PrepareFrameGeometry(LResult, LRuntime.Scene, AMaximumProposals);
    Result := not ((LScope <> nil) and LScope.Cancelled) and
      not Generation.Cancelled and Context.Active and View.Connected;

    if not Result then
    begin
      Exit;
    end;
    Result := Coordinated.ApplyCoordinatedLayout(LLayout.Plan, LLayout.Camera,
      PresentationRevision, LStateLease, LScope);
  finally
    LStateLease.Retire;
  end;
end;

end.
