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



unit nyx.programs.execution;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.state, nyx.programs, nyx.programs.types, nyx.programs.contract,
  nyx.scheduler, nyx.application.context, nyx.linq.execution;

type
  { Copied explicit scalar-state binding. Authoring uses typed state-reference
    overloads, not arbitrary application property names. No store is retained. }
  TNyxProgramStateBinding = record
  private
    FName: TNyxProgramSymbolRef;
    FState: TNyxText;
    FKind: TNyxStateKind;
    FDirection: TNyxProgramDirection;
  public
    property Name: TNyxProgramSymbolRef read FName;
    property StateName: TNyxText read FState;
    property StateKind: TNyxStateKind read FKind;
    property Direction: TNyxProgramDirection read FDirection;
  end;

  { Managed immutable state-route recipe. Derivation detaches binding membership.
    Sources/targets are exact typed references; each contract port is mapped once.
    JSON/bytes/opaque handles stay available to direct Run, not scalar state. }
  INyxProgramStateBindings = interface(IInterface)
    ['{9D69BC4B-085E-45F1-A9B0-000000000112}']
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxTextStateRef): INyxProgramStateBindings; overload;
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxBooleanStateRef): INyxProgramStateBindings; overload;
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxIntegerStateRef): INyxProgramStateBindings; overload;
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxNumberStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxTextStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxBooleanStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxIntegerStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxNumberStateRef): INyxProgramStateBindings; overload;
    function Count: Integer;
    function Binding(AIndex: Integer): TNyxProgramStateBinding;
  end;

  { UI-only capture, independently owned program execution, guarded UI delivery.
    One ordinary State.Apply publishes ALL scalar outputs at the captured state
    identity/revision and page generation. Stale, failed, cancelled or retired
    runs retain the accepted state. Observer exceptions after an entered commit
    retain ordinary State.Apply semantics; no rollback is fabricated.

    This adapter admits output-only result publication, refusing any referenced
    host proposal before submission and checking the completed journal again.
    Scene/camera/resource proposal admission needs its own owning adapter; a
    proposed call is never ignored or executed individually here.

    Default defers on UI on both targets. Native neThreaded uses actual workers;
    browser refusal/capabilities follow the existing scheduler. Explicit query/
    clock services must be immutable/thread-safe for the chosen worker policy.
    Returned runs support in-flight native cancellation through an owned scope;
    browser synchronous interpreter calls do not yield to another UI event. }
  TNyxProgramExecution = class
  public
    class function Submit(const AContext: INyxApplicationContext;
      const AProgram: INyxProgram; const ABindings: INyxProgramStateBindings;
      APolicy: TNyxExecutionPolicy = neUIQueue): INyxQueryRun; overload; static;
    class function Submit(const AContext: INyxApplicationContext;
      const AProgram: INyxProgram; const ABindings: INyxProgramStateBindings;
      const AOptions: INyxProgramRunOptions;
      APolicy: TNyxExecutionPolicy = neUIQueue): INyxQueryRun; overload; static;
  end;

function NyxProgramStateBindings: INyxProgramStateBindings;

implementation

uses nyx.linq;

type
  { Default immutable binding recipe stores copied names/families only. }
  TBindings = class(TInterfacedObject, INyxProgramStateBindings)
  private
    FItems: array of TNyxProgramStateBinding;
    function Add(const AName: TNyxProgramSymbolRef; const AState: TNyxText;
      AKind: TNyxStateKind; ADirection: TNyxProgramDirection): INyxProgramStateBindings;
  public
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxTextStateRef): INyxProgramStateBindings; overload;
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxBooleanStateRef): INyxProgramStateBindings; overload;
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxIntegerStateRef): INyxProgramStateBindings; overload;
    function InputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxNumberStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxTextStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxBooleanStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxIntegerStateRef): INyxProgramStateBindings; overload;
    function OutputState(const AName: TNyxProgramSymbolRef;
      const AState: TNyxNumberStateRef): INyxProgramStateBindings; overload;
    function Count: Integer;
    function Binding(AIndex: Integer): TNyxProgramStateBinding;
  end;

  { Owned combined token joins local user cancellation, captured page generation
    and an optional caller scope. It owns no application/scheduler or run. }
  TProgramScope = class(TInterfacedObject, INyxCancellationScope)
  public
    Local: INyxCancellationScope;
    Generation: INyxCancellationScope;
    Caller: INyxCancellationScope;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

  { Public run owns the ordinary query receipt and the same cooperative token
    captured before any worker can enter. No worker-to-run ownership cycle forms. }
  TProgramRun = class(TInterfacedObject, INyxQueryRun)
  public
    Run: INyxQueryRun;
    Scope: INyxCancellationScope;
    procedure Cancel;
    function GetPhase: TNyxQueryPhase;
    function GetFailure: TNyxText;
  end;

  { Only immutable input/program/services cross into computation. No live store,
    renderer, document or application capability belongs to this selector. }
  TProgramSelector = class(TInterfacedObject, INyxSelector<Integer, INyxProgramResult>)
  public
    ProgramValue: INyxProgram;
    Inputs: INyxProgramValues;
    Options: INyxProgramRunOptions;
    Scope: INyxCancellationScope;
    function Project(const AValue: Integer): INyxProgramResult;
  end;

  { UI courier alone retains the weak application port and expected raw store
    identity. It dereferences state only after active generation/identity checks.
    Target names/families are independently copied during UI capture. }
  TProgramPort = class(TInterfacedObject, INyxQueryResultPort<INyxProgramResult>)
  public
    Context: INyxApplicationContext;
    Scheduler: INyxScheduler;
    Scope: INyxCancellationScope;
    ExpectedStore: TNyxState;
    Revision: Integer;
    Targets: array of TNyxProgramStateBinding;
    function Publish(const AValues: TNyxSequenceArray<INyxProgramResult>): Boolean;
  end;

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxProgram.Create(npdType, Default(TNyxProgramSpan), AMessage);
  end;
end;

function StateType(AKind: TNyxStateKind): TNyxProgramType;
begin
  case AKind of
    nskText: Result := NyxProgramType(npkText);
    nskBoolean: Result := NyxProgramType(npkBoolean);
    nskInteger: Result := NyxProgramType(npkInteger);
    nskNumber: Result := NyxProgramType(npkNumber);
  end;
end;

function NyxProgramStateBindings: INyxProgramStateBindings;
begin
  Result := TBindings.Create;
end;

function TBindings.Add(const AName: TNyxProgramSymbolRef; const AState: TNyxText;
  AKind: TNyxStateKind; ADirection: TNyxProgramDirection): INyxProgramStateBindings;
var
  LValue: TBindings;
  LIndex: Integer;
begin
  NyxProgramSymbol(AName.Name);
  Require(AState <> '', 'Program state bindings require a typed state reference');
  for LIndex := 0 to High(FItems) do
  begin
    Require(not FItems[LIndex].Name.Same(AName), 'Each program port is mapped only once');

    if (ADirection = npvOutput) and (FItems[LIndex].Direction = npvOutput) then
    begin
      Require(FItems[LIndex].StateName <> AState, 'Program output destinations must be distinct');
    end;
  end;
  LValue := TBindings.Create;
  Result := LValue;
  SetLength(LValue.FItems, Length(FItems) + 1);
  for LIndex := 0 to High(FItems) do
  begin
    LValue.FItems[LIndex] := FItems[LIndex];
  end;
  LIndex := Length(FItems);
  LValue.FItems[LIndex].FName := AName;
  LValue.FItems[LIndex].FState := AState;
  LValue.FItems[LIndex].FKind := AKind;
  LValue.FItems[LIndex].FDirection := ADirection;
end;

function TBindings.InputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxTextStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskText, npvInput);
end;

function TBindings.InputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxBooleanStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskBoolean, npvInput);
end;

function TBindings.InputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxIntegerStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskInteger, npvInput);
end;

function TBindings.InputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxNumberStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskNumber, npvInput);
end;

function TBindings.OutputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxTextStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskText, npvOutput);
end;

function TBindings.OutputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxBooleanStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskBoolean, npvOutput);
end;

function TBindings.OutputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxIntegerStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskInteger, npvOutput);
end;

function TBindings.OutputState(const AName: TNyxProgramSymbolRef;
  const AState: TNyxNumberStateRef): INyxProgramStateBindings;
begin
  Result := Add(AName, AState.Name, nskNumber, npvOutput);
end;

function TBindings.Count: Integer;
begin
  Result := Length(FItems);
end;

function TBindings.Binding(AIndex: Integer): TNyxProgramStateBinding;
begin
  Require((AIndex >= 0) and (AIndex < Length(FItems)), 'State binding index is out of range');
  Result := FItems[AIndex];
end;

procedure TProgramScope.Cancel;
begin
  Local.Cancel;
end;

function TProgramScope.GetCancelled: Boolean;
begin
  Result := Local.Cancelled or ((Generation <> nil) and Generation.Cancelled) or
    ((Caller <> nil) and Caller.Cancelled);
end;

procedure TProgramRun.Cancel;
begin

  if Run.Phase in [nqpSucceeded, nqpDiscarded, nqpFailed, nqpCancelled] then
  begin
    Exit;
  end;
  Run.Cancel;
  Scope.Cancel;
end;

function TProgramRun.GetPhase: TNyxQueryPhase;
begin
  Result := Run.Phase;
end;

function TProgramRun.GetFailure: TNyxText;
begin
  Result := Run.Failure;
end;

function TProgramSelector.Project(const AValue: Integer): INyxProgramResult;
begin
  try
    Result := ProgramValue.Run(Inputs, Options.Cancellation(Scope));
  except
    on E: ENyxProgram do
    begin

      if E.Diagnostic.Kind <> npdCancelled then
      begin
        raise;
      end;
      { The ordinary query checks the captured token after Project. A cancelled
        placeholder cannot reach the receiver or become a successful output. }
      Scope.Cancel;
      Result := nil;
    end;
  end;
end;

function TProgramPort.Publish(const AValues: TNyxSequenceArray<INyxProgramResult>): Boolean;
var
  LState: TNyxState;
  LAssignments: array of TNyxStateAssignment;
  LOutputs: INyxProgramValues;
  LValue: INyxProgramValue;
  LIndex: Integer;
begin
  Scheduler.RequireUI;
  { Cancellation is an external interface too. Read it before the final active
    test so a reentrant caller scope cannot leave an already retired context. }
  Result := not Scope.Cancelled and Context.Active;

  if not Result then
  begin
    Exit;
  end;
  LState := Context.State;
  Require(LState = ExpectedStore, 'Application state owner changed before behavior delivery');
  Require((Length(AValues) = 1) and (AValues[0] <> nil), 'Behavior delivery requires one complete result');
  Require(AValues[0].ProposalCount = 0, 'State publication cannot ignore host proposals');
  LOutputs := AValues[0].Outputs;
  Require((LOutputs <> nil) and (LOutputs.Count = Length(Targets)),
    'Behavior output frame changed after capture');
  SetLength(LAssignments, Length(Targets));
  for LIndex := 0 to High(Targets) do
  begin
    LValue := LOutputs.Value(Targets[LIndex].Name);
    Require((LValue <> nil) and LValue.ValueType.Same(StateType(Targets[LIndex].StateKind)),
      'Behavior output has a different declared scalar family');
    LAssignments[LIndex] := NyxStateAssign(Targets[LIndex].StateName, LValue.AsState);
  end;
  { Alternative result/frame/value getters can reenter UI code. No raw captured
    store is dereferenced until their whole conversion has finished and the
    owning context/page is still current. Retired work is discarded, not applied. }
  Result := not Scope.Cancelled and Context.Active;

  if not Result then
  begin
    Exit;
  end;
  Require(Context.State = ExpectedStore, 'Application state owner changed during behavior conversion');
  LState.Apply(LAssignments, Revision);
end;

class function TNyxProgramExecution.Submit(const AContext: INyxApplicationContext;
  const AProgram: INyxProgram; const ABindings: INyxProgramStateBindings;
  APolicy: TNyxExecutionPolicy): INyxQueryRun;
begin
  Result := Submit(AContext, AProgram, ABindings, NyxProgramRun, APolicy);
end;

class function TNyxProgramExecution.Submit(const AContext: INyxApplicationContext;
  const AProgram: INyxProgram; const ABindings: INyxProgramStateBindings;
  const AOptions: INyxProgramRunOptions; APolicy: TNyxExecutionPolicy): INyxQueryRun;
var
  LState: TNyxState;
  LCaptured: TNyxState;
  LGeneration: INyxCancellationScope;
  LScheduler: INyxScheduler;
  LRevision: Integer;
  LContract: TNyxProgramContract;
  LBindings: array of TNyxProgramStateBinding;
  LBinding: TNyxProgramStateBinding;
  LPort: TProgramPort;
  LPortLease: INyxQueryResultPort<INyxProgramResult>;
  LSelector: TProgramSelector;
  LSelectorLease: INyxSelector<Integer, INyxProgramResult>;
  LSequence: INyxSequence<INyxProgramResult>;
  LRun: TProgramRun;
  LScope: TProgramScope;
  LScopeLease: INyxCancellationScope;
  LInputs: INyxProgramValues;
  LIndex: Integer;
  LPart: Integer;
  LFound: Integer;
  LOutputs: Integer;
begin
  Require((AContext <> nil) and (AProgram <> nil) and (ABindings <> nil) and
    (AOptions <> nil), 'Program publication requires a context, compiled program, bindings and options');
  AOptions.Validate;
  LScheduler := AContext.Scheduler;
  LScheduler.RequireUI;
  LScheduler.Admit(APolicy);
  Require(AContext.Active, 'Program publication requires an active application');
  LState := AContext.State;
  LGeneration := AContext.Generation;
  Require((LState <> nil) and (LGeneration <> nil) and not LGeneration.Cancelled,
    'Program publication requires live state/page generation');
  LRevision := LState.Revision;
  LContract := AProgram.Contract;
  Require(not AProgram.MayPropose, 'Host proposal programs require a separate owning transaction adapter');
  Require(ABindings.Count = LContract.PortCount, 'Map every program input/output port exactly once');
  SetLength(LBindings, ABindings.Count);
  LInputs := NyxProgramValues;
  LOutputs := 0;
  LCaptured := LState.Clone;
  try
    for LIndex := 0 to High(LBindings) do
    begin
      LBinding := ABindings.Binding(LIndex);
      LFound := -1;
      for LPart := 0 to LContract.PortCount - 1 do
      begin

        if LContract.Port(LPart).Name.Same(LBinding.Name) and
          (LContract.Port(LPart).Direction = LBinding.Direction) and
          LContract.Port(LPart).ValueType.Same(StateType(LBinding.StateKind)) then
        begin
          LFound := LPart;
          Break;
        end;
      end;
      Require(LFound >= 0, 'State binding must match an exact declared scalar program port');
      Require(LCaptured.Has(LBinding.StateName) and
        (LCaptured.Value(LBinding.StateName).Kind = LBinding.StateKind),
        'A behavior state source/destination is missing or has a different family');
      for LPart := 0 to LIndex - 1 do
      begin
        Require(not LBindings[LPart].Name.Same(LBinding.Name), 'Duplicate mapped program port');

        if (LBinding.Direction = npvOutput) and (LBindings[LPart].Direction = npvOutput) then
        begin
          Require(LBindings[LPart].StateName <> LBinding.StateName, 'Duplicate mapped output state');
        end;
      end;
      LBindings[LIndex] := LBinding;

      if LBinding.Direction = npvInput then
      begin
        LInputs := LInputs.WithValue(LBinding.Name, NyxProgramValue(LCaptured.Value(LBinding.StateName)));
      end
      else
      begin
        Inc(LOutputs);
      end;
    end;
    Require(LOutputs > 0, 'State publication requires at least one declared scalar output');
    { Alternative contract/binding getters may reenter application code. Keep
      the exact captured authority/revision valid before adopting queued work. }
    Require(AContext.Active and not LGeneration.Cancelled,
      'Application retired during behavior capture');
    Require((AContext.State = LState) and (LState.Revision = LRevision),
      'Application state changed during behavior capture');
    LScope := TProgramScope.Create;
    LScopeLease := LScope;
    LScope.Local := NewNyxCancellationScope;
    LScope.Generation := LGeneration;
    LScope.Caller := AOptions.CancellationScope;
    LSelector := TProgramSelector.Create;
    LSelectorLease := LSelector;
    LSelector.ProgramValue := AProgram;
    LSelector.Inputs := LInputs;
    LSelector.Options := AOptions;
    LSelector.Scope := LScopeLease;
    LSequence := TNyxSelect<Integer, INyxProgramResult>.From(
      TNyxSequence<Integer>.From([0]), LSelectorLease);
    LPort := TProgramPort.Create;
    LPortLease := LPort;
    LPort.Context := AContext;
    LPort.Scheduler := LScheduler;
    LPort.Scope := LScopeLease;
    LPort.ExpectedStore := LState;
    LPort.Revision := LRevision;
    SetLength(LPort.Targets, LOutputs);
    LOutputs := 0;
    for LIndex := 0 to High(LBindings) do
    begin

      if LBindings[LIndex].Direction = npvOutput then
      begin
        LPort.Targets[LOutputs] := LBindings[LIndex];
        Inc(LOutputs);
      end;
    end;
    LRun := TProgramRun.Create;
    Result := LRun;
    LRun.Scope := LScopeLease;
    LRun.Run := TNyxQuery<INyxProgramResult>.Submit(LSequence, LScheduler, LPortLease,
      APolicy, LScopeLease);
  finally
    LCaptured.Free;
  end;
end;

end.
