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
unit nyx.pascal.work;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.scheduler;

type
  { Logical bounded work boundaries, not elapsed time or scalar/CPU instructions.
    A token is admitted before scanning/reading, including direct emission of
    its final lexical row. A separate row is admitted before a later copy;
    direct emission creates no intermediate vector/import to charge again.
    Metadata tree nodes and binding/evaluator steps have their own charges.
    A memo operation is bounded to its fixed 64-entry/8192-node implementation;
    proof traversal is separately admitted per examined node. Existing source,
    token, node, depth and per-query ceilings remain independently enforced. }
  TNyxPascalAnalysisPhase = (papSourceToken, papFrozenRow, papProjectionToken,
    papLexicalToken, papLexicalRow, papFilterToken, papFilterRow, papSyntaxToken,
    papMetadataValue, papMetadataNode, papDiagnosticRow, papTypeToken,
    papTypeNode, papTypeProof, papTypeMemo, papWorldEntry, papWorldToken,
    papBindingStep, papConditionInput, papConstantPlan, papConstantStep);

  { Value-only detached receipt. Each phase counts admitted work, including work
    subsequently abandoned on refusal/cancellation; a failed admission increments
    nothing. No source, callback, synchronization primitive or interface is held. }
  TNyxPascalAnalysisReceipt = record
  private
    FMaximum: Integer;
    FTotal: Integer;
    FCounts: array[TNyxPascalAnalysisPhase] of Integer;
  public
    function Used(APhase: TNyxPascalAnalysisPhase): Integer;
    property Total: Integer read FTotal;
    property Maximum: Integer read FMaximum;
  end;

  { Exact pre-operation refusal. This exception owns scalar observations only;
    callers may wrap it in their existing operation-specific diagnostic family. }
  ENyxPascalAnalysisWorkLimit = class(Exception)
  private
    FPhase: TNyxPascalAnalysisPhase;
    FBefore: Integer;
    FRequested: Integer;
    FMaximum: Integer;
  public
    constructor Create(APhase: TNyxPascalAnalysisPhase; ABefore, ARequested,
      AMaximum: Integer); reintroduce;
    property Phase: TNyxPascalAnalysisPhase read FPhase;
    property WorkBefore: Integer read FBefore;
    property Requested: Integer read FRequested;
    property Maximum: Integer read FMaximum;
  end;

  { Optional execution capability carried by the ordinary cancellation scope.
    It is explicit caller-owned policy, never a global/thread-local counter or
    authority inferred from telemetry. One synchronous operation owns a budget;
    concurrent readers use independent budgets. Published source/world/type
    values must retire this capability before returning. Retire releases the
    borrowed parent cancellation lease and refuses all later work. Cancel affects
    this operation only, never the caller's outer cancellation generation. A
    work-bearing parent is also admitted before committing a child's counters;
    nested budgets enforce both ceilings rather than bypassing the outer one. }
  INyxPascalAnalysisBudget = interface(INyxCancellationScope)
    ['{17A51354-8B8E-4916-B109-546A7D817056}']
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;

{ No work is admitted at construction. A positive maximum is immutable; nil
  parent means a locally cancellable operation with no external dependency. }
function NewNyxPascalAnalysisBudget(AMaximum: Integer;
  const AParent: INyxCancellationScope = nil): INyxPascalAnalysisBudget;

{ Optional policy dispatch at a real bounded work boundary. Plain/nil cancellation
  scopes retain their existing behavior. The caller checks ordinary cancellation
  as before; an attached budget also checks it before every admission. }
procedure NyxPascalAdmitAnalysis(const AScope: INyxCancellationScope;
  APhase: TNyxPascalAnalysisPhase; AUnits: Integer = 1);

{ Stable English diagnostic label for a closed phase; unknown ordinals refuse. }
function NyxPascalAnalysisPhaseName(APhase: TNyxPascalAnalysisPhase): String;

implementation

uses nyx.pascal.lexing;

type
  { Sole mutable owner during a synchronous operation. It owns only numbers and
    an outward cancellation lease; no parser/source/result can be retained here. }
  TAnalysisBudget = class(TInterfacedObject, INyxCancellationScope,
    INyxPascalAnalysisBudget)
  private
    FParent: INyxCancellationScope;
    FData: TNyxPascalAnalysisReceipt;
    FCancelled: Integer;
    FRetired: Boolean;
  public
    constructor Create(AMaximum: Integer; const AParent: INyxCancellationScope);
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function Receipt: TNyxPascalAnalysisReceipt;
    procedure Retire;
  end;

function TNyxPascalAnalysisReceipt.Used(APhase: TNyxPascalAnalysisPhase): Integer;
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
  begin
    raise EArgumentException.Create('Unknown Pascal analysis work phase');
  end;
  Result := FCounts[APhase];
end;

constructor ENyxPascalAnalysisWorkLimit.Create(APhase: TNyxPascalAnalysisPhase;
  ABefore, ARequested, AMaximum: Integer);
begin

  if (AMaximum < 1) or (ABefore < 0) or (ABefore > AMaximum) or
    (ARequested < 1) or (ARequested <= AMaximum - ABefore) then
  begin
    raise EArgumentException.Create('Analysis refusal needs exact exhausted admission counts');
  end;
  inherited CreateFmt('Pascal analysis work exceeds its aggregate budget ' +
    '[phase=%s used=%d requested=%d limit=%d]',
    [NyxPascalAnalysisPhaseName(APhase), ABefore, ARequested, AMaximum]);
  FPhase := APhase;
  FBefore := ABefore;
  FRequested := ARequested;
  FMaximum := AMaximum;
end;

constructor TAnalysisBudget.Create(AMaximum: Integer;
  const AParent: INyxCancellationScope);
begin
  inherited Create;

  if AMaximum < 1 then
  begin
    raise EArgumentException.Create('Pascal analysis work needs a positive maximum');
  end;
  FData := Default(TNyxPascalAnalysisReceipt);
  FData.FMaximum := AMaximum;
  FParent := AParent;
end;

procedure TAnalysisBudget.Cancel;
begin
  {$ifdef PAS2JS}
  FCancelled := 1;
  {$else}
  InterlockedExchange(FCancelled, 1);
  {$endif}
end;

function TAnalysisBudget.GetCancelled: Boolean;
begin
  {$ifdef PAS2JS}
  Result := FCancelled <> 0;
  {$else}
  Result := InterlockedCompareExchange(FCancelled, 0, 0) <> 0;
  {$endif}
  Result := Result or FRetired or ((FParent <> nil) and FParent.Cancelled);
end;

procedure TAnalysisBudget.Admit(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if GetCancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal analysis work cancelled or retired');
  end;

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 0) then
  begin
    raise EArgumentException.Create('Pascal analysis needs a known phase and nonnegative work');
  end;

  if AUnits > FData.FMaximum - FData.FTotal then
  begin
    raise ENyxPascalAnalysisWorkLimit.Create(APhase, FData.FTotal, AUnits, FData.FMaximum);
  end;
  NyxPascalAdmitAnalysis(FParent, APhase, AUnits);
  Inc(FData.FTotal, AUnits);
  Inc(FData.FCounts[APhase], AUnits);
end;

function TAnalysisBudget.Receipt: TNyxPascalAnalysisReceipt;
begin
  Result := FData;
end;

procedure TAnalysisBudget.Retire;
begin
  FRetired := True;
  FParent := nil;
end;

function NewNyxPascalAnalysisBudget(AMaximum: Integer;
  const AParent: INyxCancellationScope): INyxPascalAnalysisBudget;
begin
  Result := TAnalysisBudget.Create(AMaximum, AParent);
end;

procedure NyxPascalAdmitAnalysis(const AScope: INyxCancellationScope;
  APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
var
  LBudget: INyxPascalAnalysisBudget;
begin

  if Supports(AScope, INyxPascalAnalysisBudget, LBudget) then
  begin
    LBudget.Admit(APhase, AUnits);
  end;
end;

function NyxPascalAnalysisPhaseName(APhase: TNyxPascalAnalysisPhase): String;
const
  CNames: array[TNyxPascalAnalysisPhase] of String = ('source-token', 'frozen-row',
    'projection-token', 'lexical-token', 'lexical-row', 'filter-token', 'filter-row',
    'syntax-token', 'metadata-value', 'metadata-node', 'diagnostic-row', 'type-token',
    'type-node', 'type-proof', 'type-memo', 'world-entry', 'world-token', 'binding-step',
    'condition-input', 'constant-plan', 'constant-step');
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) then
  begin
    raise EArgumentException.Create('Unknown Pascal analysis work phase');
  end;
  Result := CNames[APhase];
end;

end.
