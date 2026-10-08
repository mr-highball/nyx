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

unit nyx.studio.transactions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.data, nyx.studio.projects, nyx.studio.edits,
  nyx.studio.stateedits, nyx.studio.collectionedits, nyx.studio.resourceedits;

const
  { Count every design operation and every nested data change. Domain wrappers
    never multiply the original transaction's admission budget. }
  NyxMaximumTransactionChanges = 64;

type
  { Closed domains describe ordered authoring steps, never arbitrary dispatch
    names. A step owns only copied immutable wire values, with no document,
    catalog, widget or COM-interface fields (also valid under pas2js). }
  TNyxTransactionDomain = (ntdDesign, ntdState, ntdCollections, ntdResources);

  TNyxTransactionStep = record
  private
    FDefined: Boolean;
    FDomain: TNyxTransactionDomain;
    FChanges: TNyxDataValue;
  public
    property Defined: Boolean read FDefined;
    property Domain: TNyxTransactionDomain read FDomain;
  end;

  { Owns independent, ordered steps. Candidate stages the whole paired project
    without touching its input. Each ordinary domain group must admit before
    the next runs; dependencies therefore follow their definitions. A late
    failure releases all temporary owners and produces no active publication.
    Callers publish the final pair once for one design/Pascal Undo checkpoint.
    Source drafts refuse; handwritten helpers survive ordinary source merging. }
  INyxProjectTransaction = interface
    ['{7BA3D279-E9C6-4FB2-A9A5-6D56860364A8}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
    { Design-only calls can retain the ordinary direct tree/source candidate
      path without decoding an extra paired session. Mixed domains return False
      and nil; they must use the complete paired candidate. }
    function TryDesignPatch(out APatch: INyxDesignPatch): Boolean;
    property Count: Integer read GetCount;
  end;

{ Normalize specialized typed patches to independent value snapshots. Nil
  patches refuse before allocating a transaction. Design steps can surround
  data steps; the transaction does not impose a layout-first ordering. }
function NyxDesignStep(const APatch: INyxDesignPatch): TNyxTransactionStep;
function NyxStateStep(const APatch: INyxStateBindingPatch): TNyxTransactionStep;
function NyxCollectionStep(const APatch: INyxCollectionPatch): TNyxTransactionStep;
function NyxResourceStep(const APatch: INyxResourcePatch): TNyxTransactionStep;

{ Requires constructed steps and 1..64 total leaf changes, including nested
  state/collection/resource groups. Each data group retains its own 1..32 limit. }
function NyxProjectTransaction(const ASteps: array of TNyxTransactionStep):
  INyxProjectTransaction;

{ Strict external boundary: existing design operations stay flat; op state or
  collections/resources carries exactly changes, using its strict typed decoder.
  Consecutive design operations retain ordinary grouped admission. No nested
  transaction, source editing, permission change or root-review bypass exists. }
function ReadNyxProjectTransaction(const AOperations: TNyxDataValue):
  INyxProjectTransaction;

{ Extend the existing design array schema with the same state/collection change
  schemas. Promote recursive query definitions to the tool schema root so their
  local JSON pointers resolve correctly. Runtime also enforces the total budget. }
function NyxTransactionAgentSchema(const ADesignOperations: TNyxDataValue): TNyxDataValue;

implementation

uses
  nyx.text, nyx.model, nyx.studio.session;

type
  TProjectTransaction = class(TInterfacedObject, INyxProjectTransaction)
  private
    FSteps: array of TNyxTransactionStep;
    FCount: Integer;
  public
    constructor Create(const ASteps: array of TNyxTransactionStep);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
    function TryDesignPatch(out APatch: INyxDesignPatch): Boolean;
  end;

function NyxDesignStep(const APatch: INyxDesignPatch): TNyxTransactionStep;
begin

  if APatch = nil then
  begin
    raise ENyxModel.Create('Construct the design patch before composing a transaction');
  end;
  Result := Default(TNyxTransactionStep);
  Result.FDomain := ntdDesign;
  Result.FChanges := NyxDesignChanges(ReadNyxDesignPatch(
    NyxDesignChanges(APatch).ToData)).ToData;
  Result.FDefined := True;
end;

function NyxStateStep(const APatch: INyxStateBindingPatch): TNyxTransactionStep;
begin

  if APatch = nil then
  begin
    raise ENyxModel.Create('Construct the state patch before composing a transaction');
  end;
  Result := Default(TNyxTransactionStep);
  Result.FDomain := ntdState;
  Result.FChanges := ReadNyxStateBindingPatch(APatch.ToData).ToData;
  Result.FDefined := True;
end;

function NyxCollectionStep(const APatch: INyxCollectionPatch): TNyxTransactionStep;
begin

  if APatch = nil then
  begin
    raise ENyxModel.Create('Construct the collection patch before composing a transaction');
  end;
  Result := Default(TNyxTransactionStep);
  Result.FDomain := ntdCollections;
  Result.FChanges := ReadNyxCollectionPatch(APatch.ToData).ToData;
  Result.FDefined := True;
end;

function NyxResourceStep(const APatch: INyxResourcePatch): TNyxTransactionStep;
begin

  if APatch = nil then
  begin
    raise ENyxModel.Create('Construct the resource patch before composing a transaction');
  end;
  Result := Default(TNyxTransactionStep);
  Result.FDomain := ntdResources;
  Result.FChanges := ReadNyxResourcePatch(APatch.ToData).ToData;
  Result.FDefined := True;
end;

constructor TProjectTransaction.Create(const ASteps: array of TNyxTransactionStep);
var
  LIndex: Integer;
  LLast: Integer;
  LChange: Integer;
  LPrevious: Integer;
  LJoined: array of TNyxDataValue;
begin
  inherited Create;

  if (Length(ASteps) < 1) or (Length(ASteps) > NyxMaximumTransactionChanges) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 total changes');
  end;
  for LIndex := 0 to High(ASteps) do
  begin

    if not ASteps[LIndex].Defined then
    begin
      raise ENyxModel.Create('Every transaction step must be constructed');
    end;
    Inc(FCount, ASteps[LIndex].FChanges.Count);

    if FCount > NyxMaximumTransactionChanges then
    begin
      raise ENyxModel.Create('A transaction requires 1..64 total changes, including data groups');
    end;
    LLast := High(FSteps);

    if (LLast >= 0) and (ASteps[LIndex].Domain = ntdDesign) and
      (FSteps[LLast].Domain = ntdDesign) then
    begin
      { Flat wire operations share ordinary design admission. Normalize adjacent
        typed steps identically, including forward references within that group. }
      LPrevious := FSteps[LLast].FChanges.Count;
      SetLength(LJoined, LPrevious + ASteps[LIndex].FChanges.Count);
      for LChange := 0 to LPrevious - 1 do
      begin
        LJoined[LChange] := FSteps[LLast].FChanges.Item(LChange);
      end;
      for LChange := 0 to ASteps[LIndex].FChanges.Count - 1 do
      begin
        LJoined[LPrevious + LChange] := ASteps[LIndex].FChanges.Item(LChange);
      end;
      FSteps[LLast].FChanges := NyxArray(LJoined);
    end
    else
    begin
      SetLength(FSteps, Length(FSteps) + 1);
      FSteps[High(FSteps)].FDefined := True;
      FSteps[High(FSteps)].FDomain := ASteps[LIndex].Domain;
      FSteps[High(FSteps)].FChanges := ASteps[LIndex].FChanges.Copy;
    end;
  end;
end;

function NyxProjectTransaction(const ASteps: array of TNyxTransactionStep):
  INyxProjectTransaction;
begin
  Result := TProjectTransaction.Create(ASteps);
end;

function TProjectTransaction.GetCount: Integer;
begin
  Result := FCount;
end;

function TProjectTransaction.TryDesignPatch(out APatch: INyxDesignPatch): Boolean;
begin
  APatch := nil;
  Result := (Length(FSteps) = 1) and (FSteps[0].Domain = ntdDesign);

  if Result then
  begin
    APatch := ReadNyxDesignPatch(FSteps[0].FChanges);
  end;
end;

function TProjectTransaction.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LIndex: Integer;
  LSession: TNyxStudioSession;
begin

  if APair.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before applying an agent transaction');
  end;
  Result := APair;
  for LIndex := 0 to High(FSteps) do
  begin
    case FSteps[LIndex].Domain of
      ntdDesign:
        begin
          LSession := TNyxStudioSession.Create(Result);
          try
            LSession.ApplyPatch(ReadNyxDesignPatch(FSteps[LIndex].FChanges));
            Result := LSession.ProjectSnapshot;
          finally
            LSession.Free;
          end;
        end;
      ntdState:
        begin
          Result := ReadNyxStateBindingPatch(FSteps[LIndex].FChanges).Candidate(Result);
        end;
      ntdCollections:
        begin
          Result := ReadNyxCollectionPatch(FSteps[LIndex].FChanges).Candidate(Result);
        end;
      ntdResources:
        begin
          LSession := TNyxStudioSession.Create(Result);
          try
            LSession.ApplyPatch(ReadNyxResourcePatch(FSteps[LIndex].FChanges));
            Result := LSession.ProjectSnapshot;
          finally
            LSession.Free;
          end;
        end;
    end;
  end;
end;

function TProjectTransaction.ToData: TNyxDataValue;
const
  CNames: array[TNyxTransactionDomain] of TNyxText = ('', 'state', 'collections', 'resources');
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
  LChange: Integer;
  LCount: Integer;
begin
  SetLength(LValues, FCount);
  LCount := 0;
  for LIndex := 0 to High(FSteps) do
  begin

    if FSteps[LIndex].Domain = ntdDesign then
    begin
      for LChange := 0 to FSteps[LIndex].FChanges.Count - 1 do
      begin
        LValues[LCount] := FSteps[LIndex].FChanges.Item(LChange);
        Inc(LCount);
      end;
    end
    else
    begin
      LValues[LCount] := NyxObject([
        NyxField('op', NyxData(CNames[FSteps[LIndex].Domain])),
        NyxField('changes', FSteps[LIndex].FChanges)]);
      Inc(LCount);
    end;
  end;
  SetLength(LValues, LCount);
  Result := NyxArray(LValues);
end;

function ReadNyxProjectTransaction(const AOperations: TNyxDataValue):
  INyxProjectTransaction;
var
  LSteps: array of TNyxTransactionStep;
  LDesign: array of TNyxDataValue;
  LWire: TNyxDataValue;
  LName: TNyxText;
  LIndex: Integer;

  procedure AddStep(const AStep: TNyxTransactionStep);
  begin
    SetLength(LSteps, Length(LSteps) + 1);
    LSteps[High(LSteps)].FDefined := AStep.Defined;
    LSteps[High(LSteps)].FDomain := AStep.Domain;
    LSteps[High(LSteps)].FChanges := AStep.FChanges.Copy;
  end;

  procedure FlushDesign;
  begin

    if Length(LDesign) > 0 then
    begin
      AddStep(NyxDesignStep(ReadNyxDesignPatch(NyxArray(LDesign))));
      LDesign := nil;
    end;
  end;

begin

  if (AOperations.Kind <> ndArray) or (AOperations.Count < 1) or
    (AOperations.Count > NyxMaximumTransactionChanges) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 total changes');
  end;
  LSteps := nil;
  LDesign := nil;
  for LIndex := 0 to AOperations.Count - 1 do
  begin
    LWire := AOperations.Item(LIndex);
    LName := LWire.Field('op').AsText;

    if (LName = 'state') or (LName = 'collections') or (LName = 'resources') then
    begin

      if (LWire.Kind <> ndObject) or (LWire.Count <> 2) then
      begin
        raise ENyxModel.Create('A data transaction group requires exactly op and changes');
      end;
      FlushDesign;

      if LName = 'state' then
      begin
        AddStep(NyxStateStep(ReadNyxStateBindingPatch(LWire.Field('changes'))));
      end
      else if LName = 'collections' then
      begin
        AddStep(NyxCollectionStep(ReadNyxCollectionPatch(LWire.Field('changes'))));
      end
      else
      begin
        AddStep(NyxResourceStep(ReadNyxResourcePatch(LWire.Field('changes'))));
      end;
    end
    else
    begin
      SetLength(LDesign, Length(LDesign) + 1);
      LDesign[High(LDesign)] := LWire;
    end;
  end;
  FlushDesign;
  Result := NyxProjectTransaction(LSteps);
end;

function NyxTransactionAgentSchema(const ADesignOperations: TNyxDataValue): TNyxDataValue;
var
  LCollection: TNyxDataValue;
  LAlternatives: TNyxDataValue;
  LBranches: array of TNyxDataValue;
  LIndex: Integer;

  function Changes(const ASchema: TNyxDataValue): TNyxDataValue;
  var
    LOptions: TNyxDataValue;
    LOption: TNyxDataValue;
    LChoice: Integer;
  begin
    LOptions := ASchema.Field('oneOf');
    for LChoice := 0 to LOptions.Count - 1 do
    begin
      LOption := LOptions.Item(LChoice).Field('properties');

      if LOption.Field('mode').Field('const').AsText = 'apply' then
      begin
        Exit(LOption.Field('changes'));
      end;
    end;
    raise ENyxModel.Create('The domain schema must advertise its ordinary apply changes');
  end;

  function Group(const AName: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('type', NyxData('object')),
      NyxField('properties', NyxObject([
        NyxField('op', NyxObject([NyxField('const', NyxData(AName))])),
        NyxField('changes', AChanges)])),
      NyxField('required', NyxArray([NyxData('op'), NyxData('changes')])),
      NyxField('additionalProperties', NyxData(False))]);
  end;

begin
  LCollection := NyxCollectionAgentSchema;
  LAlternatives := ADesignOperations.Field('items').Field('oneOf');
  SetLength(LBranches, LAlternatives.Count + 3);
  for LIndex := 0 to LAlternatives.Count - 1 do
  begin
    LBranches[LIndex] := LAlternatives.Item(LIndex);
  end;
  LBranches[LAlternatives.Count] := Group('state', Changes(NyxStateAgentSchema));
  LBranches[LAlternatives.Count + 1] := Group('collections', Changes(LCollection));
  LBranches[LAlternatives.Count + 2] := Group('resources', Changes(NyxResourceAgentSchema));
  Result := NyxObject([NyxField('type', NyxData('object')),
    NyxField('$defs', LCollection.Field('$defs')),
    NyxField('properties', NyxObject([
      NyxField('expectedRevision', NyxObject([NyxField('type', NyxData('integer')),
        NyxField('minimum', NyxData(1)), NyxField('maximum', NyxData(High(Integer)))])),
      NyxField('operationId', NyxObject([NyxField('type', NyxData('string')),
        NyxField('minLength', NyxData(1)), NyxField('maxLength', NyxData(120))])),
      NyxField('operations', NyxObject([NyxField('type', NyxData('array')),
        NyxField('minItems', NyxData(1)),
        NyxField('maxItems', NyxData(NyxMaximumTransactionChanges)),
        NyxField('items', NyxObject([NyxField('oneOf', NyxArray(LBranches))]))]))])),
    NyxField('required', NyxArray([NyxData('expectedRevision'),
      NyxData('operationId'), NyxData('operations')])),
    NyxField('additionalProperties', NyxData(False))]);
end;

end.
