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
unit nyx.test.contract;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.model;

{ These fixtures are shared by native/pas2js and compiled reconstruction.
  Runtime journeys separately prove the same declarations on physical controls. }
procedure AddNyxContractFixture(ADocument: TNyxDocument);
function RunNyxContractTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.state,
  nyx.data,
  nyx.contract,
  nyx.catalog,
  nyx.schema,
  nyx.binding,
  nyx.binding.types,
  nyx.behavior,
  nyx.composition,
  nyx.codec,
  nyx.codegen,
  nyx.studio.session,
  nyx.studio.view;

procedure AddNyxContractFixture(ADocument: TNyxDocument);
var
  LRoot: TNyxNode;
  LRatioInput: TNyxNode;
  LCommitButton: TNyxNode;
  LCatalog: TNyxCatalog;
begin
  LCatalog := TNyxCatalog.Create;
  try
    ADocument.Pages[0].Add(LCatalog.NewNode(nkRating, 'fixture-rating'));
  finally
    LCatalog.Free;
  end;

  LRoot := TNyxNode.Create(nkColumn, 'fixture-ratio');
  ADocument.Pages[0].Add(LRoot);
  LRoot.Configure.Compound(True).Done;
  LRoot.Contract
    .Value(NyxNumberDomain.Range(0, 1).Choices([0.125, 0.5]))
    .Field(NyxPart('ratio'), NyxNumberDomain.Range(0, 1).Choices([0.125, 0.5]));
  LRoot.Configure.Value(0.5).Done;

  LRatioInput := TNyxNode.Create(nkInput, 'fixture-ratio-input');
  LRoot.Add(LRatioInput);
  LRatioInput.Configure.PartName(NyxPart('ratio')).InputType(niNumber).Value(0.125).Done;

  LCommitButton := TNyxNode.Create(nkButton, 'fixture-ratio-commit');
  LRoot.Add(LCommitButton);
  LCommitButton.Configure.PartName(NyxPart('commit')).Text('Apply ratio')
    .OnClick(NyxEvent('ratio/committed/🌙')).Done;
  LCommitButton.Contract
    .On(ntClick, NyxPartValue(NyxPart('ratio')), NyxNumberDomain.Range(0, 1));

  LRoot := TNyxNode.Create(nkColumn, 'fixture-integer-limit');
  ADocument.Pages[0].Add(LRoot);
  LRoot.Contract.Value(NyxIntegerDomain).Field(NyxPart('amount'), NyxIntegerDomain);
  LRoot.Configure.Compound(True).Value(High(Integer)).Done;
  LRatioInput := TNyxNode.Create(nkInput, 'fixture-integer-input');
  LRoot.Add(LRatioInput);
  LRatioInput.Configure.PartName(NyxPart('amount')).InputType(niNumber).Value(High(Integer)).Done;

  { This valid imported descriptor has deliberate member order, empty fields
    and noncanonical decimal spelling. Source must retain it at the explicit
    descriptor boundary, while normal authored contracts use fluent builders. }
  LRoot := TNyxNode.Create(nkColumn, 'fixture-imported-contract');
  ADocument.Pages[0].Add(LRoot);
  LRoot.Contract.Metadata(TNyxDataValue.ParseJSON(
    '{"fields":[],"version":1,"value":{"max":1.0,"min":0.0,"type":"number"}}'));
  LRoot.SetProp('value', '1.00');
end;

function RunNyxContractTests: Integer;
var
  LDomain: TNyxValueDomain;
  LBaselineDomain: TNyxNumberDomain;
  LTextChoices: array of TNyxText;
  LTextDomain: TNyxTextDomain;
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LDispatch: TNyxDispatch;
  LRetained: TNyxEventInfo;
  LNode: TNyxNode;
  LCatalog: TNyxCatalog;
  LSession: TNyxStudioSession;
  LBefore: TNyxText;
  LSource: TNyxText;
  LRejected: Boolean;
  LRevision: Integer;
  LWire: TNyxText;
  LIndex: Integer;
  LBadDomains: array[0..9] of TNyxText;
  LBadContracts: array[0..8] of TNyxText;
  LDefaultDomain: TNyxValueDomain;
  LDefinition: TNyxNode;
  LInstance: TNyxNode;
  LSibling: TNyxNode;
  LRealized: TNyxNode;
  LItems: array of TNyxDataValue;
  LShell: TNyxDocument;
  LShellState: TNyxStudioViewState;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxContract.Create('Contract fixture: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure RejectWire(const ADomain: TNyxValueDomain; const AWire: TNyxText);
  begin
    LRejected := False;
    try
      ADomain.ReadWire(AWire);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'invalid scalar wire must be refused');
  end;

begin
  Result := 0;
  LRejected := False;
  try
    LDefaultDomain.Validate;
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'default managed domain record is not an admitted declaration');
  LDomain := NyxNoDomain;
  Check(not LDomain.Defined, 'absence is explicit');
  LRejected := False;
  try
    LDomain.ReadWire('guess');
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'absence does not guess text');
  SetLength(LTextChoices, 2);
  LTextChoices[0] := 'Café / 🌙';
  LTextChoices[1] := '漢字';
  LTextDomain := NyxTextDomain.Choices(LTextChoices);
  LTextChoices[0] := 'caller edit';
  Check(LTextDomain.Definition.ReadWire('Café / 🌙').AsText = TNyxText('Café / 🌙'),
    'choice builders retain independent Unicode snapshots');
  RejectWire(LTextDomain.Definition, 'caller edit');
  Check(not NyxBooleanDomain.Definition.ReadWire('false').AsBoolean,
    'Boolean domains preserve false');
  RejectWire(NyxBooleanDomain.Definition, 'False');
  Check(NyxIntegerDomain.Range(Low(Integer), High(Integer)).Definition
    .ReadWire('-2147483648').AsInteger = Low(Integer), 'complete signed integer boundary');
  RejectWire(NyxIntegerDomain.Definition, '2147483648');
  RejectWire(NyxIntegerDomain.Definition, '1.0');
  RejectWire(NyxIntegerDomain.Definition, '$10');
  LBaselineDomain := NyxNumberDomain;
  LDomain := LBaselineDomain.Range(0, 1).Choices([0.1, 0.5]).Definition;
  Check(LDomain.ReadWire('1e-1').AsNumber = Double(0.1),
    'numeric choice membership compares values rather than decimal spelling');
  RejectWire(LDomain, '0.2');
  RejectWire(LDomain, '2');
  RejectWire(LDomain, 'NaN');
  Check(LBaselineDomain.Definition.ReadWire('2').AsNumber = 2,
    'fluent narrowing retains its baseline');
  SetLength(LItems, NyxMaximumDomainChoices + 1);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxData(LIndex);
  end;
  LRejected := False;
  try
    TNyxValueDomain.FromData(NyxObject([
      NyxField('type', NyxData('integer')), NyxField('choices', NyxArray(LItems))
    ]));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'domain choice budget is enforced before publication');

  LBadDomains[0] := '{"type":"text","min":0,"max":1}';
  LBadDomains[1] := '{"type":"number","min":0}';
  LBadDomains[2] := '{"type":"integer","min":1,"max":0}';
  LBadDomains[3] := '{"type":"integer","min":0.5,"max":1}';
  LBadDomains[4] := '{"type":"number","choices":[1,1.0]}';
  LBadDomains[5] := '{"type":"boolean","choices":["true"]}';
  LBadDomains[6] := '{"type":"text","choices":[]}';
  LBadDomains[7] := '{"type":"number","choices":[null]}';
  LBadDomains[8] := '{"type":"guess"}';
  LBadDomains[9] := '{"type":"text","|":true}';
  for LIndex := 0 to High(LBadDomains) do
  begin
    LRejected := False;
    try
      TNyxValueDomain.FromData(TNyxDataValue.ParseJSON(LBadDomains[LIndex]));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'invalid domain descriptor must be refused');
  end;

  LDocument := TNyxDocument.Create;
  LCopy := nil;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LCatalog := TNyxCatalog.Create;
  LSession := TNyxStudioSession.Create;
  try
    LDocument.AddPage(TNyxNode.Create(nkPage, 'home'));
    AddNyxContractFixture(LDocument);
    LNode := LCatalog.NewNode(nkSearchField, 'search');
    LDocument.Pages[0].Add(LNode);
    LNode.Part(NyxPart('query')).Configure.Value('Exact / 🌙').Done;
    Check(NyxBindingKinds(LNode, bpValue) = [],
      'compound container does not expose an invented self value');
    Check(NyxBindingKinds(LNode.Part(NyxPart('query')), bpValue) = [nskText],
      'named fields expose exact binding kinds');
    Check(NyxBindingKinds(LDocument.Find('fixture-rating'), bpValue) = [nskInteger],
      'rating binding is an integer contract');
    LNode := TNyxNode.Create(nkColumn, 'untyped-container');
    LDocument.Pages[0].Add(LNode);
    Check(NyxBindingKinds(LNode, bpValue) = [], 'ordinary container has no guessed value');
    LNode.Contract.Value(NyxIntegerDomain.Range(1, 5));
    Check(NyxBindingKinds(LNode, bpValue) = [nskInteger], 'local declaration exposes its exact family');
    LNode.Extensions.SetValue(NyxExtension(NyxContractKey), NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('value', NyxBooleanDomain.Definition.ToData)
    ]));
    Check(NyxBindingKinds(LNode, bpValue) = [nskBoolean],
      'read cache observes metadata edits made through the extension boundary');
    LNode.Extensions.Remove(NyxExtension(NyxContractKey));
    Check(NyxBindingKinds(LNode, bpValue) = [], 'read cache observes removed declarations');

    ValidateNyxDocumentProperties(LDocument);
    LWire := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LCopy) = LWire, 'versioned declarations round-trip exactly');
    Check(LCopy.Find('fixture-ratio').Contract.Snapshot.ToJSON =
      LDocument.Find('fixture-ratio').Contract.Snapshot.ToJSON, 'clones retain declarations');
    LCopy.Find('fixture-ratio').Contract.Value(NyxNumberDomain.Range(0, 2));
    Check(LCopy.Find('fixture-ratio').Contract.Snapshot.ToJSON <>
      LDocument.Find('fixture-ratio').Contract.Snapshot.ToJSON, 'local declarations are independent');
    LCopy.Find('fixture-ratio').Contract.Assign(LDocument.Find('fixture-ratio').Contract);
    Check(TNyxCodec.Encode(LCopy) = LWire, 'explicit assignment restores a complete owned contract');
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('.Value(NyxIntegerDomain.Range(1, 5))', LSource) > 0,
      'generated selections use typed fluent domains');
    Check((Pos('LSearchQueryInput: INyxInput;', LSource) > 0) and
      (Pos('LFixtureRatingStar5Button: INyxButton;', LSource) > 0),
      'generated compound locals use named purposes rather than factory serials');
    Check(Pos('.Field(NyxPart(''ratio''), NyxNumberDomain.Range(0, 1).Choices([0.125, 0.5]))',
      LSource) > 0, 'generated named fields are crafted declarations');
    Check(Pos('.On(ntClick, NyxPartValue(NyxPart(''ratio'')), NyxNumberDomain.Range(0, 1))',
      LSource) > 0, 'generated event payloads name their field');
    Check(Pos('.Metadata(', LSource) > 0, 'noncanonical imported metadata remains explicit');
    Check((Pos('.Value(High(Integer))', LSource) > 0) and
      (Pos('.Metadata(atValue, ''1.00'')', LSource) > 0),
      'crafted limit expressions and explicit numeric spelling boundaries remain exact');

    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;
    LDispatch := LLive.Dispatch(LRoot.Find('fixture-ratio-commit'), ntClick);
    Check((LDispatch.Info.SourceID = 'fixture-ratio') and
      (LDispatch.Info.OriginID = 'fixture-ratio-commit') and
      (LDispatch.Info.TargetID = 'fixture-ratio-commit') and
      (LDispatch.Info.ValueID = 'fixture-ratio-input'), 'payload identity is separate from action target');
    Check(LDispatch.Info.HasValue and (LDispatch.Info.ValueKind = nskNumber) and
      (LDispatch.Info.Value.AsNumber = 0.125), 'declared numeric event carries its named field');
    LRetained := LDispatch.Info.Copy;
    LDispatch := LLive.Dispatch(LRoot.Find('search').Part(NyxPart('search')), ntClick);
    Check((LDispatch.Info.ValueKind = nskText) and
      (LDispatch.Info.Value.AsText = TNyxText('Exact / 🌙')) and
      (LDispatch.Info.ValueID = LRoot.Find('search').Part(NyxPart('query')).ID),
      'default search action carries exact query text');
    LDispatch := LLive.Dispatch(LRoot.Find('fixture-rating').Part(NyxPart('star-5')), ntClick);
    Check((LDispatch.Info.ValueKind = nskInteger) and (LDispatch.Info.Value.AsInteger = 5),
      'selection action preserves the declared integer family');
    LRoot.Find('fixture-rating').Part(NyxPart('star-5')).Configure.Metadata(atOption, '05').Done;
    LDispatch := LLive.Dispatch(LRoot.Find('fixture-rating').Part(NyxPart('star-5')), ntClick);
    Check((LDispatch.Info.Value.AsInteger = 5) and
      (LRoot.Find('fixture-rating').Part(NyxPart('star-5')).Prop('pressed') = 'true'),
      'numeric selection presentation compares scalar meaning rather than wire spelling');
    LRevision := LStore.Revision;
    LBefore := LRoot.Find('fixture-ratio-input').Prop('value');
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('fixture-ratio-input'), '0.2');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision) and
      (LRoot.Find('fixture-ratio-input').Prop('value') = LBefore),
      'range/choice rejection retains state and realized controls');
    LRoot.Find('fixture-ratio-commit').Contract.Signal(ntClick);
    LDispatch := LLive.Dispatch(LRoot.Find('fixture-ratio-commit'), ntClick);
    Check(not LDispatch.Info.HasValue and (LDispatch.Info.Value.Kind = ndNull) and
      (LDispatch.Info.ValueID = ''), 'explicit signal has no inferred payload');
    Check((LRetained.Value.AsNumber = 0.125) and (LRetained.ValueID = 'fixture-ratio-input'),
      'retained declared event stays independent of later declarations');

    LSession.Load(LWire);
    LShellState := DefaultNyxStudioViewState;
    LSession.Select('fixture-rating');
    LShell := BuildNyxStudioView(LSession, LShellState);
    try
      ValidateNyxDocumentProperties(LShell);
      Check((LShell.Find('inspector-value').Kind = 'spin') and
        (LShell.Find('inspector-value').Prop('min') = '1') and
        (LShell.Find('inspector-value').Prop('max') = '5'),
        'rating inspector uses its declared integer range');
    finally
      LShell.Free;
    end;
    LSession.Select('fixture-integer-limit');
    LShell := BuildNyxStudioView(LSession, LShellState);
    try
      ValidateNyxDocumentProperties(LShell);
      Check((LShell.Find('inspector-value').Kind = 'input') and
        (NyxNodeValueDomain(LShell.Find('inspector-value')).Kind = nskInteger) and
        (LShell.Find('inspector-value').Prop('value') = '2147483647'),
        'full signed integer inspector keeps its type without narrowing to a spinner');
    finally
      LShell.Free;
    end;
    LSession.Select('fixture-ratio-input');
    LBefore := LSession.Save;
    LRejected := False;
    try
      LSession.SetProperty('value', '0.2');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore),
      'invalid designer value preserves the accepted document');
    LSession.SetProperty('value', '0.5');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'domain declarations survive designer history');
    LSession.Redo;
    Check(LSession.Document.Find('fixture-ratio-input').Prop('value') = '0.5',
      'redo preserves admitted typed fields');

    LCopy.Find('fixture-ratio').Contract.Field(NyxPart('ratio'), NyxBooleanDomain);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LCopy);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'declared field cannot change the physical control scalar family');
    LRejected := False;
    try
      LSession.Load(TNyxCodec.Encode(LCopy));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document.Find('fixture-ratio-input').Prop('value') = '0.5'),
      'invalid imported field preserves the accepted design');
    LCopy.Find('fixture-ratio').Contract.Assign(LDocument.Find('fixture-ratio').Contract);
    LCopy.Find('fixture-ratio').Contract.Field(NyxPart('missing'), NyxTextDomain);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LCopy);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'final realized declaration requires its named part');

    { Both registry instantiation and reusable overlays must carry independently
      owned root contracts and bindings, not just properties/descendants. }
    LDocument.State.SetValue(NyxIntegerState('meter'), 2);
    LDefinition := TNyxNode.Create(nkColumn, 'meter-definition');
    LDocument.AddComponent(LDefinition);
    LDefinition.Contract.Value(NyxIntegerDomain.Range(1, 5));
    LDefinition.Configure.Compound(True).Value(2).Done;
    LDefinition.Binds.Value(NyxIntegerState('meter')).Done;
    LCatalog.RegisterRecipe(NyxCustomKind('meter'), 'Meter', 'Custom', LDefinition);
    LInstance := LCatalog.NewNode(NyxCustomKind('meter'), 'catalog-meter');
    LDocument.Pages[0].Add(LInstance);
    Check((LInstance.BindingCount = 1) and
      (NyxNodeValueDomain(LInstance).ReadWire('5').AsInteger = 5),
      'registry instantiation retains root contracts and binding descriptors');
    LInstance.Contract.Value(NyxIntegerDomain.Range(1, 3));
    Check(NyxNodeValueDomain(LDefinition).ReadWire('5').AsInteger = 5,
      'registry instance customization retains the template');
    LInstance := TNyxNode.Create(nkComponent, 'meter-instance');
    LDocument.Pages[0].Add(LInstance);
    LInstance.Configure.Component(NyxComponent('meter-definition')).Done;
    LInstance.Contract.Value(NyxIntegerDomain.Range(1, 3));
    LSibling := TNyxNode.Create(nkComponent, 'meter-sibling');
    LDocument.Pages[0].Add(LSibling);
    LSibling.Configure.Component(NyxComponent('meter-definition')).Done;
    LRealized := RealizeNyxView(LDocument, LInstance);
    try
      Check((LRealized.BindingCount = 1) and
        (NyxNodeValueDomain(LRealized).ReadWire('3').AsInteger = 3),
        'reusable instance carries its overridden declaration and inherited binding');
      RejectWire(NyxNodeValueDomain(LRealized), '5');
    finally
      LRealized.Free;
    end;
    LRealized := RealizeNyxView(LDocument, LSibling);
    try
      Check(NyxNodeValueDomain(LRealized).ReadWire('5').AsInteger = 5,
        'sibling reusable view retains its inherited declaration');
    finally
      LRealized.Free;
    end;

    LBadContracts[0] := '{"version":2}';
    LBadContracts[1] := '{"version":1,"fields":[{"part":"a","domain":null}]}';
    LBadContracts[2] := '{"version":1,"fields":[{"part":"a//b","domain":{"type":"text"}}]}';
    LBadContracts[3] := '{"version":1,"fields":[{"part":"a","domain":{"type":"text"}},{"part":"a","domain":{"type":"text"}}]}';
    LBadContracts[4] := '{"version":1,"events":[{"trigger":"design-select","source":"none","domain":null}]}';
    LBadContracts[5] := '{"version":1,"events":[{"trigger":"click","source":"none","domain":{"type":"text"}}]}';
    LBadContracts[6] := '{"version":1,"events":[{"trigger":"click","source":"part","domain":{"type":"text"}}]}';
    LBadContracts[7] := '{"version":1,"events":[{"trigger":"click","source":"guess","domain":null}]}';
    LBadContracts[8] := '{"version":1,"events":[{"trigger":"click","source":"none","domain":null},{"trigger":"click","source":"none","domain":null}]}';
    LNode := LDocument.Find('fixture-ratio');
    LBefore := LNode.Contract.Snapshot.ToJSON;
    for LIndex := 0 to High(LBadContracts) do
    begin
      LRejected := False;
      try
        LNode.Contract.Metadata(TNyxDataValue.ParseJSON(LBadContracts[LIndex]));
      except
        on LException: Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LNode.Contract.Snapshot.ToJSON = LBefore),
        'invalid declaration retains its complete baseline');
    end;
  finally
    LSession.Free;
    LCatalog.Free;
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LCopy.Free;
    LDocument.Free;
  end;
  Check((LRetained.Value.AsNumber = 0.125) and (LRetained.SourceID = 'fixture-ratio'),
    'declared event data survives disposal');
end;

end.
