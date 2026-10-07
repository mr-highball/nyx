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

program nyx_agent_transaction_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$else}Classes, nyx.studio.mcp,{$endif}
  nyx.text, nyx.data, nyx.types, nyx.controls, nyx.model, nyx.codec, nyx.codegen, nyx.catalog,
  nyx.state, nyx.binding.types, nyx.collections, nyx.collections.view.types,
  nyx.collections.query, nyx.studio.projects, nyx.studio.agents, nyx.studio.edits,
  nyx.studio.stateedits, nyx.studio.collectionedits, nyx.studio.collectionintent,
  nyx.studio.transactions, nyx.studio.session;

type
  { An external ordinary command can keep the original candidate-only contract.
    It borrows the supplied tree/catalog and returns a completely owned clone. }
  TCandidateOnlyPatch = class(TInterfacedObject, INyxDesignPatch)
  public
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
  end;

var
  LAgent: TNyxAgentSession;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LSeed: TNyxProjectPair;
  LPair: TNyxProjectPair;
  LExport: TNyxProjectPair;
  LTransaction: INyxProjectTransaction;
  LLayout: INyxDesignPatch;
  LData: INyxCollectionPatch;
  LSteps: array of TNyxTransactionStep;
  LOperations: array of TNyxDataValue;
  LArgs: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LSchema: TNyxDataValue;
  LTool: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LIntent: TNyxStudioCollectionIntent;
  LRevision: Integer;
  LCount: Integer;
  LIndex: Integer;
  LRejected: Boolean;
  LInitial: TNyxText;
  LAccepted: TNyxText;
  LText: TNyxText;
  LCustomSession: TNyxStudioSession;

function TCandidateOnlyPatch.Candidate(ADocument: TNyxDocument;
  ACatalog: TNyxCatalog): TNyxDocument;
begin
  Result := ADocument.Clone;
  Result.Title := 'An ordinary custom command';
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Combined transaction: ' + AReason);
  end;
  Inc(LCount);
end;

function PairText: TNyxText;
begin
  Result := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
end;

function Arguments(const AID: TNyxText; const AOperations: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', AOperations)]);
end;

procedure Apply(const AID: TNyxText; const ATransaction: INyxProjectTransaction);
begin
  LAgent.Call('nyx_transaction', 'Scooty',
    Arguments(AID, ATransaction.ToData), 'transaction-owner');
  LRevision := LAgent.Revision;
end;

procedure Refuses(const AArguments: TNyxDataValue; const AReason: TNyxText;
  const AOwner: TNyxText = 'transaction-owner');
var
  LBefore: TNyxText;
  LState: TNyxDataValue;
  LAfter: TNyxDataValue;
  LFailed: Boolean;
begin
  LBefore := PairText;
  LState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
  LFailed := False;
  try
    LAgent.Call('nyx_transaction', 'Scooty', AArguments, AOwner);
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  LAfter := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
  Check(LFailed and (LAgent.Revision = LRevision) and (PairText = LBefore) and
    (LState.Field('canUndo').AsBoolean = LAfter.Field('canUndo').AsBoolean) and
    (LState.Field('canRedo').AsBoolean = LAfter.Field('canRedo').AsBoolean) and
    (LState.Field('selection').AsText = LAfter.Field('selection').AsText) and
    (LState.Field('view').AsText = LAfter.Field('view').AsText), AReason);
end;

procedure History(const ADirection, AID: TNyxText);
begin
  LAgent.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]));
  LRevision := LAgent.Revision;
end;

{$ifndef PAS2JS}
procedure ExportPair;
var
  LDirectory: TNyxText;
  LStream: TFileStream;

  procedure WriteBytes(const AName, AText: TNyxText);
  begin
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;

begin

  if ParamCount = 1 then
  begin
    LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
    ForceDirectories(LDirectory);
    WriteBytes('nyx.generated.view.pas', LExport.Source);
    WriteBytes('design.nyx', LExport.Design);
    WriteBytes('project.nyxpair', EncodeNyxProject(LExport));
  end;
end;
{$endif}

procedure Run;
begin
  LAgent := nil;
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'A paired workspace';
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxLabel('guide').Configure.Text('Keep crafting.').Done);
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LSeed.Source := TNyxText(StringReplace(String(LSeed.Source), 'implementation' + #10,
      'implementation' + #10 + #10 + '{ Handwritten helper retained by every domain. }' + #10 +
      'function WorkspaceNote: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10, []));
  finally
    LDocument.Free;
  end;
  LPage := nil;
  LAgent := TNyxAgentSession.Create(LSeed);
  try
    LRevision := LAgent.Revision;
    LInitial := PairText;
    LText := TNyxText('A calm 🌙 desk');
    LKey := NyxCollection('work-items');
    LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"input","id":"workspace-note","parent":"home"},' +
      '{"op":"create","kind":"table","id":"work-table","parent":"home"},' +
      '{"op":"create","kind":"column","id":"side-stack","parent":"home"}]'));
    LData := NyxCollectionPatch([
      NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('task'), '')
        .Integer(NyxIntegerField('priority'), 1), [
        NyxCollectionItem(NyxItem(LKey, 'plan')).WithValue(NyxTextField('task'), 'Plan the next idea'),
        NyxCollectionItem(NyxItem(LKey, 'build')).WithValue(NyxTextField('task'), 'Build something useful')
          .WithValue(NyxIntegerField('priority'), 3)]),
      NyxBindCollection(NyxBindingOwner('work-table'), cpTable,
        NyxCollectionView(LKey).Column(NyxTextField('task'), 'Task', cmEditable)
          .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
          .Query(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(2))
            .OrderBy(NyxIntegerField('priority'), nsdDescending)))]);
    SetLength(LSteps, 4);
    LSteps[0] := NyxDesignStep(LLayout);
    LSteps[1] := NyxStateStep(NyxStateBindingPatch([
      NyxCreateDefault(NyxStateValue(NyxTextState('note'), LText)),
      NyxCreateDefault(NyxStateValue(NyxTextState('raw-note'), LText + TNyxText(#0))),
      NyxBindControl(NyxBindingOwner('workspace-note'), bpValue, NyxTextState('note'), bdTwoWay)]));
    LSteps[2] := NyxCollectionStep(LData);
    LSteps[3] := NyxDesignStep(NyxPlacementPatch([
      NyxPlaceControl(NyxControl('workspace-note'), NyxControl('side-stack'), nplInside)]));
    LTransaction := NyxProjectTransaction(LSteps);
    Check(LTransaction.Count = 9, 'total counts nested leaf changes');
    LSteps[0] := Default(TNyxTransactionStep);
    LLayout := nil;
    LData := nil;
    Check(ReadNyxProjectTransaction(LTransaction.ToData).Candidate(LSeed).Source =
      LTransaction.Candidate(LSeed).Source, 'copied typed and wire stages admit the same crafted source');
    Check(EncodeNyxProject(LSeed) = LInitial, 'candidate admission retains its exact borrowed pair');
    LArgs := Arguments('compose', LTransaction.ToData);
    LReceipt := LAgent.Call('nyx_transaction', 'Scooty', LArgs, 'transaction-owner');
    LRevision := LAgent.Revision;
    Check(LRevision = LArgs.Field('expectedRevision').AsInteger + 1, 'one monotonic revision for all domains');
    LAccepted := PairText;
    LExport := LAgent.PreviewPair(LRevision, 'home');
    LLayout := TCandidateOnlyPatch.Create;
    LCustomSession := TNyxStudioSession.Create(LExport);
    try
      LCustomSession.ApplyPatch(LLayout);
      Check(LCustomSession.Document.Title = 'An ordinary custom command',
        'candidate-only custom commands retain the original ordinary interface');
    finally
      LCustomSession.Free;
    end;
    LRejected := False;
    try
      NyxDesignStep(LLayout);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'opaque custom commands must opt into semantic snapshot composition');
    LLayout := NyxReusablePatch([
      NyxDeriveComponent(NyxControl('side-stack'), NyxComponent('note-card'), [
        NyxIdentity(NyxControl('workspace-note'), NyxControl('template-note'))]),
      NyxInstantiateComponent(NyxComponent('note-card'), NyxControl('first-note'), NyxControl('home')),
      NyxOverrideComponentPart(NyxControl('first-note'), NyxControl('first-properties'), NyxPart('.'), noProperties),
      NyxInheritComponentPart(NyxControl('first-note'), NyxControl('first-properties'), NyxPart('.'))]);
    LTransaction := NyxProjectTransaction([NyxDesignStep(LLayout)]);
    Check(NyxDesignChanges(LLayout).Count = 4, 'specialized reusable patches expose their full ordered count');
    Check(LTransaction.Candidate(LExport).Source =
      ReadNyxProjectTransaction(LTransaction.ToData).Candidate(LExport).Source,
      'typed derive/instance/override/inherit retain exact wire semantics');
    LDocument := TNyxCodec.Decode(LExport.Design);
    try
      Check(LDocument.Find('workspace-note').Parent.ID = 'side-stack', 'design step after data is applied in order');
      Check(LDocument.State.Value('note').TextValue = LText, 'exact supplementary Unicode bound default');
      Check(LDocument.State.Value('raw-note').TextValue = LText + TNyxText(#0),
        'unbound NUL data stays exact without weakening portable control text admission');
      Check(LDocument.Find('workspace-note').BindingCount = 1, 'new input is bound to new scalar data');
      Check(LDocument.Collections.Snapshot(LKey).Count = 2, 'new table has independent owned rows');
      Check(LDocument.Find('work-table').CollectionView.QueryPolicy.Defined,
        'typed collection query shares the composition');
    finally
      LDocument.Free;
    end;
    Check((Pos('INyxInput', LExport.Source) > 0) and
      (Pos('INyxTable', LExport.Source) > 0) and
      (Pos('function WorkspaceNote', LExport.Source) > 0), 'specialized source and handwritten helper survive');
    Check(LAgent.Call('nyx_transaction', 'New display name', LArgs, 'transaction-owner').ToJSON =
      LReceipt.ToJSON, 'same actor-bound retry returns exact receipt without another mutation');
    Check(PairText = LAccepted, 'retry retains the exact pair');
    Refuses(LArgs, 'foreign authority cannot consume the stale receipt', 'foreign-owner');
    Refuses(Arguments('compose', NyxArray([NyxObject([
      NyxField('op', NyxData('title')), NyxField('value', NyxData('Different payload'))])])),
      'same operation identity refuses a different payload');
    LArgs := Arguments('stale', LTransaction.ToData);
    History('undo', 'undo-whole');
    Check(PairText = LInitial, 'one Undo removes all controls, data, bindings and query');
    Check(not LAgent.Call('nyx_session', 'Scooty', NyxObject([])).Field('canUndo').AsBoolean,
      'temporary candidate histories never leak into active Undo');
    Refuses(LArgs, 'stale revision preserves accepted pair and Redo');
    History('redo', 'redo-whole');
    Check(PairText = LAccepted, 'one Redo restores the exact whole composition');

    LTransaction := NyxProjectTransaction([
      NyxDesignStep(ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
        '[{"op":"title","value":"Must roll back"}]'))),
      NyxStateStep(NyxStateBindingPatch([
        NyxSetDefault(NyxStateValue(NyxTextState('note'), 'Must roll back'))])),
      NyxCollectionStep(NyxCollectionPatch([
        NyxAppendCollectionRow(NyxCollectionItem(NyxItem(LKey, 'discarded'))),
        NyxSetCollectionQuery(NyxBindingOwner('missing-table'), LKey, cpTable, NyxCollectionQuery)]))]);
    Refuses(Arguments('late-failure', LTransaction.ToData),
      'late collection failure rolls back design, scalar changes and earlier rows');
    Refuses(Arguments('root-bypass', TNyxDataValue.ParseJSON(
      '[{"op":"state","changes":[{"op":"set","name":"note","kind":"text","value":"Discard"}]},' +
      '{"op":"delete","id":"home"}]')), 'root removal still requires its reviewed tool');
    Refuses(Arguments('shape', TNyxDataValue.ParseJSON(
      '[{"op":"state","changes":[{"op":"set","name":"note","kind":"text","value":"Discard"}],"extra":true}]')),
      'unknown group fields refuse before publication');
    Refuses(Arguments('family', TNyxDataValue.ParseJSON(
      '[{"op":"state","changes":[{"op":"set","name":"note","kind":"integer","value":3}]}]')),
      'scalar-family substitution refuses');
    Refuses(Arguments('nested', TNyxDataValue.ParseJSON(
      '[{"op":"state","changes":[{"op":"collections","changes":[]}]}]')),
      'nested domains refuse');
    Refuses(Arguments('empty', NyxArray([])), 'empty transaction refuses');

    SetLength(LOperations, 64);
    for LIndex := 0 to 62 do
    begin
      LOperations[LIndex] := NyxObject([NyxField('op', NyxData('title')),
        NyxField('value', NyxData('A paired workspace'))]);
    end;
    LOperations[63] := NyxObject([NyxField('op', NyxData('state')),
      NyxField('changes', NyxStateBindingPatch([
        NyxSetDefault(NyxStateValue(NyxTextState('note'), LText)),
        NyxSetDefault(NyxStateValue(NyxTextState('note'), LText))]).ToData)]);
    Refuses(Arguments('budget', NyxArray(LOperations)), 'nested groups cannot exceed 64 total changes');
    LOperations[63] := NyxObject([NyxField('op', NyxData('state')),
      NyxField('changes', NyxStateBindingPatch([
        NyxSetDefault(NyxStateValue(NyxTextState('note'), LText))]).ToData)]);
    Check(ReadNyxProjectTransaction(NyxArray(LOperations)).Count = 64, 'exact total budget remains usable');
    Check(EncodeNyxProject(ReadNyxProjectTransaction(NyxArray(LOperations)).Candidate(LExport)) =
      LAccepted, 'exact-budget no-op candidate keeps the paired source');
    LRejected := False;
    try
      NyxProjectTransaction([Default(TNyxTransactionStep)]);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unconstructed typed step refuses');

    LAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    Refuses(Arguments('permission', NyxArray([LOperations[0]])), 'read-only permission gates the combined tool');
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('edit'))]));
    LPair := LExport;
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + TNyxText(#10 + '{ Pending handwritten work }');
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('guide')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;
    Refuses(Arguments('draft', NyxArray([LOperations[0]])), 'exact pending draft and base survive refusal');
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('project', NyxData(EncodeNyxProject(LExport))),
      NyxField('selection', NyxData('guide')), NyxField('view', NyxData('home'))]));
    LRevision := LAgent.Revision;

    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaRemove;
    LIntent.Key := LKey;
    Apply('remove-dependent', NyxProjectTransaction([
      NyxDesignStep(ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
        '[{"op":"delete","id":"side-stack"},{"op":"delete","id":"work-table"}]'))),
      NyxStateStep(NyxStateBindingPatch([
        NyxRemoveDefault(NyxStudioState(NyxTextState('note'))),
        NyxRemoveDefault(NyxStudioState(NyxTextState('raw-note')))])),
      NyxCollectionStep(NyxCollectionPatch([NyxCollectionIntentChange(LIntent)]))]));
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      Check((LDocument.Find('workspace-note') = nil) and not LDocument.State.Has('note') and
        (LDocument.Collections.Count = 0), 'ordered cleanup admits all three domains together');
    finally
      LDocument.Free;
    end;
    History('undo', 'undo-cleanup');
    Check(PairText = LAccepted, 'one cleanup Undo restores exact scalar and collection dependencies');

    {$ifndef PAS2JS}
    LSchema := NyxStudioMCPTools.Field('tools');
    for LIndex := 0 to LSchema.Count - 1 do
    begin
      LTool := LSchema.Item(LIndex);

      if LTool.Field('name').AsText = 'nyx_transaction' then
      begin
        LTool := LTool.Field('inputSchema');
        Check(LTool.Field('$defs').ToJSON = NyxCollectionAgentSchema.Field('$defs').ToJSON,
          'recursive query references resolve at the actual tool schema root');
        Check(Pos('"state"', LTool.ToJSON) > 0, 'actual tool advertises scalar groups');
        Check(Pos('"collections"', LTool.ToJSON) > 0, 'actual tool advertises collection groups');
        Break;
      end;
    end;
    Check(LIndex < LSchema.Count, 'existing transaction tool remains discoverable');
    ExportPair;
    WriteLn('PASS ', LCount, ' combined semantic transaction checks');
    {$else}
    document.body.textContent := 'PASS ' + IntToStr(LCount) + ' combined semantic transaction checks';
    document.body.setAttribute('data-nyx-agent-transactions', 'passed');
    {$endif}
  finally
    LAgent.Free;
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-agent-transactions', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
