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

{ Included by the maintained production semantic/compiler consumer. Direct data
  tools must work against the same exact handwritten baseline as mixed groups.
  These tests invoke real compiler children and ordinary paired history, never
  a caller-supplied projection or a protected Studio project. }
procedure SemanticFocusedData(const AFailure: TNyxText);
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LDocument: TNyxDocument;
  LBinding: TNyxBindingSpec;
  LRows: INyxCollectionSnapshot;
  LKey: TNyxCollectionRef;
  LCounter: Integer;

  function Arguments(const AID: TNyxText; const AChanges: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('apply')),
      NyxField('expectedRevision', NyxData(Revision)), NyxField('operationId', NyxData(AID)),
      NyxField('changes', AChanges)]);
  end;

  { Retain the exact pre/post project, prove one paired command, then return to
    the original fixture. Receipt retries keep their initiating tool identity. }
  procedure Apply(ADomain: TNyxTransactionDomain; const AChanges: TNyxDataValue);
  var
    LTool: TNyxText;
  begin
    Inc(LCounter);
    LTool := NyxTransactionToolName(ADomain);
    LBefore := Observe;
    LArguments := Arguments('focused-data-' + IntToStr(LCounter), AChanges);
    Marker(GGate, True);
    try
      LReceipt := GEngine.InvokeTool(LTool, COwner, CActor, LArguments);
      Check((LReceipt.Field('publication').Field('state').AsText = 'pending') and
        (LReceipt.Field('purpose').AsText = 'transaction') and
        (LReceipt.Field('scope').AsText = 'construction'),
        LTool + ' returns an unpublished constructor receipt');
      Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
        LTool + ' enqueue preserves the exact current pair');
      Check(GEngine.InvokeTool(LTool, COwner, CActor, LArguments).ToJSON = LReceipt.ToJSON,
        LTool + ' exact retry retains one queued job');
      Refuse('nyx_transaction', NyxFocusedTransactionArguments(ADomain, LArguments),
        LTool + ' retry cannot alias its normalized mixed transaction');
    finally
      Marker(GGate, False);
    end;
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('publication').Field('state').AsText = 'committed') and
      (LStatus.Field('publication').Field('revision').AsInteger =
        LArguments.Field('expectedRevision').AsInteger + 1),
      LTool + ' actual constructor publishes exactly one next revision');
    LAfter := Observe;
    Check(GEngine.InvokeTool(LTool, COwner, CActor, LArguments).ToJSON = LReceipt.ToJSON,
      LTool + ' completed retry preserves its original acknowledgement');
    Check(Pos('class function TNotebookCards.Caption',
      DecodeNyxProject(LAfter.Field('project').AsText).Source) > 0,
      LTool + ' retains the unrelated handwritten helper');
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      LTool + ' one Undo restores the complete original pair');
    Check(History('redo').Field('project').AsText = LAfter.Field('project').AsText,
      LTool + ' one Redo restores the complete constructed pair');
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      LTool + ' leaves its owned fixture restored');
  end;

begin
  LInitial := Observe;
  LCounter := 0;
  Apply(ntdState, NyxStateBindingPatch([
    NyxCreateDefault(NyxStateValue(NyxTextState('semantic-message'), 'First value')),
    NyxBindControl(NyxBindingOwner('heading-1'), bpText,
      NyxTextState('semantic-message'), bdFromState),
    NyxRenameDefault(NyxStudioState(NyxTextState('semantic-message')),
      NyxStudioState(NyxTextState('renamed-message'))),
    NyxSetDefault(NyxStateValue(NyxTextState('renamed-message'), CText))]).ToData);
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.State.GetValue(NyxTextState('renamed-message')) = CText) and
      LDocument.Find('heading-1').FindBinding(bpText, LBinding) and
      (LBinding.StateName = 'renamed-message') and (LBinding.ValueKind = nskText),
      'direct state construction preserves exact supplementary text and migrated binding');
  finally
    LDocument.Free;
  end;

  LKey := NyxCollection('focused-notes');
  Apply(ntdCollections, NyxCollectionPatch([
    NyxDefineCollection(LKey, NyxCollectionSchema.Text(NyxTextField('label'), CText)
      .Integer(NyxIntegerField('rank'), 3, NyxIntegerDomain.Range(0, 9)),
      [NyxCollectionItem(NyxItem(LKey, 'first'))]),
    NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(LKey, 'first'))
      .WithValue(NyxIntegerField('rank'), 7))]).ToData);
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    LRows := LDocument.Collections.Snapshot(LKey);
    Check((LRows.Count = 1) and
      (LRows.ItemAt(0).GetValue(NyxTextField('label')) = CText) and
      (LRows.ItemAt(0).GetValue(NyxIntegerField('rank')) = 7),
      'direct collection construction retains exact text and typed row values');
  finally
    LRows := nil;
    LDocument.Free;
  end;

  Apply(ntdResources, NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('focused-copy'), NyxDefaultLocale,
      NyxJSONResource('{"caption":"' + CText + '","exact":9007199254740993}')),
    NyxBindResource(NyxControl('heading-1'), bpText,
      NyxResourceValue(NyxResourceRef('focused-copy')).Field('caption'))]).ToData);
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.Resources.Definition(NyxResourceRef('focused-copy'), NyxDefaultLocale)
      .Data.Field('exact').AsDecimal.Text = '9007199254740993') and
      LDocument.Find('heading-1').FindBinding(bpText, LBinding) and
      (LBinding.Source = bsResource),
      'direct resource construction retains exact numeric tokens and binding source');
  finally
    LDocument.Free;
  end;

  LArguments := Arguments('focused-invalid-tail', NyxStateBindingPatch([
    NyxCreateDefault(NyxStateValue(NyxTextState('partial'), 'Must not survive')),
    NyxBindControl(NyxBindingOwner('missing'), bpText,
      NyxTextState('partial'), bdFromState)]).ToData);
  Refuse('nyx_state', LArguments, 'focused late invalid binding refuses the complete group');
  LArguments := Arguments('focused-failure', NyxStateBindingPatch([
    NyxSetDefault(NyxStateValue(NyxIntegerState('notes'), 8))]).ToData);
  Marker(AFailure, True);
  try
    LBefore := Observe;
    LReceipt := GEngine.InvokeTool('nyx_state', COwner, CActor, LArguments);
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'focused throwing constructor retains the complete accepted pair');
  finally
    Marker(AFailure, False);
  end;

  LArguments := Arguments('focused-draft', NyxStateBindingPatch([
    NyxSetDefault(NyxStateValue(NyxIntegerState('notes'), 8))]).ToData);
  Draft(DecodeNyxProject(Observe.Field('project').AsText).Source + CNewDraft);
  Refuse('nyx_state', LArguments, 'focused data Apply refuses an unfinished draft');
  ClearDraft;

  LArguments := Arguments('focused-cancel', NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('cancel-copy'), NyxDefaultLocale,
      NyxTextResource('Cancelled work'))]).ToData);
  Marker(GGate, True);
  try
    LBefore := Observe;
    LReceipt := GEngine.InvokeTool('nyx_resources', COwner, CActor, LArguments);
    GEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('cancel')), NyxField('job', LReceipt.Field('job')),
      NyxField('expectedRevision', NyxData(Revision)),
      NyxField('operationId', NyxData('focused-cancel-job'))]));
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
      (Observe.Field('project').AsText = LBefore.Field('project').AsText),
      'focused cancelled constructor cannot publish its data group');
  finally
    Marker(GGate, False);
  end;
  Check(Observe.Field('project').AsText = LInitial.Field('project').AsText,
    'focused data journey restores its exact original owned pair');
end;
