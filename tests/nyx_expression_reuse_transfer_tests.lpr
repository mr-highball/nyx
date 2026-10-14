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

program nyx_expression_reuse_transfer_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.expressions,
  nyx.binding.types, nyx.model, nyx.controls, nyx.composition, nyx.binding,
  nyx.codec, nyx.codegen, nyx.source, nyx.files, nyx.studio.files,
  nyx.studio.projects, nyx.studio.session, nyx.studio.agents,
  nyx.studio.edits, nyx.studio.stateedits, nyx.studio.transactions,
  nyx.generated.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Deliberately mutable extension facade. Admission must consume its descriptor
    and own a normalized tree, never delegate evaluation to this live object. }
  TForeignExpression = class(TInterfacedObject, INyxExpression)
  public
    Data: TNyxDataValue;
    function GetKind: TNyxStateKind;
    function GetOperator: TNyxExpressionOperator;
    function GetNodeCount: Integer;
    function GetDepth: Integer;
    function GetChildCount: Integer;
    function Child(AIndex: Integer): INyxExpression;
    function GetDependencyCount: Integer;
    function Dependency(AIndex: Integer): TNyxExpressionDependency;
    procedure ValidateInputs(const AInputs: INyxExpressionInputs);
    function Value(const AInputs: INyxExpressionInputs): TNyxStateValue;
    function ToData: TNyxDataValue;
  end;

var
  GChecks: Integer;

function TForeignExpression.GetKind: TNyxStateKind;
begin
  Result := nskText;
end;

function TForeignExpression.GetOperator: TNyxExpressionOperator;
begin
  Result := neoLiteral;
end;

function TForeignExpression.GetNodeCount: Integer;
begin
  Result := 1;
end;

function TForeignExpression.GetDepth: Integer;
begin
  Result := 1;
end;

function TForeignExpression.GetChildCount: Integer;
begin
  Result := 0;
end;

function TForeignExpression.Child(AIndex: Integer): INyxExpression;
begin
  Result := nil;
  raise Exception.Create('Foreign live child access is forbidden');
end;

function TForeignExpression.GetDependencyCount: Integer;
begin
  Result := 0;
end;

function TForeignExpression.Dependency(AIndex: Integer): TNyxExpressionDependency;
begin
  Result := Default(TNyxExpressionDependency);
  raise Exception.Create('Foreign live dependency access is forbidden');
end;

procedure TForeignExpression.ValidateInputs(const AInputs: INyxExpressionInputs);
begin
  raise Exception.Create('Foreign live validation is forbidden');
end;

function TForeignExpression.Value(const AInputs: INyxExpressionInputs): TNyxStateValue;
begin
  Result := Default(TNyxStateValue);
  raise Exception.Create('Foreign live evaluation is forbidden');
end;

function TForeignExpression.ToData: TNyxDataValue;
begin
  Result := Data;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Expression reuse/transfer: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Verify;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LProjection: TNyxNode;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LAgent: TNyxAgentSession;
  LImported: TNyxStudioSession;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LTransaction: INyxProjectTransaction;
  LPair: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LFiles: TNyxTextFiles;
  LCaption: INyxTextExpression;
  LOverride: INyxTextExpression;
  LLabel: INyxLabel;
  LColumn: INyxColumn;
  LSpec: TNyxBindingSpec;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LPacket: TNyxText;
  LImportedBefore: TNyxText;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LRefused: Boolean;
  LForeign: TForeignExpression;
  LForeignLease: INyxExpression;
  LAdmitted: TNyxBindingSpec;
  {$ifndef PAS2JS}
  LDirectory: TNyxText;
  LStream: TFileStream;
  {$endif}

  function Observe: TNyxText;
  begin
    Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
  end;

  procedure Apply(const ATransaction: INyxProjectTransaction; const AID: TNyxText);
  begin
    LRequest := NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData(AID)),
      NyxField('operations', ATransaction.ToData)]);
    LReceipt := LAgent.Call('nyx_transaction', 'Expression reuse workshop', LRequest);
  end;

  procedure History(const ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Expression reuse workshop', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('reuse-' + ADirection + '-' +
        TNyxText(IntToStr(LAgent.Revision)))),
      NyxField('direction', NyxData(ADirection))]));
  end;

begin
  LDocument := BuildNyxDocument;
  LCopy := nil;
  LProjection := nil;
  LParsed := nil;
  LWorkspace := nil;
  LAgent := nil;
  LImported := nil;
  LStore := nil;
  LLive := nil;
  try
    LColumn := NewNyxColumn('calculated-card');
    LColumn.Configure.Padding(16).Gap(8).Done;
    LDocument.AddComponent(LColumn);
    LLabel := NewNyxLabel('calculated-caption');
    LLabel.Configure.PartName(NyxPart('caption')).Done;
    LColumn.Add(LLabel);
    LCaption := NyxTextExpression(TNyxText('Total 🌙: ')).Append(
      NyxNumberExpression(NyxNumberState('price')).Times(
        NyxIntegerExpression(NyxIntegerState('quantity')).AsNumber)
        .Plus(NyxNumberExpression(NyxNumberState('tax'))).AsText);
    LLabel.Binds.Text(LCaption).Done;
    LDocument.Validate;
    LForeign := TForeignExpression.Create;
    LForeignLease := LForeign;
    LForeign.Data := LCaption.ToData;
    LAdmitted := TNyxBindingSpec.Computed(bpText, LForeignLease);
    LForeign.Data := NyxTextExpression('Mutated foreign source').ToData;
    Check(LAdmitted.Expression.ToData.ToJSON = LCaption.ToData.ToJSON,
      'alternate mutable implementation admits an independent exact descriptor');
    Check((LAdmitted.Expression as INyxTextExpression).Evaluate(
      CaptureNyxExpressionInputs(LDocument.State)) = TNyxText('Total 🌙: 40'),
      'normalized binding never delegates evaluation to the alternate implementation');
    LForeignLease := nil;
    LForeign := nil;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    FreeAndNil(LDocument);
    LAgent := TNyxAgentSession.Create(LPair);
    LBefore := Observe;
    LOverride := NyxTextExpression('Own quantity: ').Append(
      NyxIntegerExpression(NyxIntegerState('quantity')).AsText);
    LTransaction := NyxProjectTransaction([
      NyxDesignStep(NyxReusablePatch([
        NyxDeriveComponent(NyxControl('calculated-card'), NyxComponent('featured-card'), [
          NyxIdentity(NyxControl('calculated-caption'), NyxControl('featured-caption'))]),
        NyxInstantiateComponent(NyxComponent('featured-card'), NyxControl('featured-first'),
          NyxControl('home')),
        NyxInstantiateComponent(NyxComponent('featured-card'), NyxControl('featured-second'),
          NyxControl('home')),
        NyxOverrideComponentPart(NyxControl('featured-first'), NyxControl('first-caption-rule'),
          NyxPart('caption'), noProperties)])),
      NyxStateStep(NyxStateBindingPatch([
        NyxBindControl(NyxBindingOwner('first-caption-rule'), bpText, LOverride)]))]);
    Apply(LTransaction, 'derive-calculated-card');
    LAfter := Observe;
    Check(LAfter <> LBefore, 'semantic derivation/instances/computed override publish one pair');
    Check(LAgent.Call('nyx_transaction', 'Expression reuse workshop', LRequest).ToJSON =
      LReceipt.ToJSON, 'exact grouped semantic retry is idempotent');
    History('undo');
    Check(Observe = LBefore, 'one paired Undo restores pre-derivation project');
    History('redo');
    Check(Observe = LAfter, 'one paired Redo restores derived definitions and formula');
    LRefused := False;
    try
      Apply(NyxProjectTransaction([
        NyxDesignStep(NyxReusablePatch([
          NyxDeriveComponent(NyxControl('featured-card'), NyxComponent('invalid-card'), [
            NyxIdentity(NyxControl('featured-caption'), NyxControl('invalid-caption'))])])),
        NyxStateStep(NyxStateBindingPatch([
          NyxBindControl(NyxBindingOwner('invalid-caption'), bpEnabled,
            NyxTextExpression('Wrong target family'))]))]), 'invalid-derived-formula');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Observe = LAfter),
      'invalid derived binding rolls back the complete semantic group');
    Apply(NyxProjectTransaction([NyxDesignStep(NyxReusablePatch([
      NyxInheritComponentPart(NyxControl('featured-first'), NyxControl('first-caption-rule'),
        NyxPart('caption'))]))]), 'inherit-calculated-caption');
    LPair := DecodeNyxProject(Observe);
    LCopy := TNyxCodec.Decode(LPair.Design);
    LProjection := RealizeNyxView(LCopy, LCopy.Find('home'));
    LStore := LCopy.State.Clone;
    LLive := TNyxLiveBindings.Create(LProjection, LStore);
    LLive.Activate;
    Check((LCopy.Find('first-caption-rule') = nil) and
      (LProjection.Find(NyxQualifiedID('featured-first', 'featured-caption')).Prop('text') =
        TNyxText('Total 🌙: 40')), 'restoring inheritance retires only the computed override');
    FreeAndNil(LLive);
    FreeAndNil(LStore);
    FreeAndNil(LProjection);
    FreeAndNil(LCopy);
    History('undo');
    Check(Observe = LAfter, 'inheritance Undo restores exact independent instance formula');
    LPair := DecodeNyxProject(LAfter);
    LDocument := TNyxCodec.Decode(LPair.Design);
    Check(LDocument.Find('calculated-caption').FindBinding(bpText, LSpec) and
      (LSpec.Expression.ToData.ToJSON = LCaption.ToData.ToJSON),
      'derivation retains original definition expression exactly');
    Check(LDocument.Find('featured-caption').FindBinding(bpText, LSpec) and
      (LSpec.Expression.ToData.ToJSON = LCaption.ToData.ToJSON),
      'derived definition owns the exact copied formula');
    LProjection := RealizeNyxView(LDocument, LDocument.Find('home'));
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LProjection, LStore);
    LLive.Activate;
    Check(LProjection.Find(NyxQualifiedID('featured-first', 'featured-caption')).Prop('text') =
      'Own quantity: 3', 'first runtime instance uses its own computed override');
    Check(LProjection.Find(NyxQualifiedID('featured-second', 'featured-caption')).Prop('text') =
      TNyxText('Total 🌙: 40'), 'sibling instance retains inherited exact formula');
    LStore.SetValue(NyxIntegerState('quantity'), 4);
    Check((LProjection.Find(NyxQualifiedID('featured-first', 'featured-caption')).Prop('text') =
      'Own quantity: 4') and
      (LProjection.Find(NyxQualifiedID('featured-second', 'featured-caption')).Prop('text') =
        TNyxText('Total 🌙: 52.5')), 'runtime update recomputes independent instance formulas');
    Check(TNyxCodec.Encode(LDocument) = LPair.Design,
      'runtime realization never mutates either authored definition');
    FreeAndNil(LLive);
    FreeAndNil(LStore);
    FreeAndNil(LProjection);
    LCopy := LDocument.Clone;
    LCopy.Find('featured-caption').SetBinding(TNyxBindingSpec.Computed(bpText,
      NyxTextExpression('Changed clone')));
    Check(LDocument.Find('featured-caption').FindBinding(bpText, LSpec) and
      (LSpec.Expression.ToData.ToJSON = LCaption.ToData.ToJSON),
      'cloned definition edits preserve source ownership');
    FreeAndNil(LCopy);
    LFiles := NyxStudioProjectCompanion(LPair);
    LPacket := ReadNyxStudioProjectFiles(LFiles);
    Check(EncodeNyxProject(DecodeNyxProject(LPacket)) = EncodeNyxProject(LPair),
      'adjacent file transfer retains accepted design and Pascal exactly');
    AdmitNyxProject(DecodeNyxProject(LPacket), nprRequireMatch, LParsed, LWorkspace, LResolved);
    Check((TNyxCodec.Encode(LParsed) = LPair.Design) and
      (LWorkspace.Render(LParsed) = LPair.Source), 'transferred source admits independently with exact no-op output');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPending := LPair;
    LPending.Pending := True;
    LPending.DraftBase := LPair.Source;
    LPending.Draft := LPair.Source + #10 + TNyxText('// Unfinished reusable idea 🌙');
    LFiles := NyxStudioProjectBackup(LPending);
    LPacket := ReadNyxStudioProjectFiles(LFiles);
    Check(LPacket = EncodeNyxProject(LPending),
      'project backup carries exact accepted formula and unfinished Unicode draft/base');
    LImported := TNyxStudioSession.Create;
    LImportedBefore := EncodeNyxProject(LImported.ProjectSnapshot);
    LImported.AdoptProject(DecodeNyxProject(LPacket));
    Check(EncodeNyxProject(LImported.ProjectSnapshot) = LPacket,
      'fresh editor admits exact computed reusable backup and pending text');
    LImported.Undo;
    Check(EncodeNyxProject(LImported.ProjectSnapshot) = LImportedBefore,
      'project adoption is one ordinary paired Undo command');
    LImported.Redo;
    Check(EncodeNyxProject(LImported.ProjectSnapshot) = LPacket,
      'project adoption Redo retains both formula and unfinished draft');
    LImported.DiscardSourceDraft;
    LImported.Select('featured-caption');
    LImported.SetBinding(TNyxBindingSpec.Computed(bpText,
      NyxTextExpression('Derived quantity: ').Append(
        NyxIntegerExpression(NyxIntegerState('quantity')).AsText)));
    Check(LImported.Document.Find('calculated-caption').FindBinding(bpText, LSpec) and
      (LSpec.Expression.ToData.ToJSON = LCaption.ToData.ToJSON),
      'editing reopened derived definition preserves the original recipe');
    LImported.RenameState('quantity', 'units');
    Check(LImported.Document.State.Has('units') and
      LImported.Document.Find('first-caption-rule').FindBinding(bpText, LSpec) and
      (LSpec.Expression.Dependency(0).Name = 'units'),
      'typed rename rewrites reopened instance dependencies through source admission');
    LImported.Undo;
    Check(LImported.Document.State.Has('quantity'), 'rename Undo restores shared state identity');
    LImportedBefore := EncodeNyxProject(LImported.ProjectSnapshot);
    LRefused := False;
    try
      LImported.AdoptProject(NyxProjectPair(LPair.Design, 'Invalid Pascal'));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LImported.ProjectSnapshot) = LImportedBefore),
      'invalid transfer refuses without replacing reopened expressions');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      LStream := TFileStream.Create(LDirectory + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LPair.Source[1], Length(LPair.Source));
      finally
        LStream.Free;
      end;
      LStream := TFileStream.Create(LDirectory + 'project.nyx', fmCreate);
      try
        LPacket := EncodeNyxProject(LPair);
        LStream.WriteBuffer(LPacket[1], Length(LPacket));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LLive.Free;
    LStore.Free;
    ReleaseNyxNode(LProjection);
    LWorkspace.Free;
    LParsed.Free;
    LImported.Free;
    LAgent.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-expression-reuse', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' expression reuse/transfer checks';
    {$else}
    WriteLn('PASS ', GChecks, ' expression reuse/transfer checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-reuse', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
