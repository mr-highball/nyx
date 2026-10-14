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

program nyx_computed_binding_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$else}Classes,{$endif}
  nyx.text, nyx.types, nyx.state, nyx.expressions, nyx.data, nyx.model,
  nyx.controls, nyx.binding.types, nyx.binding, nyx.codec, nyx.codegen,
  nyx.composition, nyx.source, nyx.studio.projects, nyx.studio.session;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Computed binding: ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LLabel: INyxLabel;
  LButton: INyxButton;
  LMemo: INyxMemo;
  LCard: INyxCard;
  LInstance: INyxComponent;
  LTotal: INyxNumberExpression;
  LCaption: INyxTextExpression;
  LEnabled: INyxBooleanExpression;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Calculated order';
    Result.State.SetValue(NyxBooleanState('ready'), True)
      .SetValue(NyxIntegerState('quantity'), 3)
      .SetValue(NyxNumberState('price'), 12.5)
      .SetValue(NyxNumberState('tax'), 2.5)
      .SetValue(NyxTextState('notes'), 'Your notes');
    LTotal := NyxNumberExpression(NyxNumberState('price'))
      .Times(NyxIntegerExpression(NyxIntegerState('quantity')).AsNumber)
      .Plus(NyxNumberExpression(NyxNumberState('tax')));
    LCaption := NyxTextExpression('Total: ').Append(LTotal.AsText);
    LEnabled := NyxBooleanExpression(NyxBooleanState('ready')).AndAlso(
      NyxIntegerExpression(NyxIntegerState('quantity')).GreaterThan(NyxIntegerExpression(0)));
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(16).Gap(12).Done;
    Result.AddPage(LPage);
    LLabel := NewNyxLabel('total');
    LLabel.Binds.Text(LCaption).Done;
    LPage.Add(LLabel);
    LButton := NewNyxButton('submit');
    LButton.WithText('Submit order');
    LButton.Binds.Enabled(LEnabled).Done;
    LPage.Add(LButton);
    LMemo := NewNyxMemo('computed-value');
    LMemo.WithText('Calculated summary');
    LMemo.Binds.Value(LCaption).Done;
    LPage.Add(LMemo);
    LMemo := NewNyxMemo('notes');
    LMemo.WithText('Your notes');
    LMemo.Binds.Value(NyxTextState('notes')).Done;
    LPage.Add(LMemo);
    LCard := NewNyxCard('price-card');
    Result.AddComponent(LCard);
    LLabel := NewNyxLabel('price-label');
    LLabel.Configure.PartName(NyxPart('total')).Done;
    LLabel.Binds.Text(LCaption).Done;
    LCard.Add(LLabel);
    LInstance := NewNyxComponent('first');
    LInstance.Configure.Component(NyxComponent('price-card')).Done;
    LInstance.OverridePart(NyxPart('total'), noProperties).Binds.Text(
      NyxTextExpression('Own: ').Append(NyxIntegerExpression(NyxIntegerState('quantity')).AsText)).Done;
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second');
    LInstance.Configure.Component(NyxComponent('price-card')).Done;
    LPage.Add(LInstance);
    LPage := NewNyxPage('review');
    Result.AddPage(LPage);
    LLabel := NewNyxLabel('review-total');
    LLabel.Binds.Text(LCaption).Done;
    LPage.Add(LLabel);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Verify;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LParsed: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LCaption: TNyxText;
  LPair: TNyxProjectPair;
  LSpec: TNyxBindingSpec;
  LRevision: Integer;
  LRefused: Boolean;
  LValue: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LDirectory: TNyxText;
  {$endif}
begin
  LDocument := Fixture;
  LCopy := nil;
  LParsed := nil;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    Check(LDocument.HasComputedBindings, 'document detects authored and reusable computed descriptors');
    LBefore := TNyxCodec.Encode(LDocument);
    LValue := TNyxDataValue.ParseJSON(LBefore);
    Check(LValue.Field('version').AsInteger = 12, 'computed descriptors select design version twelve');
    LCopy := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCopy) = LBefore, 'exact computed tree and reusable override codec round trip');
    LCopy.Find('total').Binds.Clear(bpText).Done;
    Check(LDocument.Find('total').FindBinding(bpText, LSpec) and (LSpec.Source = bsExpression),
      'decoded descriptor changes preserve the original tree');
    FreeAndNil(LCopy);
    SetLength(LFields, LValue.Count);
    for LIndex := 0 to LValue.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LValue.Key(LIndex), LValue.Field(LValue.Key(LIndex)));

      if LValue.Key(LIndex) = 'version' then
      begin
        LFields[LIndex] := NyxField('version', NyxData(11));
      end;
    end;
    LRefused := False;
    try
      LCopy := TNyxCodec.Decode(NyxObject(LFields).ToJSON);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'older wire versions refuse computed descriptors without changing accepted work');
    FreeAndNil(LCopy);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('nyx.expressions', LSource) > 0) and (Pos('.AndAlso(', LSource) > 0) and
      (Pos('.Times(', LSource) > 0) and (Pos('.Append(', LSource) > 0),
      'generated Pascal uses typed fluent composition');
    Check(Pos('NyxExpressionFromData', LSource) = 0, 'generated source does not dump expression wire data');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'ordinary source admission reconstructs the complete expression design');
    Check(LWorkspace.Render(LParsed) = LSource, 'unchanged generated expression source remains exact');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('submit');
    LSession.SetProperty('text', 'Send order');
    Check(LSession.CanUndo and (LSession.Source <> LSource), 'ordinary paired edit retains computed source');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'one paired Undo restores exact design and computed Pascal');
    LSession.Redo;
    Check(LSession.Document.HasComputedBindings, 'paired Redo retains expression ownership');
    FreeAndNil(LSession);
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;
    Check(LRoot.Find('total').Prop('text') = 'Total: 40', 'shared projection evaluates calculated caption');
    Check(LRoot.Find('submit').Prop('enabled') = 'true', 'shared projection evaluates compound condition');
    Check(LRoot.Find(NyxQualifiedID('first', 'price-card')).Part(NyxPart('total')).Prop('text') = 'Own: 3',
      'instance-only computed override has independent expression meaning');
    Check(LRoot.Find(NyxQualifiedID('second', 'price-card')).Part(NyxPart('total')).Prop('text') = 'Total: 40',
      'sibling instance retains inherited expression meaning');
    LStore.SetValue(NyxIntegerState('quantity'), 0);
    Check((LRoot.Find('total').Prop('text') = 'Total: 2.5') and
      (LRoot.Find('submit').Prop('enabled') = 'false'),
      'one state update changes both computed consumers / caption ' + LRoot.Find('total').Prop('text') +
      ' / enabled ' + LRoot.Find('submit').Prop('enabled'));
    Check((LDocument.State.GetValue(NyxIntegerState('quantity')) = 3) and
      (TNyxCodec.Encode(LDocument) = LBefore), 'runtime calculations preserve document-owned defaults');
    LRevision := LStore.Revision;
    LCaption := LRoot.Find('total').Prop('text');
    LRefused := False;
    try
      LStore.Apply([
        NyxStateValue(NyxIntegerState('quantity'), 2),
        NyxStateValue(NyxNumberState('price'), 1.7e308)]);
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Revision = LRevision) and
      (LStore.GetValue(NyxIntegerState('quantity')) = 0) and
      (LStore.GetValue(NyxNumberState('price')) = 12.5) and
      (LRoot.Find('total').Prop('text') = LCaption),
      'arithmetic failure refuses the entire multi-key candidate before state/UI publication');
    LRefused := False;
    try
      LStore.Remove(NyxNumberState('tax').Name);
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Revision = LRevision) and LStore.Has('tax'),
      'missing expression dependency refuses before publication');
    LRefused := False;
    try
      LStore.Apply([NyxStateAssign('quantity', TNyxStateValue.FromText('wrong kind'))]);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Revision = LRevision),
      'wrong-kind expression dependency preserves accepted state');
    LRefused := False;
    try
      LLive.Edit(LRoot.Find('computed-value'), 'Cannot write a formula');
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Revision = LRevision) and
      (LRoot.Find('computed-value').Prop('value') = 'Total: 2.5'),
      'computed Value never writes into its input keys');
    LStore.SetValue(NyxIntegerState('quantity'), 4);
    Check((LRoot.Find('total').Prop('text') = 'Total: 52.5') and
      (LRoot.Find('submit').Prop('enabled') = 'true'), 'a rejected candidate leaves subsequent valid updates usable');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      LStream := TFileStream.Create(LDirectory + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
      LStream := TFileStream.Create(LDirectory + 'design.nyx', fmCreate);
      try
        LStream.WriteBuffer(LBefore[1], Length(LBefore));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LCopy.Free;
    LLive.Free;
    LStore.Free;
    ReleaseNyxNode(LRoot);
    LDocument.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-computed-bindings', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' computed binding checks';
    {$else}
    WriteLn('PASS ', GChecks, ' computed binding checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-computed-bindings', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
