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

unit nyx.test.binding;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

{ One authored contract for portable, DOM and real LCL event journeys. }
function CreateNyxBindingFixture: TNyxDocument;
function RunNyxBindingTests: Integer;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.controls,
  nyx.responsive,
  nyx.state,
  nyx.binding.types,
  nyx.binding,
  nyx.behavior,
  nyx.catalog,
  nyx.composition,
  nyx.codec,
  nyx.codegen,
  nyx.schema,
  nyx.test.contract,
  nyx.publication,
  nyx.resources,
  nyx.studio.session;

function CreateNyxBindingFixture: TNyxDocument;
var
  LPage: TNyxNode;
  LCatalog: TNyxCatalog;
  LReplyCaption: TNyxNode;
  LReplyMemo: TNyxNode;
  LReplyMirrorMemo: TNyxNode;
  LRememberCheckbox: TNyxNode;
  LRatioInput: TNyxNode;
  LQuantityCaption: TNyxNode;
  LReviewCaption: TNyxNode;
  LSearch: TNyxNode;
  LStepper: TNyxNode;
begin
  LCatalog := TNyxCatalog.Create;
  try
    Result := TNyxDocument.Create;
    try
      Result.Title := 'Shared state';
      Result.State
        .SetValue(NyxTextState('🌙/reply'), 'Café / 🌙 / 漢字')
        .SetValue(NyxBooleanState('enabled'), True)
        .SetValue(NyxBooleanState('visible'), True)
        .SetValue(NyxBooleanState('readonly'), False)
        .SetValue(NyxBooleanState('checked'), False)
        .SetValue(NyxIntegerState('quantity'), 2)
        .SetValue(NyxNumberState('ratio'), 0.1)
        .SetValue(NyxIntegerState('width'), 300);

      LPage := TNyxNode.Create(nkPage, 'editor');
      Result.AddPage(LPage);
      LPage.Configure.Layout(nlColumn).Gap(12).Padding(12).Done;
      LPage.Binds.Enabled(NyxBooleanState('enabled')).Done;

      LReplyCaption := TNyxNode.Create(nkLabel, 'reply-caption');
      LPage.Add(LReplyCaption);
      LReplyCaption.Binds.Text(NyxTextState('🌙/reply')).Visible(NyxBooleanState('visible')).Done;

      LReplyMemo := TNyxNode.Create(nkMemo, 'reply-memo');
      LPage.Add(LReplyMemo);
      LReplyMemo.Configure.Text('Reply').Done;
      LReplyMemo.Binds.Value(NyxTextState('🌙/reply')).ReadOnly(NyxBooleanState('readonly'))
        .Width(NyxIntegerState('width')).Done;

      LReplyMirrorMemo := TNyxNode.Create(nkMemo, 'reply-mirror');
      LPage.Add(LReplyMirrorMemo);
      LReplyMirrorMemo.Configure.Text('Same reply').Done;
      LReplyMirrorMemo.Binds.Value(NyxTextState('🌙/reply')).Done;

      LRememberCheckbox := TNyxNode.Create(nkCheckbox, 'remember-checkbox');
      LPage.Add(LRememberCheckbox);
      LRememberCheckbox.Configure.Text('Remember').Done;
      LRememberCheckbox.Binds.Value(NyxBooleanState('checked')).Done;

      LRatioInput := TNyxNode.Create(nkInput, 'ratio-input');
      LPage.Add(LRatioInput);
      LRatioInput.Configure.InputType(niNumber).Text('Ratio').Done;
      LRatioInput.Binds.Value(NyxNumberState('ratio')).Done;

      LQuantityCaption := TNyxNode.Create(nkLabel, 'quantity-caption');
      LPage.Add(LQuantityCaption);
      LQuantityCaption.Binds.Text(NyxIntegerState('quantity')).Done;

      LSearch := LCatalog.NewNode(nkSearchField, 'search');
      LPage.Add(LSearch);
      { This compound shares a multiline reply. Specialize its query part through
        the public projection enum; the default search recipe remains single-line. }
      LSearch.Part('query').Configure.ProjectAs(nkMemo).Done;
      LSearch.Part('query').Binds.Value(NyxTextState('🌙/reply')).Done;

      LStepper := LCatalog.NewNode(nkNumberStepper, 'quantity-stepper');
      LPage.Add(LStepper);
      LStepper.Part('value').Binds.Value(NyxIntegerState('quantity')).Done;
      AddNyxContractFixture(Result);

      { A second page exercises application-owned state rather than a value stored
        in a discarded control. Both pages refer to the same authored key. }
      LPage := TNyxNode.Create(nkPage, 'review');
      Result.AddPage(LPage);
      LReviewCaption := TNyxNode.Create(nkLabel, 'review-caption');
      LPage.Add(LReviewCaption);
      LReviewCaption.Binds.Text(NyxTextState('🌙/reply')).Done;
      Result.Validate;
    except
      Result.Free;
      raise;
    end;
  finally
    LCatalog.Free;
  end;
end;

type
  TBindingProbe = class
  public
    Root: TNyxNode;
    Store: TNyxState;
    Calls: Integer;
    ValueCalls: Integer;
    Reject: Boolean;
    procedure Sync;
    function ValueSync(ANode: TNyxNode): Boolean;
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
  end;

procedure TBindingProbe.Sync;
begin
  Inc(Calls);

  if Root.Find('reply-memo').Prop('value') <>
    Store.GetValue(NyxTextState('🌙/reply')) then
  begin
    raise ENyxState.Create('Observer sees an incomplete projection');
  end;
end;

function TBindingProbe.ValueSync(ANode: TNyxNode): Boolean;
begin
  Inc(ValueCalls);
  Result := True;
end;

type
  { Borrowed realized identity proves the optional callback runs after complete
    publication. This independent probe retains no target or model ownership. }
  TValuePlanProbe = class
  public
    Root: TNyxNode;
    FullCalls: Integer;
    FieldCalls: Integer;
    AcceptField: Boolean;
    RaiseField: Boolean;
    SawPublication: Boolean;
    Value: TNyxText;
    procedure FullSync;
    function FieldSync(ANode: TNyxNode): Boolean;
  end;

procedure TValuePlanProbe.FullSync;
begin
  Inc(FullCalls);
end;

function TValuePlanProbe.FieldSync(ANode: TNyxNode): Boolean;
begin
  Inc(FieldCalls);
  SawPublication := (Root.Find(ANode.ID) = ANode) and (ANode.Prop('value') = Value);

  if RaiseField then
  begin
    raise ENyxState.Create('Intentional admitted field notification failure');
  end;
  Result := AcceptField;
end;

procedure TBindingProbe.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if Reject then
  begin
    raise ENyxState.Create('Domain command rejected');
  end;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Binding fixture: ' + AReason);
  end;
  Inc(ACount);
end;

function RunMetadataTests: Integer;
const
  CInvalidBindings: array[0..9] of TNyxText = (
    '{"property":"unknown","state":"reply","type":"text","direction":"two-way"}',
    '{"property":"value","state":"reply","type":"text","direction":"automatic"}',
    '{"property":"value","state":"reply","type":"unknown","direction":"two-way"}',
    '{"property":"value","clear":false}',
    '{"property":"value","clear":true,"state":"reply"}',
    '{"property":"value","state":"reply","type":"boolean","direction":"two-way"}',
    '{"property":"value","clear":true},{"property":"value","clear":true}',
    '{"property":"text","state":"reply","type":"text","direction":"two-way"}',
    '{"property":"visible","state":"reply","type":"text","direction":"from-state"}',
    '{"property":"value","state":"missing","type":"text","direction":"two-way"}');
var
  LIndex: Integer;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LSession: TNyxStudioSession;
  LBaseline: TNyxText;
  LRejected: Boolean;
begin
  Result := 0;
  for LIndex := 0 to High(CInvalidBindings) do
  begin
    LDecoded := nil;
    LRejected := False;
    try
      try
        { This is deliberately malformed persistence data, not authoring API. }
        LDecoded := TNyxCodec.Decode('{"version":1,"title":"Binding admission",' +
          '"state":{"reply":{"type":"text","value":"Accepted"}},' +
          '"pages":[{"kind":"memo","id":"reply-memo","props":{},"children":[],"bindings":[' +
          CInvalidBindings[LIndex] + ']}],"components":[]}');
      except
        on LException: Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'malformed binding metadata rejected ' + IntToStr(LIndex), Result);
    finally
      LDecoded.Free;
    end;
  end;
  LDocument := CreateNyxBindingFixture;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.SetStateValues([NyxStateValue(NyxIntegerState('quantity'), 3)]);
    LSession.Undo;
    LBaseline := LSession.Save;
    LRejected := False;
    try
      LSession.SetStateValues([NyxStateValue(NyxIntegerState('quantity'), 1000)]);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline),
      'bound default command rejects before changing history', Result);
    LSession.Redo;
    Check(LSession.Document.State.GetValue(NyxIntegerState('quantity')) = 3,
      'bound default rejection preserves redo', Result);
    LDocument.State.SetValue(NyxIntegerState('quantity'), 1000);
    LBaseline := LSession.Save;
    LRejected := False;
    try
      LSession.Load(TNyxCodec.Encode(LDocument));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline),
      'invalid bound defaults cannot replace the accepted design', Result);
    LRejected := False;
    try
      TNyxCodegen.Generate(LDocument);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'generation refuses invalid bound defaults before source emission', Result);
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

function RunReusableBindingTests: Integer;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LDefinition: TNyxNode;
  LDefinitionMemo: TNyxNode;
  LPage: TNyxNode;
  LFirst: TNyxNode;
  LSecond: TNyxNode;
  LThird: TNyxNode;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LSpec: TNyxBindingSpec;
  LRevision: Integer;
  LRejected: Boolean;
begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  try
    LDocument.State.SetValue(NyxTextState('🌙/reply'), 'Inherited reply')
      .SetValue(NyxTextState('other-reply'), 'Independent reply');
    LDefinition := TNyxNode.Create(nkColumn, 'reply-card');
    LDocument.AddComponent(LDefinition);
    LDefinitionMemo := TNyxNode.Create(nkMemo, 'definition-reply-memo');
    LDefinition.Add(LDefinitionMemo);
    LDefinitionMemo.Configure.PartName(NyxPart('editor')).Value('Authored fallback').Done;
    LDefinitionMemo.Binds.Value(NyxTextState('🌙/reply')).Done;
    LPage := TNyxNode.Create(nkPage, 'bindings-customization');
    LDocument.AddPage(LPage);
    LFirst := TNyxNode.Create(nkComponent, 'first');
    LPage.Add(LFirst);
    LFirst.Configure.Component(NyxComponent('reply-card')).Done;
    LFirst.OverridePart('editor').Binds.Clear(bpValue).Done;
    LSecond := TNyxNode.Create(nkComponent, 'second');
    LPage.Add(LSecond);
    LSecond.Configure.Component(NyxComponent('reply-card')).Done;
    LSecond.OverridePart('editor').Binds.Value(NyxTextState('other-reply'), bdFromState).Done;
    LThird := TNyxNode.Create(nkComponent, 'third');
    LPage.Add(LThird);
    LThird.Configure.Component(NyxComponent('reply-card')).Done;
    LCopy := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    try
      Check(TNyxCodec.Encode(LCopy) = TNyxCodec.Encode(LDocument),
        'binding overrides and unbinding survive wire ownership', Result);
    finally
      LCopy.Free;
    end;
    LRoot := RealizeNyxView(LDocument, LPage);
    LStore := LDocument.State.Clone;
    LLive := nil;
    try
      LLive := TNyxLiveBindings.Create(LRoot, LStore);
      LLive.Activate;
      LFirst := LRoot.Find(NyxQualifiedID('first', 'reply-card')).Part('editor');
      LSecond := LRoot.Find(NyxQualifiedID('second', 'reply-card')).Part('editor');
      LThird := LRoot.Find(NyxQualifiedID('third', 'reply-card')).Part('editor');
      Check(not LFirst.FindBinding(bpValue, LSpec) and
        (LFirst.Prop('value') = 'Authored fallback'), 'instance clears inherited binding', Result);
      Check((LSecond.Prop('value') = 'Independent reply') and
        (LThird.Prop('value') = 'Inherited reply'), 'instance binding leaves sibling contract intact', Result);
      LRevision := LStore.Revision;
      LLive.Edit(LFirst, 'Unbound edit');
      Check((LStore.Revision = LRevision) and
        (LStore.GetValue(NyxTextState('🌙/reply')) = 'Inherited reply'),
        'unbound instance edit does not write inherited state', Result);
      LRejected := False;
      try
        LLive.Edit(LSecond, 'Forbidden state-only edit');
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LSecond.Prop('value') = 'Independent reply') and
        (LStore.Revision = LRevision), 'state-only instance edit rejects without mutation', Result);
      LLive.Edit(LThird, 'Accepted inherited edit');
      Check((LStore.GetValue(NyxTextState('🌙/reply')) = 'Accepted inherited edit') and
        (LFirst.Prop('value') = 'Unbound edit') and (LSecond.Prop('value') = 'Independent reply'),
        'inherited edit respects independent instance bindings', Result);
      Check((LDefinitionMemo.Bindings[0].StateName = TNyxText('🌙/reply')) and
        (LDefinitionMemo.Prop('value') = 'Authored fallback') and
        (LDocument.State.GetValue(NyxTextState('🌙/reply')) = 'Inherited reply'),
        'runtime customization never mutates definition or defaults', Result);
    finally
      LLive.Free;
      LStore.Free;
      LRoot.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

function RunProjectionTests: Integer;
var
  LDocument: TNyxDocument;
  LPage: TNyxNode;
  LMemo: TNyxNode;
  LInput: TNyxNode;
  LNavigation: TNyxNode;
  LCommand: TNyxNode;
  LCopy: TNyxNode;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LRevision: Integer;
  LRejected: Boolean;
  LPlatform: TNyxPlatform;
  LDispatch: TNyxDispatch;
begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  try
    LDocument.State.SetValue(NyxTextState('🌙/reply'), 'First' + #13#10 + 'Second' + #13 + 'Third');
    LPage := TNyxNode.Create(nkPage, 'text-projection');
    LDocument.AddPage(LPage);
    LMemo := TNyxNode.Create(nkMemo, 'multiline-reply-memo');
    LPage.Add(LMemo);
    LMemo.Binds.Value(NyxTextState('🌙/reply')).Done;
    LNavigation := TNyxNode.Create(nkRow, 'compact-navigation');
    LPage.Add(LNavigation);
    LNavigation.Configure.Visible(False)
      .WhenViewport(TNyxViewportCondition.Any.WidthBelow(600)).Visible(True).Done;
    LCommand := TNyxNode.Create(nkButton, 'compact-command');
    LNavigation.Add(LCommand);
    LCommand.Configure.Text('Continue').Done;
    LRoot := RealizeNyxView(LDocument, LPage);
    LStore := LDocument.State.Clone;
    LLive := nil;
    try
      LLive := TNyxLiveBindings.Create(LRoot, LStore);
      LLive.Activate;
      { A real command candidate must preserve the effective ancestor policy.
        Ordinary stored-only clones remain suitable for independent view builds. }
      for LPlatform := npfBrowser to npfNativeLCL do
      begin
        LRoot.ApplyViewport(390, 844, LPlatform);
        LRevision := LStore.Revision;
        LDispatch := LLive.Dispatch(LRoot.Find('compact-command'), ntClick);
        Check((LDispatch.EventName <> '') and (LDispatch.Source = LRoot.Find('compact-command')) and
          not LDispatch.Changed and (LStore.Revision = LRevision),
          'responsive visible command retains its mounted policy and delivers its event', Result);
        LCopy := LRoot.CloneRuntimeProjection;
        try
          LRoot.ApplyViewport(1200, 844, LPlatform);
          Check((LCopy.Find('compact-navigation').Prop('visible') = 'true') and
            (LRoot.Find('compact-navigation').Prop('visible') = 'false'),
            'runtime command snapshot owns independent viewport overlays', Result);
        finally
          LCopy.Free;
        end;
        LRejected := False;
        try
          LLive.Dispatch(LRoot.Find('compact-command'), ntClick);
        except
          on ENyxState do
          begin
            LRejected := True;
          end;
        end;
        Check(LRejected and (LStore.Revision = LRevision),
          'currently hidden command still refuses without changing runtime state', Result);
        LRoot.ApplyViewport(390, 844, LPlatform);
        LCopy := LRoot.Clone;
        try
          Check((LCopy.Find('compact-navigation').Prop('visible') = 'false') and
            (LNavigation.Prop('visible') = 'false'),
            'stored clone and authored defaults retain their independent presentation semantics', Result);
        finally
          LCopy.Free;
        end;
      end;
      LMemo := LRoot.Find('multiline-reply-memo');
      Check((LMemo.Prop('value') = 'First' + #10 + 'Second' + #10 + 'Third') and
        (LStore.GetValue(NyxTextState('🌙/reply')) =
          'First' + #13#10 + 'Second' + #13 + 'Third'),
        'memo projection normalizes line endings without rewriting exact defaults', Result);
      LRevision := LStore.Revision;
      LLive.Edit(LMemo, LMemo.Prop('value'));
      Check((LStore.Revision = LRevision) and
        (LStore.GetValue(NyxTextState('🌙/reply')) = LDocument.State.GetValue(NyxTextState('🌙/reply'))),
        'unchanged projected value does not rewrite CRLF state', Result);
      LRejected := False;
      try
        LStore.SetValue(NyxTextState('🌙/reply'), 'First' + #0 + 'Second');
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.Revision = LRevision) and
        (LMemo.Prop('value') = 'First' + #10 + 'Second' + #10 + 'Third'),
        'lossy NUL control projection rejects before state publication', Result);
    finally
      LLive.Free;
      LStore.Free;
      LRoot.Free;
    end;
    LDocument.State.SetValue(NyxTextState('🌙/reply'), 'Accepted single line');
    LInput := TNyxNode.Create(nkInput, 'single-line-reply-input');
    LPage.Add(LInput);
    LInput.Binds.Value(NyxTextState('🌙/reply')).Done;
    LRoot := RealizeNyxView(LDocument, LPage);
    LStore := LDocument.State.Clone;
    LLive := nil;
    try
      LLive := TNyxLiveBindings.Create(LRoot, LStore);
      LLive.Activate;
      LRevision := LStore.Revision;
      LRejected := False;
      try
        LStore.SetValue(NyxTextState('🌙/reply'), 'First' + #10 + 'Second');
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.Revision = LRevision) and
        (LRoot.Find('single-line-reply-input').Prop('value') = 'Accepted single line') and
        (LRoot.Find('multiline-reply-memo').Prop('value') = 'Accepted single line'),
        'shared key honors the single-line consumer before publishing a multiline edit', Result);
    finally
      LLive.Free;
      LStore.Free;
      LRoot.Free;
    end;
    { Ordinary authored values share control representability rules with bound
      values, while the document itself remains an exact persistence source. }
    LInput.Binds.Clear(bpValue).Done;
    LMemo := TNyxNode.Create(nkMemo, 'unbound-reply-memo');
    LPage.Add(LMemo);
    LMemo.Configure.Value('First' + #13#10 + 'Second').Done;
    LRoot := RealizeNyxView(LDocument, LPage);
    try
      ApplyNyxBindings(LRoot, LDocument.State);
      Check((LRoot.Find('unbound-reply-memo').Prop('value') = 'First' + #10 + 'Second') and
        (LMemo.Prop('value') = 'First' + #13#10 + 'Second'),
        'unbound memo normalizes runtime text without rewriting authored source', Result);
    finally
      LRoot.Free;
    end;
    LInput.Configure.Value('First' + #10 + 'Second').Done;
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LInput.Prop('value') = 'First' + #10 + 'Second'),
      'unbound single-line text rejects lossy projection without changing the source', Result);
    LInput.Configure.Value('First' + #0 + 'Second').Done;
    LRejected := False;
    try
      TNyxCodegen.Generate(LDocument);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LInput.Prop('value') = 'First' + #0 + 'Second'),
      'generated control text refuses NUL while preserving the authored value', Result);
  finally
    LDocument.Free;
  end;
end;

function RunValuePlanTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LProbe: TValuePlanProbe;
  LRejected: Boolean;
  LFullCalls: Integer;
  LFieldCalls: Integer;
begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LProbe := TValuePlanProbe.Create;
  try
    LDocument.AddPage(NewNyxPage('field-review'));
    LDocument.Pages[0].Add(NewNyxInput('entry').Configure.Value('Original').Done);
    LDocument.Pages[0].Add(NewNyxSearchField('search'));
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LProbe.Root := LRoot;
    LProbe.Value := TNyxText('A complete value / 🌙');
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.OnSync := LProbe.FullSync;
    LLive.Activate;
    LLive.Edit(LRoot.Find('entry'), LProbe.Value);
    Check((LProbe.FullCalls = 1) and (LProbe.FieldCalls = 0),
      'legacy target synchronization remains the default', Result);
    LLive.OnValueSync := LProbe.FieldSync;
    LProbe.AcceptField := True;
    LProbe.Value := 'An independently admitted field';
    LLive.Edit(LRoot.Find('entry'), LProbe.Value);
    Check(LProbe.SawPublication and (LProbe.FullCalls = 1) and (LProbe.FieldCalls = 1),
      'field synchronization observes the exact fully published realized value', Result);
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('entry'), 'First line' + #10 + 'Second line');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRoot.Find('entry').Prop('value') = LProbe.Value) and
      (LProbe.FullCalls = 1) and (LProbe.FieldCalls = 1),
      'full representability admission refuses before either synchronization callback', Result);
    LProbe.AcceptField := False;
    LProbe.Value := 'Fallback remains available';
    LLive.Edit(LRoot.Find('entry'), LProbe.Value);
    Check(LProbe.SawPublication and (LProbe.FullCalls = 2) and (LProbe.FieldCalls = 2),
      'unsupported target field falls back to ordinary complete synchronization', Result);
    LProbe.Value := 'A compound search';
    LLive.Edit(LRoot.Find('search').Part('query'), LProbe.Value);
    LFullCalls := LProbe.FullCalls;
    LFieldCalls := LProbe.FieldCalls;
    LLive.Dispatch(LRoot.Find('search').Part('clear'), ntClick);
    Check((LProbe.FullCalls = LFullCalls + 1) and
      (LProbe.FieldCalls = LFieldCalls) and
      (LRoot.Find('search').Part('query').Prop('value') = ''),
      'compound actions retain full synchronization instead of an edit-only field plan', Result);
    LProbe.AcceptField := True;
    LProbe.RaiseField := True;
    LProbe.Value := 'Retained after notification failure';
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('entry'), LProbe.Value);
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('Intentional admitted field notification failure', LException.Message) > 0;
      end;
    end;
    Check(LRejected and LProbe.SawPublication and
      (LRoot.Find('entry').Prop('value') = LProbe.Value),
      'field notification failure retains the already admitted complete projection', Result);
    Check(LDocument.Find('entry').Prop('value') = 'Original',
      'field synchronization never rewrites authored values', Result);
  finally
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LProbe.Free;
    LDocument.Free;
  end;
end;

type
  { These probes retire authority, never raw owners, inside an entered command.
    Each test keeps the coordinator, realized roots and store until it unwinds.
    Successor owns only its subscription; the test owns its raw coordinator. }
  TBindingRetirementProbe = class
  public
    Live: TNyxLiveBindings;
    Successor: TNyxLiveBindings;
    SuccessorRoot: TNyxNode;
    Store: TNyxState;
    ReplaceOnSync: Boolean;
    RetireOnSync: Boolean;
    RetireOnValueSync: Boolean;
    RetireOnValidate: Boolean;
    RejectValidation: Boolean;
    FailNotification: Boolean;
    Calls: Integer;
    ValueCalls: Integer;
    SuccessorCalls: Integer;
    ValidatorCalls: Integer;
    ObserverCalls: Integer;
    SawEnteredCommand: Boolean;
    SawIdleStore: Boolean;
    SawBusyStore: Boolean;
    RejectedSyncRevival: Boolean;
    RejectedValueRevival: Boolean;
    EntryRevision: Integer;
    procedure AttemptCallbackRevival;
    procedure Sync;
    function ValueSync(ANode: TNyxNode): Boolean;
    procedure SuccessorSync;
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure Observe(AState: TNyxState; AChanges: TNyxStateChanges);
  end;

procedure TBindingRetirementProbe.AttemptCallbackRevival;
begin
  { Reassignment is attempted before this entered callback/command returns. }
  try
    Live.OnSync := Sync;
  except
    on LException: ENyxState do
    begin
      RejectedSyncRevival := Pos('cannot acquire a target callback',
        LException.Message) > 0;
    end;
  end;
  try
    Live.OnValueSync := ValueSync;
  except
    on LException: ENyxState do
    begin
      RejectedValueRevival := Pos('cannot acquire a value callback',
        LException.Message) > 0;
    end;
  end;
  Live.OnSync := nil;
  Live.OnValueSync := nil;
end;

procedure TBindingRetirementProbe.Sync;
begin
  Inc(Calls);
  SawEnteredCommand := Live.CommandActive;
  SawIdleStore := not Store.Busy;
  EntryRevision := Store.Revision;

  if RetireOnSync or ReplaceOnSync then
  begin
    RetireOnSync := False;
    Live.Retire;
    Live.Retire;
    AttemptCallbackRevival;
  end;

  if ReplaceOnSync then
  begin
    ReplaceOnSync := False;
    Successor := TNyxLiveBindings.Create(SuccessorRoot, Store);
    Successor.OnSync := SuccessorSync;
    Successor.Activate;
    { The old validator would reject removal of its still-bound key. No-op
      Apply has returned idle, so this is a legitimate same-store successor. }
    Store.Remove(NyxTextState('retiring-only'));
    Store.SetValue(NyxTextState('shared'), 'Successor accepted / 🌙');
  end;
end;

function TBindingRetirementProbe.ValueSync(ANode: TNyxNode): Boolean;
begin
  Inc(ValueCalls);
  Result := True;

  if RetireOnValueSync then
  begin
    RetireOnValueSync := False;
    SawEnteredCommand := Live.CommandActive;
    SawIdleStore := not Store.Busy;
    Live.Retire;
    AttemptCallbackRevival;
    { Request the ordinary fallback after retiring its receiver authority. }
    Result := False;
  end;
end;

procedure TBindingRetirementProbe.SuccessorSync;
begin
  Inc(SuccessorCalls);

  if SuccessorRoot.Find('replacement-entry').Prop('value') <>
    Store.GetValue(NyxTextState('shared')) then
  begin
    raise ENyxState.Create('Retirement successor sees an incomplete projection');
  end;
end;

procedure TBindingRetirementProbe.Validate(ACandidate: TNyxState;
  AChanges: TNyxStateChanges);
begin
  Inc(ValidatorCalls);

  if RejectValidation then
  begin
    raise ENyxState.Create('Intentional retirement fixture validation refusal');
  end;

  if RetireOnValidate then
  begin
    RetireOnValidate := False;
    SawBusyStore := Store.Busy;
    Live.Retire;
    Live.Retire;
  end;
end;

procedure TBindingRetirementProbe.Observe(AState: TNyxState;
  AChanges: TNyxStateChanges);
begin
  Inc(ObserverCalls);

  if FailNotification then
  begin
    raise ENyxState.Create('Intentional retirement fixture notification failure');
  end;
end;

{ Independent authored seed: the replacement deliberately omits the old-only
  binding. The shared public fixture and its existing journey stay unchanged. }
function CreateBindingRetirementFixture: TNyxDocument;
var
  LPage: INyxPage;
begin
  Result := TNyxDocument.Create;
  try
    Result.State.SetValue(NyxTextState('retiring-only'), 'Old accepted')
      .SetValue(NyxTextState('shared'), 'Shared accepted');
    LPage := NewNyxPage('retiring-page');
    Result.AddPage(LPage);
    LPage.Node.Add(NewNyxInput('retiring-entry'));
    LPage.Node.Find('retiring-entry').Binds.Value(NyxTextState('retiring-only')).Done;
    LPage := NewNyxPage('replacement-page');
    Result.AddPage(LPage);
    LPage.Node.Add(NewNyxInput('replacement-entry'));
    LPage.Node.Find('replacement-entry').Binds.Value(NyxTextState('shared')).Done;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function RunBindingIdleRetirementTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LSuccessorRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LUnused: TNyxLiveBindings;
  LProbe: TBindingRetirementProbe;
  LDispatch: TNyxDispatch;
  LRejected: Boolean;
  LRevision: Integer;
begin
  Result := 0;
  LDocument := CreateBindingRetirementFixture;
  LRoot := nil;
  LSuccessorRoot := nil;
  LStore := nil;
  LLive := nil;
  LUnused := nil;
  LProbe := TBindingRetirementProbe.Create;
  try
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LSuccessorRoot := RealizeNyxView(LDocument, LDocument.Pages[1]);
    LStore := LDocument.State.Clone;
    LUnused := TNyxLiveBindings.Create(LRoot, LStore);
    LUnused.Retire;
    LUnused.Retire;
    LRejected := False;
    try
      LUnused.Activate;
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('Retired live bindings', LException.Message) > 0;
      end;
    end;
    Check(LRejected and not LUnused.RefreshReady and
      (LStore.GetValue(NyxTextState('retiring-only')) = 'Old accepted'),
      'retirement before Activate is terminal and preserves borrowed owners', Result);
    FreeAndNil(LUnused);
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LProbe.Live := LLive;
    LProbe.Store := LStore;
    LProbe.SuccessorRoot := LSuccessorRoot;
    LProbe.ReplaceOnSync := True;
    LLive.OnSync := LProbe.Sync;
    LLive.OnValueSync := LProbe.ValueSync;
    LLive.Activate;
    LRevision := LStore.Revision;
    LDispatch := LLive.Dispatch(LRoot.Find('retiring-entry'), ntClick);
    Check(LProbe.SawEnteredCommand and LProbe.SawIdleStore and
      (LProbe.EntryRevision = LRevision),
      'no-assignment OnSync can retire while its command remains entered and store idle', Result);
    Check((LProbe.Calls = 1) and (LProbe.ValueCalls = 0) and
      not Assigned(LLive.OnSync) and not Assigned(LLive.OnValueSync) and
      not LLive.RefreshReady and not LLive.CommandActive,
      'retirement clears both callbacks and lets the retained entered command finish', Result);
    Check(LProbe.RejectedSyncRevival and LProbe.RejectedValueRevival,
      'neither callback can be reassigned during the retired entered command', Result);
    Check((LProbe.Successor <> nil) and LProbe.Successor.RefreshReady and
      not LStore.Has('retiring-only') and (LProbe.SuccessorCalls = 2) and
      (LStore.Revision = LRevision + 2),
      'same-store successor immediately replaces old validation and receives only its own changes', Result);
    Check((LDispatch.Source = LRoot.Find('retiring-entry')) and
      (LDispatch.Target = LRoot.Find('retiring-entry')) and
      (LRoot.Find('retiring-entry').Prop('value') = 'Old accepted') and
      (LDocument.State.GetValue(NyxTextState('retiring-only')) = 'Old accepted'),
      'Retire leaves returned borrowed identities and the independent authored seed intact', Result);
    LRejected := False;
    try
      LLive.Activate;
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('Retired live bindings', LException.Message) > 0;
      end;
    end;
    Check(LRejected, 'activated retirement cannot be revived by Activate', Result);
    LRejected := False;
    try
      LLive.Dispatch(LRoot.Find('retiring-entry'), ntClick);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LProbe.Calls = 1),
      'retired command refuses before reading a removed old binding', Result);
    LLive.Retire;
    LStore.SetValue(NyxTextState('shared'), 'Still only the successor');
    Check((LProbe.Calls = 1) and (LProbe.SuccessorCalls = 3) and
      (LSuccessorRoot.Find('replacement-entry').Prop('value') = 'Still only the successor'),
      'repeated retirement neither disconnects nor synchronizes the successor', Result);
  finally
    LUnused.Free;
    LLive.Free;
    LProbe.Successor.Free;
    LStore.Free;
    LSuccessorRoot.Free;
    LRoot.Free;
    LProbe.Free;
    LDocument.Free;
  end;
end;

function RunBindingEnteredRetirementTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LToken: TNyxStateSubscription;
  LProbe: TBindingRetirementProbe;
  LRevision: Integer;
  LRejected: Boolean;
begin
  Result := 0;
  LDocument := CreateBindingRetirementFixture;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LToken := nil;
  LProbe := TBindingRetirementProbe.Create;
  try
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LProbe.Live := LLive;
    LProbe.Store := LStore;
    LLive.OnSync := LProbe.Sync;
    LLive.OnValueSync := LProbe.ValueSync;
    LLive.Activate;
    LToken := LStore.Subscribe(LProbe.Observe, LProbe.Validate);
    LRevision := LStore.Revision;
    LProbe.RejectValidation := True;
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('retiring-entry'), 'Rejected');
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('Intentional retirement fixture validation refusal',
          LException.Message) > 0;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision) and
      (LRoot.Find('retiring-entry').Prop('value') = 'Old accepted') and
      (LProbe.Calls = 0) and (LProbe.ObserverCalls = 0) and LLive.RefreshReady,
      'ordinary validation refusal still preserves projection, revision and live authority', Result);
    LProbe.RejectValidation := False;
    LProbe.RetireOnValidate := True;
    LLive.Edit(LRoot.Find('retiring-entry'), 'Admitted while retiring');
    Check(LProbe.SawBusyStore and (LStore.Revision = LRevision + 1) and
      (LStore.GetValue(NyxTextState('retiring-only')) = 'Admitted while retiring') and
      (LRoot.Find('retiring-entry').Prop('value') = 'Admitted while retiring'),
      'disconnecting during validation preserves the admitted command and its retained projection', Result);
    Check((LProbe.Calls = 0) and (LProbe.ValueCalls = 0) and
      (LProbe.ObserverCalls = 1) and not LLive.CommandActive and
      not Assigned(LLive.OnSync) and not Assigned(LLive.OnValueSync),
      'removed old observer is skipped and neither revoked target callback runs', Result);
    FreeAndNil(LToken);
    FreeAndNil(LLive);
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LProbe.Live := LLive;
    LLive.OnSync := LProbe.Sync;
    LLive.OnValueSync := LProbe.ValueSync;
    LProbe.RetireOnSync := True;
    LProbe.FailNotification := True;
    LLive.Activate;
    LToken := LStore.Subscribe(LProbe.Observe, LProbe.Validate);
    LRevision := LStore.Revision;
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('retiring-entry'), 'Committed before notification failure');
    except
      on LException: ENyxStateNotification do
      begin
        LRejected := Pos('Intentional retirement fixture notification failure',
          LException.Message) > 0;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision + 1) and
      (LStore.GetValue(NyxTextState('retiring-only')) = 'Committed before notification failure') and
      (LRoot.Find('retiring-entry').Prop('value') = 'Committed before notification failure'),
      'retiring OnSync plus later notification failure retains committed state and projection', Result);
    Check((LProbe.Calls = 1) and not LProbe.SawIdleStore and
      LProbe.SawEnteredCommand and not LLive.CommandActive and not LLive.RefreshReady,
      'committed failure unwinds the retained command without reviving retired authority', Result);
    LProbe.FailNotification := False;
    LLive.Retire;
    LStore.SetValue(NyxTextState('retiring-only'), 'External committed successor value');
    Check((LProbe.Calls = 1) and (LProbe.ValueCalls = 0) and
      (LProbe.ObserverCalls = 3) and
      (LRoot.Find('retiring-entry').Prop('value') = 'Committed before notification failure'),
      'later writes do not touch retired projection while independent subscribers remain connected', Result);
  finally
    LToken.Free;
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LProbe.Free;
    LDocument.Free;
  end;
end;

function RunBindingValueRetirementTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LProbe: TBindingRetirementProbe;
  LRevision: Integer;
begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LProbe := TBindingRetirementProbe.Create;
  try
    LDocument.AddPage(NewNyxPage('retiring-value-page'));
    LDocument.Pages[0].Add(NewNyxInput('unbound-entry').Configure.Value('Old unbound').Done);
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LProbe.Live := LLive;
    LProbe.Store := LStore;
    LProbe.RetireOnValueSync := True;
    LLive.OnSync := LProbe.Sync;
    LLive.OnValueSync := LProbe.ValueSync;
    LLive.Activate;
    LRevision := LStore.Revision;
    LLive.Edit(LRoot.Find('unbound-entry'), 'Accepted unbound retirement');
    Check(LProbe.SawEnteredCommand and LProbe.SawIdleStore and
      (LProbe.ValueCalls = 1) and (LProbe.Calls = 0) and
      LProbe.RejectedSyncRevival and LProbe.RejectedValueRevival,
      'retiring value-only notification cannot revive either callback or run its requested full fallback', Result);
    Check((LRoot.Find('unbound-entry').Prop('value') = 'Accepted unbound retirement') and
      (LStore.Revision = LRevision) and not LLive.CommandActive and not LLive.RefreshReady and
      not Assigned(LLive.OnSync) and not Assigned(LLive.OnValueSync),
      'value-only retirement preserves admitted projection and unwinds without a state assignment', Result);
  finally
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LProbe.Free;
    LDocument.Free;
  end;
end;

function RunBindingResourceRetirementTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LPrepared: INyxPreparedPublication;
  LProbe: TBindingRetirementProbe;
  LRevision: Integer;
  LRejected: Boolean;
begin
  Result := 0;
  LDocument := CreateBindingRetirementFixture;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LProbe := TBindingRetirementProbe.Create;
  try
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LProbe.Live := LLive;
    LProbe.Store := LStore;
    LLive.OnSync := LProbe.Sync;
    LLive.OnValueSync := LProbe.ValueSync;
    LLive.Activate;
    LRevision := LStore.Revision;
    LPrepared := LLive.PrepareResourcePublication(LDocument.Resources,
      Default(TNyxLocaleRef), Default(TNyxLocaleRef));
    Check((LPrepared <> nil) and LLive.ResourcePublicationBusy,
      'a valid resource preparation exists before logical retirement', Result);
    LLive.Retire;
    LLive.Retire;
    LRejected := False;
    try
      LPrepared.Validate;
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('retired', LException.Message) > 0;
      end;
    end;
    Check(LRejected and not Assigned(LLive.OnSync) and not Assigned(LLive.OnValueSync),
      'retirement revokes the retained resource receiver and both target callbacks', Result);
    LPrepared.Retire;
    LPrepared := nil;
    LRejected := False;
    try
      LPrepared := LLive.PrepareResourcePublication(LDocument.Resources,
        Default(TNyxLocaleRef), Default(TNyxLocaleRef));
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('active idle bindings', LException.Message) > 0;
      end;
    end;
    Check(LRejected and (LPrepared = nil) and (LStore.Revision = LRevision) and
      (LRoot.Find('retiring-entry').Prop('value') = 'Old accepted') and (LProbe.Calls = 0),
      'retired resource admission refuses without changing accepted data or notifying', Result);
    LStore.SetValue(NyxTextState('retiring-only'), 'After abandoned preparation');
    Check((LProbe.Calls = 0) and
      (LRoot.Find('retiring-entry').Prop('value') = 'Old accepted'),
      'retiring the abandoned preparation releases its hold without restoring the old subscription', Result);
  finally
    LPrepared := nil;
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LProbe.Free;
    LDocument.Free;
  end;
end;

function RunNyxBindingTests: Integer;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LProbe: TBindingProbe;
  LToken: TNyxStateSubscription;
  LMemo: TNyxNode;
  LDispatch: TNyxDispatch;
  LRevision: Integer;
  LRejected: Boolean;
  LSource: TNyxText;
begin
  Result := RunMetadataTests + RunReusableBindingTests + RunProjectionTests + RunValuePlanTests;
  Inc(Result, RunBindingIdleRetirementTests + RunBindingEnteredRetirementTests +
    RunBindingValueRetirementTests + RunBindingResourceRetirementTests);
  LDocument := CreateNyxBindingFixture;
  try
    LCopy := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    try
      Check(TNyxCodec.Encode(LCopy) = TNyxCodec.Encode(LDocument),
        'typed binding wire round trip', Result);
      LCopy.Find('reply-memo').Binds.Clear(bpValue).Done;
      Check(LDocument.Find('reply-memo').Bindings[0].StateName = TNyxText('🌙/reply'),
        'clearing a copied binding preserves the authored baseline', Result);
      LSource := TNyxCodegen.Generate(LCopy);
      Check((Pos('.Clear(bpValue)', LSource) > 0) and
        (Pos('.Value(LRatioNumberState)', LSource) > 0) and
        (Pos('LRatioNumberState: TNyxNumberStateRef;', LSource) > 0) and
        (Pos('LReplyMemo.Binds', LSource) > 0),
        'crafted generated binding calls use typed references and purposes', Result);
    finally
      LCopy.Free;
    end;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LProbe := TBindingProbe.Create;
    LProbe.Root := LRoot;
    LProbe.Store := LStore;
    LLive := nil;
    LToken := nil;
    try
      LLive := TNyxLiveBindings.Create(LRoot, LStore);
      LLive.OnSync := LProbe.Sync;
      LLive.OnValueSync := LProbe.ValueSync;
      LLive.Activate;
      LToken := LStore.Subscribe(nil, LProbe.Validate);
      LMemo := LRoot.Find('reply-memo');
      Check((LMemo.Prop('value') = TNyxText('Café / 🌙 / 漢字')) and
        (LRoot.Find('quantity-caption').Prop('text') = '2') and
        (LRoot.Find('ratio-input').Prop('value') = '0.1'),
        'initial typed scalar projection', Result);
      LDispatch := LLive.Edit(LMemo, 'Crafted / 🌙' + #10 + 'Second line');
      Check((LDispatch.Source = LMemo) and (LDispatch.EventName = 'change') and
        (LStore.GetValue(NyxTextState('🌙/reply')) = LMemo.Prop('value')) and
        (LRoot.Find('reply-mirror').Prop('value') = LMemo.Prop('value')) and
        (LRoot.Find('reply-caption').Prop('text') = LMemo.Prop('value')),
        'one accepted edit synchronizes all consumers', Result);
      Check(LDocument.State.GetValue(NyxTextState('🌙/reply')) = TNyxText('Café / 🌙 / 漢字'),
        'runtime edits preserve authored defaults', Result);
      Check(LRoot.Find('reply-memo') = LMemo, 'projection retains node identity', Result);
      Check(LProbe.ValueCalls = 0,
        'live state bindings retain complete synchronization despite an available field hook', Result);
      LRevision := LStore.Revision;
      LLive.Edit(LMemo, LMemo.Prop('value'));
      Check(LStore.Revision = LRevision, 'no-op edit preserves revision', Result);
      LLive.Edit(LRoot.Find('remember-checkbox'), 'true');
      Check(LStore.GetValue(NyxBooleanState('checked')), 'Boolean edit retains Boolean meaning', Result);
      LLive.Edit(LRoot.Find('ratio-input'), '0.125');
      Check(LStore.GetValue(NyxNumberState('ratio')) = 0.125,
        'number edit retains Double meaning', Result);
      LLive.Dispatch(LRoot.Find('quantity-stepper').Part('increment'), ntClick);
      Check((LStore.GetValue(NyxIntegerState('quantity')) = 3) and
        (LRoot.Find('quantity-caption').Prop('text') = '3'),
        'compound step updates integer state and caption', Result);
      LProbe.Reject := True;
      LRevision := LStore.Revision;
      LRejected := False;
      try
        LLive.Dispatch(LRoot.Find('search').Part('clear'), ntClick);
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.Revision = LRevision) and
        (LRoot.Find('search').Part('query').Prop('value') = LMemo.Prop('value')),
        'rejected compound clear preserves the complete accepted projection', Result);
      LProbe.Reject := False;
      LLive.Dispatch(LRoot.Find('search').Part('clear'), ntClick);
      Check((LStore.GetValue(NyxTextState('🌙/reply')) = '') and
        (LMemo.Prop('value') = ''), 'accepted compound clear writes state once', Result);
      LRevision := LStore.Revision;
      LRejected := False;
      try
        LLive.Edit(LRoot.Find('ratio-input'), '0.1oops');
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.Revision = LRevision) and
        (LRoot.Find('ratio-input').Prop('value') = '0.125'),
        'incomplete number is rejected without accepted mutation', Result);
      LRejected := False;
      try
        LStore.SetValue(NyxIntegerState('quantity'), 1000);
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.Revision = LRevision) and
        (LStore.GetValue(NyxIntegerState('quantity')) = 3),
        'external out-of-range update fails before publication', Result);
      LRejected := False;
      try
        LStore.Remove(NyxTextState('🌙/reply'));
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and LStore.Has('🌙/reply') and (LStore.Revision = LRevision),
        'removing a mounted bound key is rejected', Result);
      LStore.SetValue(NyxBooleanState('readonly'), True);
      LRejected := False;
      try
        LLive.Edit(LMemo, 'Forbidden');
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LMemo.Prop('value') = ''), 'read-only edit is rejected', Result);
      LStore.SetValue(NyxBooleanState('enabled'), False);
      LRejected := False;
      try
        LLive.Dispatch(LRoot.Find('quantity-stepper').Part('increment'), ntClick);
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.GetValue(NyxIntegerState('quantity')) = 3),
        'ancestor disabled state blocks compound commands', Result);
      LStore.Apply([NyxStateValue(NyxBooleanState('enabled'), True),
        NyxStateValue(NyxBooleanState('readonly'), False),
        NyxStateValue(NyxIntegerState('width'), 420),
        NyxStateValue(NyxBooleanState('visible'), False)]);
      Check((LMemo.Prop('width') = '420') and
        (LRoot.Find('reply-caption').Prop('visible') = 'false'),
        'one batch projects layout and Boolean properties', Result);
      LLive.Free;
      LLive := nil;
      LRevision := LProbe.Calls;
      LStore.SetValue(NyxTextState('🌙/reply'), 'After unmount');
      Check(LProbe.Calls = LRevision, 'destroyed view disconnects its subscription', Result);
    finally
      LToken.Free;
      LLive.Free;
      LProbe.Free;
      LStore.Free;
      LRoot.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

end.
