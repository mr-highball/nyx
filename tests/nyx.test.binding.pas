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
    Reject: Boolean;
    procedure Sync;
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
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LRevision: Integer;
  LRejected: Boolean;
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
    LRoot := RealizeNyxView(LDocument, LPage);
    LStore := LDocument.State.Clone;
    LLive := nil;
    try
      LLive := TNyxLiveBindings.Create(LRoot, LStore);
      LLive.Activate;
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
  Result := RunMetadataTests + RunReusableBindingTests + RunProjectionTests;
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
