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
unit nyx.test.source.canvas;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.studio.projects;

{ English fixture shared by immutable replay and actual native editor input.
  The returned pair owns its values; no document/interface is retained. }
function NyxCanvasSourceFixture: TNyxProjectPair;
{ Borrow an exact ordinary field or named reusable part from a caller-owned
  realization. The result never survives that root; nil means absent identity. }
function NyxCanvasSourceField(ARoot: TNyxNode; const AOwner: TNyxText;
  const APart: TNyxText = ''): TNyxNode;
{ Wire, fresh platform/default admission, named-part ownership and exact paired
  history. ACompiledPair supplies a real admitted canvas companion for compilation.
  These checks do not establish physical target input or browser execution. }
function RunNyxCanvasSourceTests(out ACompiledPair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.types, nyx.controls, nyx.state, nyx.binding.types, nyx.binding,
  nyx.composition, nyx.platform, nyx.codec, nyx.codegen, nyx.data, nyx.schema,
  nyx.source.preparation, nyx.studio.session;

function NyxCanvasSourceFixture: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LMemo: INyxMemo;
  LInput: INyxInput;
  LBody: INyxColumn;
  LCard: INyxColumn;
  LReference: INyxComponent;
  LParts: TNyxStrings;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Canvas input review';
    LDocument.State.SetValue(NyxTextState('reply'), 'English state default')
      .SetValue(NyxIntegerState('quantity'), 2)
      .SetValue(NyxBooleanState('locked'), True);
    LMemo := NewNyxMemo('definition-reply');
    LMemo.Configure.PartName(NyxPart('editor')).Text('Write a reply')
      .Value('English template default').Done;
    LBody := NewNyxColumn('reply-body');
    LBody.Configure.PartName(NyxPart('body')).Done;
    LBody.Add(LMemo);
    LCard := NewNyxColumn('reply-card');
    LCard.Add(LBody);
    LDocument.AddComponent(LCard);
    LReference := NewNyxComponent('nested-reply');
    LReference.Configure.Component(NyxComponent('reply-card')).PartName(NyxPart('nested')).Done;
    LDocument.AddComponent(NewNyxColumn('envelope').Add(LReference));
    LDocument.AddPage(NewNyxPage('home').Add(NewNyxLabel('caption').WithText('Canvas input')));
    LReference := NewNyxComponent('first');
    LReference.Configure.Component(NyxComponent('reply-card')).Done;
    LDocument.Pages[0].Add(LReference);
    LReference := NewNyxComponent('second');
    LReference.Configure.Component(NyxComponent('reply-card')).Done;
    LDocument.Pages[0].Add(LReference);
    LReference := NewNyxComponent('wrapped');
    LReference.Configure.Component(NyxComponent('envelope')).Done;
    LDocument.Pages[0].Add(LReference);
    { The ordinary unbound page qualifies retained fields. A separate state page
      deliberately exercises the existing full-projection binding/context guard. }
    LDocument.AddPage(NewNyxPage('state'));
    LMemo := NewNyxMemo('bound-reply');
    LMemo.Configure.Text('Shared reply').Value('Explicit fallback').Done;
    LMemo.Binds.Value(NyxTextState('reply')).Done;
    LDocument.Pages[1].Add(LMemo);
    LMemo := NewNyxMemo('projected-reply');
    LMemo.Configure.Text('State projection').Done;
    LMemo.Binds.Value(NyxTextState('reply'), bdFromState).Done;
    LDocument.Pages[1].Add(LMemo);
    LMemo := NewNyxMemo('locked-reply');
    LMemo.Configure.Text('Read only reply').Done;
    LMemo.Binds.Value(NyxTextState('reply')).ReadOnly(NyxBooleanState('locked')).Done;
    LDocument.Pages[1].Add(LMemo);
    LInput := NewNyxInput('quantity');
    LInput.Configure.Text('Quantity').InputType(niNumber).Minimum(0).Maximum(9).Done;
    LInput.Binds.Value(NyxIntegerState('quantity')).Done;
    LDocument.Pages[1].Add(LInput);
    LMemo := NewNyxMemo('platform-reply');
    LMemo.Configure.Text('Platform policy').ReadOnly(True)
      .ForPlatform(npfBrowser).ReadOnly(False).Done;
    LDocument.Pages[0].Add(LMemo);
    LParts := TNyxStrings.Create;
    try
      LParts.Add('{ Handwritten canvas helper / supplementary Unicode: 🌙 }');
      LParts.Add(TNyxCodegen.Generate(LDocument));
      Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LParts.Join(#10));
    finally
      LParts.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

function NyxCanvasSourceField(ARoot: TNyxNode; const AOwner, APart: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if ARoot = nil then
  begin
    Exit;
  end;

  if ARoot.DesignID = AOwner then
  begin

    if APart = '' then
    begin
      Exit(ARoot);
    end;
    Exit(ARoot.Part(APart));
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    Result := NyxCanvasSourceField(ARoot.Children[LIndex], AOwner, APart);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

function RunNyxCanvasSourceTests(out ACompiledPair: TNyxProjectPair): Integer;
var
  LSession: TNyxStudioSession;
  LSchemas: INyxSchemaSnapshot;
  LRequest: TNyxStudioDesignRequest;
  LWire: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LReceived: INyxPreparedDesign;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LDefinition: TNyxText;
  LEdit: TNyxStudioDesignEdit;
  LRoot: TNyxNode;
  LField: TNyxNode;
  LVariant: Integer;
  LRefused: Boolean;
  LData: TNyxDataValue;
  LMountContext: TNyxStudioCommandContext;

  function WithField(const AObject: TNyxDataValue; const AName: TNyxText;
    const AValue: TNyxDataValue): TNyxDataValue;
  var
    LIndex: Integer;
    LFields: array of TNyxDataField;
  begin
    SetLength(LFields, AObject.Count);
    for LIndex := 0 to High(LFields) do
    begin
      LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

      if LFields[LIndex].Name = AName then
      begin
        LFields[LIndex] := NyxField(AName, AValue);
      end;
    end;
    Result := NyxObject(LFields);
  end;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Canvas source: ' + AReason);
    end;
    Inc(Result);
  end;

  function Proposal(const AOwner, APart, AValue: TNyxText;
    APlatform: TNyxPlatform = npfNativeLCL): TNyxStudioDesignEdit;
  var
    LView: TNyxNode;
    LInput: TNyxNode;
  begin
    LView := RealizeNyxView(LSession.Document, LSession.ActiveView);
    try
      ApplyNyxPlatform(LView, APlatform);
      ApplyNyxBindings(LView, LSession.Document.State);
      LInput := NyxCanvasSourceField(LView, AOwner, APart);
      Check(LInput <> nil, 'Fixture resolves the exact requested field');
      { Genuine adapter proposals are wire text, including deliberately invalid
        numeric drafts. Typed authoring rejects those before this boundary. }
      LInput.SetProp(NyxAttributeName(atValue), AValue);
      Result := LSession.CaptureCanvasValue(LInput, APlatform, LSession.CommandContext);
    finally
      LView.Free;
    end;
  end;

  function Replay(const AEdit: TNyxStudioDesignEdit): TNyxSourceCompletion;
  begin
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(AEdit, LSchemas.Revision);
    LWire := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    Check(LWire.SameRequest(LRequest), 'Private wire owns exact field/platform/value and pair');
    LBefore := LSession.ProjectSnapshot;
    LPrepared := PrepareNyxStudioDesign(LWire, LSchemas);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'Independent replay does not touch accepted nodes, draft or history');
    LReceived := ReceiveNyxPreparedDesign(TNyxDataValue.ParseJSON(LPrepared.ToData.ToJSON),
      LRequest, LSchemas);
    Result := LSession.CompleteDesignRequest(LRequest, LReceived);
    LReceived := nil;
    LPrepared := nil;
  end;

begin
  Result := 0;
  ACompiledPair := Default(TNyxProjectPair);
  LSession := TNyxStudioSession.Create(NyxCanvasSourceFixture);
  try
    LSession.Select('caption');
    LDefinition := TNyxDataValue.ParseJSON(LSession.Save).Field('components').Item(0).ToJSON;
    LBefore := LSession.ProjectSnapshot;
    LEdit := Proposal('first', 'body/editor', TNyxText('Crafted reply / 🌙') + #10 + 'Second line');
    Check(Replay(LEdit) = nscApplied, 'Named inherited canvas field admits one isolated edit');
    Check((LSession.Document.Find('first').Count = 1) and
      (LSession.Document.Find('first').Children[0].Prop('path') = 'body/editor'),
      'Instance receives its own direct named-part override');
    Check(TNyxDataValue.ParseJSON(LSession.Save).Field('components').Item(0).ToJSON = LDefinition,
      'Shared reusable definition remains byte-for-byte independent');
    Check((LSession.Document.Find('second').Count = 0) and (LSession.SelectedID = 'caption'),
      'Sibling instance and independent selection remain untouched');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check((LSession.Save = LBefore.Design) and (LSession.Source = LBefore.Source) and
      not LSession.CanUndo, 'One Undo restores the exact pair without processor history');
    LSession.Redo;
    Check((LSession.Save = LAfter.Design) and (LSession.Source = LAfter.Source),
      'Redo restores the admitted instance-only pair');
    LEdit := Proposal('wrapped', 'nested/body/editor', 'Nested English reply');
    Check(Replay(LEdit) = nscApplied, 'Expanded nested reference follows the outer named path');
    Check(LSession.Document.Find('wrapped').Children[0].Prop('path') = 'nested/body/editor',
      'Nested reference customization belongs to the outer authored instance');
    LSession.Activate('state');
    LEdit := Proposal('bound-reply', '', 'Shared English reply / 🌙');
    Check(Replay(LEdit) = nscApplied, 'Two-way canvas input updates its typed authored default');
    Check((LSession.Document.State.GetValue(NyxTextState('reply')) =
      TNyxText('Shared English reply / 🌙')) and
      (LSession.Document.Find('bound-reply').Prop('value') = 'Explicit fallback'),
      'State default changes without replacing the explicit fallback property');
    LEdit := Proposal('quantity', '', '0003');
    Check(Replay(LEdit) = nscApplied, 'Numeric wire input admits the exact Integer contract');
    Check(LSession.Document.State.GetValue(NyxIntegerState('quantity')) = 3,
      'Leading-zero input remains a typed Integer default');
    ACompiledPair := LSession.ProjectSnapshot;
    Check(Replay(LEdit) = nscUnchanged, 'Same typed default creates no extra history');

    for LVariant := 0 to 3 do
    begin
      case LVariant of
        0: LEdit := Proposal('projected-reply', '', 'Forbidden projection');
        1: LEdit := Proposal('locked-reply', '', 'Forbidden read-only edit');
        2: LEdit := Proposal('quantity', '', 'not an integer');
        3: LEdit := Proposal('quantity', '', '100');
      end;
      LBefore := LSession.ProjectSnapshot;
      Check(Replay(LEdit) = nscRejected, 'Wrong direction/policy/type/range refuses atomically');
      Check((LSession.Source = LBefore.Source) and (LSession.Save = LBefore.Design),
        'Refused canvas input retains the complete accepted pair');
    end;
    LSession.Activate('home');
    LEdit := Proposal('platform-reply', '', 'Native policy refuses');
    Check(Replay(LEdit) = nscRejected, 'Native replay respects its concrete platform policy');
    LEdit := Proposal('platform-reply', '', 'Browser policy allows', npfBrowser);
    Check(Replay(LEdit) = nscApplied, 'Browser replay respects its distinct typed override');
    LEdit := Proposal('first', 'body/editor', 'Pending source retained');
    LSession.SetSourceDraft(LSession.Source + TNyxText(#10 + '// Exact pending helper / 🌙'));
    LBefore := LSession.ProjectSnapshot;
    Check(Replay(LEdit) = nscApplied, 'Canvas command admits beside a pending application draft');
    Check((LSession.DraftSource = LBefore.Draft) and
      (LSession.SourceDraftBase = LBefore.DraftBase), 'Exact draft and original base survive');
    LSession.DiscardSourceDraft;
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.LoadProject(LBefore);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale,
      'Same-ID project load retires an earlier captured field');
    LPrepared := nil;

    LRoot := RealizeNyxView(LSession.Document, LSession.ActiveView);
    LMountContext := LSession.CommandContext;
    try
      LField := NyxCanvasSourceField(LRoot, 'first', 'body/editor');
      LSession.LoadProject(LSession.ProjectSnapshot);
      LRefused := False;
      try
        LSession.CaptureCanvasValue(LField, npfNativeLCL, LMountContext);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Old mounted field cannot edit a same-ID reloaded project');
      LSession.Activate('reply-card');
      LRefused := False;
      try
        LSession.CaptureCanvasValue(LField, npfNativeLCL, LSession.CommandContext);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Old mounted-view proposal cannot follow later navigation');
    finally
      LRoot.Free;
    end;
    LSession.Activate('home');
    LEdit := Proposal('first', 'body/editor', 'Current wire proposal');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LData := LRequest.ToData;
    LData := WithField(LData, 'edit', WithField(LData.Field('edit'), 'platform', NyxData(999)));
    LRefused := False;
    try
      ReadNyxStudioDesignRequest(LData);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Private wire refuses platform values outside the enum');
  finally
    LReceived := nil;
    LPrepared := nil;
    LSchemas := nil;
    LSession.Free;
  end;
end;

end.
