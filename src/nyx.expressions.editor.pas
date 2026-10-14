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

unit nyx.expressions.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.binding.types, nyx.state, nyx.expressions;

type
  TNyxExpressionEditorField = (eefSource, eefPreview, eefApply, eefSummary);
  { Exact proposal owns descriptor/source context only. Its expression interface
    is reconstructed on demand through the immutable binding specification. }
  TNyxExpressionProposal = record
  private
    FOwner: TNyxControlRef;
    FTarget: TNyxBindingProperty;
    FBaseline: TNyxText;
    FBinding: TNyxBindingSpec;
  public
    property Owner: TNyxControlRef read FOwner;
    property Target: TNyxBindingProperty read FTarget;
    property Baseline: TNyxText read FBaseline;
    property Binding: TNyxBindingSpec read FBinding;
  end;
  { Specialized managed compound. Only its owned control tree is retained;
    source/document/state are borrowed during construction and proposal reading.
    Direct callers and Studio use the same source field and named parts. }
  INyxExpressionEditor = interface(INyxCard)
    ['{EF006C51-336A-4E80-A87D-00A5193C1020}']
    function GetOwner: TNyxControlRef;
    function GetTarget: TNyxBindingProperty;
    function GetSource: TNyxText;
    procedure SetSource(const AValue: TNyxText);
    property Owner: TNyxControlRef read GetOwner;
    property Target: TNyxBindingProperty read GetTarget;
    property Source: TNyxText read GetSource write SetSource;
  end;
  TNyxExpressionEditor = class(TNyxCard, INyxExpressionEditor)
  public
    function GetOwner: TNyxControlRef;
    function GetTarget: TNyxBindingProperty;
    function GetSource: TNyxText;
    procedure SetSource(const AValue: TNyxText);
  end;
  { Park unfinished fluent Pascal independently of a mounted form. Restore only
    the matching owner/target/default/binding context; changed context retires it.
    Capture with an absent form leaves a parked draft intact. }
  TNyxExpressionEditorDraft = record
  private
    FEditor: TNyxText;
    FBaseline: TNyxText;
    FSource: TNyxText;
  public
    procedure Capture(const AEditor: TNyxText; ARoot: TNyxNode);
    function Restore(ARoot: TNyxNode): Boolean;
    procedure Clear;
  end;

function NyxExpressionEditorFieldID(const AEditor: TNyxText;
  AField: TNyxExpressionEditorField): TNyxText;
{ Copy the exact binding/default context. Missing owners/unsupported targets
  refuse; returned context owns no model/renderer and is never agent authority. }
function NyxExpressionEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty): TNyxText;
{ Public Card/CodeEditor/Button/Code composition. The writable field contains a closed
  fluent Pascal expression, not JSON or executable arbitrary source. }
function NewNyxExpressionEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty): INyxExpressionEditor;
{ False means an unrelated action. A recognized foreign/stale/malformed form
  refuses; proposal admission parses, evaluates defaults and validates a detached
  full document before returning. Neither model nor shell field is changed. }
function ReadNyxExpressionEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AProposal: TNyxExpressionProposal;
  out APreview: Boolean): Boolean;
{ Update only this form's summary after a successful read. Adapters synchronize
  their existing mount; preview never publishes project/history changes. }
procedure ShowNyxExpressionPreview(ARoot: TNyxNode; const AEditor: TNyxText;
  const AProposal: TNyxExpressionProposal; ADocument: TNyxDocument);

implementation

uses
  Math, nyx.data, nyx.binding, nyx.codegen, nyx.source, nyx.composition;

const
  { Private chrome metadata is an explicit projection boundary. Stable names
    carry copied owner/enum/baseline values; routing decodes typed choices and
    checks exact ownership rather than dispatching executable strings. }
  CEditor: TNyxText = 'nyx.expression.editor';
  COwner: TNyxText = 'nyx.expression.owner';
  CTarget: TNyxText = 'nyx.expression.target';
  CBaseline: TNyxText = 'nyx.expression.baseline';
  CFields: array[TNyxExpressionEditorField] of TNyxText =
    ('source', 'preview', 'apply', 'summary');

function NyxExpressionEditorFieldID(const AEditor: TNyxText;
  AField: TNyxExpressionEditorField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxExpressionEditorField))) or
    (Ord(AField) > Ord(High(TNyxExpressionEditorField))) then
  begin
    raise ENyxExpression.Create('Unknown expression form field');
  end;
  Result := AEditor + '-' + CFields[AField];
end;

function StateData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  case AValue.Kind of
    nskText:
      begin
        Result := NyxData(AValue.TextValue);
      end;
    nskBoolean:
      begin
        Result := NyxData(AValue.BooleanValue);
      end;
    nskInteger:
      begin
        Result := NyxData(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        { Decimal spelling stays exact across native/browser baseline comparison. }
        Result := NyxData(AValue.NumberText);
      end;
  end;
end;

function BindingData(const ASpec: TNyxBindingSpec): TNyxDataValue;
var
  LValue: TNyxDataValue;
begin
  LValue := NyxNull;

  if not ASpec.Cleared then
  begin
    case ASpec.Source of
      bsState:
        begin
          LValue := NyxData(ASpec.StateName);
        end;
      bsExpression:
        begin
          LValue := ASpec.Expression.ToData;
        end;
      bsResource:
        begin
          LValue := ASpec.ResourceValue.ToData;
        end;
      bsResourceImage:
        begin
          LValue := ASpec.ResourceImage.ToData;
        end;
    end;
  end;
  Result := NyxObject([
    NyxField('cleared', NyxData(ASpec.Cleared)),
    NyxField('source', NyxData(Ord(ASpec.Source))),
    NyxField('kind', NyxData(Ord(ASpec.ValueKind))),
    NyxField('direction', NyxData(Ord(ASpec.Direction))),
    NyxField('value', LValue)]);
end;

function NyxExpressionEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty): TNyxText;
var
  LOwner: TNyxNode;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LLocal: TNyxDataValue;
  LEffective: TNyxDataValue;
  LDefaults: array of TNyxDataValue;
  LSpec: TNyxBindingSpec;
  LValue: TNyxStateValue;
  LIndex: Integer;
begin

  if ADocument = nil then
  begin
    raise ENyxExpression.Create('Expression form requires its owning document');
  end;
  LOwner := ADocument.Find(AOwner.ID);

  if LOwner = nil then
  begin
    raise ENyxExpression.Create('Expression form owner is missing');
  end;
  LRoot := RealizeNyxContext(ADocument, LOwner, LProjection);
  try

    if (LProjection = nil) or (NyxBindingKinds(LProjection, ATarget) = []) then
    begin
      raise ENyxExpression.Create('This property exposes no scalar expression contract');
    end;
    LLocal := NyxNull;
    LEffective := NyxNull;
    for LIndex := 0 to LOwner.BindingCount - 1 do
    begin

      if LOwner.Bindings[LIndex].Target = ATarget then
      begin
        LLocal := BindingData(LOwner.Bindings[LIndex]);
      end;
    end;

    if LProjection.FindBinding(ATarget, LSpec) then
    begin
      LEffective := BindingData(LSpec);
    end;
    SetLength(LDefaults, ADocument.State.Count);
    for LIndex := 0 to High(LDefaults) do
    begin
      LValue := ADocument.State.Value(ADocument.State.Key(LIndex));
      LDefaults[LIndex] := NyxObject([
        NyxField('name', NyxData(ADocument.State.Key(LIndex))),
        NyxField('kind', NyxData(NyxStateKindName(LValue.Kind))),
        NyxField('value', StateData(LValue))]);
    end;
    Result := NyxObject([
      NyxField('owner', NyxData(AOwner.ID)), NyxField('kind', NyxData(LOwner.Kind)),
      NyxField('projection', NyxData(LProjection.ProjectionKind)),
      NyxField('target', NyxData(NyxBindingPropertyName(ATarget))),
      NyxField('local', LLocal), NyxField('effective', LEffective),
      NyxField('defaults', NyxArray(LDefaults))]).ToJSON;
  finally
    LRoot.Free;
  end;
end;

function TNyxExpressionEditor.GetOwner: TNyxControlRef;
begin
  Result := NyxControl(Node.Prop(COwner));
end;

function TNyxExpressionEditor.GetTarget: TNyxBindingProperty;
begin

  if not TryNyxBindingProperty(Node.Prop(CTarget), Result) then
  begin
    raise ENyxExpression.Create('Expression form target changed');
  end;
end;

function TNyxExpressionEditor.GetSource: TNyxText;
begin
  Result := (Part(NyxPart('source')) as INyxCodeEditor).Value;
end;

procedure TNyxExpressionEditor.SetSource(const AValue: TNyxText);
begin
  Part(NyxPart('source')).Configure.Value(AValue).Done;
end;

function InitialExpression(ADocument: TNyxDocument; AOwner: TNyxNode;
  ATarget: TNyxBindingProperty): INyxExpression;
var
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxBindingSpec;
  LKind: TNyxStateKind;
  LAllowed: TNyxStateKinds;
  LValue: TNyxStateValue;
  LName: TNyxText;
  LIndex: Integer;
begin
  LRoot := RealizeNyxContext(ADocument, AOwner, LProjection);
  try

    if LProjection.FindBinding(ATarget, LSpec) and (LSpec.Source = bsExpression) then
    begin
      Exit(LSpec.Expression);
    end;
    LAllowed := NyxBindingKinds(LProjection, ATarget);
    for LIndex := 0 to ADocument.State.Count - 1 do
    begin
      LName := ADocument.State.Key(LIndex);
      LValue := ADocument.State.Value(LName);

      if LValue.Kind in LAllowed then
      begin
        case LValue.Kind of
          nskText:
            begin
              Exit(NyxTextExpression(NyxTextState(LName)));
            end;
          nskBoolean:
            begin
              Exit(NyxBooleanExpression(NyxBooleanState(LName)));
            end;
          nskInteger:
            begin
              Exit(NyxIntegerExpression(NyxIntegerState(LName)));
            end;
          nskNumber:
            begin
              Exit(NyxNumberExpression(NyxNumberState(LName)));
            end;
        end;
      end;
    end;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if LKind in LAllowed then
      begin
        case LKind of
          nskText:
            begin
              Exit(NyxTextExpression(''));
            end;
          nskBoolean:
            begin
              Exit(NyxBooleanExpression(True));
            end;
          nskInteger:
            begin
              Exit(NyxIntegerExpression(0));
            end;
          nskNumber:
            begin
              Exit(NyxNumberExpression(0.0));
            end;
        end;
      end;
    end;
    raise ENyxExpression.Create('Expression property has no supported scalar family');
  finally
    LRoot.Free;
  end;
end;

function NewNyxExpressionEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty): INyxExpressionEditor;
var
  LBaseline: TNyxText;
  LExpression: INyxExpression;
  LButton: INyxButton;
  LField: TNyxExpressionEditorField;
begin
  LBaseline := NyxExpressionEditorBaseline(ADocument, AOwner, ATarget);
  LExpression := InitialExpression(ADocument, ADocument.Find(AOwner.ID), ATarget);
  Result := TNyxExpressionEditor.Create(AID);
  Result.Configure.Padding(12).Gap(8).Done;
  Result.Node.SetProp(COwner, AOwner.ID)
    .SetProp(CTarget, NyxBindingPropertyName(ATarget)).SetProp(CBaseline, LBaseline);
  Result.Add(NewNyxLabel(AID + '-title').WithText('Computed ' + NyxBindingPropertyTitle(ATarget)));
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Compose typed Pascal expressions. Preview uses project defaults; runtime values stay independent.'));
  Result.Add(NewNyxCodeEditor(NyxExpressionEditorFieldID(AID, eefSource)).Configure
    .PartName(NyxPart('source')).Text('Fluent Pascal expression')
    .AccessibleName('Fluent Pascal expression')
    .Height(220).Value(TNyxCodegen.Expression(LExpression)).Done);
  for LField := eefPreview to eefApply do
  begin
    LButton := NewNyxButton(NyxExpressionEditorFieldID(AID, LField));
    LButton.Configure.PartName(NyxPart(CFields[LField])).Text('Preview expression').Done;

    if LField = eefApply then
    begin
      LButton.Configure.Text('Apply binding').Variant(nvPrimary).Done;
    end;
    LButton.Node.SetProp(CEditor, AID);
    Result.Add(LButton);
  end;
  Result.Add(NewNyxCode(NyxExpressionEditorFieldID(AID, eefSummary)).Configure
    .PartName(NyxPart('summary')).Height(160).Text(
      'Computed bindings read state. Use a direct binding when this control edits a state value.').Done);
end;

function ReadNyxExpressionEditor(AButton, ARoot: TNyxNode;
  ADocument: TNyxDocument; out AProposal: TNyxExpressionProposal;
  out APreview: Boolean): Boolean;
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
  LID: TNyxText;
  LOwner: TNyxControlRef;
  LTarget: TNyxBindingProperty;
  LBinding: TNyxBindingSpec;
  LExpression: INyxExpression;
  LCandidate: TNyxDocument;
begin
  Result := False;
  AProposal := Default(TNyxExpressionProposal);
  APreview := False;

  if (AButton = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  LID := AButton.Prop(CEditor);
  APreview := AButton.SourceID = NyxExpressionEditorFieldID(LID, eefPreview);

  if not APreview and (AButton.SourceID <> NyxExpressionEditorFieldID(LID, eefApply)) then
  begin
    Exit;
  end;

  if ARoot = nil then
  begin
    raise ENyxExpression.Create('Expression form is no longer mounted');
  end;
  LEditor := ARoot.Find(LID);

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxExpression.Create('Expression action belongs to another form');
  end;
  LInput := LEditor.Find(NyxExpressionEditorFieldID(LID, eefSource));

  if (LInput = nil) or (LInput.Kind <> NyxKindName(nkCodeEditor)) or
    not TryNyxBindingProperty(LEditor.Prop(CTarget), LTarget) then
  begin
    raise ENyxExpression.Create('Expression form contract changed');
  end;
  LOwner := NyxControl(LEditor.Prop(COwner));

  if NyxExpressionEditorBaseline(ADocument, LOwner, LTarget) <> LEditor.Prop(CBaseline) then
  begin
    raise ENyxExpression.Create('Binding/default context changed; reopen this expression form');
  end;
  LExpression := ReadNyxExpressionSource(LInput.Prop('value'));
  LExpression.Value(CaptureNyxExpressionInputs(ADocument.State));
  LBinding := TNyxBindingSpec.Computed(LTarget, LExpression);
  LCandidate := ADocument.Clone;
  try
    LCandidate.Find(LOwner.ID).SetBinding(LBinding);
    LCandidate.Validate;
  finally
    LCandidate.Free;
  end;
  AProposal.FOwner := LOwner;
  AProposal.FTarget := LTarget;
  AProposal.FBaseline := LEditor.Prop(CBaseline);
  AProposal.FBinding := LBinding.Copy;
  Result := True;
end;

procedure ShowNyxExpressionPreview(ARoot: TNyxNode; const AEditor: TNyxText;
  const AProposal: TNyxExpressionProposal; ADocument: TNyxDocument);
var
  LSummary: TNyxNode;
  LExpression: INyxExpression;
  LValue: TNyxStateValue;
  LText: TNyxText;
  LDependency: TNyxExpressionDependency;
  LIndex: Integer;
begin

  if (ARoot = nil) or (ADocument = nil) or
    (NyxExpressionEditorBaseline(ADocument, AProposal.Owner, AProposal.Target) <>
      AProposal.Baseline) then
  begin
    raise ENyxExpression.Create('Expression preview context changed');
  end;
  LSummary := ARoot.Find(NyxExpressionEditorFieldID(AEditor, eefSummary));

  if LSummary = nil then
  begin
    raise ENyxExpression.Create('Expression preview summary is missing');
  end;
  LExpression := AProposal.Binding.Expression;
  LValue := LExpression.Value(CaptureNyxExpressionInputs(ADocument.State));
  case LValue.Kind of
    nskText:
      begin
        LText := LValue.TextValue;
      end;
    nskBoolean:
      begin
        LText := 'False';

        if LValue.BooleanValue then
        begin
          LText := 'True';
        end;
      end;
    nskInteger:
      begin
        LText := TNyxText(IntToStr(LValue.IntegerValue));
      end;
    nskNumber:
      begin
        LText := LValue.NumberText;
      end;
  end;
  LText := 'Default preview: ' + LText;
  LText := TNyxCodegen.Expression(LExpression) + #10 + #10 + LText + #10 + 'State inputs:';
  for LIndex := 0 to Min(LExpression.DependencyCount, 8) - 1 do
  begin
    LDependency := LExpression.Dependency(LIndex);
    LText := LText + #10 + LDependency.Name + ' (' + NyxStateKindName(LDependency.Kind) + ')';
  end;

  if LExpression.DependencyCount > 8 then
  begin
    LText := LText + #10 + TNyxText(IntToStr(LExpression.DependencyCount - 8)) + ' more inputs';
  end;
  LSummary.Configure.Text(LText).Done;
end;

procedure TNyxExpressionEditorDraft.Clear;
begin
  FEditor := '';
  FBaseline := '';
  FSource := '';
end;

procedure TNyxExpressionEditorDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(AEditor);

  if LEditor = nil then
  begin
    Exit;
  end;
  LInput := LEditor.Find(NyxExpressionEditorFieldID(AEditor, eefSource));

  if (LInput = nil) or (LInput.Kind <> NyxKindName(nkCodeEditor)) then
  begin
    Clear;
    Exit;
  end;
  FEditor := AEditor;
  FBaseline := LEditor.Prop(CBaseline);
  FSource := LInput.Prop('value');
end;

function TNyxExpressionEditorDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
begin
  Result := False;

  if (ARoot = nil) or (FEditor = '') then
  begin
    Exit;
  end;
  LEditor := ARoot.Find(FEditor);

  if LEditor = nil then
  begin
    Exit;
  end;
  LInput := LEditor.Find(NyxExpressionEditorFieldID(FEditor, eefSource));

  if (LEditor.Prop(CBaseline) <> FBaseline) or
    (LInput = nil) or (LInput.Kind <> NyxKindName(nkCodeEditor)) then
  begin
    Clear;
    Exit;
  end;
  LInput.Configure.Value(FSource).Done;
  Result := True;
end;

end.
