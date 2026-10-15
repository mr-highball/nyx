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


unit nyx.regex.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls,
  nyx.logic, nyx.logic.regex, nyx.regex.types, nyx.regex.replacement;

type
  { Closed editor presentation for the three specialized operations. Switching
    visibility grants no runtime authority and never invokes a pattern. }
  TNyxRegexEditorKind = (nrekMatch, nrekReplace, nrekExtract);
  { Owned unfinished form text and structural parts. Invalid patterns/numbers
    remain drafts; restore does not compile or apply an operation. No document,
    renderer, compiled regex or invocation belongs to this snapshot. }
  TNyxRegexEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode;
      AIncludePristine: Boolean = True);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxRegexEditorDraft; static;
  end;

{ Reusable Nyx compound for pattern flags, explicit budgets, structural
  replacement parts and capture selection. Callers own/adopt the returned card. }
function NewNyxRegexEditor(const AID: TNyxText;
  const AOptions: TNyxRegexOptions): INyxCard;
{ Borrow a typed operation only during load, retaining independent configuration.
  Unsupported interfaces refuse; no executable implementation is stored. }
procedure LoadNyxRegexEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOperation: INyxLogicOperation);
{ Capture admits every typed choice and compiles the complete selected recipe.
  Failure preserves all form values; publication/history belongs to the caller. }
function CaptureNyxRegexEditor(const AID: TNyxText; ARoot: TNyxNode;
  AKind: TNyxRegexEditorKind): INyxLogicOperation;
{ Handle local part add/reorder/confirmed removal. True requests normal Sync;
  controls and accepted documents are never remounted or edited here. }
function HandleNyxRegexEditor(ANode, ARoot: TNyxNode): Boolean;

implementation

uses Math, nyx.bytes, nyx.flow.types;

const
  CEditor = 'nyx.regex-editor';
  CMode = 'nyx.regex-editor-kind';
  CParts = 'nyx.regex-editor-parts';
  CFields: array[0..25] of TNyxText = ('pattern', 'case', 'scan', 'classes',
    'multiline', 'dotAll', 'expanded', 'pattern-limit', 'input-limit',
    'instructions', 'work', 'states', 'matches', 'result', 'groups', 'nesting',
    'unmatched', 'capture-kind', 'capture', 'missing', 'part-kind', 'part-value',
    'part-choice', 'confirm', 'limits', 'replacement-open');
  CCases: array[TNyxRegexCase] of TNyxText = ('Exact', 'ASCII insensitive', 'Unicode insensitive');
  CScans: array[TNyxRegexScan] of TNyxText = ('First match', 'All matches');
  CClasses: array[TNyxRegexClasses] of TNyxText = ('Unicode properties', 'ASCII shorthands');
  CUnmatched: array[TNyxRegexUnmatched] of TNyxText = ('Use empty text', 'Report error');
  CMissing: array[TNyxRegexMissing] of TNyxText = ('Keep null row', 'Skip row', 'Report error');

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if ARoot <> nil then
  begin
    Result := ARoot.Find(AID);
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CEditor)) then
  begin
    raise ENyxRegex.Create('Regex editor is unavailable');
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxRegex.Create('Regex editor field is unavailable: ' + AName);
  end;
end;

procedure Value(AForm: TNyxNode; const AName, AValue: TNyxText);
begin

  if (AName = 'limits') or (AName = 'replacement-open') then
  begin

    if (AValue <> 'true') and (AValue <> 'false') then
    begin
      raise ENyxRegex.Create('Disclosure draft state must be Boolean');
    end;
    Field(AForm, AName).Configure.Value(AValue = 'true').Done;
    Exit;
  end;
  Field(AForm, AName).Configure.Value(AValue).Done;
end;

function Choice(AForm: TNyxNode; const AName: TNyxText;
  const AChoices: array of TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AChoices) do
  begin

    if Field(AForm, AName).Prop('value') = AChoices[LIndex] then
    begin
      Exit(LIndex);
    end;
  end;
  raise ENyxRegex.Create('Choose a declared regex policy: ' + AName);
end;

function Number(AForm: TNyxNode; const AName: TNyxText): Integer;
begin

  if not TryStrToInt(Field(AForm, AName).Prop('value'), Result) then
  begin
    raise ENyxRegex.Create('Regex count must be an integer: ' + AName);
  end;
end;

function Raw(AForm: TNyxNode): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Field(AForm, CFields[LIndex]).Prop('value')));
  end;
  Result := NyxObject([NyxField('kind', AForm.Extensions.Value(NyxExtension(CMode))),
    NyxField('values', NyxObject(LFields)), NyxField('parts', AForm.Extensions.Value(NyxExtension(CParts)))]);
end;

procedure ShowParts(AForm: TNyxNode);
var
  LParts: TNyxDataValue;
  LItems: TNyxText;
  LIndex: Integer;
begin
  LParts := AForm.Extensions.Value(NyxExtension(CParts));
  LItems := '';
  for LIndex := 0 to LParts.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + TNyxText(IntToStr(LIndex + 1));
  end;
  Field(AForm, 'part-choice').Configure.Items(LItems).Done;
  Field(AForm, 'part-preview').Configure.Text(LParts.ToJSON).Done;
end;

procedure Kind(AForm: TNyxNode; AKind: TNyxRegexEditorKind);
var
  LChoice: Integer;
begin
  LChoice := Ord(AKind);

  if (LChoice < Ord(Low(TNyxRegexEditorKind))) or (LChoice > Ord(High(TNyxRegexEditorKind))) then
  begin
    raise ENyxRegex.Create('Unknown regex editor kind');
  end;
  AForm.Extensions.SetValue(NyxExtension(CMode), NyxData(LChoice));
  Field(AForm, 'replacement-open').Configure.Visible(AKind = nrekReplace).Done;
  Field(AForm, 'extraction').Configure.Visible(AKind = nrekExtract).Done;
end;

procedure LoadOptions(AForm: TNyxNode; const AOptions: TNyxRegexOptions);
var
  LLimits: TNyxRegexLimits;
begin
  AOptions.Validate;
  LLimits := AOptions.Budget;
  Value(AForm, 'pattern', AOptions.Pattern);
  Value(AForm, 'case', CCases[AOptions.Comparison]);
  Value(AForm, 'scan', CScans[AOptions.Extent]);
  Value(AForm, 'classes', CClasses[AOptions.Classes]);
  Value(AForm, 'multiline', 'Whole input');
  Value(AForm, 'dotAll', 'Exclude line breaks');
  Value(AForm, 'expanded', 'Literal spaces');

  if AOptions.IsMultiline then
  begin
    Value(AForm, 'multiline', 'Each line');
  end;

  if AOptions.IsDotAll then
  begin
    Value(AForm, 'dotAll', 'Include line breaks');
  end;

  if AOptions.IsExpanded then
  begin
    Value(AForm, 'expanded', 'Ignore spaces and comments');
  end;
  Value(AForm, 'pattern-limit', IntToStr(LLimits.MaximumPatternScalars));
  Value(AForm, 'input-limit', IntToStr(LLimits.MaximumInputScalars));
  Value(AForm, 'instructions', IntToStr(LLimits.MaximumInstructions));
  Value(AForm, 'work', IntToStr(LLimits.MaximumWorkSteps));
  Value(AForm, 'states', IntToStr(LLimits.MaximumBacktrackStates));
  Value(AForm, 'matches', IntToStr(LLimits.MaximumMatches));
  Value(AForm, 'result', IntToStr(LLimits.MaximumResultBytes));
  Value(AForm, 'groups', IntToStr(LLimits.MaximumGroups));
  Value(AForm, 'nesting', IntToStr(LLimits.MaximumNesting));
end;

procedure LoadNyxRegexEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOperation: INyxLogicOperation);
var
  LForm: TNyxNode;
  LMatch: INyxRegexMatch;
  LReplace: INyxRegexReplace;
  LExtract: INyxRegexExtract;
  LCapture: TNyxRegexCaptureRef;
begin
  LForm := Form(AID, ARoot);

  if Supports(AOperation, INyxRegexMatch, LMatch) then
  begin
    LoadOptions(LForm, LMatch.Options);
    Kind(LForm, nrekMatch);
  end
  else if Supports(AOperation, INyxRegexReplace, LReplace) then
  begin
    LoadOptions(LForm, LReplace.Options);
    Kind(LForm, nrekReplace);
    Value(LForm, 'unmatched', CUnmatched[LReplace.Unmatched]);
    LForm.Extensions.SetValue(NyxExtension(CParts), LReplace.Replacement.ToData);
    ShowParts(LForm);
    Value(LForm, 'part-choice', '');

    if LReplace.Replacement.Count > 0 then
    begin
      Value(LForm, 'part-choice', '1');
    end;
  end
  else if Supports(AOperation, INyxRegexExtract, LExtract) then
  begin
    LoadOptions(LForm, LExtract.Options);
    Kind(LForm, nrekExtract);
    LCapture := LExtract.Capture;
    Value(LForm, 'capture-kind', 'Capture number');
    Value(LForm, 'capture', IntToStr(LCapture.Index));

    if LCapture.Named then
    begin
      Value(LForm, 'capture-kind', 'Capture name');
      Value(LForm, 'capture', LCapture.Name);
    end;
    Value(LForm, 'missing', CMissing[LExtract.Missing]);
  end
  else
  begin
    raise ENyxRegex.Create('Load a specialized regex match, replace or extract operation');
  end;
end;

function CaptureNyxRegexEditor(const AID: TNyxText; ARoot: TNyxNode;
  AKind: TNyxRegexEditorKind): INyxLogicOperation;
var
  LForm: TNyxNode;
  LOptions: TNyxRegexOptions;
  LLimits: TNyxRegexLimits;
  LCapture: TNyxRegexCaptureRef;
begin
  LForm := Form(AID, ARoot);
  LLimits := NyxRegexLimits.PatternScalars(Number(LForm, 'pattern-limit'))
    .InputScalars(Number(LForm, 'input-limit')).Instructions(Number(LForm, 'instructions'))
    .WorkSteps(Number(LForm, 'work')).BacktrackStates(Number(LForm, 'states'))
    .Matches(Number(LForm, 'matches')).ResultBytes(Number(LForm, 'result'))
    .Groups(Number(LForm, 'groups')).Nesting(Number(LForm, 'nesting'));
  LOptions := NyxRegex(Field(LForm, 'pattern').Prop('value'), LLimits)
    .CaseMode(TNyxRegexCase(Choice(LForm, 'case', CCases)))
    .Scan(TNyxRegexScan(Choice(LForm, 'scan', CScans)))
    .CharacterClasses(TNyxRegexClasses(Choice(LForm, 'classes', CClasses)))
    .Multiline(Choice(LForm, 'multiline', ['Whole input', 'Each line']) = 1)
    .DotAll(Choice(LForm, 'dotAll', ['Exclude line breaks', 'Include line breaks']) = 1)
    .Expanded(Choice(LForm, 'expanded', ['Literal spaces', 'Ignore spaces and comments']) = 1);
  Result := nil;
  case AKind of
    nrekMatch: Result := NewNyxRegexMatch(LOptions);
    nrekReplace:
    begin
      Result := NewNyxRegexReplace(LOptions, TNyxRegexReplacement.FromData(
        LForm.Extensions.Value(NyxExtension(CParts)), LLimits.MaximumInstructions),
        TNyxRegexUnmatched(Choice(LForm, 'unmatched', CUnmatched)));
    end;
    nrekExtract:
    begin

      if Choice(LForm, 'capture-kind', ['Capture number', 'Capture name']) = 1 then
      begin
        LCapture := NyxRegexCapture(NyxRegexGroup(Field(LForm, 'capture').Prop('value')));
      end
      else
      begin
        LCapture := NyxRegexCapture(Number(LForm, 'capture'));
      end;
      Result := NewNyxRegexExtract(LOptions, LCapture, TNyxRegexMissing(Choice(LForm, 'missing', CMissing)));
    end;
  end;
end;

function NewNyxRegexEditor(const AID: TNyxText;
  const AOptions: TNyxRegexOptions): INyxCard;
var
  LSection: INyxDisclosure;
  LReplacement: INyxDisclosure;
  LExtraction: INyxColumn;

  procedure Input(AParent: INyxControl; const AName, ACaption: TNyxText);
  begin
    AParent.Add(NewNyxInput(AID + '-' + AName).WithText(ACaption));
  end;

  procedure Select(AParent: INyxControl; const AName, ACaption, AItems, AValue: TNyxText);
  begin
    AParent.Add(NewNyxSelect(AID + '-' + AName).Configure.Text(ACaption).Items(AItems).Value(AValue).Done);
  end;

  procedure Button(AParent: INyxControl; const AName, ACaption: TNyxText);
  begin
    AParent.Add(NewNyxButton(AID + '-' + AName).WithText(ACaption));
  end;

begin
  AOptions.Validate;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxNull);
  Result.Node.Extensions.SetValue(NyxExtension(CParts), NyxRegexReplacement.Capture(0).ToData);
  Result.Add(NewNyxMemo(AID + '-pattern').Configure.Text('Regular expression pattern').Height(100).Done);
  Select(Result, 'case', 'Letter case', CCases[nrcExact] + #10 + CCases[nrcASCII] + #10 + CCases[nrcUnicode], CCases[nrcExact]);
  Select(Result, 'scan', 'Match selection', 'First match' + #10 + 'All matches', 'All matches');
  Select(Result, 'classes', 'Digit, word and space classes', 'Unicode properties' + #10 + 'ASCII shorthands', 'Unicode properties');
  Select(Result, 'multiline', 'Line anchors', 'Whole input' + #10 + 'Each line', 'Whole input');
  Select(Result, 'dotAll', 'Dot behavior', 'Exclude line breaks' + #10 + 'Include line breaks', 'Exclude line breaks');
  Select(Result, 'expanded', 'Pattern spacing', 'Literal spaces' + #10 + 'Ignore spaces and comments', 'Literal spaces');
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Positions count Unicode characters. Flags, capture choices and resource limits are explicit. ' +
    'Invalid patterns and exhausted limits preserve the previous application output.'));
  LReplacement := NewNyxDisclosure(AID + '-replacement-open').WithText('Replacement parts');
  LReplacement.Configure.Value(True).Done;
  Result.Add(LReplacement);
  Select(LReplacement.BodyColumn, 'unmatched', 'Unmatched capture', 'Use empty text' + #10 + 'Report error', 'Use empty text');
  Select(LReplacement.BodyColumn, 'part-kind', 'New replacement part', 'Literal text' + #10 + 'Capture number' + #10 + 'Capture name', 'Literal text');
  Input(LReplacement.BodyColumn, 'part-value', 'Literal text, capture number or capture name');
  Button(LReplacement.BodyColumn, 'part-add', 'Add replacement part');
  Select(LReplacement.BodyColumn, 'part-choice', 'Selected replacement part', '1', '1');
  Button(LReplacement.BodyColumn, 'part-up', 'Move part up');
  Button(LReplacement.BodyColumn, 'part-down', 'Move part down');
  Select(LReplacement.BodyColumn, 'confirm', 'Removal warning', 'Keep' + #10 + 'Confirm removal', 'Keep');
  Button(LReplacement.BodyColumn, 'part-remove', 'Remove part (confirmation required)');
  LReplacement.BodyColumn.Add(NewNyxCode(AID + '-part-preview'));
  LExtraction := NewNyxColumn(AID + '-extraction');
  Result.Add(LExtraction);
  Select(LExtraction, 'capture-kind', 'Extract selected capture', 'Capture number' + #10 + 'Capture name', 'Capture number');
  Input(LExtraction, 'capture', 'Capture number or name');
  Value(Result.Node, 'capture', '0');
  Select(LExtraction, 'missing', 'Unmatched capture', 'Keep null row' + #10 + 'Skip row' + #10 + 'Report error', 'Keep null row');
  LSection := NewNyxDisclosure(AID + '-limits').WithText('Execution limits');
  LSection.Configure.Value(False).Done;
  Result.Add(LSection);
  Input(LSection.BodyColumn, 'pattern-limit', 'Maximum pattern characters');
  Input(LSection.BodyColumn, 'input-limit', 'Maximum input characters');
  Input(LSection.BodyColumn, 'instructions', 'Maximum compiled instructions');
  Input(LSection.BodyColumn, 'work', 'Maximum work steps per complete phase');
  Input(LSection.BodyColumn, 'states', 'Maximum live backtracking states');
  Input(LSection.BodyColumn, 'matches', 'Maximum matches');
  Input(LSection.BodyColumn, 'result', 'Complete result byte budget');
  Input(LSection.BodyColumn, 'groups', 'Maximum capture groups');
  Input(LSection.BodyColumn, 'nesting', 'Maximum nesting (up to 256)');
  LoadOptions(Result.Node, AOptions);
  Kind(Result.Node, nrekMatch);
  ShowParts(Result.Node);
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), Raw(Result.Node));
end;

function HandleNyxRegexEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LAction: TNyxText;
  LParts: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LAdded: TNyxRegexReplacement;
  LPart: TNyxDataValue;
  LIndex: Integer;
  LSelected: Integer;
  LTarget: Integer;
  LCount: Integer;
begin
  Result := False;
  LForm := ANode;
  while (LForm <> nil) and not LForm.Extensions.Has(NyxExtension(CEditor)) do
  begin
    LForm := LForm.Parent;
  end;

  if LForm = nil then
  begin
    Exit;
  end;
  LAction := Copy(ANode.ID, Length(LForm.ID) + 2, MaxInt);
  Result := (LAction = 'part-add') or (LAction = 'part-up') or
    (LAction = 'part-down') or (LAction = 'part-remove');

  if not Result then
  begin
    Exit;
  end;
  LParts := LForm.Extensions.Value(NyxExtension(CParts));
  LCount := LParts.Count;
  SetLength(LItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItems[LIndex] := LParts.Item(LIndex);
  end;
  LSelected := -1;
  TryStrToInt(Field(LForm, 'part-choice').Prop('value'), LSelected);
  Dec(LSelected);

  if LAction = 'part-add' then
  begin
    LAdded := NyxRegexReplacement;
    case Choice(LForm, 'part-kind', ['Literal text', 'Capture number', 'Capture name']) of
      0: LAdded := LAdded.Literal(Field(LForm, 'part-value').Prop('value'));
      1: LAdded := LAdded.Capture(Number(LForm, 'part-value'));
      2: LAdded := LAdded.Capture(NyxRegexGroup(Field(LForm, 'part-value').Prop('value')));
    end;
    SetLength(LItems, LCount + 1);
    LItems[LCount] := LAdded.ToData.Item(0);
    LSelected := LCount;
  end
  else
  begin

    if (LSelected < 0) or (LSelected >= LCount) then
    begin
      raise ENyxRegex.Create('Choose an exact replacement part first');
    end;

    if LAction = 'part-remove' then
    begin

      if Field(LForm, 'confirm').Prop('value') <> 'Confirm removal' then
      begin
        raise ENyxRegex.Create('Confirm removal before deleting a replacement part');
      end;
      for LIndex := LSelected to LCount - 2 do
      begin
        LItems[LIndex] := LItems[LIndex + 1];
      end;
      SetLength(LItems, LCount - 1);
      LSelected := Min(LSelected, LCount - 2);
    end
    else
    begin
      LTarget := LSelected - 1;

      if LAction = 'part-down' then
      begin
        LTarget := LSelected + 1;
      end;

      if (LTarget < 0) or (LTarget >= LCount) then
      begin
        Exit;
      end;
      LPart := LItems[LSelected];
      LItems[LSelected] := LItems[LTarget];
      LItems[LTarget] := LPart;
      LSelected := LTarget;
    end;
  end;
  LForm.Extensions.SetValue(NyxExtension(CParts), NyxArray(LItems));
  ShowParts(LForm);
  Value(LForm, 'part-choice', '');

  if LSelected >= 0 then
  begin
    Value(LForm, 'part-choice', IntToStr(LSelected + 1));
  end;
end;

procedure TNyxRegexEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxRegexEditorDraft.Defined: Boolean;
begin
  Result := FData.Kind = ndObject;
end;

procedure TNyxRegexEditorDraft.Capture(const AID: TNyxText;
  ARoot: TNyxNode; AIncludePristine: Boolean);
var
  LForm: TNyxNode;
  LRaw: TNyxDataValue;
begin
  Clear;

  if (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  LRaw := Raw(LForm);

  if not AIncludePristine and (LRaw.ToJSON = LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON) then
  begin
    Exit;
  end;
  FData := NyxObject([NyxField('version', NyxData(1)), NyxField('form', LRaw)]);
end;

function TNyxRegexEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LRaw: TNyxDataValue;
  LIndex: Integer;
begin
  Result := False;

  if not Defined or (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  LRaw := FData.Field('form');
  for LIndex := 0 to High(CFields) do
  begin
    Value(LForm, CFields[LIndex], LRaw.Field('values').Field(CFields[LIndex]).AsText);
  end;
  LForm.Extensions.SetValue(NyxExtension(CParts), LRaw.Field('parts'));
  ShowParts(LForm);
  Kind(LForm, TNyxRegexEditorKind(LRaw.Field('kind').AsInteger));
  Result := True;
end;

function TNyxRegexEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxRegexEditorDraft.FromData(const AData: TNyxDataValue): TNyxRegexEditorDraft;
var
  LRaw: TNyxDataValue;
  LKind: Integer;
  LIndex: Integer;
begin
  Result.Clear;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'form']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxRegex.Create('Unknown regex form draft version');
  end;
  LRaw := AData.Field('form');
  ValidateNyxFlowFields(LRaw, ['kind', 'values', 'parts']);
  LKind := LRaw.Field('kind').AsInteger;

  if (LKind < Ord(Low(TNyxRegexEditorKind))) or (LKind > Ord(High(TNyxRegexEditorKind))) then
  begin
    raise ENyxRegex.Create('Unknown draft regex editor kind');
  end;
  ValidateNyxFlowFields(LRaw.Field('values'), CFields);
  for LIndex := 0 to High(CFields) do
  begin

    if NyxUTF8ByteCount(LRaw.Field('values').Field(CFields[LIndex]).AsText) > 2 * 1024 * 1024 then
    begin
      raise ENyxRegexLimit.Create('Regex draft field exceeds two MiB');
    end;
  end;
  if ((LRaw.Field('values').Field('limits').AsText <> 'true') and
      (LRaw.Field('values').Field('limits').AsText <> 'false')) or
    ((LRaw.Field('values').Field('replacement-open').AsText <> 'true') and
      (LRaw.Field('values').Field('replacement-open').AsText <> 'false')) then
  begin
    raise ENyxRegex.Create('Regex disclosure draft states must be Boolean');
  end;
  { Draft text may contain a temporarily invalid or larger instruction budget.
    Validate structural parts under the owning document's admitted byte limit,
    independently of unfinished count text; capture applies the execution limit. }
  TNyxRegexReplacement.FromData(LRaw.Field('parts'), High(Integer));
  Result.FData := AData.Copy;
end;

end.
