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

unit nyx.times.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.times, nyx.contract;

type
  { Stable named fields of the reusable clock-policy compound. Bounds are
    independently optional; reversed defined bounds span midnight. }
  TNyxTimeDomainEditorField = (ntfMinimum, ntfMaximum, ntfStepMode,
    ntfMilliseconds, ntfChoices, ntfApply, ntfInherit);
  { An absent step and explicit Any have the same unrestricted admission but
    distinct source descriptors. Preserve that distinction through no-op edits. }
  TNyxTimeDomainEditorStep = (ntsDefault, ntsAny, ntsMilliseconds);
  { Independently copied proposal. The receiving owner rechecks selection and
    local/effective baseline before atomic publication. No control/session/tree
    references escape capture; the editor never owns the authored component. }
  TNyxTimeDomainEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Inherit: Boolean;
    Domain: TNyxValueDomain;
  end;
  { Ephemeral input for one mounted clock form. All five values are copied;
    partial step/choice text is deliberately not parsed until Apply. Copies
    retain no renderer, node, contract or interface and cannot mutate each other.
    Exact editor/owner/local-and-effective baseline protects selection changes
    and inherited-policy publication. This never enters design/source history. }
  TNyxTimeDomainEditorDraft = record
  private
    FEditorID: TNyxText;
    FOwner: TNyxText;
    FBaseline: TNyxText;
    FValues: array[ntfMinimum..ntfChoices] of TNyxText;
  public
    { Absent forms leave the snapshot parked across Properties/Events switches.
      A present valid form replaces it atomically; malformed metadata/field
      kinds retire it. Capture reads disposable form values, never the owner. }
    procedure Capture(const AEditorID: TNyxText; AShellRoot: TNyxNode);
    { Restore before rendering. A changed owner/baseline or complete field
      shape retires the snapshot without a partial write. An absent form keeps
      its parked input. Bounds retain the exact form's accepted clock text;
      adapters' opaque/incomplete physical picker buffers are not captured. }
    function Restore(AShellRoot: TNyxNode): Boolean;
    { Project replacement explicitly retires even identical owner/baseline text. }
    procedure Clear;
  end;

{ Canonical descendant identity for a public form field. The caller supplies
  the independently owned editor's identity; no control lifetime is retained. }
function NyxTimeDomainEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxTimeDomainEditorField): TNyxText;
{ English display/wire vocabulary for the closed step selector. User text is
  admitted to this enum only at Capture's mounted editor boundary. }
function NyxTimeDomainEditorStepName(AStep: TNyxTimeDomainEditorStep): TNyxText;
{ Exact copied local and effective declarations protect inherited-policy changes.
  Nil/invalid contracts and non-clock domains raise ENyxContract. }
function NyxTimeDomainEditorBaseline(AContract: TNyxContract;
  const AEffective: TNyxValueDomain): TNyxText;
{ Public Nyx composition uses specialized Card/Time/Select/Input/Memo/Button/
  Label interfaces. Descendants have ordinary independent ownership. Bounds and
  choices retain wire precision. Restore is disabled without a local declaration.
  Composition does not modify either borrowed contract or effective descriptor. }
function NewNyxTimeDomainEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  AContract: TNyxContract; const AEffective: TNyxValueDomain): INyxCard;
{ Only exact mounted Apply/Restore buttons are recognized. Unrelated controls
  return False. Forged/stale/missing fields, invalid clock/step/choices and budget
  violations raise ENyxContract without changing the editor or authored document.
  Empty choices use the explicit English "(empty)" entry; blank lines refuse. }
function CaptureNyxTimeDomainEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxTimeDomainEditorChange): Boolean;

implementation

const
  CEditorKey = 'nyx.time-domain-editor';
  COwnerKey = 'nyx.time-domain-editor.owner';
  CBaselineKey = 'nyx.time-domain-editor.baseline';
  CEmptyChoice = '(empty)';
  CFields: array[TNyxTimeDomainEditorField] of TNyxText =
    ('minimum', 'maximum', 'step-mode', 'milliseconds', 'choices', 'apply', 'inherit');
  CStepNames: array[TNyxTimeDomainEditorStep] of TNyxText =
    ('No step declaration', 'Any millisecond', 'Fixed milliseconds');
  CInputKinds: array[ntfMinimum..ntfChoices] of TNyxKind =
    (nkTime, nkTime, nkSelect, nkInput, nkMemo);

function NyxTimeDomainEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxTimeDomainEditorField): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CFields[AField];
end;

function NyxTimeDomainEditorStepName(AStep: TNyxTimeDomainEditorStep): TNyxText;
begin
  Result := CStepNames[AStep];
end;

procedure TNyxTimeDomainEditorDraft.Clear;
var
  LField: TNyxTimeDomainEditorField;
begin
  FEditorID := '';
  FOwner := '';
  FBaseline := '';
  for LField := ntfMinimum to ntfChoices do
  begin
    FValues[LField] := '';
  end;
end;

procedure TNyxTimeDomainEditorDraft.Capture(const AEditorID: TNyxText;
  AShellRoot: TNyxNode);
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
  LField: TNyxTimeDomainEditorField;
  LCandidate: TNyxTimeDomainEditorDraft;
begin
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(AEditorID);
  end;

  if LEditor = nil then
  begin
    Exit;
  end;

  if (AEditorID = '') or (LEditor.Prop(COwnerKey) = '') or
    (LEditor.Prop(CBaselineKey) = '') then
  begin
    Clear;
    Exit;
  end;
  LCandidate := Default(TNyxTimeDomainEditorDraft);
  LCandidate.FEditorID := AEditorID;
  LCandidate.FOwner := LEditor.Prop(COwnerKey);
  LCandidate.FBaseline := LEditor.Prop(CBaselineKey);
  for LField := ntfMinimum to ntfChoices do
  begin
    LInput := LEditor.Find(NyxTimeDomainEditorFieldID(AEditorID, LField));

    if (LInput = nil) or (LInput.Kind <> NyxKindName(CInputKinds[LField])) then
    begin
      Clear;
      Exit;
    end;
    LCandidate.FValues[LField] := LInput.Prop('value');
  end;
  Self := LCandidate;
end;

function TNyxTimeDomainEditorDraft.Restore(AShellRoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LInput: TNyxNode;
  LField: TNyxTimeDomainEditorField;
begin
  Result := False;

  if (FEditorID = '') or (AShellRoot = nil) then
  begin
    Exit;
  end;
  LEditor := AShellRoot.Find(FEditorID);

  if LEditor = nil then
  begin
    Exit;
  end;

  if (LEditor.Prop(COwnerKey) <> FOwner) or
    (LEditor.Prop(CBaselineKey) <> FBaseline) then
  begin
    Clear;
    Exit;
  end;
  { Validate every target before publishing any text. A same-ID replacement
    with another kind cannot receive a clock-form proposal accidentally. }
  for LField := ntfMinimum to ntfChoices do
  begin
    LInput := LEditor.Find(NyxTimeDomainEditorFieldID(FEditorID, LField));

    if (LInput = nil) or (LInput.Kind <> NyxKindName(CInputKinds[LField])) then
    begin
      Clear;
      Exit;
    end;
  end;
  for LField := ntfMinimum to ntfChoices do
  begin
    LEditor.Find(NyxTimeDomainEditorFieldID(FEditorID, LField))
      .Configure.Value(FValues[LField]).Done;
  end;
  Result := True;
end;

function HasMember(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxTimeDomainEditorBaseline(AContract: TNyxContract;
  const AEffective: TNyxValueDomain): TNyxText;
begin

  if AContract = nil then
  begin
    raise ENyxContract.Create('Time constraints require the authored contract');
  end;
  AContract.Validate;
  AEffective.Validate;

  if not AEffective.ClockTime then
  begin
    raise ENyxContract.Create('Time constraints require an effective clock domain');
  end;
  Result := NyxObject([NyxField('local', AContract.Snapshot),
    NyxField('effective', AEffective.ToData)]).ToJSON;
end;

function NewNyxTimeDomainEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  AContract: TNyxContract; const AEffective: TNyxValueDomain): INyxCard;
var
  LData: TNyxDataValue;
  LChoices: TNyxDataValue;
  LLines: TNyxStrings;
  LMinimum: TNyxClockTime;
  LMaximum: TNyxClockTime;
  LLocal: TNyxValueDomain;
  LDeclared: Boolean;
  LBaseline: TNyxText;
  LStep: TNyxTimeDomainEditorStep;
  LMilliseconds: Integer;
  LIndex: Integer;
  LButton: INyxButton;
begin

  if (AID = '') or (AOwner.ID = '') then
  begin
    raise ENyxContract.Create('Time constraints require exact editor/owner identities');
  end;
  LBaseline := NyxTimeDomainEditorBaseline(AContract, AEffective);
  LData := AEffective.ToData;
  LMinimum := NyxNoTime;
  LMaximum := NyxNoTime;

  if HasMember(LData, 'min') then
  begin
    LMinimum := TNyxClockTime.FromText(LData.Field('min').AsText);
  end;

  if HasMember(LData, 'max') then
  begin
    LMaximum := TNyxClockTime.FromText(LData.Field('max').AsText);
  end;
  LStep := ntsDefault;
  LMilliseconds := 1;

  if HasMember(LData, 'step') then
  begin
    LStep := ntsAny;

    if AEffective.TimeStepMilliseconds > 0 then
    begin
      LStep := ntsMilliseconds;
      LMilliseconds := AEffective.TimeStepMilliseconds;
    end;
  end;
  LDeclared := AContract.FindValue(LLocal);
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.Node.SetProp(COwnerKey, AOwner.ID).SetProp(CBaselineKey, LBaseline);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).WithText('Time constraints'));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(
    'Leave either bound empty. An end before the start spans midnight.'));
  Result.Add(NewNyxTime(NyxTimeDomainEditorFieldID(AID, ntfMinimum))
    .Configure.PartName(NyxPart('minimum')).Text('Earliest time').Value(LMinimum).Done);
  Result.Add(NewNyxTime(NyxTimeDomainEditorFieldID(AID, ntfMaximum))
    .Configure.PartName(NyxPart('maximum')).Text('Latest time').Value(LMaximum).Done);
  Result.Add(NewNyxSelect(NyxTimeDomainEditorFieldID(AID, ntfStepMode))
    .Configure.PartName(NyxPart('step-mode')).Text('Step policy')
    .Items(CStepNames[ntsDefault] + #10 + CStepNames[ntsAny] + #10 + CStepNames[ntsMilliseconds])
    .Value(CStepNames[LStep]).Done);
  { The proposal is text until Apply. A native numeric widget could coerce an
    invalid fractional/overflow draft before the typed integer admission sees
    it. Keep that draft observable and admit exact milliseconds at Capture. }
  Result.Add(NewNyxInput(NyxTimeDomainEditorFieldID(AID, ntfMilliseconds))
    .Configure.PartName(NyxPart('milliseconds')).Text('Step in milliseconds (fixed policy)')
    .Hint('Enter a positive whole number of milliseconds.')
    .Value(TNyxText(IntToStr(LMilliseconds))).Done);
  LLines := TNyxStrings.Create;
  try

    if HasMember(LData, 'choices') then
    begin
      LChoices := LData.Field('choices');
      for LIndex := 0 to LChoices.Count - 1 do
      begin

        if LChoices.Item(LIndex).AsText = '' then
        begin
          LLines.Add(CEmptyChoice);
        end
        else
        begin
          LLines.Add(LChoices.Item(LIndex).AsText);
        end;
      end;
    end;
    Result.Add(NewNyxMemo(NyxTimeDomainEditorFieldID(AID, ntfChoices))
      .Configure.PartName(NyxPart('choices')).Text('Allowed times (one per line)')
      .Value(LLines.Join(#10)).Done);
  finally
    LLines.Free;
  end;
  Result.Add(NewNyxLabel(AID + TNyxText('-choice-help')).WithText(
    'Use HH:MM, HH:MM:SS or HH:MM:SS.fff. Add (empty) for no time in a restricted list.'));
  LButton := NewNyxButton(NyxTimeDomainEditorFieldID(AID, ntfApply));
  LButton.Configure.PartName(NyxPart('apply')).Text('Apply time constraints').Done;
  LButton.Node.SetProp(CEditorKey, AID);
  Result.Add(LButton);
  LButton := NewNyxButton(NyxTimeDomainEditorFieldID(AID, ntfInherit));
  LButton.Configure.PartName(NyxPart('inherit')).Text('Restore inherited constraints')
    .Enabled(LDeclared).Done;
  LButton.Node.SetProp(CEditorKey, AID);
  Result.Add(LButton);
end;

function CaptureNyxTimeDomainEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxTimeDomainEditorChange): Boolean;
var
  LEditorID: TNyxText;
  LEditor: TNyxNode;
  LMinimum: TNyxClockTime;
  LMaximum: TNyxClockTime;
  LDomain: TNyxTimeDomain;
  LStep: TNyxTimeDomainEditorStep;
  LStepFound: Boolean;
  LLines: TNyxStrings;
  LTimes: array of TNyxClockTime;
  LIndex: Integer;
  LText: TNyxText;

  function Value(AField: TNyxTimeDomainEditorField): TNyxText;
  var
    LField: TNyxNode;
  begin
    LField := LEditor.Find(NyxTimeDomainEditorFieldID(LEditorID, AField));

    if LField = nil then
    begin
      raise ENyxContract.Create('The time-constraint field is no longer mounted');
    end;
    Result := LField.Prop('value');
  end;

begin
  AChange := Default(TNyxTimeDomainEditorChange);
  Result := (AButton <> nil) and (AButton.Prop(CEditorKey) <> '');

  if not Result then
  begin
    Exit;
  end;
  LEditorID := AButton.Prop(CEditorKey);
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(LEditorID);
  end;

  if (LEditor = nil) or (LEditor.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxContract.Create('Select the current time-constraint editor before applying');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwnerKey));
  AChange.Baseline := LEditor.Prop(CBaselineKey);
  AChange.Domain := NyxNoDomain;
  AChange.Inherit := AButton.ID = NyxTimeDomainEditorFieldID(LEditorID, ntfInherit);

  if AChange.Inherit then
  begin
    Exit;
  end;

  if AButton.ID <> NyxTimeDomainEditorFieldID(LEditorID, ntfApply) then
  begin
    raise ENyxContract.Create('Unknown time-constraint editor command');
  end;
  LDomain := NyxTimeDomain;
  try
    LMinimum := TNyxClockTime.FromText(Value(ntfMinimum));
    LMaximum := TNyxClockTime.FromText(Value(ntfMaximum));

    if LMinimum.Defined and LMaximum.Defined then
    begin
      LDomain := LDomain.Range(LMinimum, LMaximum);
    end
    else if LMinimum.Defined then
    begin
      LDomain := LDomain.Minimum(LMinimum);
    end
    else if LMaximum.Defined then
    begin
      LDomain := LDomain.Maximum(LMaximum);
    end;
    LText := Value(ntfStepMode);
    LStepFound := False;
    for LStep := Low(TNyxTimeDomainEditorStep) to High(TNyxTimeDomainEditorStep) do
    begin

      if LText = CStepNames[LStep] then
      begin
        LStepFound := True;
        Break;
      end;
    end;

    if not LStepFound then
    begin
      raise ENyxContract.Create('Select a published clock step policy');
    end;
    case LStep of
      ntsDefault:
        begin
          { Preserve an absent step declaration rather than insert Any. }
        end;
      ntsAny:
        begin
          LDomain := LDomain.AnyStep;
        end;
      ntsMilliseconds:
        begin
          LDomain := LDomain.StepMilliseconds(NyxIntegerDomain.Range(1, High(Integer))
            .Definition.ReadWire(Value(ntfMilliseconds)).AsInteger);
        end;
    end;
    LText := Value(ntfChoices);

    if LText <> '' then
    begin
      LLines := TNyxStrings.Create;
      try
        LLines.Text := LText;

        if (LLines.Count < 1) or (LLines.Count > NyxMaximumDomainChoices) then
        begin
          raise ENyxContract.Create('Allowed times require at most 128 entries');
        end;
        SetLength(LTimes, LLines.Count);
        for LIndex := 0 to LLines.Count - 1 do
        begin

          if LLines[LIndex] = CEmptyChoice then
          begin
            LTimes[LIndex] := NyxNoTime;
          end
          else
          begin

            if LLines[LIndex] = '' then
            begin
              raise ENyxContract.Create('Use (empty) for an optional empty time');
            end;
            LTimes[LIndex] := TNyxClockTime.FromText(LLines[LIndex]);
          end;
        end;
        LDomain := LDomain.Choices(LTimes);
      finally
        LLines.Free;
      end;
    end;
    AChange.Domain := LDomain.Definition;
  except
    on LException: ENyxTimeValue do
    begin
      raise ENyxContract.Create(LException.Message);
    end;
  end;
end;

end.
