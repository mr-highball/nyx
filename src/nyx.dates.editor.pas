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

unit nyx.dates.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.dates, nyx.contract;

type
  { Stable composition fields; bounds are inclusive Gregorian calendar dates.
    Both empty bounds mean unrestricted. Choices are exact canonical dates, one
    per line, with the explicit English "(empty)" entry for an optional date. }
  TNyxDateDomainEditorField = (ndfMinimum, ndfMaximum, ndfChoices, ndfApply, ndfInherit);
  { Copied capture contains no mutable contract, control, renderer or session.
    Baseline includes the local declarations and effective inherited domain.
    The admitting owner must compare it again against its fresh candidate. }
  TNyxDateDomainEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Inherit: Boolean;
    Domain: TNyxValueDomain;
  end;

function NyxDateDomainEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxDateDomainEditorField): TNyxText;
{ Borrow the authored contract during this call only. The result owns exact
  copied local/effective policy, protecting inherited changes as well as local
  ones; no other declaration is erased by a date-constraint edit. }
function NyxDateDomainEditorBaseline(AContract: TNyxContract;
  const AEffective: TNyxValueDomain): TNyxText;
{ Reusable public Nyx compound built from specialized Card/Date/Memo/Button/Label
  interfaces. Its descendants have ordinary independent ownership. Only admitted
  effective calendar domains compose; Restore is disabled without a local value
  declaration. The constructor does not change the borrowed authored component. }
function NewNyxDateDomainEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  AContract: TNyxContract; const AEffective: TNyxValueDomain): INyxCard;
{ Recognize exact mounted Apply/Restore buttons only. Unrelated controls return
  False; forged/stale buttons, incomplete bounds, invalid/duplicate choices and
  choice budget failures raise ENyxContract. Capture leaves editor/model intact.
  Domain is independent; publication still needs complete document admission. }
function CaptureNyxDateDomainEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxDateDomainEditorChange): Boolean;

implementation

const
  CEditorKey = 'nyx.date-domain-editor';
  COwnerKey = 'nyx.date-domain-editor.owner';
  CBaselineKey = 'nyx.date-domain-editor.baseline';
  CEmptyChoice = '(empty)';
  CFields: array[TNyxDateDomainEditorField] of TNyxText =
    ('minimum', 'maximum', 'choices', 'apply', 'inherit');

function NyxDateDomainEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxDateDomainEditorField): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CFields[AField];
end;

function HasMember(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

function NyxDateDomainEditorBaseline(AContract: TNyxContract;
  const AEffective: TNyxValueDomain): TNyxText;
begin

  if AContract = nil then
  begin
    raise ENyxContract.Create('Date constraints require the authored contract');
  end;
  AContract.Validate;
  AEffective.Validate;

  if not AEffective.CalendarDate then
  begin
    raise ENyxContract.Create('Date constraints require an effective calendar domain');
  end;
  Result := NyxObject([NyxField('local', AContract.Snapshot),
    NyxField('effective', AEffective.ToData)]).ToJSON;
end;

function NewNyxDateDomainEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  AContract: TNyxContract; const AEffective: TNyxValueDomain): INyxCard;
var
  LData: TNyxDataValue;
  LChoices: TNyxDataValue;
  LLines: TNyxStrings;
  LMinimum: TNyxCalendarDate;
  LMaximum: TNyxCalendarDate;
  LLocal: TNyxValueDomain;
  LDeclared: Boolean;
  LBaseline: TNyxText;
  LIndex: Integer;
  LButton: INyxButton;
begin

  if (AID = '') or (AOwner.ID = '') then
  begin
    raise ENyxContract.Create('Date constraints require exact editor/owner identities');
  end;
  LBaseline := NyxDateDomainEditorBaseline(AContract, AEffective);
  LData := AEffective.ToData;
  LMinimum := NyxNoDate;
  LMaximum := NyxNoDate;

  if HasMember(LData, 'min') then
  begin
    LMinimum := TNyxCalendarDate.FromText(LData.Field('min').AsText);
    LMaximum := TNyxCalendarDate.FromText(LData.Field('max').AsText);
  end;
  LDeclared := AContract.FindValue(LLocal);
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.Node.SetProp(COwnerKey, AOwner.ID).SetProp(CBaselineKey, LBaseline);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).WithText('Date constraints'));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(
    'Bounds include both dates. Leave both empty for any date.'));
  Result.Add(NewNyxDate(NyxDateDomainEditorFieldID(AID, ndfMinimum))
    .Configure.PartName(NyxPart('minimum')).Text('Earliest date').Value(LMinimum).Done);
  Result.Add(NewNyxDate(NyxDateDomainEditorFieldID(AID, ndfMaximum))
    .Configure.PartName(NyxPart('maximum')).Text('Latest date').Value(LMaximum).Done);
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
    Result.Add(NewNyxMemo(NyxDateDomainEditorFieldID(AID, ndfChoices))
      .Configure.PartName(NyxPart('choices')).Text('Allowed dates (one per line)')
      .Value(LLines.Join(#10)).Done);
  finally
    LLines.Free;
  end;
  Result.Add(NewNyxLabel(AID + TNyxText('-choice-help')).WithText(
    'Leave the list blank for any date. Use YYYY-MM-DD; add (empty) to allow no date in a restricted list.'));
  LButton := NewNyxButton(NyxDateDomainEditorFieldID(AID, ndfApply));
  LButton.Configure.PartName(NyxPart('apply')).Text('Apply date constraints').Done;
  LButton.Node.SetProp(CEditorKey, AID);
  Result.Add(LButton);
  LButton := NewNyxButton(NyxDateDomainEditorFieldID(AID, ndfInherit));
  LButton.Configure.PartName(NyxPart('inherit')).Text('Restore inherited constraints')
    .Enabled(LDeclared).Done;
  LButton.Node.SetProp(CEditorKey, AID);
  Result.Add(LButton);
end;

function CaptureNyxDateDomainEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxDateDomainEditorChange): Boolean;
var
  LEditorID: TNyxText;
  LEditor: TNyxNode;
  LMinimum: TNyxCalendarDate;
  LMaximum: TNyxCalendarDate;
  LDomain: TNyxTextDomain;
  LLines: TNyxStrings;
  LDates: array of TNyxCalendarDate;
  LIndex: Integer;
  LText: TNyxText;

  function Value(AField: TNyxDateDomainEditorField): TNyxText;
  var
    LField: TNyxNode;
  begin
    LField := LEditor.Find(NyxDateDomainEditorFieldID(LEditorID, AField));

    if LField = nil then
    begin
      raise ENyxContract.Create('The date-constraint field is no longer mounted');
    end;
    Result := LField.Prop('value');
  end;

begin
  AChange := Default(TNyxDateDomainEditorChange);
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
    raise ENyxContract.Create('Select the current date-constraint editor before applying');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwnerKey));
  AChange.Baseline := LEditor.Prop(CBaselineKey);
  AChange.Domain := NyxNoDomain;
  AChange.Inherit := AButton.ID = NyxDateDomainEditorFieldID(LEditorID, ndfInherit);

  if AChange.Inherit then
  begin
    Exit;
  end;

  if AButton.ID <> NyxDateDomainEditorFieldID(LEditorID, ndfApply) then
  begin
    raise ENyxContract.Create('Unknown date-constraint editor command');
  end;
  LDomain := NyxDateDomain;
  try
    LMinimum := TNyxCalendarDate.FromText(Value(ndfMinimum));
    LMaximum := TNyxCalendarDate.FromText(Value(ndfMaximum));

    if LMinimum.Defined or LMaximum.Defined then
    begin
      LDomain := LDomain.Range(LMinimum, LMaximum);
    end;
    LText := Value(ndfChoices);

    if LText <> '' then
    begin
      LLines := TNyxStrings.Create;
      try
        LLines.Text := LText;

        if (LLines.Count < 1) or (LLines.Count > NyxMaximumDomainChoices) then
        begin
          raise ENyxContract.Create('Allowed dates require at most 128 entries');
        end;
        SetLength(LDates, LLines.Count);
        for LIndex := 0 to LLines.Count - 1 do
        begin

          if LLines[LIndex] = CEmptyChoice then
          begin
            LDates[LIndex] := NyxNoDate;
          end
          else
          begin

            if LLines[LIndex] = '' then
            begin
              raise ENyxContract.Create('Use (empty) for an optional empty date');
            end;
            LDates[LIndex] := TNyxCalendarDate.FromText(LLines[LIndex]);
          end;
        end;
        LDomain := LDomain.Choices(LDates);
      finally
        LLines.Free;
      end;
    end;
    AChange.Domain := LDomain.Definition;
  except
    on LException: ENyxDateValue do
    begin
      raise ENyxContract.Create(LException.Message);
    end;
  end;
end;

end.
