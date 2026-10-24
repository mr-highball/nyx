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
unit nyx.collections.workbench.policy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data;

type
  { Authored permissions name accepted-RAW commands, independently of whether
    the host provides clipboard authority. SelectedSource includes selected
    off-page/filtered-out rows; SelectedPage intersects the captured page.
    CellRange grants no selection and never implicitly enables range input. }
  TNyxWorkbenchCopyAction = (nwcaCell, nwcaSelectedSource, nwcaSelectedPage,
    nwcaCellRange);

  { A closed, copied permission set. Empty explicitly disables copy defaults;
    it does not prevent readonly inspection or claim an unavailable service. }
  TNyxWorkbenchCopyActions = set of TNyxWorkbenchCopyAction;

  { VisiblePresentation uses visible authored fields in resolved presentation
    order. AllArrangement also includes hidden authored columns, in complete
    arrangement order; it never includes unbound schema-only fields. }
  TNyxWorkbenchCopyFields = (nwcfVisiblePresentation, nwcfAllArrangement);

  { Delimiters apply between serialized TSV rows only. Embedded cell text is
    escaped exactly, never normalized, and there is no trailing terminator.
    Platform clipboard normalization has its own explicitly weaker result. }
  TNyxWorkbenchLineEnding = (nwleLF, nwleCRLF);

  { Pure immutable authoring choices, with no host, callback, target, selection,
    draft, receipt or runtime budget. Fluent changes return admitted copies.
    Default records mean absent; an explicit factory-default policy is present.
    Absence leaves legacy mounts unconfigured. Presence may request managed
    actions, but a renderer still needs independently supplied runtime services.

    Range selection, copy permissions and focused-draft context are independent.
    None changes accepted values or hijacks an active editor's ordinary Ctrl+C.
    FocusedDraft enables deliberate readonly context capture, never acceptance
    or implicit draft output. RAW remains the ordinary representation. }
  TNyxTableWorkbenchPolicy = record
  private
    FDefined: Boolean;
    FRange: Boolean;
    FCopy: TNyxWorkbenchCopyActions;
    FFields: TNyxWorkbenchCopyFields;
    FHeaders: Boolean;
    FLines: TNyxWorkbenchLineEnding;
    FFocusedDraft: Boolean;
  public
    function RangeSelection(AEnabled: Boolean): TNyxTableWorkbenchPolicy;
    function AllowCopy(AValue: TNyxWorkbenchCopyActions): TNyxTableWorkbenchPolicy;
    function CopyFields(AValue: TNyxWorkbenchCopyFields): TNyxTableWorkbenchPolicy;
    function Headers(AEnabled: Boolean): TNyxTableWorkbenchPolicy;
    function LineEnding(AValue: TNyxWorkbenchLineEnding): TNyxTableWorkbenchPolicy;
    function FocusedDraft(AEnabled: Boolean): TNyxTableWorkbenchPolicy;
    function AllowsCopy(AAction: TNyxWorkbenchCopyAction): Boolean;
    function Copy: TNyxTableWorkbenchPolicy;
    procedure Validate;
    { Closed version-one value shape. Null retains absence; scalar coercion,
      unknown/duplicate actions, missing/extra fields and versions refuse. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxTableWorkbenchPolicy; static;
    property Defined: Boolean read FDefined;
    property RangeSelectionEnabled: Boolean read FRange;
    property CopyActions: TNyxWorkbenchCopyActions read FCopy;
    { AllArrangement includes hidden authored columns, not schema-only fields. }
    property FieldChoice: TNyxWorkbenchCopyFields read FFields;
    property IncludeHeaders: Boolean read FHeaders;
    property Lines: TNyxWorkbenchLineEnding read FLines;
    property HasFocusedDraft: Boolean read FFocusedDraft;
  end;

{ Explicit defaults match ordinary copy options: cell/source/page permissions,
  visible presentation fields, headers and LF. Range and draft context are off.
  Creating this value alone grants no clipboard or filesystem authority. }
function NyxTableWorkbench: TNyxTableWorkbenchPolicy;

implementation

const
  CActionNames: array[TNyxWorkbenchCopyAction] of TNyxText =
    ('cell', 'selected-source', 'selected-page', 'cell-range');

procedure RequireActions(AValue: TNyxWorkbenchCopyActions);
begin

  if AValue - [Low(TNyxWorkbenchCopyAction)..High(TNyxWorkbenchCopyAction)] <> [] then
  begin
    raise EArgumentException.Create('Unknown table workbench copy action');
  end;
end;

function NyxTableWorkbench: TNyxTableWorkbenchPolicy;
begin
  Result := Default(TNyxTableWorkbenchPolicy);
  Result.FDefined := True;
  Result.FCopy := [nwcaCell, nwcaSelectedSource, nwcaSelectedPage];
  Result.FFields := nwcfVisiblePresentation;
  Result.FHeaders := True;
  Result.FLines := nwleLF;
end;

procedure TNyxTableWorkbenchPolicy.Validate;
begin

  if not FDefined then
  begin
    raise EArgumentException.Create('Create a table workbench policy before configuring it');
  end;
  RequireActions(FCopy);

  if (Ord(FFields) < Ord(Low(TNyxWorkbenchCopyFields))) or
    (Ord(FFields) > Ord(High(TNyxWorkbenchCopyFields))) or
    (Ord(FLines) < Ord(Low(TNyxWorkbenchLineEnding))) or
    (Ord(FLines) > Ord(High(TNyxWorkbenchLineEnding))) then
  begin
    raise EArgumentException.Create('Unknown table workbench field or line-ending choice');
  end;
end;

function TNyxTableWorkbenchPolicy.RangeSelection(AEnabled: Boolean): TNyxTableWorkbenchPolicy;
begin
  Validate;
  Result := Self;
  Result.FRange := AEnabled;
end;

function TNyxTableWorkbenchPolicy.AllowCopy(AValue: TNyxWorkbenchCopyActions): TNyxTableWorkbenchPolicy;
begin
  Validate;
  RequireActions(AValue);
  Result := Self;
  Result.FCopy := AValue;
end;

function TNyxTableWorkbenchPolicy.CopyFields(AValue: TNyxWorkbenchCopyFields): TNyxTableWorkbenchPolicy;
begin
  Validate;
  Result := Self;
  Result.FFields := AValue;
  Result.Validate;
end;

function TNyxTableWorkbenchPolicy.Headers(AEnabled: Boolean): TNyxTableWorkbenchPolicy;
begin
  Validate;
  Result := Self;
  Result.FHeaders := AEnabled;
end;

function TNyxTableWorkbenchPolicy.LineEnding(AValue: TNyxWorkbenchLineEnding): TNyxTableWorkbenchPolicy;
begin
  Validate;
  Result := Self;
  Result.FLines := AValue;
  Result.Validate;
end;

function TNyxTableWorkbenchPolicy.FocusedDraft(AEnabled: Boolean): TNyxTableWorkbenchPolicy;
begin
  Validate;
  Result := Self;
  Result.FFocusedDraft := AEnabled;
end;

function TNyxTableWorkbenchPolicy.AllowsCopy(AAction: TNyxWorkbenchCopyAction): Boolean;
begin
  Validate;

  if (Ord(AAction) < Ord(Low(TNyxWorkbenchCopyAction))) or
    (Ord(AAction) > Ord(High(TNyxWorkbenchCopyAction))) then
  begin
    raise EArgumentException.Create('Unknown table workbench copy action');
  end;
  Result := AAction in FCopy;
end;

function TNyxTableWorkbenchPolicy.Copy: TNyxTableWorkbenchPolicy;
begin
  Result := Self;
end;

function TNyxTableWorkbenchPolicy.ToData: TNyxDataValue;
var
  LAction: TNyxWorkbenchCopyAction;
  LActions: array of TNyxDataValue;
  LCount: Integer;
  LFields: TNyxText;
  LLines: TNyxText;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  SetLength(LActions, Ord(High(TNyxWorkbenchCopyAction)) + 1);
  LCount := 0;
  for LAction := Low(TNyxWorkbenchCopyAction) to High(TNyxWorkbenchCopyAction) do
  begin

    if LAction in FCopy then
    begin
      LActions[LCount] := NyxData(CActionNames[LAction]);
      Inc(LCount);
    end;
  end;
  SetLength(LActions, LCount);
  LFields := 'visible-presentation';

  if FFields = nwcfAllArrangement then
  begin
    LFields := 'all-arrangement';
  end;
  LLines := 'lf';

  if FLines = nwleCRLF then
  begin
    LLines := 'crlf';
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('rangeSelection', NyxData(FRange)), NyxField('copyActions', NyxArray(LActions)),
    NyxField('copyFields', NyxData(LFields)), NyxField('headers', NyxData(FHeaders)),
    NyxField('lineEnding', NyxData(LLines)), NyxField('focusedDraft', NyxData(FFocusedDraft))]);
end;

class function TNyxTableWorkbenchPolicy.FromData(
  const AData: TNyxDataValue): TNyxTableWorkbenchPolicy;
var
  LPolicy: TNyxTableWorkbenchPolicy;
  LActions: TNyxDataValue;
  LAction: TNyxWorkbenchCopyAction;
  LCopy: TNyxWorkbenchCopyActions;
  LIndex: Integer;
  LName: TNyxText;
  LFound: Boolean;
begin
  { Build a complete local candidate. A failed assignment keeps the caller's
    previous policy even when its result destination aliases that same value. }
  AData.Validate;

  if AData.Kind = ndNull then
  begin
    Exit(Default(TNyxTableWorkbenchPolicy));
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 7) then
  begin
    raise EArgumentException.Create('A table workbench policy requires seven exact members');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if Pos('|' + AData.Key(LIndex) + '|',
      '|version|rangeSelection|copyActions|copyFields|headers|lineEnding|focusedDraft|') = 0 then
    begin
      raise EArgumentException.Create('Unknown table workbench policy member');
    end;
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise EArgumentException.Create('Unsupported table workbench policy version');
  end;
  LPolicy := NyxTableWorkbench.RangeSelection(AData.Field('rangeSelection').AsBoolean)
    .Headers(AData.Field('headers').AsBoolean)
    .FocusedDraft(AData.Field('focusedDraft').AsBoolean);
  LName := AData.Field('copyFields').AsText;

  if LName = 'all-arrangement' then
  begin
    LPolicy := LPolicy.CopyFields(nwcfAllArrangement);
  end
  else if LName <> 'visible-presentation' then
  begin
    raise EArgumentException.Create('Unknown saved table workbench field choice');
  end;
  LName := AData.Field('lineEnding').AsText;

  if LName = 'crlf' then
  begin
    LPolicy := LPolicy.LineEnding(nwleCRLF);
  end
  else if LName <> 'lf' then
  begin
    raise EArgumentException.Create('Unknown saved table workbench line ending');
  end;
  LActions := AData.Field('copyActions');

  if (LActions.Kind <> ndArray) or
    (LActions.Count > Ord(High(TNyxWorkbenchCopyAction)) + 1) then
  begin
    raise EArgumentException.Create('Copy actions require a bounded closed list');
  end;
  LCopy := [];
  for LIndex := 0 to LActions.Count - 1 do
  begin
    LName := LActions.Item(LIndex).AsText;
    LFound := False;
    for LAction := Low(TNyxWorkbenchCopyAction) to High(TNyxWorkbenchCopyAction) do
    begin

      if LName = CActionNames[LAction] then
      begin

        if LAction in LCopy then
        begin
          raise EArgumentException.Create('Duplicate table workbench copy action');
        end;
        Include(LCopy, LAction);
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise EArgumentException.Create('Unknown saved table workbench copy action');
    end;
  end;
  LPolicy := LPolicy.AllowCopy(LCopy);
  LPolicy.Validate;
  Result := LPolicy;
end;

end.
