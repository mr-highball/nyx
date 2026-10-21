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

unit nyx.date.range.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.date.range;

type
  { Immediate copied proposals; no editable field or unsubmitted draft exists. }
  TNyxDateRangeEditorAction = (ndraOrdered, ndraIndependent, ndraInherit);

  { Detached command value. Owner is the exact authored node or descriptor;
    Baseline captures its raw context and effective range. Options is available
    only for explicit policies. Inherit is represented by its own action. }
  TNyxDateRangeEditorChange = record
  private
    FOwner: TNyxControlRef;
    FAction: TNyxDateRangeEditorAction;
    FBaseline: TNyxText;
    function GetInherit: Boolean;
    function GetOptions: TNyxDateRangeOptions;
    procedure Validate;
  public
    function Copy: TNyxDateRangeEditorChange;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxDateRangeEditorChange; static;
    property Owner: TNyxControlRef read FOwner;
    property Action: TNyxDateRangeEditorAction read FAction;
    property Inherit: Boolean read GetInherit;
    property Options: TNyxDateRangeOptions read GetOptions;
    property Baseline: TNyxText read FBaseline;
  end;

function NyxDateRangeEditorActionID(const AID: TNyxText;
  AAction: TNyxDateRangeEditorAction): TNyxText;
function NyxDateRangeEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
function NyxDateRangeEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  AAction: TNyxDateRangeEditorAction): TNyxDateRangeEditorChange;
procedure ValidateNyxDateRangeEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxDateRangeEditorChange);
{ Ordinary public Nyx content. Construction borrows the document only while
  copying its displayed policy and baseline; no controller/model is retained. }
function NewNyxDateRangeEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
{ Capture only the currently mounted action of an intact public form. The
  resulting change never mutates the document. Unrelated input returns False. }
function CaptureNyxDateRangeEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxDateRangeEditorChange): Boolean;

implementation

uses
  SysUtils, nyx.codec, nyx.composition;

const
  CVersion = 'nyx.date-range-editor.version';
  CFormVersion = '1';
  COwner = 'nyx.date-range-editor.owner';
  CBaseline = 'nyx.date-range-editor.baseline';
  CEditor = 'nyx.date-range-editor.editor';
  CAction = 'nyx.date-range-editor.action';
  CPath = 'path';
  CNames: array[TNyxDateRangeEditorAction] of TNyxText =
    ('ordered', 'independent', 'inherit');
  CLabels: array[TNyxDateRangeEditorAction] of TNyxText =
    ('Keep dates in order', 'Keep dates independent', 'Use inherited ordering');

procedure CheckAction(AAction: TNyxDateRangeEditorAction);
begin

  if (Ord(AAction) < Ord(Low(TNyxDateRangeEditorAction))) or
    (Ord(AAction) > Ord(High(TNyxDateRangeEditorAction))) then
  begin
    raise ENyxDateRange.Create('Unknown date-range editor action');
  end;
end;

procedure ResolveOwner(ADocument: TNyxDocument; const AOwner: TNyxControlRef;
  out ARaw, AContext, AProjection: TNyxNode);
var
  LIndex: Integer;
  LPath: TNyxText;
begin
  ARaw := nil;
  AContext := nil;
  AProjection := nil;

  if ADocument = nil then
  begin
    raise ENyxDateRange.Create('Date-range editor requires its document');
  end;
  ARaw := ADocument.FindControl(AOwner);

  if ARaw = nil then
  begin
    raise ENyxDateRange.Create('Date-range editor owner is missing');
  end;

  if ARaw.Kind = NyxKindName(nkSlotOverride) then
  begin

    if (ARaw.Parent = nil) or
      (ARaw.Parent.Kind <> NyxKindName(nkComponent)) or
      (ARaw.StoredProp(NyxAttributeName(atOverrideMode)) = NyxOverrideName(noRemove)) then
    begin
      raise ENyxDateRange.Create('Date-range editor requires an available named descriptor');
    end;
    LPath := ARaw.StoredProp(CPath);
    for LIndex := 0 to ARaw.Parent.Count - 1 do
    begin

      if (ARaw.Parent.Children[LIndex] <> ARaw) and
        (ARaw.Parent.Children[LIndex].Kind = NyxKindName(nkSlotOverride)) and
        (ARaw.Parent.Children[LIndex].StoredProp(CPath) = LPath) then
      begin
        raise ENyxDateRange.Create('Date-range editor descriptor is ambiguous');
      end;
    end;
  end;
  AContext := RealizeNyxContext(ADocument, ARaw, AProjection);
  try

    if (AProjection = nil) or (AProjection.Kind <> NyxKindName(nkDateRange)) then
    begin
      raise ENyxDateRange.Create('This authored selection is not a date range');
    end;
    ValidateNyxDateRangeOptions(AProjection);
  except
    FreeAndNil(AContext);
    AProjection := nil;
    raise;
  end;
end;

function TNyxDateRangeEditorChange.GetInherit: Boolean;
begin
  Validate;
  Result := FAction = ndraInherit;
end;

function TNyxDateRangeEditorChange.GetOptions: TNyxDateRangeOptions;
begin
  Validate;

  if FAction = ndraInherit then
  begin
    raise ENyxDateRange.Create('Inherited ordering has no local options');
  end;
  Result := TNyxDateRangeOptions.Independent;

  if FAction = ndraOrdered then
  begin
    Result := TNyxDateRangeOptions.Ordered;
  end;
end;

procedure TNyxDateRangeEditorChange.Validate;
begin
  CheckAction(FAction);

  if (FOwner.ID = '') or (FBaseline = '') then
  begin
    raise ENyxDateRange.Create('Date-range change requires its owner and exact baseline');
  end;
end;

function TNyxDateRangeEditorChange.Copy: TNyxDateRangeEditorChange;
begin
  Validate;
  Result.FOwner := NyxControl(FOwner.ID);
  Result.FAction := FAction;
  Result.FBaseline := FBaseline;
end;

function TNyxDateRangeEditorChange.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('owner', NyxData(FOwner.ID)),
    NyxField('action', NyxData(CNames[FAction])),
    NyxField('baseline', NyxData(FBaseline))]);
end;

class function TNyxDateRangeEditorChange.FromData(
  const AValue: TNyxDataValue): TNyxDateRangeEditorChange;
var
  LName: TNyxText;
  LAction: TNyxDateRangeEditorAction;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> 4) or
    (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxDateRange.Create('Date-range change requires an exact version-one object');
  end;
  Result.FOwner := NyxControl(AValue.Field('owner').AsText);
  Result.FBaseline := AValue.Field('baseline').AsText;
  LName := AValue.Field('action').AsText;
  for LAction := Low(TNyxDateRangeEditorAction) to High(TNyxDateRangeEditorAction) do
  begin

    if LName = CNames[LAction] then
    begin
      Result.FAction := LAction;
      Result.Validate;
      Exit;
    end;
  end;
  raise ENyxDateRange.Create('Unknown date-range editor action');
end;

function NyxDateRangeEditorActionID(const AID: TNyxText;
  AAction: TNyxDateRangeEditorAction): TNyxText;
begin
  CheckAction(AAction);
  Result := NyxControl(AID).ID + '-' + CNames[AAction];
end;

function NyxDateRangeEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LRaw: TNyxNode;
  LRoot: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
begin
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    LRoot := LRaw;
    while LRoot.Parent <> nil do
    begin
      LRoot := LRoot.Parent;
    end;
    Result := NyxObject([NyxField('owner', NyxData(LRaw.ID)),
      NyxField('local', TNyxCodec.NodeSnapshot(LRaw)),
      NyxField('context', TNyxCodec.NodeSnapshot(LRoot)),
      NyxField('effective', TNyxCodec.NodeSnapshot(LProjection))]).ToJSON;
  finally
    LContext.Free;
  end;
end;

function NyxDateRangeEditorChange(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef;
  AAction: TNyxDateRangeEditorAction): TNyxDateRangeEditorChange;
begin
  CheckAction(AAction);
  Result.FOwner := NyxControl(AOwner.ID);
  Result.FAction := AAction;
  Result.FBaseline := NyxDateRangeEditorBaseline(ADocument, AOwner);

  if (AAction = ndraInherit) and
    not HasNyxDateRangeOptions(ADocument.FindControl(AOwner)) then
  begin
    raise ENyxDateRange.Create('This date range has no local ordering to inherit');
  end;
end;

procedure ValidateNyxDateRangeEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxDateRangeEditorChange);
var
  LChange: TNyxDateRangeEditorChange;
  LCurrent: TNyxDateRangeEditorChange;
begin
  LChange := AChange.Copy;
  LCurrent := NyxDateRangeEditorChange(ADocument, LChange.Owner, LChange.Action);

  if LCurrent.Baseline <> LChange.Baseline then
  begin
    raise ENyxDateRange.Create('Date-range ordering changed; reopen this form');
  end;
end;

function OrderLabel(const AOptions: TNyxDateRangeOptions): TNyxText;
begin
  Result := 'independent';

  if AOptions.Order = ndroAscending then
  begin
    Result := 'start must be on or before finish';
  end;
end;

function NewNyxDateRangeEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): INyxCard;
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LLocal: TNyxText;
  LEffective: TNyxDateRangeOptions;
  LBaseline: TNyxText;
  LAction: TNyxDateRangeEditorAction;
  LButton: INyxButton;
begin
  LContext := nil;
  try
    ResolveOwner(ADocument, AOwner, LRaw, LContext, LProjection);
    LBaseline := NyxDateRangeEditorBaseline(ADocument, AOwner);
    LEffective := GetNyxDateRangeOptions(LProjection);
    LLocal := 'inherited/default';

    if HasNyxDateRangeOptions(LRaw) then
    begin
      LLocal := OrderLabel(GetNyxDateRangeOptions(LRaw));
    end;
    Result := NewNyxCard(AID);
    Result.Configure.Gap(10).Padding(16).Done;
    Result.Node.SetProp(CVersion, CFormVersion).SetProp(COwner, LRaw.ID).
      SetProp(CBaseline, LBaseline);
    Result.Add(NewNyxLabel(AID + '-title').WithText('Date range ordering'));
    Result.Add(NewNyxLabel(AID + '-local').WithText('Local policy: ' + LLocal));
    Result.Add(NewNyxLabel(AID + '-effective').WithText(
      'Effective policy: ' + OrderLabel(LEffective)));
    Result.Add(NewNyxLabel(AID + '-help').WithText(
      'Ordering allows equal dates and empty endpoints. Each date keeps its own bounds. ' +
      'Dates are never swapped or filled automatically.'));
    for LAction := Low(TNyxDateRangeEditorAction) to High(TNyxDateRangeEditorAction) do
    begin
      LButton := NewNyxButton(NyxDateRangeEditorActionID(AID, LAction));
      LButton.Configure.Text(CLabels[LAction]).PartName(NyxPart(CNames[LAction])).
        AccessibleName(CLabels[LAction]).Done;
      LButton.Node.SetProp(CEditor, AID).SetProp(CAction, CNames[LAction]);

      if LAction = ndraInherit then
      begin
        LButton.Configure.Enabled(HasNyxDateRangeOptions(LRaw)).Done;
      end;
      Result.Add(LButton);
    end;
  finally
    LContext.Free;
  end;
end;

function ActionNode(AEditor: TNyxNode; const AForm: TNyxText;
  AAction: TNyxDateRangeEditorAction): TNyxNode;
var
  LIndex: Integer;
  LID: TNyxText;
begin
  Result := nil;
  LID := NyxDateRangeEditorActionID(AForm, AAction);
  for LIndex := 0 to AEditor.Count - 1 do
  begin

    if AEditor.Children[LIndex].SourceID = LID then
    begin

      if Result <> nil then
      begin
        raise ENyxDateRange.Create('Date-range form action is ambiguous');
      end;
      Result := AEditor.Children[LIndex];
    end;
  end;

  if (Result = nil) or (Result.Kind <> NyxKindName(nkButton)) or
    (Result.ProjectionKind <> NyxKindName(nkButton)) or
    (Result.StoredProp(CEditor) <> AForm) or
    (Result.StoredProp(CAction) <> CNames[AAction]) or
    (Result.StoredProp(NyxAttributeName(atPart)) <> CNames[AAction]) then
  begin
    raise ENyxDateRange.Create('Date-range form action is missing or malformed');
  end;
end;

function CaptureNyxDateRangeEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AChange: TNyxDateRangeEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LButton: TNyxNode;
  LAction: TNyxDateRangeEditorAction;
  LOwner: TNyxControlRef;
  LForm: TNyxText;
  LSelected: TNyxDateRangeEditorAction;
  LFound: Boolean;
begin
  AChange := Default(TNyxDateRangeEditorChange);
  Result := False;

  if (AButton = nil) or (AButton.StoredProp(CEditor) = '') then
  begin
    Exit;
  end;

  if (AShellRoot = nil) or (AShellRoot.Find(AButton.ID) <> AButton) then
  begin
    raise ENyxDateRange.Create('This date-range form belongs to a retired view');
  end;
  LForm := AButton.StoredProp(CEditor);
  LEditor := AButton.Parent;

  if (LEditor = nil) or (LEditor.Kind <> NyxKindName(nkCard)) or
    (LEditor.StoredProp(CVersion) <> CFormVersion) or
    ((LEditor.SourceID <> LForm) and (LEditor.ID <> LForm)) then
  begin
    raise ENyxDateRange.Create('Date-range form is missing or malformed');
  end;
  LFound := False;
  LSelected := ndraOrdered;
  for LAction := Low(TNyxDateRangeEditorAction) to High(TNyxDateRangeEditorAction) do
  begin
    LButton := ActionNode(LEditor, LForm, LAction);

    if AButton = LButton then
    begin
      LFound := True;
      LSelected := LAction;
    end;
  end;

  if not LFound then
  begin
    raise ENyxDateRange.Create('Unknown date-range form action');
  end;

  if AButton.Prop(NyxAttributeName(atEnabled), 'true') <> 'true' then
  begin
    raise ENyxDateRange.Create('This date-range action is unavailable');
  end;
  LOwner := NyxControl(LEditor.StoredProp(COwner));

  if LEditor.StoredProp(CBaseline) <> NyxDateRangeEditorBaseline(ADocument, LOwner) then
  begin
    raise ENyxDateRange.Create('Date-range ordering changed; reopen this form');
  end;
  AChange := NyxDateRangeEditorChange(ADocument, LOwner, LSelected);
  Result := True;
end;

end.
