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
unit nyx.parts.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls;

const
  { Explicit context-refresh boundary for a retained Parts form. Only this
    complete document baseline may advance; version, instance, named paths,
    descriptors and action membership remain strict. A host must fence/retire
    old event work through the ordinary form publication before adopting it. }
  NyxPartsEditorBaselineKey = 'nyx.parts-editor.baseline';

type
  { These actions change only one instance's local override. Restore means
    original inheritance, never a cached previous customization or definition edit. }
  TNyxPartsEditorAction = (npaHide, npaRestore);
  { Copied single-operation intent. Exact document content includes referenced
    definitions and occupied IDs. No tree, renderer, callback or interface is
    retained; controllers still own source admission and paired Undo/Redo. }
  TNyxPartsEditorProposal = record
  private
    FInstance: TNyxControlRef;
    FPath: TNyxPartRef;
    FDescriptor: TNyxControlRef;
    FAction: TNyxPartsEditorAction;
    FBaseline: TNyxText;
  public
    property Instance: TNyxControlRef read FInstance;
    property Path: TNyxPartRef read FPath;
    property Descriptor: TNyxControlRef read FDescriptor;
    property Action: TNyxPartsEditorAction read FAction;
    property Baseline: TNyxText read FBaseline;
  end;
  { Copied presentation identity for a currently focused Parts action. Resolve
    follows the same form, instance and exact named path through row reordering
    and Hide/Restore changes. It grants no edit or project-load authority and
    retains no document, node, renderer, callback or physical control. }
  TNyxPartsEditorFocus = record
  private
    FFormID: TNyxText;
    FInstance: TNyxControlRef;
    FPath: TNyxPartRef;
    function GetDefined: Boolean;
  public
    function Resolve(ARoot: TNyxNode): TNyxControlRef;
    property FormID: TNyxText read FFormID;
    property Instance: TNyxControlRef read FInstance;
    property Path: TNyxPartRef read FPath;
    property Defined: Boolean read GetDefined;
  end;

{ The document is borrowed synchronously. The complete saved baseline is
  deliberately conservative: definition, instance and occupied-ID changes all
  require reopening the form, even when a changed sibling is unrelated. }
function NyxPartsEditorBaseline(ADocument: TNyxDocument;
  const AInstance: TNyxControlRef): TNyxText;
{ Prepare one exact remove/inherit operation and validate a private candidate.
  Root removal, unknown parts, Hide on already-hidden parts and discarding an
  existing override payload refuse without changing the supplied document. }
function NyxPartsEditorProposal(ADocument: TNyxDocument;
  const AInstance: TNyxControlRef; const APath: TNyxPartRef;
  AAction: TNyxPartsEditorAction): TNyxPartsEditorProposal;
{ Recheck the complete current baseline, action and exact descriptor identity.
  This grants no permission to bypass a controller's project/revision guards. }
procedure ValidateNyxPartsEditorProposal(ADocument: TNyxDocument;
  const AProposal: TNyxPartsEditorProposal);
{ Stable chrome IDs use an ordinal, never user-controlled part-path text. }
function NyxPartsEditorActionID(const AID: TNyxText; ARowIndex: Integer;
  AAction: TNyxPartsEditorAction): TNyxText;
{ Public Card/Column/Label/Button composition. Realized named paths and authored
  remove descriptors are both listed, so hidden parts remain restorable.
  Rows fit the existing model node ceiling; excess input refuses, never truncates.
  The returned form owns only its controls and copied inert metadata. }
function NewNyxPartsEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AInstance: TNyxControlRef): INyxCard;
{ Unrelated controls return False. A recognized action must belong to this live
  form and its exact fresh row/baseline. Refusal leaves Proposal undefined and
  changes neither form nor document. Controllers apply its existing typed patch. }
function CaptureNyxPartsEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AProposal: TNyxPartsEditorProposal): Boolean;
{ Copy only a valid live form action's presentation identity. Unrelated,
  malformed or unavailable actions return False and clear Focus. The host must
  capture physical focus itself and retain its ordinary project/load guards. }
function TryCaptureNyxPartsEditorFocus(AButton, AShellRoot: TNyxNode;
  out AFocus: TNyxPartsEditorFocus): Boolean;

implementation

uses
  nyx.codec, nyx.composition, nyx.data, nyx.text.index;

const
  CEditor = 'nyx.parts-editor';
  CVersion = 'nyx.parts-editor.version';
  CInstance = 'nyx.parts-editor.instance';
  CBaseline = NyxPartsEditorBaselineKey;
  CPath = 'nyx.parts-editor.path';
  CDescriptor = 'nyx.parts-editor.descriptor';
  CRow = 'nyx.parts-editor.row';
  CAction = 'nyx.parts-editor.action';
  CFormVersion = '1';
  COverridePath = 'path';
  CRootPath = '.';
  CActionNames: array[TNyxPartsEditorAction] of TNyxText = ('hide', 'restore');
  { Four chrome nodes plus four controls per row share the existing tree cap.
    This is a complete-form bound, not a larger document or component allowance. }
  CMaximumRows = (NyxMaximumNodes - 4) div 4;

type
  { A detached row has no borrowed runtime pointer. The source rule remains an
    exact typed identity, while reason text explains an unavailable action. }
  TPartRow = record
    Path: TNyxPartRef;
    Descriptor: TNyxControlRef;
    Action: TNyxPartsEditorAction;
    Available: Boolean;
    Reason: TNyxText;
  end;
  { Temporary copied rows preserve observed order; an index only detects paths
    and resolves rules. No vector or mutable index survives in the public form. }
  TPartRows = array of TPartRow;

procedure CheckAction(AAction: TNyxPartsEditorAction);
begin

  if (Ord(AAction) < Ord(Low(TNyxPartsEditorAction))) or
    (Ord(AAction) > Ord(High(TNyxPartsEditorAction))) then
    raise ENyxModel.Create('Unknown instance-part action');
end;

function ActionName(AAction: TNyxPartsEditorAction): TNyxText;
begin
  CheckAction(AAction);
  Result := CActionNames[AAction];
end;

function TNyxPartsEditorFocus.GetDefined: Boolean;
begin
  Result := (FFormID <> '') and (FInstance.ID <> '') and
    (FPath.Name <> '') and (FPath.Name <> CRootPath);
end;

function TryCaptureNyxPartsEditorFocus(AButton, AShellRoot: TNyxNode;
  out AFocus: TNyxPartsEditorFocus): Boolean;
var
  LEditor, LRow, LBody: TNyxNode;
  LFormID, LPath, LInstance, LOrdinal: TNyxText;
  LAt, LChildAt, LActions: Integer;
  LAction: TNyxPartsEditorAction;
  LCandidate: TNyxPartsEditorFocus;
begin
  Result := False;
  AFocus := Default(TNyxPartsEditorFocus);

  if (AButton = nil) or (AShellRoot = nil) or
    (AButton.ProjectionKind <> NyxKindName(nkButton)) or
    (AButton.Prop(NyxAttributeName(atEnabled), 'true') <> 'true') or
    (AButton.Prop(NyxAttributeName(atVisible), 'true') <> 'true') then
    Exit;
  LFormID := AButton.Prop(CEditor);

  if LFormID = '' then
    Exit;
  LEditor := AShellRoot.Find(LFormID);

  if (LEditor = nil) or (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LEditor.Prop(CVersion) <> CFormVersion) or (LEditor.Find(AButton.ID) <> AButton) then
    Exit;
  LInstance := LEditor.Prop(CInstance);
  LPath := AButton.Prop(CPath);

  if (LInstance = '') or (LPath = '') or (LPath = CRootPath) or
    (LPath[1] = '/') or (LPath[Length(LPath)] = '/') or (Pos('//', LPath) > 0) or
    (AButton.Prop(CDescriptor) = '') then
    Exit;
  LOrdinal := AButton.Prop(CRow);

  if not TryStrToInt(LOrdinal, LAt) or (LAt < 0) or (LAt >= CMaximumRows) then
    Exit;

  if LOrdinal <> TNyxText(IntToStr(LAt)) then
    Exit;

  if AButton.Prop(CAction) = CActionNames[npaHide] then
    LAction := npaHide
  else if AButton.Prop(CAction) = CActionNames[npaRestore] then
    LAction := npaRestore
  else
    Exit;
  LRow := AButton.Parent;

  if (LRow = nil) or (LRow.ProjectionKind <> NyxKindName(nkColumn)) or
    (LRow.Prop(CPath) <> LPath) or
    (LRow.SourceID <> LFormID + '-row-' + LOrdinal) or
    (AButton.Prop(NyxAttributeName(atPart)) <> 'action') or
    (AButton.SourceID <> NyxPartsEditorActionID(LFormID, LAt, LAction)) then
    Exit;
  LBody := LRow.Parent;

  if (LBody = nil) or (LBody.Parent <> LEditor) or
    (LBody.ProjectionKind <> NyxKindName(nkColumn)) or
    (LBody.SourceID <> LFormID + '-rows') or
    (LBody.Prop(NyxAttributeName(atPart)) <> 'rows') or
    (LBody.Count > CMaximumRows) or (LAt >= LBody.Count) then
    Exit;

  if LBody.Children[LAt] <> LRow then
    Exit;

  if LRow.Count > NyxMaximumNodes then
    Exit;
  LActions := 0;
  for LChildAt := 0 to LRow.Count - 1 do
  begin

    if LRow.Children[LChildAt].Prop(NyxAttributeName(atPart)) = 'action' then
      Inc(LActions);
  end;

  if LActions <> 1 then
    Exit;
  LCandidate := Default(TNyxPartsEditorFocus);
  LCandidate.FFormID := LFormID;
  LCandidate.FInstance := NyxControl(LInstance);
  LCandidate.FPath := NyxPart(LPath);
  AFocus := LCandidate;
  Result := True;
end;

function TNyxPartsEditorFocus.Resolve(ARoot: TNyxNode): TNyxControlRef;
var
  LEditor, LBody, LRow, LButton: TNyxNode;
  LRowAt, LChildAt, LMatches, LPaths, LVisited: Integer;
  LCurrent: TNyxPartsEditorFocus;
  LCandidate: TNyxControlRef;
begin
  Result := Default(TNyxControlRef);

  if not Defined or (ARoot = nil) then
    Exit;
  LEditor := ARoot.Find(FFormID);

  if (LEditor = nil) or (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LEditor.Prop(CVersion) <> CFormVersion) or
    (LEditor.Prop(CInstance) <> FInstance.ID) then
    Exit;
  LBody := LEditor.Find(FFormID + '-rows');

  if (LBody = nil) or (LBody.Parent <> LEditor) or
    (LBody.ProjectionKind <> NyxKindName(nkColumn)) or
    (LBody.Count > CMaximumRows) then
    Exit;
  LMatches := 0;
  LPaths := 0;
  LVisited := 0;
  LCandidate := Default(TNyxControlRef);
  for LRowAt := 0 to LBody.Count - 1 do
  begin
    Inc(LVisited);

    if LVisited > NyxMaximumNodes then
      Exit;
    LRow := LBody.Children[LRowAt];

    if LRow.Prop(CPath) <> FPath.Name then
      Continue;
    Inc(LPaths);

    if LPaths > 1 then
      Exit;
    for LChildAt := 0 to LRow.Count - 1 do
    begin
      Inc(LVisited);

      if LVisited > NyxMaximumNodes then
        Exit;
      LButton := LRow.Children[LChildAt];

      if LButton.Prop(NyxAttributeName(atPart)) <> 'action' then
        Continue;
      Inc(LMatches);

      if (LMatches > 1) or
        not TryCaptureNyxPartsEditorFocus(LButton, ARoot, LCurrent) then
        Exit;

      if (LCurrent.FormID <> FFormID) or (LCurrent.Instance.ID <> FInstance.ID) or
        (LCurrent.Path.Name <> FPath.Name) then
        Exit;
      LCandidate := NyxControl(LButton.ID);
    end;
  end;

  if LMatches = 1 then
    Result := LCandidate;
end;

function RequireInstance(ADocument: TNyxDocument;
  const AInstance: TNyxControlRef): TNyxNode;
begin

  if (ADocument = nil) or (AInstance.ID = '') then
    raise ENyxModel.Create('Parts editor requires its owning document and instance');
  Result := ADocument.Find(AInstance.ID);

  if (Result = nil) or (Result.ProjectionKind <> NyxKindName(nkComponent)) then
    raise ENyxModel.Create('Parts editor requires a current reusable instance');
end;

function NyxPartsEditorBaseline(ADocument: TNyxDocument;
  const AInstance: TNyxControlRef): TNyxText;
begin
  RequireInstance(ADocument, AInstance);
  Result := NyxObject([NyxField('instance', NyxData(AInstance.ID)),
    NyxField('document', NyxData(TNyxCodec.Encode(ADocument)))]).ToJSON;
end;

function FreeDescriptor(ADocument: TNyxDocument): TNyxControlRef;
var
  LName: TNyxText;
  LSerial: Integer;
begin
  LName := 'part-hidden';
  LSerial := 1;
  while ADocument.Find(LName) <> nil do
  begin
    Inc(LSerial);

    if LSerial > NyxMaximumNodes + 1 then
      raise ENyxModel.Create('Instance-part descriptor identity budget exhausted');
    LName := 'part-hidden-' + TNyxText(IntToStr(LSerial));
  end;
  Result := NyxControl(LName);
end;

function CollectRows(ADocument: TNyxDocument;
  const AInstance: TNyxControlRef): TPartRows;
var
  LInstance, LRuntime, LRule: TNyxNode;
  LRules, LPaths: TNyxTextIndex;
  LCount, LCapacity, LVisited, LAt, LRuleAt: Integer;
  LFree: TNyxControlRef;
  LRows: TPartRows;

  procedure Append(const APath: TNyxText; AHidden: Boolean);
  var
    LNewCapacity, LProperty: Integer;
  begin

    if (APath = '') or (APath = CRootPath) or (LPaths.IndexOf(APath) >= 0) then
      raise ENyxModel.Create('Instance parts need distinct non-root named paths');

    if LCount >= CMaximumRows then
      raise ENyxModel.Create('Complete instance-parts form exceeds the model node allowance');

    if LCount = LCapacity then
    begin
      LNewCapacity := LCapacity * 2 + 16;

      if LNewCapacity > CMaximumRows then
        LNewCapacity := CMaximumRows;
      SetLength(LRows, LNewCapacity);
      LCapacity := LNewCapacity;
    end;
    LRows[LCount].Path := NyxPart(APath);
    LRows[LCount].Descriptor := LFree;
    LRows[LCount].Available := True;
    LRows[LCount].Action := npaHide;
    LRows[LCount].Reason := 'Inherited part';
    LRuleAt := LRules.IndexOf(APath);

    if LRuleAt >= 0 then
    begin
      LRule := LInstance.Children[LRuleAt];
      LRows[LCount].Descriptor := NyxControl(LRule.ID);
      LRows[LCount].Reason := 'Customized part';

      if LRule.Count > 0 then
      begin
        LRows[LCount].Available := False;
        LRows[LCount].Reason := 'This part has custom content; hiding cannot discard it.';
      end;
      for LProperty := 0 to LRule.Props.Count - 1 do

        if (LRule.Props.Names[LProperty] <> COverridePath) and
          (LRule.Props.Names[LProperty] <> NyxAttributeName(atOverrideMode)) then
        begin
          LRows[LCount].Available := False;
          LRows[LCount].Reason := 'This part has local properties; hiding cannot discard them.';
        end;

      if LRule.BindingCount > 0 then
      begin
        LRows[LCount].Available := False;
        LRows[LCount].Reason := 'This part has local bindings; hiding cannot discard them.';
      end;
    end;

    if AHidden then
    begin

      if (LRuleAt < 0) or (LInstance.Children[LRuleAt].Prop(NyxAttributeName(atOverrideMode)) <>
        NyxOverrideName(noRemove)) then
        raise ENyxModel.Create('A hidden part requires its exact local remove descriptor');
      LRows[LCount].Action := npaRestore;
      LRows[LCount].Reason := 'Hidden in this instance';
    end;
    LPaths.AddFirst(APath, LCount);
    Inc(LCount);
  end;

  procedure Visit(ANode: TNyxNode; const APath: TNyxText; ADepth: Integer);
  var
    LIndex: Integer;
    LChild: TNyxNode;
    LPath: TNyxText;
  begin
    Inc(LVisited);

    if (LVisited > NyxMaximumNodes) or (ADepth > NyxMaximumTreeDepth) then
      raise ENyxModel.Create('Instance-part traversal exceeds the existing model allowance');
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LChild := ANode.Children[LIndex];

      if LChild.Prop(NyxAttributeName(atPart)) = '' then
        Continue;
      LPath := LChild.Prop(NyxAttributeName(atPart));

      if APath <> '' then
        LPath := APath + '/' + LPath;
      Append(LPath, False);
      Visit(LChild, LPath, ADepth + 1);
    end;
  end;

begin
  Result := nil;
  LRows := nil;
  LInstance := RequireInstance(ADocument, AInstance);
  LRules := TNyxTextIndex.Create;
  LPaths := TNyxTextIndex.Create;
  LRuntime := nil;
  LCount := 0;
  LCapacity := 0;
  LVisited := 0;
  try
    LFree := FreeDescriptor(ADocument);
    for LAt := 0 to LInstance.Count - 1 do
    begin
      LRule := LInstance.Children[LAt];

      if (LRule.Kind <> NyxKindName(nkSlotOverride)) or
        (LRules.IndexOf(LRule.Prop(COverridePath)) >= 0) then
        raise ENyxModel.Create('Instance-part rules require unique authored descriptors');
      LRules.AddFirst(LRule.Prop(COverridePath), LAt);
    end;
    LRuntime := RealizeNyxView(ADocument, LInstance);
    Visit(LRuntime, '', 1);
    for LAt := 0 to LInstance.Count - 1 do
    begin
      LRule := LInstance.Children[LAt];

      if LRule.Prop(NyxAttributeName(atOverrideMode)) = NyxOverrideName(noRemove) then
        Append(LRule.Prop(COverridePath), True);
    end;
    SetLength(LRows, LCount);
    Result := LRows;
  finally
    LRuntime.Free;
    LPaths.Free;
    LRules.Free;
  end;
end;

procedure Preflight(ADocument: TNyxDocument; const AProposal: TNyxPartsEditorProposal);
var
  LCandidate: TNyxDocument;
  LInstance, LRule, LRuntime: TNyxNode;
begin
  LCandidate := ADocument.Clone;
  LRuntime := nil;
  try
    LInstance := RequireInstance(LCandidate, AProposal.Instance);

    if AProposal.Action = npaHide then
      LInstance.OverridePart(AProposal.Path, noRemove).Named(AProposal.Descriptor.ID)
    else
    begin
      LRule := LCandidate.Find(AProposal.Descriptor.ID);

      if (LRule = nil) or (LRule.Parent <> LInstance) or
        (LRule.Prop(COverridePath) <> AProposal.Path.Name) or
        (LRule.Prop(NyxAttributeName(atOverrideMode)) <> NyxOverrideName(noRemove)) then
        raise ENyxModel.Create('The exact hidden-part descriptor is no longer available');
      { Remove extracts the child then releases its ownership lease. The clone
        owns this rule exclusively; no additional Free is permitted here. }
      LInstance.Remove(LRule);
    end;
    LCandidate.Validate;
    LRuntime := RealizeNyxView(LCandidate, LInstance);

    if AProposal.Action = npaRestore then
      LRuntime.Part(AProposal.Path);
  finally
    LRuntime.Free;
    LCandidate.Free;
  end;
end;

function ProposalFromRow(ADocument: TNyxDocument; const AInstance: TNyxControlRef;
  const ARow: TPartRow; const ABaseline: TNyxText): TNyxPartsEditorProposal;
var
  LCandidate: TNyxPartsEditorProposal;
begin

  if not ARow.Available then
    raise ENyxModel.Create(ARow.Reason);
  LCandidate := Default(TNyxPartsEditorProposal);
  LCandidate.FInstance := AInstance;
  LCandidate.FPath := ARow.Path;
  LCandidate.FDescriptor := ARow.Descriptor;
  LCandidate.FAction := ARow.Action;
  LCandidate.FBaseline := ABaseline;
  Preflight(ADocument, LCandidate);
  Result := LCandidate;
end;

function NyxPartsEditorProposal(ADocument: TNyxDocument;
  const AInstance: TNyxControlRef; const APath: TNyxPartRef;
  AAction: TNyxPartsEditorAction): TNyxPartsEditorProposal;
var
  LBaseline: TNyxText;
  LRows: TPartRows;
  LAt: Integer;
begin
  Result := Default(TNyxPartsEditorProposal);
  CheckAction(AAction);

  if (APath.Name = '') or (APath.Name = CRootPath) then
    raise ENyxModel.Create('Only a named non-root instance part can be hidden or restored');
  LBaseline := NyxPartsEditorBaseline(ADocument, AInstance);
  LRows := CollectRows(ADocument, AInstance);
  for LAt := 0 to High(LRows) do

    if LRows[LAt].Path.Name = APath.Name then
    begin

      if LRows[LAt].Action <> AAction then
        raise ENyxModel.Create('Choose the current Hide or Restore action for this part');
      Exit(ProposalFromRow(ADocument, AInstance, LRows[LAt], LBaseline));
    end;
  raise ENyxModel.Create('The named instance part is no longer available: ' + APath.Name);
end;

procedure ValidateNyxPartsEditorProposal(ADocument: TNyxDocument;
  const AProposal: TNyxPartsEditorProposal);
var
  LCurrent: TNyxPartsEditorProposal;
begin

  if (AProposal.Baseline = '') or
    (NyxPartsEditorBaseline(ADocument, AProposal.Instance) <> AProposal.Baseline) then
    raise ENyxModel.Create('Instance parts changed; reopen this form before applying');
  LCurrent := NyxPartsEditorProposal(ADocument, AProposal.Instance, AProposal.Path, AProposal.Action);

  if LCurrent.Descriptor.ID <> AProposal.Descriptor.ID then
    raise ENyxModel.Create('The exact instance-part descriptor identity changed');
end;

function NyxPartsEditorActionID(const AID: TNyxText; ARowIndex: Integer;
  AAction: TNyxPartsEditorAction): TNyxText;
begin
  CheckAction(AAction);

  if (AID = '') or (ARowIndex < 0) or (ARowIndex >= CMaximumRows) then
    raise ENyxModel.Create('Parts action requires a form and bounded row ordinal');
  Result := AID + '-row-' + TNyxText(IntToStr(ARowIndex)) + '-' + ActionName(AAction);
end;

function NewNyxPartsEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AInstance: TNyxControlRef): INyxCard;
var
  LBaseline, LRowName, LCaption: TNyxText;
  LRows: TPartRows;
  LCandidate: INyxCard;
  LBody, LRow: INyxColumn;
  LButton: INyxButton;
  LAt: Integer;
begin

  if AID = '' then
    raise ENyxModel.Create('Parts editor requires a stable form identity');
  LBaseline := NyxPartsEditorBaseline(ADocument, AInstance);
  LRows := CollectRows(ADocument, AInstance);
  LCandidate := NewNyxCard(AID);
  LCandidate.Configure.Gap(8).Padding(12).Done;
  LCandidate.Node.SetProp(CVersion, CFormVersion).SetProp(CInstance, AInstance.ID).SetProp(CBaseline, LBaseline);
  LCandidate.Add(NewNyxLabel(AID + '-title').WithText('Instance parts'));
  LCandidate.Add(NewNyxLabel(AID + '-help').WithText(
    'Hide a named part in this instance. Restore original removes the local override and inherits the definition; ' +
    'it does not recover earlier local customizations.'));
  LBody := NewNyxColumn(AID + '-rows');
  LBody.Configure.PartName(NyxPart('rows')).Gap(8).Done;
  LCandidate.Add(LBody);
  for LAt := 0 to High(LRows) do
  begin
    LRowName := 'row-' + TNyxText(IntToStr(LAt));
    LRow := NewNyxColumn(AID + '-' + LRowName);
    LRow.Configure.PartName(NyxPart(LRowName)).Gap(4).Done;
    LBody.Add(LRow);
    LRow.Node.SetProp(CPath, LRows[LAt].Path.Name);
    LRow.Add(NewNyxLabel(AID + '-' + LRowName + '-path').Configure
      .PartName(NyxPart('path')).Text(LRows[LAt].Path.Name).Done);
    LRow.Add(NewNyxLabel(AID + '-' + LRowName + '-status').Configure
      .PartName(NyxPart('status')).Text(LRows[LAt].Reason).Done);
    LCaption := 'Hide part';

    if LRows[LAt].Action = npaRestore then
      LCaption := 'Restore original';
    LButton := NewNyxButton(NyxPartsEditorActionID(AID, LAt, LRows[LAt].Action));
    LButton.Configure.PartName(NyxPart('action')).Text(LCaption)
      .AccessibleName(LCaption + ': ' + LRows[LAt].Path.Name).Enabled(LRows[LAt].Available).Done;
    LButton.Node.SetProp(CEditor, AID).SetProp(CRow, IntToStr(LAt))
      .SetProp(CAction, ActionName(LRows[LAt].Action)).SetProp(CPath, LRows[LAt].Path.Name)
      .SetProp(CDescriptor, LRows[LAt].Descriptor.ID);
    LRow.Add(LButton);
  end;

  if Length(LRows) = 0 then
    LBody.Add(NewNyxLabel(AID + '-empty').WithText('This instance has no named parts.'));
  Result := LCandidate;
end;

function CaptureNyxPartsEditor(AButton, AShellRoot: TNyxNode;
  ADocument: TNyxDocument; out AProposal: TNyxPartsEditorProposal): Boolean;
var
  LEditor, LParent: TNyxNode;
  LID, LBaseline: TNyxText;
  LInstance: TNyxControlRef;
  LRows: TPartRows;
  LAt: Integer;
  LAction: TNyxPartsEditorAction;
  LCandidate: TNyxPartsEditorProposal;
begin
  Result := False;
  AProposal := Default(TNyxPartsEditorProposal);

  if (AButton = nil) or (AButton.Prop(CEditor) = '') then
    Exit;
  LID := AButton.Prop(CEditor);

  if (AShellRoot = nil) or (AButton.ProjectionKind <> NyxKindName(nkButton)) then
    raise ENyxModel.Create('Parts action requires its live typed owning form');
  LEditor := AShellRoot.Find(LID);

  if (LEditor = nil) or (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (LEditor.Prop(CVersion) <> CFormVersion) or (LEditor.Find(AButton.ID) <> AButton) then
    raise ENyxModel.Create('Parts action belongs to a missing or different form');
  LInstance := NyxControl(LEditor.Prop(CInstance));
  LBaseline := NyxPartsEditorBaseline(ADocument, LInstance);

  if LBaseline <> LEditor.Prop(CBaseline) then
    raise ENyxModel.Create('Instance parts changed; reopen this form before applying');

  if not TryStrToInt(AButton.Prop(CRow), LAt) or (LAt < 0) then
    raise ENyxModel.Create('Parts action requires its exact row ordinal');
  LRows := CollectRows(ADocument, LInstance);

  if LAt >= Length(LRows) then
    raise ENyxModel.Create('The current parts row is no longer available');
  LAction := LRows[LAt].Action;
  LParent := AButton.Parent;

  if (AButton.SourceID <> NyxPartsEditorActionID(LID, LAt, LAction)) or
    (AButton.Prop(CAction) <> ActionName(LAction)) or
    (AButton.Prop(CPath) <> LRows[LAt].Path.Name) or
    (AButton.Prop(CDescriptor) <> LRows[LAt].Descriptor.ID) or
    (LParent = nil) or (LParent.Prop(CPath) <> LRows[LAt].Path.Name) then
    raise ENyxModel.Create('Parts action does not match its exact current row');
  LCandidate := ProposalFromRow(ADocument, LInstance, LRows[LAt], LBaseline);
  AProposal := LCandidate;
  Result := True;
end;

end.
