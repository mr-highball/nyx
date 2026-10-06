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
unit nyx.content.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.content, nyx.responsive, nyx.presentations;

type
  { Closed fields of the reusable editor. IDs are stable composition identities,
    not executable property names. Sizes are logical available-content pixels;
    minimum is inclusive, maximum exclusive, and zero maximum means unbounded. }
  TNyxContentEditorField = (ncfScope, ncfPlatform, ncfRecipe, ncfPresentation,
    ncfWidthMinimum, ncfWidthMaximum, ncfHeightMinimum, ncfHeightMaximum,
    ncfOrientation, ncfApply);

  { Capture owns an independent registry and exact mounted baseline. The caller
    must compare that baseline with its current owner before admitting the edit.
    No document, rendered control or recipe definition survives capture. }
  TNyxContentEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
  end;

function NyxContentEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxContentEditorField): TNyxText;
{ Public Nyx compound: specialized card/select/spin/button/label interfaces.
  All inputs are borrowed only during composition, then copied into immutable
  metadata. The returned card and its parent own descendants normally. Application
  names retain exact Unicode; separate fields prevent caption/name collisions.
  Empty choices remain visible but cannot be applied. Existing rules show their
  exact scope and offer removal; upserting that scope retains its order. }
function NewNyxContentEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  const AContent: INyxContent; const ADefault: TNyxComponentRef;
  const ARecipes: array of TNyxComponentRef;
  const APresentations: array of TNyxPresentationRef): INyxCard;
{ Recognizes only a mounted editor's own apply/remove buttons. Unrelated controls
  return False. Missing fields, stale/forged buttons, invalid choices and incomplete
  intervals raise without changing the editor or accepted model. Closed choice
  failures use ENyxContent; invalid numeric intervals use EArgumentException.
  AContent returns an independent complete candidate; document admission remains
  responsible for every recipe dependency and inactive branch. }
function CaptureNyxContentEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxContentEditorChange; out AContent: INyxContent): Boolean;

implementation

const
  CEditorKey = 'nyx.content-editor';
  COwnerKey = 'nyx.content-editor.owner';
  CBaselineKey = 'nyx.content-editor.baseline';
  CRecipesKey = 'nyx.content-editor.recipes';
  CPresentationsKey = 'nyx.content-editor.presentations';
  CRemoveKey = 'nyx.content-editor.remove';
  CFields: array[TNyxContentEditorField] of TNyxText =
    ('scope', 'platform', 'recipe', 'presentation', 'width-minimum', 'width-maximum',
      'height-minimum', 'height-maximum', 'orientation', 'apply');
  CScopes: array[TNyxContentScope] of TNyxText =
    ('Default', 'Available size', 'Named presentation');
  CPlatforms: array[TNyxPlatform] of TNyxText = ('All targets', 'Browser', 'Native LCL');
  COrientations: array[TNyxViewportOrientation] of TNyxText =
    ('Any orientation', 'Portrait', 'Landscape', 'Square');

function NyxContentEditorFieldID(const AEditorID: TNyxText;
  AField: TNyxContentEditorField): TNyxText;
begin
  Result := AEditorID + TNyxText('-') + CFields[AField];
end;

function RuleCaption(const ARule: TNyxContentRule): TNyxText;
begin
  Result := CScopes[ARule.Scope];

  if ARule.Scope = ncsViewport then
  begin
    Result := ARule.Viewport.Caption;
  end
  else if ARule.Scope = ncsPresentation then
  begin
    Result := TNyxText('Presentation / ') + ARule.Presentation.Name;
  end;
  Result := Result + TNyxText(' · ') + CPlatforms[ARule.Platform] + TNyxText(' → ') +
    ARule.Component.Name;
end;

function NewNyxContentEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  const AContent: INyxContent; const ADefault: TNyxComponentRef;
  const ARecipes: array of TNyxComponentRef;
  const APresentations: array of TNyxPresentationRef): INyxCard;
var
  LContent: INyxContent;
  LRecipes: array of TNyxDataValue;
  LPresentations: array of TNyxDataValue;
  LItems: TNyxStrings;
  LNames: TNyxText;
  LFirst: TNyxText;
  LIndex: Integer;
  LRow: INyxColumn;
  LButton: INyxButton;
begin

  if (AID = '') or (AOwner.ID = '') then
  begin
    raise ENyxContent.Create('Content editor requires exact editor and control identities');
  end;
  LContent := NewNyxContent;

  if AContent <> nil then
  begin
    LContent := AContent.Clone.Done;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Done;
  Result.Node.SetProp(COwnerKey, AOwner.ID).SetProp(CBaselineKey, LContent.ToData.ToJSON);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).Configure.Text('Content recipes').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).Configure.Text(
    'Use a different reusable component for a size, presentation or target. Each choice replaces the whole recipe.').Done);
  Result.Add(NewNyxLabel(AID + TNyxText('-default')).Configure.Text(
    TNyxText('Current default / ') + ADefault.Name).Done);
  Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfScope)).Configure.Text('When')
    .Items(CScopes[ncsDefault] + #10 + CScopes[ncsViewport] + #10 + CScopes[ncsPresentation])
    .Value(CScopes[ncsDefault]).Done);
  Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfPlatform)).Configure.Text('Target')
    .Items(CPlatforms[npfAny] + #10 + CPlatforms[npfBrowser] + #10 + CPlatforms[npfNativeLCL])
    .Value(CPlatforms[npfAny]).Done);
  LItems := TNyxStrings.Create;
  try
    SetLength(LRecipes, Length(ARecipes));
    for LIndex := 0 to High(ARecipes) do
    begin
      LRecipes[LIndex] := NyxData(ARecipes[LIndex].Name);
      LItems.Add(ARecipes[LIndex].Name);
    end;
    LNames := LItems.Join(#10);
    LFirst := '';

    if Length(ARecipes) > 0 then
    begin
      LFirst := ARecipes[0].Name;
    end;
    for LIndex := 0 to High(ARecipes) do
    begin

      if ARecipes[LIndex].Name = ADefault.Name then
      begin
        LFirst := ADefault.Name;
        Break;
      end;
    end;
    Result.Node.SetProp(CRecipesKey, NyxArray(LRecipes).ToJSON);
    Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfRecipe)).Configure.Text('Reusable recipe')
      .Items(LNames).Value(LFirst).Enabled(Length(ARecipes) > 0).Done);
    LItems.Clear;
    SetLength(LPresentations, Length(APresentations));
    for LIndex := 0 to High(APresentations) do
    begin
      LPresentations[LIndex] := NyxData(APresentations[LIndex].Name);
      LItems.Add(APresentations[LIndex].Name);
    end;
    LNames := LItems.Join(#10);
    LFirst := '';

    if Length(APresentations) > 0 then
    begin
      LFirst := APresentations[0].Name;
    end;
    Result.Node.SetProp(CPresentationsKey, NyxArray(LPresentations).ToJSON);
    Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfPresentation)).Configure.Text('Named presentation')
      .Items(LNames).Value(LFirst).Enabled(Length(APresentations) > 0).Done);
  finally
    LItems.Free;
  end;
  Result.Add(NewNyxLabel(AID + TNyxText('-size-help')).Configure.Text(
    'Size fields apply to Available size. Minimums include the boundary; upper bounds exclude it. Zero upper bound means no limit.').Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfWidthMinimum)).Configure
    .Text('Minimum width').Minimum(0).Maximum(1000000).Value(0).Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfWidthMaximum)).Configure
    .Text('Below width').Minimum(0).Maximum(1000000).Value(640).Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfHeightMinimum)).Configure
    .Text('Minimum height').Minimum(0).Maximum(1000000).Value(0).Done);
  Result.Add(NewNyxSpin(NyxContentEditorFieldID(AID, ncfHeightMaximum)).Configure
    .Text('Below height').Minimum(0).Maximum(1000000).Value(0).Done);
  Result.Add(NewNyxSelect(NyxContentEditorFieldID(AID, ncfOrientation)).Configure.Text('Available orientation')
    .Items(COrientations[nvoAny] + #10 + COrientations[nvoPortrait] + #10 +
      COrientations[nvoLandscape] + #10 + COrientations[nvoSquare])
    .Value(COrientations[nvoAny]).Done);
  LButton := NewNyxButton(NyxContentEditorFieldID(AID, ncfApply));
  LButton.Configure.Text('Use recipe in this scope').Enabled(Length(ARecipes) > 0).Done;
  LButton.Node.SetProp(CEditorKey, AID);
  Result.Add(LButton);
  for LIndex := 0 to LContent.Count - 1 do
  begin
    LRow := NewNyxColumn(AID + TNyxText('-rule-') + TNyxText(IntToStr(LIndex)));
    LRow.Configure.Gap(4).Done;
    LRow.Add(NewNyxLabel(LRow.ID + TNyxText('-caption')).Configure
      .Text(RuleCaption(LContent.Rule(LIndex))).Done);
    LButton := NewNyxButton(LRow.ID + TNyxText('-remove'));
    LButton.Configure.Text('Remove this choice').Done;
    LButton.Node.SetProp(CEditorKey, AID).SetProp(CRemoveKey, TNyxText(IntToStr(LIndex)));
    LRow.Add(LButton);
    Result.Add(LRow);
  end;
end;

function CaptureNyxContentEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxContentEditorChange; out AContent: INyxContent): Boolean;
var
  LEditor: TNyxNode;
  LEditorID: TNyxText;
  LScope: TNyxContentScope;
  LPlatform: TNyxPlatform;
  LOrientation: TNyxViewportOrientation;
  LCondition: TNyxViewportCondition;
  LIndex: Integer;
  LRule: TNyxContentRule;
  LTarget: INyxContent;
  LMinimum: Integer;
  LMaximum: Integer;

  function Value(AField: TNyxContentEditorField): TNyxText;
  var
    LField: TNyxNode;
  begin
    LField := LEditor.Find(NyxContentEditorFieldID(LEditorID, AField));

    if LField = nil then
    begin
      raise ENyxContent.Create('Content editor field is no longer mounted');
    end;
    Result := LField.Prop('value');
  end;

  function Number(AField: TNyxContentEditorField): Integer;
  begin

    if not TryStrToInt(Value(AField), Result) or (Result < 0) or (Result > 1000000) then
    begin
      raise ENyxContent.Create('Content sizes require complete integers from 0 to 1000000');
    end;
  end;

  function Choice(const AValue: TNyxText; const AChoices: array of TNyxText): Integer;
  var
    LChoice: Integer;
  begin
    for LChoice := 0 to High(AChoices) do
    begin

      if AValue = AChoices[LChoice] then
      begin
        Exit(LChoice);
      end;
    end;
    raise ENyxContent.Create('Choose an available content scope, target or orientation');
  end;

  function Reference(const AKey: TNyxText; AField: TNyxContentEditorField): TNyxText;
  var
    LValues: TNyxDataValue;
    LChoice: Integer;
  begin
    Result := Value(AField);
    LValues := TNyxDataValue.ParseJSON(LEditor.Prop(AKey));
    for LChoice := 0 to LValues.Count - 1 do
    begin

      if Result = LValues.Item(LChoice).AsText then
      begin
        Exit;
      end;
    end;
    raise ENyxContent.Create('Choose an available reusable recipe or named presentation');
  end;
begin
  AChange := Default(TNyxContentEditorChange);
  AContent := nil;
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
    raise ENyxContent.Create('Select the current content editor before applying a choice');
  end;
  AChange.Owner := NyxControl(LEditor.Prop(COwnerKey));
  AChange.Baseline := LEditor.Prop(CBaselineKey);
  AContent := NyxContentFromData(TNyxDataValue.ParseJSON(AChange.Baseline));

  if AButton.Prop(CRemoveKey) <> '' then
  begin

    if not TryStrToInt(AButton.Prop(CRemoveKey), LIndex) then
    begin
      raise ENyxContent.Create('Recipe removal requires its exact registered scope');
    end;
    LRule := AContent.Rule(LIndex);
    LTarget := AContent;
    case LRule.Scope of
      ncsDefault: LTarget := AContent;
      ncsViewport: LTarget := LTarget.WhenViewport(LRule.Viewport);
      ncsPresentation: LTarget := LTarget.WhenPresentation(LRule.Presentation);
    end;
    LTarget.ForPlatform(LRule.Platform).Clear;
    AContent := AContent.Done;
    Exit;
  end;

  if AButton.ID <> NyxContentEditorFieldID(LEditorID, ncfApply) then
  begin
    raise ENyxContent.Create('Unknown content editor command');
  end;
  LScope := TNyxContentScope(Choice(Value(ncfScope), CScopes));
  LPlatform := TNyxPlatform(Choice(Value(ncfPlatform), CPlatforms));
  LTarget := AContent;
  case LScope of
    ncsDefault: LTarget := AContent;
    ncsViewport:
      begin
        LOrientation := TNyxViewportOrientation(Choice(Value(ncfOrientation), COrientations));
        LCondition := TNyxViewportCondition.Any.Orientation(LOrientation);
        LMinimum := Number(ncfWidthMinimum);
        LMaximum := Number(ncfWidthMaximum);

        if LMaximum = 0 then
        begin
          LCondition := LCondition.WidthAtLeast(LMinimum);
        end
        else
        begin
          LCondition := LCondition.WidthBetween(LMinimum, LMaximum);
        end;
        LMinimum := Number(ncfHeightMinimum);
        LMaximum := Number(ncfHeightMaximum);

        if LMaximum = 0 then
        begin
          LCondition := LCondition.HeightAtLeast(LMinimum);
        end
        else
        begin
          LCondition := LCondition.HeightBetween(LMinimum, LMaximum);
        end;

        if LCondition.IsAny then
        begin
          raise ENyxContent.Create('Choose Default for an unrestricted size');
        end;
        LTarget := LTarget.WhenViewport(LCondition);
      end;
    ncsPresentation:
      LTarget := LTarget.WhenPresentation(NyxPresentation(Reference(CPresentationsKey, ncfPresentation)));
  end;
  LTarget.ForPlatform(LPlatform).Use(NyxComponent(Reference(CRecipesKey, ncfRecipe)));
  AContent := AContent.Done;
end;

end.
