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


{ Maintained by tools/nyx_control_contracts.lpr; edit the contract source, then regenerate. }
unit nyx.controls.documentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.catalog, nyx.documentation.declarations,
  nyx.documentation.source, nyx.documentation.typeexpressions, nyx.pascal.binding;

{ Bind an explicitly supplied declaration package to the generated built-in
  creators. No files, backend, controls or callbacks are opened or executed.
  This companion maps declarations; it does not manufacture missing core facts
  or turn a partial source package into a complete API reference. }
function NewNyxControlsDocumentationSource(
  const APackage: INyxDocumentationPackage): INyxCatalogDocumentationSource;
{ Match generated creator intent to exact current public source declarations.
  Returned ordinals belong only to this world and are input annotations for
  CaptureNyxDocumentationSourcePackage, never persisted member identities.
  The caller owns the shared construction; this function does not retire it. }
function NyxControlsDocumentationCreators(const AWorld: INyxPascalSemanticWorld;
  AModuleIndex: Integer; const AConstruction: INyxDocumentationTypeConstruction):
  TNyxDocumentationSourceCreators;

implementation

uses SysUtils, nyx.text, nyx.types, nyx.documentation,
  nyx.documentation.registration, nyx.pascal.types, nyx.pascal.syntax,
  nyx.pascal.syntax.translated;

type
  { One captured package supplies the complete real unit descriptor. Exact
    source-provided member identities survive overload lookup; enum ordinals
    never become member identities. The catalog borrows this immutable mapping
    only while creating its ordinary retained package bindings. }
  TControlDocumentationSource = class(TInterfacedObject, INyxCatalogDocumentationSource)
  private
    FPackage: INyxDocumentationPackage;
    FUnit: INyxDocumentationType;
    FPrimaries: array[TNyxKind] of TNyxDocumentationRef;
    function RequireFactory(const AName, AResult, AIDDefault,
      AConstructionDefault: TNyxText): TNyxDocumentationRef;
  public
    constructor Create(const APackage: INyxDocumentationPackage);
    function PackageOwner: INyxDocumentationPackage;
    function PrimaryForKind(AKind: TNyxKind): TNyxDocumentationRef;
  end;

function NyxControlsDocumentationCreators(const AWorld: INyxPascalSemanticWorld;
  AModuleIndex: Integer; const AConstruction: INyxDocumentationTypeConstruction):
  TNyxDocumentationSourceCreators;
var
  LWorld: INyxPascalSemanticWorld;
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LConstruction: INyxDocumentationTypeConstruction;
  LWindows: INyxDocumentationTextWindows;
  LValues: TNyxDocumentationSourceCreators;

  procedure Spend(AUnits: Integer = 1);
  begin
    CheckNyxDocumentationTypeConstruction(LConstruction);
    LConstruction.Spend(AUnits);
    CheckNyxDocumentationTypeConstruction(LConstruction);
  end;

  procedure Refuse(const AName: TNyxText);
  begin
    raise ENyxDocumentation.Create('Generated creator source is missing, stale or ambiguous: ' + AName);
  end;

  procedure RequireFactory(AKind: TNyxKind; const AName, AResult,
    AIDDefault, AConstructionDefault: TNyxText);
  var
    LAt, LFound, LMatches, LParameterAt, LParameterCount: Integer;
    LDeclaration, LParameter: TNyxPascalSyntaxDeclaration;

    procedure RequireParameter(const AExpectedName, AExpectedType,
      AExpectedDefault: TNyxText; AMode: TNyxPascalSyntaxParameterMode);
    begin
      Spend;

      if (LParameter.Name <> AExpectedName) or (LParameter.ParameterMode <> AMode) or
        (LParameter.Flags <> [psdfDefaultArgument]) or
        (LParameter.TypeSpan.Length = 0) or (LParameter.ExpressionSpan.Length = 0) then
        Refuse(AName);

      if (Trim(LWindows.CopyWindow(LParameter.TypeSpan)) <> AExpectedType) or
        (LWindows.CopyWindow(LParameter.ExpressionSpan) <> AExpectedDefault) then
        Refuse(AName);
    end;

  begin
    Spend;
    LFound := -1;
    LMatches := 0;
    LAt := LSyntax.Named(0, AName);
    while LAt >= 0 do
    begin
      Spend;
      LDeclaration := LSyntax.Declaration(LAt);

      if LDeclaration.Visibility = psavInterface then
      begin
        Inc(LMatches);
        LFound := LAt;
      end;
      LAt := LDeclaration.NextNamed;
    end;

    if LMatches <> 1 then
      Refuse(AName);
    LDeclaration := LSyntax.Declaration(LFound);

    if (LDeclaration.Kind <> psdkRoutine) or (LDeclaration.RoutineKind <> prkFunction) or
      (LDeclaration.Scope <> 0) or (LDeclaration.OwnerName <> '') or
      (LDeclaration.Flags * [psdfIncomplete, psdfConditional, psdfUnsupported,
        psdfIncompleteHeader, psdfClassMember, psdfStatic] <> []) or
      (LDeclaration.GenericSpan.Length <> 0) or (LDeclaration.TypeSpan.Length = 0) or
      (LDeclaration.NestedScope < 0) then
      Refuse(AName);

    if not LSyntax.Scope(LDeclaration.NestedScope).Complete or
      LSyntax.Scope(LDeclaration.NestedScope).UnsupportedSyntax then
      Refuse(AName);
    LParameterAt := LSyntax.Scope(LDeclaration.NestedScope).FirstDeclaration;
    LParameterCount := 0;
    while LParameterAt >= 0 do
    begin
      Spend;
      LParameter := LSyntax.Declaration(LParameterAt);

      if (LParameter.Kind <> psdkParameter) or
        (LParameter.Scope <> LDeclaration.NestedScope) then
        Refuse(AName);
      case LParameterCount of
        0: RequireParameter('AID', 'TNyxText', AIDDefault, pspmConst);
        1: RequireParameter('AConstruction', 'TNyxConstruction', AConstructionDefault, pspmValue);
      else
        Refuse(AName);
      end;
      Inc(LParameterCount);
      LParameterAt := LParameter.Next;
    end;

    if (LParameterCount <> 2) or
      (Trim(LWindows.CopyWindow(LDeclaration.TypeSpan)) <> AResult) then
      Refuse(AName);
    LValues[Ord(AKind)].ModuleIndex := AModuleIndex;
    LValues[Ord(AKind)].DeclarationIndex := LFound;
  end;

begin
  Result := nil;
  { Passed values are leased before any admission callback. Only exact owned
    immutable views are inspected; no alternate getter supplies source facts. }
  LWorld := AWorld;
  LConstruction := AConstruction;
  CheckNyxDocumentationTypeConstruction(LConstruction);
  Spend;

  if not NyxPascalOwnedSemanticWorld(LWorld) then
    Refuse('world');

  if (AModuleIndex < 0) or (AModuleIndex >= LWorld.Count) then
    Refuse('module');
  LView := LWorld.ModuleAt(AModuleIndex);

  if not NyxPascalOwnedTranslatedSyntax(LView) then
    Refuse('view');

  if not LView.CompleteTranslation or not LView.Source.Resolved then
    Refuse('translation');
  LSyntax := LView.Syntax;

  if (LSyntax.DiagnosticCount <> 0) or (LSyntax.ScopeCount = 0) then
    Refuse('syntax');

  if (LSyntax.Scope(0).Name <> 'nyx.controls') or
    not LSyntax.Scope(0).Complete or LSyntax.Scope(0).UnsupportedSyntax then
    Refuse('nyx.controls');
  Spend(Ord(High(TNyxKind)) + 2);
  SetLength(LValues, Ord(High(TNyxKind)) + 1);
  LWindows := NewNyxDocumentationTextWindows(LSyntax.Lexical.Source, LConstruction);
  try
    RequireFactory(nkPage, 'NewNyxPage', 'INyxPage', '''''', 'ncoDefault');
    RequireFactory(nkColumn, 'NewNyxColumn', 'INyxColumn', '''''', 'ncoDefault');
    RequireFactory(nkRow, 'NewNyxRow', 'INyxRow', '''''', 'ncoDefault');
    RequireFactory(nkGrid, 'NewNyxGrid', 'INyxGrid', '''''', 'ncoDefault');
    RequireFactory(nkPanel, 'NewNyxPanel', 'INyxPanel', '''''', 'ncoDefault');
    RequireFactory(nkCard, 'NewNyxCard', 'INyxCard', '''''', 'ncoDefault');
    RequireFactory(nkGroup, 'NewNyxGroup', 'INyxGroup', '''''', 'ncoDefault');
    RequireFactory(nkToolbar, 'NewNyxToolbar', 'INyxToolbar', '''''', 'ncoDefault');
    RequireFactory(nkScroll, 'NewNyxScroll', 'INyxScroll', '''''', 'ncoDefault');
    RequireFactory(nkTabs, 'NewNyxTabs', 'INyxTabs', '''''', 'ncoDefault');
    RequireFactory(nkTab, 'NewNyxTab', 'INyxTab', '''''', 'ncoDefault');
    RequireFactory(nkHeading, 'NewNyxHeading', 'INyxHeading', '''''', 'ncoDefault');
    RequireFactory(nkLabel, 'NewNyxLabel', 'INyxLabel', '''''', 'ncoDefault');
    RequireFactory(nkButton, 'NewNyxButton', 'INyxButton', '''''', 'ncoDefault');
    RequireFactory(nkLink, 'NewNyxLink', 'INyxLink', '''''', 'ncoDefault');
    RequireFactory(nkInput, 'NewNyxInput', 'INyxInput', '''''', 'ncoDefault');
    RequireFactory(nkMemo, 'NewNyxMemo', 'INyxMemo', '''''', 'ncoDefault');
    RequireFactory(nkCheckbox, 'NewNyxCheckbox', 'INyxCheckbox', '''''', 'ncoDefault');
    RequireFactory(nkSwitch, 'NewNyxSwitch', 'INyxSwitch', '''''', 'ncoDefault');
    RequireFactory(nkRadio, 'NewNyxRadio', 'INyxRadio', '''''', 'ncoDefault');
    RequireFactory(nkSelect, 'NewNyxSelect', 'INyxSelect', '''''', 'ncoDefault');
    RequireFactory(nkSpin, 'NewNyxSpin', 'INyxSpin', '''''', 'ncoDefault');
    RequireFactory(nkSlider, 'NewNyxSlider', 'INyxSlider', '''''', 'ncoDefault');
    RequireFactory(nkDate, 'NewNyxDate', 'INyxDate', '''''', 'ncoDefault');
    RequireFactory(nkTime, 'NewNyxTime', 'INyxTime', '''''', 'ncoDefault');
    RequireFactory(nkColor, 'NewNyxColor', 'INyxColor', '''''', 'ncoDefault');
    RequireFactory(nkList, 'NewNyxList', 'INyxList', '''''', 'ncoDefault');
    RequireFactory(nkTable, 'NewNyxTable', 'INyxTable', '''''', 'ncoDefault');
    RequireFactory(nkTree, 'NewNyxTree', 'INyxTree', '''''', 'ncoDefault');
    RequireFactory(nkImage, 'NewNyxImage', 'INyxImage', '''''', 'ncoDefault');
    RequireFactory(nkAvatar, 'NewNyxAvatar', 'INyxAvatar', '''''', 'ncoDefault');
    RequireFactory(nkProgress, 'NewNyxProgress', 'INyxProgress', '''''', 'ncoDefault');
    RequireFactory(nkBadge, 'NewNyxBadge', 'INyxBadge', '''''', 'ncoDefault');
    RequireFactory(nkAlert, 'NewNyxAlert', 'INyxAlert', '''''', 'ncoDefault');
    RequireFactory(nkSeparator, 'NewNyxSeparator', 'INyxSeparator', '''''', 'ncoDefault');
    RequireFactory(nkSpacer, 'NewNyxSpacer', 'INyxSpacer', '''''', 'ncoDefault');
    RequireFactory(nkCode, 'NewNyxCode', 'INyxCode', '''''', 'ncoDefault');
    RequireFactory(nkCodeEditor, 'NewNyxCodeEditor', 'INyxCodeEditor', '''''', 'ncoDefault');
    RequireFactory(nkDesignSurface, 'NewNyxDesignSurface', 'INyxDesignSurface', '''''', 'ncoDefault');
    RequireFactory(nkComponent, 'NewNyxComponent', 'INyxComponent', '''''', 'ncoDefault');
    RequireFactory(nkSplitView, 'NewNyxSplitView', 'INyxSplitView', '''''', 'ncoDefault');
    RequireFactory(nkLabeledButton, 'NewNyxLabeledButton', 'INyxLabeledButton', '''''', 'ncoDefault');
    RequireFactory(nkSplitButton, 'NewNyxSplitButton', 'INyxSplitButton', '''''', 'ncoDefault');
    RequireFactory(nkSearchField, 'NewNyxSearchField', 'INyxSearchField', '''''', 'ncoDefault');
    RequireFactory(nkFormField, 'NewNyxFormField', 'INyxFormField', '''''', 'ncoDefault');
    RequireFactory(nkLoginForm, 'NewNyxLoginForm', 'INyxLoginForm', '''''', 'ncoDefault');
    RequireFactory(nkSettingsPanel, 'NewNyxSettingsPanel', 'INyxSettingsPanel', '''''', 'ncoDefault');
    RequireFactory(nkDateRange, 'NewNyxDateRange', 'INyxDateRange', '''''', 'ncoDefault');
    RequireFactory(nkNumberStepper, 'NewNyxNumberStepper', 'INyxNumberStepper', '''''', 'ncoDefault');
    RequireFactory(nkSegmentedControl, 'NewNyxSegmentedControl', 'INyxSegmentedControl', '''''', 'ncoDefault');
    RequireFactory(nkRating, 'NewNyxRating', 'INyxRating', '''''', 'ncoDefault');
    RequireFactory(nkDataToolbar, 'NewNyxDataToolbar', 'INyxDataToolbar', '''''', 'ncoDefault');
    RequireFactory(nkFilterBar, 'NewNyxFilterBar', 'INyxFilterBar', '''''', 'ncoDefault');
    RequireFactory(nkPagination, 'NewNyxPagination', 'INyxPagination', '''''', 'ncoDefault');
    RequireFactory(nkBreadcrumbs, 'NewNyxBreadcrumbs', 'INyxBreadcrumbs', '''''', 'ncoDefault');
    RequireFactory(nkSidebarNav, 'NewNyxSidebarNav', 'INyxSidebarNav', '''''', 'ncoDefault');
    RequireFactory(nkMasterDetail, 'NewNyxMasterDetail', 'INyxMasterDetail', '''''', 'ncoDefault');
    RequireFactory(nkListCard, 'NewNyxListCard', 'INyxListCard', '''''', 'ncoDefault');
    RequireFactory(nkDataCard, 'NewNyxDataCard', 'INyxDataCard', '''''', 'ncoDefault');
    RequireFactory(nkKanbanBoard, 'NewNyxKanbanBoard', 'INyxKanbanBoard', '''''', 'ncoDefault');
    RequireFactory(nkTimeline, 'NewNyxTimeline', 'INyxTimeline', '''''', 'ncoDefault');
    RequireFactory(nkStatCard, 'NewNyxStatCard', 'INyxStatCard', '''''', 'ncoDefault');
    RequireFactory(nkMetricGrid, 'NewNyxMetricGrid', 'INyxMetricGrid', '''''', 'ncoDefault');
    RequireFactory(nkEmptyState, 'NewNyxEmptyState', 'INyxEmptyState', '''''', 'ncoDefault');
    RequireFactory(nkConfirmationDialog, 'NewNyxConfirmationDialog', 'INyxConfirmationDialog', '''''', 'ncoDefault');
    RequireFactory(nkNotificationCard, 'NewNyxNotificationCard', 'INyxNotificationCard', '''''', 'ncoDefault');
    RequireFactory(nkToast, 'NewNyxToast', 'INyxToast', '''''', 'ncoDefault');
    RequireFactory(nkWizardStep, 'NewNyxWizardStep', 'INyxWizardStep', '''''', 'ncoDefault');
    RequireFactory(nkStepper, 'NewNyxStepper', 'INyxStepper', '''''', 'ncoDefault');
    RequireFactory(nkProfileCard, 'NewNyxProfileCard', 'INyxProfileCard', '''''', 'ncoDefault');
    RequireFactory(nkMediaCard, 'NewNyxMediaCard', 'INyxMediaCard', '''''', 'ncoDefault');
    RequireFactory(nkAvatarGroup, 'NewNyxAvatarGroup', 'INyxAvatarGroup', '''''', 'ncoDefault');
    RequireFactory(nkCommentThread, 'NewNyxCommentThread', 'INyxCommentThread', '''''', 'ncoDefault');
    RequireFactory(nkPropertyGrid, 'NewNyxPropertyGrid', 'INyxPropertyGrid', '''''', 'ncoDefault');
    RequireFactory(nkCommandBar, 'NewNyxCommandBar', 'INyxCommandBar', '''''', 'ncoDefault');
    RequireFactory(nkFloatingActionPanel, 'NewNyxFloatingActionPanel', 'INyxFloatingActionPanel', '''''', 'ncoDefault');
    RequireFactory(nkSlotOverride, 'NewNyxSlotOverride', 'INyxSlotOverride', '''''', 'ncoDefault');
    RequireFactory(nkDisclosure, 'NewNyxDisclosure', 'INyxDisclosure', '''''', 'ncoDefault');
    RequireFactory(nkCanvas, 'NewNyxCanvas', 'INyxCanvas', '''''', 'ncoDefault');
    RequireFactory(nkDrawing, 'NewNyxDrawing', 'INyxDrawing', '''''', 'ncoDefault');
    RequireFactory(nkToolCanvas, 'NewNyxToolCanvas', 'INyxToolCanvas', '''''', 'ncoDefault');
    CheckNyxDocumentationTypeConstruction(LConstruction);
    Result := LValues;
  finally
    LWindows.Retire;
  end;
end;

{ Only an exact bare name and its explicit nominal binding match these
  generated signatures. Compound syntax, unresolved/formal names and alias
  substitution never collapse into a convenient legacy named projection. }
function SameNamedUse(const AUse: INyxDocumentationTypeUse;
  const AExpected: TNyxDocumentationRef): Boolean;
var
  LExpression: INyxDocumentationTypeExpression;
begin
  Result := False;

  if AUse = nil then
    Exit;
  LExpression := AUse.Expression;

  if LExpression = nil then
    Exit(AUse.Reference.Same(AExpected));

  if not LExpression.Syntax.Complete or
    (LExpression.Syntax.Root < 0) or
    (LExpression.Syntax.Node(LExpression.Syntax.Root).Kind <> ptxName) or
    (LExpression.NamedUseCount <> 1) or
    (LExpression.NamedUseNode(0) <> LExpression.Syntax.Root) or
    (LExpression.NamedUseArity(0) <> 0) or (AUse.BindingCount <> 1) then
    Exit;
  Result := AUse.BindingAt(0).Same(AExpected);
end;

function SameLegacyParameter(const AParameter: TNyxDocumentationParameter;
  const AName: TNyxText; const AType: TNyxDocumentationRef;
  AMode: TNyxDocumentationParameterMode; const ADefault: TNyxText): Boolean;
begin
  Result := (AParameter.Name = AName) and (AParameter.Passing = AMode) and
    AParameter.TypeReference.Same(AType) and AParameter.HasDefault and
    (AParameter.DefaultPascal = ADefault);
end;

function SameRichParameter(const AParameter: INyxDocumentationParameterDeclaration;
  const AName: TNyxText; const AType: TNyxDocumentationRef;
  AMode: TNyxDocumentationParameterMode; const ADefault: TNyxText): Boolean;
begin
  Result := (AParameter <> nil) and (AParameter.Name = AName) and
    (AParameter.Passing = AMode) and SameNamedUse(AParameter.TypeUse, AType) and
    AParameter.HasDefault and (AParameter.DefaultPascal = ADefault);
end;

function TControlDocumentationSource.RequireFactory(const AName, AResult,
  AIDDefault, AConstructionDefault: TNyxText): TNyxDocumentationRef;
var
  LIndex, LMatches: Integer;
  LMember: INyxDocumentationMember;
  LSignature: INyxDocumentationSignature;
  LCapability: INyxDocumentationMemberSignature;
  LRich: INyxDocumentationRichSignature;
  LMatchesShape: Boolean;
begin
  Result := Default(TNyxDocumentationRef);
  LMatches := 0;
  for LIndex := 0 to FUnit.MemberCount - 1 do
  begin
    LMember := FUnit.MemberAt(LIndex);

    if LMember.Identity.MemberName <> AName then
      Continue;

    if not (LMember.Kind in [ndmFactory, ndmRoutine]) then
      Continue;
    LRich := nil;

    if Supports(LMember, INyxDocumentationMemberSignature, LCapability) then
      LRich := LCapability.DeclaredSignature;

    if LRich <> nil then
    begin
      LMatchesShape := (LRich.ParameterCount = 2) and
        SameNamedUse(LRich.ResultUse, NyxDocumentationType('nyx.controls', AResult)) and
        SameRichParameter(LRich.ParameterAt(0), 'AID',
          NyxDocumentationType('nyx.text', 'TNyxText'), ndpConst, AIDDefault) and
        SameRichParameter(LRich.ParameterAt(1), 'AConstruction',
          NyxDocumentationType('nyx.controls', 'TNyxConstruction'), ndpValue, AConstructionDefault);
    end
    else
    begin
      LSignature := LMember.Signature;
      LMatchesShape := (LSignature <> nil) and (LSignature.ParameterCount = 2) and
        LSignature.ResultType.Same(NyxDocumentationType('nyx.controls', AResult)) and
        SameLegacyParameter(LSignature.ParameterAt(0), 'AID',
          NyxDocumentationType('nyx.text', 'TNyxText'), ndpConst, AIDDefault) and
        SameLegacyParameter(LSignature.ParameterAt(1), 'AConstruction',
          NyxDocumentationType('nyx.controls', 'TNyxConstruction'), ndpValue, AConstructionDefault);
    end;

    if not LMatchesShape then
      Continue;
    Inc(LMatches);
    Result := LMember.Identity;
  end;

  if LMatches <> 1 then
    raise ENyxDocumentation.Create('Generated creator metadata is missing, stale or ambiguous: ' + AName);
end;

constructor TControlDocumentationSource.Create(const APackage: INyxDocumentationPackage);
begin
  inherited Create;
  FPackage := CaptureNyxDocumentationPackage(APackage);

  if FPackage = nil then
    raise ENyxDocumentation.Create('Generated creator metadata requires its declaration package');
  FUnit := FPackage.FindDeclaration(NyxDocumentationUnit('nyx.controls'));

  if FUnit = nil then
    raise ENyxDocumentation.Create('Generated creator metadata requires the complete controls unit');
  { These expectations come from the same checked declaration IR that emits
    interfaces, classes and factories. No second author-maintained help map is
    consulted. A supplied package retains its original declaration provenance. }
  FPrimaries[nkPage] := RequireFactory('NewNyxPage', 'INyxPage', '''''', 'ncoDefault');
  FPrimaries[nkColumn] := RequireFactory('NewNyxColumn', 'INyxColumn', '''''', 'ncoDefault');
  FPrimaries[nkRow] := RequireFactory('NewNyxRow', 'INyxRow', '''''', 'ncoDefault');
  FPrimaries[nkGrid] := RequireFactory('NewNyxGrid', 'INyxGrid', '''''', 'ncoDefault');
  FPrimaries[nkPanel] := RequireFactory('NewNyxPanel', 'INyxPanel', '''''', 'ncoDefault');
  FPrimaries[nkCard] := RequireFactory('NewNyxCard', 'INyxCard', '''''', 'ncoDefault');
  FPrimaries[nkGroup] := RequireFactory('NewNyxGroup', 'INyxGroup', '''''', 'ncoDefault');
  FPrimaries[nkToolbar] := RequireFactory('NewNyxToolbar', 'INyxToolbar', '''''', 'ncoDefault');
  FPrimaries[nkScroll] := RequireFactory('NewNyxScroll', 'INyxScroll', '''''', 'ncoDefault');
  FPrimaries[nkTabs] := RequireFactory('NewNyxTabs', 'INyxTabs', '''''', 'ncoDefault');
  FPrimaries[nkTab] := RequireFactory('NewNyxTab', 'INyxTab', '''''', 'ncoDefault');
  FPrimaries[nkHeading] := RequireFactory('NewNyxHeading', 'INyxHeading', '''''', 'ncoDefault');
  FPrimaries[nkLabel] := RequireFactory('NewNyxLabel', 'INyxLabel', '''''', 'ncoDefault');
  FPrimaries[nkButton] := RequireFactory('NewNyxButton', 'INyxButton', '''''', 'ncoDefault');
  FPrimaries[nkLink] := RequireFactory('NewNyxLink', 'INyxLink', '''''', 'ncoDefault');
  FPrimaries[nkInput] := RequireFactory('NewNyxInput', 'INyxInput', '''''', 'ncoDefault');
  FPrimaries[nkMemo] := RequireFactory('NewNyxMemo', 'INyxMemo', '''''', 'ncoDefault');
  FPrimaries[nkCheckbox] := RequireFactory('NewNyxCheckbox', 'INyxCheckbox', '''''', 'ncoDefault');
  FPrimaries[nkSwitch] := RequireFactory('NewNyxSwitch', 'INyxSwitch', '''''', 'ncoDefault');
  FPrimaries[nkRadio] := RequireFactory('NewNyxRadio', 'INyxRadio', '''''', 'ncoDefault');
  FPrimaries[nkSelect] := RequireFactory('NewNyxSelect', 'INyxSelect', '''''', 'ncoDefault');
  FPrimaries[nkSpin] := RequireFactory('NewNyxSpin', 'INyxSpin', '''''', 'ncoDefault');
  FPrimaries[nkSlider] := RequireFactory('NewNyxSlider', 'INyxSlider', '''''', 'ncoDefault');
  FPrimaries[nkDate] := RequireFactory('NewNyxDate', 'INyxDate', '''''', 'ncoDefault');
  FPrimaries[nkTime] := RequireFactory('NewNyxTime', 'INyxTime', '''''', 'ncoDefault');
  FPrimaries[nkColor] := RequireFactory('NewNyxColor', 'INyxColor', '''''', 'ncoDefault');
  FPrimaries[nkList] := RequireFactory('NewNyxList', 'INyxList', '''''', 'ncoDefault');
  FPrimaries[nkTable] := RequireFactory('NewNyxTable', 'INyxTable', '''''', 'ncoDefault');
  FPrimaries[nkTree] := RequireFactory('NewNyxTree', 'INyxTree', '''''', 'ncoDefault');
  FPrimaries[nkImage] := RequireFactory('NewNyxImage', 'INyxImage', '''''', 'ncoDefault');
  FPrimaries[nkAvatar] := RequireFactory('NewNyxAvatar', 'INyxAvatar', '''''', 'ncoDefault');
  FPrimaries[nkProgress] := RequireFactory('NewNyxProgress', 'INyxProgress', '''''', 'ncoDefault');
  FPrimaries[nkBadge] := RequireFactory('NewNyxBadge', 'INyxBadge', '''''', 'ncoDefault');
  FPrimaries[nkAlert] := RequireFactory('NewNyxAlert', 'INyxAlert', '''''', 'ncoDefault');
  FPrimaries[nkSeparator] := RequireFactory('NewNyxSeparator', 'INyxSeparator', '''''', 'ncoDefault');
  FPrimaries[nkSpacer] := RequireFactory('NewNyxSpacer', 'INyxSpacer', '''''', 'ncoDefault');
  FPrimaries[nkCode] := RequireFactory('NewNyxCode', 'INyxCode', '''''', 'ncoDefault');
  FPrimaries[nkCodeEditor] := RequireFactory('NewNyxCodeEditor', 'INyxCodeEditor', '''''', 'ncoDefault');
  FPrimaries[nkDesignSurface] := RequireFactory('NewNyxDesignSurface', 'INyxDesignSurface', '''''', 'ncoDefault');
  FPrimaries[nkComponent] := RequireFactory('NewNyxComponent', 'INyxComponent', '''''', 'ncoDefault');
  FPrimaries[nkSplitView] := RequireFactory('NewNyxSplitView', 'INyxSplitView', '''''', 'ncoDefault');
  FPrimaries[nkLabeledButton] := RequireFactory('NewNyxLabeledButton', 'INyxLabeledButton', '''''', 'ncoDefault');
  FPrimaries[nkSplitButton] := RequireFactory('NewNyxSplitButton', 'INyxSplitButton', '''''', 'ncoDefault');
  FPrimaries[nkSearchField] := RequireFactory('NewNyxSearchField', 'INyxSearchField', '''''', 'ncoDefault');
  FPrimaries[nkFormField] := RequireFactory('NewNyxFormField', 'INyxFormField', '''''', 'ncoDefault');
  FPrimaries[nkLoginForm] := RequireFactory('NewNyxLoginForm', 'INyxLoginForm', '''''', 'ncoDefault');
  FPrimaries[nkSettingsPanel] := RequireFactory('NewNyxSettingsPanel', 'INyxSettingsPanel', '''''', 'ncoDefault');
  FPrimaries[nkDateRange] := RequireFactory('NewNyxDateRange', 'INyxDateRange', '''''', 'ncoDefault');
  FPrimaries[nkNumberStepper] := RequireFactory('NewNyxNumberStepper', 'INyxNumberStepper', '''''', 'ncoDefault');
  FPrimaries[nkSegmentedControl] := RequireFactory('NewNyxSegmentedControl', 'INyxSegmentedControl', '''''', 'ncoDefault');
  FPrimaries[nkRating] := RequireFactory('NewNyxRating', 'INyxRating', '''''', 'ncoDefault');
  FPrimaries[nkDataToolbar] := RequireFactory('NewNyxDataToolbar', 'INyxDataToolbar', '''''', 'ncoDefault');
  FPrimaries[nkFilterBar] := RequireFactory('NewNyxFilterBar', 'INyxFilterBar', '''''', 'ncoDefault');
  FPrimaries[nkPagination] := RequireFactory('NewNyxPagination', 'INyxPagination', '''''', 'ncoDefault');
  FPrimaries[nkBreadcrumbs] := RequireFactory('NewNyxBreadcrumbs', 'INyxBreadcrumbs', '''''', 'ncoDefault');
  FPrimaries[nkSidebarNav] := RequireFactory('NewNyxSidebarNav', 'INyxSidebarNav', '''''', 'ncoDefault');
  FPrimaries[nkMasterDetail] := RequireFactory('NewNyxMasterDetail', 'INyxMasterDetail', '''''', 'ncoDefault');
  FPrimaries[nkListCard] := RequireFactory('NewNyxListCard', 'INyxListCard', '''''', 'ncoDefault');
  FPrimaries[nkDataCard] := RequireFactory('NewNyxDataCard', 'INyxDataCard', '''''', 'ncoDefault');
  FPrimaries[nkKanbanBoard] := RequireFactory('NewNyxKanbanBoard', 'INyxKanbanBoard', '''''', 'ncoDefault');
  FPrimaries[nkTimeline] := RequireFactory('NewNyxTimeline', 'INyxTimeline', '''''', 'ncoDefault');
  FPrimaries[nkStatCard] := RequireFactory('NewNyxStatCard', 'INyxStatCard', '''''', 'ncoDefault');
  FPrimaries[nkMetricGrid] := RequireFactory('NewNyxMetricGrid', 'INyxMetricGrid', '''''', 'ncoDefault');
  FPrimaries[nkEmptyState] := RequireFactory('NewNyxEmptyState', 'INyxEmptyState', '''''', 'ncoDefault');
  FPrimaries[nkConfirmationDialog] := RequireFactory('NewNyxConfirmationDialog', 'INyxConfirmationDialog', '''''', 'ncoDefault');
  FPrimaries[nkNotificationCard] := RequireFactory('NewNyxNotificationCard', 'INyxNotificationCard', '''''', 'ncoDefault');
  FPrimaries[nkToast] := RequireFactory('NewNyxToast', 'INyxToast', '''''', 'ncoDefault');
  FPrimaries[nkWizardStep] := RequireFactory('NewNyxWizardStep', 'INyxWizardStep', '''''', 'ncoDefault');
  FPrimaries[nkStepper] := RequireFactory('NewNyxStepper', 'INyxStepper', '''''', 'ncoDefault');
  FPrimaries[nkProfileCard] := RequireFactory('NewNyxProfileCard', 'INyxProfileCard', '''''', 'ncoDefault');
  FPrimaries[nkMediaCard] := RequireFactory('NewNyxMediaCard', 'INyxMediaCard', '''''', 'ncoDefault');
  FPrimaries[nkAvatarGroup] := RequireFactory('NewNyxAvatarGroup', 'INyxAvatarGroup', '''''', 'ncoDefault');
  FPrimaries[nkCommentThread] := RequireFactory('NewNyxCommentThread', 'INyxCommentThread', '''''', 'ncoDefault');
  FPrimaries[nkPropertyGrid] := RequireFactory('NewNyxPropertyGrid', 'INyxPropertyGrid', '''''', 'ncoDefault');
  FPrimaries[nkCommandBar] := RequireFactory('NewNyxCommandBar', 'INyxCommandBar', '''''', 'ncoDefault');
  FPrimaries[nkFloatingActionPanel] := RequireFactory('NewNyxFloatingActionPanel', 'INyxFloatingActionPanel', '''''', 'ncoDefault');
  FPrimaries[nkSlotOverride] := RequireFactory('NewNyxSlotOverride', 'INyxSlotOverride', '''''', 'ncoDefault');
  FPrimaries[nkDisclosure] := RequireFactory('NewNyxDisclosure', 'INyxDisclosure', '''''', 'ncoDefault');
  FPrimaries[nkCanvas] := RequireFactory('NewNyxCanvas', 'INyxCanvas', '''''', 'ncoDefault');
  FPrimaries[nkDrawing] := RequireFactory('NewNyxDrawing', 'INyxDrawing', '''''', 'ncoDefault');
  FPrimaries[nkToolCanvas] := RequireFactory('NewNyxToolCanvas', 'INyxToolCanvas', '''''', 'ncoDefault');
end;

function TControlDocumentationSource.PackageOwner: INyxDocumentationPackage;
begin
  Result := FPackage;
end;

function TControlDocumentationSource.PrimaryForKind(AKind: TNyxKind): TNyxDocumentationRef;
begin

  if (Ord(AKind) < Ord(Low(TNyxKind))) or (Ord(AKind) > Ord(High(TNyxKind))) then
    raise ENyxDocumentation.Create('Unknown generated creator kind');
  Result := FPrimaries[AKind];
end;

function NewNyxControlsDocumentationSource(
  const APackage: INyxDocumentationPackage): INyxCatalogDocumentationSource;
begin
  Result := TControlDocumentationSource.Create(APackage);
end;

end.
