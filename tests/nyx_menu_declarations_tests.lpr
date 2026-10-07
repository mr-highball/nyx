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

program nyx_menu_declarations_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$IFDEF PAS2JS}Web,{$ELSE}Classes,{$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.menu.types, nyx.popover.types,
  nyx.typeahead, nyx.menu.declarations, nyx.root.types, nyx.data,
  nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.composition, nyx.source,
  nyx.studio.agents, nyx.studio.projects, nyx.studio.edits, nyx.menu, nyx.schema;

type
  TFault = (mfEmpty, mfDuplicatePart, mfDoubleRadio, mfMissingBranch,
    mfCycle, mfTooDeep, mfTooWide, mfMissingRoot, mfMissingInvokerPlan,
    mfUnknownPolicy, mfStringBoolean, mfWrongItemFields, mfExtraField,
    mfScopedAttachment, mfOpaqueRootCollision, mfOpaqueNodeCollision,
    mfMissingPart, mfWrongPartKind, mfWrongInvokerProjection, mfInvokerAction);

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{ Edit structured values at the deliberate transport boundary. JSON whitespace
  is not semantic; refusal fixtures must actually change their claimed field. }
function WithField(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  SetLength(LFields, AObject.Count);
  LFound := False;
  for LIndex := 0 to AObject.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex].Value := AValue;
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

function Actions: INyxMenuDefinition;
begin
  Result := NewNyxMenuDefinition(NyxReusableRoot('actions-content'),
    NyxMenu('Document actions').Wrap(False)
      .Opening(nmoLast).TypeAhead(NyxTypeAhead.WindowMilliseconds(1700).Match(ntmExact))
      .Presentation(NyxPopover('Document actions').Size(320, 480)
        .Placement(npsAbove, npaEnd).Sizing(npzFixed).Spacing(11, 17)
        .Focus(NyxPart('copy')).DismissOn([npdEscape])))
    .Action(NyxPart('copy'), NyxMenuCommand('copy-draft'), False)
    .Check(NyxPart('guides'), NyxMenuCommand('show-guides'), True)
    .Separator(NyxPart('divider'))
    .Submenu(NyxPart('density'), NyxMenuRef('density'));
end;

function Density: INyxMenuDefinition;
begin
  Result := NewNyxMenuDefinition(NyxReusableRoot('density-content'), NyxMenu('Density'))
    .Radio(NyxPart('roomy'), NyxMenuCommand('roomy'), NyxMenuGroup('spacing'), True)
    .Radio(NyxPart('compact'), NyxMenuCommand('compact'), NyxMenuGroup('spacing'), False);
end;

{ English content roots are normal specialized Nyx controls. Unicode below is
  qualification data, independent of the starting demo language. }
function Design: TNyxDocument;
var
  LHome: INyxPage;
  LActions: INyxColumn;
  LDensity: INyxColumn;
  LButton: INyxButton;
  LSeparator: INyxSeparator;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Menu declarations';
    LHome := NewNyxPage('home');
    LButton := NewNyxButton('open-actions');
    LButton.Text := 'Actions';
    LButton.Configure.Menu(NyxMenuRef('actions')).Done;
    LHome.Add(LButton);
    Result.AddPage(LHome);
    LActions := NewNyxColumn('actions-content');
    LButton := NewNyxButton('copy-draft');
    LButton.Text := 'Copy draft';
    LButton.Configure.PartName(NyxPart('copy')).Done;
    LActions.Add(LButton);
    LButton := NewNyxButton('show-guides');
    LButton.Text := 'Show guides';
    LButton.Configure.PartName(NyxPart('guides')).Done;
    LActions.Add(LButton);
    LSeparator := NewNyxSeparator('action-divider');
    LSeparator.Configure.PartName(NyxPart('divider')).Done;
    LActions.Add(LSeparator);
    LButton := NewNyxButton('choose-density');
    LButton.Text := 'Density';
    LButton.Configure.PartName(NyxPart('density')).Done;
    LActions.Add(LButton);
    Result.AddComponent(LActions);
    LDensity := NewNyxColumn('density-content');
    LButton := NewNyxButton('roomy-density');
    LButton.Configure.PartName(NyxPart('roomy')).Text('Roomy').Done;
    LDensity.Add(LButton);
    LButton := NewNyxButton('compact-density');
    LButton.Configure.PartName(NyxPart('compact')).Text('Compact').Done;
    LDensity.Add(LButton);
    Result.AddComponent(LDensity);
    Result.Menus.Define(NyxMenuRef('actions'), Actions)
      .Define(NyxMenuRef('density'), Density);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Fault(AKind: TFault);
var
  LDocument: TNyxDocument;
  LBook: INyxMenuDeclarations;
  LPlan: INyxMenuDefinition;
  LIndex: Integer;
  LText: TNyxText;
  LData: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LFailed: Boolean;
begin
  LDocument := Design;
  LBook := NewNyxMenuDeclarations;
  LFailed := False;
  try
    try
      case AKind of
        mfEmpty:
          LBook.Define(NyxMenuRef('empty'), NewNyxMenuDefinition(
            NyxPageRoot('home'), NyxMenu('Empty')));
        mfDuplicatePart:
          LPlan := Actions.Action(NyxPart('copy'), NyxMenuCommand('duplicate'));
        mfDoubleRadio:
          LPlan := Density.Radio(NyxPart('other'), NyxMenuCommand('other'),
            NyxMenuGroup('spacing'), True);
        mfMissingBranch:
          begin
            LBook.Define(NyxMenuRef('actions'), Actions);
            LBook.Validate;
          end;
        mfCycle:
          begin
            LPlan := NewNyxMenuDefinition(NyxPageRoot('home'), NyxMenu('Cycle'))
              .Submenu(NyxPart('branch'), NyxMenuRef('cycle'));
            LBook.Define(NyxMenuRef('cycle'), LPlan);
            LBook.Validate;
          end;
        mfTooDeep:
          begin
            for LIndex := 0 to 8 do
            begin
              LPlan := NewNyxMenuDefinition(NyxPageRoot('home'), NyxMenu('Depth'));

              if LIndex < 8 then
              begin
                LPlan := LPlan.Submenu(NyxPart('branch'),
                  NyxMenuRef('level-' + IntToStr(LIndex + 1)));
              end
              else
              begin
                LPlan := LPlan.Action(NyxPart('finish'), NyxMenuCommand('finish'));
              end;
              LBook.Define(NyxMenuRef('level-' + IntToStr(LIndex)), LPlan);
            end;
            LBook.Validate;
          end;
        mfTooWide:
          begin
            LPlan := NewNyxMenuDefinition(NyxPageRoot('home'), NyxMenu('Branches'));
            for LIndex := 0 to 255 do
            begin
              LPlan := LPlan.Submenu(NyxPart('branch-' + IntToStr(LIndex)),
                NyxMenuRef('leaves'));
            end;
            LBook.Define(NyxMenuRef('branches'), LPlan);
            LPlan := NewNyxMenuDefinition(NyxPageRoot('home'), NyxMenu('Leaves'));
            for LIndex := 0 to 255 do
            begin
              LPlan := LPlan.Action(NyxPart('leaf-' + IntToStr(LIndex)),
                NyxMenuCommand('command-' + IntToStr(LIndex)));
            end;
            LBook.Define(NyxMenuRef('leaves'), LPlan);
            LBook.Validate;
          end;
        mfMissingRoot:
          begin
            LDocument.Menus.Define(NyxMenuRef('missing-root'),
              NewNyxMenuDefinition(NyxReusableRoot('absent'), NyxMenu('Absent'))
                .Action(NyxPart('copy'), NyxMenuCommand('copy-draft')));
            LDocument.Validate;
          end;
        mfMissingInvokerPlan:
          begin
            LDocument.Find('open-actions').Configure.Menu(NyxMenuRef('absent')).Done;
            LDocument.Validate;
          end;
        mfUnknownPolicy:
          NyxMenuDefinitionFromData(TNyxDataValue.ParseJSON(
            StringReplace(Actions.ToData.ToJSON, '"above"', '"sideways"', [])));
        mfStringBoolean:
          begin
            LData := Actions.ToData;
            NyxMenuDefinitionFromData(WithField(LData, 'options',
              WithField(LData.Field('options'), 'wrap', NyxData('false'))));
          end;
        mfWrongItemFields:
          begin
            LData := Actions.ToData;
            SetLength(LItems, LData.Field('items').Count);
            for LIndex := 0 to High(LItems) do
            begin
              LItems[LIndex] := LData.Field('items').Item(LIndex);
            end;
            LItems[0] := WithField(LItems[0], 'unexpected', NyxData(True));
            NyxMenuDefinitionFromData(WithField(LData, 'items', NyxArray(LItems)));
          end;
        mfExtraField:
          begin
            LText := Actions.ToData.ToJSON;
            LText := '{"extra":true,' + Copy(LText, 2, MaxInt);
            NyxMenuDefinitionFromData(TNyxDataValue.ParseJSON(LText));
          end;
        mfScopedAttachment:
          LDocument.Find('open-actions').Configure.ForPlatform(npfBrowser)
            .Menu(NyxMenuRef('actions'));
        mfOpaqueRootCollision:
          begin
            LDocument.Extensions.SetValue(NyxExtension('menus'), NyxData('legacy'));
            LDocument.Validate;
          end;
        mfOpaqueNodeCollision:
          begin
            LDocument.Find('open-actions').Extensions.SetValue(
              NyxExtension('menu'), NyxData('legacy'));
            LDocument.Validate;
          end;
        mfMissingPart:
          begin
            LDocument.Menus.Define(NyxMenuRef('actions'),
              NewNyxMenuDefinition(NyxReusableRoot('actions-content'), NyxMenu('Missing'))
                .Action(NyxPart('absent'), NyxMenuCommand('absent')));
            ValidateNyxDocumentProperties(LDocument);
          end;
        mfWrongPartKind:
          begin
            LDocument.Menus.Define(NyxMenuRef('actions'),
              NewNyxMenuDefinition(NyxReusableRoot('actions-content'), NyxMenu('Wrong kind'))
                .Action(NyxPart('divider'), NyxMenuCommand('divider')));
            ValidateNyxDocumentProperties(LDocument);
          end;
        mfWrongInvokerProjection:
          begin
            LDocument.Find('open-actions').Configure.ProjectAs(nkMemo).Done;
            ValidateNyxDocumentProperties(LDocument);
          end;
        mfInvokerAction:
          begin
            LDocument.Find('open-actions').Configure.Action(naToggle).Done;
            ValidateNyxDocumentProperties(LDocument);
          end;
      end;
    except
      on LException: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Required menu declaration refusal / ' + IntToStr(Ord(AKind)));
  finally
    LDocument.Free;
  end;
end;

procedure Journey;
const
  CUnicode: TNyxText = 'Actions 👋 / café';
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LView: TNyxNode;
  LBook: INyxMenuDeclarations;
  LPlan: INyxMenuDefinition;
  LPrior: INyxMenuDefinition;
  LReference: TNyxMenuRef;
  LPart: TNyxPartRef;
  LWire: TNyxText;
  LSource: TNyxText;
  LFault: TFault;
  LWorkspace: TNyxSourceWorkspace;
  {$IFNDEF PAS2JS}
  LStream: TFileStream;
  {$ENDIF}
begin
  LDocument := Design;
  LCopy := nil;
  LView := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LPrior := Density;
    LPlan := LPrior.Action(NyxPart('reset'), NyxMenuCommand('reset-density'));
    Check((LPrior.Count = 2) and (LPlan.Count = 3), 'Builder keeps prior plan immutable');
    LBook := LDocument.Menus.Clone;
    LReference := LBook.Reference(0);
    LReference.Name := 'changed';
    Check(LBook.Reference(0).Name = 'actions', 'Returned reference cannot edit registry');
    LPart := LBook.Definition(NyxMenuRef('actions')).Item(0).Part;
    LPart.Name := 'changed';
    Check(LBook.Definition(NyxMenuRef('actions')).Item(0).Part.Name = 'copy',
      'Returned part cannot edit a retained entry');
    LBook.Define(NyxMenuRef('density'), LPlan);
    Check(LBook.Reference(1).Name = 'density', 'Replacement retains original position');
    Check(LDocument.Menus.Definition(NyxMenuRef('density')).Count = 2,
      'Registry clone cannot alter document defaults');
    LBook.Remove(NyxMenuRef('actions'));
    Check(LDocument.Menus.Count = 2, 'Removing cloned declaration leaves owner unchanged');
    LBook := nil;
    Check(LPlan.Item(0).IsChecked, 'Retained immutable plan survives registry retirement');
    LWire := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger = 6,
      'Typed menu meaning selects version six');
    LCopy := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LCopy) = LWire, 'Complete portable design round trips exactly');
    Check(LCopy.Find('open-actions').MenuReference.Name = 'actions',
      'Specialized invoker reference survives persistence');
    Check(LCopy.Menus.Definition(NyxMenuRef('actions')).Options.Placement.InitialFocus.Name = 'copy',
      'Complete authored policy survives persistence');
    Check(LCopy.Menus.Definition(NyxMenuRef('actions')).Item(3).Submenu.Name = 'density',
      'Nested declaration reference survives persistence');
    LView := RealizeNyxView(LCopy, LCopy.Pages[0]);
    Check(LView.Find('open-actions').MenuReference.Name = 'actions',
      'Actual portable composition retains invoker meaning');
    LView.Free;
    LView := nil;
    LCopy.Find('open-actions').Configure.NoMenu.Done;
    Check(LCopy.Find('open-actions').HasMenu and
      (LCopy.Find('open-actions').MenuReference.Name = ''),
      'Explicit clear differs from missing inheritance');
    LView := LCopy.Find('open-actions').Clone;
    Check(LView.HasMenu and (LView.MenuReference.Name = ''), 'Clone retains explicit clear');
    LView.Free;
    LView := nil;
    LCopy.Find('open-actions').Configure.InheritMenu.Done;
    Check(not LCopy.Find('open-actions').HasMenu, 'Inherit removes local attachment');
    Check(LDocument.Find('open-actions').MenuReference.Name = 'actions',
      'Cloned node configuration leaves source document unchanged');
    LCopy.Free;
    LCopy := nil;
    LPlan := NewNyxMenuDefinition(NyxReusableRoot('actions-content'),
      NyxMenu(CUnicode)).Action(NyxPart('copy'), NyxMenuCommand(CUnicode))
      .Check(NyxPart('guides'), NyxMenuCommand('show-guides'), True)
      .Separator(NyxPart('divider'))
      .Submenu(NyxPart('density'), NyxMenuRef('density'));
    LDocument.Menus.Define(NyxMenuRef(CUnicode), LPlan);
    LDocument.Find('open-actions').Configure.Menu(NyxMenuRef(CUnicode)).Done;
    LWire := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LWire);
    Check(LCopy.Menus.Definition(NyxMenuRef(CUnicode)).Options.Placement.Title = CUnicode,
      'Supplementary Unicode policy title is exact');
    Check(LCopy.Menus.Definition(NyxMenuRef(CUnicode)).Item(0).Command.Name = CUnicode,
      'Supplementary Unicode command/reference is exact');
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.menu');
    Check(Pos('NewNyxMenuDefinition(', LSource) > 0, 'Generation uses specialized declarations');
    Check(Pos('.Menu(NyxMenuRef(', LSource) > 0, 'Generation uses a typed invoker reference');
    Check(Pos('.Radio(NyxPart(', LSource) > 0, 'Generation preserves typed radio/group meaning');
    Check(Pos('NyxMenuDefinitionFromData', LSource) = 0, 'Generation uses crafted fluent Pascal');
    Check(TNyxCodegen.Generate(LCopy, 'nyx.generated.menu') = LSource,
      'No-op persistence gives deterministic generated Pascal');
    LCopy.Free;
    LCopy := nil;
    LCopy := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCopy) = LWire,
      'Managed source reconstructs complete typed menu definitions and invokers');
    LWorkspace.Accept(LDocument, LSource);
    Check(LWorkspace.Render(LDocument) = LSource,
      'Menu source admission retains the deterministic companion');
    {$IFNDEF PAS2JS}

    if ParamCount > 0 then
    begin
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LStream.WriteBuffer(Pointer(LSource)^, Length(LSource));
      finally
        LStream.Free;
      end;
    end;
    {$ENDIF}
    for LFault := Low(TFault) to High(TFault) do
    begin
      Fault(LFault);
    end;
  finally
    LWorkspace.Free;
    LView.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

procedure SemanticJourney;
var
  LDocument: TNyxDocument;
  LAgent: TNyxAgentSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LReply: TNyxDataValue;
  LPlan: INyxMenuDefinition;
  LRecipe: INyxMenuRecipe;
  LCopy: TNyxDocument;
  LFailure: Boolean;
begin
  LDocument := Design;
  LAgent := nil;
  LCopy := nil;
  try
    ValidateNyxDocumentProperties(LDocument);
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.generated.menu'));
    LAgent := TNyxAgentSession.Create(LBefore);
    LAgent.InheritPermission(apEdit);
    LPlan := NewNyxMenuDefinition(NyxReusableRoot('density-content'), NyxMenu('Spacing 👋'))
      .Radio(NyxPart('roomy'), NyxMenuCommand('roomy'), NyxMenuGroup('spacing'), False)
      .Radio(NyxPart('compact'), NyxMenuCommand('compact'), NyxMenuGroup('spacing'), True);
    LReply := LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(1)), NyxField('operationId', NyxData('menu-change')),
      NyxField('operations', NyxArray([
        NyxDefineMenu(NyxMenuRef('density'), LPlan).ToData,
        NyxAttachMenu(NyxControl('open-actions'), NyxMenuRef('density')).ToData]))]));
    Check(LAgent.Revision = 2, 'Grouped menu definition/attachment changes one revision');
    LAfter := LAgent.ReviewSeed(2);
    Check(Pos('Spacing 👋', LAfter.Source) > 0, 'Semantic menu edit changes the actual paired Pascal');
    Check(Pos('.Menu(NyxMenuRef(''density''))', LAfter.Source) > 0,
      'Semantic menu attachment uses the public typed builder');
    LReply := LAgent.Call('nyx_menus', 'Scooty', NyxObject([
      NyxField('limit', NyxData(1))]));
    Check((LReply.Field('definitions').Count = 1) and LReply.Field('hasMore').AsBoolean,
      'Menu summaries remain bounded and expose continuation');
    LReply := LAgent.Call('nyx_menus', 'Scooty', NyxObject([
      NyxField('name', NyxData('density')), NyxField('itemOffset', NyxData(1)),
      NyxField('itemLimit', NyxData(1)), NyxField('textOffset', NyxData(8)),
      NyxField('textLimit', NyxData(1))]));
    Check(LReply.Field('options').Field('title').AsText = TNyxText('👋'),
      'Menu policy windows preserve supplementary Unicode scalars');
    Check((LReply.Field('items').Count = 1) and
      LReply.Field('items').Item(0).Field('checked').AsBoolean,
      'Exact menu inspection pages ordered native Boolean defaults');
    Check((LAgent.Revision = 2) and (LAgent.ReviewSeed(2).Source = LAfter.Source),
      'Bounded menu inspection leaves the accepted pair/revision unchanged');
    LFailure := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(2)), NyxField('operationId', NyxData('bad-remove')),
        NyxField('operations', NyxArray([NyxRemoveMenu(NyxMenuRef('density')).ToData]))]));
    except
      on LException: Exception do
      begin
        LFailure := True;
      end;
    end;
    Check(LFailure and (LAgent.Revision = 2) and
      (LAgent.ReviewSeed(2).Source = LAfter.Source),
      'Removing a referenced menu refuses the complete pair/history edit');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(2)), NyxField('operationId', NyxData('undo-menu'))]));
    Check((LAgent.ReviewSeed(3).Source = LBefore.Source) and
      (LAgent.ReviewSeed(3).Design = LBefore.Design), 'One Undo restores exact definition and invoker pair');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('direction', NyxData('redo')),
      NyxField('expectedRevision', NyxData(3)), NyxField('operationId', NyxData('redo-menu'))]));
    Check((LAgent.ReviewSeed(4).Source = LAfter.Source) and
      (LAgent.ReviewSeed(4).Design = LAfter.Design), 'One Redo restores exact grouped menu pair');
    LCopy := TNyxCodec.Decode(LAfter.Design);
    LRecipe := NewNyxDeclaredMenuRecipe(LCopy, NyxMenuRef('actions'));
    Check(LRecipe.Items[3].Recipe.Items[1].IsChecked,
      'Saved submenu resolves into an independent runtime recipe with defaults');
    LCopy.Menus.Define(NyxMenuRef('density'), Density);
    Check(LRecipe.Items[3].Recipe.Items[1].IsChecked,
      'Submenu snapshot is independent of later document registry replacement');
    LCopy.Free;
    LCopy := nil;
    LCopy := LRecipe.Items[3].Recipe.CopyDocument;
    Check(LCopy.Menus.Definition(NyxMenuRef('density')).Item(1).IsChecked,
      'Nested owned content remains valid after the original document retires');
  finally
    LCopy.Free;
    LAgent.Free;
    LDocument.Free;
  end;
end;

{ Fast standalone builds retain only reachable menu/content dependencies. A
  reusable promoted to the isolated page must also retarget its menu root. }
procedure ViewJourney;
var
  LDocument: TNyxDocument;
  LView: TNyxDocument;
  LBefore: TNyxText;
begin
  LDocument := Design;
  LView := nil;
  try
    LDocument.Menus.Define(NyxMenuRef('unused-density'), Density);
    LBefore := TNyxCodec.Encode(LDocument);
    LView := CloneNyxViewDocument(LDocument, LDocument.Pages[0]);
    Check((LView.Menus.Count = 2) and (LView.ComponentCount = 2),
      'Standalone page retains reachable menus and both reusable content roots');
    Check(not LView.Menus.Contains(NyxMenuRef('unused-density')),
      'Standalone page excludes unused menu declarations');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'Standalone menu closure leaves its source document exact');
    FreeAndNil(LView);
    LDocument.Find('copy-draft').Configure.Menu(NyxMenuRef('actions')).Done;
    LView := CloneNyxViewDocument(LDocument, LDocument.FindComponent('actions-content'));
    Check((LView.Menus.Definition(NyxMenuRef('actions')).Root.Kind = nrPage) and
      (LView.Menus.Definition(NyxMenuRef('actions')).Root.Name = 'actions-content'),
      'Promoted reusable menu content resolves to the isolated page root');
    ValidateNyxDocumentProperties(LView);
    Check(LView.Menus.Contains(NyxMenuRef('density')) and (LView.ComponentCount = 1),
      'Promoted reusable keeps its nested content dependency exactly once');
  finally
    LView.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Journey;
    ViewJourney;
    SemanticJourney;
    WriteLn('PASS ', GChecks, ' menu declaration checks');
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$ENDIF}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LException.Message);
      {$ELSE}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
end.
