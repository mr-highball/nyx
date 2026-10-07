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

program nyx_menu_bar_declarations_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$IFDEF PAS2JS}Web,{$ELSE}Classes,{$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.root.types, nyx.data, nyx.model,
  nyx.responsive, nyx.presentations,
  nyx.controls, nyx.menu.types, nyx.menu.declarations, nyx.menu.bar.declarations,
  nyx.typeahead, nyx.codec, nyx.codegen, nyx.source, nyx.composition, nyx.schema,
  nyx.studio.agents, nyx.studio.edits, nyx.studio.projects, nyx.generated.view;

const
  CUnicode: TNyxText = 'Workspace 🧭 é / 👩‍💻';
var
  GChecks: Integer;

type
  TWireFault = (wfBooleanText, wfHeadingBooleanText, wfFractionalWindow,
    wfZeroWindow, wfUnknownSearch, wfExtraField, wfMissingHeadingField,
    wfEmptyHeadings, wfDuplicateHeading, wfEmptyPart, wfUnknownVersion,
    wfOversized);
  { An extension may retain mutable authoring state behind the interface.
    Attachment must normalize public getters, never retain this implementation
    or rely on its optional serialization method. }
  TForeignBar = class(TInterfacedObject, INyxMenuBarDefinition)
  public
    Plan: INyxMenuBarDefinition;
    function GetOptions: TNyxMenuBarOptions;
    function GetCount: Integer;
    function Item(AIndex: Integer): INyxMenuBarHeading;
    function Heading(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
      AEnabled: Boolean = True): INyxMenuBarDefinition;
    function ToData: TNyxDataValue;
  end;

function TForeignBar.GetOptions: TNyxMenuBarOptions;
begin
  Result := Plan.Options;
end;

function TForeignBar.GetCount: Integer;
begin
  Result := Plan.Count;
end;

function TForeignBar.Item(AIndex: Integer): INyxMenuBarHeading;
begin
  Result := Plan.Item(AIndex);
end;

function TForeignBar.Heading(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
  AEnabled: Boolean): INyxMenuBarDefinition;
begin
  Result := Plan.Heading(APart, AMenu, AEnabled);
end;

function TForeignBar.ToData: TNyxDataValue;
begin
  Result := NyxNull;
  raise Exception.Create('Admission must normalize getters, not foreign serialization');
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Bar(const ALabel: TNyxText = 'Workspace commands'): INyxMenuBarDefinition;
begin
  Result := NewNyxMenuBarDefinition(NyxMenuBar(ALabel).Wrap(False).HoverSwitch(False)
    .TypeAhead(NyxTypeAhead.WindowMilliseconds(1730).Match(ntmExact)))
    .Heading(NyxPart('file'), NyxMenuRef('actions'))
    .Heading(NyxPart('edit'), NyxMenuRef('actions'))
    .Heading(NyxPart('hidden'), NyxMenuRef('actions'), False)
    .Heading(NyxPart('view'), NyxMenuRef('actions'));
end;

{ Preserve the exact MCP-authored English tree. Only typed semantic declarations
  are added to that owned candidate, through the same admission/history service. }
function MenuEdits: TNyxDataValue;
begin
  Result := NyxArray([
    NyxDefineMenu(NyxMenuRef('density'),
      NewNyxMenuDefinition(NyxPageRoot('density-options'), NyxMenu('Density'))
        .Radio(NyxPart('comfortable'), NyxMenuCommand('comfortable'),
          NyxMenuGroup('spacing'), True)
        .Radio(NyxPart('compact'), NyxMenuCommand('compact'),
          NyxMenuGroup('spacing'), False)).ToData,
    NyxDefineMenu(NyxMenuRef('appearance'),
      NewNyxMenuDefinition(NyxPageRoot('appearance-options'), NyxMenu('Appearance'))
        .Check(NyxPart('guides'), NyxMenuCommand('show-grid'), False)
        .Submenu(NyxPart('density'), NyxMenuRef('density'))).ToData,
    NyxDefineMenu(NyxMenuRef('actions'),
      NewNyxMenuDefinition(NyxPageRoot('thoughtful-actions'), NyxMenu('Actions'))
        .Action(NyxPart('cut'), NyxMenuCommand('cut'))
        .Action(NyxPart('copy'), NyxMenuCommand('copy'))
        .Action(NyxPart('paste'), NyxMenuCommand('paste'), False)
        .Separator(NyxPart('separator'))
        .Check(NyxPart('guides'), NyxMenuCommand('show-guides'), False)
        .Radio(NyxPart('comfortable'), NyxMenuCommand('comfortable'),
          NyxMenuGroup('density'), True)
        .Radio(NyxPart('compact'), NyxMenuCommand('compact'),
          NyxMenuGroup('density'), False)
        .Action(NyxPart('hidden'), NyxMenuCommand('archive'))
        .Submenu(NyxPart('appearance'), NyxMenuRef('appearance'))).ToData,
    NyxConfigureMenuBar(NyxControl('workspace-menu-bar'), Bar).ToData]);
end;

procedure SaveText(const APath, AText: TNyxText);
{$IFNDEF PAS2JS}
var
  LFile: TFileStream;
{$ENDIF}
begin
  {$IFNDEF PAS2JS}
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
  {$ENDIF}
end;

{ Fault injection is confined to the structured interchange boundary. Building
  detached object members avoids whitespace-dependent JSON substitutions. }
function WithField(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
begin
  LFound := False;
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to High(LFields) do
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

procedure WireBoundaries;
var
  LOriginal: INyxMenuBarDefinition;
  LCandidate: INyxMenuBarDefinition;
  LData: TNyxDataValue;
  LBad: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LFault: TWireFault;
  LIndex: Integer;
  LFailed: Boolean;
  LNode: TNyxNode;
  LForeignObject: TForeignBar;
  LForeign: INyxMenuBarDefinition;
begin
  LOriginal := Bar;
  LData := LOriginal.ToData;
  for LFault := Low(TWireFault) to High(TWireFault) do
  begin
    LBad := LData;
    case LFault of
      wfBooleanText:
        LBad := WithField(LData, 'options', WithField(LData.Field('options'),
          'wrap', NyxData('false')));
      wfHeadingBooleanText:
        LBad := WithField(LData, 'headings', NyxArray([
          WithField(LData.Field('headings').Item(0), 'enabled', NyxData('true'))]));
      wfFractionalWindow:
        LBad := WithField(LData, 'options', WithField(LData.Field('options'),
          'searchWindowMS', TNyxDataValue.ParseJSON('1730.5')));
      wfZeroWindow:
        LBad := WithField(LData, 'options', WithField(LData.Field('options'),
          'searchWindowMS', NyxData(0)));
      wfUnknownSearch:
        LBad := WithField(LData, 'options', WithField(LData.Field('options'),
          'searchMatch', NyxData('prefix')));
      wfExtraField:
        LBad := WithField(LData, 'unexpected', NyxData(True));
      wfMissingHeadingField:
        LBad := WithField(LData, 'headings', NyxArray([NyxObject([
          NyxField('part', NyxData('file')), NyxField('menu', NyxData('actions')),
          NyxField('unexpected', NyxData(True))])]));
      wfEmptyHeadings:
        LBad := WithField(LData, 'headings', NyxArray([]));
      wfDuplicateHeading:
        LBad := WithField(LData, 'headings', NyxArray([
          LData.Field('headings').Item(0), LData.Field('headings').Item(0)]));
      wfEmptyPart:
        LBad := WithField(LData, 'headings', NyxArray([
          WithField(LData.Field('headings').Item(0), 'part', NyxData(''))]));
      wfUnknownVersion:
        LBad := WithField(LData, 'version', NyxData(2));
      wfOversized:
        begin
          SetLength(LItems, NyxMaximumMenuBarHeadings + 1);
          for LIndex := 0 to High(LItems) do
          begin
            LItems[LIndex] := LData.Field('headings').Item(0);
          end;
          LBad := WithField(LData, 'headings', NyxArray(LItems));
        end;
    end;
    LFailed := False;
    LCandidate := nil;
    try
      LCandidate := NyxMenuBarDefinitionFromData(LBad);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    { Native interface result slots can receive an unpublished partial builder
      before a getter raises. Release that slot; accepted plans stay immutable. }
    LCandidate := nil;
    Check(LFailed and (LOriginal.ToData.ToJSON = LData.ToJSON),
      'Malformed typed menu bar refuses without modifying its accepted plan');
  end;
  LNode := TNyxNode.Create(nkRow, 'admission-boundary');
  try
    LForeignObject := TForeignBar.Create;
    LForeign := LForeignObject;
    LForeignObject.Plan := LOriginal;
    LNode.Configure.MenuBar(LForeign).Done;
    LForeignObject.Plan := NewNyxMenuBarDefinition(NyxMenuBar('Changed'))
      .Heading(NyxPart('different'), NyxMenuRef('different'));
    Check(LNode.MenuBar.ToData.ToJSON = LData.ToJSON,
      'Attachment normalizes a foreign implementation and owns independent defaults');
    for LIndex := 0 to 2 do
    begin
      LFailed := False;
      try
        case LIndex of
          0:
            LNode.Configure.ForPlatform(npfBrowser).MenuBar(LOriginal);
          1:
            LNode.Configure.WhenViewport(TNyxViewportCondition.Any.WidthBelow(640))
              .NoMenuBar;
          2:
            LNode.Configure.WhenPresentation(NyxPresentation('compact')).InheritMenuBar;
        end;
      except
        on E: Exception do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed and (LNode.MenuBar.ToData.ToJSON = LData.ToJSON),
        'Structural grouping refuses a conditional scope without replacing defaults');
    end;
  finally
    LNode.Free;
  end;
end;

{ Promote an unchanged older opaque field only by explicit admission. It must
  never be mistaken for a typed bar or overwritten during wire-version changes. }
procedure LegacyBoundary(ABase: TNyxDocument);
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LWire: TNyxText;
  LFailed: Boolean;
begin
  LDocument := ABase.Clone;
  LCopy := nil;
  try
    LDocument.Find('workspace-menu-bar').Extensions.SetValue(
      NyxExtension(NyxMenuBarWireField), NyxObject([
        NyxField('legacy', NyxData(CUnicode))]));
    LWire := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger < 7,
      'Opaque legacy data alone does not promote the design');
    LCopy := TNyxCodec.Decode(LWire);
    Check(not LCopy.Find('workspace-menu-bar').HasMenuBar and
      (TNyxCodec.Encode(LCopy) = LWire), 'Earlier opaque menuBar remains an exact extension');
    LCopy.Find('workspace-menu-bar').Configure.NoMenuBar.Done;
    LFailed := False;
    try
      TNyxCodec.Encode(LCopy);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LWire),
      'Typed promotion refuses an opaque collision and preserves the original');
  finally
    LCopy.Free;
    LDocument.Free;
  end;
end;

{ Transfer the exact authored bar into a reusable definition, then exercise
  inherited/default, explicit mask and independent instance replacement. }
procedure ReusableBoundary(ASaved: TNyxDocument);
var
  LDocument: TNyxDocument;
  LStandalone: TNyxDocument;
  LReconstructed: TNyxDocument;
  LRuntime: TNyxNode;
  LRow: TNyxNode;
  LParent: TNyxNode;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LIndex: Integer;
  LWorkspace: TNyxSourceWorkspace;
  LWire: TNyxText;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
begin
  LDocument := ASaved.Clone;
  LAgent := nil;
  LStandalone := nil;
  LReconstructed := nil;
  LRuntime := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LRow := LDocument.Find('workspace-menu-bar');
    LParent := LRow.Parent;
    for LIndex := 0 to LParent.Count - 1 do
    begin

      if LParent.Children[LIndex] = LRow then
      begin
        LDocument.AddComponent(LParent.Extract(LIndex));
        Break;
      end;
    end;
    LFirst := NewNyxComponent('first-menu-bar');
    LFirst.Configure.Component(NyxComponent('workspace-menu-bar')).Done;
    LParent.Add(LFirst.Node);
    LSecond := NewNyxComponent('second-menu-bar');
    LSecond.Configure.Component(NyxComponent('workspace-menu-bar')).NoMenuBar.Done;
    LParent.Add(LSecond.Node);
    ValidateNyxDocumentProperties(LDocument);
    LRuntime := RealizeNyxView(LDocument, LDocument.Find('menu-workspace'));
    Check(LRuntime.Find(NyxQualifiedID('first-menu-bar', 'workspace-menu-bar')).MenuBar.Count = 4,
      'A reusable row instance inherits the complete typed bar');
    Check(LRuntime.Find(NyxQualifiedID('second-menu-bar', 'workspace-menu-bar')).HasMenuBar and
      (LRuntime.Find(NyxQualifiedID('second-menu-bar', 'workspace-menu-bar')).MenuBar = nil),
      'An explicit instance mask blocks inherited grouping');
    FreeAndNil(LRuntime);
    LWire := TNyxCodec.Encode(LDocument);
    LReconstructed := LWorkspace.Candidate(LDocument,
      TNyxCodegen.Generate(LDocument, 'nyx.generated.view'));
    Check(TNyxCodec.Encode(LReconstructed) = LWire,
      'Generated reusable grouping and explicit masks reconstruct exactly');
    LAgent := TNyxAgentSession.Create(NyxProjectPair(LWire,
      TNyxCodegen.Generate(LDocument, 'nyx.generated.view')));
    LAgent.InheritPermission(apEdit);
    LReply := LAgent.Call('nyx_menus', 'Scooty', NyxObject([
      NyxField('row', NyxData('first-menu-bar')), NyxField('itemLimit', NyxData(1))]));
    Check(not LReply.Field('localDeclared').AsBoolean and
      LReply.Field('configured').AsBoolean and (LReply.Field('headings').Count = 1),
      'Bounded effective query exposes an inherited instance group');
    LReply := LAgent.Call('nyx_menus', 'Scooty', NyxObject([
      NyxField('row', NyxData('first-menu-bar')), NyxField('barScope', NyxData('local'))]));
    Check(not LReply.Field('configured').AsBoolean,
      'Explicit local query distinguishes absent instance grouping from inheritance');
    LReply := LAgent.Call('nyx_menus', 'Scooty', NyxObject([
      NyxField('row', NyxData('second-menu-bar'))]));
    Check(LReply.Field('localDeclared').AsBoolean and not LReply.Field('configured').AsBoolean,
      'Effective query distinguishes the explicit reusable mask');
    Check((LAgent.Revision = 1) and (LAgent.ReviewSeed(1).Design = LWire),
      'Effective realization queries preserve the exact accepted document and revision');
    LStandalone := CloneNyxViewDocument(LDocument, LDocument.Find('menu-workspace'));
    Check((LStandalone.Count = 4) and (LStandalone.ComponentCount = 1) and
      (LStandalone.Menus.Count = 3), 'Standalone reuse retains its bar and transitive dropdown dependencies');
    LSecond.Configure.InheritMenuBar.Done;
    LFirst.Configure.MenuBar(Bar('First workspace')).Done;
    LRuntime := RealizeNyxView(LDocument, LDocument.Find('menu-workspace'));
    Check((LRuntime.Find(NyxQualifiedID('first-menu-bar', 'workspace-menu-bar')).MenuBar.Options.Caption = 'First workspace') and
      (LRuntime.Find(NyxQualifiedID('second-menu-bar', 'workspace-menu-bar')).MenuBar.Options.Caption = 'Workspace commands') and
      (LDocument.FindComponent('workspace-menu-bar').MenuBar.Options.Caption = 'Workspace commands'),
      'Instance replacement and inheritance leave siblings and the definition independent');
  finally
    LAgent.Free;
    LWorkspace.Free;
    LRuntime.Free;
    LReconstructed.Free;
    LStandalone.Free;
    LDocument.Free;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LView: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LAgent: TNyxAgentSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSource: TNyxText;
  LWire: TNyxText;
  LReply: TNyxDataValue;
  LPlan: INyxMenuBarDefinition;
  LExpanded: TNyxNode;
  LFailure: Boolean;
  LCase: Integer;
begin
  LDocument := nil;
  LCopy := nil;
  LView := nil;
  LExpanded := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  LAgent := nil;
  try
    WireBoundaries;
    LDocument := BuildNyxDocument;
    LegacyBoundary(LDocument);
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument, 'nyx.generated.view'));
    LAgent := TNyxAgentSession.Create(LBefore);
    LAgent.InheritPermission(apEdit);
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(1)),
      NyxField('operationId', NyxData('saved-workspace-bar')),
      NyxField('operations', MenuEdits)]));
    LAfter := LAgent.ReviewSeed(2);
    Check(LAgent.Revision = 2, 'Complete grouped bar/menu admission advances one revision');
    Check(Pos('NewNyxMenuBarDefinition(', LAfter.Source) > 0,
      'The accepted source contains a crafted typed row grouping');
    LCopy := TNyxCodec.Decode(LAfter.Design);
    ReusableBoundary(LCopy);
    Check(LCopy.HasMenuBars and LCopy.HasMenuDeclarations, 'Bar selects the typed persistence boundary');
    Check(Pos('"version":7', StringReplace(LAfter.Design, ' ', '', [rfReplaceAll])) > 0,
      'Only bar documents promote to version seven');
    LPlan := LCopy.Find('workspace-menu-bar').MenuBar;
    Check((LPlan.Count = 4) and not LPlan.Options.Wraps and not LPlan.Options.Hovers and
      (LPlan.Options.Search.WindowMS = 1730) and (LPlan.Options.Search.MatchMode = ntmExact),
      'Ordered headings and complete nondefault policy reconstruct');
    Check(not LPlan.Item(2).IsEnabled, 'Logical heading enablement is a saved Boolean');
    LWire := LPlan.ToData.ToJSON;
    LPlan := LPlan.Heading(NyxPart('new-part'), NyxMenuRef('actions'));
    Check(LCopy.Find('workspace-menu-bar').MenuBar.ToData.ToJSON = LWire,
      'Fluent builder changes never mutate an attached plan');
    LSource := TNyxCodegen.Generate(LCopy, 'nyx.generated.view');
    Check(LSource = LAfter.Source, 'No-op persistence preserves deterministic Pascal');
    LView := LWorkspace.Candidate(LCopy, LSource);
    Check(TNyxCodec.Encode(LView) = LAfter.Design, 'Source admission reconstructs exact saved groups');
    FreeAndNil(LView);
    LView := CloneNyxViewDocument(LCopy, LCopy.Find('menu-workspace'));
    Check((LView.Count = 4) and (LView.Menus.Count = 3),
      'Standalone page retains transitive dropdown content and definitions');
    Check(LView.Find('workspace-menu-bar').MenuBar.Count = 4,
      'Standalone clone retains the exact coordinated row');
    FreeAndNil(LView);

    LReply := LAgent.Call('nyx_menus', 'Scooty', NyxObject([
      NyxField('row', NyxData('workspace-menu-bar')),
      NyxField('itemOffset', NyxData(1)), NyxField('itemLimit', NyxData(1))]));
    Check((LReply.Field('headings').Count = 1) and LReply.Field('hasMore').AsBoolean and
      (LReply.Field('headings').Item(0).Field('part').AsText = 'edit'),
      'Semantic row query pages only the requested heading');
    Check(LAgent.Revision = 2, 'Queries do not change revision or history');
    LReply := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('workspace-menu-bar'))]));
    Check(LReply.Field('menuBar').Field('configured').AsBoolean,
      'Bounded node context reports local grouping');

    { Each refusal must preserve both accepted files and the revision. These are
      semantic candidate failures, not mutations of the live accepted document. }
    for LCase := 0 to 5 do
    begin
      LFailure := False;
      try
        case LCase of
          0:
            LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
              NyxField('expectedRevision', NyxData(1)),
              NyxField('operationId', NyxData('stale-bar')),
              NyxField('operations', MenuEdits)]));
          1:
            LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
              NyxField('expectedRevision', NyxData(2)),
              NyxField('operationId', NyxData('missing-menu')),
              NyxField('operations', NyxArray([
                NyxConfigureMenuBar(NyxControl('workspace-menu-bar'),
                  NewNyxMenuBarDefinition(NyxMenuBar('Bad'))
                    .Heading(NyxPart('file'), NyxMenuRef('absent'))).ToData]))]));
          2:
            LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
              NyxField('expectedRevision', NyxData(2)),
              NyxField('operationId', NyxData('missing-heading')),
              NyxField('operations', NyxArray([
                NyxConfigureMenuBar(NyxControl('workspace-menu-bar'),
                  NewNyxMenuBarDefinition(NyxMenuBar('Bad'))
                    .Heading(NyxPart('absent'), NyxMenuRef('actions'))).ToData]))]));
          3:
            LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
              NyxField('expectedRevision', NyxData(2)),
              NyxField('operationId', NyxData('wrong-row')),
              NyxField('operations', NyxArray([
                NyxConfigureMenuBar(NyxControl('bar-title'), Bar).ToData]))]));
          4:
            LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
              NyxField('expectedRevision', NyxData(2)),
              NyxField('operationId', NyxData('dependent-menu')),
              NyxField('operations', NyxArray([NyxRemoveMenu(NyxMenuRef('actions')).ToData]))]));
          5:
            LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
              NyxField('expectedRevision', NyxData(2)),
              NyxField('operationId', NyxData('double-invoker')),
              NyxField('operations', NyxArray([
                NyxAttachMenu(NyxControl('bar-file'), NyxMenuRef('actions')).ToData]))]));
        end;
      except
        on E: Exception do
        begin
          LFailure := True;
        end;
      end;
      Check(LFailure and (LAgent.Revision = 2) and
        (LAgent.ReviewSeed(2).Design = LAfter.Design) and
        (LAgent.ReviewSeed(2).Source = LAfter.Source), 'Refused group preserves the complete pair');
    end;
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(2)), NyxField('direction', NyxData('undo')),
      NyxField('operationId', NyxData('undo-workspace-bar'))]));
    Check((LAgent.ReviewSeed(3).Design = LBefore.Design) and
      (LAgent.ReviewSeed(3).Source = LBefore.Source), 'One Undo restores the exact initial pair');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(3)), NyxField('direction', NyxData('redo')),
      NyxField('operationId', NyxData('redo-workspace-bar'))]));
    Check((LAgent.ReviewSeed(4).Design = LAfter.Design) and
      (LAgent.ReviewSeed(4).Source = LAfter.Source), 'One Redo restores the complete saved grouping');

    LCopy.Find('workspace-menu-bar').Configure.MenuBar(Bar(CUnicode)).Done;
    LWire := TNyxCodec.Encode(LCopy);
    LView := TNyxCodec.Decode(LWire);
    Check(LView.Find('workspace-menu-bar').MenuBar.Options.Caption = CUnicode,
      'Supplementary Unicode and combining label remain exact through wire');
    FreeAndNil(LView);
    LSource := TNyxCodegen.Generate(LCopy, 'nyx.generated.view');
    LView := LWorkspace.Candidate(LCopy, LSource);
    Check(TNyxCodec.Encode(LView) = LWire, 'Unicode label remains exact through source reconstruction');
    FreeAndNil(LView);
    LCopy.Find('workspace-menu-bar').Configure.NoMenuBar.Done;
    Check(LCopy.Find('workspace-menu-bar').HasMenuBar and
      (LCopy.Find('workspace-menu-bar').MenuBar = nil), 'Explicit mask retains local meaning');
    LCopy.Find('workspace-menu-bar').Configure.InheritMenuBar.Done;
    Check(not LCopy.Find('workspace-menu-bar').HasMenuBar, 'Inheritance removes only the local declaration');

    {$IFNDEF PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      SaveText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LAfter.Source);
      SaveText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LAfter.Design);
    end;
    {$ENDIF}
  finally
    LAgent.Free;
    LWorkspace.Free;
    LExpanded.Free;
    LView.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$ELSE}
    WriteLn('PASS ', GChecks, ' saved menu bar declaration/history checks');
    {$ENDIF}
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$ELSE}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
end.
