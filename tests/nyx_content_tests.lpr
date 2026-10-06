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
program nyx_content_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Math, nyx.text, nyx.types, nyx.content, nyx.responsive,
  nyx.presentations, nyx.containers, nyx.controls, nyx.model, nyx.composition,
  nyx.codec, nyx.codegen, nyx.source, nyx.data, nyx.state, nyx.studio.projects,
  nyx.studio.agents, nyx.studio.edits, nyx.studio.rootedits, nyx.root.types
  {$ifdef PAS2JS}, Web{$endif};

var
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(LChecks);
end;

function BuildDocument: TNyxDocument;
var
  LPage: INyxPage;
  LWide: INyxColumn;
  LCompact: INyxRow;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Presentation recipes';
    Result.State.SetValue(NyxTextState('notes'), 'A starting idea.');
    Result.Presentations.Define(NyxPresentation('focused'), TNyxPresentationCondition.Manual);
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Done;
    Result.AddPage(LPage);
    LWide := NewNyxColumn('wide-form');
    LWide.Add(NewNyxHeading('wide-title').WithText('Room to create'));
    LWide.Add(NewNyxInput('wide-name').WithText('Project name'));
    Result.AddComponent(LWide);
    Result.Find('wide-name').Binds.Value(NyxTextState('notes')).Done;
    LCompact := NewNyxRow('compact-form');
    LCompact.Add(NewNyxMemo('compact-notes').WithText('Quick notes'));
    Result.AddComponent(LCompact);
    Result.Find('compact-notes').Binds.Value(NyxTextState('notes')).Done;
    LInstance := NewNyxComponent('workspace');
    LPage.Add(LInstance);
    LInstance.Content
      .Use(NyxComponent('wide-form'))
      .WhenViewport(TNyxViewportWidth.Below(640))
      .Use(NyxComponent('compact-form'))
      .WhenPresentation(NyxPresentation('focused'))
      .Use(NyxComponent('compact-form'))
      .Done;
  except
    Result.Free;
    raise;
  end;
end;

procedure FluentJourney;
var
  LControl: INyxComponent;
  LCommon: INyxContent;
  LCompact: INyxContent;
  LNative: INyxContent;
  LClone: INyxContent;
  LBefore: TNyxText;
  LRule: TNyxContentRule;
  LReference: TNyxComponentRef;
  LRejected: Boolean;
  LIndex: Integer;
begin
  LControl := NewNyxComponent('scope-owner');
  LCommon := LControl.Content;
  LCommon.Use(NyxComponent('wide'));
  Check(LControl.Reference.Name = 'wide', 'Specialized reference reads the ordinary content recipe');
  LControl.Reference := NyxComponent('revised-wide');
  Check(LCommon.Rule(0).Component.Name = 'revised-wide', 'Specialized reference updates the base while retaining conditional scopes');
  LRule := LCommon.Rule(0);
  LReference := LRule.Component;
  LReference.Name := 'detached copy';
  Check((LReference.Name = 'detached copy') and (LCommon.Rule(0).Component.Name = 'revised-wide'),
    'Inspected rule/reference values cannot mutate registry storage');
  LCompact := LCommon.WhenViewport(TNyxViewportWidth.Below(640));
  LCompact.Use(NyxComponent('compact'));
  LNative := LCompact.ForPlatform(npfNativeLCL);
  LNative.Use(NyxComponent('native-compact'));
  LCompact.Use(NyxComponent('revised-compact'));
  Check(LCommon.Count = 3, 'Exact scope replacement retains its original position');
  Check(LCommon.Rule(1).Component.Name = 'revised-compact', 'Retained common scope remains independent');
  Check(LCommon.Rule(2).Component.Name = 'native-compact', 'Retained target scope does not redirect another facade');
  LClone := LCommon.Clone;
  LClone.WhenViewport(TNyxViewportWidth.Below(640)).Clear;
  Check((LClone.Count = 2) and (LCommon.Count = 3), 'Clone storage is independent');
  LControl := nil;
  LCommon := nil;
  LNative := nil;
  LCompact.Use(NyxComponent('after-owner'));
  Check(LCompact.Rule(1).Component.Name = 'after-owner', 'Managed scope outlives its released component safely');
  LBefore := LCompact.ToData.ToJSON;
  LRejected := False;
  try
    LCompact.Use(Default(TNyxComponentRef));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCompact.ToData.ToJSON = LBefore), 'Absent recipe refuses before mutation');
  LClone := NewNyxContent;
  for LIndex := 1 to NyxMaximumContentRules do
  begin
    LClone.WhenViewport(TNyxViewportWidth.Below(LIndex)).Use(NyxComponent('recipe'));
  end;
  LBefore := LClone.ToData.ToJSON;
  LRejected := False;
  try
    LClone.WhenViewport(TNyxViewportWidth.Below(NyxMaximumContentRules + 1)).Use(NyxComponent('recipe'));
  except
    on ENyxContent do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LClone.ToData.ToJSON = LBefore), 'Full budget failure preserves the complete registry');
end;

procedure CompositionJourney;
var
  LDocument: TNyxDocument;
  LView: TNyxNode;
  LOther: TNyxNode;
  LBefore: TNyxText;
  LFrame: TNyxViewFrame;
  LRejected: Boolean;
begin
  LDocument := BuildDocument;
  LView := nil;
  LOther := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LView := RealizeNyxView(LDocument, LDocument.Pages[0]);
    Check(LView.Find(NyxQualifiedID('workspace', 'wide-name')) <> nil, 'Default discovery selects the ordinary recipe');
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-notes')) = nil, 'Inactive controls are not realized');
    LView.Free;
    LView := nil;
    LFrame := TNyxViewFrame.At(390, 700, npfBrowser);
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], LFrame);
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-notes')) <> nil, 'Compact recipe can change control type and descendant count');
    Check(LView.Find(NyxQualifiedID('workspace', 'wide-title')) = nil, 'Compact realization does not instantiate hidden wide controls');
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-notes')).DesignID = 'workspace', 'Inherited parts retain exact editable instance identity');
    LOther := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(640, 700, npfNativeLCL));
    Check(LOther.Find(NyxQualifiedID('workspace', 'wide-name')) <> nil, 'The exclusive upper boundary restores the default recipe');
    LView.Find(NyxQualifiedID('workspace', 'compact-notes')).Configure.Text('Runtime notes');
    Check(LDocument.Find('compact-notes').Prop('text') = 'Quick notes', 'Runtime mutation never changes a shared recipe');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'Composition leaves authored persistence unchanged');
    LView.Free;
    LView := nil;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0],
      TNyxViewFrame.At(900, 700, npfBrowser).Selecting(NyxPresentation('focused')));
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-notes')) <> nil, 'Manual selection can choose an alternate control set');
    LDocument.Find('workspace').Content.ForPlatform(npfNativeLCL).Use(NyxComponent('wide-form'));
    LView.Free;
    LView := nil;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0],
      TNyxViewFrame.At(390, 700, npfNativeLCL).Selecting(NyxPresentation('focused')));
    Check(LView.Find(NyxQualifiedID('workspace', 'wide-name')) <> nil, 'Target-specific defaults override common automatic/manual recipes');
    LDocument.Find('workspace').Content.WhenPresentation(NyxPresentation('focused'))
      .ForPlatform(npfNativeLCL).Use(NyxComponent('compact-form'));
    LView.Free;
    LView := nil;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0],
      TNyxViewFrame.At(390, 700, npfNativeLCL).Selecting(NyxPresentation('focused')));
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-notes')) <> nil, 'Target-specific manual choice has final precedence');
    LRejected := False;
    try
      TNyxViewFrame.At(NaN, 600, npfBrowser);
    except
      on EArgumentException do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Nonfinite composition geometry refuses');
  finally
    LOther.Free;
    LView.Free;
    LDocument.Free;
  end;
end;

procedure WireAndSourceJourney;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LViewDocument: TNyxDocument;
  LBefore: TNyxText;
  LSource: TNyxText;
  LName: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;
  LItem: TNyxDataValue;
  LPosition: Integer;
  LReplaySource: TNyxText;
  {$ifndef PAS2JS}
  LExport: TFileStream;
  {$endif}
begin
  LDocument := BuildDocument;
  LDecoded := nil;
  LViewDocument := nil;
  try
    LName := 'Recipe ';
    for LIndex := 1 to 64 do
    begin
      LName := LName + NyxScalarText($1F680);
    end;
    LDocument.Find('compact-form').Named(LName);
    LDocument.Find('workspace').Content.WhenViewport(TNyxViewportWidth.Below(640)).Use(NyxComponent(LName));
    LDocument.Find('workspace').Content.WhenPresentation(NyxPresentation('focused')).Use(NyxComponent(LName));
    LBefore := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 5, 'Typed recipe choices select design version five');
    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore, 'Long supplementary recipe names round trip as values');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('.Content', LSource) > 0) and (Pos('.Use(NyxComponent(', LSource) > 0) and
      (Pos('INyxComponent', LSource) > 0), 'Generated source uses the specialized fluent contract');
    Check(PrepareNyxCompanion(LDocument, LDocument, LSource, False) = LSource, 'Typed source reconstructs the complete exact recipe registry');
    { Native ANSI RTL replacement would transcode the supplementary names in
      this complete source. Copy exact typed spans at the ASCII call boundary. }
    LPosition := Pos(TNyxText('.Use(NyxComponent(''wide-form''))'), LSource) +
      Length(TNyxText('.Use(NyxComponent(''wide-form''))'));
    LReplaySource := Copy(LSource, 1, LPosition - 1) +
      TNyxText(#10 + '      .Clear' + #10 + '      .Use(NyxComponent(''wide-form''))') +
      Copy(LSource, LPosition, MaxInt);
    Check(PrepareNyxCompanion(LDocument, LDocument, LReplaySource, False) <> '',
      'Handcrafted fluent clearing replays through the same managed contract');
    LViewDocument := CloneNyxViewDocument(LDocument, LDocument.Find('workspace'));
    Check(LViewDocument.ComponentCount = 2, 'Isolated builds retain every inactive transitive recipe');
    Check(PrepareNyxCompanion(LDocument, LViewDocument, LSource, True) <> '', 'Isolated companion reconciliation retains alternate recipes');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LExport := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LExport.WriteBuffer(LSource[1], Length(LSource));
      finally
        LExport.Free;
      end;
    end;
    {$endif}
    LItem := LDocument.Find('workspace').Content.Rule(0).ToData;
    LRejected := False;
    try
      NyxContentFromData(NyxObject([NyxField('version', NyxData(1)),
        NyxField('rules', NyxArray([LItem, LItem]))]));
    except
      on ENyxContent do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Duplicate wire scopes refuse the complete candidate');
    LDecoded.Free;
    LDecoded := TNyxCodec.Decode('{"version":1,"title":"Legacy data","pages":[{"kind":"page","id":"legacy","props":{},"children":[],"contentRules":{"vendor":"opaque"}}],"components":[]}');
    Check(LDecoded.Pages[0].Extensions.Has(NyxExtension(NyxContentRulesWireField)) and
      not LDecoded.HasContentRules, 'Older opaque content fields retain extension meaning');
    LDocument.Pages[0].Extensions.SetValue(NyxExtension(NyxContentRulesWireField), NyxNull);
    LRejected := False;
    try
      TNyxCodec.Encode(LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Version promotion refuses typed/opaque field collisions');
  finally
    LViewDocument.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

procedure InactiveAdmissionJourney;
var
  LDocument: TNyxDocument;
  LLink: INyxComponent;
  LRejected: Boolean;
begin
  LDocument := BuildDocument;
  try
    LDocument.Find('workspace').Content.WhenPresentation(NyxPresentation('focused')).Use(NyxComponent('missing'));
    LRejected := False;
    try
      LDocument.Validate;
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Inactive missing recipe is rejected before mounting');
    LDocument.Find('workspace').Content.WhenPresentation(NyxPresentation('focused')).Use(NyxComponent('compact-form'));
    LLink := NewNyxComponent('recursive-part');
    LLink.Content.Use(NyxComponent('compact-form'));
    LDocument.Find('compact-form').Add(LLink);
    LRejected := False;
    try
      LDocument.Validate;
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Inactive recipe recursion is rejected by the complete dependency graph');
  finally
    LDocument.Free;
  end;
end;

procedure SemanticJourney;
var
  LDocument: TNyxDocument;
  LAgent: TNyxAgentSession;
  LContent: INyxContent;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LReply: TNyxDataValue;
  LRevision: Integer;
  LRejected: Boolean;
  LRootReview: INyxRootRemoval;
  LSource: TNyxText;
  LPosition: Integer;
begin
  LDocument := BuildDocument;
  LAgent := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument);
    LPosition := Pos(TNyxText('.Content'), LSource) + Length(TNyxText('.Content'));
    LSource := Copy(LSource, 1, LPosition - 1) +
      TNyxText(#10 + '      { Keep the working space comfortable. }') +
      Copy(LSource, LPosition, MaxInt);
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LAgent := TNyxAgentSession.Create(LBefore);
    LRootReview := ReviewNyxRootRemoval(LBefore, [NyxReusableRoot('compact-form')]);
    Check(not LRootReview.Inspect.Field('ready').AsBoolean and
      (LRootReview.Inspect.Field('retainedReferences').AsInteger = 1),
      'Root cleanup reviews inactive recipe dependencies once per retained instance');
    LContent := LDocument.Find('workspace').Content.Clone;
    LContent.WhenViewport(TNyxViewportWidth.Below(640)).Use(NyxComponent('wide-form'));
    LRevision := LAgent.Revision;
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('content-group')),
      NyxField('operations', NyxArray([
        NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData('My working space'))]),
        NyxSetContent(NyxControl('workspace'), LContent).ToData]))]));
    Check(LAgent.Revision = LRevision + 1, 'Related semantic changes commit at one revision');
    LReply := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('workspace')), NyxField('limit', NyxData(1)),
      NyxField('content', NyxData(True)), NyxField('contentOffset', NyxData(1)),
      NyxField('contentLimit', NyxData(1))]));
    Check((LReply.Field('content').Field('totalRules').AsInteger = 3) and
      (LReply.Field('content').Field('rules').Count = 1), 'Semantic recipe inspection is paged independently');
    Check(LReply.Field('content').Field('rules').Item(0).Field('component').AsText = 'wide-form',
      'Bounded inspection returns only the requested recipe scope');
    LAfter := LAgent.PreviewPair(LAgent.Revision, 'home');
    Check(Pos('{ Keep the working space comfortable. }', LAfter.Source) > 0,
      'Semantic source synchronization preserves authored recipe comments');
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('content-stale')),
        NyxField('operations', NyxArray([NyxSetContent(NyxControl('workspace'), LContent).ToData]))]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Stale revision cannot overwrite recipe choices');
    LContent.WhenPresentation(NyxPresentation('focused')).Use(NyxComponent('missing'));
    LRevision := LAgent.Revision;
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('content-invalid')),
        NyxField('operations', NyxArray([
          NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData('Must stay private'))]),
          NyxSetContent(NyxControl('workspace'), LContent).ToData]))]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = LRevision) and
      (EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LAfter)),
      'One invalid inactive branch refuses every grouped mutation');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('content-undo')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LBefore),
      'One semantic Undo restores exact recipe choices and authored Pascal');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('content-redo')), NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = EncodeNyxProject(LAfter),
      'One semantic Redo restores the complete accepted pair');
  finally
    LAgent.Free;
    LDocument.Free;
  end;
end;

procedure NestedContainerJourney;
var
  LDocument: TNyxDocument;
  LView: TNyxNode;
  LShell: INyxColumn;
  LSlot: INyxColumn;
  LNested: INyxComponent;
  LOuter: INyxComponent;
  LAppended: INyxComponent;
  LCondition: TNyxPresentationRef;
  LMeasurements: TNyxContainerMeasurements;
  LNestedScope: TNyxText;
  LAppendedScope: TNyxText;
begin
  LDocument := BuildDocument;
  LView := nil;
  try
    LCondition := NyxPresentation('small recipe space');
    LDocument.Presentations.Define(LCondition, TNyxPresentationCondition.Within(
      NyxContainer('recipe space'), TNyxViewportCondition.Any.WidthBelow(300)));
    LShell := NewNyxColumn('shell-recipe');
    LSlot := NewNyxColumn('shell-body');
    LSlot.Configure.PartName(NyxPart('body')).Done;
    LShell.Add(LSlot);
    LNested := NewNyxComponent('nested-workspace');
    LNested.Content.Use(NyxComponent('wide-form')).WhenPresentation(LCondition)
      .Use(NyxComponent('compact-form')).Done;
    LSlot.Add(LNested);
    LDocument.AddComponent(LShell);
    LOuter := NewNyxComponent('shell-view');
    LOuter.Content.Use(NyxComponent('shell-recipe'));
    LOuter.Configure.QueryContainer(NyxContainer('recipe space')).Containment(nccSize).Done;
    LDocument.Pages[0].Add(LOuter);
    LAppended := NewNyxComponent('appended-workspace');
    LAppended.Content.Use(NyxComponent('wide-form')).WhenPresentation(LCondition)
      .Use(NyxComponent('compact-form')).Done;
    LOuter.OverridePart(NyxPart('body'), noAppend).Add(LAppended);
    SetLength(LMeasurements, 1);
    LMeasurements[0].RuntimeID := NyxQualifiedID('shell-view', 'shell-recipe');
    LMeasurements[0].Width := 200;
    LMeasurements[0].Height := 500;
    LNestedScope := NyxQualifiedID('shell-view', 'nested-workspace');
    LAppendedScope := NyxQualifiedID('shell-view', 'appended-workspace');
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(900, 700, npfBrowser),
      NewNyxContainerSnapshot(LMeasurements));
    Check(LView.Find(NyxQualifiedID(LNestedScope, 'compact-notes')) <> nil,
      'Instance publisher override is visible before nested recipe selection');
    Check(LView.Find(NyxQualifiedID(LAppendedScope, 'compact-notes')) <> nil,
      'Appended override payload uses its exact inherited root/slot ancestry');
    LSlot.Configure.QueryContainer(NyxContainer('recipe space')).Containment(nccSize).Done;
    LView.Free;
    LView := nil;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(900, 700, npfNativeLCL),
      NewNyxContainerSnapshot(LMeasurements));
    Check(LView.Find(NyxQualifiedID(LNestedScope, 'wide-name')) <> nil,
      'Missing nearer qualified publisher never falls through to an outer box');
    Check(LView.Find(NyxQualifiedID(LAppendedScope, 'wide-name')) <> nil,
      'Override payload preserves the same nearest-publisher refusal');
  finally
    LView.Free;
    LDocument.Free;
  end;
end;

procedure ContainerJourney;
var
  LDocument: TNyxDocument;
  LView: TNyxNode;
  LMeasurements: TNyxContainerMeasurements;
  LCondition: TNyxPresentationRef;
begin
  LDocument := BuildDocument;
  LView := nil;
  try
    LCondition := NyxPresentation('small workspace');
    LDocument.Presentations.Define(LCondition, TNyxPresentationCondition.Within(
      NyxContainer('workspace space'), TNyxViewportCondition.Any.WidthBelow(300)));
    LDocument.Find('workspace').Content.WhenViewport(TNyxViewportWidth.Below(640)).Clear;
    LDocument.Find('workspace').Content.WhenPresentation(LCondition).Use(NyxComponent('compact-form'));
    LDocument.Find('workspace').Configure.QueryContainer(NyxContainer('workspace space')).Done;
    SetLength(LMeasurements, 2);
    LMeasurements[0].RuntimeID := 'home';
    LMeasurements[0].Width := 200;
    LMeasurements[0].Height := 500;
    LMeasurements[1].RuntimeID := NyxQualifiedID('workspace', 'wide-form');
    LMeasurements[1].Width := 100;
    LMeasurements[1].Height := 500;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(900, 700, npfBrowser),
      NewNyxContainerSnapshot(LMeasurements));
    Check(LView.Find(NyxQualifiedID('workspace', 'wide-name')) <> nil, 'An instance does not select itself as a query container');
    LDocument.Pages[0].Configure.QueryContainer(NyxContainer('workspace space')).Done;
    LView.Free;
    LView := nil;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(900, 700, npfBrowser),
      NewNyxContainerSnapshot(LMeasurements));
    Check(LView.Find(NyxQualifiedID('workspace', 'compact-notes')) <> nil, 'Measured ancestor can select different descendants at an unchanged host size');
    LView.Free;
    LView := nil;
    LView := RealizeNyxView(LDocument, LDocument.Pages[0], TNyxViewFrame.At(900, 700, npfNativeLCL));
    Check(LView.Find(NyxQualifiedID('workspace', 'wide-name')) <> nil, 'Absent measurement never estimates a container from host dimensions');
  finally
    LView.Free;
    LDocument.Free;
  end;
end;

begin
  try
    FluentJourney;
    CompositionJourney;
    WireAndSourceJourney;
    InactiveAdmissionJourney;
    SemanticJourney;
    ContainerJourney;
    NestedContainerJourney;
    WriteLn('PASS ', LChecks, ' content recipe checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(LChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LException.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
