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
program nyx_presentations_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.responsive, nyx.presentations, nyx.model,
  nyx.controls, nyx.codec, nyx.codegen, nyx.schema, nyx.composition, nyx.platform,
  nyx.projection.refresh, nyx.data, nyx.source, nyx.studio.edits, nyx.studio.agents,
  nyx.studio.projects, nyx.studio.session, nyx.studio.inspector, nyx.studio.view
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The exported English companion also feeds real browser/LCL consumers. Unicode
  stress data belongs to independent qualification owners, not the starter UI. }
function Fixture(AManual: Boolean = False): TNyxDocument;
var
  LRow: INyxRow;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Room for ideas';
    Result.Presentations.Define(NyxPresentation('compact'),
      TNyxViewportCondition.Any.WidthBelow(640));
    Result.Presentations.Define(NyxPresentation('short landscape'),
      TNyxViewportCondition.Any.HeightBelow(300).Orientation(nvoLandscape));
    Result.AddPage(NewNyxPage('home').Configure.Padding(16).Gap(12).Done);
    LRow := NewNyxRow('workspace');
    Result.Pages[0].Add(LRow);
    LRow.Configure.Gap(16).Padding(0).Wrap(nfwNoWrap);
    LRow.Configure.WhenPresentation(NyxPresentation('compact')).Layout(nlColumn).Gap(8)
      .ForPlatform(npfNativeLCL).Gap(10);
    LRow.Configure.WhenPresentation(NyxPresentation('short landscape')).Layout(nlColumn).Gap(6);
    if AManual then
    begin
      Result.Presentations.Define(NyxPresentation('focused'), TNyxPresentationCondition.Manual);
      Result.Presentations.Define(NyxPresentation('wide workspace'), TNyxPresentationCondition.Manual);
      LRow.Configure.WhenPresentation(NyxPresentation('focused')).Layout(nlColumn).Gap(4)
        .ForPlatform(npfNativeLCL).Gap(5);
      LRow.Configure.WhenPresentation(NyxPresentation('wide workspace')).Layout(nlRow).Gap(20);
    end;
    LRow.Add(NewNyxMemo('notes-editor').Configure.Text('Notes').Width(200).Height(120)
      .Value('Keep this English draft.').Done);
    LRow.Add(NewNyxMemo('other-editor').Configure.Text('Companion notes').Width(160).Height(120)
      .Value('This control stays independent.').Done);
  except
    Result.Free;
    raise;
  end;
end;

procedure RegistryJourney;
var
  LRegistry: INyxPresentations;
  LClone: INyxPresentations;
  LSnapshot: INyxPresentationSnapshot;
  LReference: TNyxPresentationRef;
  LDecoded: TNyxPresentationRef;
  LPlatform: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LCondition: TNyxViewportCondition;
  LKey: TNyxText;
  LBefore: TNyxText;
  LRejected: Boolean;
  LIndex: Integer;
begin
  LRegistry := NewNyxPresentations;
  Check(LRegistry.Count = 0, 'A new registry is empty');
  LReference := NyxPresentation(TNyxText('compact:%3A:=') + NyxScalarText($1F319));
  LCondition := TNyxViewportCondition.Any.WidthBelow(640).HeightAtLeast(200).Orientation(nvoPortrait);
  LRegistry.Define(LReference, LCondition);
  LSnapshot := LRegistry.Snapshot;
  LClone := LRegistry.Clone;
  LRegistry.Define(LReference, TNyxViewportCondition.Any.WidthBelow(800));
  Check(LRegistry.Count = 1, 'Replacement retains definition order without duplication');
  Check(LSnapshot.Condition(LReference).Same(LCondition), 'Runtime snapshot owns independent values');
  Check(LClone.Condition(LReference).Same(LCondition), 'Clone owns independent definition values');
  LRegistry.Remove(LReference);
  Check((LRegistry.Count = 0) and (LSnapshot.Count = 1), 'Removal cannot change an existing snapshot');
  LRegistry := nil;
  Check(LSnapshot.Condition(LReference).Same(LCondition), 'Snapshot safely outlives its mutable registry');
  LRegistry := NyxPresentationsFromData(LSnapshot.ToData);
  Check(LRegistry.Reference(0).Name = LReference.Name, 'Strict wire preserves supplementary names exactly');
  LKey := NyxPresentationKey(LReference, npfNativeLCL, atVisible);
  Check(TryNyxPresentationKey(LKey, LDecoded, LPlatform, LAttribute) and
    (LDecoded.Name = LReference.Name) and (LPlatform = npfNativeLCL) and (LAttribute = atVisible),
    'Canonical escaped Unicode/percent/colon scope round trips');
  Check(not TryNyxPresentationKey('@nyx.presentation:compact%3a:any:visible',
    LDecoded, LPlatform, LAttribute), 'Alternative lowercase escape spelling refuses');
  Check(not TryNyxPresentationKey('@nyx.presentation:compact:any:component',
    LDecoded, LPlatform, LAttribute), 'Structural meaning cannot vary through a presentation');
  LRejected := False;
  try
    NyxPresentation('  ');
  except
    on ENyxPresentation do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Whitespace-only names refuse');
  LRejected := False;
  try
    NyxPresentation('compact' + #10);
  except
    on ENyxPresentation do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Control characters in presentation names refuse');
  LBefore := LRegistry.ToData.ToJSON;
  LRejected := False;
  try
    LRegistry.Define(NyxPresentation('always'), TNyxViewportCondition.Any);
  except
    on ENyxPresentation do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRegistry.ToData.ToJSON = LBefore), 'Invalid predicates refuse before mutation');
  LRejected := False;
  try
    NyxPresentationsFromData(NyxObject([NyxField('version', NyxData(1)),
      NyxField('definitions', NyxArray([LSnapshot.ToData.Field('definitions').Item(0),
        LSnapshot.ToData.Field('definitions').Item(0)]))]));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Duplicate exact names refuse strict registry admission');
  for LIndex := 1 to NyxMaximumPresentations - 1 do
  begin
    LRegistry.Define(NyxPresentation(TNyxText('size-') + TNyxText(IntToStr(LIndex))),
      TNyxViewportCondition.Any.WidthBelow(640));
  end;
  LBefore := LRegistry.ToData.ToJSON;
  LRejected := False;
  try
    LRegistry.Define(NyxPresentation('overflow'), TNyxViewportCondition.Any.WidthBelow(640));
  except
    on ENyxPresentation do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRegistry.ToData.ToJSON = LBefore), 'Definition budget refuses without changing baseline');
end;

procedure ManualJourney;
var
  LDocument, LRuntime: TNyxNode;
  LOwner, LDecoded: TNyxDocument;
  LSession: TNyxStudioSession;
  LRegistry: INyxPresentations;
  LSnapshot: INyxPresentationSnapshot;
  LChoice: TNyxPresentationSelection;
  LFocused: TNyxPresentationRef;
  LWire, LSource, LBefore: TNyxText;
  LPair: TNyxProjectPair;
  LRejected: Boolean;
  LData: TNyxDataValue;
begin
  LOwner := Fixture;
  LDecoded := nil;
  LRuntime := nil;
  LSession := nil;
  { These are borrowed only within their owning document's lifetime. }
  LDocument := LOwner.Find('workspace');
  LFocused := NyxPresentation('focused');
  try
    LOwner.Presentations.Define(LFocused, TNyxPresentationCondition.Manual);
    LDocument.Configure.WhenPresentation(LFocused).Layout(nlRow).Gap(3)
      .ForPlatform(npfNativeLCL).Gap(7);
    { A later automatic rule cannot accidentally defeat an explicit choice. }
    LDocument.Configure.WhenViewport(TNyxViewportWidth.Below(640)).Gap(12);
    LChoice := TNyxPresentationSelection.Use(LFocused);
    LChoice.Validate(LOwner.Presentations);
    LRegistry := LOwner.Presentations.Clone;
    LSnapshot := LOwner.Presentations.Snapshot;
    LRegistry.Remove(LFocused);
    Check(LSnapshot.Definition(LFocused).Activation = npaManual,
      'Manual definitions retain independent immutable snapshots');
    Check(not LChoice.Reconciled(LRegistry).Reference.Defined,
      'An admitted removal clears only copied presentation state');
    LRejected := False;
    try
      LSnapshot.Condition(LFocused);
    except
      on ENyxPresentation do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Legacy viewport inspection refuses a manual definition');
    LWire := TNyxCodec.Encode(LOwner);
    LData := TNyxDataValue.ParseJSON(LWire).Field('presentations');
    Check(LData.Field('version').AsInteger = 2, 'Manual registries select their explicit version-two boundary');
    LDecoded := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LDecoded) = LWire, 'Manual activation persists without hidden viewport predicates');
    LSource := TNyxCodegen.Generate(LOwner);
    Check(Pos('TNyxPresentationCondition.Manual', LSource) > 0, 'Manual source uses the dedicated typed construct');
    Check(PrepareNyxCompanion(LOwner, LOwner, LSource, False) = LSource,
      'Manual generated source reconstructs the exact paired design');
    LRuntime := RealizeNyxView(LOwner, LOwner.Pages[0]);
    ApplyNyxPlatform(LRuntime, npfBrowser);
    LRuntime.ApplyViewport(390, 700, npfBrowser);
    Check(LRuntime.Find('workspace').Prop('gap') = '12', 'No manual choice preserves automatic authored precedence');
    LRuntime.ApplyViewport(390, 700, npfBrowser, LChoice);
    Check((LRuntime.Find('workspace').Prop('gap') = '3') and
      (LRuntime.Find('workspace').Prop('layout') = 'row'), 'Manual common scopes win after automatic common scopes');
    LRuntime.ApplyViewport(390, 700, npfNativeLCL, LChoice);
    Check(LRuntime.Find('workspace').Prop('gap') = '7', 'Concrete manual scopes win after common and automatic scopes');
    LRuntime.ApplyViewport(390, 700, npfNativeLCL);
    Check(LRuntime.Find('workspace').Prop('gap') = '10', 'Clearing a choice restores automatic target scopes');
    LRejected := False;
    try
      TNyxPresentationSelection.Use(NyxPresentation('compact')).Validate(LSnapshot);
    except
      on ENyxPresentation do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Explicit selection refuses automatic names');
    Check(TNyxCodec.Encode(LOwner) = LWire, 'Runtime selection never persists into the authored pair');
    LPair := NyxProjectPair(LWire, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([NyxDefinePresentation(LFocused,
      TNyxViewportCondition.Any.WidthBelow(480)).ToData])));
    Check(LSession.Document.Presentations.Definition(LFocused).Activation = npaAutomatic,
      'A paired update can deliberately replace manual activation with an automatic condition');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'One definition Undo restores exact manual activation and source');
    Check(ReadNyxStudioPresentationChoice(NyxStudioPresentationChoice(LChoice), LSnapshot).Same(LChoice),
      'The ordinary preview chooser retains exact manual names');
    { Manual constraints must also be valid while an automatic rule is active. }
    LOwner.Find('notes-editor').Configure.MinimumWidth(100)
      .WhenPresentation(LFocused).MaximumWidth(90);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LOwner);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Both-target constraint admission checks every exclusive manual choice');
  finally
    LSession.Free;
    LRuntime.Free;
    LDecoded.Free;
    LOwner.Free;
  end;
end;

procedure ModelJourney(out APair: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LClone: TNyxDocument;
  LRuntime: TNyxNode;
  LOther: TNyxNode;
  LBefore: TNyxText;
  LSource: TNyxText;
  LRejected: Boolean;
  LScope: INyxConfiguration;
  LDefinition: INyxCard;
begin
  LDocument := Fixture({$ifdef NYX_MANUAL_CONSUMER}True{$else}False{$endif});
  LClone := nil;
  LRuntime := nil;
  LOther := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 4,
      'Typed presentations select version four without affecting older empty designs');
    LClone := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LClone) = LBefore, 'Named scopes and definitions persist exactly');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('Result.Presentations.Define(NyxPresentation(''compact'')', LSource) > 0) and
      (Pos('.WhenPresentation(NyxPresentation(''compact''))', LSource) > 0) and
      (Pos('@nyx.presentation:', LSource) = 0), 'Generated source uses crafted typed definition and scope calls');
    Check(PrepareNyxCompanion(LDocument, LDocument, LSource, False) = LSource,
      'Named fluent source reconstructs the exact admitted pair');
    APair := NyxProjectPair(LBefore, LSource);
    LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
    ApplyNyxPlatform(LRuntime, npfBrowser);
    LRuntime.ApplyViewport(390, 700, npfBrowser);
    Check(LRuntime.Find('workspace').Prop('layout') = 'column', 'Named compact scope resolves at real host dimensions');
    Check(LRuntime.Find('workspace').Prop('gap') = '8', 'Common named scope applies copied dimensions');
    LRuntime.ApplyViewport(640, 420, npfBrowser);
    Check(LRuntime.Find('workspace').Prop('gap') = '16', 'Exclusive named boundary restores authored defaults');
    LDocument.Presentations.Define(NyxPresentation('compact'), TNyxViewportCondition.Any.WidthBelow(900));
    LRuntime.ApplyViewport(800, 700, npfBrowser);
    Check(LRuntime.Find('workspace').Prop('gap') = '16', 'Realized snapshot does not borrow mutable document conditions');
    LOther := RealizeNyxView(LDocument, LDocument.Pages[0]);
    ApplyNyxPlatform(LOther, npfBrowser);
    Check(CanRefreshNyxProjection(LRuntime, LOther), 'A shared condition change admits retained presentation properties');
    Check(RefreshNyxProjectionProperties(LRuntime, LOther, nil, []), 'Retained projection adopts one independent new snapshot');
    LRuntime.ApplyViewport(800, 700, npfBrowser);
    Check(LRuntime.Find('workspace').Prop('gap') = '8', 'Changing one shared definition reaches its retained control');
    Check(NyxProjectionContext(LClone) = NyxProjectionContext(LDocument),
      'Document framing and shared definitions do not force unrelated input remounts');
    LOther.Free;
    LOther := nil;
    LDocument.Free;
    LDocument := nil;
    LRuntime.ApplyViewport(390, 700, npfBrowser);
    Check(LRuntime.Find('workspace').Prop('gap') = '8', 'Realized named rules safely outlive the authored document');
    LClone.Find('workspace').Configure.WhenViewport(TNyxViewportWidth.Below(640)).Gap(12);
    LOther := RealizeNyxView(LClone, LClone.Pages[0]);
    ApplyNyxPlatform(LOther, npfBrowser);
    LOther.ApplyViewport(390, 700, npfBrowser);
    Check(LOther.Find('workspace').Prop('gap') = '12', 'Anonymous and named collisions preserve original authored order');
    LOther.Free;
    LOther := RealizeNyxView(LClone, LClone.Pages[0]);
    ApplyNyxPlatform(LOther, npfNativeLCL);
    LOther.ApplyViewport(390, 700, npfNativeLCL);
    Check(LOther.Find('workspace').Prop('gap') = '10', 'Concrete target named scope wins after common rules');
    LOther.Free;
    LOther := nil;
    LDefinition := NewNyxCard('shared-card');
    LClone.AddComponent(LDefinition);
    LScope := LDefinition.Configure.WhenPresentation(NyxPresentation('compact'));
    LScope.Padding(8).ForPlatform(npfNativeLCL).Padding(10);
    LDefinition.Configure.Padding(20);
    LClone.Pages[0].Add(NewNyxComponent('shared-use').Configure.Component(NyxComponent('shared-card')).Done);
    LOther := RealizeNyxView(LClone, LClone.Pages[0]);
    LOther.ApplyViewport(390, 700, npfBrowser);
    Check(LOther.Find(NyxQualifiedID('shared-use', 'shared-card')).Prop('padding') = '8',
      'Reusable realization inherits named rules without duplicated ownership');
    LClone.Find('notes-editor').Configure.MinimumWidth(100).MaximumWidth(400)
      .WhenPresentation(NyxPresentation('compact')).MaximumWidth(90);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LClone);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Piecewise both-target admission rejects conflicting named constraints');
    LClone.Find('notes-editor').Configure.WhenPresentation(NyxPresentation('compact')).MaximumWidth(400);
    LClone.Presentations.Remove(NyxPresentation('compact'));
    LRejected := False;
    try
      LClone.Validate;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Document admission refuses dangling named rules');
  finally
    LScope := nil;
    LDefinition := nil;
    LOther.Free;
    LRuntime.Free;
    LClone.Free;
    LDocument.Free;
  end;
end;

procedure EditorJourney(const APair: TNyxProjectPair);
var
  LAgent: TNyxAgentSession;
  LSession: TNyxStudioSession;
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LReply: TNyxDataValue;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LRejected: Boolean;
  LKey: TNyxText;
begin
  LAgent := TNyxAgentSession.Create(APair);
  LSession := nil;
  LShell := nil;
  try
    LReply := LAgent.Call('nyx_presentations', 'Scooty', NyxObject([NyxField('limit', NyxData(1))]));
    Check((LReply.Field('definitions').Count = 1) and LReply.Field('hasMore').AsBoolean,
      'Semantic definition pages are bounded');
    Check(LAgent.Revision = 1, 'Read-only definition inspection preserves revision');
    LReply := LAgent.Call('nyx_presentations', 'Scooty', NyxObject([NyxField('name', NyxData('compact'))]));
    Check(LReply.Field('definition').Field('widthMaximum').AsInteger = 640,
      'Exact semantic definition reads only requested context');
    LKey := NyxPresentationKey(NyxPresentation('compact'), npfAny, atVisible);
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(1)), NyxField('operationId', NyxData('named-visibility')),
      NyxField('operations', NyxArray([
        NyxUsePresentation(NyxControl('other-editor'), NyxPresentation('compact'), atVisible).ToData,
        NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('other-editor')),
          NyxField('properties', NyxObject([NyxField(LKey, NyxData(False))]))])]))]));
    LAfter := LAgent.ReviewSeed(LAgent.Revision);
    Check(Pos('.Visible(False)', LAfter.Source) > 0, 'Grouped semantic override stays typed in generated Pascal');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([NyxField('expectedRevision', NyxData(2)),
      NyxField('operationId', NyxData('undo-named')), NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.ReviewSeed(LAgent.Revision)) = EncodeNyxProject(APair),
      'One semantic Undo restores exact source and definition pair');
    LBefore := LAgent.ReviewSeed(LAgent.Revision);
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('dangling-removal')),
        NyxField('operations', NyxArray([NyxRemovePresentation(NyxPresentation('compact')).ToData]))]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (EncodeNyxProject(LAgent.ReviewSeed(LAgent.Revision)) = EncodeNyxProject(LBefore)),
      'Refused definition removal preserves paired source and history baseline');
    LSession := TNyxStudioSession.Create(APair);
    LSession.Select('notes-editor');
    LState := DefaultNyxStudioViewState;
    LShell := BuildNyxStudioView(LSession, LState);
    Check(LShell.Find(NyxStudioPresentationUseID) <> nil, 'Leaf controls expose named presentation authoring');
    LShell.Find(NyxStudioPresentationAttributeID).Configure.Value('visible');
    Check(CaptureNyxViewportInspector(LSession, LShell.Find(NyxStudioPresentationUseID), LShell.Pages[0], LEdit),
      'Nyx-built Inspector captures a typed exact-owner presentation command');
    LRequest := LSession.PrepareDesignRequest(LEdit, NyxSchemaRevision);
    Check(ReadNyxStudioDesignRequest(LRequest.ToData).SameRequest(LRequest),
      'Version-eight worker ticket preserves exact named intent');
    LPrepared := PrepareNyxStudioDesign(LRequest, CaptureNyxSchemas);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'The ordinary paired publication accepts its exact prepared ticket');
    Check(LSession.Selected.Props.IndexOfName(LKey) >= 0,
      'Ordinary isolated processor publishes a typed named leaf property');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(APair),
      'Ordinary Inspector Undo restores the exact accepted pair');
  finally
    LPrepared := nil;
    LShell.Free;
    LSession.Free;
    LAgent.Free;
  end;
end;

{ Names can be much longer than a control ID, and contain supplementary text.
  Qualify the complete paired editor boundary rather than only the registry.
  The Studio field ID must remain bounded while its exact reserved key survives. }
procedure UnicodeJourney;
var
  LDocument: TNyxDocument;
  LClone: TNyxDocument;
  LShell: TNyxDocument;
  LSession: TNyxStudioSession;
  LState: TNyxStudioViewState;
  LReference: TNyxPresentationRef;
  LName: TNyxText;
  LKey: TNyxText;
  LSource: TNyxText;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LIndex: Integer;
  LFound: Boolean;
  LInfos: TNyxPropertyInfos;
  LAgent: TNyxAgentSession;
  LRejected: Boolean;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LName := 'Wide:%=';
  for LIndex := 1 to 121 do
  begin
    LName := LName + NyxScalarText($1F319);
  end;
  LReference := NyxPresentation(LName);
  LDocument := Fixture;
  LClone := nil;
  LShell := nil;
  LSession := nil;
  LAgent := nil;
  try
    LDocument.Presentations.Define(LReference, TNyxViewportCondition.Any.WidthBelow(500));
    LDocument.Find('notes-editor').Configure.WhenPresentation(LReference).Visible(False);
    LKey := NyxPresentationKey(LReference, npfAny, atVisible);
    LSource := TNyxCodegen.Generate(LDocument);
    {$ifndef PAS2JS}

    if ParamCount > 1 then
    begin
      LFile := TFileStream.Create(ParamStr(2), fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LClone := TNyxCodec.Decode(LBefore.Design);
    Check((LClone.Presentations.Reference(2).Name = LName) and
      (TNyxCodec.Encode(LClone) = LBefore.Design),
      'Full scalar-budget presentation names persist exactly in the document');
    Check(PrepareNyxCompanion(LDocument, LClone, LSource, False) = LSource,
      'Supplementary named fluent source admits exact paired Unicode');
    ValidateNyxDocumentProperties(LClone);
    LAgent := TNyxAgentSession.Create(LBefore);
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(1)), NyxField('operationId', NyxData('long-name-value')),
      NyxField('operations', NyxArray([NyxSetPresentation(NyxControl('notes-editor'),
        LReference, atVisible, NyxData(True)).ToData]))]));
    Check(Pos('.Visible(True)', LAgent.ReviewSeed(LAgent.Revision).Source) > 0,
      'Semantic scalar upsert preserves the full Unicode name without long JSON keys');
    LAfter := LAgent.ReviewSeed(LAgent.Revision);
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(2)), NyxField('operationId', NyxData('long-name-wrong-family')),
        NyxField('operations', NyxArray([NyxSetPresentation(NyxControl('notes-editor'),
          LReference, atVisible, NyxData('false')).ToData]))]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = 2) and
      (EncodeNyxProject(LAgent.ReviewSeed(LAgent.Revision)) = EncodeNyxProject(LAfter)),
      'Wrong scalar family refuses atomically without revision, source or history changes');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(2)), NyxField('operationId', NyxData('long-name-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.ReviewSeed(LAgent.Revision)) = EncodeNyxProject(LBefore),
      'Semantic Unicode value Undo restores exact paired bytes');
    LSession := TNyxStudioSession.Create(LBefore);
    LSession.Select('notes-editor');
    LState := DefaultNyxStudioViewState;
    LShell := BuildNyxStudioView(LSession, LState);
    LInfos := NyxProperties(LSession.Selected, LSession.Document);
    LFound := False;
    for LIndex := 0 to High(LInfos) do
    begin
      LFound := LFound or (LInfos[LIndex].Key = LKey);
    end;
    Check(LFound, 'Published Inspector metadata retains the exact long Unicode scope key');
    Check(LShell.Find(NyxStudioPresentationChoiceID).Prop('items') =
      TNyxText('compact') + #10 + TNyxText('short landscape') + #10 + LName,
      'Nyx-built Inspector preserves every exact supplementary presentation choice');
    LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([NyxDefinePresentation(LReference,
      TNyxViewportCondition.Any.WidthBelow(700)).ToData])));
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Source <> LBefore.Source) and
      (LSession.Document.Presentations.Reference(2).Name = LName),
      'One paired definition update preserves its exact long Unicode identity');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One Unicode definition Undo restores the exact paired bytes');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'One Unicode definition Redo restores the exact paired bytes');
  finally
    LAgent.Free;
    LShell.Free;
    LSession.Free;
    LClone.Free;
    LDocument.Free;
  end;
end;

var
  LPair: TNyxProjectPair;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  GChecks := 0;
  RegistryJourney;
  ManualJourney;
  ModelJourney(LPair);
  EditorJourney(LPair);
  UnicodeJourney;
  {$ifndef PAS2JS}

  if ParamCount > 0 then
  begin
    LFile := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LFile.WriteBuffer(LPair.Source[1], Length(LPair.Source));
    finally
      LFile.Free;
    end;
  end;
  WriteLn('PASS ', GChecks, ' named presentation ownership/wire/source/semantic/paired checks');
  {$else}
  document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' named presentation checks';
  document.body.setAttribute('data-result', 'passed');
  document.body.setAttribute('data-checks', IntToStr(GChecks));
  {$endif}
end.
