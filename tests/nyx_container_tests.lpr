{ Copyright (c) mr-highball. SPDX-License-Identifier: MIT.
  Shared contract/admission/history qualification of the MCP-built companion. }
program nyx_container_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, {$ifndef PAS2JS}Classes,{$endif}
  nyx.text, nyx.types, nyx.containers, nyx.responsive,
  nyx.presentations, nyx.model, nyx.composition, nyx.codec, nyx.data,
  nyx.codegen, nyx.schema, nyx.platform, nyx.source, nyx.generated.view,
  nyx.studio.edits, nyx.studio.projects, nyx.studio.session
  {$ifdef PAS2JS}, Web{$endif};

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

procedure MeasurementJourney;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LMeasurements: TNyxContainerMeasurements;
  LSnapshot: INyxContainerSnapshot;
  LWidth, LHeight: Double;
  LRejected: Boolean;
begin
  LDocument := BuildNyxDocument;
  LRoot := nil;
  try
    LRoot := RealizeNyxView(LDocument, LDocument.Find('container-room'));
    SetLength(LMeasurements, 2);
    LMeasurements[0].RuntimeID := NyxQualifiedID('small-card', 'adaptive-card');
    LMeasurements[0].Width := 200.25;
    LMeasurements[0].Height := 150;
    LMeasurements[1].RuntimeID := NyxQualifiedID('large-card', 'adaptive-card');
    LMeasurements[1].Width := 400.5;
    LMeasurements[1].Height := 150;
    LSnapshot := NewNyxContainerSnapshot(LMeasurements);
    LMeasurements[0].Width := 900;
    Check(LSnapshot.TrySize(LMeasurements[0].RuntimeID, LWidth, LHeight) and
      (LWidth = 200.25) and (LHeight = 150), 'Measurements are exact independent copies');
    Check(not LSnapshot.TrySize('adaptive-card', LWidth, LHeight),
      'An unqualified reusable ID never borrows another instance measurement');
    LRoot.ApplyViewport(800, 480, npfBrowser, TNyxPresentationSelection.None, LSnapshot);
    Check(LRoot.Find(NyxQualifiedID('small-card', 'card-body')).Prop('layout') = 'column',
      'Small qualified instance selects its own compact rule');
    Check(LRoot.Find(NyxQualifiedID('large-card', 'card-body')).Prop('layout') = 'row',
      'Large qualified instance retains its own wide rule');
    Check(LRoot.Find('outside-status').Prop('text') = 'Outside the cards', 'Missing publisher remains inactive');
    LRoot.ApplyViewport(200, 100, npfBrowser);
    Check(LRoot.Find(NyxQualifiedID('small-card', 'card-body')).Prop('layout') = 'row',
      'Compatibility viewport projection never guesses container measurements');
    LMeasurements[0].Width := -1;
    LRejected := False;
    try
      NewNyxContainerSnapshot(LMeasurements);
    except
      on ENyxContainer do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LSnapshot.TrySize(LMeasurements[0].RuntimeID, LWidth, LHeight) and
      (LWidth = 200.25), 'Negative candidate refuses without changing an existing snapshot');
    LMeasurements[0].Width := 200;
    LMeasurements[1].RuntimeID := LMeasurements[0].RuntimeID;
    LRejected := False;
    try
      NewNyxContainerSnapshot(LMeasurements);
    except
      on ENyxContainer do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Duplicate runtime measurements refuse');
    Check(NyxContainerEligible(nccWidth, TNyxViewportCondition.Any.WidthBelow(300)),
      'Width containment is eligible for width queries');
    Check(not NyxContainerEligible(nccWidth, TNyxViewportCondition.Any.HeightBelow(300)),
      'Width containment is ineligible for height queries');
    Check(NyxContainerEligible(nccSize, TNyxViewportCondition.Any.Orientation(nvoPortrait)),
      'Full size containment is eligible for orientation queries');
  finally
    LSnapshot := nil;
    LRoot.Free;
    LDocument.Free;
  end;
end;

procedure AncestryJourney;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LMeasurements: TNyxContainerMeasurements;
  LReference: TNyxPresentationRef;
begin
  LDocument := BuildNyxDocument;
  LRoot := nil;
  try
    LDocument.Find('container-room').Configure.QueryContainer(NyxContainer('card space')).Containment(nccSize);
    LReference := NyxPresentation('tall parent');
    LDocument.Presentations.Define(LReference, TNyxPresentationCondition.Within(
      NyxContainer('card space'), TNyxViewportCondition.Any.Orientation(nvoPortrait)));
    LDocument.Find('card-status').Configure.WhenPresentation(LReference).Text('Tall ancestral space');
    LDocument.Find('container-room').Configure.WhenPresentation(LReference).Text('Self query must stay inactive');
    LRoot := RealizeNyxView(LDocument, LDocument.Find('container-room'));
    SetLength(LMeasurements, 3);
    LMeasurements[0].RuntimeID := 'container-room';
    LMeasurements[0].Width := 500;
    LMeasurements[0].Height := 700;
    LMeasurements[1].RuntimeID := NyxQualifiedID('small-card', 'adaptive-card');
    LMeasurements[1].Width := 200;
    LMeasurements[1].Height := 100;
    LMeasurements[2].RuntimeID := NyxQualifiedID('large-card', 'adaptive-card');
    LMeasurements[2].Width := 400;
    LMeasurements[2].Height := 100;
    LRoot.ApplyViewport(800, 480, npfNativeLCL, TNyxPresentationSelection.None,
      NewNyxContainerSnapshot(LMeasurements));
    Check(LRoot.Find(NyxQualifiedID('small-card', 'card-status')).Prop('text') = 'Tall ancestral space',
      'Orientation skips a nearer width-only publisher and uses the eligible ancestor');
    Check(LRoot.Find(NyxQualifiedID('large-card', 'card-status')).Prop('text') = 'Tall ancestral space',
      'The same outer full-size publisher can serve both independent descendants');
    Check(LRoot.Prop('text') <> 'Self query must stay inactive', 'A publisher cannot query itself');
    SetLength(LMeasurements, 1);
    LRoot.ApplyViewport(800, 480, npfNativeLCL, TNyxPresentationSelection.None,
      NewNyxContainerSnapshot(LMeasurements));
    Check(LRoot.Find(NyxQualifiedID('small-card', 'card-body')).Prop('layout') = 'row',
      'Missing nearest width box never falls through to an outer measured publisher');
  finally
    LRoot.Free;
    LDocument.Free;
  end;
end;

procedure AdmissionJourney;
var
  LDocument, LClone: TNyxDocument;
  LSession: TNyxStudioSession;
  LBefore, LAfter: TNyxProjectPair;
  LReference: TNyxPresentationRef;
  LUnicode: TNyxText;
  LRejected: Boolean;
  {$ifndef PAS2JS}
  LExport: TFileStream;
  {$endif}
begin
  LDocument := BuildNyxDocument;
  LClone := nil;
  LSession := nil;
  try
    LUnicode := TNyxText('A writer''s space : ') + NyxScalarText($1F680);
    LReference := NyxPresentation('compact card');
    LDocument.Presentations.Define(LReference, TNyxPresentationCondition.Within(
      NyxContainer(LUnicode), TNyxViewportCondition.Any.WidthBelow(300)));
    LDocument.Find('adaptive-card').Configure.QueryContainer(NyxContainer(LUnicode));
    Check(LDocument.Presentations.ToData.Field('version').AsInteger = 3, 'Container definitions use strict version-three wire data');
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    {$ifndef PAS2JS}
    { This optional test artifact compiles unchanged on both targets. Its text
      values remain English; only the dedicated Unicode query name differs. }

    if ParamCount = 1 then
    begin
      LExport := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LExport.WriteBuffer(LBefore.Source[1], Length(LBefore.Source));
      finally
        LExport.Free;
      end;
    end;
    {$endif}
    LClone := TNyxCodec.Decode(LBefore.Design);
    Check(TNyxCodec.Encode(LClone) = LBefore.Design, 'Supplementary container names persist exactly');
    Check(PrepareNyxCompanion(LDocument, LClone, LBefore.Source, False) = LBefore.Source,
      'Typed Unicode fluent source admits an exact companion');
    ValidateNyxDocumentProperties(LClone);
    LSession := TNyxStudioSession.Create(LBefore);
    LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([NyxDefinePresentation(LReference,
      TNyxPresentationCondition.Within(NyxContainer(LUnicode),
      TNyxViewportCondition.Any.WidthBelow(280))).ToData])));
    LAfter := LSession.ProjectSnapshot;
    Check(EncodeNyxProject(LAfter) <> EncodeNyxProject(LBefore), 'One definition edit changes the accepted pair');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore), 'One Undo restores exact design and source');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'One Redo restores exact design and source');
    LRejected := False;
    try
      LSession.ApplyPatch(ReadNyxDesignPatch(NyxArray([
        NyxObject([NyxField('op', NyxData('update')), NyxField('id', NyxData('card-notes')),
          NyxField('properties', NyxObject([NyxField('min-width', NyxData(240))]))]),
        NyxSetPresentation(NyxControl('card-notes'), LReference, atMaximumWidth,
          NyxData(180)).ToData])));
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter)),
      'Conflicting grouped bounds refuse atomically without changing either accepted file');
    LRejected := False;
    try
      LDocument.Find('adaptive-card').Configure.ForPlatform(npfBrowser).Containment(nccSize);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Container publication cannot be conditional target metadata');
    LDocument.Find('card-notes').Configure.MinimumWidth(240);
    LDocument.Find('card-notes').Configure.WhenPresentation(LReference).MaximumWidth(180);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Conflicting container-specific bounds refuse before source or mount');
    LDocument.Find('card-notes').Configure.WhenPresentation(LReference).MaximumWidth(260);
    ValidateNyxDocumentProperties(LDocument);
    Check(True, 'Compatible container-specific bounds remain supported');
    LDocument.Find('card-notes').Configure.Clear(atMinimumWidth).MaximumWidth(400);
    LDocument.Find('card-notes').Configure.WhenPresentation(LReference).MinimumWidth(360).MaximumWidth(400);
    LDocument.Presentations.Define(NyxPresentation('spacious card'),
      TNyxPresentationCondition.Within(NyxContainer(LUnicode),
      TNyxViewportCondition.Any.WidthAtLeast(300)));
    LDocument.Find('card-notes').Configure.WhenPresentation(NyxPresentation('spacious card')).MaximumWidth(320);
    ValidateNyxDocumentProperties(LDocument);
    Check(True, 'Mutually exclusive bounds in one coordinate space remain valid');
    LDocument.Presentations.Define(NyxPresentation('small host'), TNyxViewportCondition.Any.WidthBelow(640));
    LDocument.Find('card-notes').Configure.WhenPresentation(NyxPresentation('small host')).MaximumWidth(320);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Host and container allocations are independently partitioned');
    LDocument.Find('card-notes').Configure.WhenPresentation(NyxPresentation('small host')).MaximumWidth(400);
    LDocument.Find('container-room').Configure.QueryContainer(NyxContainer(LUnicode)).Containment(nccSize);
    LDocument.Presentations.Define(NyxPresentation('tall outer frame'),
      TNyxPresentationCondition.Within(NyxContainer(LUnicode),
      TNyxViewportCondition.Any.Orientation(nvoPortrait)));
    LDocument.Find('card-notes').Configure.WhenPresentation(NyxPresentation('tall outer frame')).MaximumWidth(320);
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'A skipped ineligible width publisher and outer size publisher have independent spaces');
    LDocument.Find('card-notes').Configure.WhenPresentation(LReference).MinimumWidth(300);
    ValidateNyxDocumentProperties(LDocument);
    Check(True, 'Compatible nested independent bounds admit');
    LDocument.Find('adaptive-card').Configure.Containment(nccSize).Clear(atContainerContainment);
    Check(LDocument.Find('adaptive-card').ContainerContainment = nccWidth, 'Clearing containment restores its typed width default');
    LDocument.Find('adaptive-card').Configure.Clear(atQueryContainer);
    Check(not LDocument.Find('adaptive-card').QueryContainer.Defined, 'Clearing a publisher removes the exact declaration');
    TNyxCodegen.Generate(LDocument);
    Check(True, 'Cleared publisher metadata generates typed Clear calls');
  finally
    LSession.Free;
    LClone.Free;
    LDocument.Free;
  end;
end;

begin
  try
    MeasurementJourney;
    AncestryJourney;
    AdmissionJourney;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' container contract checks';
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' container contract/ancestry/admission/paired checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LException.Message);
      {$else}
      raise;
      {$endif}
    end;
  end;
end.
