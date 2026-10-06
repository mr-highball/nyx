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


unit nyx.test.constraints;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.projects;

{ Independent English design; callers own both copied files. No listener or
  observing project is involved. The journey uses the actual semantic core and
  isolated paired processor, returning its admitted source for real compilation. }
function NyxConstraintsFixture: TNyxProjectPair;
function RunNyxConstraintsJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.layout.constraints, nyx.layout.flow,
  nyx.codec, nyx.codegen, nyx.schema, nyx.studio.agents, nyx.studio.session,
  nyx.studio.inspector, nyx.studio.sourcejobs;

function NyxConstraintsFixture: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LRow: INyxRow;
  LColumn: INyxColumn;
  LBadge: INyxBadge;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Make room for your ideas';
    LDocument.AddPage(NewNyxPage('home').Configure.Padding(0).Gap(12).Done);
    LRow := NewNyxRow('notes-row');
    LRow.Configure.Width(400).Height(100).Gap(10).Padding(0)
      .Wrap(nfwNoWrap).Align(ncaStretch);
    LRow.Add(NewNyxMemo('notes-editor').Configure.Text('Notes')
      .Value('Keep these English notes.').Flex(1).MinimumWidth(180)
      .MaximumHeight(60).HeightSizing(nsFill).Done);
    LRow.Add(NewNyxLabel('side-caption').Configure.Text('Ideas').Flex(1)
      .MaximumWidth(80).Done);
    LDocument.Find('home').Add(LRow);

    LRow := NewNyxRow('mixed-row');
    LRow.Configure.Width(100).Height(80).Gap(0).Padding(0).Wrap(nfwNoWrap);
    LRow.Add(NewNyxLabel('minimum-caption').Configure.Text('Minimum')
      .Flex(1).MinimumWidth(60).Done);
    LRow.Add(NewNyxLabel('maximum-caption').Configure.Text('Maximum')
      .Flex(1).MaximumWidth(20).Done);
    LDocument.Find('home').Add(LRow);

    LColumn := NewNyxColumn('weighted-column');
    LColumn.Configure.Width(400).Height(200).Gap(10).Padding(0);
    LColumn.Add(NewNyxMemo('first-editor').Configure.Text('First')
      .Value('A short note.').Flex(1).HeightSizing(nsFill).MaximumHeight(50).Done);
    LColumn.Add(NewNyxMemo('second-editor').Configure.Text('Second')
      .Value('Room to write.').Flex(1).HeightSizing(nsFill).MinimumHeight(80).Done);
    LDocument.Find('home').Add(LColumn);

    LRow := NewNyxRow('wrapped-row');
    LRow.Configure.Width(100).Gap(10).Padding(0).Wrap(nfwWrap);
    LRow.Add(NewNyxButton('first-action').Configure.Text('First')
      .Flex(1).MinimumWidth(60).Height(32).Done);
    LRow.Add(NewNyxButton('second-action').Configure.Text('Second')
      .Flex(1).MinimumWidth(60).Height(32).Done);
    LDocument.Find('home').Add(LRow);
    LDocument.Find('home').Add(NewNyxLabel('bounded-caption').Configure
      .Text('A bounded caption').Width(350)
      .Constraints(NyxSizeConstraints.MaximumWidth(140)
        .MinimumHeight(40).MaximumHeight(50)).Done);
    LDocument.Find('home').Add(NewNyxSpacer('zero-space').Configure
      .Flex(0).MaximumWidth(0).MaximumHeight(0).Done);
    LDocument.Find('home').Add(NewNyxLabel('overflow-caption').Configure
      .Text('Leading content remains reachable').Width(100).Height(20)
      .MinimumWidth(500).Done);
    LDocument.Find('overflow-caption').Configure.ForPlatform(npfNativeLCL).MaximumWidth(1000);
    LBadge := NewNyxBadge('platform-badge');
    LBadge.Configure.Text('Preview').Height(40)
      .Constraints(NyxSizeConstraints.MinimumWidth(20).MaximumWidth(200));
    LBadge.Configure.ForPlatform(npfNativeLCL).MinimumWidth(90).MaximumWidth(110);
    LBadge.Configure.ForPlatform(npfBrowser).MinimumWidth(80).MaximumWidth(100);
    LDocument.Find('home').Add(LBadge);
    ValidateNyxDocumentProperties(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    { Exercise the friendly copied value contract through Studio's strict source
      reader as well as Pascal compilation; regeneration may expand it into the
      equivalent readable scalar methods. No arbitrary call is evaluated. }
    LSource := StringReplace(LSource, 'nyx.controls;',
      'nyx.controls, nyx.layout.constraints;', []);
    LSource := StringReplace(LSource,
      '      .Clear(atMinimumWidth)' + #10 + '      .MaximumWidth(140)' + #10 +
      '      .MinimumHeight(40)' + #10 + '      .MaximumHeight(50)',
      '      .Constraints(NyxSizeConstraints.Width(NyxSizeRange.Maximum(140))' + #10 +
      '        .Height(NyxSizeRange.Minimum(40).Maximum(50)))', []);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), StringReplace(LSource, 'implementation',
      '{ Keep this handwritten English comment. }' + #10 + 'implementation', []));
  finally
    LBadge := nil;
    LColumn := nil;
    LRow := nil;
    LDocument.Free;
  end;
end;

function RunNyxConstraintsJourney(out APair: TNyxProjectPair): Integer;
var
  LChecks, LIndex, LBudget: Integer;
  LRange, LCopy: TNyxSizeRange;
  LItems: TNyxFlowItems;
  LRanges: TNyxSizeRanges;
  LSizes, LPositions: TNyxFlowSizes;
  LLines: TNyxFlowLines;
  LRefused: Boolean;
  LDocument: TNyxDocument;
  LAgent: TNyxAgentSession;
  LRevision: Integer;
  LBefore, LAfter: TNyxProjectPair;
  LQuery: TNyxDataValue;
  LSession: TNyxStudioSession;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LCommands: TNyxSourceCommands;
  LReset: TNyxNode;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Size constraints: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure Sizes(AAvailable, AFirst, ASecond: Integer; const AReason: TNyxText);
  begin
    LSizes := NyxFlowSizes(AAvailable, 0, LItems, LRanges);
    Check((LSizes[0] = AFirst) and (LSizes[1] = ASecond), AReason);
  end;

  function Update(const AID: TNyxText; const AFields: array of TNyxDataField): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('op', NyxData('update')),
      NyxField('id', NyxData(AID)), NyxField('properties', NyxObject(AFields))]);
  end;

  function Arguments(const AID: TNyxText; const AChanges: array of TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('operationId', NyxData(AID)),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operations', NyxArray(AChanges))]);
  end;

  procedure Refuses(const AName: TNyxText; const AChanges: array of TNyxDataValue);
  var
    LBaseline: TNyxProjectPair;
    LRejected: Boolean;
  begin
    LBaseline := LAgent.PreviewPair(LRevision, 'home');
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', Arguments(AName, AChanges));
    except
      on E: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = LRevision) and
      (EncodeNyxProject(LBaseline) = EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'))),
      'atomic refusal preserves paired files/revision: ' + AName);
  end;

  procedure History(const ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('history-' + IntToStr(LRevision))),
      NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;
begin
  LChecks := 0;
  LRange := NyxSizeRange.Minimum(20).Maximum(80);
  LCopy := LRange.WithoutMinimum.Maximum(0);
  Check((LRange.Clamp(1) = 20) and (LRange.Clamp(100) = 80), 'copied range clamps both ends');
  Check(LCopy.HasMaximum and (LCopy.Clamp(100) = 0), 'explicit zero differs from absent maximum');
  Check(LCopy.WithoutMaximum.Clamp(100) = 100, 'clearing restores an unbounded maximum');
  LRefused := False;
  try
    LCopy := LRange.Minimum(81);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRange.MinimumValue = 20), 'failed builder retains original policy');

  SetLength(LItems, 2);
  SetLength(LRanges, 2);
  for LIndex := 0 to 1 do
  begin
    LItems[LIndex].Visible := True;
    LItems[LIndex].NaturalSize := 0;
    LItems[LIndex].Weight := 1;
    LRanges[LIndex] := NyxSizeRange;
  end;
  LRanges[0] := NyxSizeRange.Minimum(60);
  LRanges[1] := NyxSizeRange.Maximum(20);
  Sizes(100, 80, 20, 'mixed violations freeze maxima before redistributing');
  Sizes(60, 60, 0, 'minimum owns a completely consumed budget');
  Sizes(40, 60, 0, 'minimum overflow stays nonnegative');
  LRanges[0] := NyxSizeRange.Maximum(20);
  LRanges[1] := NyxSizeRange.Minimum(60);
  Sizes(100, 20, 80, 'source order does not change mixed-bound allocation');
  LRanges[0] := NyxSizeRange.Maximum(20);
  LRanges[1] := NyxSizeRange.Maximum(30);
  Sizes(100, 20, 30, 'capped siblings leave remaining space for alignment');
  LPositions := NyxFlowPositions(100, 0, LItems, LSizes, njEnd);
  Check((LPositions[0] = 50) and (LPositions[1] = 70), 'positions consume actual capped sizes');
  LRanges[0] := NyxSizeRange.Minimum(60);
  LRanges[1] := NyxSizeRange.Minimum(60);
  Sizes(100, 60, 60, 'minimum overflow retains each child');
  LPositions := NyxFlowPositions(100, 0, LItems, LSizes, njEnd);
  Check(LPositions[0] = 0, 'overflow retains reachable leading alignment');
  LLines := NyxFlowLines(100, 10, LItems, LRanges, True);
  Check((Length(LLines) = 2) and (LLines[1].First = 1), 'weighted minima determine wrapped lines');
  LItems[1].Visible := False;
  LSizes := NyxFlowSizes(100, 10, LItems, LRanges);
  Check((LSizes[0] = 100) and (LSizes[1] = 0), 'hidden bounds consume neither space nor gap');
  LItems[1].Visible := True;
  LRanges[0] := NyxSizeRange.Minimum(20).Maximum(80);
  LRanges[1] := NyxSizeRange;
  LItems[1].Weight := 2;
  for LBudget := 20 to 220 do
  begin
    LSizes := NyxFlowSizes(LBudget, 0, LItems, LRanges);
    Check((LSizes[0] >= 20) and (LSizes[0] <= 80) and
      (LSizes[0] + LSizes[1] = LBudget), 'bounds and cumulative rounding retain every pixel');
  end;

  LAgent := TNyxAgentSession.Create(NyxConstraintsFixture);
  try
    LRevision := LAgent.Revision;
    LBefore := LAgent.PreviewPair(LRevision, 'home');
    LQuery := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('notes-editor')), NyxField('keys', NyxArray([
        NyxData('min-width'), NyxData('max-width'), NyxData('min-height'), NyxData('max-height')])),
      NyxField('limit', NyxData(4))]));
    Check(LQuery.Field('node').Field('id').AsText = 'notes-editor', 'bounded semantic inspection uses the same selected component');
    Refuses('negative', [Update('notes-editor', [NyxField('min-width', NyxData(-1))])]);
    Refuses('fractional', [Update('notes-editor', [NyxField('min-width', NyxData(1.5))])]);
    Refuses('inverted', [Update('notes-editor', [NyxField('max-width', NyxData(170))])]);
    Refuses('platform-inherited', [Update('overflow-caption', [NyxField('min-width', NyxData(1001))])]);
    Refuses('platform-inverted', [Update('platform-badge', [
      NyxField(NyxPlatformKey(npfNativeLCL, atMaximumWidth), NyxData(85))])]);
    Refuses('group-inverted', [
      Update('notes-editor', [NyxField('text', NyxData('Must not publish'))]),
      Update('side-caption', [NyxField('min-width', NyxData(81))])]);
    Refuses('too-large', [Update('notes-editor', [
      NyxField('max-height', NyxData(MaximumNyxLayoutBound + 1))])]);
    LAgent.Call('nyx_transaction', 'Scooty', Arguments('room-for-notes', [
      Update('notes-editor', [NyxField('min-width', NyxData(200)),
        NyxField('max-width', NyxData(300)), NyxField('max-height', NyxData(48))]),
      Update('first-editor', [NyxField('max-height', NyxData(40))]),
      Update('second-editor', [NyxField('min-height', NyxData(90))])]));
    LRevision := LAgent.Revision;
    LAfter := LAgent.PreviewPair(LRevision, 'home');
    Check((Pos('.MinimumWidth(200)', LAfter.Source) > 0) and
      (Pos('.MaximumWidth(300)', LAfter.Source) > 0) and
      (Pos('INyxMemo', LAfter.Source) > 0), 'generated source uses specialized typed configuration');
    Check(Pos('Keep this handwritten English comment.', LAfter.Source) > 0, 'handwritten source survives size authoring');
    LDocument := TNyxCodec.Decode(LAfter.Design);
    try
      Check(NyxNodeSizeConstraints(LDocument.Find('notes-editor')).WidthRange.MaximumValue = 300,
        'persistence retains exact admitted bounds');
      Check(NyxNodeSizeConstraints(LDocument.Find('platform-badge'), npfNativeLCL)
        .WidthRange.MinimumValue = 90, 'native override is independent');
      Check(NyxNodeSizeConstraints(LDocument.Find('platform-badge'), npfBrowser)
        .WidthRange.MinimumValue = 80, 'browser override is independent');
    finally
      LDocument.Free;
    end;
    History('undo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LBefore),
      'one Undo restores all related bounds and their source');
    History('redo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LAfter),
      'one Redo restores the exact accepted pair');
    APair := LAfter;
  finally
    LAgent.Free;
  end;

  LSession := TNyxStudioSession.Create(APair);
  LSchemas := CaptureNyxSchemas;
  try
    LSession.Select('side-caption');
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LReset := TNyxNode.Create(nkButton, 'retained-reset');
    try
      LReset.SetProp(NyxStudioPropertyClearKey, NyxAttributeName(atMaximumWidth))
        .SetProp(NyxStudioPropertyOwnerKey, 'notes-editor');
      LBefore := LSession.ProjectSnapshot;
      LRefused := False;
      try
        LCommands.Route(LReset, ntClick, nil);
      except
        on E: ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
        'retained reset refuses after selection changes');
      LReset.SetProp(NyxStudioPropertyOwnerKey, 'side-caption')
        .SetProp(NyxStudioPropertyClearKey, NyxAttributeName(atText));
      LRefused := False;
      try
        LCommands.Route(LReset, ntClick, nil);
      except
        on E: ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and not LCommands.Busy,
        'size reset cannot execute a different property command');
    finally
      LReset.Free;
      LCommands.Free;
    end;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaProperty;
    LEdit.Selection := 'notes-editor';
    LEdit.View := 'home';
    LEdit.Name := NyxAttributeName(atMaximumHeight);
    LEdit.Value := '44';
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LRequest := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    LBefore := LSession.ProjectSnapshot;
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'isolated processor admits an ordinary property edit');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preparation leaves accepted owners untouched');
    LPrepared := ReceiveNyxPreparedDesign(TNyxDataValue.ParseJSON(LPrepared.ToData.ToJSON),
      LRequest, LSchemas);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'ordinary property publishes one paired result');
    Check(LSession.Document.Find('notes-editor').Prop('max-height') = '44',
      'typed scalar reaches the ordinary inspector workflow');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'ordinary property has paired Undo');
  finally
    LSession.Free;
  end;
  Result := LChecks;
end;

end.
