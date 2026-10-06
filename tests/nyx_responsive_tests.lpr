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
program nyx_responsive_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Math, nyx.text, nyx.types, nyx.responsive, nyx.model,
  nyx.controls, nyx.codec, nyx.codegen, nyx.schema, nyx.platform, nyx.composition,
  nyx.data, nyx.studio.session, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.inspector, nyx.studio.view, nyx.studio.sourcejobs
  {$ifdef PAS2JS}, Web{$endif};

var
  LChecks: Integer;
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LRow: INyxRow;
  LCommon: INyxConfiguration;
  LCompact: INyxConfiguration;
  LNative: INyxConfiguration;
  LWidth: TNyxViewportWidth;
  LPlatform: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LBefore: TNyxText;
  LSource: TNyxText;
  LRejected: Boolean;
  LMetadata: TNyxPropertyInfos;
  LIndex: Integer;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LAccepted: TNyxProjectPair;
  LReply: TNyxDataValue;
  LRevision: Integer;
  LSession: TNyxStudioSession;
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

procedure Transaction(const AID: TNyxText; const AOperations: TNyxDataValue);
begin
  LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(AID)),
    NyxField('operations', AOperations)]));
  LRevision := LAgent.Revision;
end;

begin
  LDocument := nil;
  LRoot := nil;
  LAgent := nil;
  LSession := nil;
  LShell := nil;
  try
    LChecks := 0;
    Check(TNyxViewportWidth.Below(640).Matches(639.5), 'Fractional available width matches');
    Check(not TNyxViewportWidth.Below(640).Matches(640), 'Upper boundary is exclusive');
    Check(TNyxViewportWidth.AtLeast(640).Matches(640), 'Lower boundary is inclusive');
    Check(TNyxViewportWidth.Between(390, 640).Matches(390), 'Interval includes its beginning');
    Check(not TNyxViewportWidth.Between(390, 640).Matches(389), 'Interval excludes lower widths');
    LRejected := False;
    try
      TNyxViewportWidth.Between(640, 640);
    except
      on EArgumentException do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Empty intervals refuse');
    LRejected := False;
    try
      TNyxViewportWidth.Any.Matches(NaN);
    except
      on EArgumentException do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Nonfinite geometry refuses');
    Check(TryNyxViewportKey(NyxViewportKey(TNyxViewportWidth.Below(640), npfNativeLCL,
      atGap), LWidth, LPlatform, LAttribute) and (LPlatform = npfNativeLCL) and
      (LAttribute = atGap) and LWidth.Same(TNyxViewportWidth.Below(640)), 'Canonical wire identity');
    Check(not TryNyxViewportKey('@nyx.viewport:00:640:any:gap', LWidth,
      LPlatform, LAttribute), 'Noncanonical bounds refuse');

    LDocument := TNyxDocument.Create;
    LDocument.Title := 'Room for ideas';
    LDocument.AddPage(NewNyxPage('home').Configure.Padding(16).Gap(12).Done);
    LRow := NewNyxRow('workspace');
    LDocument.Pages[0].Add(LRow);
    LCommon := LRow.Configure;
    LCommon.Gap(16).Wrap(nfwNoWrap);
    LCompact := LCommon.WhenViewport(TNyxViewportWidth.Below(640));
    LCompact.Layout(nlColumn).Gap(8);
    LNative := LCompact.ForPlatform(npfNativeLCL);
    LNative.Gap(10);
    LCommon.Padding(0);
    LRow.Add(NewNyxMemo('notes-editor').Configure.Text('Notes').Width(200).Height(120)
      .Value('Keep this English draft.').Done);
    LRow.Add(NewNyxMemo('other-editor').Configure.Text('Companion notes').Width(160).Height(120)
      .Value('This control stays independent.').Done);
    Check(LRow.Node.Prop('gap') = '16', 'Retained defaults are independent of compact scopes');
    Check(LRow.Node.Prop(NyxViewportKey(TNyxViewportWidth.Below(640), npfAny, atGap)) = '8',
      'Retained target facade cannot change common compact value');
    Check(LRow.Node.Configure.ForPlatform(npfNativeLCL).WhenViewport(TNyxViewportWidth.Below(640)) =
      LRow.Node.Configure.WhenViewport(TNyxViewportWidth.Below(640)).ForPlatform(npfNativeLCL),
      'Viewport and platform facade selection commute');
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('.WhenViewport(TNyxViewportWidth.Below(640))', LSource) > 0) and
      (Pos('INyxRow', LSource) > 0) and (Pos('@nyx.viewport', LSource) = 0),
      'Generated Pascal keeps typed crafted authoring');

    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    ApplyNyxPlatform(LRoot, npfBrowser);
    LRoot.ApplyViewport(390, npfBrowser);
    Check(LRoot.Find('workspace').Prop('layout') = 'column', 'Browser compact direction');
    Check(LRoot.Find('workspace').Prop('gap') = '8', 'Browser common compact metrics');
    LRoot.Find('workspace').SetProp('gap', '23');
    LRoot.ApplyViewport(640, npfBrowser);
    Check(LRoot.Find('workspace').Prop('gap') = '23', 'Leaving a rule restores current live default');
    Check(LRoot.Find('workspace').Prop('layout', 'row') = 'row', 'Leaving a rule restores default direction');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'Viewport projection never changes authored persistence');
    LRoot.Free;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    ApplyNyxPlatform(LRoot, npfNativeLCL);
    LRoot.ApplyViewport(390, npfNativeLCL);
    Check(LRoot.Find('workspace').Prop('gap') = '10', 'Concrete target overrides matching common scope');
    LRoot.Free;
    LRoot := nil;
    LMetadata := NyxProperties(LRow.Node, LDocument);
    LRejected := True;
    for LIndex := 0 to High(LMetadata) do
    begin

      if LMetadata[LIndex].Key = NyxViewportKey(TNyxViewportWidth.Below(640), npfAny, atGap) then
      begin
        LRejected := False;
        Check((LMetadata[LIndex].ValueType = npInteger) and
          (Pos('Below 640 px', LMetadata[LIndex].Title) > 0), 'Inspector preserves scoped types and intent');
      end;
    end;
    Check(not LRejected, 'Responsive metadata is discoverable');
    LPair := NyxProjectPair(LBefore, LSource);
    LAgent := TNyxAgentSession.Create(LPair);
    LRevision := LAgent.Revision;
    Transaction('responsive-group', NyxArray([NyxObject([
      NyxField('op', NyxData('update')), NyxField('id', NyxData('workspace')),
      NyxField('properties', NyxObject([
        NyxField(NyxViewportKey(TNyxViewportWidth.Between(640, 900), npfAny, atGap), NyxData(20)),
        NyxField(NyxViewportKey(TNyxViewportWidth.Between(640, 900), npfAny, atColumns), NyxData(2))]))])]));
    LAccepted := LAgent.PreviewPair(LRevision, 'home');
    Check(Pos('TNyxViewportWidth.Between(640, 900)', LAccepted.Source) > 0,
      'Semantic grouped creation generates typed interval source');
    LReply := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('workspace')), NyxField('keys', NyxArray([
        NyxData(NyxViewportKey(TNyxViewportWidth.Between(640, 900), npfAny, atGap))]))]));
    Check(LReply.Field('properties').Item(0).Field('value').AsInteger = 20,
      'Bounded semantic query returns a typed numeric scoped value');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('responsive-undo')),
      NyxField('direction', NyxData('undo'))]));
    LRevision := LAgent.Revision;
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LPair),
      'One semantic Undo restores the entire paired source/design');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('responsive-redo')),
      NyxField('direction', NyxData('redo'))]));
    LRevision := LAgent.Revision;
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LAccepted),
      'One semantic Redo restores exact authored rules');
    LRejected := False;
    try
      Transaction('responsive-bad-bounds', NyxArray([NyxObject([
        NyxField('op', NyxData('update')), NyxField('id', NyxData('workspace')),
        NyxField('properties', NyxObject([
          NyxField(NyxViewportKey(TNyxViewportWidth.Below(800), npfAny, atMinimumWidth), NyxData(300)),
          NyxField(NyxViewportKey(TNyxViewportWidth.Between(600, 900), npfNativeLCL, atMaximumWidth), NyxData(200))]))])]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LAgent.Revision = LRevision), 'Overlapping effective native bounds refuse atomically');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LAccepted),
      'Failed admission preserves accepted Pascal and rules');

    LSession := TNyxStudioSession.Create(LAccepted);
    LSession.Select('workspace');
    LState := Default(TNyxStudioViewState);
    LShell := BuildNyxStudioView(LSession, LState, nil);
    LShell.Find(NyxStudioViewportMinimumID).Configure.Value(900);
    LShell.Find(NyxStudioViewportMaximumID).Configure.Value(1200);
    Check(CaptureNyxViewportInspector(LSession, LShell.Find(NyxStudioViewportApplyID),
      LShell.Pages[0], LEdit), 'Nyx-built Inspector captures an independent responsive intent');
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LRequest := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'Independent paired processor admits the Inspector rule');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAccepted),
      'Preparation cannot publish changes prematurely');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'Ordinary paired publication admits generated viewport source');
    Check(Pos('TNyxViewportWidth.Between(900, 1200)', LSession.ProjectSnapshot.Source) > 0,
      'Inspector and semantic tools use the same typed generator');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAccepted),
      'One Inspector Undo restores the exact paired design');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LFile.WriteBuffer(LAccepted.Source[1], Length(LAccepted.Source));
      finally
        LFile.Free;
      end;
    end;
    WriteLn('PASS ', LChecks, ' responsive contract/semantic/paired checks');
    {$else}
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' responsive checks';
    document.body.setAttribute('data-nyx-responsive', 'passed');
    {$endif}
  finally
    LPrepared := nil;
    LSchemas := nil;
    LShell.Free;
    LSession.Free;
    LAgent.Free;
    LCommon := nil;
    LCompact := nil;
    LNative := nil;
    LRow := nil;
    LRoot.Free;
    LDocument.Free;
  end;
end.
