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

unit nyx.test.resize;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.projects;

{ An independent English project. Exported source is compiled unchanged by the
  ordinary control consumer; neither journey contacts a listener/user project. }
function NyxResizeFixture: TNyxProjectPair;
function RunNyxResizeJourney(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.schema, nyx.designer.resize, nyx.designer.guides, nyx.layout.constraints,
  nyx.studio.edits, nyx.studio.session, nyx.studio.agents;

function NyxResizeFixture: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LRow: INyxRow;
  LSource: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Room for ideas';
    LDocument.AddPage(NewNyxPage('home').Configure.Padding(16).Gap(12).Done);
    LDocument.Find('home').Add(NewNyxHeading('title').Configure.Text('Make room for ideas').Done);
    LRow := NewNyxRow('notes-row');
    LRow.Configure.Width(480).Gap(12).Padding(0).Wrap(nfwNoWrap);
    LRow.Add(NewNyxMemo('notes-editor').Configure.Text('Notes')
      .Value('Keep this English draft.').Width(200).Height(120).Flex(1)
      .MinimumWidth(100).MaximumWidth(400).MinimumHeight(40).MaximumHeight(240).Done);
    LRow.Add(NewNyxMemo('other-editor').Configure.Text('Companion notes')
      .Value('This control stays independent.').Width(160).Height(120).Done);
    LDocument.Find('home').Add(LRow);
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := StringReplace(LSource, 'implementation',
      '{ Keep this handwritten English resize helper comment. }' + #10 + 'implementation', []);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LRow := nil;
    LDocument.Free;
  end;
end;

{ Strict previous-shape replay. Earlier versions must not acquire appended
  action vocabulary simply because the current reader knows that enum ordinal. }
function EarlierRequest(const AData: TNyxDataValue; AVersion: Integer): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LEdit: TNyxDataValue;
  LIndex: Integer;
  LCount: Integer;
begin
  LEdit := AData.Field('edit');
  SetLength(LFields, LEdit.Count);
  LCount := 0;
  for LIndex := 0 to LEdit.Count - 1 do
  begin

    if (LEdit.Key(LIndex) <> 'resize') and (LEdit.Key(LIndex) <> 'presentation') and
      ((AVersion <> 5) or (LEdit.Key(LIndex) <> 'placement')) then
    begin
      LFields[LCount] := NyxField(LEdit.Key(LIndex), LEdit.Field(LEdit.Key(LIndex)));
      Inc(LCount);
    end;
  end;
  SetLength(LFields, LCount);
  LEdit := NyxObject(LFields);
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = 'version' then
    begin
      LFields[LIndex] := NyxField('version', NyxData(AVersion));
    end
    else if AData.Key(LIndex) = 'edit' then
    begin
      LFields[LIndex] := NyxField('edit', LEdit);
    end;
  end;
  Result := NyxObject(LFields);
end;

function RunNyxResizeJourney(out APair: TNyxProjectPair): Integer;
var
  LChecks: Integer;
  LSize: TNyxResizeSize;
  LPreview: TNyxResizePreview;
  LPoint: TNyxResizePoint;
  LPolicy: TNyxResizePolicy;
  LCopy: TNyxResizePolicy;
  LRefused: Boolean;
  LAgent: TNyxAgentSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LRevision: Integer;
  LQuery: TNyxDataValue;
  LChange: TNyxResizeChange;
  LSession: TNyxStudioSession;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LWire: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LGuides, LGuideCopy: TNyxAlignmentContext;
  LGuide: TNyxAlignmentGuide;
  LBox: TNyxGuideBox;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Resize: ' + AReason);
    end;
    Inc(LChecks);
  end;

  procedure History(const ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('resize-history-' + IntToStr(LRevision))),
      NyxField('direction', NyxData(ADirection))]));
    LRevision := LAgent.Revision;
  end;

begin
  LChecks := 0;
  LSize := NyxResizeSize(200, 120);
  Check(not Default(TNyxResizePreview).Active, 'default presentation is an explicit clear');
  LPreview := NyxResizePreview(NyxControl('notes-editor'), LSize);
  Check(LPreview.Active and (LPreview.Control.ID = 'notes-editor') and
    LPreview.Size.SameSize(LSize), 'preview carries owned typed identity and logical size');
  LRefused := False;
  try
    NyxResizePreview(NyxControl('notes-editor'), Default(TNyxResizeSize));
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'undefined geometry refuses before any presentation');
  LPolicy := NyxResizePolicy;
  { Geometry comes from adapters in the physical journeys. These portable
    checks qualify deterministic numerical selection, copied ownership and
    bounds independently of either widgetset's allocation choices. }
  LGuides := NyxAlignmentContext(NyxGuideBox(20, 30, 200, 120),
    NyxGuideBox(0, 0, 600, 400), NyxControl('layout'));
  LGuideCopy := LGuides.Peer(NyxControl('reference'), NyxGuideBox(300, 60, 217, 143));
  Check((LGuides.PeerCount = 0) and (LGuideCopy.PeerCount = 1),
    'fluent guide copies own independent peer arrays on both compilers');
  LGuides := LGuideCopy;
  LGuideCopy := LGuides.Peer(NyxControl('second'), NyxGuideBox(400, 200, 220, 140));
  Check((LGuides.PeerCount = 1) and (LGuideCopy.PeerCount = 2),
    'appending preserves the populated baseline');
  LCopy := LPolicy.Guides(LGuides);
  LPreview := NyxResizePreview(NyxControl('notes-editor'), LCopy.Adjust(LSize, nraBoth, 14, 22));
  Check(LPreview.Size.SameSize(NyxResizeSize(217, 143)) and
    (LPreview.Size.WidthGuide.Kind = ngkEqualSize) and
    (LPreview.Size.HeightGuide.Kind = ngkEqualSize), 'nearby matching dimensions win before grid');
  Check((LPreview.Size.WidthGuide.Reference.ID = 'reference') and
    (Pos('reference', LPreview.Size.WidthGuide.Caption) > 0), 'guides explain exact reference identity');
  LBox := LPreview.Size.WidthGuide.Segment(1, 217, 143);
  Check(LBox.Defined and (LBox.Left = 300) and (LBox.Top = 54) and (LBox.Width = 217),
    'matching width paints an honest peer measurement bar in the parent plane');
  Check(LCopy.Adjust(LSize, nraBoth, 14, 22, True).SameSize(NyxResizeSize(214, 142)),
    'Alt bypasses guides and grid together');
  Check(LCopy.Adjust(LSize, nraBoth, 14, 22, False, True).SameSize(NyxResizeSize(216, 144)),
    'keyboard bypass avoids sticky guides while retaining grid policy');
  Check(LCopy.Adjust(LSize, nraBoth, 0, 0).SameSize(LSize) and
    (LCopy.Adjust(LSize, nraBoth, 0, 0).WidthGuide.Kind = ngkNone), 'no movement invents no guide or edit');
  LCopy := LCopy.Bounds(NyxSizeConstraints.MaximumWidth(215));
  Check((LCopy.Adjust(LSize, nraWidth, 14, 0).Width = 215) and
    (LCopy.Adjust(LSize, nraWidth, 14, 0).WidthGuide.Kind = ngkNone), 'bounds refuse an invalid nearby guide');
  Check(LGuideCopy.Snap(ngaWidth, 219, 218, 225, LGuide) = 220,
    'a nearer invalid candidate does not mask an eligible bounded peer');
  Check(LGuideCopy.Snap(ngaWidth, 218.5, 0, 1000, LGuide) = 217,
    'equal distances retain stable peer order');
  LGuides := NyxAlignmentContext(NyxGuideBox(20, 30, 200, 120),
    NyxGuideBox(0, 0, 600, 400), NyxControl('layout'))
    .Peer(NyxControl('reference'), NyxGuideBox(250, 100, 90, 40)).Positions(True);
  Check((LGuides.Snap(ngaWidth, 318, 0, 1000, LGuide) = 320) and
    (LGuide.Kind = ngkEdge), 'absolute layout snaps the positive edge');
  LBox := LGuide.Segment(0, 320, 120);
  Check((LBox.Left = 340) and (LBox.Width = 1), 'edge line uses the reference edge, not its size');
  Check((LGuides.Snap(ngaWidth, 547, 0, 1000, LGuide) = 550) and
    (LGuide.Kind = ngkCenter), 'absolute layout can align centers');
  Check(not LGuides.SameContext(LGuides.Positions(False)) and
    not LGuides.SameContext(LGuides.Tolerance(5)) and LGuides.SameContext(LGuides),
    'release comparison observes geometry policy without retaining model objects');
  Check((LGuides.Positions(False).Snap(ngaWidth, 318, 0, 1000, LGuide) = 318) and
    (LGuide.Kind = ngkNone), 'flow layouts do not promise unstable position alignment');
  LRefused := False;
  try
    NyxGuideBox(NaN, 0, 100, 100);
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nonfinite captured geometry refuses');
  LRefused := False;
  try
    LGuides.Peer(NyxControl('reference'), NyxGuideBox(0, 0, 10, 10));
  except
    on EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'duplicate reference refuses without changing the snapshot');
  LPoint := NyxResizePoint(-12.5, 204.25);
  Check(LPoint.Defined and (LPoint.X = -12.5) and (LPoint.Y = 204.25),
    'stable-plane coordinates preserve signed fractional values');
  LRefused := False;
  try
    NyxResizePoint(NaN, 0);
  except
    on LException: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'non-finite coordinates refuse before gesture capture');
  Check(LPolicy.Adjust(LSize, nraBoth, 13, 21).SameSize(NyxResizeSize(216, 144)),
    'nearest grid is deterministic on both dimensions');
  Check(LPolicy.Adjust(LSize, nraWidth, 4, 30).SameSize(NyxResizeSize(208, 120)),
    'grid ties increase, unchanged axis is exact');
  Check(LPolicy.Adjust(LSize, nraBoth, 13, 21, True).SameSize(NyxResizeSize(213, 141)),
    'Alt-style bypass retains unrounded dimensions');
  LCopy := LPolicy.Grid(16).KeyboardStep(16);
  Check((LPolicy.GridSize = 8) and (LCopy.GridSize = 16) and (LCopy.KeyStep = 16),
    'fluent policies preserve the original value');
  LCopy := LPolicy.Bounds(NyxSizeConstraints.MinimumWidth(205).MaximumHeight(139));
  Check(LCopy.Adjust(LSize, nraBoth, -1000, 1000).SameSize(NyxResizeSize(205, 139)),
    'exact off-grid bounds win after snapping');
  Check(LCopy.Adjust(LSize, nraHeight, -1000, -1000).SameSize(NyxResizeSize(200, 0)),
    'unchanged axis remains outside a bound when only the other axis changes');
  Check(LPolicy.Adjust(LSize, nraBoth, -1E100, 1E100)
    .SameSize(NyxResizeSize(0, MaximumNyxLayoutBound)), 'wide deltas clamp before integer conversion');
  LRefused := False;
  try
    LPolicy.Grid(0);
  except
    on LException: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.GridSize = 8), 'invalid copied policy retains its baseline');
  LRefused := False;
  try
    LPolicy.Adjust(Default(TNyxResizeSize), nraBoth, 0, 0);
  except
    on LException: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'uninitialized geometry refuses');
  LRefused := False;
  try
    LPolicy.Adjust(LSize, nraBoth, NaN, 0);
  except
    on LException: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'non-finite input refuses');

  LAgent := TNyxAgentSession.Create(NyxResizeFixture);
  try
    LQuery := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    LRevision := LQuery.Field('revision').AsInteger;
    LQuery := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('notes-editor')), NyxField('keys', NyxArray([
        NyxData('width'), NyxData('height'), NyxData('flex')])), NyxField('limit', NyxData(3))]));
    Check(LQuery.Defined, 'bounded semantic inspection precedes the edit');
    LBefore := LAgent.PreviewPair(LRevision, 'home');
    LChange := NyxResizeControl(NyxControl('notes-editor'), nraBoth, NyxResizeSize(240, 152));
    Check(TNyxResizeChange.FromData(TNyxDataValue.ParseJSON(LChange.ToData.ToJSON))
      .SameChange(LChange), 'typed grouped intent survives its exact wire boundary');
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('resize-notes')),
      NyxField('operations', NyxArray([LChange.Operation(True)]))]));
    LRevision := LAgent.Revision;
    LAfter := LAgent.PreviewPair(LRevision, 'home');
    Check((Pos('.Width(240)', LAfter.Source) > 0) and
      (Pos('.Height(152)', LAfter.Source) > 0) and (Pos('INyxMemo', LAfter.Source) > 0),
      'semantic dimensions generate specialized readable Pascal');
    Check(Pos('handwritten English resize helper comment.', LAfter.Source) > 0,
      'semantic edits retain handcrafted source');
    History('undo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LBefore),
      'one semantic Undo restores both dimensions and Pascal');
    History('redo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LAfter),
      'one Redo restores the exact resize pair');
    APair := LAfter;
  finally
    LAgent.Free;
  end;

  LSession := TNyxStudioSession.Create(APair);
  LSchemas := CaptureNyxSchemas;
  try
    LSession.Select('notes-editor');
    LBefore := LSession.ProjectSnapshot;
    LEdit := LSession.CaptureResize(NyxResizeControl(NyxControl('notes-editor'),
      nraBoth, NyxResizeSize(272, 168)), LSession.CommandContext);
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    Check(LRequest.SameRequest(LWire), 'version seven owns a complete copied resize command');
    LPrepared := PrepareNyxStudioDesign(LWire, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'independent processor admits the grouped resize');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preparation leaves accepted geometry and Pascal unchanged');
    LPrepared := ReceiveNyxPreparedDesign(TNyxDataValue.ParseJSON(LPrepared.ToData.ToJSON),
      LRequest, LSchemas);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'isolated ordinary publication applies one resize');
    LAfter := LSession.ProjectSnapshot;
    Check((LSession.Selected.Prop('width') = '272') and
      (LSession.Selected.Prop('height') = '168') and (LSession.Selected.Prop('flex') = '0'),
      'row main-axis resize releases weight and sets both pixel dimensions');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'one ordinary Undo restores both files');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'ordinary Redo is exact');
    LRefused := False;
    try
      ReadNyxStudioDesignRequest(EarlierRequest(LRequest.ToData, 6));
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'version six cannot smuggle an appended resize action');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaProperty;
    LEdit.Selection := 'notes-editor';
    LEdit.View := 'home';
    LEdit.Name := NyxAttributeName(atText);
    LEdit.Value := 'An earlier caption edit';
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    Check(LRequest.SameRequest(ReadNyxStudioDesignRequest(EarlierRequest(LRequest.ToData, 6))),
      'exact version-six property tickets remain compatible');
    Check(LRequest.SameRequest(ReadNyxStudioDesignRequest(EarlierRequest(LRequest.ToData, 5))),
      'exact version-five property tickets remain compatible');

    LEdit := LSession.CaptureResize(NyxResizeControl(NyxControl('notes-editor'),
      nraHeight, NyxResizeSize(272, 176)), LSession.CommandContext);
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.SetTitle('Changed during the gesture');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale,
      'stale pair cannot publish prepared dimensions');
    LSession.LoadProject(LAfter);
    LRefused := False;
    try
      LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired captured intent cannot follow matching IDs through reload');
    LBefore := LSession.ProjectSnapshot;
    LRefused := False;
    try
      LSession.Resize(NyxResizeControl(NyxControl('home'), nraBoth, NyxResizeSize(300, 200)));
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'root sizing stays with its separate authoring contract');
    LSession.Select('notes-row');
    LSession.SetProperty(NyxPlatformKey(npfNativeLCL, atLayout), NyxLayoutName(nlColumn));
    LBefore := LSession.ProjectSnapshot;
    LRefused := False;
    try
      LSession.Resize(NyxResizeControl(NyxControl('notes-editor'), nraWidth, NyxResizeSize(280, 168)));
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'divergent effective parent flows cannot silently alter the other axis');
    LSession.Resize(NyxResizeControl(NyxControl('notes-editor'), nraWidth,
      NyxResizeSize(280, 168), npfNativeLCL));
    Check((LSession.Document.Find('notes-editor').Prop(
      NyxPlatformKey(npfNativeLCL, atWidth)) = '280') and
      (LSession.Document.Find('notes-editor').Prop('width') = '272'),
      'explicit target intent retains the portable dimension');
    APair := LAfter;
  finally
    LPrepared := nil;
    LSchemas := nil;
    LSession.Free;
  end;
  Result := LChecks;
end;

end.
