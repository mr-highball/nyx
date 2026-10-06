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
unit nyx.test.move;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Shared policy and independent processor admission. Actual target gestures are
  qualified separately by the semantic-source Studio consumers. }
function RunNyxMoveChecks: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.data, nyx.schema, nyx.presentations, nyx.designer.guides, nyx.designer.move,
  nyx.layout.constraints, nyx.studio.edits, nyx.studio.session, nyx.studio.projects;

function RunNyxMoveChecks: Integer;
var
  LChecks: Integer;
  LContext: TNyxAlignmentContext;
  LPolicy: TNyxMovePolicy;
  LPosition, LStart: TNyxMovePosition;
  LGuide: TNyxAlignmentGuide;
  LDocument: TNyxDocument;
  LPanel: INyxPanel;
  LSession: TNyxStudioSession;
  LBefore, LAfter: TNyxProjectPair;
  LRequest: TNyxStudioDesignRequest;
  LIntent: TNyxStudioDesignEdit;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LData, LEdit: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex, LCount: Integer;
  LRefused: Boolean;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create('Move checks: ' + AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LStart := NyxMovePosition(20, 30);
  LContext := NyxAlignmentContext(NyxGuideBox(20, 30, 200, 120),
    NyxGuideBox(0, 0, 600, 440), NyxControl('workspace')).Positions(True)
    .Peer(NyxControl('other-editor'), NyxGuideBox(240, 70, 180, 120));
  LPolicy := NyxMovePolicy.Guides(LContext);
  LPosition := LPolicy.Adjust(LStart, 23, 36);
  Check(LPosition.SamePosition(NyxMovePosition(40, 70)), 'trailing/leading edges and tops snap together');
  Check((LPosition.HorizontalGuide.Reference.ID = 'other-editor') and
    (LPosition.VerticalGuide.Reference.ID = 'other-editor'), 'copied explanations retain exact reference');
  Check((LPosition.HorizontalGuide.OwnerBox.Left = 40) and
    (LPosition.HorizontalGuide.OwnerBox.Top = 70), 'both axes rebase guide segments to the final proposed face');
  Check(LPosition.HorizontalGuide.Segment(0, 200, 120).Left = 240, 'line aligns the moving trailing face');
  Check(LPolicy.Adjust(LStart, 23, 36, True).SamePosition(NyxMovePosition(43, 66)), 'bypass retains exact unsnapped origin');
  Check(LPolicy.Adjust(LStart, 0, 0).SamePosition(LStart), 'tap preserves an off-grid accepted origin');
  Check(LPolicy.Adjust(LStart, 23, 0).Top = 30, 'untouched axis retains its exact origin');
  Check(LPolicy.Guides(LContext.Positions(False)).Adjust(LStart, 23, 36)
    .SamePosition(NyxMovePosition(40, 64)), 'flow contexts provide only the requested grid policy');
  Check(LPolicy.Adjust(LStart, -1E100, 1E100).SamePosition(NyxMovePosition(0, 100000)), 'wide deltas clamp before conversion');
  Check(LPolicy.Bounds(Default(TNyxSizeRange).Minimum(45),
    Default(TNyxSizeRange).Maximum(67)).Adjust(LStart, 23, 41)
    .SamePosition(NyxMovePosition(45, 67)), 'off-grid origin bounds win exactly');
  Check(LContext.SnapPosition(ngaWidth, 198, 0, 100000, LGuide) = 200,
    'parent centering keeps the captured owner size');
  Check(LGuide.Kind = ngkCenter, 'centering is a distinct typed explanation');
  Check(LPolicy.Grid(16).Adjust(LStart, 40, 100, False, True)
    .SamePosition(NyxMovePosition(64, 128)), 'keyboard-style movement bypasses sticky guides');

  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('home'));
    LPanel := NewNyxPanel('workspace');
    LPanel.Configure.Layout(nlAbsolute).Width(600).Height(440).Padding(0).Done;
    LDocument.Find('home').Add(LPanel);
    LPanel.Add(NewNyxMemo('notes-editor').Configure.Text('Notes').Value('English text')
      .Left(20).Top(30).Width(200).Height(120).Done);
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  LSession := TNyxStudioSession.Create(LBefore);
  LSchemas := CaptureNyxSchemas;
  try
    LSession.Select('notes-editor');
    LRequest := LSession.PrepareDesignRequest(LSession.CapturePosition(
      NyxPositionControl(NyxControl('notes-editor'), LPosition), LSession.CommandContext), LSchemas.Revision);
    LData := LRequest.ToData;
    Check((LData.Field('version').AsInteger = 9) and (LData.Field('edit').Count = 15), 'new intent has an exact versioned worker shape');
    Check(LRequest.SameRequest(ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LData.ToJSON))), 'typed position survives worker round trip');
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'independent processor admits the typed position');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied, 'paired publication admits one position command');
    LAfter := LSession.ProjectSnapshot;
    Check((LSession.Selected.Prop('left') = '40') and (LSession.Selected.Prop('top') = '70') and
      (Pos('.Left(40)', LAfter.Source) > 0) and (Pos('.Top(70)', LAfter.Source) > 0), 'processor publishes typed source and both origin axes');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore), 'one Undo restores the exact pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'one Redo restores the exact pair');
    { Reconstruct the preceding exact v8 shape; its reader must refuse the
      appended action rather than acquiring new vocabulary by enum ordinal. }
    LEdit := LData.Field('edit');
    SetLength(LFields, LEdit.Count - 1);
    LCount := 0;
    for LIndex := 0 to LEdit.Count - 1 do
    begin

      if LEdit.Key(LIndex) <> 'position' then
      begin
        LFields[LCount] := NyxField(LEdit.Key(LIndex), LEdit.Field(LEdit.Key(LIndex)));
        Inc(LCount);
      end;
    end;
    LData := NyxObject([NyxField('version', NyxData(8)), NyxField('owner', LData.Field('owner')),
      NyxField('generation', LData.Field('generation')), NyxField('nextId', LData.Field('nextId')),
      NyxField('schemaRevision', LData.Field('schemaRevision')), NyxField('pair', LData.Field('pair')),
      NyxField('edit', NyxObject(LFields))]);
    LRefused := False;
    try
      ReadNyxStudioDesignRequest(LData);
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'preceding tickets cannot smuggle a new position action');
    { The deployed review/preview helpers still emit v8. Qualify their ordinary
      property path through the new processor before overlaying the new worker;
      rejecting new actions must not reject supported historical vocabulary. }
    LIntent := Default(TNyxStudioDesignEdit);
    LIntent.Action := sdaProperty;
    LIntent.Selection := 'notes-editor';
    LIntent.View := 'home';
    LIntent.Name := NyxAttributeName(atText);
    LIntent.Value := 'An earlier helper caption';
    LRequest := LSession.PrepareDesignRequest(LIntent, LSchemas.Revision);
    LData := LRequest.ToData;
    LEdit := LData.Field('edit');
    LCount := 0;
    for LIndex := 0 to LEdit.Count - 1 do
    begin

      if LEdit.Key(LIndex) <> 'position' then
      begin
        LFields[LCount] := NyxField(LEdit.Key(LIndex), LEdit.Field(LEdit.Key(LIndex)));
        Inc(LCount);
      end;
    end;
    LData := NyxObject([NyxField('version', NyxData(8)), NyxField('owner', LData.Field('owner')),
      NyxField('generation', LData.Field('generation')), NyxField('nextId', LData.Field('nextId')),
      NyxField('schemaRevision', LData.Field('schemaRevision')), NyxField('pair', LData.Field('pair')),
      NyxField('edit', NyxObject(LFields))]);
    Check(LRequest.SameRequest(ReadNyxStudioDesignRequest(LData)),
      'exact version-eight property tickets remain compatible');
    LPrepared := PrepareNyxStudioDesign(ReadNyxStudioDesignRequest(LData), LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'earlier helper ticket reaches independent preparation');
    LSession.Document.Find('workspace').Configure.Layout(nlRow).Done;
    LRefused := False;
    try
      ValidateNyxPositionOwner(LSession.Document, NyxControl('notes-editor'));
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'flow authoring refuses free positioning');
    LSession.Document.Find('workspace').Configure.Layout(nlAbsolute).Done;
    LSession.Selected.Configure.ForPlatform(npfBrowser).Left(8).Done;
    LRefused := False;
    try
      ValidateNyxPositionOwner(LSession.Document, NyxControl('notes-editor'));
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'target origin refuses ambiguous portable movement');
  finally
    LPrepared := nil;
    LSchemas := nil;
    LSession.Free;
  end;
  Result := LChecks;
end;

end.
