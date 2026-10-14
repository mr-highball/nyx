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
program nyx_canvas_document_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.root.types, nyx.data, nyx.state, nyx.expressions,
  nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.source,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.declarations,
  nyx.canvas.workspace, nyx.studio.projects, nyx.studio.session
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Canvas document: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Edit exact portable source units without the native ANSI RTL's implicit
  codepage conversion. Surrounding supplementary text remains byte-for-byte. }
function ReplaceFirst(const AText, AExpected, AReplacement: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(AExpected, AText);

  if LPosition = 0 then
  begin
    raise Exception.Create('Qualification source edit did not find its expected text');
  end;
  Result := Copy(AText, 1, LPosition - 1) + AReplacement +
    Copy(AText, LPosition + Length(AExpected), MaxInt);
end;

function Fixture: TNyxDocument;
var
  LCard: INyxCard;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LStatus: INyxLabel;
  LDefinition: INyxCanvasDefinition;
  LLayer: TNyxCanvasLayerRef;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Canvas workshop';
    Result.AddPage(NewNyxPage('home'));
    Result.State.SetValue(NyxTextState('reply'), 'Start here');
    LCard := NewNyxCard('reply-card');
    LMemo := NewNyxMemo('reply-memo');
    LMemo.Text := 'Write a reply';
    LMemo.Binds.Value(NyxTextState('reply'));
    LCard.Add(LMemo);
    LButton := NewNyxButton('send-button');
    LButton.Text := 'Post reply';
    LCard.Add(LButton);
    { The newer canvas wire version must retain the preceding expression family
      alongside direct bindings, not merely round-trip its own registry. }
    LStatus := NewNyxLabel('reply-status');
    LStatus.Binds.Text(NyxTextExpression('Reply: ')
      .Append(NyxTextExpression(NyxTextState('reply')))).Done;
    LCard.Add(LStatus);
    Result.AddComponent(LCard);
    LLayer := NyxCanvasLayer('Controls 🐾');
    LDefinition := NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(800, 600, NyxCanvasPoint(-90.25, 10.5), 1.25,
        NyxCanvasZoomLimits(0.125, 16)))
      .WithGrid(NyxCanvasGrid(24.5, ncgsLines, ncsmGrid)
        .AtOrigin(NyxCanvasPoint(-3.75, 2.5)))
      .WithScaling(nccsLogicalSize)
      .WithLayer(LLayer)
      .WithLayer(NyxCanvasLayer('Hidden'), False, False)
      .WithItem(NyxCanvasItem('Reply 🐾'), NyxReusableRoot('reply-card'),
        NyxCanvasPlacement(LLayer, NyxCanvasBounds(-270.25, -140.5, 360.75, 300.125))
          .Pickable(False));
    Result.Canvases.Define(NyxCanvasRef('workshop 🐾'), LDefinition);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LDefinition: INyxCanvasDefinition;
  LRegistry: INyxCanvasDeclarations;
  LCanvas: INyxCanvasWorkspace;
  LOtherCanvas: INyxCanvasWorkspace;
  LPlan: INyxCanvasDefinition;
  LBefore: TNyxText;
  LSource: TNyxText;
  LChanged: TNyxText;
  LValue: TNyxDataValue;
  LFields: array of TNyxDataField;
  LCase: Integer;
  LIndex: Integer;
  LRefused: Boolean;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LDraftPair: TNyxProjectPair;
  LAppliedPair: TNyxProjectPair;
  LItem: TNyxCanvasDeclarationItem;
  LFit: TNyxCanvasCamera;
  LFrame: TNyxCanvasControlFrame;
  {$ifndef PAS2JS}
  LDirectory: TNyxText;
  LStream: TFileStream;
  {$endif}
begin
  LDocument := nil;
  LCopy := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LDocument := Fixture;
    LBefore := TNyxCodec.Encode(LDocument);
    LValue := TNyxDataValue.ParseJSON(LBefore);
    Check(LValue.Field('version').AsInteger = 13, 'canvas registry selects explicit design version');
    LCopy := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCopy) = LBefore, 'exact full design codec round trip');
    Check(LCopy.HasComputedBindings, 'version 13 retains the preceding typed expression family');
    LDefinition := LCopy.Canvases.Definition(NyxCanvasRef('workshop 🐾'));
    Check((LDefinition.ItemCount = 1) and (LDefinition.LayerCount = 2) and
      (LDefinition.Camera.WorldCenter.X = -90.25) and
      (LDefinition.Grid.Spacing = 24.5) and (LDefinition.Scaling = nccsLogicalSize),
      'saved geometry and closed policies retain exact values');
    LItem := LDefinition.Items[0];
    Check((LItem.Item.Name = TNyxText('Reply 🐾')) and
      (LItem.Content.Kind = nrReusable) and (LItem.Content.Name = 'reply-card') and
      not LItem.Placement.IsPickable and not LDefinition.Layers[1].IsVisible,
      'saved references, supplementary names and independent flags survive');
    LPlan := LDefinition.WithLayer(NyxCanvasLayer('Extra'));
    Check((LDefinition.LayerCount = 2) and (LPlan.LayerCount = 3),
      'fluent plans retain independent arrays on both targets');
    LItem.Placement := LItem.Placement.Visible(False);
    Check(LDefinition.Items[0].Placement.IsVisible, 'returned placement metadata is detached');
    LRegistry := LCopy.Canvases;
    FreeAndNil(LCopy);
    Check(LRegistry.Definition(NyxCanvasRef('workshop 🐾')).ItemCount = 1,
      'retained registry survives document retirement without root backreference');
    LCopy := LDocument.Clone;
    LCopy.Canvases.Remove(NyxCanvasRef('workshop 🐾'));
    Check((LDocument.Canvases.Count = 1) and (LCopy.Canvases.Count = 0),
      'document clones own independent definition registries');
    FreeAndNil(LCopy);
    LCanvas := NewNyxCanvasWorkspace('first', LDocument, NyxCanvasRef('workshop 🐾'));
    LOtherCanvas := NewNyxCanvasWorkspace('second', LDocument, NyxCanvasRef('workshop 🐾'));
    Check((LCanvas.Scene.Count = 1) and (LCanvas.Scene.LayerCount = 2) and
      (LCanvas.Grid.Origin.X = -3.75), 'saved definition reconstructs public runtime workspace');
    Check((LCanvas.Scene.Control(NyxCanvasItem('Reply 🐾')).Node <>
      LOtherCanvas.Scene.Control(NyxCanvasItem('Reply 🐾')).Node) and
      (LCanvas.Scene.Control(NyxCanvasItem('Reply 🐾')).Node <>
        LDocument.Components[0]), 'runtime scenes independently own template clones');
    LCanvas.Scene.Control(NyxCanvasItem('Reply 🐾')).Node.Find('reply-memo')
      .SetProp('text', 'Edited runtime caption');
    Check((LOtherCanvas.Scene.Control(NyxCanvasItem('Reply 🐾')).Node.Find('reply-memo')
      .Prop('text') = 'Write a reply') and (TNyxCodec.Encode(LDocument) = LBefore),
      'runtime descriptor edits preserve siblings and authored source');
    LFit := FitNyxCanvasScene(NyxCanvasCamera(374, 600, NyxCanvasPoint(0, 0), 2),
      LCanvas.Scene.Snapshot, nccsLogicalSize);
    LFrame := NyxCanvasControlFrame(LFit, LDefinition.Items[0].Placement.Bounds, nccsLogicalSize);
    Check((LFrame.Left >= 0) and (LFrame.Left + LFrame.Width <= 375),
      'logical-size fit centers the actual fixed allocation in a narrow viewport');
    LCanvas.Scene.AddControl(NyxCanvasItem('Fit probe'),
      NyxCanvasPlacement(NyxCanvasLayer('Controls 🐾'), NyxCanvasBounds(600, 500, 150, 100)),
      NewNyxMemo('fit-probe-memo'));
    LFit := FitNyxCanvasScene(NyxCanvasCamera(374, 600, NyxCanvasPoint(0, 0)),
      LCanvas.Scene.Snapshot, nccsLogicalSize);
    for LIndex := 0 to 1 do
    begin
      LFrame := NyxCanvasControlFrame(LFit,
        LCanvas.Scene.Snapshot.Items[LIndex].Placement.Bounds, nccsLogicalSize);
      Check((LFrame.Left >= 0) and (LFrame.Top >= 0) and
        (LFrame.Left + LFrame.Width <= 375) and (LFrame.Top + LFrame.Height <= 601),
        'logical-size grouped fit retains every actual extent ' + IntToStr(LIndex));
    end;
    LCanvas.Scene.Remove(NyxCanvasItem('Fit probe'));
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NewNyxCanvasDefinition', LSource) > 0) and
      (Pos('.WithCamera(', LSource) > 0) and (Pos('.WithItem(', LSource) > 0) and
      (Pos('NyxCanvasDefinitionFromData', LSource) = 0),
      'generated Pascal uses crafted typed canvas construction, never a JSON dump');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'source admission reconstructs full canvas document');
    Check(LWorkspace.Render(LParsed) = LSource, 'unchanged source preserves exact generated bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('send-button');
    LSession.SetProperty('text', 'Send reply');
    Check(LSession.CanUndo, 'ordinary paired control editing retains saved canvases');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'paired Undo restores exact canvas design and Pascal');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'paired Redo restores the exact changed canvas pair');
    LChanged := ReplaceFirst(LAfter.Source, '24.5', '32.75');
    LSession.SetSourceDraft(LChanged);
    LDraftPair := LSession.ProjectSnapshot;
    LRefused := False;
    try
      LSession.PrepareSourceEditRequest(LChanged + #10, 1);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) =
      EncodeNyxProject(LDraftPair)), 'pending canvas source retains the entire pair and buffer');
    LSession.ApplySourceDraft;
    Check(not LSession.SourceDraftPending and
      (LSession.Document.Canvases.Definition(NyxCanvasRef('workshop 🐾')).Grid.Spacing = 32.75),
      'ordinary source application publishes typed canvas settings');
    Check(LDocument.Canvases.Definition(NyxCanvasRef('workshop 🐾')).Grid.Spacing = 24.5,
      'accepted source application preserves the independent original definition');
    LAppliedPair := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'canvas source Undo restores the exact previous accepted pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAppliedPair),
      'canvas source Redo restores the exact applied pair');
    FreeAndNil(LSession);
    LValue := TNyxDataValue.ParseJSON(LBefore);
    SetLength(LFields, LValue.Count);
    for LIndex := 0 to LValue.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LValue.Key(LIndex), LValue.Field(LValue.Key(LIndex)));

      if LValue.Key(LIndex) = 'version' then
      begin
        LFields[LIndex] := NyxField('version', NyxData(12));
      end;
    end;
    LCopy := TNyxCodec.Decode(NyxObject(LFields).ToJSON);
    Check((LCopy.Canvases.Count = 0) and LCopy.Extensions.Has(NyxExtension('canvases')),
      'older opaque canvas data retains extension meaning instead of implicit promotion');
    LRefused := False;
    try
      LCopy.Canvases.Define(NyxCanvasRef('workshop 🐾'), LDefinition);
      LCopy.Validate;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'typed canvas promotion refuses opaque extension collision');
    FreeAndNil(LCopy);
    for LCase := 0 to 7 do
    begin
      LRefused := False;
      try
        case LCase of
          0: LDefinition.WithLayer(NyxCanvasLayer('Controls 🐾'));
          1: LDefinition.WithItem(NyxCanvasItem('Reply 🐾'), NyxReusableRoot('reply-card'),
            NyxCanvasPlacement(NyxCanvasLayer('Controls 🐾'), NyxCanvasBounds(0, 0, 10, 10)));
          2: LDefinition.WithItem(NyxCanvasItem('new'), NyxReusableRoot('reply-card'),
            NyxCanvasPlacement(NyxCanvasLayer('missing'), NyxCanvasBounds(0, 0, 10, 10)));
          3: LDocument.Canvases.Define(NyxCanvasRef('bad'), nil);
          4: NyxCanvasDefinitionFromData(NyxObject([]));
          5: NyxCanvasDeclarationsFromData(NyxArray([NyxObject([
            NyxField('name', NyxData('same')), NyxField('definition', LDefinition.ToData)]),
            NyxObject([NyxField('name', NyxData('same')),
              NyxField('definition', LDefinition.ToData)])]));
          6:
            begin
              LCopy := LDocument.Clone;
              LCopy.RemoveRoot(NyxReusableRoot('reply-card'));
              LCopy.Validate;
            end;
          7:
            begin
              LChanged := ReplaceFirst(LSource, 'nccsLogicalSize', '''logical-size''');
              LParsed := TNyxSourceWorkspace.PrepareDraft(LChanged, LWorkspace);
            end;
        end;
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
        'invalid typed/source/wire candidate preserves original ' + IntToStr(LCase));
      FreeAndNil(LCopy);
      FreeAndNil(LParsed);
      FreeAndNil(LWorkspace);
    end;
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      LStream := TFileStream.Create(LDirectory + 'nyx.generated.view.pas', fmCreate);
      try
        LStream.WriteBuffer(LSource[1], Length(LSource));
      finally
        LStream.Free;
      end;
      LStream := TFileStream.Create(LDirectory + 'design.nyx', fmCreate);
      try
        LStream.WriteBuffer(LBefore[1], Length(LBefore));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-canvas-document', 'passed');
    document.body.setAttribute('data-canvas-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' canvas document/source/history checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-document', 'failed');
      document.body.setAttribute('data-canvas-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
