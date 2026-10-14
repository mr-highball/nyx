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


program nyx_tool_canvas_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.source, nyx.schema, nyx.controls, nyx.tool.ports, nyx.flow.types, nyx.binding.types,
  nyx.canvas.types, nyx.canvas.declarations, nyx.view.behavior, nyx.composition,
  nyx.studio.projects, nyx.studio.session, nyx.test.tool.canvas
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tool Canvas contract: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure Verify;
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LCanvas: INyxToolCanvas;
  LOther: INyxToolCanvas;
  LPorts: INyxToolControlPorts;
  LPort: TNyxToolControlPort;
  LAgain: TNyxToolControlPort;
  LSource: TNyxText;
  LEncoded: TNyxText;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LRoot: TNyxNode;
  LIndex: Integer;
  LInputs: Integer;
  LSignals: Integer;
  LValues: Integer;
  LFailed: Boolean;
begin
  LDocument := ToolCanvasDocument;
  LDecoded := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    ValidateNyxDocumentProperties(LDocument);
    LCanvas := NewNyxToolCanvas('independent-tool').WithScene(NyxCanvasRef('tools'));
    LOther := NewNyxToolCanvas('other-tool');
    Check((LCanvas.Node.Kind = NyxKindName(nkToolCanvas)) and
      (LCanvas.Scene.Name = 'tools'), 'specialized managed factory retains a typed scene');
    LCanvas.WorkspaceCanvas.Configure.Height(620).Done;
    Check(LOther.WorkspaceCanvas.Node.Prop('height') <> '620',
      'named parts belong independently to each factory instance');
    Check((LCanvas.ToolsCard <> nil) and (LCanvas.ToolsPortHostScroll <> nil),
      'public recipe exposes reusable workspace and inspector hosts');
    LRoot := LDocument.Find('editor-template');
    Check(NyxViewRequiresBehavior(LDocument, LRoot),
      'embedded executable declarations require explicit application authority');
    LEncoded := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LEncoded);
    Check(TNyxCodec.Encode(LDecoded) = LEncoded, 'complete scene/routes/document wire round trip');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('INyxToolCanvas', LSource) > 0) and
      (Pos('NewNyxToolCanvas(', LSource) > 0), 'generated source keeps specialized types/factories');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LEncoded, 'typed source reconstructs the exact admitted document');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source regeneration preserves exact bytes');
    LPair := NyxProjectPair(LEncoded, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('tool-canvas');
    LSession.SetProperty('padding', '7');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'ordinary compound edit Undo restores the complete design/source pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'ordinary compound edit Redo restores the exact changed pair');

    LPorts := NyxToolControlPorts(NyxCanvasItem('Editor'), LDocument.Find('source-memo'), LDocument);
    LInputs := 0;
    LSignals := 0;
    LValues := 0;
    for LIndex := 0 to LPorts.Count - 1 do
    begin
      LPort := LPorts.Port(LIndex);
      Check((LPort.Item.Name = 'Editor') and (LPort.Control.ID = 'source-memo'),
        'port addresses retain exact item/control identity');

      if LPort.Role = ntprBoundState then
      begin
        Inc(LInputs);
        Check((LPort.Direction = nfdInput) and (LPort.ValueType = nftText) and
          (LPort.StateName = 'source'), 'direct state projection advertises its exact writable family');
      end
      else if LPort.Role = ntprEventSignal then
      begin
        Inc(LSignals);
        Check((LPort.Direction = nfdOutput) and (LPort.ValueType = nftSignal),
          'invocation signal remains distinct from data');
      end
      else if LPort.Role = ntprEventValue then
      begin
        Inc(LValues);
        Check((LPort.Direction = nfdOutput) and (LPort.ValueType = nftText) and
          LPort.PayloadOptional, 'optional accepted scalar has an explicit typed output');
      end;
      Check(LPort.ToData.Field('control').AsText = 'source-memo',
        'port data is queryable without executable host authority');
    end;
    Check((LInputs = 1) and (LSignals > 1) and (LValues > 0),
      'memo exposes distinct event and existing bound-state ports');
    LPort := LPorts.Port(0);
    LAgain := LPorts.Port(0);
    LPort.Title := 'Changed caller copy';
    LPort.Item := NyxCanvasItem('Different item');
    Check((LPorts.Port(0).Title = LAgain.Title) and
      (LPorts.Port(0).Item.Name = 'Editor'), 'returned port records cannot mutate the earlier snapshot');
    Check(LPorts.Matches(0, 'invocation'), 'creator intent participates in discovery');
    LFailed := False;
    try
      LPorts.Port(LPorts.Count);
    except
      on E: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'bounded port access refuses an out-of-window index');
    LDocument.Find('source-memo').SetBinding(TNyxBindingSpec.Clear(bpValue));
    Check(LPorts.Count > 0, 'immutable port snapshot survives later source mutation');
    LPorts := NyxToolControlPorts(NyxCanvasItem('Editor'), LDocument.Find('source-memo'), LDocument);
    LInputs := 0;
    for LIndex := 0 to LPorts.Count - 1 do
    begin

      if LPorts.Port(LIndex).Role = ntprBoundState then
      begin
        Inc(LInputs);
      end;
    end;
    Check(LInputs = 0, 'cleared projection does not fabricate a writable state port');
    FreeAndNil(LDecoded);
    LDecoded := TNyxCodec.Decode(LEncoded);
    LDecoded.Find('tool-canvas').Part(NyxPart('workspace')).SetProp('projection-kind', 'label');
    LFailed := False;
    try
      ValidateNyxDocumentProperties(LDecoded);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'typed Tool Canvas admission refuses a replaced required workspace role');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LEncoded);
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Verify;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-tool-contract', 'passed');
    document.body.setAttribute('data-tool-contract-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' Tool Canvas/port/source/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-tool-contract', 'failed');
      document.body.setAttribute('data-tool-contract-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
