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
program nyx_diagram_lcl_benchmark;

{$mode delphi}{$H+}{$codepage utf8}

{ One fresh native process qualifies one complete mounted workload. The ordinary
  public Nyx editor owns chrome/canvas/content; this program supplies no alternate
  toolkit, service, recovery store or image capture. Three warmed samples separate
  synchronous publication/key dispatch from actual forwarded WM_PAINT calls.
  Forced painting does not measure compositor presentation or trusted hardware.
  Allocator samples/high-water cover this process, including setup and caches;
  they are not OS private bytes or an isolated per-view peak. }

uses
  Interfaces, Forms, Controls, StdCtrls, Classes, SysUtils, Math, LMessages,
  {$ifdef MSWINDOWS}Windows,{$endif}
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec, nyx.controls, nyx.state,
  nyx.diagram, nyx.diagram.content, nyx.diagram.editor, nyx.diagram.editor.lcl,
  nyx.canvas.geometry, nyx.canvas.types, nyx.canvas.lcl, nyx.drawing.lcl,
  nyx.benchmark.diagram;

type
  { Borrow a mounted key surface solely to dispatch its installed native hook.
    It creates/owns no widget and claims no physical keyboard input. }
  TKeyAccess = class(TWinControl);

  { One owned qualification session. Document/store/host outlive the mounted view.
    The paint spy borrows its exact drawing, forwards the original WindowProc and
    restores it before disconnection or object destruction. No source/project
    authority or global state survives this session. }
  TMountedBenchmark = class
  private
    FDocument: TNyxDocument;
    FState: TNyxState;
    FHost: TForm;
    FDescriptor: INyxDiagramEditor;
    FView: INyxLCLDiagramEditorView;
    FCanvas: INyxLCLCanvasWorkspaceView;
    FContent: INyxDiagramContentView;
    FPicture: TNyxLCLDrawing;
    FOriginalProc: TWndMethod;
    FPaints: Integer;
    FError: TNyxText;
    FBefore: TNyxText;
    FMemo: TMemo;
    FOriginalException: TExceptionEvent;
    function Memo: TMemo;
    function Picture: TNyxLCLDrawing;
    procedure WatchPaint(var AMessage: TLMessage);
    procedure PictureRetiring(ASender: TObject);
    procedure Failed(ASender: TObject; AError: Exception);
    procedure Pump;
    procedure Require(ACondition: Boolean; const AMessage: TNyxText);
    procedure ForcePaint;
    procedure CheckRetained;
    procedure DetachSpy;
  public
    constructor Create(AVertices, AWidth, AHeight: Integer);
    destructor Destroy; override;
    { Return bounded scalar receipts only after complete model, memo/caret,
      revision, paint and teardown ownership gates have passed. }
    function Measure: TNyxDataValue;
  end;

{$ifdef MSWINDOWS}
var
  GFrequency: Int64;
{$endif}

function Clock: Double;
{$ifdef MSWINDOWS}
var
  LCounter: Int64;
{$endif}
begin
  {$ifdef MSWINDOWS}
  QueryPerformanceCounter(LCounter);
  Result := LCounter / GFrequency * 1000;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

function FirstKind(ARoot: TNyxNode; AKind: TNyxKind): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if ARoot.Kind = NyxKindName(AKind) then
  begin
    Exit(ARoot);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    Result := FirstKind(ARoot.Children[LIndex], AKind);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

constructor TMountedBenchmark.Create(AVertices, AWidth, AHeight: Integer);
var
  LPlan: INyxDiagram;
  LNote: INyxColumn;
  LMemo: INyxMemo;
  LButton: INyxButton;
  LOptions: TNyxDiagramEditorOptions;
begin
  inherited Create;
  FOriginalException := Application.OnException;
  Application.OnException := Failed;
  FDocument := TNyxDocument.Create;
  FDocument.Title := 'Mounted diagram workload';
  FDocument.AddPage(NewNyxPage('home'));
  FDocument.State.SetValue(NyxTextState('note'), 'A note worth keeping');
  LNote := NewNyxColumn('Benchmark note');
  LNote.Configure.Padding(8).Gap(8).Done;
  LMemo := NewNyxMemo('note-editor').WithText('Notes');
  LMemo.Configure.Height(64).Done;
  LMemo.Binds.Value(NyxTextState('note'));
  LNote.Add(LMemo);
  LButton := NewNyxButton('note-action').WithText('Keep note');
  LNote.Add(LButton);
  FDocument.AddComponent(LNote);
  FDocument.Validate;
  FBefore := TNyxCodec.Encode(FDocument);
  FState := FDocument.State.Clone;
  LPlan := NyxBenchmarkDiagram(AVertices);
  LPlan := LPlan.WithVertex(LPlan.Vertex(0)
    .AtBounds(NyxCanvasBounds(-640, -240, 240, 180))
    .WithContent(NyxDiagramComponent(NyxComponent('Benchmark note'))));
  LOptions := NyxDiagramEditorOptions.ToolsExpanded(False).Nudge(1)
    .WithCamera(NyxCanvasCamera(AWidth, AHeight, LPlan.Vertex(0).Bounds.Center));
  FDescriptor := NewNyxDiagramEditor('mounted-benchmark', LPlan).WithOptions(LOptions);
  FHost := TForm.CreateNew(nil);
  FHost.Caption := 'Nyx mounted workspace qualification';
  FHost.ClientWidth := AWidth;
  FHost.ClientHeight := AHeight;
  FHost.Show;
  Pump;
  FView := MountNyxLCLDiagramEditor(FDescriptor, FDocument, FHost, FState);
  FCanvas := FView.Canvas as INyxLCLCanvasWorkspaceView;
  Require(Supports(FView, INyxDiagramContentView, FContent), 'content capability');
  Pump;
  Pump;
  FView.SelectVertex(NyxBenchmarkVertex(0));
  Pump;
  FMemo := Memo;
  FMemo.Text := 'An edited note worth keeping';
  FMemo.SelStart := 4;
  FMemo.SelLength := 3;
  Require(FState.GetValue(NyxTextState('note')) = TNyxText('An edited note worth keeping'),
    'ordinary memo input publishes to its borrowed runtime state');
  FPicture := Picture;
  Require(FPicture.Showing and FPicture.HandleAllocated, 'drawing is visibly mounted');
  FOriginalProc := FPicture.WindowProc;
  FPicture.WindowProc := WatchPaint;
  FPicture.AddHandlerOnBeforeDestruction(PictureRetiring);
  ForcePaint;
end;

procedure TMountedBenchmark.Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if FError <> '' then
  begin
    raise Exception.Create(FError);
  end;

  if not ACondition then
  begin
    raise Exception.Create('Mounted diagram: ' + AMessage);
  end;
end;

procedure TMountedBenchmark.Failed(ASender: TObject; AError: Exception);
begin
  FError := AError.ClassName + ': ' + AError.Message;
end;

procedure TMountedBenchmark.Pump;
begin
  Application.ProcessMessages;
  CheckSynchronize;
  Require(True, 'native host remains healthy');
end;

function TMountedBenchmark.Memo: TMemo;
var
  LItem: TNyxCanvasItemRef;
  LNode: TNyxNode;
begin
  LItem := FContent.ContentItem(NyxBenchmarkVertex(0));
  LNode := FirstKind(FCanvas.RendererFor(LItem).Root, nkMemo);
  Require(LNode <> nil, 'ordinary reusable contains its memo');
  Result := TMemo(FCanvas.RendererFor(LItem).InputFor(LNode.ID));
  Require(Result <> nil, 'ordinary memo has a native input');
end;

function TMountedBenchmark.Picture: TNyxLCLDrawing;
begin
  Result := TNyxLCLDrawing(FCanvas.RendererFor(NyxCanvasItem('Diagram picture'))
    .ControlFor('mounted-benchmark-diagram-picture'));
  Require(Result <> nil, 'ordinary canvas has its drawing');
end;

procedure TMountedBenchmark.WatchPaint(var AMessage: TLMessage);
begin

  if AMessage.Msg = LM_PAINT then
  begin
    Inc(FPaints);
  end;
  FOriginalProc(AMessage);
end;

procedure TMountedBenchmark.PictureRetiring(ASender: TObject);
begin
  FPicture.WindowProc := FOriginalProc;
  FPicture := nil;
  FOriginalProc := nil;
end;

procedure TMountedBenchmark.DetachSpy;
begin

  if (FPicture <> nil) and Assigned(FOriginalProc) then
  begin
    FPicture.WindowProc := FOriginalProc;
    FPicture.RemoveHandlerOnBeforeDestruction(PictureRetiring);
    FPicture := nil;
    FOriginalProc := nil;
  end;
end;

procedure TMountedBenchmark.ForcePaint;
var
  LBefore: Integer;
begin
  Require((FPicture <> nil) and (Picture = FPicture) and FPicture.Showing,
    'same visible drawing survives publication');
  LBefore := FPaints;
  FCanvas.Surface.Repaint;
  FPicture.Repaint;
  Require(FPaints > LBefore, 'forced repaint reached the actual drawing paint handler');
end;

procedure TMountedBenchmark.CheckRetained;
begin
  Require((Memo = FMemo) and (FMemo.Text = 'An edited note worth keeping') and
    (FMemo.SelStart = 4) and (FMemo.SelLength = 3), 'same memo/text/caret survive');
  Require(FState.GetValue(NyxTextState('note')) = TNyxText('An edited note worth keeping'),
    'same runtime value survives');
  Require(TNyxCodec.Encode(FDocument) = FBefore, 'caller document stays exact');
end;

function TMountedBenchmark.Measure: TNyxDataValue;
var
  LApply: array[0..2] of TNyxDataValue;
  LPaint: array[0..2] of TNyxDataValue;
  LKey: array[0..2] of TNyxDataValue;
  LIndex: Integer;
  LRevision: Integer;
  LBounds: TNyxCanvasBounds;
  LExpected: INyxDiagram;
  LStart: Double;
  LApplyTime: Double;
  LPaintTime: Double;
  LKeyTime: Double;
  LFace: TWinControl;
  LCode: Word;
  LHeapBefore: Double;
  LHeapAfter: Double;
  LHeapDisconnected: Double;
  LVertices: Integer;
  LEdges: Integer;
begin
  LHeapBefore := GetFPCHeapStatus.CurrHeapUsed;
  for LIndex := -1 to 2 do
  begin
    LRevision := FView.Revision;
    LBounds := FView.Diagram.Vertex(0).Bounds;
    LBounds := NyxCanvasBounds(LBounds.Left + 1.125, LBounds.Top - 0.75,
      LBounds.Width, LBounds.Height);
    LExpected := FView.Diagram.WithVertex(FView.Diagram.Vertex(0).AtBounds(LBounds));
    LStart := Clock;
    FView.SetGeometry(NyxBenchmarkVertex(0), LBounds, LRevision);
    LApplyTime := Clock - LStart;
    LStart := Clock;
    ForcePaint;
    LPaintTime := Clock - LStart;
    Pump;
    Require((FView.Revision = LRevision + 1) and
      (FView.Diagram.ToData.ToJSON = LExpected.ToData.ToJSON), 'complete geometry and revision');
    CheckRetained;
    LFace := FView.Renderer.FocusFor(NyxDiagramEditorPart('mounted-benchmark', ndepSelection).ID);
    Require(Assigned(TKeyAccess(LFace).OnKeyDown), 'ordinary native keyboard hook');
    LCode := $27;
    LRevision := FView.Revision;
    LStart := Clock;
    TKeyAccess(LFace).OnKeyDown(LFace, LCode, []);
    LKeyTime := Clock - LStart;
    Require((LCode = 0) and (FView.Revision = LRevision + 1),
      'actual key hook consumed and published its nudge');
    Require(FView.Diagram.Vertex(0).Bounds.Left = LBounds.Left + 1,
      'the native key hook published the exact configured world nudge');
    ForcePaint;
    Pump;
    CheckRetained;

    if LIndex >= 0 then
    begin
      LApply[LIndex] := NyxData(LApplyTime);
      LPaint[LIndex] := NyxData(LPaintTime);
      LKey[LIndex] := NyxData(LKeyTime);
    end;
  end;
  LVertices := FView.Diagram.VertexCount;
  LEdges := FView.Diagram.EdgeCount;
  LHeapAfter := GetFPCHeapStatus.CurrHeapUsed;
  DetachSpy;
  FView.Disconnect;
  Require(not FView.Connected, 'retained view reports retirement');
  FContent := nil;
  FCanvas := nil;
  FView := nil;
  FMemo := nil;
  Pump;
  LHeapDisconnected := GetFPCHeapStatus.CurrHeapUsed;
  Result := NyxObject([NyxField('vertices', NyxData(LVertices)),
    NyxField('edges', NyxData(LEdges)),
    NyxField('width', NyxData(FHost.ClientWidth)), NyxField('height', NyxData(FHost.ClientHeight)),
    NyxField('applyMs', NyxArray(LApply)), NyxField('forcedPaintMs', NyxArray(LPaint)),
    NyxField('keyDispatchMs', NyxArray(LKey)), NyxField('paintMessages', NyxData(FPaints)),
    NyxField('heapBeforeBytes', NyxData(LHeapBefore)),
    NyxField('heapAfterBytes', NyxData(LHeapAfter)),
    NyxField('heapAfterDisconnectBytes', NyxData(LHeapDisconnected)),
    NyxField('processHeapHighWaterBytes', NyxData(Double(GetFPCHeapStatus.MaxHeapUsed))),
    NyxField('framePresentationMeasured', NyxData(False))]);
end;

destructor TMountedBenchmark.Destroy;
begin
  DetachSpy;

  if FView <> nil then
  begin
    FView.Disconnect;
  end;
  FContent := nil;
  FCanvas := nil;
  FView := nil;
  FDescriptor := nil;
  Application.ProcessMessages;
  CheckSynchronize;
  FHost.Free;
  FState.Free;
  FDocument.Free;
  Application.OnException := FOriginalException;
  inherited Destroy;
end;

var
  LBenchmark: TMountedBenchmark;
  LCount: Integer;
  LWidth: Integer;
  LHeight: Integer;
  LReceipt: TNyxDataValue;
begin
  {$ifdef MSWINDOWS}QueryPerformanceFrequency(GFrequency);{$endif}
  LBenchmark := nil;
  try

    if (ParamCount <> 3) or not TryStrToInt(ParamStr(1), LCount) or
      not TryStrToInt(ParamStr(2), LWidth) or not TryStrToInt(ParamStr(3), LHeight) or
      ((LCount <> 16) and (LCount <> 64) and (LCount <> 256)) or
      not (((LWidth = 800) and (LHeight = 600)) or ((LWidth = 390) and (LHeight = 640))) then
    begin
      raise Exception.Create('Supply 16/64/256 vertices and either 800 600 or 390 640');
    end;
    Application.Initialize;
    LBenchmark := TMountedBenchmark.Create(LCount, LWidth, LHeight);
    LReceipt := LBenchmark.Measure;
    FreeAndNil(LBenchmark);
    WriteLn(LReceipt.ToJSON);
  except
    on E: Exception do
    begin
      LBenchmark.Free;
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
