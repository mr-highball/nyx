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

program nyx_canvas_graph_semantic_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics,
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.editing,
  nyx.canvas.declarations, nyx.flow.types, nyx.flow.declarations,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.lcl,
  nyx.studio.exchange, nyx.studio.workspaces,
  nyx.test.editor.exchange, nyx.test.canvas.graph.layout;

type
  { Stock native Studio owns all controls, commands and source/history views.
    Only its private asynchronous transport is substituted with an independent
    production semantic session; this fixture never binds a listener. }
  TGraphStudio = class(TNyxNativeStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

  { Borrowed native button access exercises its normal control notification.
    It neither owns the control nor claims physical keyboard/pointer coverage. }
  TControlAccess = class(TControl);

  { Fixture-owned exception sink outlives deferred UI retirement and records
    framework callback failures instead of showing an unattended modal dialog. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;

var
  GCore: TNyxAgentSession;
  GStudio: TGraphStudio;
  GExchange: TNyxTestEditorExchange;
  GForm: TForm;
  GFailure: TFailure;
  GChecks: Integer;

function TGraphStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TNyxTestEditorExchange.Create(GCore);
  Result := GExchange;
end;

procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.ClassName + ': ' + AError.Message;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Observing semantic graph layout: ' + AReason);
  end;

  if (GFailure <> nil) and (GFailure.Message <> '') then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  Inc(GChecks);
end;

procedure Turn;
begin
  CheckSynchronize;
  Application.ProcessMessages;

  if GExchange <> nil then
  begin

    if GExchange.RequestPending then
    begin
      GExchange.Deliver;
    end
    else if GExchange.TickPending then
    begin
      GExchange.FireTick;
    end;
  end;

  if GFailure.Message <> '' then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  Sleep(1);
end;

procedure Observe(const APair: TNyxProjectPair);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Turn;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Owned native observation deadline: ' + GStudio.Agents.Status);
    end;
  until GStudio.Agents.Connected and not GStudio.PresentationPending and
    not GStudio.SourceBusy and
    (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(APair));
  Check(True, 'ordinary native observation receives the exact complete semantic pair');
end;

{ Win32 TMemo presents LF as CRLF. Project that display convention using typed
  UTF-8 slices only; never rewrite accepted source/drafts or pass supplementary
  source text through an ANSI String oracle. Already-native endings stay exact. }
function MemoText(const ASource: TNyxText): TNyxText;
var
  LIndex: Integer;
  LStart: Integer;
begin
  Result := '';
  LStart := 1;
  for LIndex := 1 to Length(ASource) do
  begin

    if (ASource[LIndex] = #10) and
      ((LIndex = 1) or (ASource[LIndex - 1] <> #13)) then
    begin
      Result := Result + Copy(ASource, LStart, LIndex - LStart) + TNyxText(#13#10);
      LStart := LIndex + 1;
    end;
  end;
  Result := Result + Copy(ASource, LStart, MaxInt);
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := GCore.Call(ATool, 'Scooty owned layout workshop', AArguments);
end;

procedure History(const ADirection, AID: TNyxText);
begin
  Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData(AID)), NyxField('direction', NyxData(ADirection))]));
end;

function PlacementRequest(const AID: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData(AID)), NyxField('operations', NyxArray([
      NyxPositionCanvasGraphNode(NyxCanvasRef('Layout board'), NyxFlowRef(GraphLayoutFlowName),
        NyxFlowStep(GraphLayoutStepName), NyxCanvasPoint(-456.125, 912.5)).ToData,
      NyxPositionCanvasGraphNode(NyxCanvasRef('Layout board'), NyxFlowRef(GraphLayoutFlowName),
        NyxFlowStep('Format'), NyxCanvasPoint(300.75, -99)).ToData,
      NyxAutomaticCanvasGraphNode(NyxCanvasRef('Layout board'), NyxFlowRef('Future graph'),
        NyxFlowStep('Arrange')).ToData]))]);
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LInput: TControl;
  LMemo: TMemo;
  LStarted: QWord;
  LFailed: Boolean;
  LTurn: Integer;
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
begin
  Check((ParamCount = 2) and not DirectoryExists(ParamStr(1)),
    'supply a fresh owned project directory and a capture path');
  LDocument := GraphLayoutDocument;
  try
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  GCore := TNyxAgentSession.Create(LBefore);
  { Preclaim only this fresh independent fixture. Later ordinary observing
    claims must not replace it with Studio's default starter document. }
  GCore.Exchange(NyxObject([NyxField('op', NyxData('claim')),
    NyxField('project', NyxData(EncodeNyxProject(LBefore))),
    NyxField('selection', NyxData('layout-tools')), NyxField('view', NyxData('home'))]));
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(20, 20, 1400, 1000);
  GForm.Show;
  GStudio := TGraphStudio.Create(GForm, ParamStr(1));
  GStudio.Run;
  GStudio.ConnectService('http://owned-qualification.invalid', NyxPrimaryWorkspace);
  Observe(LBefore);
  TControlAccess(GStudio.ShellView.ControlFor('action-code')).Click;
  LStarted := GetTickCount64;
  repeat
    Turn;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Owned source presentation deadline');
    end;
  until not GStudio.PresentationPending;
  LInput := GStudio.CodeView.InputFor('studio-code');
  Check(LInput is TMemo, 'source presentation exposes its ordinary actual memo input');
  LMemo := TMemo(LInput);
  Check((GStudio.CodeView.Root.Prop('value') = LBefore.Source) and
    (LMemo.Text = MemoText(LBefore.Source)), 'source control presents exact accepted UTF-8 with native line endings');
  Call('nyx_transaction', PlacementRequest('arrange'));
  LAfter := GCore.PreviewPair(GCore.Revision, 'home');
  Observe(LAfter);
  Check((GStudio.CodeView.InputFor('studio-code') = LMemo) and (LMemo.Text = MemoText(LAfter.Source)),
    'semantic layout publication retains the same source editor and updates its exact text');
  Check(Length(NyxCanvasGraphNodes(GStudio.Session.Document.Canvases.Definition(
    NyxCanvasRef('Layout board')))) = 2, 'ordinary Studio receives the complete arranged saved layout');
  History('undo', 'undo-arrange');
  Observe(LBefore);
  Check((GStudio.CodeView.InputFor('studio-code') = LMemo) and (LMemo.Text = MemoText(LBefore.Source)),
    'paired semantic Undo visibly restores exact source in the same editor');
  History('redo', 'redo-arrange');
  Observe(LAfter);
  Check((GStudio.CodeView.InputFor('studio-code') = LMemo) and (LMemo.Text = MemoText(LAfter.Source)),
    'paired semantic Redo visibly restores exact arranged source');
  LMemo.Text := LAfter.Source + #10 + '// unfinished layout draft';
  LStarted := GetTickCount64;
  repeat
    Turn;
    LPending := GCore.PreviewPair(GCore.Revision, 'home');

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Owned draft synchronization deadline');
    end;
  until LPending.Pending;
  Observe(LPending);
  LFailed := False;
  try
    Call('nyx_transaction', PlacementRequest('draft-refusal'));
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (EncodeNyxProject(GCore.PreviewPair(GCore.Revision, 'home')) =
    EncodeNyxProject(LPending)), 'pending source refuses semantic layout without changing accepted pair/buffer/base');
  Observe(LPending);
  Check((GStudio.CodeView.InputFor('studio-code') = LMemo) and (LMemo.Text = MemoText(LPending.Draft)),
    'ordinary observer keeps the exact unfinished source and editor on refusal');
  LBitmap := GForm.GetFormImage;
  LPNG := TPortableNetworkGraphic.Create;
  try
    LPNG.Assign(LBitmap);
    LPNG.SaveToFile(ParamStr(2));
  finally
    LPNG.Free;
    LBitmap.Free;
  end;
  FreeAndNil(GStudio);
  GExchange := nil;
  FreeAndNil(GForm);
  FreeAndNil(GCore);
  for LTurn := 1 to 5 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
  end;
  Check(GFailure.Message = '', 'deferred native observer retirement has no callback failure');
end;

begin
  try
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    Run;
    WriteLn('PASS ', GChecks, ' ordinary native semantic graph observation checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  FreeAndNil(GStudio);
  GExchange := nil;
  FreeAndNil(GForm);
  FreeAndNil(GCore);
  Application.OnException := nil;
  FreeAndNil(GFailure);
end.

