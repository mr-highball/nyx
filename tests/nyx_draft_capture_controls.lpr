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
program nyx_draft_capture_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.collections, nyx.collections.view, nyx.studio.hierarchy,
  nyx.text, nyx.data, nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.types,
  nyx.studio.lcl, nyx.studio.exchange, nyx.studio.session, nyx.studio.projects,
  nyx.studio.agents, nyx.studio.workspaces, nyx.test.editor.exchange;

type
  { Real Win32 timers and queued replies surround the actual portable private
    protocol. This exercises ordinary Nyx Studio callbacks/painting without
    starting a listener, enrolling Codex or replacing another project's files. }
  TClockedExchange = class(TNyxTestEditorExchange)
  private
    FClock: TTimer;
    procedure Tick(ASender: TObject);
    procedure Reply(AData: PtrInt);
  public
    constructor Create(ASession: TNyxAgentSession);
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
  end;

  TCaptureStudio = class(TNyxNativeStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

  TControlAccess = class(TControl);
  TFailureObserver = class
  public
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GServer: TNyxAgentSession;
  GExchange: TClockedExchange;
  GStudio: TCaptureStudio;
  GForm: TForm;
  GFailure: TFailureObserver;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.Message;
end;

constructor TClockedExchange.Create(ASession: TNyxAgentSession);
begin
  inherited Create(ASession);
  FClock := TTimer.Create(nil);
  FClock.Enabled := False;
  FClock.OnTimer := Tick;
end;

destructor TClockedExchange.Destroy;
begin
  CancelRequest;
  CancelTick;
  FreeAndNil(FClock);
  inherited Destroy;
end;

procedure TClockedExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin
  inherited Post(AConnect, AToken, ABody, AReply);
  Application.QueueAsyncCall(Reply, 0);
end;

procedure TClockedExchange.Reply(AData: PtrInt);
begin

  if RequestPending then
  begin
    Deliver;
  end;
end;

procedure TClockedExchange.CancelRequest;
begin
  Application.RemoveAsyncCalls(Self);
  inherited CancelRequest;
end;

procedure TClockedExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  inherited Schedule(ADelayMS, ATick);
  FClock.Enabled := False;
  FClock.Interval := ADelayMS;
  FClock.Enabled := True;
end;

procedure TClockedExchange.CancelTick;
begin

  if FClock <> nil then
  begin
    FClock.Enabled := False;
  end;
  inherited CancelTick;
end;

procedure TClockedExchange.Tick(ASender: TObject);
begin
  FClock.Enabled := False;
  FireTick;
end;

function TCaptureStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TClockedExchange.Create(GServer);
  Result := GExchange;
end;

procedure Pump;
begin
  CheckSynchronize;
  Application.ProcessMessages;

  if GFailure.Failure <> '' then
  begin
    raise Exception.Create(GFailure.Failure);
  end;
end;

procedure WaitForDraft(const ADraft: TNyxText);
var
  LStarted: QWord;
  LState: TNyxDataValue;
  LPair: TNyxProjectPair;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if not GStudio.Agents.Busy and (GExchange.Commits > 0) then
    begin
      LState := GServer.Exchange(NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
      LPair := DecodeNyxProject(LState.Field('project').AsText);

      if LPair.Pending and (LPair.Draft = ADraft) then
      begin
        Exit;
      end;
    end;

    if GetTickCount64 - LStarted > 45000 then
    begin
      raise Exception.Create('The actual editor draft did not reach its independent paired server');
    end;
    Sleep(1);
  until False;
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

function SizedPair(AControls: Integer): TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LIndex: Integer;
  LSource: TNyxText;
  LFixture: TNyxStudioSession;
begin
  LDocument := TNyxDocument.Create;
  LFixture := TNyxStudioSession.Create;
  try
    { Keep the original source benchmark's complete document/names/comments/
      expression and byte sizes. Unicode here is private qualification input;
      the initial application labels and visible editor chrome remain English. }
    LDocument.Title := 'Source workspace';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('Before edit'));
    LPage.Add(NewNyxLabel('stable').WithText('Stable caption'));
    for LIndex := 2 to AControls - 1 do
    begin
      LPage.Add(NewNyxLabel('caption-' + IntToStr(LIndex))
        .WithText('Caption ' + IntToStr(LIndex)));
    end;
    LDocument.AddPage(LPage);
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := StringReplace(LSource, 'LMessageLabel', 'LMessageCaption', [rfReplaceAll]);
    LSource := StringReplace(LSource, 'LMessageCaption :=',
      TNyxText('{ Keep this note / 🌙 / 漢字 }') + #10 + '    LMessageCaption :=', [rfReplaceAll]);
    LSource := StringReplace(LSource, '''Stable caption''',
      '''Stable '' + ''caption''', [rfReplaceAll]);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    { The original reported byte count is the accepted pair after its visual
      caption change and button insertion, not the earlier generated builder.
      Replay those same public operations before loading the actual editor. }
    LFixture.LoadProject(Result);
    LFixture.Select('message');
    LFixture.SetProperty('text', 'After edit');
    LFixture.Select('home');
    LFixture.AddControl(NewNyxButton('added-action').WithText('Continue'));
    Result := LFixture.ProjectSnapshot;
  finally
    LFixture.Free;
    LDocument.Free;
  end;
end;

procedure Journey(AControls, ABytes: Integer);
var
  LPair: TNyxProjectPair;
  LCode: TMemo;
  LBefore: TNyxText;
  LDraft: TNyxText;
  LIndex: Integer;
  LPosts: Integer;
  LPaints: Integer;
  LCaret: Integer;
  LStarted: QWord;
  LFirstMS: QWord;
  LBurstMS: QWord;
  LReadyMS: QWord;
  LState: TNyxDataValue;
  LTree: TTreeView;
  LLast: TTreeNode;
  LCommits: Integer;
begin
  LPair := SizedPair(AControls);
  Check(Length(LPair.Source) = ABytes, 'Original source byte size is unchanged / got ' +
    IntToStr(Length(LPair.Source)));
  GServer := TNyxAgentSession.Create(LPair);
  GForm := TForm.Create(nil);
  GForm.ClientWidth := 1100;
  GForm.ClientHeight := 780;
  GStudio := TCaptureStudio.Create(GForm,
    IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects-' + IntToStr(AControls));
  try
    GStudio.LoadProject(LPair);
    GStudio.Run;
    GForm.Show;
    Pump;
    GStudio.ConnectService('http://127.0.0.1:8388', NyxPrimaryWorkspace);
    Pump;
    Check(GStudio.Agents.Connected and not GStudio.Agents.Conflict,
      'Actual native Studio attaches to its independent protocol session');
    { Keep the real code pane visible in presentation captures. The Agents
      composition remains available through its ordinary toolbar toggle. }
    TControlAccess(GStudio.ShellView.ControlFor('action-agents')).Click;
    Pump;
    LTree := TTreeView(GStudio.ShellView.ControlFor('studio-hierarchy'));
    Check((LTree <> nil) and (LTree.Height = 280) and
      (LTree.Items.Count = AControls + 2),
      'The public Nyx tree represents every component inside one bounded native viewport');
    LLast := LTree.Items.FindNodeWithText('label / caption-' + IntToStr(AControls - 1));
    Check(LLast <> nil, 'The hierarchy includes the final component without truncation');
    LTree.Selected := LLast;
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GStudio.Session.SelectedID = 'caption-' + IntToStr(AControls - 1) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 5000 then
      begin
        Break;
      end;
      Sleep(1);
    until False;
    Check(GStudio.Session.SelectedID = 'caption-' + IntToStr(AControls - 1),
      'Actual native tree selection routes the exact component identity / ' +
      GStudio.Session.SelectedID + ' / ' + GStudio.Status);
    Check((GStudio.CanvasView.ViewViewport.Y.Position > 0) and
      (GStudio.CanvasView.ControlFor(GStudio.Session.SelectedID).Height > 0),
      'Hierarchy navigation reveals the final full-size canvas control');
    TControlAccess(GStudio.ShellView.ControlFor('action-code')).Click;
    Pump;
    LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
    Check(LCode <> nil, 'Large project consumes the ordinary Nyx code editor');
    LBefore := GStudio.Session.Source;
    Check(LBefore = LPair.Source, 'Complete crafted source reaches the mounted native memo');
    LCode.SetFocus;
    LPosts := GExchange.Posts;
    LCommits := GExchange.Commits;
    LPaints := GStudio.PaintCount;
    LStarted := GetTickCount64;
    LCode.Text := LBefore + #10 + '// Private notes ';
    LCode.SelStart := Length(LCode.Text);
    LFirstMS := GetTickCount64 - LStarted;
    LDraft := LBefore + #10 + '// Private notes ';
    LStarted := GetTickCount64;
    for LIndex := 1 to 80 do
    begin
      LCode.SelText := 'x';
      LDraft := LDraft + 'x';
    end;
    LBurstMS := GetTickCount64 - LStarted;
    LCaret := LCode.SelStart;
    Check((GExchange.Posts = LPosts) and (GStudio.PaintCount = LPaints),
      'Actual input callbacks neither post a whole pair nor repaint per keystroke');
    Check(GStudio.Session.DraftSource = LDraft, 'Actual memo editing owns the exact final local text immediately');
    Check(Pos('waiting', GStudio.Agents.Status) > 0,
      'The native observing agent view exposes pending local typing');
    LStarted := GetTickCount64;
    WaitForDraft(LDraft);
    LReadyMS := GetTickCount64 - LStarted;
    Check((GExchange.Commits = LCommits + 1) and (GStudio.Session.Source = LBefore),
      'One actual timer commit shares the burst without changing accepted source');
    Check((GStudio.CodeView.InputFor('studio-code') = LCode) and
      LCode.Focused and (LCode.SelStart = LCaret),
      'Timer/observation painting retain the actual memo, focus and caret');
    LState := GServer.Call('nyx_session', 'Capture control qualification', NyxObject([]));
    Check(not LState.Field('canUndo').AsBoolean,
      'Actual native draft capture creates no accepted-content Undo entry');
    WriteLn(AControls, ',', ABytes, ',', LFirstMS, ',', LBurstMS, ',', LReadyMS);

    if AControls = 128 then
    begin
      LCode.SelStart := 0;
      Capture('draft-capture-desktop');
      GForm.ClientWidth := 390;
      GForm.ClientHeight := 800;
      Pump;
      LCode.SelStart := 0;
      Capture('draft-capture-390');
      GForm.ClientWidth := 1100;
      Pump;
    end;

    { Pending capture and queued typed selection must retire before the borrowed
      receiver/session is destroyed. Do not pump between selection and disposal. }
    LCode.SelStart := Length(LCode.Text);
    LCode.SelText := 'x';
    GStudio.ShellView.CollectionView(NyxStudioHierarchyID).Select(
      NyxItem(GStudio.ShellView.CollectionView(NyxStudioHierarchyID).Spec.Key, 'home'));
    FreeAndNil(GStudio);
    Pump;
    Check(GFailure.Failure = '',
      'Retiring pending capture and typed selection cannot notify freed Studio controls');
  finally
    FreeAndNil(GStudio);
    FreeAndNil(GForm);
    FreeAndNil(GServer);
    GExchange := nil;
  end;
end;

begin
  GFailure := TFailureObserver.Create;
  try

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply an owned artifact directory');
    end;
    ForceDirectories(ParamStr(1));
    Application.Initialize;
    Application.OnException := GFailure.Failed;
    WriteLn('controls,source_utf8_bytes,first_edit_ms,80_insert_callbacks_ms,timer_and_protocol_ms');
    Journey(128, 25094);
    Journey(512, 98822);
    Journey(2048, 400022);
    WriteLn('PASS ', GChecks, ' actual native draft capture/control checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GFailure.Free;
end.
