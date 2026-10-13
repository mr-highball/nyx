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

program nyx_studio_progress_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model,
  nyx.controls, nyx.codec, nyx.behavior, nyx.generated.view, nyx.studio.session, nyx.studio.projects,
  nyx.studio.view, nyx.studio.buildview, nyx.studio.builds, nyx.studio.builddiagnostics,
  nyx.studio.sourcejobs
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, Graphics, IntfGraphics, FPWritePNG,
    LCLIntf, LCLType, nyx.render.lcl, nyx.widgets.lcl, nyx.progress.lcl{$endif};

{$ifndef PAS2JS}
type
  TButtonAccess = class(TNyxLCLButton);
{$endif}

var
  GChecks: Integer;
  GRunning: TNyxDataValue;
  GComplete: TNyxDataValue;
  GSource: TNyxText;
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  GLoadStep: Integer;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Studio progress: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Detached scalar replacement for explicit wire-boundary cases. It never changes
  the real semantic compiler receipts supplied to this consumer. }
function Changed(const AValue: TNyxDataValue; const AName: TNyxText;
  const AReplacement: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFound: Boolean;
  LCount: Integer;
begin
  LFound := False;
  { The qualified pas2js revision can omit a record getter call in a complex
    array index. Cache the scalar before sizing/indexing on both targets. }
  LCount := AValue.Count;
  SetLength(LFields, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));

    if AValue.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AReplacement);
      LFound := True;
    end;
  end;

  if LFound then
  begin
    SetLength(LFields, LCount);
  end
  else
  begin
    LFields[LCount] := NyxField(AName, AReplacement);
  end;
  Result := NyxObject(LFields);
end;

function Envelope(const AItem: TNyxDataValue): TNyxDataValue;
var
  LState: TNyxBuildJobState;
begin
  LState := ParseNyxBuildJobState(AItem.Field('state').AsText);
  Result := NyxObject([
    NyxField('running', NyxData(Ord(LState = bjsRunning))),
    NyxField('queued', NyxData(Ord(LState = bjsQueued))),
    NyxField('cancelling', NyxData(Ord(LState = bjsCancelling))),
    NyxField('items', NyxArray([AItem]))]);
end;

procedure Shared;
const
  CStates: array[TNyxBuildJobState] of TNyxText =
    ('queued', 'running', 'cancelling', 'succeeded', 'failed', 'cancelled');
var
  LState: TNyxBuildJobState;
  LActivity: TNyxStudioBuildActivity;
  LItem: TNyxDataValue;
  LRoot: TNyxNode;
  LOriginal: TNyxText;
begin
  Check((GRunning.Field('items').Count = 1) and
    (GRunning.Field('items').Item(0).Field('state').AsText = 'running'),
    'real MCP receipt observed actual compiler execution');
  Check((GComplete.Field('items').Count = 1) and
    (GComplete.Field('items').Item(0).Field('state').AsText = 'succeeded'),
    'real MCP receipt observed terminal compiler success');
  Check(GRunning.Field('items').Item(0).Field('job').AsText =
    GComplete.Field('items').Item(0).Field('job').AsText, 'both receipts belong to the exact same job');
  LOriginal := GRunning.ToJSON;
  for LState := Low(TNyxBuildJobState) to High(TNyxBuildJobState) do
  begin
    LItem := Changed(GRunning.Field('items').Item(0), 'state', NyxData(CStates[LState]));
    LActivity := ReadNyxStudioBuildActivity(LItem);
    Check(LActivity.Ongoing = (LState in [bjsQueued, bjsRunning, bjsCancelling]),
      'closed execution state determines honest ongoing activity');
    LRoot := BuildNyxStudioBuildJobs(Envelope(LItem), False, True);
    try
      Check((LRoot.Find('studio-builds-progress').Prop('visible') = 'true') = LActivity.Ongoing,
        'one public Nyx indicator follows the typed execution state');
      Check(LRoot.Find('studio-builds-progress').Prop('indeterminate') = 'true',
        'activity never invents a percentage');
      Check(LRoot.Find('studio-build-0-cancel').Prop('enabled') = 'false',
        'activity presentation grants no cancellation authority');
      Check(LRoot.Find('studio-build-0-inspect').Extensions.Value(
        NyxStudioInspectBuildKey).AsText = LItem.Field('job').AsText,
        'diagnostic identity retains the exact immutable job');
    finally
      LRoot.Free;
    end;
  end;
  LItem := Changed(GComplete.Field('items').Item(0), 'purpose', NyxData('transaction'));
  LItem := Changed(LItem, 'operation', NyxData('source-edit'));
  LItem := Changed(LItem, 'publication', NyxObject([
    NyxField('state', NyxData('pending')), NyxField('revision', NyxNull)]));
  LActivity := ReadNyxStudioBuildActivity(LItem);
  Check(LActivity.Ongoing and (Pos('Awaiting publication', LActivity.Caption) = 1),
    'successful compilation still awaits independent editor admission');
  LItem := Changed(LItem, 'publication', NyxObject([
    NyxField('state', NyxData('committed')), NyxField('revision', NyxData(4))]));
  LActivity := ReadNyxStudioBuildActivity(LItem);
  Check(not LActivity.Ongoing and (Pos('Applied', LActivity.Caption) = 1),
    'published changes stop activity and retain honest applied feedback');
  LItem := Changed(LItem, 'publication', NyxObject([
    NyxField('state', NyxData('refused')), NyxField('revision', NyxNull)]));
  LActivity := ReadNyxStudioBuildActivity(LItem);
  Check(not LActivity.Ongoing and (Pos('Not applied', LActivity.Caption) = 1),
    'refused publication stops activity without claiming application');
  Check(GRunning.ToJSON = LOriginal, 'presentation cases preserve exact real metadata');
end;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  {$endif}
  { Owns an isolated real source processor and the public Nyx source pane. Backend
    metadata is a recorded semantic receipt, not execution/publication authority. }
  TReview = class
  private
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FRenderer: TRenderer;
    FHost: THost;
    FShell: TNyxDocument;
    FState: TNyxStudioViewState;
    FStage: Integer;
    FBefore: TNyxText;
    FAfter: TNyxText;
    FSeenPreparing: Boolean;
    FJobs: TNyxDataValue;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Refresh;
    procedure ClickApply;
    procedure CheckSourceActivity(AExpected: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Step: Boolean;
    {$ifndef PAS2JS}
    procedure Capture(const APath: String);
    {$endif}
  end;

var
  GReview: TReview;
  GPolls: Integer;

constructor TReview.Create;
var
  LDoc: TNyxDocument;
begin
  inherited Create;
  LDoc := BuildNyxDocument;
  try
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDoc), GSource));
  finally
    LDoc.Free;
  end;
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FState := DefaultNyxStudioViewState;
  FState.CodeVisible := True;
  FJobs := GRunning;
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('main'));
  FHost.style.cssText := 'height:760px;width:100%;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(30, 30, 1000, 800);
  FHost.Show;
  {$endif}
  Refresh;
end;

destructor TReview.Destroy;
begin
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}FHost.Free;{$endif}
  inherited Destroy;
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState = nssPreparing then
  begin
    FSeenPreparing := FSeenPreparing or FCommands.Busy;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
  LPage: INyxPage;
begin
  FState.SourceWorking := FCommands.Busy;
  FState.SourceStatus := FCommands.Message;
  LShell := TNyxDocument.Create;
  try
    LPage := NewNyxPage('studio-work-review');
    LShell.AddPage(LPage);
    LPage.Configure.Padding(16).Gap(12).Done;
    LPage.Add(NewNyxHeading('review-title').WithText('A clearer Pascal workspace'));
    LPage.Add(NewNyxLabel('review-help').WithText(
      'Recorded compiler activity and an independently owned source workspace.'));
    LPage.Node.Add(BuildNyxStudioBuildJobs(FJobs, False, True));
    LPage.Node.Add(BuildNyxStudioSourcePane(FSession, FState, nil));
    FRenderer.Render(LShell, LShell.Pages[0], FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
end;

procedure TReview.ClickApply;
begin
  {$ifdef PAS2JS}
  FRenderer.ElementFor('action-apply-source').click;
  {$else}
  TButtonAccess(FRenderer.ControlFor('action-apply-source')).Click;
  {$endif}
  { The widget callback has returned. Only now refresh the owned presentation,
    while ordinary source couriers retain their real independent execution. }
  Refresh;
end;

procedure TReview.CheckSourceActivity(AExpected: Boolean);
var
  LNode: TNyxNode;
begin
  LNode := FRenderer.Root.Find('studio-source-progress');
  Check((LNode.Prop('visible') = 'true') = AExpected,
    'mounted public source pane uses actual queue activity, not status wording');
  {$ifdef PAS2JS}
  Check(not TJSHTMLProgressElement(FRenderer.ElementFor(LNode.ID)).hasAttribute('value'),
    'source activity is real indeterminate HTML progress');
  {$else}
  Check(TNyxLCLProgress(FRenderer.ControlFor(LNode.ID)).Indeterminate,
    'source activity consumes the reusable LCL progress adapter');
  {$endif}
end;

function TReview.Step: Boolean;
var
  LDraft: TNyxText;
begin
  Result := False;
  case FStage of
    0:
      begin
        CheckSourceActivity(False);
        LDraft := StringReplace(FSession.Source, 'A job finding its rhythm',
          'Make room for ideas', []);
        Check(LDraft <> FSession.Source, 'exact semantic seed supplies the purposeful source edit');
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        FSession.SetSourceDraft(LDraft);
        Refresh;
        ClickApply;
        Check(FSeenPreparing, 'real source worker publishes an owning busy transition');
        CheckSourceActivity(True);
        FStage := 1;
      end;
    1:
      begin

        if FCommands.Busy then
        begin
          Exit;
        end;
        Refresh;
        Check(FCommands.State = nssApplied, 'real source preparation publishes the valid draft');
        CheckSourceActivity(False);
        FAfter := EncodeNyxProject(FSession.ProjectSnapshot);
        Check(FSession.Document.Find('progress-title').Prop('text') = 'Make room for ideas',
          'ordinary Apply reads and publishes the actual mounted Pascal draft');
        FSession.Undo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
          'one paired Undo restores the previous accepted pair after consuming its staging buffer');
        FSession.Redo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FAfter,
          'one paired Redo restores the exact applied pair');
        LDraft := StringReplace(FSession.Source, '.Gap(16)', '.Gap(''sixteen'')', []);
        Check(LDraft <> FSession.Source, 'refusal case changes an actual typed configuration call');
        FSession.SetSourceDraft(LDraft);
        Refresh;
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        ClickApply;
        CheckSourceActivity(True);
        FStage := 2;
      end;
    2:
      begin

        if FCommands.Busy then
        begin
          Exit;
        end;
        Refresh;
        Check(FCommands.State in [nssRejected, nssFailed], 'invalid typed source reaches terminal refusal');
        CheckSourceActivity(False);
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
          'refusal preserves exact accepted pair and pending draft');
        FJobs := GComplete;
        Refresh;
        Check(FRenderer.Root.Find('studio-builds-progress').Prop('visible') = 'false',
          'real terminal compiler receipt stops aggregate activity');
        FJobs := GRunning;
        Refresh;
        {$ifdef PAS2JS}
        document.body.setAttribute('data-capture-checkpoint', 'studio-progress');
        FStage := 3;
        {$else}
        Result := True;
        {$endif}
      end;
    3:
      begin
        {$ifdef PAS2JS}
        Result := document.body.getAttribute('data-capture-observed') = 'studio-progress';
        {$endif}
      end;
  end;
end;

{$ifndef PAS2JS}
procedure TReview.Capture(const APath: String);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LDC: HDC;
begin
  FHost.BringToFront;
  FHost.Update;
  Application.ProcessMessages;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(FHost.ClientWidth, FHost.ClientHeight);
    LDC := GetDC(FHost.Handle);
    try
      Check(BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
        LDC, 0, 0, SRCCOPY), 'owned displayed Studio component client captured');
    finally
      ReleaseDC(FHost.Handle, LDC);
    end;
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(APath, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

procedure Drive;
begin
  try
    Inc(GPolls);

    if GPolls > 2400 then
    begin
      raise Exception.Create('Owning source journey exceeded its observation budget');
    end;

    if GReview.Step then
    begin
      {$ifndef PAS2JS}

      if ParamCount > 1 then
      begin
        GReview.Capture(ParamStr(2));
      end;
      {$endif}
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-studio-progress-checks', IntToStr(GChecks));
      document.body.setAttribute('data-studio-progress', 'passed');
      {$else}
      WriteLn('PASS ', GChecks, ' real metadata/public Studio/source-worker activity checks');
      {$endif}
    end
    {$ifdef PAS2JS}
    else
    begin
      window.setTimeout(@Drive, 25);
    end
    {$endif};
  except
    on LException: Exception do
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-studio-progress', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-studio-progress', 'failed');
    Exit;
  end;
  case GLoadStep of
    0:
      begin
        GRunning := TNyxDataValue.ParseJSON(GRequest.responseText);
      end;
    1:
      begin
        GComplete := TNyxDataValue.ParseJSON(GRequest.responseText);
      end;
    2:
      begin
        GSource := GRequest.responseText;
      end;
  end;
  GRequest := nil;
  Inc(GLoadStep);

  if GLoadStep = 3 then
  begin
    Shared;
    GReview := TReview.Create;
    Drive;
  end;
end;

procedure LoadNext;
const
  CFiles: array[0..2] of String = ('build-jobs-running.json',
    'build-jobs-succeeded.json', 'nyx.generated.view.pas');
begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onload := @Loaded;
  GRequest.open('GET', CFiles[GLoadStep], True);
  GRequest.send;
end;

procedure AwaitInputs;
begin

  if (GLoadStep < 3) and (GRequest = nil) then
  begin
    LoadNext;
  end;

  if GLoadStep < 3 then
  begin
    window.setTimeout(@AwaitInputs, 20);
  end;
end;
{$else}
function ReadBytes(const APath: String): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 4194304) then
    begin
      raise Exception.Create('Source/metadata input requires bounded nonempty UTF-8 bytes');
    end;
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  AwaitInputs;
  {$else}
  Application.Initialize;
  try
    GRunning := TNyxDataValue.ParseJSON(ReadBytes(
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'build-jobs-running.json'));
    GComplete := TNyxDataValue.ParseJSON(ReadBytes(
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'build-jobs-succeeded.json'));
    GSource := ReadBytes(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas');
    Shared;
    GReview := TReview.Create;
    while GReview <> nil do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Drive;
      Sleep(25);
    end;
  except
    on LException: Exception do
    begin
      FreeAndNil(GReview);
      WriteLn('FAIL startup / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
