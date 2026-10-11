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
program nyx_http_source_workshop;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.editing, nyx.data, nyx.codec, nyx.model,
  nyx.studio.projects, nyx.studio.workspaces, nyx.studio.lcl,
  nyx.studio.sourcecompilation.shared.native, nyx.studio.preview,
  nyx.test.mcp.client, nyx.test.browser.pipe;

type
  TControlAccess = class(TControl);
  { LCL reports asynchronous callback failures here. The observer outlives the
    complete native Studio, and teardown detaches the borrowed handler last. }
  TFailureObserver = class
    MessageText: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

const
  COwnedLabel = 'Manual presentations';
  CHelperName = 'ComposeWorkshopTitle';
  CStateName = 'workshop-summary';
  CMemoID = 'workshop-notes';
  CLabelID = 'workshop-preview';

var
  GClient: TNyxMCPTestClient;
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GWorkspace: TNyxWorkspaceRef;
  GEvidence: TNyxText;
  GSerial: Integer;
  GChecks: Integer;
  GRevision: Integer;
  GOperationPrefix: TNyxText;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  MessageText := UTF8Encode(UnicodeString(AException.ClassName + ': ' + AException.Message));
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if (GObserver <> nil) and (GObserver.MessageText <> '') then
  begin
    raise Exception.Create('Native HTTP callback: ' + GObserver.MessageText);
  end;

  if not ACondition then
  begin
    raise Exception.Create('HTTP source workshop: ' + AReason);
  end;
  Inc(GChecks);
end;

{ These are private byte receipts, not exported designs. This consumer never
  reads credentials into document properties, source text or console output. }
procedure Save(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(GEvidence + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function Read(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check((LStream.Size > 0) and (LStream.Size <= 4 * 1024 * 1024),
      'qualification input is bounded');
    SetLength(Result, LStream.Size);
    LStream.ReadBuffer(Result[1], Length(Result));
  finally
    LStream.Free;
  end;
end;

function Operation: TNyxText;
begin
  Inc(GSerial);
  Result := GOperationPrefix + '-' + IntToStr(GSerial);
end;

{ Every semantic request names the explicit existing owned workshop. Omitted
  context would address the user primary and is deliberately impossible here. }
function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LReply: TNyxDataValue;
begin
  LReply := GClient.Tool(ATool, NyxWithWorkspace(AArguments, GWorkspace));

  if LReply.Field('isError').AsBoolean then
  begin
    Save('semantic-refusal.json', LReply.ToJSON);
    raise Exception.Create('Owned workshop semantic operation refused: ' + ATool);
  end;
  Result := LReply.Field('structuredContent');
end;

function Session: TNyxDataValue;
begin
  Result := Call('nyx_session', NyxObject([]));
end;

{ Bounded, revision-pinned scalar windows reconstruct only this explicitly owned
  demo source. A new revision invalidates the whole read; it is never silently
  rebased across edits. Primary/user source is not paged to prepare the fixture. }
function Source: TNyxText;
var
  LWindow: TNyxDataValue;
  LOffset: Integer;
  LNext: Integer;
begin
  Result := '';
  LOffset := 0;
  repeat
    LWindow := Call('nyx_pascal', NyxObject([
      NyxField('mode', NyxData('unit')), NyxField('offset', NyxData(LOffset)),
      NyxField('count', NyxData(4096)),
      NyxField('expectedRevision', NyxData(GRevision))]));
    Result := Result + LWindow.Field('text').AsText;
    LNext := LWindow.Field('nextOffset').AsInteger;
    Check((LNext > LOffset) and (NyxTextScalarCount(Result) <= 262144),
      'owned source windows progress within their scalar budget');
    LOffset := LNext;
  until LOffset = LWindow.Field('total').AsInteger;
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if GObserver.MessageText <> '' then
    begin
      raise Exception.Create('Native HTTP callback: ' + GObserver.MessageText);
    end;

    if not GStudio.SourceBusy and not GStudio.PresentationPending then
    begin
      Exit;
    end;
    Sleep(1);
  until GetTickCount64 - LStarted >= 180000;
  raise Exception.Create('Native HTTP source/presentation work exceeded its budget');
end;

{ Readiness also requires the independent observer to acknowledge the exact
  semantic source. A completed compiler token alone cannot establish adoption. }
procedure AwaitSource(const AExpected: TNyxText);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GStudio.Agents.Connected and not GStudio.Agents.Busy and
      (GStudio.Session.Source = AExpected) then
    begin
      Exit;
    end;
    Sleep(10);
  until GetTickCount64 - LStarted >= 180000;
  Save('observer-source-refusal.pas', GStudio.Session.Source);
  Save('observer-status-refusal.txt', GStudio.Agents.Status);
  raise Exception.Create('Native observer did not acknowledge the exact owned source');
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
begin

  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  Check(LControl <> nil, 'ordinary mounted action ' + AID);
  TControlAccess(LControl).Click;
  Pump;
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
    GForm.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(GEvidence + AName, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

function ScalarOffset(const AText: TNyxText; APosition: Integer): Integer;
begin
  Result := NyxTextScalarCount(Copy(AText, 1, APosition - 1));
end;

{ All helper authoring first uses semantic MCP. The subsequent physical Apply
  deliberately qualifies compiled constructor admission that the literal-only
  source-edit tool cannot supply; it is not screenshot-driven demo composition. }
function AddHelper(const ASource: TNyxText): TNyxText;
var
  LPosition: Integer;
  LText: TNyxText;
  LReply: TNyxDataValue;
begin
  LPosition := Pos('implementation', ASource);
  Check((LPosition > 0) and (Pos(CHelperName, ASource) = 0),
    'owned source has one fresh helper insertion');
  Inc(LPosition, Length('implementation'));
  LText := #10#10 +
    '{ An ordinary Pascal helper survives visual editing of this view. }' + #10 +
    'function ComposeWorkshopTitle: TNyxText;' + #10 +
    'const' + #10 +
    '  CWords: array[0..2] of TNyxText = (''Room'', ''for'', ''ideas'');' + #10 +
    'var' + #10 +
    '  LIndex: Integer;' + #10 +
    'begin' + #10 +
    '  Result := '''';' + #10 +
    '  for LIndex := Low(CWords) to High(CWords) do' + #10 +
    '  begin' + #10#10 +
    '    if LIndex > Low(CWords) then' + #10 +
    '    begin' + #10 +
    '      Result := Result + TNyxText('' '');' + #10 +
    '    end;' + #10 +
    '    Result := Result + CWords[LIndex];' + #10 +
    '  end;' + #10 +
    'end;';
  LReply := Call('nyx_pascal', NyxObject([
    NyxField('mode', NyxData('edit-unit')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)),
    NyxField('changes', NyxArray([NyxObject([
      NyxField('offset', NyxData(ScalarOffset(ASource, LPosition))),
      NyxField('expected', NyxData('')), NyxField('replacement', NyxData(LText))])]))]));
  GRevision := LReply.Field('revision').AsInteger;
  Result := Source;
  Check(Pos('function ' + CHelperName, Result) > 0,
    'semantic helper admission retains crafted Pascal');
end;

function WaitConstruction(const AReceipt: TNyxDataValue): TNyxDataValue;
var
  LStarted: QWord;
begin
  Check(AReceipt.Field('job').AsText <> '', 'semantic group owns a constructor job');
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', AReceipt.Field('job'))]));
    Save('construction-status.json', Result.ToJSON);

    if (Result.Field('state').AsText = 'succeeded') and
      (Result.Field('publication').Field('state').AsText = 'committed') then
    begin
      Break;
    end;

    if (Result.Field('state').AsText = 'failed') or
      (Result.Field('state').AsText = 'cancelled') or
      (Result.Field('publication').Field('state').AsText = 'refused') then
    begin
      raise Exception.Create('Semantic constructor did not succeed');
    end;
    Application.ProcessMessages;
    Sleep(25);
  until GetTickCount64 - LStarted >= 180000;
  Check(Result.Field('state').AsText = 'succeeded',
    'actual native semantic constructor reaches successful join');
  Check((Result.Field('publication').Field('state').AsText = 'committed') and
    (Result.Field('publication').Field('revision').AsInteger = GRevision + 1),
    'successful construction durably publishes exactly one shared revision');
  GRevision := Session.Field('revision').AsInteger;
  Check(GRevision = Result.Field('publication').Field('revision').AsInteger,
    'semantic session acknowledges the exact committed constructor revision');
end;

procedure Run(const AOrigin, ABaselinePath: TNyxText);
var
  LBaseline: TNyxDataValue;
  LEntry: TNyxDataValue;
  LPair: TNyxProjectPair;
  LFrame: TNyxDataValue;
  LBefore: TNyxText;
  LHelperSource: TNyxText;
  LComputedSource: TNyxText;
  LGroupSource: TNyxText;
  LInput: TMemo;
  LPosition: Integer;
  LIndex: Integer;
  LFound: Integer;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
begin
  LBaseline := TNyxDataValue.ParseJSON(Read(ABaselinePath));
  LFound := 0;
  for LIndex := 0 to LBaseline.Count - 1 do
  begin
    LEntry := LBaseline.Item(LIndex);

    if LEntry.Field('workspace').AsText = GWorkspace.ID then
    begin
      Inc(LFound);
      Check((LEntry.Field('label').AsText = COwnedLabel) and
        not LEntry.Field('pendingDraft').AsBoolean,
        'explicit baseline identifies the earlier owned manual-presentation demo');
      LPair := DecodeNyxProject(LEntry.Field('project').AsText);
      GRevision := LEntry.Field('revision').AsInteger;
    end;
  end;
  Check((LBaseline.Count = 9) and (LFound = 1) and (GWorkspace.ID <> ''),
    'one explicit owned demo is selected; primary is never a test target');
  LFrame := Session;
  Check((LFrame.Field('revision').AsInteger = GRevision) and
    not LFrame.Field('pendingDraft').AsBoolean,
    'current owned project matches the captured immutable baseline');
  LBefore := Source;
  Check(LBefore = LPair.Source, 'baseline source remains exact before mutations');
  Save('before.nyxproject', EncodeNyxProject(LPair));
  LHelperSource := AddHelper(LBefore);
  GStudio := TNyxNativeStudio.Create(GForm, GEvidence + 'native-projects', nil,
    NewNyxSharedNativeSourceCompilerFactory(AOrigin));
  GStudio.Run;
  GStudio.ConnectService(AOrigin, GWorkspace);
  AwaitSource(LHelperSource);
  Click('action-code');
  LInput := TMemo(GStudio.CodeView.InputFor('studio-code'));
  Check(LInput <> nil, 'ordinary retained source memo is mounted');
  LPosition := Pos('Result.Title := ''Room for ideas'';', LHelperSource);
  Check(LPosition > 0, 'owned title statement is exact before physical editing');
  LComputedSource := Copy(LHelperSource, 1, LPosition - 1) +
    TNyxText('Result.Title := ComposeWorkshopTitle;') +
    Copy(LHelperSource, LPosition + Length('Result.Title := ''Room for ideas'';'), MaxInt);
  LInput.Text := LComputedSource;
  Pump;
  LComputedSource := GStudio.Session.DraftSource;
  Check((Pos('Result.Title := ComposeWorkshopTitle;', LComputedSource) > 0) and
    GStudio.Session.SourceDraftPending,
    'native editing owns the computed constructor draft');
  Click('action-apply-source');
  AwaitSource(LComputedSource);
  Check(not GStudio.Session.SourceDraftPending and
    (GStudio.Session.Document.Title = 'Room for ideas'),
    'actual HTTP/FPC Apply executes the helper loop and admits its exact meaning');
  GRevision := Session.Field('revision').AsInteger;
  Check(Source = LComputedSource, 'MCP observes the exact executed handwritten source');
  Capture('http-native-handwritten.png');
  Save('computed-source.pas', LComputedSource);
  LArguments := NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)),
    NyxField('operations', TNyxDataValue.ParseJSON(
      '[{"op":"state","changes":[{"op":"create","name":"workshop-summary","kind":"text","value":"A quiet corner for code"}]},' +
      '{"op":"create","kind":"memo","id":"workshop-notes","parent":"home","properties":{"text":"Notes","placeholder":"Write an idea..."}},' +
      '{"op":"create","kind":"label","id":"workshop-preview","parent":"home","properties":{"text":"Your notes"}},' +
      '{"op":"state","changes":[{"op":"bind","owner":"workshop-notes","target":"value","name":"workshop-summary","kind":"text","direction":"two-way"},' +
      '{"op":"bind","owner":"workshop-preview","target":"text","name":"workshop-summary","kind":"text","direction":"from-state"}]}]'))]);
  LReceipt := Call('nyx_transaction', LArguments);
  Save('construction-receipt.json', LReceipt.ToJSON);
  Check(Call('nyx_transaction', LArguments).ToJSON = LReceipt.ToJSON,
    'exact confirmed retry reuses the same semantic constructor');
  LStatus := WaitConstruction(LReceipt);
  Save('construction-final.json', LStatus.ToJSON);
  LGroupSource := Source;
  Check((Pos(CHelperName, LGroupSource) > 0) and
    (Pos('INyxMemo', LGroupSource) > 0) and
    (Pos('NyxTextState', LGroupSource) > 0),
    'compiled group retains handwritten helper and typed memo/state authoring');
  AwaitSource(LGroupSource);
  Check((GStudio.Session.Document.Find(CMemoID) <> nil) and
    (GStudio.Session.Document.Find(CLabelID) <> nil),
    'native observer mounts the newly constructed binding owners');
  Capture('http-native-bound-group.png');
  Save('group-source.pas', LGroupSource);
  LFrame := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('direction', NyxData('undo'))]));
  GRevision := LFrame.Field('revision').AsInteger;
  Check(Source = LComputedSource, 'one semantic Undo restores exact preceding Pascal');
  AwaitSource(LComputedSource);
  Check(GStudio.Session.Document.Find(CMemoID) = nil,
    'one semantic Undo removes the complete grouped construction');
  LFrame := Call('nyx_history', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('direction', NyxData('redo'))]));
  GRevision := LFrame.Field('revision').AsInteger;
  Check(Source = LGroupSource, 'one semantic Redo restores exact grouped Pascal');
  AwaitSource(LGroupSource);
  Check(GStudio.Session.Document.Find(CMemoID) <> nil,
    'native observation restores the exact grouped control owner');
  Save('terminal-session.json', Session.ToJSON);
end;

{ Application compilation is semantic and immutable. The artifact admission
  checks current source/output and its exact bounded manifest. The native job
  proves compilation; the browser pipe below additionally proves execution. }
function BuildApplication(const ATarget, AOutput: TNyxText): TNyxDataValue;
var
  LJob: TNyxText;
  LStarted: QWord;
  LArtifact: TNyxCompiledArtifact;
begin
  LJob := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)),
    NyxField('outputID', NyxData(AOutput)),
    NyxField('target', NyxData(ATarget)),
    NyxField('scope', NyxData('application'))])).Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    Result := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', NyxData(LJob)),
      NyxField('severity', NyxData('warning')), NyxField('limit', NyxData(10))]));
    Save('application-' + ATarget + '.json', Result.ToJSON);

    if Result.Field('state').AsText = 'succeeded' then
    begin
      Break;
    end;

    if (Result.Field('state').AsText = 'failed') or
      (Result.Field('state').AsText = 'cancelled') then
    begin
      raise Exception.Create('Owned application compiler job failed: ' + ATarget);
    end;
    Application.ProcessMessages;
    Sleep(25);
  until GetTickCount64 - LStarted >= 180000;
  Check(Result.Field('state').AsText = 'succeeded',
    'exact handwritten ' + ATarget + ' application compiles');
  LArtifact := AdmitNyxCompiledArtifact(Result);
  Check(LArtifact.Job = LJob, 'successful application artifact has exact job authority');
end;

{ This continuation reads an already accepted owned group; it does not recreate
  the source journey or author through browser automation. Native Interact owns
  an independent live store. Browser input runs the actual semantic job artifact
  through a fresh owned Chromium profile and ordinary keyboard events. }
procedure RunRuntime(const AOrigin, AAcceptedSourcePath: TNyxText);
const
  CMemo = '[data-node="workshop-notes"] textarea';
  CLabel = '[data-node="workshop-preview"]';
  CDefault = 'A quiet corner for code';
  CNativeEdit = 'An idea from the native preview';
  CBrowserEdit = 'An idea from the browser application';
var
  LAccepted: TNyxText;
  LDesign: TNyxText;
  LInput: TMemo;
  LLabel: TLabel;
  LStarted: QWord;
  LOutput: TNyxText;
  LStatus: TNyxDataValue;
  LBrowser: TNyxBrowserPipe;
  LValue: TNyxText;
begin
  GRevision := Session.Field('revision').AsInteger;
  LAccepted := Read(AAcceptedSourcePath);
  Check((GWorkspace.ID <> '') and (Source = LAccepted) and
    (Pos(CHelperName, LAccepted) > 0), 'runtime source is the exact accepted owned group');
  GStudio := TNyxNativeStudio.Create(GForm, GEvidence + 'native-projects', nil,
    NewNyxSharedNativeSourceCompilerFactory(AOrigin));
  GStudio.Run;
  GStudio.ConnectService(AOrigin, GWorkspace);
  AwaitSource(LAccepted);
  LDesign := TNyxCodec.Encode(GStudio.Session.Document);
  Click('action-preview');
  Click('action-canvas-expand');
  LInput := TMemo(GStudio.CanvasView.InputFor(CMemoID));
  LLabel := TLabel(GStudio.CanvasView.ControlFor(CLabelID));
  Check((LInput <> nil) and (LLabel <> nil), 'real native binding owner controls are mounted');
  Check((LInput.Text = CDefault) and (LLabel.Caption = CDefault),
    'native memo and label share the admitted typed default');
  LInput.Text := CNativeEdit;
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until (LLabel.Caption = CNativeEdit) or (GetTickCount64 - LStarted >= 5000);
  Check(LLabel.Caption = CNativeEdit, 'actual native memo input updates its live bound label');
  Check((TNyxCodec.Encode(GStudio.Session.Document) = LDesign) and
    (GStudio.Session.Source = LAccepted), 'native runtime input preserves the authored pair');
  Capture('http-native-live-binding.png');
  LOutput := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  LStatus := BuildApplication('lcl', LOutput);
  Check(LStatus.Field('diagnostics').Field('total').AsInteger = 0,
    'native handwritten application has zero compiler warnings');
  LStatus := BuildApplication('browser', LOutput);
  LBrowser := TNyxBrowserPipe.Create(AOrigin + '/' + LStatus.Field('artifact').AsText,
    GEvidence + 'browser', 1100, 800);
  try
    LStarted := GetTickCount64;
    repeat

      if LBrowser.TryFieldValue(CMemo, LValue) then
      begin
        Break;
      end;

      if LBrowser.RuntimeError <> '' then
      begin
        LBrowser.CaptureRuntimeError;
        raise Exception.Create('Compiled browser runtime failed; inspect its private receipt');
      end;
      Sleep(25);
    until GetTickCount64 - LStarted >= 30000;
    Check(LBrowser.TryFieldValue(CMemo, LValue) and (LValue = CDefault),
      'actual compiled browser memo starts with the admitted typed default');
    Check(Pos(CDefault, LBrowser.ElementHTML(CLabel)) > 0,
      'actual browser label shares the independent live default');
    LBrowser.ReplaceText(CMemo, CBrowserEdit);
    Check(LBrowser.TryFieldValue(CMemo, LValue) and (LValue = CBrowserEdit),
      'browser keyboard editing retains the exact input');
    Check(Pos(CBrowserEdit, LBrowser.ElementHTML(CLabel)) > 0,
      'actual browser keyboard input updates the live bound label');
    LBrowser.Capture('desktop-live-binding');
    LBrowser.Resize(390, 780);
    Check(LBrowser.TryFieldValue(CMemo, LValue) and (LValue = CBrowserEdit) and
      (Pos(CBrowserEdit, LBrowser.ElementHTML(CLabel)) > 0),
      'compact browser allocation retains the live binding values');
    LBrowser.Capture('compact-live-binding');
    Check(LBrowser.RuntimeError = '', 'browser execution reports no runtime exception');
  finally
    LBrowser.Free;
  end;
  Check((Session.Field('revision').AsInteger = GRevision) and (Source = LAccepted),
    'both runtime stores and application builds preserve the shared authored revision');
end;

var
  LID: TGUID;
begin
  GClient := nil;
  GStudio := nil;
  GForm := nil;
  GObserver := nil;
  try
    try
      Check((ParamCount = 5) or ((ParamCount = 6) and (ParamStr(6) = '--runtime-only')),
        'supply origin, MCP config, owned workspace, baseline/accepted source, NEW evidence root and optional --runtime-only');
      Check(Pos('http://127.0.0.1:', ParamStr(1)) = 1,
        'qualification uses the explicit existing local host');
      GWorkspace := NyxWorkspace(TNyxText(ParamStr(3)));
      GEvidence := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(5)));
      Check(not DirectoryExists(GEvidence), 'evidence directory is new');
      ForceDirectories(GEvidence);
      CreateGUID(LID);
      GOperationPrefix := TNyxText(GUIDToString(LID));
      Application.Initialize;
      GObserver := TFailureObserver.Create;
      Application.OnException := GObserver.Failed;
      GForm := TForm.Create(nil);
      GForm.SetBounds(40, 40, 1240, 900);
      GForm.Caption := 'Nyx source workshop';
      GForm.Show;
      GClient := TNyxMCPTestClient.Create(ParamStr(2), 'Scooty HTTP source workshop');

      if ParamCount = 6 then
      begin
        RunRuntime(TNyxText(ParamStr(1)), TNyxText(ParamStr(4)));
        WriteLn('PASS ', GChecks, ' native/browser live binding and semantic application checks');
      end
      else
      begin
        Run(TNyxText(ParamStr(1)), TNyxText(ParamStr(4)));
        WriteLn('PASS ', GChecks, ' actual HTTP/native/semantic source-workshop checks');
      end;
    except
      on LException: Exception do
      begin
        WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    FreeAndNil(GStudio);

    if GClient <> nil then
    begin
      GClient.Close;
    end;
    GClient.Free;
    GForm.Free;
    Application.OnException := nil;
    GObserver.Free;
  end;
end.
