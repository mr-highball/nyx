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

program nyx_progress_activity_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.binding, nyx.binding.types, nyx.schema, nyx.data, nyx.codec, nyx.codegen,
  nyx.source, nyx.contract, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.edits, nyx.studio.stateedits, nyx.studio.transactions, nyx.studio.session,
  nyx.studio.view, nyx.generated.view
  {$ifdef NYX_COMPILED_PROGRESS}, nyx.generated.progress{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, {$ifdef MSWINDOWS}Windows,{$endif}
    Classes, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics, IntfGraphics,
    FPWritePNG, LCLIntf, LCLType, nyx.render.lcl, nyx.progress.lcl{$endif};

{$ifndef PAS2JS}
type
  TCheckboxAccess = class(TCheckBox);
{$endif}

var
  GDocument: TNyxDocument;
  GChecks: Integer;
  GBefore: TNyxText;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GRenderer: TNyxBrowserRenderer;
  GTimer: Integer;
  {$else}
  GHost: TForm;
  GRenderer: TNyxLCLRenderer;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Task progress: ' + AReason);
  end;
  Inc(GChecks);
end;

{ The untouched English source is exported and built through authenticated MCP.
  Current-source mode binding and reusable presentation are explicit enrichment
  until the installed backend supplies that new capability. }
procedure Enrich(ADocument: TNyxDocument);
var
  LDefinition: INyxProgress;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LPage: TNyxNode;
  LToggle: INyxCheckbox;
begin
  ADocument.Find('progress-task').Configure.Indeterminate(True).Done;
  ADocument.Find('progress-task').Binds.Indeterminate(NyxBooleanState('job-unknown')).Done;
  LDefinition := NewNyxProgress('progress-definition').WithIndeterminate(True);
  LDefinition.Configure.Value(50).AccessibleName('Reusable task progress').Done;
  ADocument.AddComponent(LDefinition);
  LPage := ADocument.Find('progress-review');
  LToggle := NewNyxCheckbox('progress-unknown-toggle').WithText('Discovering the total');
  LToggle.Binds.Value(NyxBooleanState('job-unknown'), bdTwoWay).Done;
  LPage.Add(LToggle);
  LPage.Add(NewNyxHeading('reused-progress-title').WithText('Independent tasks'));
  ADocument.State.SetValue(NyxBooleanState('first-unknown'), False);
  ADocument.State.SetValue(NyxBooleanState('second-unknown'), True);
  LFirst := NewNyxComponent('first-progress');
  LFirst.Configure.Component(NyxComponent('progress-definition')).Done;
  LFirst.Binds.Indeterminate(NyxBooleanState('first-unknown')).Done;
  LPage.Add(LFirst);
  LSecond := NewNyxComponent('second-progress');
  LSecond.Configure.Component(NyxComponent('progress-definition')).Done;
  LSecond.Binds.Indeterminate(NyxBooleanState('second-unknown')).Done;
  LPage.Add(LSecond);
end;

procedure Shared;
var
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LRefused: Boolean;
  LProgress: INyxProgress;
  LButton: INyxButton;
  LProperties: TNyxPropertyInfos;
  LIndex: Integer;
  LFound: Boolean;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
  LRevision: Integer;
  LPairBefore: TNyxText;
  LPairAfter: TNyxText;
  LOperations: TNyxDataValue;
  LSession: TNyxStudioSession;
  LShell: TNyxDocument;
  LViewState: TNyxStudioViewState;
begin
  LSource := TNyxCodegen.Generate(GDocument);
  Check((Pos('.Indeterminate(True)', LSource) > 0) and
    (Pos('.Indeterminate(LJobUnknownBooleanState)', LSource) > 0),
    'crafted source uses typed Boolean configuration/binding calls');
  LRead := nil;
  LWorkspace := nil;
  try
    LRead := TNyxCodec.Decode(GBefore);
    Check(TNyxCodec.Encode(LRead) = GBefore, 'mode and bindings preserve exact persistence');
    FreeAndNil(LRead);
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = GBefore, 'typed source reader reconstructs exact reusable controls');
    FreeAndNil(LRead);
    FreeAndNil(LWorkspace);
    LRefused := False;
    try
      LRead := TNyxSourceWorkspace.PrepareDraft(
        StringReplace(LSource, '.Indeterminate(True)', '.Indeterminate(''true'')', []), LWorkspace);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'source admission refuses a behavioral string in place of Boolean');
  finally
    LRead.Free;
    LWorkspace.Free;
  end;
  LProgress := NewNyxProgress('typed-progress').WithIndeterminate(True);
  Check(LProgress.Node.Prop('indeterminate') = 'true', 'specialized fluent interface selects unknown work');
  LProgress.WithIndeterminate(False);
  Check(LProgress.Node.Prop('indeterminate') = 'false', 'specialized interface resumes known work');
  Check(NyxBindingKinds(LProgress.Node, bpIndeterminate) = [nskBoolean],
    'mode discovery exposes only Boolean state');
  LButton := NewNyxButton('unrelated');
  LButton.Node.Configure.Indeterminate(True).Done;
  Check(NyxBindingKinds(LButton.Node, bpIndeterminate) = [],
    'unsupported retained metadata does not invent a Button mode binding');
  LProperties := NyxProperties(LProgress.Node);
  LFound := False;
  for LIndex := 0 to High(LProperties) do
  begin

    if LProperties[LIndex].Key = 'indeterminate' then
    begin
      LFound := (LProperties[LIndex].ValueType = npBoolean) and
        (LProperties[LIndex].DefaultValue = 'false') and
        (LProperties[LIndex].Support.Browser = ncBasic) and
        (LProperties[LIndex].Support.Native = ncBasic);
    end;
  end;
  Check(LFound, 'ordinary Inspector descriptor supplies typed default and both-target help');
  LSession := TNyxStudioSession.Create(NyxProjectPair(GBefore, LSource));
  LShell := nil;
  try
    LSession.Select('progress-task');
    LViewState := DefaultNyxStudioViewState;
    LViewState.AdvancedProperties := True;
    LShell := BuildNyxStudioView(LSession, LViewState);
    Check((LShell.Find('inspector-indeterminate') <> nil) and
      (LShell.Find('inspector-indeterminate').Kind = 'select') and
      (LShell.Find('inspector-indeterminate').Prop('value') = 'true'),
      'ordinary Nyx-built Studio composes its typed mode field from the same descriptor');
  finally
    LShell.Free;
    LSession.Free;
  end;
  LRefused := False;
  try
    TNyxBindingSpec.Bound(bpIndeterminate, 'wrong', nskText, bdFromState);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'wrong state family refuses before binding publication');
  LRefused := False;
  try
    TNyxBindingSpec.Bound(bpIndeterminate, 'wrong', nskBoolean, bdTwoWay);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unknown-work mode remains a read-only state projection');

  LAgent := TNyxAgentSession.Create(NyxProjectPair(GBefore, LSource));
  try
    LRevision := LAgent.Revision;
    LPairBefore := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
    LOperations := NyxProjectTransaction([NyxStateStep(NyxStateBindingPatch([
      NyxBindControl(NyxBindingOwner('progress-task'), bpIndeterminate,
        NyxBooleanState('second-unknown'), bdFromState)]))]).ToData;
    LReply := LAgent.Call('nyx_transaction', 'Progress reviewer', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('change-progress-binding')),
      NyxField('operations', LOperations)]));
    Check(LReply.Field('revision').AsInteger > LRevision,
      'current semantic operation admits the typed progress binding');
    LPairAfter := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
    LAgent.Call('nyx_history', 'Progress reviewer', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('progress-undo')), NyxField('direction', NyxData('undo'))]));
    Check(LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText = LPairBefore, 'one semantic Undo restores exact design/source');
    LAgent.Call('nyx_history', 'Progress reviewer', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('progress-redo')), NyxField('direction', NyxData('redo'))]));
    Check(LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText = LPairAfter, 'one semantic Redo restores exact changed binding');
    LRefused := False;
    try
      LAgent.Call('nyx_transaction', 'Progress reviewer', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('stale-progress')), NyxField('operations', LOperations)]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText = LPairAfter), 'stale revision preserves the exact accepted pair');
  finally
    LAgent.Free;
  end;
end;

function RuntimeID(const ADesignID: TNyxText): TNyxText;
begin
  Result := ADesignID;

  if (ADesignID = 'first-progress') or (ADesignID = 'second-progress') then
  begin
    Result := ADesignID + '/progress-definition';
  end;
end;

procedure Mode(const ADesignID: TNyxText; AUnknown: Boolean; AValue: Integer);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LControl: TJSHTMLProgressElement;
  {$else}LControl: TProgressBar;{$endif}
begin
  LID := RuntimeID(ADesignID);
  Check(GRenderer.Root.Find(LID).Prop('value') = IntToStr(AValue),
    'realized tree retains the current Integer value during either mode');
  {$ifdef PAS2JS}
  LControl := TJSHTMLProgressElement(GRenderer.ElementFor(LID));

  if AUnknown then
  begin
    Check(not LControl.hasAttribute('value') and (LControl.position = -1),
      'actual HTML control is indeterminate');
    Check(not LControl.hasAttribute('aria-valuenow'), 'unknown work announces no stale numeric value');
  end
  else
  begin
    Check(LControl.hasAttribute('value') and (LControl.value = AValue),
      'actual HTML control resumes the retained value');
    Check(LControl.getAttribute('aria-valuenow') = IntToStr(AValue),
      'known work announces its current Integer value');
  end;
  {$else}
  LControl := TProgressBar(GRenderer.ControlFor(LID));
  Check((LControl.Style = pbstMarquee) = AUnknown,
    'actual LCL control uses the proper native progress style');
  Check(LControl.Position = AValue, 'native progress retains its current Integer position');
  {$endif}
end;

procedure Cleanup;
begin
  GRenderer.Free;
  GRenderer := nil;
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
  end;
  GHost := nil;
  {$else}FreeAndNil(GHost);{$endif}
  FreeAndNil(GDocument);
end;

{$ifdef PAS2JS}
procedure AwaitCapture;
begin

  if document.body.getAttribute('data-capture-observed') = 'task-progress' then
  begin
    window.clearInterval(GTimer);
    GTimer := -1;
    Cleanup;
    document.body.setAttribute('data-progress-checks', IntToStr(GChecks));
    document.body.setAttribute('data-progress', 'passed');
  end;
end;
{$else}
{ Compare displayed pixels at the owned progress location over real message-loop
  time. A style property alone did not establish native activity. }
procedure NativeActivity;
type
  TRow = array of TColor;
var
  LControl: TNyxLCLProgress;
  LTimer: TTimer;
  LIndex: Integer;
  LBefore: TRow;
  LAfter: TRow;
  LChanged: Boolean;
  LStarted: QWord;

  function Sample: TRow;
  var
    LBitmap: TBitmap;
    LDC: HDC;
    LPoint: TPoint;
    LX: Integer;
  begin
    Result := nil;
    Application.ProcessMessages;
    GHost.Update;
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(GHost.ClientWidth, GHost.ClientHeight);
      LDC := GetDC(GHost.Handle);
      try

        if not BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
          LDC, 0, 0, SRCCOPY) then
        begin
          raise Exception.Create('Displayed activity sample could not be captured');
        end;
      finally
        ReleaseDC(GHost.Handle, LDC);
      end;
      LPoint := GHost.ScreenToClient(LControl.ClientToScreen(Point(0, 0)));
      SetLength(Result, LControl.Width);
      for LX := 0 to High(Result) do
      begin
        Result[LX] := LBitmap.Canvas.Pixels[LPoint.X + LX, LPoint.Y + LControl.Height div 2];
      end;
    finally
      LBitmap.Free;
    end;
  end;

begin
  LControl := TNyxLCLProgress(GRenderer.ControlFor('progress-task'));
  LTimer := nil;
  for LIndex := 0 to LControl.ComponentCount - 1 do
  begin

    if LControl.Components[LIndex] is TTimer then
    begin
      LTimer := TTimer(LControl.Components[LIndex]);
    end;
  end;
  Check(LTimer <> nil, 'native activity owns its independent timer');
  Check(LTimer.Enabled, 'shown unknown progress runs its owned timer');
  LBefore := Sample;
  LStarted := GetTickCount64;
  LChanged := False;
  repeat
    Application.ProcessMessages;
    Sleep(80);
    LAfter := Sample;
    for LIndex := 0 to High(LBefore) do
    begin

      if LBefore[LIndex] <> LAfter[LIndex] then
      begin
        LChanged := True;
        Break;
      end;
    end;
  until LChanged or (GetTickCount64 - LStarted > 1600);
  Check(LChanged, 'unknown progress actually animates displayed native pixels');
  GRenderer.Root.Find('progress-task').Configure.Visible(False).Done;
  GRenderer.Sync;
  Application.ProcessMessages;
  Check(not LTimer.Enabled, 'hidden native activity parks its timer');
  GRenderer.Root.Find('progress-task').Configure.Visible(True).Done;
  GRenderer.Sync;
  Application.ProcessMessages;
  Check(LTimer.Enabled, 'reshown native activity resumes its timer');
  GRenderer.State.SetValue(NyxBooleanState('job-unknown'), False);
  Check(not LTimer.Enabled, 'known progress stops the native activity timer');
  GRenderer.State.SetValue(NyxBooleanState('job-unknown'), True);
end;

procedure Capture(const APath: String);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LDC: HDC;
begin
  GHost.BringToFront;
  GHost.Update;
  Application.ProcessMessages;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GHost.ClientWidth, GHost.ClientHeight);
    LDC := GetDC(GHost.Handle);
    try
      Check(BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
        LDC, 0, 0, SRCCOPY), 'owned displayed native progress client captured');
    finally
      ReleaseDC(GHost.Handle, LDC);
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

procedure Actual;
var
  {$ifdef PAS2JS}LIdentity: TJSHTMLElement;
  {$else}LIdentity: TControl;{$endif}
begin
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  GHost.style.cssText := 'width:100%;min-height:360px;';
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 860, 390);
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Find('progress-review'), GHost);
  {$ifndef PAS2JS}
  GHost.Show;
  Application.ProcessMessages;
  {$endif}
  Mode('progress-task', True, 35);
  Mode('first-progress', False, 50);
  Mode('second-progress', True, 50);
  {$ifdef PAS2JS}LIdentity := GRenderer.ElementFor('progress-task');
  {$else}LIdentity := GRenderer.ControlFor('progress-task');{$endif}
  GRenderer.State.SetValue(NyxIntegerState('job-completed'), 65);
  Mode('progress-task', True, 65);
  {$ifdef PAS2JS}
  GRenderer.InputFor('progress-unknown-toggle').click;
  {$else}
  {$ifdef MSWINDOWS}
  Windows.SendMessage(TWinControl(GRenderer.InputFor('progress-unknown-toggle')).Handle, BM_CLICK, 0, 0);
  {$else}
  TCheckboxAccess(GRenderer.InputFor('progress-unknown-toggle')).Toggle;
  {$endif}
  Application.ProcessMessages;
  {$endif}
  Check(not GRenderer.State.GetValue(NyxBooleanState('job-unknown')),
    'actual toggle writes Boolean state and resumes numeric progress');
  Mode('progress-task', False, 65);
  {$ifdef PAS2JS}
  Check(GRenderer.ElementFor('progress-task') = LIdentity, 'mode refresh retains exact DOM identity');
  {$else}
  Check(GRenderer.ControlFor('progress-task') = LIdentity, 'mode refresh retains exact LCL control identity');
  {$endif}
  GRenderer.State.SetValue(NyxBooleanState('first-unknown'), True);
  Mode('first-progress', True, 50);
  Mode('second-progress', True, 50);
  GRenderer.State.SetValue(NyxBooleanState('second-unknown'), False);
  Mode('first-progress', True, 50);
  Mode('second-progress', False, 50);
  GRenderer.State.SetValue(NyxBooleanState('job-unknown'), True);
  Mode('progress-task', True, 65);
  GRenderer.Sync;
  Mode('progress-task', True, 65);
  GRenderer.Root.Find('progress-task').Configure.Minimum(-25).Maximum(75).Done;
  GRenderer.Sync;
  Mode('progress-task', True, 65);
  GRenderer.State.SetValue(NyxBooleanState('job-unknown'), False);
  {$ifdef PAS2JS}
  Check((TJSHTMLProgressElement(GRenderer.ElementFor('progress-task')).max = 100) and
    (TJSHTMLProgressElement(GRenderer.ElementFor('progress-task')).value = 90) and
    (GRenderer.ElementFor('progress-task').getAttribute('aria-valuemin') = '-25') and
    (GRenderer.ElementFor('progress-task').getAttribute('aria-valuenow') = '65'),
    'known HTML interval resumes normalized geometry with exact semantic accessibility bounds');
  {$else}
  Check((TProgressBar(GRenderer.ControlFor('progress-task')).Min = -25) and
    (TProgressBar(GRenderer.ControlFor('progress-task')).Max = 75) and
    (TProgressBar(GRenderer.ControlFor('progress-task')).Position = 65),
    'known native interval resumes the retained exact bounds and position');
  {$endif}
  GRenderer.Root.Find('progress-task').Configure.Minimum(0).Maximum(100).Done;
  GRenderer.State.SetValue(NyxBooleanState('job-unknown'), True);
  GRenderer.Sync;
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime progress never rewrites the authored pair/defaults');
end;

var
  LExpected: TNyxDocument;
  LSource: TNyxText;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin
  LExpected := nil;
  {$ifdef PAS2JS}GTimer := -1;{$else}Application.Initialize;{$endif}
  try
    GDocument := nyx.generated.view.BuildNyxDocument;
    Enrich(GDocument);
    {$ifdef NYX_COMPILED_PROGRESS}
    LExpected := GDocument;
    GDocument := nyx.generated.progress.BuildNyxDocument;
    Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(LExpected),
      'compiled crafted source reconstructs exact progress/reuse/defaults');
    FreeAndNil(LExpected);
    {$endif}
    GBefore := TNyxCodec.Encode(GDocument);
    Shared;
    LSource := TNyxCodegen.Generate(GDocument, 'nyx.generated.progress');
    {$ifndef PAS2JS}

    if ParamCount >= 1 then
    begin
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
    Actual;
    {$ifndef PAS2JS}NativeActivity;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-capture-checkpoint', 'task-progress');
    GTimer := window.setInterval(@AwaitCapture, 25);
    {$else}

    if ParamCount >= 2 then
    begin
      Capture(ParamStr(2));
    end;
    Cleanup;
    WriteLn('PASS ', GChecks, ' typed progress/source/semantic/actual-control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}

      if GTimer >= 0 then
      begin
        window.clearInterval(GTimer);
        GTimer := -1;
      end;
      {$endif}
      LExpected.Free;
      Cleanup;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-progress', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
