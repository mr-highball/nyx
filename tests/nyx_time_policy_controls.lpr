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

program nyx_time_policy_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, nyx.render.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.times.editor, nyx.contract,
  nyx.model, nyx.codec, nyx.generated.time, nyx.studio.projects,
  nyx.studio.session, nyx.studio.sourcejobs, nyx.studio.view,
  nyx.behavior, nyx.studio.inspector;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TEditAccess = class(TCustomEdit);
  TControlAccess = class(TControl);
  {$endif}
  { Real public Inspector controls enter the ordinary independent paired source
    queue. No accepted project is edited directly by these field changes. Hosts
    belong only to this fixture; active editor/MCP state is never replaced. }
  TClockReview = class
  private
    FHost: THost;
    FRenderer: TRenderer;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FShell: TNyxDocument;
    FStage: Integer;
    FError: TNyxText;
    FRefusal: TNyxText;
    FBefore: TNyxText;
    FAfter: TNyxText;
    procedure Refresh;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Change(AField: TNyxTimeDomainEditorField; const AValue: TNyxText);
    procedure Click(AField: TNyxTimeDomainEditorField);
    function Pair: TNyxText;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TClockReview;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  GStarted: Double;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Clock Inspector: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
{ Export the exact accepted pair from the physical source-queue consumer. The
  independent reconstruction program compiles these bytes rather than asking
  the generator for another builder that could hide source publication errors. }
procedure SaveAccepted(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + AName,
    fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

constructor TClockReview.Create(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
begin
  inherited Create;
  LDocument := BuildNyxDocument;
  try
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
  finally
    LDocument.Free;
  end;
  FSession.Select('start-time');
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('main'));
  FHost.style.cssText := 'max-width:640px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(24, 24, 620, 840);
  FHost.Show;
  {$endif}
  Refresh;
end;

destructor TClockReview.Destroy;
begin
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}
  FHost.remove;
  {$else}
  FHost.Free;
  {$endif}
  inherited Destroy;
end;

function TClockReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TClockReview.Refresh;
var
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
begin
  LState := DefaultNyxStudioViewState;
  LState.InspectorTab := nitProperties;
  LShell := BuildNyxStudioView(FSession, LState);
  try
    Check(LShell.Find('inspector-time-domain') <> nil,
      'ordinary Studio composes the public clock editor');
    FRenderer.Render(LShell, LShell.Find('inspector-time-domain'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
end;

procedure TClockReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState in [nssFailed, nssRejected, nssStale] then
  begin
    FError := AMessage;
  end;

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;

procedure TClockReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  try
    FCommands.Route(ANode, AEvent.Trigger, FRenderer.Root);
  except
    on LException: ENyxContract do
    begin
      { Like Studio's controller, surface a capture refusal without enqueueing
        invalid fields or replacing the accepted design/source. }
      FRefusal := LException.Message;
    end;
  end;
end;

procedure TClockReview.Change(AField: TNyxTimeDomainEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TControl;
  {$endif}
begin
  LID := NyxTimeDomainEditorFieldID('inspector-time-domain', AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'actual clock-policy field is mounted');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TCustomEdit(LInput).Text := AValue;

    if AField in [ntfMinimum, ntfMaximum] then
    begin
      TEditAccess(LInput).OnEditingDone(LInput);
    end;
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue,
    'physical field retains its exact proposal');
end;

procedure TClockReview.Click(AField: TNyxTimeDomainEditorField);
var
  LID: TNyxText;
begin
  LID := NyxTimeDomainEditorFieldID('inspector-time-domain', AField);
  {$ifdef PAS2JS}
  FRenderer.ElementFor(LID).click;
  {$else}
  TControlAccess(FRenderer.ControlFor(LID)).Click;
  {$endif}
end;

function TClockReview.Step: Boolean;
var
  LDomain: TNyxValueDomain;
begin
  Result := False;

  if FError <> '' then
  begin
    raise Exception.Create(FError);
  end;

  if FCommands.Busy then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        FBefore := Pair;
        Change(ntfMinimum, '23:00:00.000');
        Change(ntfMaximum, '01:00:00.000');
        Change(ntfStepMode, NyxTimeDomainEditorStepName(ntsMilliseconds));
        Change(ntfMilliseconds, '500');
        Check(Pair = FBefore, 'field proposals preserve accepted design/source/history');
        FStage := 1;
        Click(ntfApply);
      end;
    1:
      begin
        Check(FCommands.State = nssApplied, 'actual Apply uses the independent paired queue');
        Check(FSession.Selected.Contract.FindValue(LDomain) and LDomain.ClockTime and
          (LDomain.TimeStepMilliseconds = 500), 'the authored owner receives the typed clock policy');
        Check((LDomain.ToData.Field('min').AsText = '23:00:00.000') and
          (LDomain.ToData.Field('max').AsText = '01:00:00.000'),
          'overnight bounds retain millisecond precision');
        Check(Pos('.StepMilliseconds(500)', FSession.Source) > 0,
          'near-real-time source uses crafted typed authoring');
        FAfter := Pair;
        {$ifndef PAS2JS}
        SaveAccepted('nyx.generated.time.pas', FSession.Source);
        SaveAccepted('design.nyx.json', FSession.Save);
        {$endif}
        FSession.Undo;
        Check(Pair = FBefore, 'one Undo restores the exact paired original');
        FSession.Redo;
        Check(Pair = FAfter, 'one Redo restores the exact authored pair');
        Refresh;
        Change(ntfMilliseconds, '1.5');
        FStage := 2;
        Click(ntfApply);
      end;
    2:
      begin
        Check((Pair = FAfter) and not FCommands.Busy and (FRefusal <> ''),
          'fractional step refuses without changing the accepted pair');
        Refresh;
        FStage := 3;
        Click(ntfInherit);
      end;
    3:
      begin
        Check(not FSession.Selected.Contract.FindValue(LDomain),
          'actual Restore removes only the local value declaration');
        Check(Pair = FBefore, 'restoring inheritance retains the exact original compound contract');
        Result := True;
      end;
  else
    raise Exception.Create('Unknown clock Inspector qualification stage');
  end;
end;

{$ifdef PAS2JS}
procedure Tick;
begin
  try

    if window.performance.now - GStarted > 30000 then
    begin
      raise Exception.Create('Clock Inspector qualification timed out');
    end;

    if GReview.Step then
    begin
      document.body.setAttribute('data-time-policy', 'passed');
      document.body.setAttribute('data-time-policy-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Tick, 10);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-time-policy', 'failed');
      document.body.setAttribute('data-time-policy-error', LException.Message);
    end;
  end;
end;
{$endif}

var
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LSource: TNyxText;
  LStarted: QWord;
  LDone: Boolean;
  {$endif}
begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := function(AEvent: TJSProgressEvent): Boolean
    begin
      Result := True;
      try

        if GRequest.status <> 200 then
        begin
          raise Exception.Create('Exact clock companion HTTP source is unavailable');
        end;
        GReview := TClockReview.Create(GRequest.responseText);
        GStarted := window.performance.now;
        Tick;
      except
        on LException: Exception do
        begin
          document.body.setAttribute('data-time-policy', 'failed');
          document.body.setAttribute('data-time-policy-error', LException.Message);
        end;
      end;
    end;
  GRequest.send;
  {$else}
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply exact clock source and an owned export directory');
    end;
    ForceDirectories(ParamStr(2));
    Application.Initialize;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LSource, LStream.Size);

      if LSource <> '' then
      begin
        LStream.ReadBuffer(LSource[1], Length(LSource));
      end;
    finally
      LStream.Free;
    end;
    GReview := TClockReview.Create(LSource);
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      LDone := GReview.Step;

      if not LDone then
      begin
        Sleep(5);
      end;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Native clock Inspector qualification timed out');
      end;
    until LDone;
    WriteLn('PASS ', GChecks, ' actual native clock Inspector/queue checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL after ', GChecks, ' / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GReview.Free;
  {$endif}
end.
