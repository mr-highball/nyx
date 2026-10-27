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

program nyx_outputs_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.studio.outputs,
  nyx.studio.projects, nyx.studio.menu, nyx.studio.workspaces
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.studio.lcl,
    nyx.studio.agents, nyx.studio.exchange, nyx.studio.outputstore, nyx.files,
    nyx.files.lcl{$endif};

type
  { Controlled service outcomes are qualification data. They grant no compiler
    or file authority and affect only one owned in-memory profile request. }
  TReplyMode = (rmOkay, rmUnavailable, rmMalformed);

  {$ifdef PAS2JS}
  { Studio owns its ordinary controls; only the external XHR host is substituted.
    The controlled object follows the asynchronous XHR property/method contract,
    never replaces window.XMLHttpRequest and never contacts the installed API. }
  TOutputStudio = class(TNyxStudio)
  protected
    function CreateConfigurationRequest: TJSXMLHttpRequest; override;
  end;
  {$else}
  { Controlled private exchange owns one request and one timer callback. It
    borrows the independent real agent session and never delivers inside Post.
    Profile transport/delay is substituted; ordinary agent admission is real.
    Successful saves use the production byte writer in a private file, then
    reopen its exact Unicode data. This grants no installed-host/socket authority. }
  TOutputExchange = class(TNyxStudioEditorExchange)
  private
    FBody: TNyxDataValue;
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FConnect: Boolean;
    procedure Deliver;
  public
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    procedure Pump;
    procedure ReleaseProfile;
  end;

  { Ordinary native controller with an owned exchange factory. The borrowed
    host/form and all controls retain their normal Nyx/LCL lifetimes. }
  TOutputStudio = class(TNyxNativeStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

  { Exposes Click on a borrowed renderer-owned control, never its ownership. }
  TControlInput = class(TControl);

  { Outlives test widgets and captures callback failures without a modal dialog. }
  TCallbackFailure = class
  public
    Error: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

const
  COldCompiler: TNyxText = 'C:/old-🌙/compiler.exe';
  CNewCompiler: TNyxText = 'C:/new-🌙/compiler.exe';
  CTemporaryCompiler: TNyxText = 'C:/new-🌙/temporary.exe';
  CNewRuntime: TNyxText = 'C:/new-🌙/rtl.js';

var
  GStudio: TOutputStudio;
  GStored: TNyxText;
  GPosted: TNyxText;
  GReplyText: TNyxText;
  GReplyStatus: Integer;
  GPending: Boolean;
  GPrepared: Boolean;
  GSaves: Integer;
  GMode: TReplyMode;
  GStage: Integer;
  GChecks: Integer;
  GDone: Boolean;
  GStarted: Double;
  GBefore: TNyxText;
  GOriginal: TNyxProjectPair;
  GPendingProject: TNyxProjectPair;
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  {$else}
  GCore: TNyxAgentSession;
  GExchange: TOutputExchange;
  GForm: TForm;
  GFailure: TCallbackFailure;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Prepare;
var
  LProfile: TNyxOutputConfiguration;
begin

  if not GPending or GPrepared then
  begin
    raise Exception.Create('Prepare requires exactly one unprepared owned profile request');
  end;
  GReplyStatus := 200;

  if GMode = rmUnavailable then
  begin
    GReplyStatus := 503;
    GReplyText := '{}';
  end
  else if GMode = rmMalformed then
  begin
    GReplyText := '{"unexpected":true}';
  end
  else
  begin

    if GPosted <> '' then
    begin
      LProfile := TNyxOutputConfiguration.Decode(GPosted);
      try
        {$ifdef PAS2JS}
        GStored := LProfile.Encode;
        {$else}
        { Keep controlled delivery/delay, but use the production native byte
          writer and reopen its real file. This qualifies persisted version4
          input from ordinary Apply without claiming a real HTTP exchange. }
        SaveNyxOutputProfile(TNyxText(ParamStr(1)) + TNyxText('.profile'), LProfile);
        GStored := ReadNyxTextFile(TNyxText(ParamStr(1)) + TNyxText('.profile')).Text;
        Check(GStored = LProfile.Encode,
          'ordinary Apply reopens the exact production-written Unicode profile bytes');
        {$endif}
        Inc(GSaves);
      finally
        LProfile.Free;
      end;
    end;
    GReplyText := GStored;
  end;
  GMode := rmOkay;
  GPrepared := True;
end;

{$ifdef PAS2JS}
procedure RequestOpen(const AMethod, AURL: String; AAsync: Boolean);
begin

  if (AURL <> 'api/configuration') or not AAsync or
    ((AMethod <> 'GET') and (AMethod <> 'POST')) then
  begin
    raise Exception.Create('Unexpected configuration request contract');
  end;
end;

procedure RequestHeader(const AName, AValue: String);
begin

  if (AName <> 'Content-Type') or (AValue <> 'application/json') then
  begin
    raise Exception.Create('Unexpected configuration content type');
  end;
end;

procedure RequestSend(ABody: JSValue);
begin
  GPosted := '';

  if not isUndefined(ABody) then
  begin
    GPosted := String(ABody);
  end;
  GPending := True;
  GPrepared := False;
end;

procedure RequestAbort;
begin
  GPending := False;
end;

function TOutputStudio.CreateConfigurationRequest: TJSXMLHttpRequest;
var
  LObject: TJSObject;
begin
  LObject := TJSObject.new;
  LObject['open'] := @RequestOpen;
  LObject['setRequestHeader'] := @RequestHeader;
  LObject['send'] := @RequestSend;
  LObject['abort'] := @RequestAbort;
  GRequest := TJSXMLHttpRequest(LObject);
  Result := GRequest;
end;
{$else}
procedure TCallbackFailure.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

function TOutputStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  Result := TOutputExchange.Create;
  { Studio also requests an independent completion exchange. Pump only the
    first, ordinary editor channel; replacing this pointer with the unused
    completion channel strands the actual connection before stage zero. }

  if GExchange = nil then
  begin
    GExchange := TOutputExchange(Result);
  end;
end;

destructor TOutputExchange.Destroy;
begin

  if GExchange = Self then
  begin
    GExchange := nil;
  end;
  CancelRequest;
  CancelTick;
  inherited Destroy;
end;

procedure TOutputExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin

  if Assigned(FReply) then
  begin
    raise Exception.Create('Owned exchange already has a request');
  end;
  FBody := TNyxDataValue.ParseJSON(ABody);
  FConnect := AConnect;
  FReply := AReply;

  if FBody.Field('op').AsText = 'build' then
  begin

    if FBody.Field('build').Field('mode').AsText <> 'profile' then
    begin
      raise Exception.Create('Configuration fixture must never request a compiler job');
    end;
    GPosted := '';

    if NyxAgentHas(FBody.Field('build'), 'profile') then
    begin
      GPosted := FBody.Field('build').Field('profile').ToJSON;
    end;
    GPending := True;
    GPrepared := False;
  end;
end;

procedure TOutputExchange.CancelRequest;
begin
  FReply := nil;
  GPending := False;
end;

procedure TOutputExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  FTick := ATick;
end;

procedure TOutputExchange.CancelTick;
begin
  FTick := nil;
end;

procedure TOutputExchange.Deliver;
var
  LState: TNyxDataValue;
  LBuild: TNyxDataValue;
  LFields: array of TNyxDataField;
  LCount: Integer;
  LIndex: Integer;
  LReply: TNyxEditorReply;
begin
  LReply := FReply;
  FReply := nil;
  LBuild := NyxNull;

  if GPending then
  begin
    LState := GCore.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', FBody.Field('after'))]));

    if GReplyStatus = 200 then
    begin
      LBuild := NyxObject([
        NyxField('outputID', NyxData('0123456789abcdef0123456789abcdef')),
        NyxField('profile', TNyxDataValue.ParseJSON(GReplyText))]);
    end
    else
    begin
      LBuild := NyxObject([NyxField('state', NyxData('rejected')),
        NyxField('error', NyxData('Owned profile service refused the request'))]);
    end;
    GPending := False;
  end
  else
  begin
    LState := GCore.Exchange(FBody);
  end;
  LCount := LState.Count;
  SetLength(LFields, LCount + 1 + Ord(LBuild.Kind <> ndNull));
  for LIndex := 0 to LCount - 1 do
  begin
    LFields[LIndex] := NyxField(LState.Key(LIndex), LState.Field(LState.Key(LIndex)));
  end;
  LFields[LCount] := NyxField('editorBuilds', NyxData(True));

  if LBuild.Kind <> ndNull then
  begin
    LFields[LCount + 1] := NyxField('buildReply', LBuild);
  end;
  LState := NyxObject(LFields);

  if FConnect then
  begin
    LState := NyxObject([NyxField('token', NyxData('owned-profile-fixture')),
      NyxField('endpoint', NyxData('')), NyxField('state', LState)]);
  end;
  LReply(200, LState.ToJSON);
end;

procedure TOutputExchange.ReleaseProfile;
begin

  if not GPrepared then
  begin
    raise Exception.Create('Profile reply is not prepared');
  end;
  Deliver;
end;

procedure TOutputExchange.Pump;
var
  LTick: TNyxEditorTick;
begin

  if Assigned(FReply) and not GPending then
  begin
    Deliver;
  end
  else if not Assigned(FReply) and Assigned(FTick) then
  begin
    LTick := FTick;
    FTick := nil;
    LTick;
  end;
end;
{$endif}

procedure ReleaseReply;
{$ifdef PAS2JS}
var
  LReply: TJSOnReadyStateChangeHandler;
{$endif}
begin
  {$ifdef PAS2JS}
  LReply := GRequest.onreadystatechange;
  TJSObject(GRequest)['status'] := GReplyStatus;
  TJSObject(GRequest)['responseText'] := GReplyText;
  TJSObject(GRequest)['readyState'] := TJSXMLHttpRequest.DONE;
  GPending := False;
  LReply;
  {$else}
  GExchange.ReleaseProfile;
  {$endif}
end;

procedure Click(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]')).click;
  {$else}
  TControlInput(GStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

procedure History(AUndo: Boolean);
begin
  {$ifdef PAS2JS}
  Click(NyxStudioActionMenuID);

  if AUndo then
  begin
    Click('studio-menu-undo');
  end
  else
  begin
    Click('studio-menu-redo');
  end;
  {$else}

  if AUndo then
  begin
    Click('action-undo');
  end
  else
  begin
    Click('action-redo');
  end;
  {$endif}
end;

procedure Outputs;
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LFace := TJSHTMLElement(document.querySelector('[data-node="action-outputs"]'));

  if (LFace = nil) or (LFace.getBoundingClientRect.height = 0) then
  begin
    Click(NyxStudioActionMenuID);
    Click('studio-menu-project');
    Click('studio-menu-outputs');
    Exit;
  end;
  {$endif}
  Click('action-outputs');
end;

procedure Toggle(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(document.querySelector('[data-node="' + AID + '"] input')).click;
  {$else}
  TCheckBox(GStudio.ShellView.InputFor(AID)).Checked :=
    not TCheckBox(GStudio.ShellView.InputFor(AID)).Checked;
  {$endif}
end;

function Value(const AID: TNyxText): TNyxText;
begin
  Result := GStudio.ShellView.RootFor(AID).Find(AID).Prop('value');
end;

procedure FocusInput(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(GStudio.ShellView.InputFor(AID)).focus;
  {$else}
  TWinControl(GStudio.ShellView.InputFor(AID)).SetFocus;
  {$endif}
end;

procedure CheckFocus(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  Check(document.activeElement = GStudio.ShellView.InputFor(AID),
    'output publication retains actual text-field focus');
  {$else}
  Check(TWinControl(GStudio.ShellView.InputFor(AID)).Focused,
    'output publication retains actual text-field focus');
  {$endif}
end;

function InputValue(const AID: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(GStudio.ShellView.InputFor(AID)).value;
  {$else}
  Result := TEdit(GStudio.ShellView.InputFor(AID)).Text;
  {$endif}
end;

procedure TextInput(const AID, AText: TNyxText);
{$ifdef PAS2JS}
var
  LInput: TJSHTMLInputElement;
{$else}
var
  LInput: TEdit;
{$endif}
begin
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(document.querySelector('[data-node="' + AID + '"] input'));
  LInput.value := AText;
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  LInput := TEdit(GStudio.ShellView.InputFor(AID));
  LInput.Text := AText;
  LInput.OnChange(LInput);
  {$endif}
end;

function StoredOpening: Boolean;
var
  LProfile: TNyxOutputConfiguration;
begin
  LProfile := TNyxOutputConfiguration.Decode(GStored);
  try
    Result := LProfile.FileOpening;
  finally
    LProfile.Free;
  end;
end;

function StoredHTTP: Boolean;
var
  LProfile: TNyxOutputConfiguration;
begin
  LProfile := TNyxOutputConfiguration.Decode(GStored);
  try
    Result := LProfile.HTTP;
  finally
    LProfile.Free;
  end;
end;

procedure Step;
var
  LProfile: TNyxOutputConfiguration;
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  if GExchange <> nil then
  begin
    GExchange.Pump;
  end;
  {$endif}

  if GStudio.PresentationPending or GStudio.SourceBusy then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        {$ifndef PAS2JS}

        if GCore = nil then
        begin
          GCore := TNyxAgentSession.Create(GStudio.ProjectSnapshot);
          GStudio.ConnectService('http://owned-fixture.invalid', Default(TNyxWorkspaceRef));
          Exit;
        end;

        if not GStudio.Agents.Connected then
        begin
          Exit;
        end;
        {$endif}
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'ordinary opening retains the unfinished Pascal draft before configuration');
        Outputs;
      end;
    1:
      begin

        if not GPending then
        begin
          Exit;
        end;
        Prepare;
        Check(Value('output-file-saving') = 'false', 'saving starts disabled without a selected target');
        Toggle('output-file-saving');
        ReleaseReply;
      end;
    2:
      begin
        Check(Value('output-file-opening') = 'false', 'opening starts disabled without a selected target');
        Check(Value('output-http') = 'false', 'HTTP starts disabled without a target or compiler');
        Check((Value('output-file-saving') = 'true') and (GSaves = 0),
          'delayed initial observation retains local input without unsolicited persistence');
        Click('action-save-outputs');
      end;
    3:
      begin

        if not GPending then
        begin
          Exit;
        end;
        Check(GPosted <> '', 'ordinary Apply dispatches a save');
        Prepare;
        Click('action-reload-outputs');
        Check(GPrepared and (GPosted <> ''), 'overlapping Reload retains the original pending Apply');
        Toggle('output-file-opening');
        Toggle('output-http');
        ReleaseReply;
      end;
    4:
      begin
        Check((Value('output-file-opening') = 'true') and not StoredOpening,
          'late Apply retains newer opening edits without falsely persisting them');
        Check(Value('output-http') = 'true', 'late Apply also preserves the newer independent HTTP edit');
        Check(GSaves = 1, 'explicit Apply acknowledges its submitted snapshot only');
        LProfile := TNyxOutputConfiguration.Create;
        GStored := LProfile.Encode;
        LProfile.Free;
        Click('action-reload-outputs');
      end;
    5:
      begin

        if not GPending then
        begin
          Exit;
        end;
        Check(GPosted = '', 'Reload dispatches a read, never a save');
        Prepare;
        Toggle('output-file-saving');
        Toggle('output-file-saving');
        ReleaseReply;
      end;
    6:
      begin
        Check(Value('output-file-saving') = 'true', 'changed-back saving intent survives a delayed reload');
        Check(Value('output-file-opening') = 'false', 'reload adopts untouched saved opening despite earlier edits');
        Check(Value('output-http') = 'false', 'reload adopts untouched saved HTTP despite earlier edits');
        Check(GSaves = 1, 'reload does not silently publish retained edits');
        Click('action-save-outputs');
      end;
    7, 11, 14, 21:
      begin

        if not GPending then
        begin
          Exit;
        end;
        Prepare;
        ReleaseReply;

        if GPosted = '' then
        begin
          { Native retries re-observe their identity after a service refusal;
            await the subsequent explicit save without counting observations. }
          Exit;
        end;
      end;
    8:
      begin
        Check(GSaves = 2, 'a later Apply explicitly saves retained edits');
        Toggle('output-file-saving');
        GMode := rmUnavailable;
        Click('action-reload-outputs');
      end;
    9, 13, 19:
      begin

        if not GPending then
        begin
          Exit;
        end;
        Prepare;

        if GStage = 19 then
        begin
          TextInput('output-pas2js', CTemporaryCompiler);
          TextInput('output-pas2js', CNewCompiler);
          FocusInput('output-runtime');
        end;
        ReleaseReply;

        if GStage = 13 then
        begin
          Check(Value('output-file-opening') = 'true', 'malformed reload retains the independent local edit');
          Check(Value('output-http') = 'true', 'malformed reload retains the newer HTTP edit');
          Click('action-save-outputs');
        end;
      end;
    10:
      begin
        Check(Value('output-file-saving') = 'false', 'failed reload preserves local values');
        Check(GSaves = 2, 'failed reload publishes no profile');
        Click('action-save-outputs');
      end;
    12:
      begin
        Check(GSaves = 3, 'failed reload retains dirty intent for the next Apply');
        Toggle('output-file-opening');
        Toggle('output-http');
        GMode := rmMalformed;
        Click('action-reload-outputs');
      end;
    15:
      begin
        Check(GSaves = 4, 'malformed reload also retains edits until successful Apply');
        Check(StoredOpening, 'the exact newer opening policy is persisted by explicit Apply');
        Check(StoredHTTP, 'ordinary Apply persists the version4 HTTP choice after a refused reload');
        Click('output-browser');
      end;
    16:
      begin
        FocusInput('output-pas2js');
        TextInput('output-pas2js', COldCompiler);
        Click('action-save-outputs');
      end;
    17:
      begin

        if not GPending then
        begin
          Exit;
        end;
        CheckFocus('output-pas2js');
        Prepare;
        TextInput('output-pas2js', CNewCompiler);
        TextInput('output-runtime', CNewRuntime);
        ReleaseReply;
      end;
    18:
      begin
        Check(Value('output-pas2js') = CNewCompiler, 'late save preserves newer Unicode compiler path');
        Check(Value('output-runtime') = CNewRuntime, 'late save preserves independent runtime edit');
        Click('action-reload-outputs');
      end;
    20:
      begin
        Check(Value('output-pas2js') = CNewCompiler, 'reload retains changed-back path intent');
        Check(Value('output-runtime') = '', 'successful reload adopts an untouched field independently');
        Check(InputValue('output-runtime') = '', 'focused actual input displays its adopted saved value');
        CheckFocus('output-runtime');
        Click('action-save-outputs');
      end;
    22:
      begin
        LProfile := TNyxOutputConfiguration.Decode(GStored);
        try
          Check((LProfile.Field('pas2js') = CNewCompiler) and
            (LProfile.Field('runtime') = ''), 'final Apply preserves the exact independently reconciled paths');
        finally
          LProfile.Free;
        end;
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'configuration retains the exact accepted design/source/draft pair');
        History(True);
      end;
    23:
      begin
        { Open starts an independent history; it is deliberately not Adopt.
          Configuration must add no Undo entry to that empty history. }
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'Undo after opening has no configuration command and retains the unfinished pair');
        History(False);
      end;
    24:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'Redo after opening also retains the exact accepted pair and unfinished draft/base');
        Check(Value('output-pas2js') = CNewCompiler,
          'paired source history never rolls machine configuration backward');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if Clock - GStarted > 180000 then
    begin
      raise Exception.Create('Owned Outputs journey exceeded its budget at ' + IntToStr(GStage));
    end;
    Step;
    document.body.setAttribute('data-output-studio-stage', IntToStr(GStage));

    if GDone then
    begin
      document.body.setAttribute('data-output-studio', 'passed');
      document.body.setAttribute('data-output-studio-checks', IntToStr(GChecks));
      TJSHTMLElement(document.querySelector('[data-node="action-save-outputs"]')).scrollIntoView(TJSObject.new);
      Exit;
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-output-studio', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
    end;
  end;
end;
{$endif}

procedure InitializeProfile;
var
  LProfile: TNyxOutputConfiguration;
begin
  LProfile := TNyxOutputConfiguration.Create;
  try
    GStored := LProfile.Encode;
  finally
    LProfile.Free;
  end;
end;

begin
  InitializeProfile;
  {$ifdef PAS2JS}
  GStudio := TOutputStudio.Create;
  GStudio.Run(False);
  {$else}

  if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned output project directory');
  end;
  Application.Initialize;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(20, 20, 1280, 900);
  GForm.Show;
  GFailure := TCallbackFailure.Create;
  Application.OnException := GFailure.Failed;
  GStudio := TOutputStudio.Create(GForm, ParamStr(1));
  GStudio.Run;
  {$endif}
  GOriginal := GStudio.ProjectSnapshot;
  GPendingProject := GOriginal;
  GPendingProject.Pending := True;
  GPendingProject.DraftBase := GOriginal.Source;
  GPendingProject.Draft := GOriginal.Source + TNyxText(#10 + '// Unfinished output notes 🌙');
  GBefore := EncodeNyxProject(GPendingProject);
  GStudio.SourceCommands.OpenProject(GPendingProject);
  GStarted := Clock;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  try
    while not GDone do
    begin

      if Clock - GStarted > 180000 then
      begin
        raise Exception.Create('Owned Outputs journey exceeded its budget at ' + IntToStr(GStage));
      end;
      CheckSynchronize;
      Application.ProcessMessages;
      Step;
      Sleep(20);
    end;
    WriteLn('PASS ', GChecks, ' actual ordinary Studio Outputs checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL stage ', GStage, ' / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GCore.Free;
  GForm.Free;
  Application.OnException := nil;
  GFailure.Free;
  {$endif}
end.
