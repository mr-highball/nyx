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
program nyx_studio_completion_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Windows,
  nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls, nyx.editing,
  nyx.interaction, nyx.pascal.drafts, nyx.pascal.completion, nyx.text.projection,
  nyx.studio.lcl, nyx.studio.mcp, nyx.studio.exchange, nyx.studio.outputs,
  nyx.studio.directories, nyx.studio.workspaces, nyx.studio.projects;

type
  { Controlled semantic result qualifies editor integration only. This fixture
    makes no language-resolution claim and uses no external compiler engine. }
  TProvider = class(TInterfacedObject, INyxPascalCompletionProvider)
  public
    function Complete(const AQuery: INyxPascalCompletionRequest): INyxPascalCompletionResult;
  end;
  { Actual native edit key slots. Each press releases its key; no global input,
    socket, screenshot, authored application or protected project is used. }
  TSourceMemo = class(TMemo)
  public
    property OnKeyDown;
    property OnKeyUp;
  end;
  { Borrowed native control access for an actual Nyx button or menu input. }
  TControlInput = class(TWinControl);
  { Replaces sockets with the never-listening production engine. Post and tick
    retain borrowed callbacks until a later explicit turn. Held completion replies
    permit actual typing/caret changes before delivery. The bridge owns this
    exchange; destruction revokes every callback and clears its observation slot. }
  TExchange = class(TNyxStudioEditorExchange)
  private
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FBody: TNyxDataValue;
    FToken: TNyxText;
    FConnect: Boolean;
    FReady: Boolean;
    FResponse: TNyxText;
  public
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    procedure Advance;
    procedure Flush;
  end;
  { Ordinary Studio with its two independent transports substituted. All source,
    popup, private authority, history and control dispatch code stays production. }
  TStudio = class(TNyxNativeStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;
  { Receives native UI failures while the independent host is live. No strong
    reference returns to Studio, its source memo, popup or backend. }
  TFailures = class
  public
    Text: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

const
  CBody: TNyxText = ';' + #13#10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.controls; procedure Run; implementation' + #13#10 +
    'procedure Run; var LBadge: INyxBadge; begin' + #10 +
    '// Dedicated Unicode qualification 🚀 𐐷' + #13#10 + '  LBadge.';
  CTail: TNyxText = '(''Ready'');' + #10 + 'end; end.';

var
  GEngine: TNyxStudioMCP;
  GStudio: TStudio;
  GForm: TForm;
  GFailures: TFailures;
  GExchanges: array of TExchange;
  GToken: TNyxText;
  GHoldCompletion: Boolean;
  GHoldSharing: Boolean;
  GCompletionPosts: Integer;
  GChecks: Integer;
  GStage: String;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function TProvider.Complete(const AQuery: INyxPascalCompletionRequest): INyxPascalCompletionResult;
var
  LText: TNyxText;
  LProjection: INyxTextProjection;
begin
  LText := AQuery.Sources.Text(AQuery.FileReference);
  LProjection := NewNyxTextProjection(LText, LText);

  if (AQuery.Caret >= 6) and (LProjection.AuthoredSlice(AQuery.Caret - 6, 6) = TNyxText('WithTe')) then
  begin
    Result := NewNyxPascalCompletionResult(AQuery, pcsReady, 'Controlled fixture reply',
      AQuery.Caret - 6, 'WithTe',
      [NyxPascalCompletion('WithText', 'WithText', 'Controlled badge method reply', pckMethod)], 1);
  end
  else
  begin
    Result := NewNyxPascalCompletionResult(AQuery, pcsReady, 'Controlled empty reply',
      AQuery.Caret, '', [], 0);
  end;
end;

procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin
  Text := AException.Message;
  DumpExceptionBackTrace(StdErr);
end;

destructor TExchange.Destroy;
var
  LAt: Integer;
begin
  CancelRequest;
  CancelTick;
  for LAt := 0 to High(GExchanges) do
  begin

    if GExchanges[LAt] = Self then
    begin
      GExchanges[LAt] := nil;
    end;
  end;
  inherited Destroy;
end;

procedure TExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
var
  LAt: Integer;
begin
  if Assigned(FReply) then
  begin
    raise Exception.Create('Independent transport overlapped requests');
  end;
  FReply := AReply;
  FBody := TNyxDataValue.ParseJSON(ABody);
  FToken := AToken;
  FConnect := AConnect;
  FReady := False;

  if FBody.Field('op').AsText = 'completion' then
  begin
    Inc(GCompletionPosts);
    for LAt := 0 to FBody.Count - 1 do
    begin

      if (FBody.Key(LAt) = 'source') or (FBody.Key(LAt) = 'project') then
      begin
        raise Exception.Create('Completion must send selectors rather than source or a project');
      end;
    end;
  end;
end;

procedure TExchange.CancelRequest;
begin
  FReply := nil;
  FReady := False;
  FBody := Default(TNyxDataValue);
  FResponse := '';
end;

procedure TExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  FTick := ATick;
end;

procedure TExchange.CancelTick;
begin
  FTick := nil;
end;

procedure TExchange.Advance;
var
  LTick: TNyxEditorTick;
begin
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

procedure TExchange.Flush;
var
  LValue: TNyxDataValue;
  LReply: TNyxEditorReply;
  LResponse: TNyxText;
  LCompletion: Boolean;
begin

  if not Assigned(FReply) then
  begin
    Exit;
  end;
  LCompletion := FBody.Field('op').AsText = 'completion';

  if not LCompletion and GHoldSharing then
  begin
    Exit;
  end;

  if not FReady then
  begin

    if FConnect then
    begin
      LValue := GEngine.ConnectEditor(FBody);
      GToken := LValue.Field('token').AsText;
    end
    else
    begin
      LValue := GEngine.EditorExchange(FToken, FBody);
    end;
    FResponse := LValue.ToJSON;
    FReady := True;
  end;

  if LCompletion and GHoldCompletion then
  begin
    Exit;
  end;
  LReply := FReply;
  LResponse := FResponse;
  FReply := nil;
  FReady := False;
  FResponse := '';
  FBody := Default(TNyxDataValue);
  LReply(200, LResponse);
end;

function TStudio.CreateEditorExchange: TNyxStudioEditorExchange;
var
  LExchange: TExchange;
begin
  LExchange := TExchange.Create;
  SetLength(GExchanges, Length(GExchanges) + 1);
  GExchanges[High(GExchanges)] := LExchange;
  Result := LExchange;
end;

procedure Pump;
var
  LTurn: Integer;
  LAt: Integer;
begin
  { Scheduled ticks use controlled turns, not elapsed HTTP/timer evidence. }
  for LTurn := 1 to 16 do
  begin
    Application.ProcessMessages;
    for LAt := 0 to High(GExchanges) do
    begin

      if GExchanges[LAt] <> nil then
      begin
        GExchanges[LAt].Advance;
        GExchanges[LAt].Flush;
      end;
    end;
    Application.ProcessMessages;

    if GFailures.Text <> '' then
    begin
      raise Exception.Create(GFailures.Text);
    end;
    Sleep(1);
  end;
end;

procedure Key(AInput: TSourceMemo; AKey: Word; AShift: TShiftState);
var
  LRelease: Word;
begin
  LRelease := AKey;
  AInput.OnKeyDown(AInput, AKey, AShift);
  AInput.OnKeyUp(AInput, LRelease, AShift);
  Pump;
end;

procedure MenuKey(AKey: Word);
var
  LInput: TWinControl;
  LHandle: HWND;
  LRelease: Word;
begin
  LHandle := Windows.GetFocus;
  LInput := Controls.FindControl(LHandle);
  Check((LInput <> nil) and Assigned(TControlInput(LInput).OnKeyDown),
    'The visible Actions menu has a physical key receiver');
  LRelease := AKey;
  TControlInput(LInput).OnKeyDown(LInput, AKey, []);

  if Windows.IsWindow(LHandle) and (Controls.FindControl(LHandle) = LInput) and
    Assigned(TControlInput(LInput).OnKeyUp) then
  begin
    TControlInput(LInput).OnKeyUp(LInput, LRelease, []);
  end;
  Pump;
end;

procedure FocusMenuCaption(const AText: String);
var
  LAt: Integer;
  LInput: TWinControl;
begin
  MenuKey(VK_HOME);
  for LAt := 0 to 31 do
  begin
    LInput := Controls.FindControl(Windows.GetFocus);

    if (LInput <> nil) and (Pos(AText, TControlInput(LInput).Caption) > 0) then
    begin
      Exit;
    end;
    MenuKey(VK_DOWN);
  end;
  raise Exception.Create('The visible Actions menu omitted its English command');
end;

procedure Click(const AID: TNyxText);
var
  LInput: TControl;
begin

  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LInput := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LInput := GStudio.SourceView.ControlFor(AID);
  end;
  TControlInput(LInput).Click;
  Pump;
end;

procedure CaretAtMember;
var
  LEditing: TNyxEditingSnapshot;
  LOffset: Integer;
begin
  LEditing := GStudio.CodeView.EditingFor('studio-code');
  LOffset := NyxTextScalarCount(Copy(LEditing.Text, 1, Pos('LBadge.', LEditing.Text) + 6));
  LOffset := LOffset + 6;
  GStudio.CodeView.SetTextSelection('studio-code',
    NyxTextSelection(LEditing.Text, LOffset, LOffset));
  TWinControl(GStudio.CodeView.InputFor('studio-code')).SetFocus;
end;

procedure Run;
var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LInput: TSourceMemo;
  LPrefix: TNyxText;
  LOriginal: TNyxText;
  LCompleted: TNyxText;
  LAccepted: TNyxText;
  LPosts: Integer;
  LProvider: INyxPascalCompletionProvider;
  LPopupControl: TControl;
  LSelection: TNyxTextSelection;
begin

  if (ParamCount <> 2) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned runtime and native width');
  end;
  GFailures := TFailures.Create;
  Application.OnException := GFailures.Failed;
  GStage := 'construct suspended production backend';
  LProfile := TNyxOutputConfiguration.Create;
  try
    LDirectories := TNyxStudioDirectories.ForRepository(TNyxText(GetCurrentDir))
      .RunningIn(TNyxText(ParamStr(1)) + '/backend')
      .EnrollingProject(TNyxText(ParamStr(1)) + '/backend');
    { No Start: this real backend never binds an HTTP listener. }
    GEngine := TNyxStudioMCP.Create(LDirectories, 8764, 8765, LProfile.Encode);
  finally
    LProfile.Free;
  end;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(0, 0, StrToInt(ParamStr(2)), 900);
  GForm.Show;
  GStudio := TStudio.Create(GForm, ParamStr(1) + '/editor');
  LAccepted := GStudio.Session.Source;
  LPrefix := TNyxText('unit ') + GStudio.Session.AcceptedPascalProject.MainUnit.Name + CBody;
  LOriginal := LPrefix + 'WithTe' + CTail;
  LCompleted := LPrefix + 'WithText' + CTail;
  GStudio.Session.SetSourceDraft(LOriginal);
  GStudio.Run;
  Pump;
  GStage := 'mount visible source';

  if NyxInteractionPolicy(GStudio.ShellView.Root.Find('action-code')).CanIssueCommand then
  begin
    Click('action-code');
  end
  else
  begin
    Click('action-actions');
    FocusMenuCaption('View');
    MenuKey(VK_RIGHT);
    FocusMenuCaption('Pascal');
    MenuKey(VK_RETURN);
  end;
  LInput := TSourceMemo(GStudio.CodeView.InputFor('studio-code'));
  CaretAtMember;
  GStage := 'connect two independent private exchanges';
  GStudio.ConnectService('http://owned.invalid', NyxPrimaryWorkspace);
  Pump;
  Check(Length(GExchanges) = 2, 'Polling and completion use separate owned transports');
  GEngine.EditorExchange(GToken, NyxObject([
    NyxField('op', NyxData('configure')), NyxField('permission', NyxData('disabled'))]));
  Pump;
  GStage := 'explicit unavailable provider';
  Key(LInput, VK_SPACE, [ssCtrl]);
  Check((GStudio.CompletionPopover <> nil) and GStudio.CompletionPopover.IsOpen,
    'Ctrl+Space opens an ordinary public Nyx popover');
  Check(GStudio.CompletionPopover.Renderer.Root.Find('studio-completion-status') <> nil,
    'Missing optional provider has visible typed status');
  Check(Windows.GetFocus = LInput.Handle, 'Opening completion preserves source focus');
  Key(LInput, VK_ESCAPE, []);
  Check(GStudio.CompletionPopover = nil, 'Escape retires the card after input returns');
  LProvider := TProvider.Create;
  GEngine.ConfigureCompletionProvider(LProvider);
  GStage := 'controlled semantic completion in ordinary Studio';
  CaretAtMember;
  LSelection := GStudio.CodeView.EditingFor('studio-code').Selection;
  Key(LInput, VK_SPACE, [ssCtrl]);
  Check((GStudio.CompletionPopover <> nil) and
    (GStudio.CompletionPopover.Renderer.Root.Find('studio-completion-row-0-choose') <> nil),
    'The ordinary popup presents a semantic member reply while agents are disabled');
  Check(GStudio.CodeView.InputFor('studio-code') = LInput,
    'Completion leaves the same physical source memo mounted');
  Check(GStudio.CodeView.EditingFor('studio-code').Selection.SameRange(LSelection),
    'Completion preserves the physical caret');
  Check(GStudio.CompletionPopover.Renderer.Root.Find('studio-completion-row-0-choose').Prop('text') =
    TNyxText('WithText'), 'The popup retains the typed controlled member caption');
  LPopupControl := GStudio.CompletionPopover.Renderer.ControlFor('studio-completion-row-0-choose');
  TControlInput(LPopupControl).Click;
  Pump;
  Check(GStudio.Session.DraftSource = LCompleted, 'A suggestion button publishes exact authored draft bytes');
  Check(GStudio.Session.Source = LAccepted, 'Completion does not admit or compile source');
  Check(GStudio.CompletionPopover = nil, 'Accepted card retires on a later turn');
  Key(LInput, Ord('Z'), [ssCtrl]);
  Check(GStudio.Session.DraftSource = LOriginal, 'One ordinary Undo restores the exact preceding authored draft');
  Key(LInput, Ord('Z'), [ssCtrl, ssShift]);
  Check(GStudio.Session.DraftSource = LCompleted, 'Redo restores completion on the same text timeline');
  Key(LInput, Ord('Z'), [ssCtrl]);

  GStage := 'caret movement retires delayed completion';
  CaretAtMember;
  GHoldCompletion := True;
  Key(LInput, VK_SPACE, [ssCtrl]);
  GStudio.CodeView.SetTextSelection('studio-code', NyxTextSelection(
    GStudio.CodeView.EditingFor('studio-code').Text, 0, 0));
  GHoldCompletion := False;
  Pump;
  Check(GStudio.CompletionPopover = nil, 'A delayed reply cannot reopen after the caret moves');
  Check(GStudio.Session.DraftSource = LOriginal, 'Stale reply changes no source');

  GStage := 'acknowledgement precedes submission';
  CaretAtMember;
  GHoldSharing := True;
  LInput.SelText := 'x';
  LPosts := GCompletionPosts;
  Key(LInput, VK_SPACE, [ssCtrl]);
  Check(GCompletionPosts = LPosts, 'An unsent draft cannot dispatch completion');
  Check((GStudio.CompletionPopover <> nil) and
    (GStudio.CompletionPopover.Renderer.Root.Find('studio-completion-waiting') <> nil),
    'The waiting card explains source synchronization');
  GHoldSharing := False;
  Pump;
  Check(GCompletionPosts = LPosts + 1, 'The acknowledged exact draft submits once');
  Key(LInput, VK_ESCAPE, []);
  Key(LInput, Ord('Z'), [ssCtrl]);
  Check(GStudio.Session.DraftSource = LOriginal, 'Waiting and querying add no text Undo commands');

  GStage := 'expanded source retains popup and history';
  Click('action-expand-source');
  CaretAtMember;
  Key(LInput, VK_SPACE, [ssCtrl]);
  Check(GStudio.SourceModal.IsOpen and (GStudio.CodeView.InputFor('studio-code') = LInput),
    'Expanded source retains the original memo');
  Check(GStudio.CompletionPopover <> nil, 'Completion works in expanded source');
  Key(LInput, VK_RETURN, []);
  Check(GStudio.Session.DraftSource = LCompleted, 'Enter accepts through the expanded source key producer');
  Key(LInput, Ord('Z'), [ssCtrl]);
  Check(GStudio.Session.DraftSource = LOriginal, 'Expanded completion is still one Undo entry');
  Check(GStudio.Session.Source = LAccepted, 'All journeys retain the complete accepted source');
  GStage := 'retire owners with a pending query';
  CaretAtMember;
  GHoldCompletion := True;
  Key(LInput, VK_SPACE, [ssCtrl]);
end;

begin
  Application.Initialize;
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL / ', GStage, ' / ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GStudio := nil;
  GForm.Free;
  GEngine.Free;
  GEngine := nil;
  SetLength(GExchanges, 0);
  Application.OnException := nil;
  GFailures.Free;

  if ExitCode = 0 then
  begin
    WriteLn('PASS ', GChecks, ' ordinary native Studio completion checks');
  end;
end.
