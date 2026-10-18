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

program nyx_completion_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, SyncObjs, Forms, Controls, StdCtrls, ExtCtrls, Windows,
  LCLType, nyx.text, nyx.bytes, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.theme, nyx.render.lcl, nyx.editing.lcl, nyx.widgets.lcl,
  nyx.editing, nyx.editing.history, nyx.editing.commands, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.completion.view, nyx.studio.completion.native, nyx.events,
  nyx.scheduler, nyx.behavior;

type
  { Exposes the installed LCL input slot for deterministic native callback input,
    avoiding process-global keyboard state. This is not hardware/IME evidence. }
  TMemoInput = class(TMemo)
  public
    property OnKeyDown;
    property OnKeyUp;
  end;

  { One joined analysis thread owns immutable request/provider references.
    Its completion event and copied data outlive the worker until the UI joins. }
  TAnalysis = class(TThread)
  protected
    procedure Execute; override;
  public
    Done: TEvent;
    Provider: INyxPascalCompletionProvider;
    Query: INyxPascalCompletionRequest;
    Reply: INyxPascalCompletionResult;
    Failure: TNyxText;
  end;

  { Forward declaration for a borrowed fixture receiver, never a public API. }
  THarness = class;

  { Managed mounted-input port borrows only this fixture. The owner detaches
    before renderer teardown; the native adapter owns its independent lease. }
  THistoryPort = class(TInterfacedObject, INyxTextHistoryCommands)
  public
    Owner: THarness;
    function CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
    procedure Execute(ACommand: TNyxTextHistoryCommand);
    procedure BeforeEdit(const AEditing: TNyxEditingSnapshot);
  end;

  { Sequential key callback borrows the fixture until explicit detach/cancel.
    It owns no renderer/state back-reference and never survives fixture teardown. }
  TKeyCallback = class(TNyxEventCallback)
  public
    Owner: THarness;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Owns both independent public Nyx views and the shared editor model. Its
    physical input remains mounted while the suggestion card is rebuilt.
    It never hosts HTTP, executes authored applications or alters a Studio pair. }
  THarness = class
  public
    Form: TForm;
    CodeHost: TPanel;
    MenuHost: TPanel;
    Theme: TNyxTheme;
    CodeView: TNyxLCLRenderer;
    MenuView: TNyxLCLRenderer;
    CodeDocument: TNyxDocument;
    MenuDocument: TNyxDocument;
    History: INyxTextEditHistory;
    HistoryPort: THistoryPort;
    HistoryLease: INyxTextHistoryCommands;
    Menu: INyxPascalCompletionMenu;
    Sources: INyxPascalSourceSnapshot;
    Input: TMemoInput;
    Keys: INyxEventSubscription;
    Callback: TKeyCallback;
    Failure: TNyxText;
    Notifications: Integer;
    LastAction: TNyxPascalCompletionAction;
    KeySeen: Boolean;
    GenerationBeforeKey: Integer;
    GenerationAfterKey: Integer;
    constructor Create(AWidth: Integer);
    destructor Destroy; override;
    procedure ObservePhysical;
    procedure Publish;
    procedure RebuildMenu;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure MenuEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Key(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
    procedure SendKey(AKey: Word; AShift: TShiftState);
  end;

const
  CSource: TNyxText = 'unit completion.control; interface implementation end.';
  CBuffer: TNyxText = 'unit completion.control; {$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.controls; procedure Run; implementation' + #10 +
    'procedure Run; var LBadge: INyxBadge; begin' + #10 +
    '// Dedicated Unicode qualification 🚀 𐐷' + #10 +
    'LBadge.WithTexx(''Ready''); end; end.';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure TAnalysis.Execute;
begin
  try
    try

      if Terminated then
      begin
        Exit;
      end;
      Reply := Provider.Complete(Query);
    except
      on E: Exception do
      begin
        Failure := E.Message;
      end;
    end;
  finally

    if Done <> nil then
    begin
      Done.SetEvent;
    end;
  end;
end;

procedure TKeyCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if Owner <> nil then
  begin
    Owner.Key(AEvent, AExecution);
  end;
end;

function THistoryPort.CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
begin
  Result := False;

  if Owner <> nil then
  begin

    if ACommand = nhcUndo then
    begin
      Result := Owner.History.CanUndo;
    end
    else
    begin
      Result := Owner.History.CanRedo;
    end;
  end;
end;

procedure THistoryPort.BeforeEdit(const AEditing: TNyxEditingSnapshot);
begin

  if Owner <> nil then
  begin
    Owner.ObservePhysical;
  end;
end;

procedure THistoryPort.Execute(ACommand: TNyxTextHistoryCommand);
var
  LFrame: TNyxTextEditorFrame;
  LCandidate: INyxPascalSourceSnapshot;
begin

  if (Owner = nil) or not CanExecute(ACommand) then
  begin
    Exit;
  end;

  if ACommand = nhcUndo then
  begin
    Owner.History.UndoFrame(LFrame);
  end
  else
  begin
    Owner.History.RedoFrame(LFrame);
  end;
  LCandidate := Owner.Sources.WithBuffer(TNyxPascalSourceRef.Main, LFrame.Text);

  if ACommand = nhcUndo then
  begin
    Owner.History.Undo;
  end
  else
  begin
    Owner.History.Redo;
  end;
  Owner.Sources := LCandidate;
  Owner.Publish;
end;

constructor THarness.Create(AWidth: Integer);
var
  LCode: INyxCodeEditor;
  LText: TNyxText;
  LCaret: Integer;
  LEditing: TNyxEditingSnapshot;
begin
  inherited Create;
  Theme := TNyxTheme.Create;
  Form := TForm.Create(nil);
  Form.SetBounds(0, 0, AWidth, 760);
  CodeHost := TPanel.Create(Form);
  CodeHost.Parent := Form;
  CodeHost.SetBounds(0, 0, AWidth, 340);
  MenuHost := TPanel.Create(Form);
  MenuHost.Parent := Form;
  MenuHost.SetBounds(0, 340, AWidth, 400);
  Form.HandleNeeded;
  CodeHost.HandleNeeded;
  MenuHost.HandleNeeded;
  CodeDocument := TNyxDocument.Create;
  LCode := NewNyxCodeEditor('code').WithText('Pascal source');
  LCode.Configure.Value(CBuffer).ReadOnly(False).Flex(1).Done;
  CodeDocument.AddPage(LCode);
  CodeView := TNyxLCLRenderer.Create(Theme);
  MenuView := TNyxLCLRenderer.Create(Theme);
  CodeView.OnEvent := Changed;
  MenuView.OnEvent := MenuEvent;
  CodeView.Render(CodeDocument, CodeDocument.Pages[0], CodeHost);
  Input := TMemoInput(CodeView.InputFor('code'));
  Input.HandleNeeded;
  { The Win32 memo may normalize line endings. Capture its exact physical frame
    and bind that value as the current buffer before querying or editing it. }
  LText := NyxLCLInputText(Input);
  LCaret := NyxTextScalarCount(Copy(LText, 1, Pos('WithTexx', LText) - 1)) + 6;
  CodeView.SetTextSelection('code', NyxTextSelection(LText, LCaret, LCaret));
  LEditing := CodeView.EditingFor('code');
  History := NewNyxTextEditHistory(NyxTextEditorFrame(LEditing.Text, LEditing.Selection));
  HistoryPort := THistoryPort.Create;
  HistoryLease := HistoryPort;
  HistoryPort.Owner := Self;
  CodeView.BindTextHistory('code', HistoryPort);
  Sources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('completion.control'), CSource))
    .WithBuffer(TNyxPascalSourceRef.Main, LEditing.Text);
  CodeView.Root.Configure.Value(LEditing.Text).Done;
  Menu := NewNyxPascalCompletionMenu(History);
  Callback := TKeyCallback.Create;
  Callback.Owner := Self;
  Keys := CodeView.Events.OnKeyDown(NyxControlEvents('code')).Subscribe(Callback);
end;

destructor THarness.Destroy;
begin

  if HistoryPort <> nil then
  begin
    HistoryPort.Owner := nil;
  end;

  if Callback <> nil then
  begin
    Callback.Owner := nil;
  end;

  if Keys <> nil then
  begin
    Keys.Cancel;
  end;
  Keys := nil;
  CodeView.Free;
  MenuView.Free;
  CodeDocument.Free;
  MenuDocument.Free;
  Form.Free;
  Theme.Free;
  inherited Destroy;
end;

procedure THarness.ObservePhysical;
var
  LEditing: TNyxEditingSnapshot;
begin
  LEditing := CodeView.EditingFor('code');
  History.Observe(NyxTextEditorFrame(LEditing.Text, LEditing.Selection), LEditing.Composing);
  Sources := Sources.WithBuffer(TNyxPascalSourceRef.Main, LEditing.Text);
end;

procedure THarness.Publish;
begin
  CodeView.Root.Configure.Value(History.Current.Text).Done;
  CodeView.Sync;
  CodeView.SetTextSelection('code', History.Current.Selection);
  Inc(Notifications);
end;

procedure THarness.RebuildMenu;
var
  LCard: INyxCard;
  LNext: TNyxDocument;
  LPrevious: TNyxDocument;
begin
  LCard := Menu.Build('completion', Sources);
  LNext := TNyxDocument.Create;
  try
    LNext.AddPage(LCard);
    MenuView.Render(LNext, LNext.Pages[0], MenuHost);
    LPrevious := MenuDocument;
    MenuDocument := LNext;
    LNext := nil;
    LPrevious.Free;
  finally
    LNext.Free;
  end;
end;

procedure THarness.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (History <> nil) and (AEvent.Trigger = ntChange) and (ANode.ID = 'code') then
  begin
    ObservePhysical;
    Inc(Notifications);
  end;
end;

procedure THarness.MenuEvent(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if Menu.Route('completion', MenuView.Root, ANode, AEvent.Trigger, Sources) = pcaAccept then
  begin
    Sources := Menu.Accept(Sources);
    Publish;
    RebuildMenu;
  end;
end;

procedure THarness.Key(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LAction: TNyxPascalCompletionAction;
  LFrame: TNyxTextEditorFrame;
  LCandidate: INyxPascalSourceSnapshot;
begin

  if not AEvent.HasKeyboard then
  begin
    Exit;
  end;
  KeySeen := True;
  GenerationBeforeKey := History.Generation;
  ObservePhysical;
  GenerationAfterKey := History.Generation;
  LAction := Menu.Key(AEvent.Keyboard, Sources);
  LastAction := LAction;

  if LAction = pcaNone then
  begin
    Exit;
  end;
  NyxEventResponse(AExecution).Consume;
  case LAction of
    pcaSelect, pcaDismiss: RebuildMenu;
    pcaAccept:
    begin
      Sources := Menu.Accept(Sources);
      Publish;
      RebuildMenu;
    end;
    pcaUndo, pcaRedo:
    begin

      if LAction = pcaUndo then
      begin

        if not History.UndoFrame(LFrame) then
        begin
          Exit;
        end;
      end
      else
      begin

        if not History.RedoFrame(LFrame) then
        begin
          Exit;
        end;
      end;
      LCandidate := Sources.WithBuffer(TNyxPascalSourceRef.Main, LFrame.Text);

      if LAction = pcaUndo then
      begin
        History.Undo;
      end
      else
      begin
        History.Redo;
      end;
      Sources := LCandidate;
      Publish;
      RebuildMenu;
    end;
  else
    raise Exception.Create('Unexpected fixture command');
  end;
end;

procedure THarness.SendKey(AKey: Word; AShift: TShiftState);
var
  LHandler: TKeyEvent;
  LReleased: Word;
begin
  LHandler := Input.OnKeyDown;
  Check(Assigned(LHandler), 'Actual native memo owns a keyboard producer');
  KeySeen := False;
  LReleased := AKey;
  LHandler(Input, AKey, AShift);
  LHandler := Input.OnKeyUp;

  if Assigned(LHandler) then
  begin
    LHandler(Input, LReleased, AShift);
  end;
  Check(AKey = 0, 'The typed source action consumes the platform default (seen=' +
    BoolToStr(KeySeen, True) + ', action=' + IntToStr(Ord(LastAction)) +
    ', state=' + IntToStr(Ord(Menu.State)) + ', generations=' +
    IntToStr(GenerationBeforeKey) + '/' + IntToStr(GenerationAfterKey) + ')');
end;

function ReadText(const APath: String): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure Run;
var
  LHost: THarness;
  LTools: TNyxDataValue;
  LWorker: TAnalysis;
  LTicket: TNyxPascalCompletionTicket;
  LBefore: TNyxTextEditorFrame;
  LInput: TMemoInput;
  LCompleted: TNyxText;
  LStarted: QWord;
  LWidth: Integer;
  LWorkerStarted: Boolean;
begin
  LHost := nil;
  LWorker := nil;
  LWorkerStarted := False;
  try

    if ParamCount <> 6 then
    begin
      raise Exception.Create('Supply SDK, toolchain JSON, owned scratch, helper, FPC sources and native width');
    end;
    LWidth := StrToInt(ParamStr(6));
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    LHost := THarness.Create(LWidth);
    LBefore := LHost.History.Current;
    LInput := LHost.Input;
    LWorker := TAnalysis.Create(True);
    LWorker.FreeOnTerminate := False;
    LWorker.Done := TEvent.Create(nil, True, False, '');
    LWorker.Query := LHost.Menu.BeginQuery(LHost.Sources, TNyxPascalSourceRef.Main,
      TNyxPascalSourceRef.Main, LTicket);
    LWorker.Provider := NewNyxNativePascalCompletionProvider(TNyxPascalCompletionHost.Using(
      ParamStr(4), ParamStr(1), String(LTools.Field('FPC').AsText), ParamStr(5),
      String(LTools.Field('LAZARUS').AsText), ParamStr(3)));
    LHost.RebuildMenu;
    LStarted := GetTickCount64;
    LWorker.Start;
    LWorkerStarted := True;
    while LWorker.Done.WaitFor(1) <> wrSignaled do
    begin
      Application.ProcessMessages;

      if GetTickCount64 - LStarted > 45000 then
      begin
        raise Exception.Create('Owned analysis observation deadline exceeded; joining the same worker');
      end;
    end;
    LWorker.WaitFor;
    Check(LWorker.Failure = '', 'Joined provider resolves without a worker failure');
    Check((LWorker.Reply <> nil) and (LWorker.Reply.Status = pcsReady),
      'Actual SDK produces the view input');
    Check(LHost.Menu.Present(LTicket, LWorker.Reply, LHost.Sources),
      'Current reply reaches the public completion view');
    LHost.RebuildMenu;
    Check(LHost.MenuView.Root.Find('completion-row-0-choose').Prop('text') = 'WithText',
      'The ordinary Nyx button displays the real specialized member');
    LHost.SendKey(VK_DOWN, []);
    Check(LHost.Input = LInput, 'Menu navigation retains the physical memo');
    TNyxLCLButton(LHost.MenuView.ControlFor('completion-row-0-choose')).Click;
    LCompleted := LHost.History.Current.Text;
    Check((LHost.Menu.State = pcvClosed) and (LHost.History.UndoCount = 1) and
      (Pos('WithText(', LCompleted) > 0), 'Actual button input performs one completion transaction');
    Check((NyxLCLInputText(LHost.Input) = LCompleted) and
      (LHost.CodeView.TextSelectionFor('code').Start = LHost.History.Current.Selection.Start),
      'Mounted text and scalar caret match the proposed edit');
    LHost.SendKey(VK_Z, [ssCtrl]);
    Check((NyxLCLInputText(LHost.Input) = LBefore.Text) and
      LHost.History.Current.SameValue(LBefore), 'Native Ctrl+Z restores exact text and pre-completion caret');
    LHost.SendKey(VK_Z, [ssCtrl, ssShift]);
    Check(NyxLCLInputText(LHost.Input) = LCompleted, 'Native redo restores the completed source');
    LHost.Input.SelText := 'x';
    LHost.ObservePhysical;
    Check(LHost.History.Current.Text <> LCompleted, 'Ordinary memo editing enters the same text history');
    LHost.SendKey(VK_Z, [ssCtrl]);
    Check((NyxLCLInputText(LHost.Input) = LCompleted) and (LHost.Input = LInput),
      'Undo of later typing retains completion and the same physical control');
    Check(LHost.Sources.AcceptedText(TNyxPascalSourceRef.Main) = CSource,
      'The complete physical journey never admits authored source');
    Check(Windows.SendMessageW(LHost.Input.Handle, EM_CANUNDO, 0, 0) <> 0,
      'The physical native context-menu query sees the owned completion Undo');
    Windows.SendMessageW(LHost.Input.Handle, WM_UNDO, 0, 0);
    Check((NyxLCLInputText(LHost.Input) = LBefore.Text) and
      LHost.History.Current.SameValue(LBefore),
      'Actual WM_UNDO shares the exact completion timeline and caret');
    Check(Windows.SendMessageW(LHost.Input.Handle, EM_CANUNDO, 0, 0) = 0,
      'The physical native query reports the exhausted owned timeline');
    Windows.SendMessageW(LHost.Input.Handle, EM_UNDO, 0, 0);
    Check(NyxLCLInputText(LHost.Input) = LBefore.Text,
      'An exhausted native Undo cannot fall through to the stale widget stack');
  finally

    if LWorker <> nil then
    begin

      if not LWorkerStarted then
      begin
        LWorker.Terminate;
        LWorker.Start;
      end;
      LWorker.WaitFor;
      LWorker.Done.Free;
      LWorker.Free;
    end;
    LHost.Free;
  end;
end;

begin
  try
    Application.Initialize;
    Run;
    WriteLn('PASS ', GChecks, ' actual native completion/history controls');
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
    end;
  end;
end.
