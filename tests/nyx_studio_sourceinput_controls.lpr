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

program nyx_studio_sourceinput_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Windows,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.editing, nyx.editing.lcl, nyx.interaction, nyx.studio.lcl, nyx.studio.projects;

type
  { Exposes deterministic installed key slots, never global keyboard injection.
    Each press includes release so later shortcuts cannot become false repeats. }
  TSourceMemo = class(TMemo)
  public
    property OnKeyDown;
    property OnKeyUp;
  end;
  { Protected Click bridge uses the actual ordinary Nyx control's input slot. }
  TControlInput = class(TControl);
  { Inherited keyboard/caption access for the currently focused native menu face.
    The popover retains it; this fixture never stores it beyond one dispatch. }
  TMenuInput = class(TWinControl);
  { Receives UI exceptions for this independent host; it outlives all dispatched
    application work and holds no editor/view back-reference. }
  TFailures = class
  public
    MessageText: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GForm: TForm;
  GStudio: TNyxNativeStudio;
  GFailures: TFailures;
  GChecks: Integer;
  GStage: String;

procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin
  MessageText := AException.Message;
  DumpExceptionBackTrace(StdErr);
end;

procedure Check(ACondition: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if GFailures.MessageText <> '' then
    begin
      raise Exception.Create(GFailures.MessageText);
    end;

    if not GStudio.PresentationPending and not GStudio.SourceCommands.Busy then
    begin
      Exit;
    end;
    Sleep(1);
  until GetTickCount64 - LStarted > 15000;
  raise Exception.Create('Owned source presentation did not reach its terminal state');
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
  TControlInput(LControl).Click;
  Pump;
end;

procedure HistoryKey(AInput: TSourceMemo; ARedo: Boolean = False);
var
  LKey: Word;
  LRelease: Word;
  LShift: TShiftState;
begin
  LKey := Ord('Z');
  LRelease := LKey;
  LShift := [ssCtrl];

  if ARedo then
  begin
    Include(LShift, ssShift);
  end;
  AInput.OnKeyDown(AInput, LKey, LShift);
  Check(LKey = 0, 'The retained source view owns this history shortcut');
  AInput.OnKeyUp(AInput, LRelease, LShift);
  Pump;
end;

procedure MenuKey(AKey: Word);
var
  LWindow: HWND;
  LInput: TWinControl;
  LRelease: Word;
begin
  LWindow := Windows.GetFocus;
  LInput := Controls.FindControl(LWindow);

  if (LInput = nil) or not Assigned(TMenuInput(LInput).OnKeyDown) then
  begin
    raise Exception.Create('The ordinary Actions menu has no focused keyboard input');
  end;
  LRelease := AKey;
  TMenuInput(LInput).OnKeyDown(LInput, AKey, []);

  if Windows.IsWindow(LWindow) and (Controls.FindControl(LWindow) = LInput) and
    Assigned(TMenuInput(LInput).OnKeyUp) then
  begin
    TMenuInput(LInput).OnKeyUp(LInput, LRelease, []);
  end;
  Pump;
end;

procedure FocusMenuCaption(const AText: String);
var
  LIndex: Integer;
  LInput: TWinControl;
begin
  MenuKey(VK_HOME);
  for LIndex := 0 to 31 do
  begin
    LInput := Controls.FindControl(Windows.GetFocus);

    if (LInput <> nil) and (Pos(AText, TMenuInput(LInput).Caption) > 0) then
    begin
      Exit;
    end;
    MenuKey(VK_DOWN);
  end;
  raise Exception.Create('The ordinary Actions menu does not expose its expected English command');
end;

procedure Run;
const
  CFirst: TNyxText = #10 + '// First input 🚀 𐐷';
  CSecond: TNyxText = #10 + '// Second input';
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LPair: TNyxProjectPair;
  LMemo: TSourceMemo;
  LOriginal: TNyxText;
  LFirst: TNyxText;
  LSecond: TNyxText;
  LSelection: TNyxTextSelection;
  LRejected: Boolean;
  LExternal: TNyxText;
  LHost: TControl;
begin

  if (ParamCount <> 2) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned runtime and native host width');
  end;
  GFailures := TFailures.Create;
  GStage := 'construct independent Studio';
  Application.OnException := GFailures.Failed;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(0, 0, StrToInt(ParamStr(2)), 900);
  GForm.Show;
  { No service connection, compiler, captured image or shared project is used. }
  GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('A comfortable source workspace'));
    LDocument.AddPage(LPage);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LPage := nil;
    LDocument.Free;
  end;
  GStudio.LoadProject(LPair);
  GStage := 'mount ordinary Studio';
  GStudio.Run;
  Pump;
  GStage := 'show source';

  if NyxInteractionPolicy(GStudio.ShellView.Root.Find('action-code')).CanIssueCommand then
  begin
    Click('action-code');
  end
  else
  begin
    { Compact chrome deliberately hides secondary header faces. Exercise its
      visible Actions -> View -> Pascal path instead of clicking a hidden face. }
    Click('action-actions');
    FocusMenuCaption('View');
    MenuKey(VK_RIGHT);
    FocusMenuCaption('Pascal');
    MenuKey(VK_RETURN);
  end;
  WriteLn('Source root / ', GStudio.CodeView.Root <> nil,
    ' / pane ', GStudio.SourceView.Root <> nil);

  if (GStudio.SourceView.Root <> nil) and
    (GStudio.SourceView.Root.Find('studio-code-host') <> nil) then
  begin
    LHost := GStudio.SourceView.ControlFor('studio-code-host');
    WriteLn('Source host / ', LHost.Width, ' x ', LHost.Height,
      ' / visible ', LHost.Visible);
  end;
  GStage := 'read mounted source input';
  LMemo := TSourceMemo(GStudio.CodeView.InputFor('studio-code'));
  LOriginal := LMemo.Text;
  GStudio.CodeView.SetTextSelection('studio-code',
    NyxTextSelection(LOriginal, NyxTextScalarCount(LOriginal), NyxTextScalarCount(LOriginal)));
  Pump;
  LSelection := GStudio.CodeView.TextSelectionFor('studio-code');
  LMemo.SelText := String(CFirst);
  Pump;
  LFirst := LMemo.Text;
  Check((GStudio.Session.DraftSource = LFirst) and (LFirst <> LOriginal),
    'Ordinary typing publishes the physical UTF-8 draft to the same session');
  Check(not GStudio.Session.CanUndo, 'Text typing does not manufacture paired design history');
  LMemo.SelText := String(CSecond);
  Pump;
  LSecond := LMemo.Text;
  HistoryKey(LMemo);
  Check((LMemo.Text = LFirst) and (GStudio.Session.DraftSource = LFirst),
    'Ordinary Studio Ctrl+Z restores its preceding typed draft');
  HistoryKey(LMemo, True);
  Check(LMemo.Text = LSecond, 'Ordinary Studio redo restores its second input');
  GStudio.CodeView.Root.Configure.ReadOnly(True).Done;
  GStudio.CodeView.Sync;
  Windows.SendMessageW(LMemo.Handle, WM_UNDO, 0, 0);
  HistoryKey(LMemo);
  Check((LMemo.Text = LSecond) and (GStudio.Session.DraftSource = LSecond),
    'Readonly presentation refuses both native and keyboard history commands');
  GStudio.CodeView.Root.Configure.ReadOnly(False).Done;
  GStudio.CodeView.Sync;
  Controls.RecreateWnd(LMemo);
  GStudio.RequestRefresh;
  Pump;
  Check((GStudio.CodeView.InputFor('studio-code') = LMemo) and (LMemo.Text = LSecond),
    'Native handle recreation preserves the physical mixed-line-ending frame');
  Check(Windows.SendMessageW(LMemo.Handle, EM_CANUNDO, 0, 0) <> 0,
    'The ordinary native edit menu queries the shared source timeline');
  Windows.SendMessageW(LMemo.Handle, WM_UNDO, 0, 0);
  Pump;
  Check((LMemo.Text = LFirst) and (GStudio.Session.DraftSource = LFirst),
    'Actual native context Undo uses the same session/draft timeline');
  Windows.SendMessageW(LMemo.Handle, EM_UNDO, 0, 0);
  Pump;
  Check((LMemo.Text = LOriginal) and
    GStudio.CodeView.TextSelectionFor('studio-code').SameRange(LSelection),
    'The second native Undo preserves exact pre-typing text and scalar caret');
  Check(not GStudio.Session.SourceDraftPending and (GStudio.Session.DraftSource = LPair.Source),
    'Returning to the physical baseline restores exact original authored line endings');
  Check(Windows.SendMessageW(LMemo.Handle, EM_CANUNDO, 0, 0) = 0,
    'The native menu sees exhausted text history');
  HistoryKey(LMemo);
  Windows.SendMessageW(LMemo.Handle, WM_UNDO, 0, 0);
  Check(LMemo.Text = LOriginal, 'Neither empty keyboard nor context Undo revives a stale platform stack');
  HistoryKey(LMemo, True);
  HistoryKey(LMemo, True);
  Click('action-expand-source');
  Check(GStudio.SourceModal.IsOpen and (GStudio.CodeView.InputFor('studio-code') = LMemo),
    'Expansion retains the same ordinary source input');
  HistoryKey(LMemo);
  Check(LMemo.Text = LFirst, 'Source history remains live in its expanded modal');
  Click('action-expand-source');
  HistoryKey(LMemo, True);
  Check((LMemo.Text = LSecond) and (GStudio.CodeView.InputFor('studio-code') = LMemo),
    'Returning from the modal retains the same input and Redo');
  Click('action-reset-source');
  Check(not GStudio.Session.SourceDraftPending, 'Restore accepted resolves the draft through ordinary Studio');
  HistoryKey(LMemo);
  Check(not GStudio.Session.SourceDraftPending,
    'A reset text timeline cannot silently undo the explicit paired Restore command');
  LExternal := LPair.Source + CSecond;
  GStudio.Session.SetSourceDraft(LExternal);
  HistoryKey(LMemo);
  Check(GStudio.Session.DraftSource = LExternal,
    'A stale physical command cannot overwrite an independently changed source draft');
  GStudio.RequestRefresh;
  Pump;
  Check(LMemo.Text = AdjustLineBreaks(String(LExternal)),
    'Ordinary reconciliation admits a fresh baseline for the changed authoring frame');
  LRejected := False;
  try
    RestoreNyxLCLInputText(LMemo, TNyxText('Invalid') + #0 + 'frame');
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GStudio.Session.DraftSource = LExternal) and
    (LMemo.Text = AdjustLineBreaks(String(LExternal))),
    'Unavailable native NUL projection refuses before changing source or input');
  Check((GStudio.Session.Source = LPair.Source) and (GStudio.Session.Save = LPair.Design),
    'Typing/history/modal operations retain exact accepted design and Pascal');
end;

begin
  try
    Application.Initialize;
    Run;
    WriteLn('PASS ', GChecks, ' ordinary native Studio source input checks');
  except
    on E: Exception do
    begin
      WriteLn(StdErr, GStage, ': ', E.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GForm.Free;
  Application.OnException := nil;
  GFailures.Free;
  Application.ProcessMessages;
end.
