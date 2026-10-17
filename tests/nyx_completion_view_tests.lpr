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

program nyx_completion_view_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.editing.history, nyx.types,
  nyx.controls, nyx.model, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.completion.view {$ifdef PAS2JS}, Web{$endif};

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

function Frame(const AText: TNyxText; ACaret: Integer): TNyxTextEditorFrame;
begin
  Result := NyxTextEditorFrame(AText, NyxTextSelection(AText, ACaret, ACaret));
end;

procedure HistoryChecks;
const
  COriginal: TNyxText = 'A 🚀 𐐷' + #0 + #13 + #10 + 'WithTexx;';
var
  LHistory: INyxTextEditHistory;
  LSmall: INyxTextEditHistory;
  LBefore: TNyxTextEditorFrame;
  LPeek: TNyxTextEditorFrame;
  LGeneration: Integer;
  LRejected: Boolean;
  LAt: Integer;
  LStart: Integer;
begin
  LHistory := NewNyxTextEditHistory(Frame('a', 1));
  Check(not LHistory.CanUndo and not LHistory.CanRedo, 'Fresh text owns independent empty stacks');
  LHistory.Observe(Frame('ab', 2));
  Check(LHistory.Undo and (LHistory.Current.Text = 'a'), 'Observed physical input has ordinary Undo');
  LGeneration := LHistory.Generation;
  LHistory.Observe(Frame('a', 0));
  Check(LHistory.CanRedo and (LHistory.Generation > LGeneration),
    'Selection changes retain Redo but retire asynchronous caret proposals');
  Check(LHistory.Redo and (LHistory.Current.Text = 'ab') and
    (LHistory.Current.Selection.Start = 2), 'Redo restores text and exact after-selection');
  Check(LHistory.Undo and (LHistory.Current.Selection.Start = 0),
    'Undo retains the latest selection before the redone command');

  LStart := NyxTextScalarCount('A 🚀 𐐷' + #0 + #13 + #10);
  LHistory.Reset(Frame(COriginal, LStart + 4));
  LBefore := LHistory.Current;
  LHistory.Replace(LBefore, LStart, 'WithTexx', 'WithText');
  Check((LHistory.Current.Text = Copy(COriginal, 1, Pos('WithTexx', COriginal) - 1) + 'WithText;') and
    (LHistory.Current.Selection.Start = LStart + 8) and (LHistory.UndoCount = 1),
    'One Unicode/NUL/line-ending-preserving completion is one text transaction');
  Check(LHistory.UndoFrame(LPeek) and LPeek.SameValue(LBefore),
    'Owners can stage source-budget admission against an independent next Undo frame');
  Check(LHistory.Undo and LHistory.Current.SameValue(LBefore), 'Undo restores exact pre-completion text and middle caret');
  Check(LHistory.Redo and (LHistory.Current.Selection.Start = LStart + 8), 'Redo restores the completed caret');
  LRejected := False;
  try
    LHistory.Replace(LBefore, LStart, 'WithTexx', 'Other');
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LHistory.UndoCount = 1), 'A stale expected frame cannot change text or history');

  LHistory.Reset(Frame('base', 4));
  LHistory.Observe(Frame('base first', 10), True);
  LGeneration := LHistory.Generation;
  LHistory.Observe(Frame('base first', 10), True);
  Check((LHistory.Generation = LGeneration) and not LHistory.CanUndo,
    'Repeated physical composition observation adds no generation or Undo');
  LRejected := False;
  try
    LHistory.Undo;
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LHistory.Composing, 'History cannot terminate an active IME transaction');
  LHistory.Observe(Frame('base final', 10), False);
  Check((LHistory.UndoCount = 1) and LHistory.Undo and (LHistory.Current.Text = 'base'),
    'All composition drafts commit one final text entry');
  LHistory.Observe(Frame('base temporary', 14), True);
  LHistory.Observe(Frame('base', 4), False);
  Check((LHistory.UndoCount = 0) and LHistory.CanRedo,
    'Composition returning to its base creates no command and preserves Redo');

  LSmall := NewNyxTextEditHistory(Frame('0', 1),
    TNyxTextHistoryLimits.Defaults.UpToEntries(2).UpToBytes(128));
  for LAt := 1 to 4 do
  begin
    LSmall.Observe(Frame(IntToStr(LAt), 1));
  end;
  Check((LSmall.UndoCount = 2) and LSmall.Truncated and (LSmall.RetainedBytes <= 128),
    'Both stacks obey retained frame and byte budgets');
  Check(LSmall.Undo and LSmall.Undo and not LSmall.Undo and
    (LSmall.Current.Text = '2'), 'Eviction keeps the nearest valid history order');
  LBefore := LSmall.Current;
  LRejected := False;
  try
    LSmall.Replace(LBefore, 0, '2', StringOfChar('x', 200));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LSmall.Current.SameValue(LBefore) and LSmall.CanRedo,
    'A programmatic edit refuses before losing its promised Undo/Redo');
  LSmall.Observe(Frame(StringOfChar('x', 200), 200));
  Check((LSmall.Current.Scalars = 200) and LSmall.Undo,
    'Oversized physical input remains real input and can Undo its retained predecessor');
  Check(not LSmall.CanRedo and LSmall.Truncated,
    'An unretainable physical frame is diagnosed rather than exceeding its retention limit');
  LGeneration := LSmall.Generation;
  LSmall.Reset(LSmall.Current);
  Check((LSmall.Generation > LGeneration) and not LSmall.CanUndo and not LSmall.CanRedo,
    'Explicit buffer reset retires proposals even when text is identical');
end;

procedure MenuChecks;
const
  CAccepted: TNyxText = 'unit completion.fixture; interface implementation end.';
  CPrefix: TNyxText = '// A badge 🚀 𐐷' + #10 + 'LBadge.';
  CTail: TNyxText = 'WithTexx;';
var
  LProject: INyxPascalProject;
  LSources: INyxPascalSourceSnapshot;
  LChanged: INyxPascalSourceSnapshot;
  LCandidate: INyxPascalSourceSnapshot;
  LHistory: INyxTextEditHistory;
  LMenu: INyxPascalCompletionMenu;
  LOther: INyxPascalCompletionMenu;
  LQuery: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LTicket: TNyxPascalCompletionTicket;
  LNewTicket: TNyxPascalCompletionTicket;
  LOtherTicket: TNyxPascalCompletionTicket;
  LCard: INyxCard;
  LOldCard: INyxCard;
  LMain: TNyxPascalSourceRef;
  LHelper: TNyxPascalSourceRef;
  LBefore: TNyxTextEditorFrame;
  LStart: Integer;
  LRejected: Boolean;

  function Reply(const AQuery: INyxPascalCompletionRequest): INyxPascalCompletionResult;
  begin
    Result := NewNyxPascalCompletionResult(AQuery, pcsReady, '', LStart, 'WithTexx', [
      NyxPascalCompletion('WithText', 'WithText', 'function WithText(const AText: TNyxText): INyxBadge;', pckMethod),
      NyxPascalCompletion('WithTheme', 'WithTheme', 'A second typed choice', pckMethod)], 4);
  end;
begin
  LMain := TNyxPascalSourceRef.Main;
  LHelper := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('helper.inc'));
  LProject := NewNyxPascalProject(NyxPascalUnit('completion.fixture'), CAccepted)
    .WithInclude(NyxPascalInclude('helper.inc'), '{ Kept independent }');
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LMain, CPrefix + CTail);
  LStart := NyxTextScalarCount(CPrefix);
  LHistory := NewNyxTextEditHistory(Frame(CPrefix + CTail, LStart + 4));
  LBefore := LHistory.Current;
  LMenu := NewNyxPascalCompletionMenu(LHistory);
  Check((LMenu.Key(NyxKeyStroke(nkZKey, [nmControl], True), LSources) = pcaUndo) and
    not LHistory.Undo, 'An exhausted timeline owns repeated Undo without falling through to native history');
  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  Check((LMenu.State = pcvPending) and (LQuery.Caret = LStart + 4), 'Menu captures the exact physical scalar caret');
  LCard := LMenu.Build('completion', LSources);
  Check((LCard.Node.Find('completion-status').Prop('text') = 'Finding suggestions...') and
    (LCard.Node.Find('completion-rows') = nil), 'Pending presentation contains no speculative symbols');
  LReply := Reply(LQuery);
  Check(LMenu.Present(LTicket, LReply, LSources) and (LMenu.Selected = 0), 'Matching provider page selects its first row');
  Check(LMenu.Key(NyxKeyStroke(nkDownKey), LSources) = pcaSelect, 'Arrow navigation stays a presentation action');
  Check((LMenu.Selected = 1) and (LMenu.Key(NyxKeyStroke(nkUpKey), LSources) = pcaSelect) and
    (LMenu.Selected = 0), 'Arrow navigation updates the typed selection');
  LOldCard := LMenu.Build('completion', LSources);
  LCard := LMenu.Build('completion', LSources);
  Check(LMenu.Route('completion', LCard.Node, LOldCard.Node.Find('completion-row-0-choose'),
    ntClick, LSources) = pcaNone, 'A retired physical row cannot address a replacement mount');
  Check(LMenu.Route('completion', LCard.Node, LCard.Node.Find('completion-previous'),
    ntClick, LSources) = pcaNone, 'Disabled page controls cannot issue an action');
  Check(LMenu.Route('completion', LCard.Node, LCard.Node.Find('completion-row-0-choose'),
    ntClick, LSources) = pcaAccept, 'The current physical row resolves its typed accept action');
  LCandidate := LMenu.Accept(LSources);
  Check((LCandidate.Text(LMain) = CPrefix + 'WithText;') and
    (LCandidate.AcceptedText(LMain) = CAccepted) and
    (LCandidate.Text(LHelper) = LSources.Text(LHelper)) and (LMenu.State = pcvClosed),
    'Acceptance stages only the selected buffer, retaining accepted and helper inputs');
  Check((LHistory.UndoCount = 1) and LHistory.Undo and LHistory.Current.SameValue(LBefore),
    'Completion uses the same text Undo as ordinary typing');

  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  LReply := Reply(LQuery);
  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LNewTicket);
  Check(not LMenu.Present(LTicket, LReply, LSources) and (LMenu.State = pcvPending),
    'An equal older query cannot replace the latest pending request');
  Check(LMenu.Present(LNewTicket, Reply(LQuery), LSources), 'The latest ticket remains publishable');
  LOther := NewNyxPascalCompletionMenu(LHistory);
  LOther.BeginQuery(LSources, LMain, LMain, LOtherTicket);
  Check(not LOther.Present(LNewTicket, Reply(LQuery), LSources),
    'Another view cannot borrow this menu ticket');

  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  LHistory.Observe(Frame(CPrefix + CTail, LStart + 3));
  Check(not LMenu.Present(LTicket, Reply(LQuery), LSources) and (LMenu.State = pcvClosed),
    'A moved caret retires suggestions without changing source');
  LHistory.Reset(LBefore);
  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  LHistory.Observe(Frame(CPrefix + CTail + 'x', LStart + 4));
  LHistory.Undo;
  Check(LHistory.Current.SameValue(LBefore) and
    not LMenu.Present(LTicket, Reply(LQuery), LSources),
    'Editing and Undo back to equal bytes cannot revive an old result');
  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  LChanged := LSources.WithBuffer(LHelper, '{ A newer dependent helper }');
  Check(not LMenu.Present(LTicket, Reply(LQuery), LChanged),
    'Changed supporting buffers retire a page even when its current text is identical');

  LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  LReply := NewNyxPascalCompletionResult(LQuery, pcsUnavailable, '', 0, '', [], 0);
  Check(LMenu.Present(LTicket, LReply, LSources) and
    (LMenu.MessageText = 'Completion is not configured'), 'Unavailable provider gets useful presentation without fake rows');
  Check(LMenu.Key(NyxKeyStroke(nkEscapeKey), LSources) = pcaDismiss, 'Escape closes capability feedback');
  LHistory.Observe(LBefore, True);
  LRejected := False;
  try
    LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LMenu.Key(NyxKeyStroke(nkSpaceKey, [nmControl]), LSources) = pcaNone),
    'Composition retains its native keys and cannot request a completion');
end;

begin
  try
    HistoryChecks;
    MenuChecks;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' completion view/history checks';
    {$else}
    WriteLn('PASS ', GChecks, ' completion view/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
