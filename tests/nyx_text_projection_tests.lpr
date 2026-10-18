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

program nyx_text_projection_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.text.projection, nyx.editing.history,
  nyx.pascal.project, nyx.pascal.names, nyx.pascal.sources, nyx.pascal.drafts,
  nyx.pascal.completion, nyx.pascal.completion.view
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure HistoryAndCompletion;
const
  CAccepted: TNyxText = 'unit projected.fixture; interface implementation end.';
  CAuthored: TNyxText = '// Header 🚀' + #13#10 + 'LBadge.WithTe';
  CPhysical: TNyxText = '// Header 🚀' + #10 + 'LBadge.WithTe';
  CInsertion: TNyxText = 'WithText' + #13#10 + 'Next';
  CExpectedAuthor: TNyxText = '// Header 🚀' + #13#10 + 'LBadge.WithText' + #13#10 + 'Next';
  CExpectedPhysical: TNyxText = '// Header 🚀' + #10 + 'LBadge.WithText' + #10 + 'Next';
var
  LFrame: TNyxTextEditorFrame;
  LAfter: TNyxTextEditorFrame;
  LHistory: INyxTextEditHistory;
  LMenu: INyxPascalCompletionMenu;
  LTicket: TNyxPascalCompletionTicket;
  LSources: INyxPascalSourceSnapshot;
  LCandidate: INyxPascalSourceSnapshot;
  LQuery: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LRejected: Boolean;
begin
  LFrame := NyxProjectedTextEditorFrame(NewNyxTextProjection(CAuthored, CPhysical),
    NyxTextSelection(CPhysical, NyxTextScalarCount(CPhysical), NyxTextScalarCount(CPhysical)), nteLF);
  LHistory := NewNyxTextEditHistory(LFrame);
  LMenu := NewNyxPascalCompletionMenu(LHistory);
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('projected.fixture'), CAccepted))
    .WithBuffer(TNyxPascalSourceRef.Main, CAuthored);
  LQuery := LMenu.BeginQuery(LSources, TNyxPascalSourceRef.Main,
    TNyxPascalSourceRef.Main, LTicket);
  Check((LQuery.Caret = NyxTextScalarCount(CAuthored)) and LQuery.Matches(LSources),
    'Completion captures authored offsets while retaining its physical caret');
  LReply := NewNyxPascalCompletionResult(LQuery, pcsReady, '',
    NyxTextScalarCount(CAuthored) - 6, 'WithTe',
    [NyxPascalCompletion('WithText', CInsertion, 'A typed projected insertion', pckMethod)], 1);
  Check(LMenu.Present(LTicket, LReply, LSources), 'An exact authored provider reply presents normally');
  LCandidate := LMenu.Accept(LSources);
  LAfter := LHistory.Current;
  Check((LCandidate.Text(TNyxPascalSourceRef.Main) = CExpectedAuthor) and
    (LAfter.AuthoredText = CExpectedAuthor) and (LAfter.Text = CExpectedPhysical),
    'Completion preserves untouched authored bytes and applies explicit browser newline presentation');
  Check((LHistory.UndoCount = 1) and
    (LAfter.Selection.Start = NyxTextScalarCount(CExpectedPhysical)),
    'One projected completion is one text entry with an exact physical scalar caret');
  LHistory.Undo;
  Check(LHistory.Current.SameValue(LFrame), 'Undo restores authored/displayed bytes, policy and caret together');
  LHistory.Redo;
  Check(LHistory.Current.SameValue(LAfter), 'Redo restores the complete projected completion frame');
  Check(LCandidate.AcceptedText(TNyxPascalSourceRef.Main) = CAccepted,
    'Projected acceptance remains a draft and never admits accepted source');

  LFrame := NyxProjectedTextEditorFrame(NewNyxTextProjection('A' + #13#10 + 'B', 'A' + #10 + 'B'),
    NyxTextSelection('A' + #10 + 'B', 3, 3), nteLF);
  Check(LFrame.RetainedBytes = 39, 'Distinct authored text participates in the exact frame byte budget');
  LHistory := NewNyxTextEditHistory(LFrame);
  LHistory.Replace(LFrame, 1, #10, #10);
  Check((LHistory.Current.Text = LFrame.Text) and
    (LHistory.Current.AuthoredText = TNyxText('A') + #10 + 'B') and (LHistory.UndoCount = 1),
    'An authored-only newline edit remains undoable when display text is identical');
  LHistory.Undo;
  Check(LHistory.Current.SameValue(LFrame), 'Authored-only Undo restores original source bytes');
  LHistory := NewNyxTextEditHistory(LFrame, TNyxTextHistoryLimits.Defaults.UpToBytes(40));
  LRejected := False;
  try
    LHistory.Replace(LFrame, 3, '', 'B');
  except
    on ENyxTextHistory do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LHistory.Current.SameValue(LFrame) and not LHistory.CanUndo,
    'Projected byte-budget failure refuses before either frame or stack changes');
end;

procedure Run;
const
  CAuthored: TNyxText = 'A🚀' + #10 + 'B' + #13 + '𐐷' + #13#10 + 'end' + #0;
  CBody: TNyxText = 'A🚀' + #13#10 + 'B' + #13#10 + '𐐷' + #13#10 + 'end' + #0;
  CCode: TNyxText = '// Header 🚀' + #13#10 + 'LBadge.WithTexx;' + #10 + '// Tail' + #13;
  CCodePhysical: TNyxText = '// Header 🚀' + #10 + 'LBadge.WithTexx;' + #10 + '// Tail' + #10;
  CExpected: TNyxText = '// Header 🚀' + #13#10 + 'LBadge.WithText;' + #10 + '// Tail' + #13;
var
  LProjection: INyxTextProjection;
  LOther: INyxTextProjection;
  LResult: INyxTextProjection;
  LInsert: TNyxTextInsertion;
  LRejected: Boolean;
  LAt: Integer;
  LA: TNyxTextLineEndings;
  LP: TNyxTextLineEndings;
  LSource: TNyxText;
  LView: TNyxText;
  LMapped: Integer;
  LSelection: TNyxTextSelection;
begin
  LProjection := NewNyxTextProjection(CAuthored, CBody);
  Check((LProjection.Authored = CAuthored) and (LProjection.Presented = CBody),
    'Whole mixed line endings, supplementary text and NUL remain exact');
  for LAt := 0 to LProjection.AuthoredScalars do
  begin
    Check(LProjection.ToAuthored(LProjection.ToPresented(LAt)) = LAt,
      'Every representable authored scalar boundary roundtrips exactly');
  end;
  LRejected := False;
  try
    LProjection.ToAuthored(3);
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Default exact projection refuses a physical CRLF interior');
  Check((LProjection.ToAuthored(3, npaBefore) = 2) and
    (LProjection.ToAuthored(3, npaAfter) = 3), 'Explicit affinity selects the complete newline edge');
  LOther := NewNyxTextProjection(CBody, CAuthored);
  LRejected := False;
  try
    LOther.ToPresented(3);
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Reverse exact mapping also refuses an authored CRLF interior');
  LSelection := LProjection.PresentedSelection(NyxTextSelection(CAuthored, 0,
    LProjection.AuthoredScalars, ntdBackward));
  Check(LProjection.AuthoredSelection(LSelection).SameRange(
    NyxTextSelection(CAuthored, 0, LProjection.AuthoredScalars, ntdBackward)),
    'Selection edges and direction project without guessing');
  Check(not LProjection.AuthoredSelection(Default(TNyxTextSelection)).Defined,
    'Unavailable selection stays unavailable');

  for LA := nteLF to nteCR do
  begin
    LSource := NyxPresentText(CAuthored, LA);
    for LP := nteLF to nteCR do
    begin
      LView := NyxPresentText(CAuthored, LP);
      LOther := NewNyxTextProjection(LSource, LView);
      for LAt := 0 to LOther.AuthoredScalars do
      begin
        LRejected := False;
        try
          LMapped := LOther.ToPresented(LAt);
        except
          on ENyxTextProjection do
          begin
            LRejected := True;
          end;
        end;

        if not LRejected then
        begin
          Check(LOther.ToAuthored(LMapped) = LAt, 'All newline-policy pairs preserve exact representable boundaries');
        end
        else
        begin
          Check((LA = nteCRLF) and (LP <> nteCRLF), 'Only a wider authored newline may lack an exact boundary');
        end;
      end;
    end;
  end;
  Check(NyxPresentText(CAuthored, ntePreserve) = CAuthored,
    'Preserve presentation does not normalize authored text');
  LProjection := NewNyxTextProjection(CCode, CCodePhysical);
  LResult := LProjection.ObservePresented(
    '// Header 🚀' + #10 + 'LBadge.WithText;' + #10 + '// Tail' + #10);
  Check((LResult.Authored = CExpected) and (LProjection.Authored = CCode),
    'Observed member editing preserves all untouched mixed authored endings and immutable input');
  LResult := LProjection.ReplacePresented(0, '', NyxTextInsertion('Start' + #13#10)
    .UsingLineEndings(nteLF));
  Check((LResult.Authored = TNyxText('Start') + #13#10 + CCode) and
    (LResult.Presented = TNyxText('Start') + #10 + CCodePhysical),
    'An explicit insertion owns distinct authored and displayed bytes');
  LResult := NewNyxTextProjection('A' + #10 + 'B', 'A' + #13#10 + 'B')
    .ObservePresented('A' + #13 + 'B');
  Check((LResult.Authored = TNyxText('A') + #10 + 'B') and
    (LResult.Presented = TNyxText('A') + #13 + 'B'), 'Target-only newline respelling retains the original authoring');
  LResult := NewNyxTextProjection('A' + #10 + 'B', 'A' + #13#10 + 'B')
    .ObservePresented('A' + #13 + 'X' + #10 + 'B');
  Check(LResult.Authored = TNyxText('A') + #13 + 'X' + #10 + 'B',
    'A physical edit inside a newline expands to exact complete mapped edges');
  LResult := NewNyxTextProjection('Aé', 'Aé').ObservePresented('AΩ');
  Check(LResult.Authored = TNyxText('AΩ'), 'Common UTF-8 suffix bytes cannot split different Unicode scalars');
  LResult := NewNyxTextProjection('éZ', 'éZ').ObservePresented('ñZ');
  Check(LResult.Authored = TNyxText('ñZ'), 'Common scalar prefix comparison never splits UTF-8 lead bytes');
  LRejected := False;
  try
    NewNyxTextProjection('é', 'é');
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Canonical-looking combining forms are not silently normalized');
  LRejected := False;
  try
    LProjection.ReplacePresented(0, 'foreign', NyxTextInsertion('new'));
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LProjection.Authored = CCode), 'Wrong expected bytes refuse without changing the projection');
  LRejected := False;
  try
    LProjection.ToAuthored(-1);
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Negative scalar boundaries refuse');
  LRejected := False;
  try
    LProjection.PresentedSlice(0, LProjection.PresentedScalars + 1);
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Overlong scalar ranges refuse');
  LInsert := Default(TNyxTextInsertion);
  LRejected := False;
  try
    LInsert.Validate;
  except
    on ENyxTextProjection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Undefined insertion values refuse');
end;

begin
  try
    Run;
    HistoryAndCompletion;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' text projection checks';
    {$else}
    WriteLn('PASS ', GChecks, ' text projection checks');
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
