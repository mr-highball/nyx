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

unit nyx.test.source.managed;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

function RunNyxManagedSourceTests: Integer;
{ Emits a real hand-edited, visually regenerated companion for both compilers.
  Caller owns the returned design. Source includes deliberate names, comments,
  an unchanged constant expression and a reusable definition. }
function CreateNyxManagedSourceFixture(out ASource: TNyxText): TNyxDocument;
{ Exact fixture substitution, shared by real editor journeys. This intentionally
  edits arbitrary occurrences; production identifier reconciliation is tokenized. }
function EditNyxManagedFixture(const ASource, ABefore, AAfter: TNyxText): TNyxText;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.data,
  nyx.state,
  nyx.codec,
  nyx.codegen,
  nyx.controls,
  nyx.contract,
  nyx.callbacks,
  nyx.source,
  nyx.studio.session;

function ReplaceFirst(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
  LParts: TNyxStrings;
begin
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    raise Exception.Create('Managed fixture could not find: ' + ABefore);
  end;
  LParts := TNyxStrings.Create;
  try
    LParts.Add(Copy(ASource, 1, LPosition - 1));
    LParts.Add(AAfter);
    LParts.Add(Copy(ASource, LPosition + Length(ABefore), MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function EditNyxManagedFixture(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LParts: TNyxStrings;
  LCursor: Integer;
  LPosition: Integer;
begin
  { Fixture edits use the same exact portable storage as the production lexer.
    Native SysUtils.StringReplace takes ANSI String and can change user text. }

  if ABefore = '' then
  begin
    raise Exception.Create('A fixture substitution requires a nonempty match');
  end;
  LParts := TNyxStrings.Create;
  try
    LCursor := 1;
    LPosition := Pos(ABefore, ASource);
    while LPosition > 0 do
    begin
      LParts.Add(Copy(ASource, LCursor, LPosition - LCursor));
      LParts.Add(AAfter);
      LCursor := LPosition + Length(ABefore);
      LPosition := Pos(ABefore, Copy(ASource, LCursor, MaxInt));

      if LPosition > 0 then
      begin
        Inc(LPosition, LCursor - 1);
      end;
    end;
    LParts.Add(Copy(ASource, LCursor, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function AuthoredSource(ADocument: TNyxDocument): TNyxText;
begin
  Result := TNyxCodegen.Generate(ADocument, 'nyx.managed.view');
  Result := EditNyxManagedFixture(Result, 'LNotesMemo', 'LJournalMemo');
  Result := EditNyxManagedFixture(Result, 'LTitleLabel', 'LQuietHeading');
  Result := EditNyxManagedFixture(Result, 'LReplyTextState', 'LDraftReply');
  Result := ReplaceFirst(Result, 'LJournalMemo is literal help text',
    'LNotesMemo is literal help text');
  Result := ReplaceFirst(Result, '.Text(''A thoughtful title'')',
    '.Text(TNyxText(''A thoughtful '') + { word choice / 🌙 } TNyxText(''title''))');
  Result := ReplaceFirst(Result, '.Gap(12)', '.Gap({ rhythm / 🌙 } 12)');
  Result := ReplaceFirst(Result, 'NyxData(''Thoughtful'')',
    'NyxData(TNyxText(''Thought'') + { nested wording / 🌙 } TNyxText(''ful''))');
  Result := ReplaceFirst(Result, 'LJournalMemo := NewNyxMemo',
    '{ Journal notes are deliberately named / 漢字. }' + #10 +
    '    LJournalMemo := NewNyxMemo');
  Result := ReplaceFirst(Result,
    '    LJournalMemo.Contract' + #10 +
    '      .Value(NyxTextDomain.Choices(['''', ''First line'']));' + #10, '');
  Result := ReplaceFirst(Result, '  except',
    '    LJournalMemo.Contract.Value(NyxTextDomain.Choices(['''', ''First line'']));' + #10 +
    '  except');
end;

function BaseDesign: TNyxDocument;
var
  LPage: TNyxNode;
  LMemo: TNyxNode;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Handcrafted views';
    Result.State.SetValue(NyxTextState('reply'), TNyxText('Ready to write / 🌙'));
    Result.Extensions.SetValue(NyxExtension('app.notes'), NyxObject([
      NyxField('caption', NyxData(TNyxText('Thoughtful'))), NyxField('revision', NyxData(1))]));
    LPage := TNyxNode.Create(nkColumn, 'home').Configure.Padding(20).Gap(12).Done;
    Result.AddPage(LPage);
    LPage.Add(TNyxNode.Create(nkLabel, 'title').Configure.Text('A thoughtful title').Done);
    LMemo := TNyxNode.Create(nkMemo, 'notes').Configure.Text('Notes').Value('First line')
      .Hint('LNotesMemo is literal help text').Done;
    LPage.Add(LMemo);
    LMemo.Contract.Value(NyxTextDomain.Choices(['', 'First line']));
    Result.AddComponent(TNyxNode.Create(nkCard, 'welcome')
      .Add(TNyxNode.Create(nkLabel, 'headline').Configure
        .PartName(NyxPart('headline')).Text('A quiet corner').Done));
  except
    Result.Free;
    raise;
  end;
end;

function CreateNyxManagedSourceFixture(out ASource: TNyxText): TNyxDocument;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LLine: Integer;
begin
  LDocument := BaseDesign;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.SetSourceDraft(AuthoredSource(LDocument));
    LSession.ApplySourceDraft;
    LSession.Select('home');
    LSession.SetProperty('gap', '18');
    LSession.AddControl(NewNyxMemo('journal').WithText('Another journal'));
    LSession.RenameState('reply', 'journal/reply');
    LSession.Select('notes');
    LSession.AddCallback(ntAfterEnter, LLine);
    ASource := LSession.Source;
    Result := LSession.Document.Clone;
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

function RunNyxManagedSourceTests: Integer;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LBeforeSource: TNyxText;
  LSnapshot: TNyxText;
  LCandidate: TNyxDocument;
  LReduced: TNyxDocument;
  LControl: INyxMemo;
  LLine: Integer;
  LWorkspace: TNyxSourceWorkspace;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Managed source: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Reject(const ABefore, AAfter: TNyxText);
  begin
    LSession.SetSourceDraft(ReplaceFirst(LSession.Source, ABefore, AAfter));
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore) and (LSession.Source = LBeforeSource),
      'invalid rename/type/construction retains the accepted pair: ' + AAfter);
    Check(LSession.DraftSource <> LBeforeSource, 'rejected spelling remains an editable draft');
    LSession.DiscardSourceDraft;
  end;

begin
  Result := 0;
  LDocument := BaseDesign;
  LSession := TNyxStudioSession.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    LBefore := LSession.Save;
    LSource := AuthoredSource(LDocument);
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;

    Check(LSession.Save = LBefore,
      'deliberate control/state names and comments admit without changing the design');
    Check(LSession.Source = LSource,
      'accepted names and comments remain exact in the source');
    LSession.Select('home');
    LSession.SetProperty('gap', '18');
    LSource := LSession.Source;
    Check(Pos('LJournalMemo: INyxMemo;', LSource) > 0,
      'visual regeneration keeps the specialized authored local');
    Check(Pos('LDraftReply: TNyxTextStateRef;', LSource) > 0,
      'visual regeneration keeps the authored typed state local');
    Check(Pos('TNyxText(''A thoughtful '') + { word choice / 🌙 } TNyxText(''title'')', LSource) > 0,
      'an unrelated change retains the exact authored constant expression');
    Check((Pos('{ rhythm / 🌙 }', LSource) > 0) and
      (LSession.Document.Find('home').Prop('gap') = '18'),
      'changed values retain their embedded comments and new meaning');
    Check(Pos('{ Journal notes are deliberately named / 漢字. }', LSource) > 0,
      'comments beside control construction remain exact');
    Check(Pos('LNotesMemo is literal help text', LSource) > 0,
      'identifier replacement never rewrites ordinary user text');
    LSession.Undo;
    Check((LSession.Source = AuthoredSource(LDocument)) and
      (LSession.Document.Find('home').Prop('gap') = '12'),
      'paired undo restores exact accepted source spelling');
    LSession.Redo;
    Check(LSession.Source = LSource, 'paired redo restores reconciled source exactly');
    LControl := NewNyxMemo('journal').WithText('Another journal');
    LSession.AddControl(LControl);
    LControl.WithText('Only the caller changes');
    Check(LSession.Document.Find('journal').Prop('text') = 'Another journal',
      'typed insertion borrows the implementation and owns an independent clone');
    Check((Pos('LJournalMemo: INyxMemo;', LSession.Source) > 0) and
      (Pos('LJournalMemo2: INyxMemo;', LSession.Source) > 0),
      'a new default cannot collide with an existing authored local');
    LSession.RenameState('reply', 'journal/reply');
    Check((Pos('LDraftReply: TNyxTextStateRef;', LSession.Source) > 0) and
      (Pos('LDraftReply := NyxTextState(''journal/reply'')', LSession.Source) > 0),
      'explicit state-key migration keeps the deliberate Pascal name');
    LSession.Select('notes');
    LSession.AddCallback(ntAfterEnter, LLine);
    Check((LLine > 0) and
      (LSession.Document.Find('notes').Extensions.Key(0).Name = NyxContractKey) and
      (LSession.Document.Find('notes').Extensions.Key(1).Name = NyxCallbacksKey),
      'new metadata follows the retained authored contract and preserves exact store order');
    LSession.AddCallback(ntAfterEnter, LLine);
    Check(Length(NyxAuthoredEvents(LSession.Document.Find('notes'))[0].Callbacks) = 2,
      'a later visual registration updates the retained authored callback block');
    LSession.SetExtension(seoDocument, NyxExtension('app.notes'), NyxObject([
      NyxField('caption', NyxData(TNyxText('Thoughtful'))), NyxField('revision', NyxData(2))]));
    Check((Pos('nested wording / 🌙', LSession.Source) > 0) and
      (Pos('TNyxText(''Thought'')', LSession.Source) > 0) and
      (LSession.Document.Extensions.Value(NyxExtension('app.notes')).Field('revision').AsInteger = 2),
      'changing a nested sibling retains the unchanged authored data expression');
    LSession.SetExtension(seoDocument, NyxExtension(''), NyxData(TNyxText(';')));
    Check((LSession.Document.Extensions.Value(NyxExtension('')).AsText = ';') and
      (Pos('{ <nyx:source:', LSession.Source) = 0),
      'empty open keys and semicolon literals retain meaning without private merge markers');
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    Reject('LJournalMemo: INyxMemo;', 'Result: INyxMemo;');
    Reject('LJournalMemo: INyxMemo;', 'LQuietHeading: INyxMemo;');
    Reject('LJournalMemo: INyxMemo;', 'LJournalMemo: INyxBadge;');
    Reject('NewNyxMemo(''notes'')', 'NewNyxBadge(''notes'')');
    LWorkspace.Accept(LSession.Document, LSession.Source);
    LSnapshot := LWorkspace.Snapshot;
    LWorkspace.Restore(LSnapshot);
    Check(LWorkspace.Render(LSession.Document) = LBeforeSource,
      'workspace recovery retains names, expressions and comments exactly');
    LCandidate := LWorkspace.Candidate(LSession.Document, LBeforeSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore,
        'a reconciled companion remains editable through the ordinary reader');
    finally
      LCandidate.Free;
    end;
    Check(PrepareNyxCompanion(LSession.Document, LSession.Document, LSession.Source, False) =
      LSession.Source, 'a full application build retains exact accepted bytes');
    LReduced := TNyxDocument.Create;
    try
      LReduced.Title := LSession.Document.Title;
      LReduced.AddPage(LSession.Document.Pages[0].Clone);
      LReduced.State.SetValue(NyxTextState('journal/reply'),
        LSession.Document.State.GetValue(NyxTextState('journal/reply')));
      LSource := PrepareNyxCompanion(LSession.Document, LReduced, LSession.Source, True);
      Check((Pos('LJournalMemo: INyxMemo;', LSource) > 0) and
        (Pos('word choice / 🌙', LSource) > 0) and
        (Pos('LWelcomeCard: INyxCard;', LSource) = 0),
        'an isolated page retains selected authored names and expressions');
    finally
      LReduced.Free;
    end;
    LSession.Select('notes');
    LSession.DeleteSelected;
    Check((LSession.Document.Find('notes') = nil) and
      (Pos('Journal notes are deliberately named', LSession.Source) > 0),
      'deleting a control retains its authored notes as Pascal comments');
    LSession.SetSourceDraft(ReplaceFirst(LSession.Source,
      '.Text(TNyxText(''A thoughtful '') + { word choice / 🌙 } TNyxText(''title''))',
      '{ Deliberately empty configuration. }'));
    LSession.ApplySourceDraft;
    LSession.Select('title');
    LSession.SetProperty('text', 'An edited title');
    Check((LSession.Document.Find('title').Prop('text') = 'An edited title') and
      (Pos('Deliberately empty configuration.', LSession.Source) > 0),
      'a visual insertion keeps and fills an authored empty configuration block');
    { A real size-boundary failure exercises rollback after the design command,
      without relying on a test-only reconciler switch or corrupting a snapshot. }
    LSource := ReplaceFirst(LSession.Source, 'implementation' + #10,
      'implementation' + #10 + '{' + TNyxText(StringOfChar('x', 4 * 1024 * 1024 -
        Length(LSession.Source) - 64 * 1024)) + '}' + #10);
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LSession.SetTitle('A shorter title');
    LSession.Undo;
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    LRejected := False;
    try
      LSession.SetTitle(TNyxText(StringOfChar('y', 128 * 1024)));
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore) and (LSession.Source = LBeforeSource),
      'a source-size failure retains both members of the accepted visual pair');
    LSession.Redo;
    Check(LSession.Document.Title = 'A shorter title',
      'a rejected visual command preserves the preceding redo entry');
  finally
    LWorkspace.Free;
    LSession.Free;
    LDocument.Free;
  end;
end;

end.
