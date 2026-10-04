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

unit nyx.test.source.structural;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

{ Replays typed construction, ownership and rejection on detached candidates;
  checks exact paired history and reduced-view companions on both compilers. }
function RunNyxStructuralSourceTests: Integer;
{ Shared authored source gesture for actual browser/native editor journeys.
  Adds a typed page, a bound memo, an action and a reusable component/instance. }
function AddNyxStructuralFixture(const ASource: TNyxText): TNyxText;
{ Caller owns the source-edited design. Both real compilers execute its exact
  companion after a subsequent visual property edit, rather than regenerated
  source made only from the expected design. }
function CreateNyxStructuralSourceFixture(out ASource: TNyxText): TNyxDocument;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.state,
  nyx.binding.types,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.controls,
  nyx.composition,
  nyx.studio.builds,
  nyx.studio.session,
  nyx.test.source.managed;

function BaseDocument: TNyxDocument;
var
  LHome: INyxColumn;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Crafted structure';
    LHome := NewNyxColumn('home');
    Result.AddPage(LHome);
    LHome.Add(NewNyxLabel('intro').WithText('A quiet start / 🌙'));
    LHome.Add(NewNyxMemo('obsolete'));
  except
    Result.Free;
    raise;
  end;
end;

function AddNyxStructuralFixture(const ASource: TNyxText): TNyxText;
begin
  Result := EditNyxManagedFixture(ASource, 'var' + #10,
    'var' + #10 +
    '  LNotesPage: INyxColumn;' + #10 +
    '  LReplyEditor: INyxMemo;' + #10 +
    '  LSendReply: INyxButton;' + #10 +
    '  LNoticeCard: INyxCard;' + #10 +
    '  LNoticeBadge: INyxBadge;' + #10 +
    '  LNoticeInstance: INyxComponent;' + #10 +
    '  LQuickAction: INyxLabeledButton;' + #10 +
    '  LReplyText: TNyxTextStateRef;' + #10);
  Result := EditNyxManagedFixture(Result, '  except' + #10,
    '    { A page written by hand / 🌙 漢字. }' + #10 +
    '    LReplyText := NyxTextState(''code/reply'');' + #10 +
    '    Result.State.SetValue(LReplyText, ''From crafted source / 🌙'');' + #10 +
    '    LNoticeCard := NewNyxCard(''code-notice'');' + #10 +
    '    Result.AddComponent(LNoticeCard);' + #10 +
    '    LNoticeBadge := NewNyxBadge(''code-badge'').WithText(''CRAFTED'');' + #10 +
    '    LNoticeCard.Add(LNoticeBadge);' + #10 +
    '    LNoticeBadge.Configure.PartName(NyxPart(''caption'')).Done;' + #10 +
    '    LNotesPage := NewNyxColumn(''code-notes'');' + #10 +
    '    Result.AddPage(LNotesPage);' + #10 +
    '    LReplyEditor := NewNyxMemo(''code-reply'').WithText(''Reply notes'');' + #10 +
    '    LNotesPage.Add(LReplyEditor);' + #10 +
    '    LReplyEditor.Binds.Value(LReplyText).Done;' + #10 +
    '    LSendReply := TNyxButton.Create(''code-send'').WithText(''Send reply'');' + #10 +
    '    LNotesPage.Add(LSendReply);' + #10 +
    '    LQuickAction := NewNyxLabeledButton(''code-action'');' + #10 +
    '    LNotesPage.Add(LQuickAction);' + #10 +
    '    LNoticeInstance := NewNyxComponent(''code-instance'');' + #10 +
    '    LNotesPage.Insert(0, LNoticeInstance);' + #10 +
    '    LNoticeInstance.Configure.Component(NyxComponent(''code-notice'')).Done;' + #10 +
    '  except' + #10);
end;

function CreateNyxStructuralSourceFixture(out ASource: TNyxText): TNyxDocument;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
begin
  LDocument := BaseDocument;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    ASource := AddNyxStructuralFixture(LSession.Source);
    ASource := EditNyxManagedFixture(ASource, '  LObsoleteMemo: INyxMemo;' + #10, '');
    ASource := EditNyxManagedFixture(ASource,
      '    LObsoleteMemo := NewNyxMemo(''obsolete'');' + #10 +
      '    LHomeColumn.Add(LObsoleteMemo);' + #10, '');
    { Move the original label's construction/adoption after the new page exists.
      Reparenting changes the source ownership statement, without cloned residue. }
    ASource := EditNyxManagedFixture(ASource,
      '    LHomeColumn.Add(LIntroLabel);' + #10, '');
    ASource := EditNyxManagedFixture(ASource, '  except' + #10,
      '    LNotesPage.Add(LIntroLabel);' + #10 + '  except' + #10);
    { Configuration must follow ownership. Move its authored block too. }
    ASource := EditNyxManagedFixture(ASource,
      '    LIntroLabel.Configure' + #10 +
      '      .Text(''A quiet start / 🌙'')' + #10 + '      .Done;' + #10, '');
    ASource := EditNyxManagedFixture(ASource, '  except' + #10,
      '    LIntroLabel.Configure.Text(''A quiet start / 🌙'').Done;' + #10 + '  except' + #10);
    LSession.SetSourceDraft(ASource);
    LSession.ApplySourceDraft;
    LSession.Select('code-reply');
    LSession.SetProperty('hint', 'Keep a thoughtful reply / 🌙');
    LSession.Select('code-action');
    LSession.SetProperty('gap', '9');
    ASource := EditNyxManagedFixture(LSession.Source, 'unit nyx.generated.view;',
      'unit nyx.structural.view;');
    Result := LSession.Document.Clone;
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

function RunNyxStructuralSourceTests: Integer;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LDraft: TNyxText;
  LBefore: TNyxText;
  LBeforeSource: TNyxText;
  LRejected: Boolean;
  LRuntime: TNyxNode;
  LWords: TNyxStrings;
  LJoined: TNyxText;
  LIsolated: TNyxDocument;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Structural source: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Reject(const ABefore, AAfter: TNyxText);
  begin
    LDraft := EditNyxManagedFixture(LSource, ABefore, AAfter);
    LCandidate := nil;
    LRejected := False;
    try
      try
        LCandidate := LWorkspace.Candidate(LDocument, LDraft);
      except
        on LException: Exception do
        begin
          LRejected := True;
          Check(LException.Message <> '', 'diagnostic for ' + AAfter);
        end;
      end;
    finally
      LCandidate.Free;
      LCandidate := nil;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore) and
      (LWorkspace.Render(LDocument) = LSource), 'atomic rejection of ' + AAfter);
  end;

begin
  Result := 0;
  LWords := TNyxStrings.Create;
  try
    LWords.Add('Joined / 🌙 漢字' + #0 + 'exact');
    LJoined := 'A reused caller buffer';
    {$IFNDEF PAS2JS}
    { Reproduce a caller buffer whose runtime tag was lost by native ASCII
      concatenation. The join must return exact UTF-8 to its next typed consumer. }
    SetCodePage(RawByteString(LJoined), 0, False);
    {$ENDIF}
    LJoined := LWords.Join;
    {$IFNDEF PAS2JS}

    if StringCodePage(LJoined) <> CP_UTF8 then
    begin
      raise Exception.Create('Joined native text lost its UTF-8 runtime tag');
    end;
    {$ENDIF}
    Check(LJoined = TNyxText('Joined / 🌙 漢字') + #0 + 'exact',
      'joined text remains exact Unicode/NUL through reused native result buffers');
  finally
    LWords.Free;
  end;
  LDocument := BaseDocument;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    LDraft := AddNyxStructuralFixture(LSource);
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check((LCandidate.Count = 2) and (LCandidate.ComponentCount = 1),
        'source creates a page and an independent reusable definition');
      Check((LCandidate.Find('code-reply').Kind = 'memo') and
        (LCandidate.Find('code-reply').Prop('text') = 'Reply notes'),
        'specialized factories and fluent captions retain their exact kind');
      Check(LCandidate.Find('code-notes').Children[0].ID = 'code-instance',
        'typed Insert preserves source-defined child order');
      Check(LCandidate.State.GetValue(NyxTextState('code/reply')) = 'From crafted source / 🌙',
        'new declared state and binding travel with the source-created control');
      Check((LCandidate.Find('code-action').Count > 0) and
        (LCandidate.Find('code-action').Part(NyxPart('button')) <> nil),
        'default compound construction supplies independently owned reusable parts');
      LRuntime := RealizeNyxView(LCandidate, LCandidate.Find('code-instance'));
      try
        Check(LRuntime.Part('caption').Prop('text') = 'CRAFTED',
          'source-created reusable instance realizes its typed badge part');
      finally
        LRuntime.Free;
      end;
      Check(TNyxCodec.Encode(LDocument) = LBefore, 'source construction leaves its baseline independent');
    finally
      LCandidate.Free;
    end;
    LDraft := AddNyxStructuralFixture(LSource);
    LDraft := EditNyxManagedFixture(LDraft, 'LReplyText: TNyxTextStateRef;',
      'LReplyText, LUnusedBoolean: TNyxBooleanStateRef;');
    LDraft := EditNyxManagedFixture(LDraft, 'NyxTextState(''code/reply'')',
      'NyxBooleanState(''code/reply'')');
    LDraft := EditNyxManagedFixture(LDraft, '''From crafted source / 🌙''', 'True');
    LDraft := EditNyxManagedFixture(LDraft, 'LReplyEditor.Binds.Value(LReplyText)',
      'LReplyEditor.Binds.Enabled(LReplyText)');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(LCandidate.State.Value('code/reply').Kind = nskBoolean,
        'grouped declaration and consistent typed factory/use can change scalar family');
      Check(LCandidate.Find('code-reply').Bindings[0].Target = bpEnabled,
        'changed family requires an appropriately typed binding target');
    finally
      LCandidate.Free;
    end;
    Reject('LIntroLabel: INyxLabel;', 'LIntroLabel: INyxBadge;');
    Reject('LIntroLabel: INyxLabel;', 'LIntroLabel: TNyxNode;');
    Reject('LIntroLabel: INyxLabel;', 'LIntroLabel: Integer;');
    Reject('LIntroLabel: INyxLabel;', 'LIntroLabel: INyxTextInput;');
    Reject('LIntroLabel: INyxLabel;', 'LIntroLabel: INyxNode;');
    Reject('LIntroLabel: INyxLabel;', 'LHomeColumn: INyxLabel;');
    Reject('LIntroLabel: INyxLabel;', 'Result: INyxLabel;');
    Reject('NewNyxLabel(''intro'')', 'NewNyxBadge(''intro'')');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel('''')');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel(''home'')');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel(1)');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel(''intro'', ''descriptor'')');
    Reject('NewNyxLabel(''intro'')', 'NewNyxBuiltinControl(nkLabel, ''intro'')');
    Reject('LHomeColumn.Add(LIntroLabel);', 'Result.AddComponent(LIntroLabel); LHomeColumn.Add(LIntroLabel);');
    Reject('LHomeColumn.Add(LIntroLabel);', 'LIntroLabel.Add(LHomeColumn);');
    Reject('LHomeColumn.Add(LIntroLabel);', 'LHomeColumn.Add(LUnknownLabel);');
    Reject('LHomeColumn.Add(LIntroLabel);', '');
    Reject('Result.AddPage(LHomeColumn);', 'LHomeColumn.AddPage(LHomeColumn);');
    Reject('Result.AddPage(LHomeColumn);', 'Result.Add(LHomeColumn);');
    Reject('LHomeColumn.Add(LIntroLabel);', 'LHomeColumn.Insert(-1, LIntroLabel);');
    Reject('LHomeColumn.Add(LIntroLabel);', 'LHomeColumn.Insert(20, LIntroLabel);');
    Reject('LHomeColumn.Add(LIntroLabel);', 'LHomeColumn.Insert(''0'', LIntroLabel);');
    Reject('Result.Free;', 'Result.Free; Result := nil;');
    Reject('raise;', '');
    Reject('try' + #10, 'try' + #10 + '    if True then');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel(''intro''); LIntroLabel := NewNyxLabel(''second'')');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel(''intro'').Unknown(''caption'')');
    Reject('NewNyxLabel(''intro'')', 'NewNyxLabel(''intro'').WithText(1)');
    { Base-family widening and grouped declarations follow real Pascal typing. }
    LDraft := EditNyxManagedFixture(LSource, 'LIntroLabel: INyxLabel;', 'LIntroLabel: INyxCaptionControl;');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore, 'caption-family widening retains design meaning');
    finally
      LCandidate.Free;
    end;
    LDraft := EditNyxManagedFixture(LSource, '  LIntroLabel: INyxLabel;' + #10,
      '  LIntroLabel, LUnusedCaption: INyxLabel;' + #10);
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore, 'grouped unused interface declaration is harmless');
    finally
      LCandidate.Free;
    end;
    LDraft := EditNyxManagedFixture(LSource, 'LIntroLabel: INyxLabel;', 'LIntroLabel: INyxControl;');
    LDraft := EditNyxManagedFixture(LDraft, 'NewNyxLabel(''intro'')',
      'NewNyxBuiltinControl(nkLabel, ''intro'', ncoDescriptor)');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore, 'explicit dynamic built-in factory retains base typing');
    finally
      LCandidate.Free;
    end;
    LDraft := EditNyxManagedFixture(LSource, 'NewNyxLabel(''intro'')',
      'TNyxLabel.Create(''intro'')');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore,
        'specialized implementation construction retains its managed interface contract');
    finally
      LCandidate.Free;
    end;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
  LDocument := CreateNyxStructuralSourceFixture(LSource);
  try
    Check((LDocument.Find('obsolete') = nil) and (LDocument.Find('intro').Parent.ID = 'code-notes'),
      'source omission and reparenting leave no old owned residue');
    Check((LDocument.Find('code-reply').Prop('hint') = 'Keep a thoughtful reply / 🌙') and
      (Pos('LReplyEditor: INyxMemo;', LSource) > 0) and
      (Pos('A page written by hand / 🌙 漢字.', LSource) > 0),
      'subsequent visual edits retain source-created names and notes');
    Check((LDocument.Find('code-action').Prop('gap') = '9') and
      (Pos('NewNyxLabeledButton(''code-action'')', LSource) > 0),
      'visual root configuration retains the source-authored default compound factory');
    LWorkspace := TNyxSourceWorkspace.Create;
    try
      LCandidate := LWorkspace.Candidate(LDocument, LSource);
      try
        Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
          'visually reconciled structural source reconstructs the full candidate');
      finally
        LCandidate.Free;
      end;
    finally
      LWorkspace.Free;
    end;
    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Find('code-notes'));
    try
      LDraft := PrepareNyxCompanion(LDocument, LIsolated, LSource, True);
      Check((Pos('LReplyEditor: INyxMemo;', LDraft) > 0) and
        (Pos('NewNyxColumn(''home'')', LDraft) = 0),
        'isolated source-created page preserves its declarations and excludes other roots');
      LDraft := EditNyxManagedFixture(LSource, 'LHomeColumn: INyxColumn;',
        'LHomeColumn { Home note / 🌙 }, LIntroLabel: INyxControl;');
      LDraft := EditNyxManagedFixture(LDraft, '  LIntroLabel: INyxLabel;' + #10, '');
      LDraft := PrepareNyxCompanion(LDocument, LIsolated, LDraft, True);
      Check((Pos('LIntroLabel: INyxControl;', LDraft) > 0) and
        (Pos('Home note / 🌙', LDraft) > 0) and
        (Pos('LHomeColumn:', LDraft) = 0),
        'isolation prunes a grouped local without losing the retained name or Unicode note');
    finally
      LIsolated.Free;
    end;
    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Find('code-notice'));
    try
      LDraft := PrepareNyxCompanion(LDocument, LIsolated, LSource, True);
      Check((Pos('LNoticeBadge: INyxBadge;', LDraft) > 0) and
        (Pos('NewNyxMemo(''code-reply'')', LDraft) = 0),
        'isolated source-created component preserves only its owned controls');
    finally
      LIsolated.Free;
    end;
  finally
    LDocument.Free;
  end;
  LSession := TNyxStudioSession.Create;
  try
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    LDraft := AddNyxStructuralFixture(LBeforeSource);
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    Check((LSession.Document.Find('code-reply') <> nil) and (LSession.Source = LDraft),
      'shared session publishes the new document and exact companion together');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LBeforeSource),
      'paired undo removes source-created controls and restores exact source');
    LSource := EditNyxManagedFixture(LBeforeSource, 'Result.Free;', 'Result.Free; Exit;');
    LSession.SetSourceDraft(LSource);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
        Check((LException.Line > 1) and (LException.Column > 0),
          'malformed cleanup carries a source diagnostic');
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore) and (LSession.Source = LBeforeSource) and
      (LSession.DraftSource = LSource), 'rejected structure retains accepted pair and editable draft');
    LSession.Redo;
    Check((LSession.Source = LDraft) and (LSession.Document.Find('code-reply') <> nil),
      'rejected structure preserves the previous successful redo');
  finally
    LSession.Free;
  end;
  LDocument := BaseDocument;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Load(TNyxCodec.Encode(LDocument));
    LDraft := EditNyxManagedFixture(LSession.Source, 'LIntroLabel: INyxLabel;',
      'LIntroLabel, LUnusedCaptionLabel: INyxLabel;');
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    LSession.AddControl(NewNyxLabel('unused-caption'));
    Check((Pos('LUnusedCaptionLabel2: INyxLabel;', LSession.Source) > 0) and
      (Pos('LIntroLabel, LUnusedCaptionLabel: INyxLabel;', LSession.Source) > 0),
      'new visual controls reserve grouped unused authored local names');
    Check(LSession.Document.Find('unused-caption') <> nil,
      'a reserved unused name cannot prevent independent visual insertion');
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

end.
