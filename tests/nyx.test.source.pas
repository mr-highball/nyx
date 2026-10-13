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

unit nyx.test.source;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

function RunNyxSourceTests: Integer;
{ Native core emission and both compilers execute this accepted edited source,
  including a preserved handwritten helper with supplementary Unicode. }
function CreateNyxEditedFixture(out ASource: TNyxText): TNyxDocument;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.data,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.controls,
  nyx.composition,
  nyx.platform,
  nyx.responsive,
  nyx.presentations,
  nyx.test.core,
  nyx.studio.builds,
  nyx.studio.session;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL source: ' + AMessage);
  end;
  Inc(ACount);
end;

function ReplaceFirst(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  { Keep source as TNyxText throughout. The native RTL's untyped ANSI result can
    lose its UTF-8 codepage when several replacements are chained together. }
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    Exit(ASource);
  end;
  Result := Copy(ASource, 1, LPosition - 1) + AAfter +
    Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

function AddHelper(const ASource: TNyxText): TNyxText;
begin
  { Add real Pascal outside the synchronized builder. The explicit interface
    declaration makes compiler reconstruction exercise it, not just retain a
    comment/string that happens to resemble application code. }
  Result := ReplaceFirst(ASource, 'implementation' + #10,
    'function AppCaption: TNyxText;' + #10 + #10 + 'implementation' + #10);
  Result := ReplaceFirst(Result, NyxViewsEnd + #10, NyxViewsEnd + #10 + #10 +
    '// Application wording is handwritten and retained by the designer.' + #10 +
    'function AppCaption: TNyxText;' + #10 +
    'begin' + #10 +
    '  Result := ''A crafted application / 🌙 漢字'';' + #10 +
    'end;' + #10);
end;

function CreateNyxEditedFixture(out ASource: TNyxText): TNyxDocument;
var
  LBase: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  LBase := CreateNyxPersistenceFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    ASource := ReplaceFirst(LWorkspace.Render(LBase), '.Text(''WELCOME'')',
      '.Text(''CRAFTED / 🌙'')');
    ASource := AddHelper(ASource);
    ASource := ReplaceFirst(ASource, '.SetValue(LEmptyTextState, '''')',
      '.SetValue(LEmptyTextState, ''Authored reply / 🌙'')');
    ASource := ReplaceFirst(ASource, '.SetValue(LZeroNumberState, 0.0)',
      '.SetValue(LZeroNumberState, 0.0)' + #10 +
      '      .SetValue(NyxTextState(''code-caption''), ''Crafted status / 🌙'')');
    ASource := ReplaceFirst(ASource, 'LEyebrowBadge.Configure' + #10 +
      '      .Text(''CRAFTED / 🌙'')' + #10 + '      .Done;',
      'LEyebrowBadge.Configure' + #10 + '      .Text(''CRAFTED / 🌙'')' + #10 +
      '      .Done;' + #10 + '    LEyebrowBadge.Binds' + #10 +
      '      .Text(NyxTextState(''code-caption''))' + #10 + '      .Done;');
    ASource := ReplaceFirst(ASource, '.Range(0, 1).Choices([0.125, 0.5])',
      '.Range(0, 2).Choices([0.125, 0.5, 1])');
    ASource := ReplaceFirst(ASource, 'NyxDecimal(''1.234567890123456789'')',
      'NyxDecimal(''9.876543210987654321'')');
    Result := LWorkspace.Candidate(LBase, ASource);
    LWorkspace.Accept(Result, ASource);
    ASource := LWorkspace.Render(Result);
    { The source is compiled under a distinct admitted unit name. }
    ASource := ReplaceFirst(ASource, 'unit nyx.generated.view;',
      'unit nyx.edited.view;');
  finally
    LWorkspace.Free;
    LBase.Free;
  end;
end;

function RunNyxSourceTests: Integer;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LDraft: TNyxText;
  LBaseline: TNyxText;
  LAccepted: TNyxText;
  LRejected: Boolean;
  LSnapshot: TNyxText;
  LRequestDocument: TNyxDocument;
  LRequestSource: TNyxText;

  procedure ScopedText;
  var
    LScopeDocument: TNyxDocument;
    LScopeWorkspace: TNyxSourceWorkspace;
    LScopeCandidate: TNyxDocument;
    LExpected: TNyxDocument;
    LRuntime: TNyxNode;
    LCaption: INyxLabel;
    LFrame: TNyxText;
    LEdited: TNyxText;
    LBefore: TNyxText;
    LRefused: Boolean;
  begin
    { Independent fluent expectations qualify scope and literal/expression
      equivalence. Neither candidate nor runtime realization borrows the source
      document; malformed argument frames must preserve both accepted owners. }
    LScopeDocument := TNyxDocument.Create;
    LScopeWorkspace := TNyxSourceWorkspace.Create;
    LScopeCandidate := nil;
    LExpected := nil;
    LRuntime := nil;
    try
      LScopeDocument.Presentations.Define(NyxPresentation('focused'), TNyxPresentationCondition.Manual);
      LScopeDocument.AddPage(NewNyxPage('home'));
      LCaption := NewNyxLabel('scope-caption').WithText('Base');
      LScopeDocument.Pages[0].Add(LCaption);
      LCaption.Configure.ForPlatform(npfBrowser).Text('Browser / 🌙').Done;
      LCaption.Configure.ForPlatform(npfNativeLCL).Text('Native / 🌙').Done;
      LCaption.Configure.WhenViewport(TNyxViewportCondition.Any.WidthBelow(640)).Text('Compact').Done;
      LCaption.Configure.WhenPresentation(NyxPresentation('focused')).Text('Reader''s / 🌙').Done;
      LFrame := LScopeWorkspace.Render(LScopeDocument);
      LBefore := TNyxCodec.Encode(LScopeDocument);
      LScopeCandidate := LScopeWorkspace.Candidate(LScopeDocument, LFrame);
      Check(TNyxCodec.Encode(LScopeCandidate) = LBefore, 'literal Text retains every typed scope', Result);
      LRuntime := RealizeNyxView(LScopeCandidate, LScopeCandidate.Pages[0]);
      ApplyNyxPlatform(LRuntime, npfBrowser);
      LRuntime.ApplyViewport(800, 700, npfBrowser);
      Check(LRuntime.Find('scope-caption').Prop('text') = TNyxText('Browser / 🌙'),
        'literal Text uses browser scope', Result);
      { Concrete platform baselines belong to independently mounted views;
        viewport/manual transitions remain local to each such realization. }
      LRuntime.Free;
      LRuntime := nil;
      LRuntime := RealizeNyxView(LScopeCandidate, LScopeCandidate.Pages[0]);
      ApplyNyxPlatform(LRuntime, npfNativeLCL);
      LRuntime.ApplyViewport(800, 700, npfNativeLCL);
      Check(LRuntime.Find('scope-caption').Prop('text') = TNyxText('Native / 🌙'),
        'literal Text uses native scope', Result);
      LRuntime.Free;
      LRuntime := nil;
      LRuntime := RealizeNyxView(LScopeCandidate, LScopeCandidate.Pages[0]);
      ApplyNyxPlatform(LRuntime, npfBrowser);
      LRuntime.ApplyViewport(390, 700, npfBrowser);
      Check(LRuntime.Find('scope-caption').Prop('text') = 'Compact', 'literal Text uses width scope', Result);
      LRuntime.ApplyViewport(800, 700, npfBrowser,
        TNyxPresentationSelection.Use(NyxPresentation('focused')));
      Check(LRuntime.Find('scope-caption').Prop('text') = TNyxText('Reader''s / 🌙'),
        'literal Text uses explicit presentation and exact quoted Unicode', Result);
      LRuntime.Free;
      LRuntime := nil;
      FreeAndNil(LScopeCandidate);
      LEdited := ReplaceFirst(LFrame, TNyxText('.Text(''Browser / 🌙'')'),
        TNyxText('.Text(''Browser / '' + TNyxText(''🌙''))'));
      LScopeCandidate := LScopeWorkspace.Candidate(LScopeDocument, LEdited);
      Check(TNyxCodec.Encode(LScopeCandidate) = LBefore,
        'scoped typed expression keeps the same literal meaning', Result);
      FreeAndNil(LScopeCandidate);
      LEdited := ReplaceFirst(LFrame, TNyxText('.Text(''Browser / 🌙'')'), '.Text('''')');
      LExpected := LScopeDocument.Clone;
      LExpected.Find('scope-caption').Configure.ForPlatform(npfBrowser).Text('').Done;
      LScopeCandidate := LScopeWorkspace.Candidate(LScopeDocument, LEdited);
      Check(TNyxCodec.Encode(LScopeCandidate) = TNyxCodec.Encode(LExpected),
        'empty literal retains scoped explicit text', Result);
      FreeAndNil(LScopeCandidate);
      LEdited := ReplaceFirst(LFrame, TNyxText('.Text(''Browser / 🌙'')'),
        TNyxText('.Text(''Browser / 🌙'', ''extra'')'));
      LRefused := False;
      try
        LScopeCandidate := LScopeWorkspace.Candidate(LScopeDocument, LEdited);
      except
        on ENyxSource do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (TNyxCodec.Encode(LScopeDocument) = LBefore) and
        (LScopeWorkspace.Render(LScopeDocument) = LFrame),
        'extra literal argument refuses atomically without changing either owner', Result);
    finally
      LRuntime.Free;
      LScopeCandidate.Free;
      LExpected.Free;
      LCaption := nil;
      LScopeWorkspace.Free;
      LScopeDocument.Free;
    end;
  end;

  procedure Reject(const ABefore, AAfter: TNyxText; const AReason: TNyxText);
  begin
    LDraft := ReplaceFirst(LSource, ABefore, AAfter);
    Check(LDraft <> LSource, 'rejection fixture changes source: ' + AReason, Result);
    LRejected := False;
    LCandidate := nil;
    try
      try
        LCandidate := LWorkspace.Candidate(LDocument, LDraft);
      except
        on LException: Exception do
        begin
          LRejected := True;
          Check(LException.Message <> '', 'useful rejection: ' + AReason, Result);
        end;
      end;
    finally
      LCandidate.Free;
      LCandidate := nil;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBaseline) and
      (LWorkspace.Render(LDocument) = LSource), 'atomic rejection: ' + AReason, Result);
  end;

begin
  Result := 0;
  ScopedText;
  LDocument := CreateNyxPersistenceFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LBaseline := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBaseline,
        'complete generated fixture reads without changing meaning', Result);
    finally
      LCandidate.Free;
    end;
    LDraft := ReplaceFirst(LSource, '.Text(''WELCOME'')',
      '.Text(''Crafted '' + TNyxText(''🌙 / 漢字''''s''))' +
      '.Extension(''source.note'', TNyxText(''🌙'') + TNyxText(#10) + NyxScalarText(0))');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(LCandidate.Find('eyebrow').Prop('text') =
        TNyxText('Crafted 🌙 / 漢字''s'),
        'typed text concatenation preserves Unicode and quotes', Result);
      Check(LCandidate.Find('eyebrow').Prop('source.note') =
        TNyxText('🌙') + NyxScalarText(10) + NyxScalarText(0),
        'explicit extension data retains exact LF and NUL', Result);
      Check(LDocument.Find('eyebrow').Prop('text') = 'WELCOME',
        'accepted tree remains independent', Result);
    finally
      LCandidate.Free;
    end;
    Reject('.Gap(20)', '.Gap(''20'')', 'numeric text cannot select Integer');
    Reject('.Gap(20)', '.Gap(20.0)', 'Double cannot select Integer');
    Reject('.Padding(32)', '.Padding(32).Layout(''column'')', 'layout requires an enum');
    Reject('.Compound(True)', '.Compound(''true'')', 'Boolean text refused');
    Reject('.OnClick(NyxSemantic(nseAdd))', '.OnClick(NyxPart(''add''))', 'reference families');
    Reject('.Padding(32)', '.Padding(2147483648)', 'integer overflow');
    Reject('.Padding(32)', '.Padding(-1)', 'schema admission');
    Reject('Result.Title :=', 'Result.Caption :=', 'unknown document assignment');
    LDraft := ReplaceFirst(LSource, 'NewNyxPage(''home'')', 'NewNyxPage(''other'')');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check((LCandidate.Find('home') = nil) and (LCandidate.Find('other') <> nil) and
        (TNyxCodec.Encode(LDocument) = LBaseline),
        'typed identity edits reconstruct an independent tree', Result);
    finally
      LCandidate.Free;
    end;
    Reject('.Text(''WELCOME'')', '.Unknown(''WELCOME'')', 'unknown method');
    Reject('.Text(''WELCOME'')', '.Text(''WELCOME)', 'unfinished string');
    Reject('.Text(''WELCOME'')', '.Text(''WELCOME'') {$ifdef never}', 'directive');
    Reject(NyxViewsBegin, '// views', 'missing boundary');
    Reject('.Text(''WELCOME'')', '.Text(''WELCOME'')' + #10 + NyxViewsBegin, 'duplicate boundary');
    Reject('.Gap(20)', '.Gap(1 + 2)', 'unsupported arithmetic');
    LDraft := ReplaceFirst(LSource, '.Text(''WELCOME'')',
      '.Text(''WELCOME'').Variant(nvDefault).Action(naNone)');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check((LCandidate.Find('eyebrow').Prop('variant') = '') and
        (LCandidate.Find('eyebrow').Prop('action') = ''),
        'default enum symbols retain their empty wire meanings', Result);
    finally
      LCandidate.Free;
    end;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;

  LSession := TNyxStudioSession.Create;
  try
    LSession.Load(LBaseline);
    LSource := LSession.Source;
    LDraft := AddHelper(ReplaceFirst(LSource, '.Text(''WELCOME'')',
      '.Text(''Crafted / 🌙'')'));
    LSession.SetSourceDraft(LDraft);
    Check((LSession.Source = LSource) and (LSession.DraftSource = LDraft),
      'unapplied buffer is separate from accepted source', Result);
    try
      LSession.ApplySourceDraft;
    except
      on LException: Exception do
      begin
        raise Exception.Create('First companion apply: ' + LException.Message);
      end;
    end;
    LAccepted := LSession.Source;
    Check((LSession.Document.Find('eyebrow').Prop('text') = TNyxText('Crafted / 🌙')) and
      (LAccepted = LDraft), 'atomic code-to-design edit retains crafted spelling', Result);
    LSession.Undo;
    Check((LSession.Save = LBaseline) and (LSession.Source = LSource),
      'paired undo restores original design and source', Result);
    LSession.Redo;
    Check((LSession.Document.Find('eyebrow').Prop('text') = TNyxText('Crafted / 🌙')) and
      (LSession.Source = LAccepted), 'paired redo restores source and design', Result);
    LSession.Select('eyebrow');
    LSession.SetProperty('text', 'Visual / 🌙');
    Check((Pos('.Text(''Visual / 🌙'')', LSession.Source) > 0) and
      (Pos('function AppCaption: TNyxText;', LSession.Source) > 0) and
      (Pos('A crafted application / 🌙 漢字', LSession.Source) > 0),
      'visual regeneration preserves real application helper', Result);
    LSnapshot := LSession.Save;
    LSource := LSession.Source;
    LSession.Undo;
    LAccepted := LSession.Source;
    LDraft := ReplaceFirst(LAccepted, '.Text(''Crafted / 🌙'')', '.Text(');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
        Check((LException.Line > 1) and (LException.Column > 0),
          'syntax error carries source position', Result);
      end;
    end;
    Check(LRejected and (LSession.Source = LAccepted) and
      (LSession.DraftSource = LDraft) and LSession.CanRedo,
      'rejected edit retains accepted Pascal, pending draft and existing redo', Result);
    { Redo restores the complete historical editor checkpoint. Its clean buffer
      replaces the currently rejected draft, which is captured in the opposite
      Undo entry with its exact original base. Rejection must not erase either
      the earlier redo command or the unfinished work's round trip. }
    LSession.Redo;
    Check((LSession.Save = LSnapshot) and (LSession.Source = LSource) and
      not LSession.SourceDraftPending and (LSession.DraftSource = LSource),
      'rejection retains the complete historical redo checkpoint', Result);
    LSession.Undo;
    Check((LSession.Source = LAccepted) and LSession.SourceDraftPending and
      (LSession.DraftSource = LDraft) and (LSession.SourceDraftBase = LAccepted),
      'undo returns the exact rejected draft and independent accepted base', Result);
    LSession.Redo;
    Check((LSession.Save = LSnapshot) and (LSession.Source = LSource) and
      not LSession.SourceDraftPending,
      'repeated traversal restores the same clean redo checkpoint', Result);
    LSession.DiscardSourceDraft;
    Check(LSession.DraftSource = LSource, 'restore accepted is explicit', Result);
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Check(LSession.Source = LSource, 'no-op source apply is stable', Result);
    { Boundary text inside a caption is ordinary text, not a delimiter. }
    LSession.SetProperty('text', NyxViewsBegin);
    LSource := LSession.Source;
    LSession.SetSourceDraft(ReplaceFirst(LSource, '.Text(''// <nyx:views>'')',
      '.Text(''A regular caption'')'));
    LSession.ApplySourceDraft;
    Check(LSession.Selected.Prop('text') = 'A regular caption',
      'delimiter discovery distinguishes literals from comments', Result);
    LSource := LSession.Source;
    LDraft := ReplaceFirst(LSource, '.Text(''A regular caption'')',
      '.Text(''Code draft'')');
    LSession.SetSourceDraft(LDraft);
    LSession.SetProperty('text', 'New visual caption');
    LSnapshot := LSession.Save;
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := Pos('design changed', LException.Message) > 0;
      end;
    end;
    Check(LRejected and (LSession.Save = LSnapshot) and
      (LSession.DraftSource = LDraft), 'stale draft cannot overwrite later visual edits', Result);
    LSource := LSession.SourceDraftBase;
    LSession.DiscardSourceDraft;
    LSession.RestoreSourceDraft(LDraft, LSource);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.SourceDraftBase = LSource) and
      (LSession.Save = LSnapshot), 'draft recovery retains stale-edit protection', Result);
  finally
    LSession.Free;
  end;
  LDocument := CreateNyxEditedFixture(LSource);
  try
    Check(LDocument.Find('eyebrow').Prop('text') = TNyxText('CRAFTED / 🌙'),
      'compiled edited fixture has concrete changed meaning', Result);
    Check(Pos('unit nyx.edited.view;', LSource) > 0,
      'companion fixture has independent admitted unit identity', Result);
    LRequestDocument := nil;
    DecodeNyxBuildRequest(EncodeNyxBuildRequest(LDocument, LSource),
      LRequestDocument, LRequestSource);
    try
      Check((LRequestSource = LSource) and
        (TNyxCodec.Encode(LRequestDocument) = TNyxCodec.Encode(LDocument)),
        'compiler envelope retains exact accepted design and Unicode source', Result);
      Check(PrepareNyxCompanion(LDocument, LDocument, LSource, False) = LSource,
        'application compilation retains exact handwritten companion', Result);
    finally
      LRequestDocument.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

end.
