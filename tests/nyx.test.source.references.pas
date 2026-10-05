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

unit nyx.test.source.references;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Qualify exact identities through fresh public document validation and source
  replay. Direct renaming, collisions and implicit recipe parts must never use
  stale admission facts or publish half of the accepted Studio pair. Broader
  Unicode here is qualification input, not starter/demo presentation. }
function RunNyxSourceReferenceTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.controls,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.studio.session,
  nyx.test.source.managed;

function RunNyxSourceReferenceTests: Integer;
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LCandidate: TNyxDocument;
  LPage: INyxColumn;
  LDefinition: INyxColumn;
  LFirst: INyxLabel;
  LSecond: INyxLabel;
  LLower: INyxLabel;
  LSearch: INyxSearchField;
  LBefore: TNyxText;
  LSource: TNyxText;
  LDraft: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Source references: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure RejectDuplicate(const AID: TNyxText);
  begin
    LRejected := False;
    try
      LDocument.Validate;
    except
      on LException: ENyxModel do
      begin
        LRejected := LException.Message = 'Duplicate node ID: ' + AID;
      end;
    end;
    Check(LRejected, 'a fresh traversal reports the exact duplicate identity');
  end;

  procedure RejectDraft(const ABefore, AAfter: TNyxText);
  begin
    LDraft := EditNyxManagedFixture(LSource, ABefore, AAfter);
    Check(LDraft <> LSource, 'the rejection fixture changes its intended statement');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
      on LException: ENyxModel do
      begin
        { Complete model admission reports recipe identity conflicts through
          its public model exception, rather than a lexer position diagnostic. }
        LRejected := Pos('Duplicate node ID:', LException.Message) = 1;
      end;
    end;
    Check(LRejected and (LSession.Source = LSource) and
      (LSession.Save = LBefore) and (LSession.DraftSource = LDraft) and
      not LSession.CanUndo and LSession.CanRedo,
      'failed replay retains both accepted owners, rejected buffer and Redo');
    LSession.DiscardSourceDraft;
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LSession := TNyxStudioSession.Create;
  try
    LPage := NewNyxColumn('home');
    LDocument.AddPage(LPage);
    LDefinition := NewNyxColumn('reusable');
    LDocument.AddComponent(LDefinition);
    { Aa and BB exercise colliding hash values through the public contract.
      Case distinctions and supplementary characters retain exact identity. }
    LFirst := NewNyxLabel('Aa').WithText('First caption');
    LSecond := NewNyxLabel('BB').WithText('Second caption');
    LLower := NewNyxLabel('case').WithText('Lowercase caption');
    LPage.Add(LFirst).Add(LSecond).Add(NewNyxLabel('Case')).Add(LLower);
    LDefinition.Add(NewNyxLabel(TNyxText('名字🌙')));
    LDefinition.Add(NewNyxLabel(TNyxText('🌙名字')));
    for LIndex := 1 to 64 do
    begin
      LDefinition.Add(NewNyxLabel('item-' + TNyxText(IntToStr(LIndex))));
    end;
    LSearch := NewNyxSearchField('search');
    LPage.Add(LSearch);
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    Check((LDocument.Find('Aa') = LFirst.Node) and
      (LDocument.Find('BB') = LSecond.Node), 'colliding keys retain separate controls');
    Check(LDocument.Find('Case') <> LDocument.Find('case'),
      'application identities remain case-sensitive');
    Check(LDocument.Find(TNyxText('名字🌙')) <> LDocument.Find(TNyxText('🌙名字')),
      'exact Unicode identities survive growth and lookup');

    LSecond.Named('Aa');
    RejectDuplicate('Aa');
    LSecond.Named('BB');
    LDocument.Validate;
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'repairing a direct mutation restores exact document admission');
    LLower.Named(TNyxText('名字🌙'));
    RejectDuplicate(TNyxText('名字🌙'));
    LLower.Named('case');
    LDefinition.Named('home');
    RejectDuplicate('home');
    LDefinition.Named('reusable');
    LDocument.Validate;
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'cross-root duplicate repairs are read freshly without retained facts');

    LSource := TNyxCodegen.Generate(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore,
        'fresh replay preserves exact case, Unicode, recipe parts and root order');
      LCandidate.Find('Aa').Configure.Text('Candidate caption').Done;
      Check((LDocument.Find('Aa').Prop('text') = 'First caption') and
        (LCandidate.Find('BB').Prop('text') = 'Second caption'),
        'retained local references belong only to the independently owned candidate');
    finally
      LCandidate.Free;
    end;

    LSession.Load(LBefore);
    LSession.Select('Aa');
    LSession.SetProperty('text', 'An ordinary visual edit');
    LSession.Undo;
    LSource := LSession.Source;
    { Descriptor construction intentionally avoids rebuilding implicit parts.
      Introducing a second set must fail complete admission atomically. }
    RejectDraft('NewNyxSearchField(''search'', ncoDescriptor)',
      'NewNyxSearchField(''search'', ncoDefault)');
    RejectDraft('LHomeColumn.Add(LAaLabel);',
      'LAaLabel.Configure.Text(''Before ownership'').Done;' + #10 +
      '    LHomeColumn.Add(LAaLabel);');
    LSession.Redo;
    Check(LSession.Document.Find('Aa').Prop('text') = 'An ordinary visual edit',
      'retained Redo still publishes the original exact visual operation');
  finally
    { Release borrowed interfaces before the owning trees and sessions. }
    LSearch := nil;
    LLower := nil;
    LSecond := nil;
    LFirst := nil;
    LDefinition := nil;
    LPage := nil;
    LSession.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

end.
