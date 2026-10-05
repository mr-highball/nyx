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
unit nyx.test.source.indexed;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Exercise independent control/state identities, colliding Pascal locals,
  retained expressions and failure/history through the public Studio session.
  This is behavior coverage; it does not depend on a particular index algorithm. }
function RunNyxIndexedSourceTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.types,
  nyx.state,
  nyx.controls,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.studio.session,
  nyx.test.source.managed;

function RunNyxIndexedSourceTests: Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LBeforeDesign: TNyxText;
  LDraft: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Indexed source: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LSession := TNyxStudioSession.Create;
  try
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('Aa').WithText('First caption'));
    LPage.Add(NewNyxLabel('BB').WithText('Second caption'));
    LPage.Add(NewNyxLabel('Case').WithText('Upper identity'));
    LPage.Add(NewNyxLabel('case').WithText('Lower identity'));
    LPage.Add(NewNyxMemo('notes:configure/🌙').WithText('Notes'));
    for LIndex := 1 to 96 do
    begin
      LPage.Add(NewNyxLabel('caption-' + IntToStr(LIndex)).WithText('Caption'));
    end;
    LDocument.AddPage(LPage);
    LDocument.State.SetValue(NyxTextState('Aa'), TNyxText('First state'));
    LDocument.State.SetValue(NyxTextState('BB'), TNyxText('Second state'));
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := EditNyxManagedFixture(LSource, 'LAaLabel', 'LAn');
    LSource := EditNyxManagedFixture(LSource, 'LBBLabel', 'LC0');
    LSource := EditNyxManagedFixture(LSource, 'LAaTextState', 'LAnState');
    LSource := EditNyxManagedFixture(LSource, 'LBBTextState', 'LC0State');
    LSource := EditNyxManagedFixture(LSource, '''Second caption''',
      '''Second '' + { independent / 🌙 } ''caption''');
    Check((Pos('LAn: INyxLabel', LSource) > 0) and
      (Pos('LC0: INyxLabel', LSource) > 0), 'fixture uses independent purposeful locals');
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Check((LSession.Source = LSource) and (LSession.Save = TNyxCodec.Encode(LDocument)),
      'crafted locals and both state identities admit the exact design/source pair');
    LSession.Select('Aa');
    LSession.SetProperty('text', 'Edited first caption');
    Check((LSession.Document.Find('Aa').Prop('text') = 'Edited first caption') and
      (LSession.Document.Find('BB').Prop('text') = 'Second caption'),
      'a visual edit updates only its exact control identity');
    Check((Pos('LAn: INyxLabel', LSession.Source) > 0) and
      (Pos('LC0: INyxLabel', LSession.Source) > 0) and
      (Pos('independent / 🌙', LSession.Source) > 0) and
      (Pos('''Second '' +', LSession.Source) > 0),
      'unrelated authored locals, comments and expressions remain exact');
    LSession.Select('case');
    LSession.SetProperty('text', 'Edited lower identity');
    Check((LSession.Document.Find('Case').Prop('text') = 'Upper identity') and
      (LSession.Document.Find('case').Prop('text') = 'Edited lower identity'),
      'application identities remain case sensitive');
    LSession.Select('notes:configure/🌙');
    LSession.SetProperty('placeholder', 'Write a note / 漢字');
    Check(LSession.Document.Find('notes:configure/🌙').Prop('placeholder') =
      TNyxText('Write a note / 漢字'), 'Unicode and section-like identities stay independent');
    LSession.RenameState('Aa', 'First/state/🌙');
    Check((LSession.Document.State.GetValue(NyxTextState('First/state/🌙')) = 'First state') and
      (LSession.Document.State.GetValue(NyxTextState('BB')) = 'Second state') and
      (Pos('LAnState: TNyxTextStateRef', LSession.Source) > 0) and
      (Pos('LC0State: TNyxTextStateRef', LSession.Source) > 0),
      'exact state migration retains both independent authored names');
    LSession.Select('Aa');
    LSession.DeleteSelected;
    Check((LSession.Document.Find('Aa') = nil) and
      (LSession.Document.Find('BB').Prop('text') = 'Second caption') and
      (Pos('LC0: INyxLabel', LSession.Source) > 0),
      'removing one control retains its independent sibling and declaration');
    LBefore := LSession.Source;
    LBeforeDesign := LSession.Save;
    LSession.Undo;
    Check(LSession.Document.Find('Aa') <> nil, 'undo restores the removed control');
    LSession.Redo;
    Check((LSession.Source = LBefore) and (LSession.Save = LBeforeDesign),
      'redo restores the byte-exact crafted pair');
    LDraft := EditNyxManagedFixture(LBefore, 'LC0: INyxLabel', 'LC0State: INyxLabel');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Source = LBefore) and
      (LSession.Save = LBeforeDesign) and (LSession.DraftSource = LDraft),
      'a cross-family local collision retains the accepted pair and rejected draft');
  finally
    LSession.Free;
    LPage := nil;
    LDocument.Free;
  end;
end;

end.
