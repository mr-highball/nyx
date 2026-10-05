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
unit nyx.test.source.context;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Same authored names are legal in independent source/design pairs. Exercise
  different source/type/state meaning, rejected drafts, structural edits and
  exact paired histories through the public session on both runtimes. }
function RunNyxSourceContextTests: Integer;

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

function RunNyxSourceContextTests: Integer;
var
  LFirst: TNyxDocument;
  LSecond: TNyxDocument;
  LPage: INyxPage;
  LLabel: INyxLabel;
  LButton: INyxButton;
  LFirstSession: TNyxStudioSession;
  LSecondSession: TNyxStudioSession;
  LFirstSource: TNyxText;
  LSecondSource: TNyxText;
  LBefore: TNyxText;
  LBeforeDesign: TNyxText;
  LOtherBefore: TNyxText;
  LDraft: TNyxText;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Source contexts: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LFirst := TNyxDocument.Create;
  LSecond := TNyxDocument.Create;
  LFirstSession := TNyxStudioSession.Create;
  LSecondSession := TNyxStudioSession.Create;
  try
    LFirst.State.SetValue(NyxTextState('caption'), TNyxText('First state / 🌙'));
    LPage := NewNyxPage('home');
    LLabel := NewNyxLabel('message').WithText('First caption');
    LLabel.Binds.Text(NyxTextState('caption'));
    LPage.Add(LLabel);
    LFirst.AddPage(LPage);
    LPage := nil;
    LLabel := nil;
    LSecond.State.SetValue(NyxBooleanState('caption'), True);
    LPage := NewNyxPage('home');
    LButton := NewNyxButton('message').WithText('Second caption');
    LButton.Binds.Enabled(NyxBooleanState('caption'));
    LPage.Add(LButton);
    LSecond.AddPage(LPage);
    LButton := nil;
    LPage := nil;
    LFirstSource := EditNyxManagedFixture(TNyxCodegen.Generate(LFirst),
      'LMessageLabel', 'LSharedControl');
    LFirstSource := EditNyxManagedFixture(LFirstSource, 'LCaptionTextState', 'LSharedState');
    LFirstSource := EditNyxManagedFixture(LFirstSource, '''First caption''',
      '''First '' + { first wording / 🌙 } ''caption''');
    LSecondSource := EditNyxManagedFixture(TNyxCodegen.Generate(LSecond),
      'LMessageButton', 'LSharedControl');
    LSecondSource := EditNyxManagedFixture(LSecondSource, 'LCaptionBooleanState', 'LSharedState');
    LSecondSource := EditNyxManagedFixture(LSecondSource, '''Second caption''',
      '''Second '' + { second wording / 漢字 } ''caption''');
    LFirstSession.Load(TNyxCodec.Encode(LFirst));
    LSecondSession.Load(TNyxCodec.Encode(LSecond));
    LFirstSession.SetSourceDraft(LFirstSource);
    LSecondSession.SetSourceDraft(LSecondSource);
    LFirstSession.ApplySourceDraft;
    LSecondSession.ApplySourceDraft;
    Check((LFirstSession.Source = LFirstSource) and
      (LSecondSession.Source = LSecondSource), 'each pair accepts its exact authored source');
    Check((LFirstSession.Save = TNyxCodec.Encode(LFirst)) and
      (LSecondSession.Save = TNyxCodec.Encode(LSecond)),
      'the same authored names retain different control and scalar families');
    LFirstSession.Select('message');
    LFirstSession.SetProperty('hint', 'First hint');
    LSecondSession.Select('message');
    LSecondSession.SetProperty('hint', 'Second hint');
    Check((Pos('LSharedControl: INyxLabel', LFirstSession.Source) > 0) and
      (Pos('LSharedState: TNyxTextStateRef', LFirstSession.Source) > 0) and
      (Pos('first wording / 🌙', LFirstSession.Source) > 0),
      'first visual edit retains its own declared types and authored expression');
    Check((Pos('LSharedControl: INyxButton', LSecondSession.Source) > 0) and
      (Pos('LSharedState: TNyxBooleanStateRef', LSecondSession.Source) > 0) and
      (Pos('second wording / 漢字', LSecondSession.Source) > 0),
      'second visual edit retains its independent declared types and expression');
    Check((LFirstSession.Document.State.GetValue(NyxTextState('caption')) =
      TNyxText('First state / 🌙')) and
      LSecondSession.Document.State.GetValue(NyxBooleanState('caption')),
      'lexical comparison does not mutate either typed default');
    LFirstSession.Select('home');
    LFirstSession.AddControl(NewNyxMemo('extra-notes').WithText('Notes'));
    Check((LFirstSession.Document.Find('extra-notes') <> nil) and
      (LSecondSession.Document.Find('extra-notes') = nil) and
      (Pos('first wording / 🌙', LFirstSession.Source) > 0),
      'structural reconciliation preserves its own state facts and unchanged expression');
    LBefore := LFirstSession.Source;
    LBeforeDesign := LFirstSession.Save;
    LOtherBefore := LSecondSession.Source;
    LDraft := EditNyxManagedFixture(LBefore, '.Hint(''First hint'')', '.Hint(False)');
    Check(LDraft <> LBefore, 'the typed rejection changes a real configured expression');
    LFirstSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LFirstSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LFirstSession.Source = LBefore) and
      (LFirstSession.Save = LBeforeDesign) and (LFirstSession.DraftSource = LDraft) and
      (LSecondSession.Source = LOtherBefore),
      'failed admission retains the exact pair/draft without affecting another workspace');
    LFirstSession.DiscardSourceDraft;
    LFirstSession.Select('message');
    LFirstSession.SetProperty('hint', 'A later first hint');
    LFirstSource := LFirstSession.Source;
    LFirstSession.Undo;
    LFirstSession.Redo;
    Check((LFirstSession.Source = LFirstSource) and
      (LSecondSession.Source = LOtherBefore) and
      (LFirstSession.Document.State.GetValue(NyxTextState('caption')) = TNyxText('First state / 🌙')),
      'post-failure edits and paired history retain independent source/state meaning');
  finally
    LSecondSession.Free;
    LFirstSession.Free;
    LButton := nil;
    LLabel := nil;
    LPage := nil;
    LSecond.Free;
    LFirst.Free;
  end;
end;

end.
