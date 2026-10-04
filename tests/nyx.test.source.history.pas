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
unit nyx.test.source.history;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Exercise immutable checkpoint lifetime, unchanged wire recovery, externally
  mutated document synchronization, paired rollback/redo and bounded session
  history through public APIs. Both target runtimes execute these cases. }
function RunNyxSourceHistoryTests: Integer;

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
  nyx.studio.history,
  nyx.test.source.managed;

function RunNyxSourceHistoryTests: Integer;
var
  LDocument: TNyxDocument;
  LRestored: TNyxDocument;
  LPage: INyxPage;
  LOriginal: TNyxSourceWorkspace;
  LWorkspace: TNyxSourceWorkspace;
  LCheckpoint: TNyxSourceCheckpoint;
  LHistory: TNyxStudioHistory;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LWire: TNyxText;
  LBaseline: TNyxText;
  LCurrent: TNyxText;
  LCurrentSource: TNyxText;
  LRetainedTitle: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Source history: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LRestored := nil;
  LOriginal := TNyxSourceWorkspace.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LHistory := TNyxStudioHistory.Create;
  LSession := TNyxStudioSession.Create;
  try
    LDocument.Title := 'Original / 🌙 / 漢字';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('Original caption'));
    LDocument.AddPage(LPage);
    LPage := nil;
    LSource := EditNyxManagedFixture(TNyxCodegen.Generate(LDocument),
      'LMessageLabel', 'LAuthoredCaption');
    LSource := '{ Handwritten frame / 🌙 }' + #10 + LSource;
    LOriginal.Accept(LDocument, LSource);
    LCheckpoint := LOriginal.Capture(LDocument);
    LWire := LOriginal.Snapshot;
    Check((LCheckpoint.Design = TNyxCodec.Encode(LDocument)) and
      (LCheckpoint.StorageBytes > 0), 'checkpoint pairs the exact accepted document');
    LHistory.Add(LCheckpoint);
    LOriginal.Reset;
    FreeAndNil(LOriginal);
    LDocument.Title := 'Changed independently';
    LWorkspace.Restore(LHistory.Last);
    LRestored := TNyxCodec.Decode(LCheckpoint.Design);
    Check(LWorkspace.Render(LRestored) = LSource,
      'captured source survives original workspace disposal and document mutation');
    Check(LWorkspace.Snapshot = LWire, 'wire recovery remains byte-for-byte compatible');
    LWorkspace.Reset;
    LWorkspace.Restore(LWire);
    Check(LWorkspace.Render(LRestored) = LSource,
      'wire and in-memory restoration have the same accepted frame');
    LRestored.Title := 'Restored edit';
    Check((Pos('Handwritten frame / 🌙', LWorkspace.Render(LRestored)) > 0) and
      (Pos('LAuthoredCaption', LWorkspace.Render(LRestored)) > 0),
      'captured custom frame and crafted locals survive later visual reconciliation');
    LHistory.Clear;
    Check((LHistory.Count = 0) and (LHistory.StorageBytes = 0) and
      (LCheckpoint.Design <> ''), 'clearing history releases entries without invalidating values');

    LSession.Load(LCheckpoint.Design);
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LBaseline := LSession.Save;
    LSession.SetTitle('Session edit');
    { Do not call Source here: history itself must synchronize these public edits. }
    LSession.Document.Title := 'External title / 🌙';
    LSession.Document.Find('message').SetProp('text', 'External caption / 漢字');
    LCurrent := LSession.Save;
    LSession.Undo;
    Check((LSession.Save = LBaseline) and (LSession.Source = LSource),
      'undo restores the original exact accepted pair');
    LSession.Redo;
    LCurrentSource := LSession.Source;
    Check((LSession.Save = LCurrent) and
      (Pos('External caption / 漢字', LCurrentSource) > 0) and
      (Pos('LAuthoredCaption', LCurrentSource) > 0),
      'redo remembers fresh public document mutations and their reconciled source');
    LSession.Undo;
    LSession.Select('message');
    LRejected := False;
    try
      LSession.SetProperty('enabled', 'not-a-boolean');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline) and (LSession.Source = LSource),
      'failed visual admission restores both owners atomically');
    LSession.Redo;
    Check((LSession.Save = LCurrent) and (LSession.Source = LCurrentSource),
      'rollback preserves the existing exact redo pair');
    LSession.SetSourceDraft(EditNyxManagedFixture(LCurrentSource,
      '''External caption / 漢字''', 'False'));
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LCurrent) and
      (LSession.Source = LCurrentSource) and (LSession.DraftSource <> LCurrentSource),
      'source rejection retains its draft and exact in-memory baseline');
    LSession.DiscardSourceDraft;

    LSession.Load(LCheckpoint.Design);
    for LIndex := 1 to 60 do
    begin
      LSession.SetTitle('History ' + IntToStr(LIndex));
    end;
    for LIndex := 1 to 60 do
    begin
      LSession.Undo;
    end;
    LRetainedTitle := LSession.Document.Title;
    Check(LRetainedTitle = 'History 10', 'count retention keeps exactly the latest fifty commands');
    for LIndex := 1 to 60 do
    begin
      LSession.Redo;
    end;
    Check(LSession.Document.Title = 'History 60', 'bounded history retains complete ordered redo pairs');
  finally
    LSession.Free;
    LHistory.Free;
    LWorkspace.Free;
    LOriginal.Free;
    LPage := nil;
    LRestored.Free;
    LDocument.Free;
  end;
end;

end.
