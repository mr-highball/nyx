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
{ Qualify paired candidate ownership and derived workspace text across failed
  admission, direct mutations, reset and both recovery paths. These cases use
  only public behavior; they never treat a cache hit as correctness evidence. }
function RunNyxSourceAdmissionTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.controls,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.studio.session,
  nyx.studio.history,
  nyx.studio.projects,
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
  LRecovered: TNyxStudioSession;
  LSource: TNyxText;
  LWire: TNyxText;
  LBaseline: TNyxText;
  LCurrent: TNyxText;
  LCurrentSource: TNyxText;
  LPair: TNyxProjectPair;
  LBeforePacket: TNyxText;
  LDraftPacket: TNyxText;
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
  LRecovered := nil;
  try
    LDocument.Title := 'Original / 🌙 / 漢字';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('Original caption'));
    LDocument.AddPage(LPage);
    LPage := nil;
    LSource := EditNyxManagedFixture(TNyxCodegen.Generate(LDocument),
      'LMessageLabel', 'LAuthoredCaption');
    LSource := TNyxText('{ Handwritten frame / 🌙 }') + #10 + LSource;
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
    Check(not LSession.SourceDraftPending,
      'successful source Apply never restores its internal staging buffer on Undo');
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

    { File restoration is an editor command even when the accepted files stay
      identical. The supplementary characters and independent base must travel
      with the pending buffer rather than being reconstructed from source. }
    LSession.Load(LCheckpoint.Design);
    LPair := LSession.ProjectSnapshot;
    LBeforePacket := EncodeNyxProject(LPair);
    LPair.Pending := True;
    LPair.Draft := 'An unfinished idea / 😀' + #10;
    LPair.DraftBase := 'An independent baseline / 🚀' + #10;
    LDraftPacket := EncodeNyxProject(LPair);
    LSession.AdoptProject(LPair);
    Check(LSession.CanUndo, 'draft-only file admission creates one reversible editor command');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBeforePacket,
      'Undo restores the complete previous editor pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LDraftPacket,
      'Redo restores the exact pending draft and independent baseline');
    LSession.SetSourceDraft('A later unfinished edit / 🌙');
    LDraftPacket := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBeforePacket,
      'Undo retains later typing in its opposite history checkpoint');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LDraftPacket,
      'Redo brings later typing back without reconstructing its baseline');
    LRecovered := TNyxStudioSession.CreateRecovered(LSession.RecoveryFrame);
    LRecovered.Undo;
    Check(EncodeNyxProject(LRecovered.ProjectSnapshot) = LBeforePacket,
      'recovered complete history restores the previous editor state');
    LRecovered.Redo;
    Check(EncodeNyxProject(LRecovered.ProjectSnapshot) = LDraftPacket,
      'recovered history retains exact later typing and stale baseline');
    LSession.Undo;
    LSession.AdoptProject(LSession.ProjectSnapshot);
    Check(LSession.CanRedo, 'unchanged project admission preserves the opposite history');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LDraftPacket,
      'no-op admission cannot replace the saved pending buffer');
    FreeAndNil(LRecovered);
    LRecovered := LSession.Clone;
    LRecovered.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LDraftPacket,
      'cloned history traversal has no shared mutable session state');

    LSession.Load(LCheckpoint.Design);
    LPair := LSession.ProjectSnapshot;
    LBeforePacket := EncodeNyxProject(LPair);
    LPair.Pending := True;
    LPair.Draft := '';
    LPair.DraftBase := LPair.Source;
    LSession.AdoptProject(LPair);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBeforePacket,
      'an empty imported draft is still one reversible editor command');
    LSession.Redo;
    Check(LSession.SourceDraftPending and (LSession.DraftSource = ''),
      'Redo distinguishes an empty unfinished buffer from absence');
    LSession.Load(LCheckpoint.Design);
    LPair := LSession.ProjectSnapshot;
    LPair.Pending := True;
    LPair.Draft := 'Ordinary synchronized typing';
    LPair.DraftBase := LPair.Source;
    LSession.AdoptProject(LPair, spaSynchronization);
    Check(LSession.SourceDraftPending and not LSession.CanUndo,
      'ordinary typing synchronization never creates a file command per keystroke');
    LSession.DiscardSourceDraft;
    LSession.SetTitle('A preceding design edit');
    LSession.Undo;
    LSession.AdoptProject(LPair, spaSynchronization);
    Check(LSession.CanRedo, 'typing synchronization does not erase existing Redo');
    LDraftPacket := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Redo;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LDraftPacket,
      'history traversal returns exact synchronized typing through its opposite entry');
  finally
    LRecovered.Free;
    LSession.Free;
    LHistory.Free;
    LWorkspace.Free;
    LOriginal.Free;
    LPage := nil;
    LRestored.Free;
    LDocument.Free;
  end;
end;

function RunNyxSourceAdmissionTests: Integer;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LSecond: TNyxDocument;
  LPage: INyxPage;
  LWorkspace: TNyxSourceWorkspace;
  LPrepared: TNyxSourceWorkspace;
  LRestored: TNyxSourceWorkspace;
  LCheckpoint: TNyxSourceCheckpoint;
  LSource: TNyxText;
  LDraft: TNyxText;
  LWire: TNyxText;
  LDesign: TNyxText;
  LChanged: TNyxText;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Paired source admission: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LSecond := TNyxDocument.Create;
  LCandidate := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  LPrepared := nil;
  LRestored := TNyxSourceWorkspace.Create;
  try
    LDocument.Title := 'Paired admission';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('Original caption'));
    LDocument.AddPage(LPage);
    LPage := nil;
    LSource := EditNyxManagedFixture(TNyxCodegen.Generate(LDocument),
      'LMessageLabel', 'LAuthoredCaption');
    LSource := EditNyxManagedFixture(LSource, '''Original caption''',
      '''Original '' + { Retain / 🌙 / 漢字 } ''caption''');
    LWorkspace.Accept(LDocument, LSource);
    LWire := LWorkspace.Snapshot;
    LDesign := TNyxCodec.Encode(LDocument);
    LDraft := EditNyxManagedFixture(LSource, '''Original ''', '''Changed ''');
    LCandidate := LWorkspace.PrepareCandidate(LDocument, LDraft, LPrepared);
    Check((LCandidate <> LDocument) and (LPrepared <> LWorkspace),
      'preparation returns independently owned document and companion');
    Check((LPrepared.Render(LCandidate) = LDraft) and
      (LCandidate.Find('message').Prop('text') = 'Changed caption'),
      'one exact draft reconstructs the complete prepared pair');
    Check((LWorkspace.Snapshot = LWire) and (TNyxCodec.Encode(LDocument) = LDesign),
      'successful preparation leaves the accepted pair unchanged');
    LCandidate.Title := 'Prepared later';
    LChanged := LPrepared.Render(LCandidate);
    Check((Pos('Prepared later', LChanged) > 0) and
      (Pos('LAuthoredCaption', LChanged) > 0) and
      (Pos('Retain / 🌙 / 漢字', LChanged) > 0),
      'later prepared edits preserve names and exact Unicode expression comments');
    Check((LWorkspace.Snapshot = LWire) and (TNyxCodec.Encode(LDocument) = LDesign),
      'editing the prepared pair cannot mutate its former accepted owners');
    FreeAndNil(LCandidate);
    FreeAndNil(LPrepared);

    LDraft := EditNyxManagedFixture(LSource,
      '''Original '' + { Retain / 🌙 / 漢字 } ''caption''', 'False');
    LRejected := False;
    try
      LCandidate := LWorkspace.PrepareCandidate(LDocument, LDraft, LPrepared);
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LCandidate = nil) and (LPrepared = nil),
      'wrong-typed admission publishes neither owner');
    Check((LWorkspace.Snapshot = LWire) and (TNyxCodec.Encode(LDocument) = LDesign),
      'wrong-typed admission retains exact accepted source and design');
    LRejected := False;
    try
      LCandidate := LWorkspace.PrepareCandidate(LDocument,
        NyxViewsBegin + #10 + LSource, LPrepared);
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LPrepared = nil) and (LWorkspace.Snapshot = LWire),
      'whole-source boundary rejection cannot publish a partial frame');

    LDocument.Find('message').SetProp('enabled', 'invalid');
    LRejected := False;
    try
      LWorkspace.Render(LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LWorkspace.Snapshot = LWire),
      'invalid direct model mutation cannot update accepted or derived text');
    LDocument.Find('message').SetProp('enabled', 'true');
    LChanged := LWorkspace.Render(LDocument);
    Check((Pos('Enabled(True)', LChanged) > 0) and
      (Pos('Retain / 🌙 / 漢字', LChanged) > 0),
      'repaired direct mutation still receives fresh full visual admission');
    LDocument.Title := 'Changed directly';
    LChanged := LWorkspace.Render(LDocument);
    Check((Pos('Changed directly', LChanged) > 0) and
      (Pos('LAuthoredCaption', LChanged) > 0),
      'a warmed workspace observes subsequent public document mutation');
    LCheckpoint := LWorkspace.Capture(LDocument);
    LWire := LWorkspace.Snapshot;
    LRestored.Restore(LCheckpoint);
    Check((LRestored.Render(LDocument) = LChanged) and (LRestored.Snapshot = LWire),
      'typed recovery preserves exact wire and source without derived history text');
    LDocument.Find('message').SetProp('hint', 'After typed restore');
    LChanged := LRestored.Render(LDocument);
    Check((Pos('After typed restore', LChanged) > 0) and
      (Pos('Retain / 🌙 / 漢字', LChanged) > 0),
      'first edit after typed restore reconstructs the exact prior design baseline');
    LRestored.Restore(LWire);
    LChanged := LRestored.Render(LDocument);
    Check((Pos('After typed restore', LChanged) > 0) and
      (Pos('LAuthoredCaption', LChanged) > 0),
      'wire recovery independently reconstructs the prior baseline for direct edits');

    LSecond.Title := 'Independent second design';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxButton('message').WithText('Second action'));
    LSecond.AddPage(LPage);
    LPage := nil;
    LDraft := EditNyxManagedFixture(TNyxCodegen.Generate(LSecond),
      'LMessageButton', 'LAuthoredCaption');
    LDraft := EditNyxManagedFixture(LDraft, 'INyxButton', 'INYXBUTTON');
    LDraft := EditNyxManagedFixture(LDraft, 'NewNyxButton', 'newnyxbutton');
    LCandidate := LWorkspace.PrepareCandidate(LSecond, LDraft, LPrepared);
    Check((LPrepared.Render(LCandidate) = LDraft) and
      (LCandidate.Find('message').Kind = 'button'),
      'case-insensitive specialized types and factories retain exact authored spelling');
    LCandidate.Find('message').SetProp('hint', 'Independent button hint');
    LChanged := LPrepared.Render(LCandidate);
    Check((Pos('INYXBUTTON', LChanged) > 0) and
      (Pos('Independent button hint', LChanged) > 0),
      'same authored local can describe a different type in an independent prepared pair');
    FreeAndNil(LCandidate);
    FreeAndNil(LPrepared);
    LWorkspace.Reset;
    LChanged := LWorkspace.Render(LSecond);
    Check((Pos('LMessageButton: INyxButton', LChanged) > 0) and
      (Pos('Retain /', LChanged) = 0),
      'Reset cannot retain former authored names or derived baseline text');
    LDraft := EditNyxManagedFixture(LChanged, 'INyxButton', 'INyxLabel');
    LRejected := False;
    try
      LCandidate := LWorkspace.PrepareCandidate(LSecond, LDraft, LPrepared);
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LPrepared = nil) and (LWorkspace.Render(LSecond) = LChanged),
      'specialized factory assignment remains strongly typed after reset');
  finally
    LPage := nil;
    LPrepared.Free;
    LCandidate.Free;
    LRestored.Free;
    LWorkspace.Free;
    LSecond.Free;
    LDocument.Free;
  end;
end;

end.
