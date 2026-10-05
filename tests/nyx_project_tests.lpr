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


program nyx_project_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  Web,
  {$ELSE}
  Classes,
  nyx.studio.projectstore,
  {$ENDIF}
  nyx.text,
  nyx.model,
  nyx.types,
  nyx.codec,
  nyx.source,
  nyx.codegen,
  nyx.studio.projects,
  nyx.studio.session,
  nyx.test.source;

const
  BadNames: array[0..6] of TNyxText =
    ('../escape', 'CON', 'a/b', 'a\b', 'name.', 'COM1', '');

var
  LCount: Integer;
  LDocument: TNyxDocument;
  LEmpty: TNyxDocument;
  LSession: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LConflict: TNyxProjectPair;
  LSource: TNyxText;
  LBaseline: TNyxText;
  LPacket: TNyxText;
  LRejected: Boolean;
  LName: TNyxText;
  LIndex: Integer;
  {$IFNDEF PAS2JS}
  LStore: TNyxProjectStore;
  LRoot: TNyxText;
  LRevision: TNyxText;
  LNextRevision: TNyxText;
  LRemote: TNyxText;
  LCurrent: TNyxText;
  LLinkedStore: TNyxProjectStore;
  LLinkRoot: TNyxText;
  {$ENDIF}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL projects: ' + AMessage);
  end;
  Inc(LCount);
end;

function ReplaceText(const AText, ABefore, AAfter: TNyxText): TNyxText;
var
  LAt: Integer;
  LParts: TNyxStrings;
begin
  LAt := Pos(ABefore, AText);

  if LAt = 0 then
  begin
    raise Exception.Create('Missing project source fixture token');
  end;
  LParts := TNyxStrings.Create;
  try
    LParts.Add(Copy(AText, 1, LAt - 1));
    LParts.Add(AAfter);
    LParts.Add(Copy(AText, LAt + Length(ABefore), MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

procedure RejectPair(const APair: TNyxProjectPair);
var
  LBefore: TNyxText;
begin
  LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
  LRejected := False;
  try
    LSession.LoadProject(APair);
  except
    on LException: Exception do
    begin
      LRejected := True;
      Check(LException.Message <> '', 'pair failure has a diagnostic');
    end;
  end;
  Check(LRejected, 'invalid/mismatched pair is rejected');
  Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
    'rejected pair preserves accepted design/source/draft');
end;

{$IFNDEF PAS2JS}
procedure WriteFile(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function ReadFile(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead);
  try
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LStream.Free;
  end;
end;
{$ENDIF}

begin
  try
    LDocument := CreateNyxEditedFixture(LSource);
    LSession := TNyxStudioSession.Create;
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
      LPacket := EncodeNyxProject(LPair);
      Check(DecodeNyxProject(LPacket).Source = LSource, 'wire retains exact crafted Pascal');
      LSession.LoadProject(DecodeNyxProject(LPacket));
      Check(LSession.Save = LPair.Design, 'paired load preserves admitted design');
      Check(LSession.Source = LSource, 'paired load preserves helpers/imports/comments/locals');
      LBaseline := EncodeNyxProject(LSession.ProjectSnapshot);
      LSession.SetSourceDraft(TNyxText('unsupported draft / 🌙') + NyxScalarText(0));
      LSession.SetTitle('New design / 漢字 🌙');
      LPair := LSession.ProjectSnapshot;
      Check(LPair.Pending and (LPair.DraftBase = LSource), 'stale draft retains its old baseline');
      LSession.LoadProject(DecodeNyxProject(EncodeNyxProject(LPair)));
      Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
        'reopen preserves exact stale/rejected draft and base');
      LRejected := False;
      try
        LSession.ApplySourceDraft;
      except
        LRejected := True;
      end;
      Check(LRejected and (LSession.DraftSource = LPair.Draft), 'recovered stale draft cannot replace design');
      LSession.DiscardSourceDraft;
      LSession.SetSourceDraft('');
      LPair := LSession.ProjectSnapshot;
      LSession.LoadProject(DecodeNyxProject(EncodeNyxProject(LPair)));
      Check(LSession.ProjectSnapshot.Pending and (LSession.DraftSource = ''),
        'empty rejected draft is distinct from no pending draft');

      LSession.LoadProject(DecodeNyxProject(LBaseline));
      LSession.SetTitle('History before bad import');
      LSession.Undo;
      LPair := DecodeNyxProject(LBaseline);
      LConflict := LPair;
      LConflict.Source := ReplaceText(LSource, '.Text(''CRAFTED / 🌙'')', '.Text(''Changed / 🌙'')');
      RejectPair(LConflict);
      LSession.Redo;
      Check(LSession.Document.Title = 'History before bad import', 'rejection retains redo history');
      LSession.LoadProject(LConflict, nprUsePascal);
      Check(LSession.Document.Find('eyebrow').Prop('text') = TNyxText('Changed / 🌙'),
        'explicit Pascal resolution updates the design');
      Check(LSession.Source = LConflict.Source, 'Pascal resolution retains crafted source exactly');
      LSession.LoadProject(LConflict, nprUseDesign);
      Check(LSession.Document.Find('eyebrow').Prop('text') = TNyxText('CRAFTED / 🌙'),
        'explicit design resolution retains design values');
      Check(LSession.ProjectSnapshot.Pending and
        (LSession.DraftSource = LConflict.Source), 'design resolution retains conflicting Pascal as draft');
      LConflict.Pending := True;
      LConflict.Draft := 'second independent rejected draft';
      LConflict.DraftBase := LSource;
      LRejected := False;
      try
        LSession.LoadProject(LConflict, nprUseDesign);
      except
        LRejected := True;
      end;
      Check(LRejected, 'design resolution cannot overwrite an independent draft');
      LConflict := LPair;
      LConflict.Source := ReplaceText(LSource, '.Text(''CRAFTED / 🌙'')', '.Text(Mystery())');
      RejectPair(LConflict);
      LSession.LoadProject(LConflict, nprUseDesign);
      Check(LSession.DraftSource = LConflict.Source, 'unsupported Pascal stays recoverable as a draft');
      LConflict := LPair;
      LConflict.Design := '{"version":1}';
      RejectPair(LConflict);
      LConflict := LPair;
      LConflict.Pending := False;
      LConflict.Draft := 'hidden text';
      RejectPair(LConflict);
      LRejected := False;
      try
        { The two fpjson formatters use different whitespace. Construct the bad
          version directly so a missing replacement cannot masquerade as rejection. }
        DecodeNyxProject('{"version":9,"design":"","source":"",' +
          '"draft":"","draftBase":"","pending":false}');
      except
        LRejected := True;
      end;
      Check(LRejected, 'unknown project version is rejected');
      LEmpty := TNyxDocument.Create;
      try
        LSession.LoadProject(NyxProjectPair(TNyxCodec.Encode(LEmpty),
          TNyxCodegen.Generate(LEmpty)));
        Check((LSession.Document.Count = 0) and (LSession.ActiveViewID = ''),
          'empty paired project loads without accessing a nonexistent page');
        LSession.AddPage;
        Check(LSession.Document.Count = 1, 'empty paired project remains authorable');
        LEmpty.AddComponent(TNyxNode.Create(nkColumn, 'reusable-root'));
        LSession.LoadProject(NyxProjectPair(TNyxCodec.Encode(LEmpty),
          TNyxCodegen.Generate(LEmpty)));
        Check((LSession.Document.Count = 0) and
          (LSession.ActiveViewID = 'reusable-root'), 'component-only project opens its reusable view');
        Check(LSession.ActiveView = LSession.Document.Components[0],
          'component-only project selection belongs to the admitted document');
      finally
        LEmpty.Free;
      end;

      for LIndex := Low(BadNames) to High(BadNames) do
      begin
        LName := BadNames[LIndex];
        LRejected := False;
        try
          ValidateNyxProjectName(LName);
        except
          LRejected := True;
        end;
        Check(LRejected, 'confined project key: ' + LName);
      end;
      ValidateNyxProjectName('My-project_2');
      Check(True, 'portable project key is admitted');

      {$IFNDEF PAS2JS}
      LRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1))) +
        'project-' + FormatDateTime('yyyymmddhhnnsszzz', Now);
      LStore := TNyxProjectStore.Create(LRoot);
      try
        Check(LStore.ReadProject('craft', LRevision) = '', 'missing project does not create members');
        Check(LStore.SaveProject('craft', '', LPair, LRevision, LRemote), 'new paired save succeeds');
        Check(LRevision <> '', 'save supplies a content revision');
        LCurrent := LStore.ReadProject('craft', LNextRevision);
        Check((LNextRevision = LRevision) and (DecodeNyxProject(LCurrent).Source = LSource),
          'reopen reads exact adjacent source');
        Check(ReadFile(LRoot + '/craft/design.nyx') = LPair.Design, 'design exists as an adjacent real file');
        Check(ReadFile(LRoot + '/craft/nyx.edited.view.pas') = LSource, 'Pascal exists under its actual unit filename');
        Check(not LStore.SaveProject('craft', '', LPair, LNextRevision, LRemote),
          'create cannot overwrite an existing project');
        Check((LNextRevision = LRevision) and (LRemote = LCurrent), 'conflict returns complete remote pair');
        LConflict := LPair;
        LConflict.Pending := True;
        LConflict.Draft := 'rejected source / 🌙';
        LConflict.DraftBase := 'older baseline / 🌙';
        Check(LStore.SaveProject('craft', LRevision, LConflict, LNextRevision, LRemote),
          'draft metadata saves with the accepted pair');
        Check(LNextRevision <> LRevision, 'draft-only saves change the concurrency revision');
        Check(DecodeNyxProject(LRemote).DraftBase = LConflict.DraftBase, 'disk recovery keeps stale draft base');
        Check(ReadFile(LRoot + '/craft/previous.nyxproject') = LCurrent, 'save retains previous complete project');

        WriteFile(LRoot + '/craft/nyx.edited.view.pas', LSource + #10 + TNyxText('// External editor / 🌙'));
        LCurrent := LStore.ReadProject('craft', LRevision);
        Check(LRevision <> LNextRevision, 'external Pascal edit changes revision without timestamp guessing');
        Check(not LStore.SaveProject('craft', LNextRevision, LPair, LNextRevision, LRemote),
          'stale client cannot overwrite an external edit');
        Check(DecodeNyxProject(LRemote).Source = LSource + #10 + TNyxText('// External editor / 🌙'),
          'conflict retains exact external source');
        { Simulate process interruption after the journal commit and after only
          one member replacement. Recovery must finish the committed whole pair. }
        WriteFile(LRoot + '/craft/pending.nyxproject', EncodeNyxProject(LPair));
        WriteFile(LRoot + '/craft/design.nyx', '{"interrupted":true}');
        LCurrent := LStore.ReadProject('craft', LRevision);
        Check(DecodeNyxProject(LCurrent).Source = LSource, 'journal recovery replaces stale Pascal');
        Check(ReadFile(LRoot + '/craft/design.nyx') = LPair.Design, 'journal recovery replaces interrupted design');
        Check(not FileExists(LRoot + '/craft/pending.nyxproject'), 'completed recovery clears the journal');
        WriteFile(LRoot + '/craft/pending.nyxproject.next', '{"incomplete":');
        Check(LStore.ReadProject('craft', LNextRevision) = LCurrent, 'incomplete uncommitted packet is ignored');
        Check(LNextRevision = LRevision, 'uncommitted write cannot alter accepted revision');
        LConflict := LPair;
        LConflict.Source := 'bad source';
        LRejected := False;
        try
          LStore.SaveProject('craft', LRevision, LConflict, LNextRevision, LRemote);
        except
          LRejected := True;
        end;
        Check(LRejected and (LStore.ReadProject('craft', LNextRevision) = LCurrent),
          'complete pair admission precedes every disk write');
      finally
        LStore.Free;
      end;
      { Optional second argument is an explicitly prepared, owned host fixture:
        redirected is a real directory link to target, whose sentinel is UTF-8
        text. Pascal qualifies public read/save refusal; platform orchestration
        prepares the link without changing a user project or dependency. }

      if ParamCount > 1 then
      begin
        LLinkRoot := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
        Check(DirectoryExists(LLinkRoot + 'redirected'),
          'redirect fixture contains a real host directory link');
        LBaseline := ReadFile(LLinkRoot + 'target/sentinel.txt');
        LLinkedStore := TNyxProjectStore.Create(LLinkRoot);
        try
          LRejected := False;
          try
            LLinkedStore.ReadProject('redirected', LRevision);
          except
            on LException: ENyxModel do
            begin
              LRejected := Pos('symbolic links', LException.Message) > 0;
            end;
          end;
          Check(LRejected, 'project reads refuse a redirected host directory');
          LRejected := False;
          try
            LLinkedStore.SaveProject('redirected', '', LPair, LRevision, LRemote);
          except
            on LException: ENyxModel do
            begin
              LRejected := Pos('symbolic links', LException.Message) > 0;
            end;
          end;
          Check(LRejected, 'project saves refuse a redirected host directory');
          Check((ReadFile(LLinkRoot + 'target/sentinel.txt') = LBaseline) and
            not FileExists(LLinkRoot + 'target/pending.nyxproject') and
            not FileExists(LLinkRoot + 'target/design.nyx') and
            not FileExists(LLinkRoot + 'target/project.nyxproject'),
            'refusal retains the real target bytes and creates no project files');
        finally
          LLinkedStore.Free;
        end;
      end;
      {$ENDIF}
    finally
      LSession.Free;
      LDocument.Free;
    end;
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-nyx-projects', 'passed');
    document.body.setAttribute('data-nyx-project-checks', IntToStr(LCount));
    {$ELSE}
    WriteLn('PASS ', LCount, ' paired project/admission/disk recovery checks');
    {$ENDIF}
  except
    on LException: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-nyx-projects', 'failed');
      document.body.setAttribute('data-nyx-project-error', LException.Message);
      {$ELSE}
      WriteLn(LException.Message);
      Halt(1);
      {$ENDIF}
    end;
  end;
end.
