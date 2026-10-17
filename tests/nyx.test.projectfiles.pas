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
unit nyx.test.projectfiles;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.projectinputs;

{ Portable ownership checks consume actual compiler results supplied by the
  native host. They do not simulate execution or grant authority from JSON.
  Browser compilation of this consumer is separate from browser execution. }
function RunNyxProjectFilesChecks(const AFirst, ASecond,
  AComment: INyxPascalProjectBuild): Integer;

implementation

uses SysUtils, nyx.text, nyx.pascal.project, nyx.source, nyx.model, nyx.codec,
  nyx.data, nyx.schema, nyx.source.preparation, nyx.studio.projects,
  nyx.studio.session, nyx.studio.sourceprojection, nyx.studio.projectionediting;

function RunNyxProjectFilesChecks(const AFirst, ASecond,
  AComment: INyxPascalProjectBuild): Integer;
const
  CUnfinished: TNyxText = '{ A separate unfinished idea 🚀';
  CBase: TNyxText = 'An earlier accepted source 𐐷';
var
  LCount: Integer;
  LSession: TNyxStudioSession;
  LRecovered: TNyxStudioSession;
  LWorkspace: TNyxSourceWorkspace;
  LDocument: TNyxDocument;
  LFirst: TNyxProjectPair;
  LSecond: TNyxProjectPair;
  LComment: TNyxProjectPair;
  LInvalid: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LBefore: TNyxText;
  LSnapshot: TNyxText;
  LCheckpoint: TNyxSourceCheckpoint;
  LRequest: TNyxStudioProjectRequest;
  LSchemas: INyxSchemaSnapshot;
  LPrepared: INyxPreparedSource;
  LRefused: Boolean;
  LFiles: TNyxPascalProjectFiles;
  LQueuedPair: TNyxProjectPair;
  LSourceRequest: TNyxStudioSourceRequest;
  LDesignRequest: TNyxStudioDesignRequest;
  LLaterDesignRequest: TNyxStudioDesignRequest;
  LEdit: TNyxStudioDesignEdit;

  procedure Check(AValue: Boolean; const AReason: String);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LCount);
  end;

  function Pair(const ABuild: INyxPascalProjectBuild): TNyxProjectPair;
  begin
    Check((ABuild <> nil) and (ABuild.Projection.State = spsExecuted),
      'each admitted project comes from actual construction');
    Result := NyxProjectPair(ABuild.Projection.Design, ABuild.Projection.Source);
    Result.Files := TNyxPascalProjectFiles.Capture(ABuild.Inputs);
    Result.Pending := True;
    Result.Draft := CUnfinished;
    Result.DraftBase := CBase;
  end;

begin
  LCount := 0;
  LFirst := Pair(AFirst);
  LSecond := Pair(ASecond);
  LComment := Pair(AComment);
  Check((LFirst.Source = LSecond.Source) and (LFirst.Source = LComment.Source),
    'all changes are supporting-file changes beside the exact same main unit');
  Check(LFirst.Design <> LSecond.Design, 'changed helper behavior changes construction');
  Check((LSecond.Design = LComment.Design) and
    (LSecond.Files.Encoded <> LComment.Files.Encoded),
    'comment-only helper change retains main/design but changes accepted files');
  Check(DecodeNyxProject(EncodeNyxProject(LFirst)).Files.Encoded = LFirst.Files.Encoded,
    'version two transports exact ordered helper inventory');
  Check(TNyxDataValue.ParseJSON(EncodeNyxProject(LFirst)).Field('version').AsInteger = 2,
    'supporting files have an explicit project format version');
  Check(TNyxDataValue.ParseJSON(EncodeNyxProject(NyxProjectPair(
    LFirst.Design, LFirst.Source))).Field('version').AsInteger = 1,
    'absence preserves the original single-unit envelope');

  LSession := TNyxStudioSession.Create;
  LRecovered := nil;
  LWorkspace := nil;
  LDocument := nil;
  try
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareProjectRequest(LFirst, nprRequireMatch, LSchemas.Revision);
    LPrepared := PrepareNyxProjectedSource(AFirst.Projection, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'actual multi-file preparation admits includes');
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscApplied,
      'ordinary project completion retains actual compiled helper ownership');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LFirst),
      'ordinary Open retains accepted files and the exact independent main draft');
    Check(not LSession.CanUndo and not LSession.CanRedo, 'Open begins deliberate new history');
    LCheckpoint := LSession.AcceptedSourceCheckpoint;
    Check(LCheckpoint.Files.Encoded = LFirst.Files.Encoded,
      'opaque accepted checkpoint owns the complete supporting inventory');

    LWorkspace := TNyxSourceWorkspace.Create;
    LWorkspace.Restore(LCheckpoint);
    LSnapshot := LWorkspace.Snapshot;
    Check(TNyxDataValue.ParseJSON(LSnapshot).Count = 7,
      'multi-file executed snapshots have an explicit additional member');
    LWorkspace.Reset;
    Check(LWorkspace.Capture.Files.IsEmpty, 'Reset retires helper ownership');
    LWorkspace.Restore(LSnapshot);
    Check((LWorkspace.Snapshot = LSnapshot) and
      (LWorkspace.Capture.Files.Encoded = LFirst.Files.Encoded),
      'checkpoint wire round trip preserves exact files without claiming execution');
    Check(LWorkspace.Capture.StorageBytes >= LFirst.Files.StorageBytes,
      'history byte accounting includes retained supporting source');
    FreeAndNil(LWorkspace);

    LSession.AdoptProjectedProject(LSecond, ASecond.Projection);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LSecond),
      'helper-only behavior change publishes the entire project');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LFirst),
      'one Undo restores helper meaning, design, main source and unfinished buffers');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LSecond),
      'one Redo restores the complete independent helper snapshot');
    LRecovered := TNyxStudioSession.CreateRecovered(LSession.RecoveryFrame);
    LRecovered.DiscardSourceDraft;
    LSourceRequest := LRecovered.PrepareSourceRequest(LSchemas.Revision);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Value := 'An idea captured before the helper change';
    LDesignRequest := LRecovered.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LQueuedPair := LComment;
    LQueuedPair.Pending := False;
    LQueuedPair.Draft := '';
    LQueuedPair.DraftBase := '';
    LRecovered.AdoptProjectedProject(LQueuedPair, AComment.Projection);
    LLaterDesignRequest := LRecovered.PrepareDesignRequest(LEdit, LSchemas.Revision);
    Check(not LDesignRequest.SameRequest(LLaterDesignRequest),
      'queued request identity includes helper bytes beside unchanged main/design');
    Check(LRecovered.CompleteSourceRequest(LSourceRequest, nil) = nscStale,
      'source completion is stale after only helper bytes change');
    Check(LRecovered.CompleteDesignRequest(LDesignRequest, nil) = nscStale,
      'visual completion is stale before inspecting an older prepared result');
    Check(LRecovered.ProjectSnapshot.Files.Encoded = LComment.Files.Encoded,
      'refusing queued results preserves the later exact helper inventory');
    FreeAndNil(LRecovered);
    LSession.AdoptProjectedProject(LComment, AComment.Projection);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LComment),
      'same main/design still publishes a changed supporting-file inventory');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LSecond),
      'same-design supporting edit is a real paired Undo command');

    LRecovered := TNyxStudioSession.CreateRecovered(LSession.RecoveryFrame);
    Check(EncodeNyxProject(LRecovered.ProjectSnapshot) = EncodeNyxProject(LSecond),
      'independent live recovery frame carries accepted supporting files');
    LRecovered.Redo;
    Check(EncodeNyxProject(LRecovered.ProjectSnapshot) = EncodeNyxProject(LComment),
      'recovered Redo retains the exact helper-only edit');
    LRecovered.SetSourceDraft(CUnfinished + ' Another idea.');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LSecond),
      'recovered main drafts never alias the original project');
    FreeAndNil(LRecovered);
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);

    LRefused := False;
    try
      LSession.AdoptProjectedProject(LComment, ASecond.Projection);
    except
      on ENyxProjectConflict do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and
      LSession.CanRedo, 'wrong helpers with identical main/design refuse and retain Redo');

    LRefused := False;
    try
      LSession.AdoptCapturedProject(LComment, LSession.AcceptedSourceCheckpoint);
    except
      on ENyxProjectConflict do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'opaque live checkpoint cannot authorize different helper data');

    LInvalid := LSecond;
    LInvalid.Files := Default(TNyxPascalProjectFiles);
    LRefused := False;
    try
      LSession.AdoptProjectedProject(LInvalid, ASecond.Projection);
    except
      on ENyxProjectConflict do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'single-unit pair cannot discard a captured construction inventory');

    LRefused := False;
    try
      AdmitNyxProject(LSecond, nprUseDesign, LDocument, LWorkspace, LResolved);
    except
      on ENyxSourceExecutionRequired do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDocument = nil) and (LWorkspace = nil),
      'literal/design resolution cannot silently remove supporting-file meaning');

    LFiles := TNyxPascalProjectFiles.Parse(LSecond.Files.Encoded, LSecond.Source);
    Check(LFiles.Encoded = LSecond.Files.Encoded, 'supporting data canonicalizes deterministically');

    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LComment),
      'all refusals preserve the earlier real Redo entry');
  finally
    LWorkspace.Free;
    LDocument.Free;
    LRecovered.Free;
    LSession.Free;
  end;
  Result := LCount;
end;

end.
