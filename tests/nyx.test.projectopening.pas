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
unit nyx.test.projectopening;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.sourceprojection;

{ Both-target saved-project admission checks. The caller supplies the actual
  constructor result from its owning target, never a simulated producer. Each
  preparation reconstructs independent owners; completion uses ordinary sealed
  opening and its exact history/draft/load guards. No file or listener is owned. }
function RunNyxProjectOpeningChecks(const AProjection: INyxSourceProjection): Integer;

implementation

uses SysUtils, nyx.text, nyx.codec, nyx.model, nyx.schema,
  nyx.source.preparation, nyx.studio.projects, nyx.studio.session,
  nyx.studio.projectionediting;

const
  CUnfinished: TNyxText = 'An unfinished idea 🚀 and é.';
  COlderBase: TNyxText = 'An older accepted baseline 𐐷.';

function RunNyxProjectOpeningChecks(const AProjection: INyxSourceProjection): Integer;
var
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LDivergent: TNyxProjectPair;
  LBefore: TNyxText;
  LBeforePair: TNyxProjectPair;
  LRequest: TNyxStudioProjectRequest;
  LSchemas: INyxSchemaSnapshot;
  LPrepared: INyxPreparedSource;
  LRefused: Boolean;
  LChecks: Integer;
  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Saved project opening: ' + AReason);
    end;
    Inc(LChecks);
  end;
  procedure Baseline;
  begin
    LSession.LoadProject(LBeforePair);
    LSession.SetTitle('Independent local work');
    LSession.Undo;
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(LSession.CanRedo and not LSession.CanUndo, 'baseline owns an actual paired Redo');
  end;
begin
  LChecks := 0;
  Check((AProjection <> nil) and (AProjection.State = spsExecuted),
    'caller supplies actual target construction');
  LSession := TNyxStudioSession.Create;
  LOther := nil;
  LDocument := nil;
  try
    LBeforePair := LSession.ProjectSnapshot;
    LPair := NyxProjectPair(AProjection.Design, AProjection.Source);
    LPair.Pending := True;
    LPair.Draft := CUnfinished;
    LPair.DraftBase := COlderBase;
    LSchemas := CaptureNyxSchemas;
    Baseline;
    LRequest := LSession.PrepareProjectRequest(LPair, nprRequireMatch, LSchemas.Revision);
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscApplied,
      'complete saved pair reaches ordinary project admission');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'whole companion, design and independent unfinished draft/base remain exact');
    Check(not LSession.CanUndo and not LSession.CanRedo,
      'opening deliberately starts new history rather than behaving as Apply');
    Check((LSession.ActiveViewID = 'notebook-1') and
      (LSession.SelectedID = LSession.ActiveViewID), 'new owned roots have usable navigation');
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscStale,
      'replay cannot transfer retired preparation into the newly loaded owner');
    LOther := TNyxStudioSession.CreateRecovered(LSession.RecoveryFrame);
    Check(EncodeNyxProject(LOther.ProjectSnapshot) = EncodeNyxProject(LPair),
      'fresh recovered owners retain actual executed source and unfinished text');
    LOther.SetSourceDraft(CUnfinished + ' Another idea.');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'recovered owners remain independent of their live source/draft origin');
    FreeAndNil(LOther);

    Baseline;
    LRequest := LSession.PrepareProjectRequest(LPair, nprRequireMatch, LSchemas.Revision);
    LSession.SetSourceDraft(CUnfinished);
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscStale,
      'typing during compilation supersedes the opening request');
    Check((EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and LSession.CanRedo,
      'newer exact draft and existing history survive stale completion');

    Baseline;
    LRequest := LSession.PrepareProjectRequest(LPair, nprRequireMatch, LSchemas.Revision);
    LSession.LoadProject(LBeforePair);
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscStale,
      'identical file and control names cannot authorize a reloaded project');

    Baseline;
    LRequest := LSession.PrepareProjectRequest(LPair, nprRequireMatch, LSchemas.Revision);
    LOther := TNyxStudioSession.Create;
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    Check(LOther.CompleteProjectRequest(LRequest, LPrepared) = nscStale,
      'another session cannot use the sealed opening request');
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscApplied,
      'refusing another session does not consume the rightful independent owners');

    LDivergent := LPair;
    LDocument := TNyxCodec.Decode(LPair.Design);
    LDocument.Title := 'A divergent saved design';
    LDivergent.Design := TNyxCodec.Encode(LDocument);
    FreeAndNil(LDocument);
    Baseline;
    LRequest := LSession.PrepareProjectRequest(LDivergent, nprRequireMatch, LSchemas.Revision);
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    LRefused := False;
    try
      LSession.CompleteProjectRequest(LRequest, LPrepared);
    except
      on ENyxProjectConflict do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore) and
      LSession.CanRedo, 'disagreeing design refuses without changing accepted files/history');
    LRequest := LSession.PrepareProjectRequest(LDivergent, nprUsePascal, LSchemas.Revision);
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscApplied,
      'explicit Pascal choice can admit the actual compiled meaning');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'explicit resolution retains the independent unfinished buffer exactly');

    Baseline;
    LRequest := LSession.PrepareProjectRequest(LPair, nprRequireMatch, LSchemas.Revision + 1);
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    Check(LSession.CompleteProjectRequest(LRequest, LPrepared) = nscStale,
      'a different creator generation cannot publish prepared owners');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'creator-generation refusal preserves the complete local pair');
  finally
    LPrepared := nil;
    LSchemas := nil;
    LDocument.Free;
    LOther.Free;
    LSession.Free;
  end;
  Result := LChecks;
end;

end.
