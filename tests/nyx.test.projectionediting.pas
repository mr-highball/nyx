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


unit nyx.test.projectionediting;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.sourceprojection;

{ Consume an actually compiled/executed result through the existing guarded
  editor publication and history, on FPC and HTTP pas2js. No synthetic execution
  claim, protected Studio session or external transport is borrowed. }
function RunNyxProjectionEditingChecks(const AProjection: INyxSourceProjection): Integer;

implementation

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.source,
  nyx.source.preparation, nyx.schema, nyx.studio.session,
  nyx.studio.projectionediting, nyx.studio.builds;

function RunNyxProjectionEditingChecks(const AProjection: INyxSourceProjection): Integer;
var
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LSchemas: INyxSchemaSnapshot;
  LPrepared: INyxPreparedSource;
  LRequest: TNyxStudioSourceRequest;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LEmptyDocument: TNyxDocument;
  LEmptyWorkspace: TNyxSourceWorkspace;
  LRestored: TNyxSourceWorkspace;
  LFrame: TNyxSourceCheckpoint;
  LSource: TNyxText;
  LDesign: TNyxText;
  LBeforeSource: TNyxText;
  LBeforeDesign: TNyxText;
  LSnapshot: TNyxText;
  LRefused: Boolean;
  LFailure: INyxSourceProjection;
  LWire: TNyxDataValue;
  LCount: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Projected source publication: ' + AReason);
    end;
    Inc(LCount);
  end;

  procedure CaptureRequest;
  begin
    LSchemas := CaptureNyxSchemas;
    LSession.SetSourceDraft(LSource);
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
  end;

begin
  LCount := 0;
  Check((AProjection <> nil) and (AProjection.State = spsExecuted),
    'qualification starts with actual admitted execution');
  LSource := AProjection.Source;
  LDesign := AProjection.Design;
  LSession := TNyxStudioSession.Create;
  LOther := nil;
  LDocument := nil;
  LWorkspace := nil;
  LEmptyDocument := nil;
  LEmptyWorkspace := nil;
  LRestored := nil;
  try
    LBeforeSource := LSession.Source;
    LBeforeDesign := LSession.Save;
    CaptureRequest;
    Check(not LPrepared.Diagnostic.Defined and (LPrepared.Source = LSource) and
      (LPrepared.Design = LDesign), 'independent prepared complete pair');
    Check(LRequest.Origin = nsoDeclarative, 'request captures original construction origin');
    LRefused := False;
    try
      LPrepared.ToData;
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'execution cannot masquerade as a literal worker reply');
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscApplied,
      'actual compiler result publishes through existing guarded Apply');
    LPrepared := nil;
    Check((LSession.Source = LSource) and (LSession.Save = LDesign) and
      not LSession.SourceDraftPending, 'exact unit/design and consumed draft');
    Check(LSession.CanUndo and not LSession.CanRedo, 'one new paired command');
    Check(LSession.PrepareSourceRequest(NyxSchemaRevision).Origin = nsoExecuted,
      'host observes typed executed origin');

    LRefused := False;
    try
      LSession.SetTitle('A visual edit awaits reconciliation');
    except
      on LException: ENyxSourceExecutionRequired do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Source = LSource) and (LSession.Save = LDesign) and
      LSession.CanUndo and not LSession.CanRedo,
      'visual refusal rolls back the complete pair and history without regeneration');

    LSession.Undo;
    Check((LSession.Source = LBeforeSource) and (LSession.Save = LBeforeDesign) and
      not LSession.SourceDraftPending, 'one Undo restores original accepted pair');
    Check(not LSession.CanUndo and LSession.CanRedo, 'single paired undo entry');
    LSession.Redo;
    Check((LSession.Source = LSource) and (LSession.Save = LDesign),
      'Redo restores complete handwritten unit and design');
    Check(LSession.PrepareSourceRequest(NyxSchemaRevision).Origin = nsoExecuted,
      'executed origin survives paired history');
    Check(LSession.CompleteSourceRequest(LRequest,
      PrepareNyxProjectedSource(AProjection, LSchemas)) = nscStale,
      'duplicate completion cannot consume another result');

    { Whole unmarked source survives both typed and serialized checkpoint
      restoration. Snapshot restoration is not source/compiler admission. }
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    LPrepared.Take(LDocument, LWorkspace);
    Check((LWorkspace.Origin = nsoExecuted) and (LWorkspace.Render(LDocument) = LSource),
      'unmarked source retains exact bytes through ordinary Render');
    LFrame := LWorkspace.Capture;
    LSnapshot := LWorkspace.Snapshot;
    LWorkspace.Free;
    LWorkspace := nil;
    LRestored := TNyxSourceWorkspace.Create;
    LRestored.Restore(LFrame);
    Check((LRestored.Origin = nsoExecuted) and (LRestored.Render(LDocument) = LSource),
      'typed frame survives original workspace release');
    LRestored.Reset;
    Check(LRestored.Origin = nsoDeclarative, 'Reset clears execution metadata');
    LRestored.Restore(LSnapshot);
    Check((LRestored.Origin = nsoExecuted) and (LRestored.Render(LDocument) = LSource),
      'serialized checkpoint retains explicit origin and exact source');

    LRefused := False;
    try
      LPrepared.Take(LEmptyDocument, LEmptyWorkspace);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDocument <> nil) and (LEmptyDocument = nil) and
      (LEmptyWorkspace = nil),
      'one-shot transfer refuses reuse without changing live destinations');
    LPrepared := nil;
    LWire := NyxObject([NyxField('version', NyxData(1)),
      NyxField('source', NyxData(LSource)), NyxField('design', NyxData(LDesign)),
      NyxField('workspace', NyxData(LSnapshot)),
      NyxField('schemaRevision', NyxData(LSchemas.Revision)),
      NyxField('diagnostic', NyxObject([
        NyxField('defined', NyxData(False)), NyxField('message', NyxData('')),
        NyxField('line', NyxData(0)), NyxField('column', NyxData(0))]))]);
    LRefused := False;
    try
      ReceiveNyxPreparedSource(LWire, LSource, LSchemas);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'bundled literal worker refuses an execution checkpoint flag');
    Check(PrepareNyxSource(LSource, LSchemas).Diagnostic.Defined,
      'ordinary source import retains strict admission');

    LSession.Free;
    LSession := TNyxStudioSession.Create;
    CaptureRequest;
    LSession.SetSourceDraft(LSource + #10);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'newer pending source refuses stale completion');
    Check((LSession.Source = LBeforeSource) and (LSession.Save = LBeforeDesign) and
      (LSession.DraftSource = LSource + #10) and not LSession.CanUndo,
      'stale result preserves original pair, newer draft and history');

    CaptureRequest;
    LOther := TNyxStudioSession.Create;
    LOther.SetSourceDraft(LSource);
    Check(LOther.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'other editor owner refuses captured completion');
    Check(LOther.Source = LBeforeSource, 'other owner pair stays exact');
    LOther.Free;
    LOther := nil;

    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision + 1);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'wrong creator revision refuses without ownership transfer');
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscApplied,
      'stale refusals retain detached owners for the correct current command');
    LPrepared := nil;

    { Real failures have no transferable owners and preserve the accepted pair.
      The failure factory carries exact source; it cannot forge an Executed state. }
    LSession.SetSourceDraft(LSource + #10);
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LFailure := NyxSourceProjectionFailure(LRequest.Source, AProjection.Target,
      spsCompilationFailed, 'Compiler rejected the new draft');
    LPrepared := PrepareNyxProjectedSource(LFailure, LSchemas);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscRejected,
      'failed projection records rejection instead of publishing');
    Check((LSession.Source = LSource) and (LSession.Save = LDesign) and
      (LSession.DraftSource = LRequest.Source) and LSession.SourceDiagnostic.Defined,
      'failure retains exact accepted pair and current draft');
    LPrepared := PrepareNyxProjectedSource(AProjection, LSchemas);
    LRefused := False;
    try
      LSession.CompleteSourceRequest(LRequest, LPrepared);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'mismatched successful source refuses current command');

    LSession.DiscardSourceDraft;
    LSession.SetSourceDraft(LSource + #10);
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LSession.Document.Title := 'A newer public document';
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'executed baseline guard refuses out-of-band model changes before Render');
    Check(LSession.Document.Title = 'A newer public document',
      'stale completion does not overwrite newer public work');
    Result := LCount;
  finally
    LEmptyWorkspace.Free;
    LEmptyDocument.Free;
    LRestored.Free;
    LWorkspace.Free;
    LDocument.Free;
    LOther.Free;
    LSession.Free;
  end;
end;

end.
