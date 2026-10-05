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
unit nyx.test.source.commands;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Shared publication/transport tests use public session and preparation contracts.
  Physical editor consumption is qualified separately on the actual adapters. }
function RunNyxSourceCommandTests: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.schema, nyx.source, nyx.source.preparation,
  nyx.studio.session, nyx.studio.projects;

function RunNyxSourceCommandTests: Integer;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LSeed: TNyxProjectPair;
  LRequest: TNyxStudioSourceRequest;
  LPrepared: INyxPreparedSource;
  LReceived: INyxPreparedSource;
  LSchemas: INyxSchemaSnapshot;
  LSource: TNyxText;
  LChanged: TNyxText;
  LWire: TNyxDataValue;
  LFailed: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Source command: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LSession := nil;
  LOther := nil;
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Source fixture';
    LDocument.AddPage(NewNyxColumn('home').Add(NewNyxMemo('notes')));
    LSeed := Default(TNyxProjectPair);
    LSeed.Design := TNyxCodec.Encode(LDocument);
    LSeed.Source := TNyxCodegen.Generate(LDocument);
    LSession := TNyxStudioSession.Create(LSeed);
    LOther := TNyxStudioSession.Create(LSeed);
    LSource := LSession.Source;
    LChanged := StringReplace(LSource, 'Source fixture', 'Prepared fixture', [rfReplaceAll]);
    LSession.SetSourceDraft(LChanged);
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxSource(LRequest.Source, LSchemas);
    LWire := LPrepared.ToData;
    LReceived := ReceiveNyxPreparedSource(LWire, LChanged, LSchemas);
    Check(not LReceived.Diagnostic.Defined and (LReceived.Design = LPrepared.Design),
      'private processor handoff retains the exact independently owned pair');
    Check(LSession.CompleteSourceRequest(LRequest, LReceived) = nscApplied,
      'current request publishes one admitted pair');
    Check((LSession.Document.Title = 'Prepared fixture') and
      (LSession.Source = LChanged) and not LSession.ProjectSnapshot.Pending,
      'publication retains exact authored source and clears only its draft');
    LSession.Undo;
    Check((LSession.Source = LSource) and (LSession.Save = LSeed.Design),
      'one Undo restores both exact accepted files');
    LSession.Redo;
    Check(LSession.Source = LChanged, 'one Redo restores the admitted source');
    LSession.Undo;
    LSession.SetSourceDraft(LChanged);
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxSource(LChanged, LSchemas);
    Check(LOther.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'identical files in another session cannot consume this ticket');
    LSession.SetSourceDraft(LChanged + #10 + '{ newer draft }');
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'later draft rejects an older completion');
    Check((LSession.Source = LSource) and LSession.CanRedo and
      (LSession.DraftSource = LChanged + #10 + '{ newer draft }'),
      'stale completion retains accepted pair, exact draft and Redo');
    LSession.SetSourceDraft(LChanged);
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LSession.Document.Title := 'Direct mutation';
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'fresh baseline comparison detects public direct mutation');
    Check((LSession.Document.Title = 'Direct mutation') and
      (LSession.DraftSource = LChanged), 'direct edit and draft remain independent');
    LSession.Document.Title := 'Source fixture';
    LSession.DiscardSourceDraft;
    Check(LSession.Source = LSource, 'direct repair remains freshly visible');
    LSession.SetSourceDraft(LSource + #10 + '''unfinished');
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxSource(LRequest.Source, LSchemas);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscRejected,
      'detached admission failure is presented without publishing');
    Check(LSession.SourceDiagnostic.Defined and (LSession.SourceDiagnostic.Line > 0)
      and LSession.CanRedo and (LSession.Source = LSource),
      'positioned source error retains pair and history');
    LSession.SetSourceDraft(LChanged);
    Check(not LSession.SourceDiagnostic.Defined, 'later typing retires an old diagnostic');
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxSource(LChanged + #10 + '{ mismatched }', LSchemas);
    LFailed := False;
    try
      LSession.CompleteSourceRequest(LRequest, LPrepared);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LSession.Source = LSource), 'wrong exact reply refuses without publication');
    LPrepared := PrepareNyxSource(LChanged, LSchemas);
    RegisterNyxSchema(NyxCustomKind('source-command-epoch-fixture'), [], []);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'later creator publication makes the captured environment stale');
    Check((LSession.Source = LSource) and (LSession.DraftSource = LChanged),
      'schema change retains both current accepted files and the draft');
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxSource(LChanged, LSchemas);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscApplied,
      'a new request admits against the current creator environment');
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    Check(not LRequest.Changed and
      (LSession.CompleteSourceRequest(LRequest, nil) = nscUnchanged),
      'an unchanged source creates no extra publication');
    LSession.SetSourceDraft(StringReplace(LChanged, 'Prepared fixture',
      'Imported fixture', [rfReplaceAll]));
    LRequest := LSession.PrepareSourceRequest(LSchemas.Revision);
    LPrepared := PrepareNyxSource(LRequest.Source, LSchemas);
    LSeed := LSession.ProjectSnapshot;
    LSession.LoadProject(LSeed);
    Check(LSession.CompleteSourceRequest(LRequest, LPrepared) = nscStale,
      'explicit project replacement retires old tickets even with identical files');
    Check(LSession.DraftSource = LSeed.Draft,
      'replacement retains its independently admitted pending draft');
  finally
    LReceived := nil;
    LPrepared := nil;
    LSchemas := nil;
    LOther.Free;
    LSession.Free;
    LDocument.Free;
  end;
end;

end.
