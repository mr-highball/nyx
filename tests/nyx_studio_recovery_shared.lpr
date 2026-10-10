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
program nyx_studio_recovery_shared;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.studio.projects, nyx.studio.session, nyx.studio.history, nyx.studio.agents,
  nyx.studio.workspaces, nyx.generated.view
  {$IFDEF PAS2JS}, Web{$ENDIF};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Observe(ASession: TNyxAgentSession): TNyxDataValue;
begin
  Result := ASession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
end;

function TitleRequest(ASession: TNyxAgentSession;
  const ATitle, AOperation: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(ASession.Revision)),
    NyxField('operationId', NyxData(AOperation)), NyxField('operations', NyxArray([
      NyxObject([NyxField('op', NyxData('title')), NyxField('value', NyxData(ATitle))])]))]);
end;

procedure ClearDraft(ASession: TNyxAgentSession);
var
  LPair: TNyxProjectPair;
  LBefore: TNyxDataValue;
begin
  LBefore := Observe(ASession);
  LPair := DecodeNyxProject(LBefore.Field('project').AsText);
  LPair.Pending := False;
  LPair.Draft := '';
  LPair.DraftBase := '';
  ASession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(ASession.Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', LBefore.Field('session').Field('selection')),
    NyxField('view', LBefore.Field('session').Field('view'))]));
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LPrimary: TNyxAgentSession;
  LCopy: TNyxAgentSession;
  LRecovered: TNyxAgentSession;
  LRegistry: TNyxStudioWorkspaces;
  LCopyRegistry: TNyxStudioWorkspaces;
  LRecoveredRegistry: TNyxStudioWorkspaces;
  LLocal: TNyxStudioSession;
  LRecoveredLocal: TNyxStudioSession;
  LFrame: TNyxAgentRecoveryFrame;
  LRegistryFrame: TNyxWorkspaceRecoveryFrame;
  LLocalFrame: TNyxStudioRecoveryFrame;
  LReference: TNyxWorkspaceRef;
  LPair: TNyxProjectPair;
  LBefore: TNyxDataValue;
  LRequest: TNyxDataValue;
  LSeedPacket: TNyxText;
  LRetry: TNyxDataValue;
  LRefused: Boolean;
begin
  LDocument := nil;
  LPrimary := nil;
  LCopy := nil;
  LRecovered := nil;
  LRegistry := nil;
  LCopyRegistry := nil;
  LRecoveredRegistry := nil;
  LLocal := nil;
  LRecoveredLocal := nil;
  try
    LDocument := BuildNyxDocument;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LSeedPacket := EncodeNyxProject(LPair);
    LPrimary := TNyxAgentSession.Create(LPair);
    LRequest := TitleRequest(LPrimary, 'An authored idea 😀', 'title-one');
    LRetry := LPrimary.Call('nyx_transaction', 'Shared recovery', LRequest);
    LPrimary.Call('nyx_transaction', 'Shared recovery',
      TitleRequest(LPrimary, 'A different idea 😀', 'title-two'));
    LPrimary.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LPrimary.Revision)), NyxField('direction', NyxData('undo'))]));
    LBefore := Observe(LPrimary);
    LPair := DecodeNyxProject(LBefore.Field('project').AsText);
    LPair.Pending := True;
    LPair.Draft := TNyxText('An unfinished buffer 😀') + LineEnding;
    LPair.DraftBase := 'An exact stale baseline 😀';
    LPrimary.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(LPrimary.Revision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('workspace')), NyxField('view', NyxData('home'))]));
    LBefore := Observe(LPrimary);
    LRegistry := TNyxStudioWorkspaces.Create(LPrimary, 'shared-recovery');
    LReference := LRegistry.OpenProject('A separate project 😀', LPair);
    LFrame := LPrimary.RecoveryFrame;
    LRegistryFrame := LRegistry.RecoveryFrame;
    LCopy := LPrimary.Clone;
    LCopyRegistry := LRegistry.Clone(LCopy);
    Check(Observe(LCopy).Field('project').AsText = LBefore.Field('project').AsText,
      'Owned rollback copy retains exact Unicode draft/base');
    Check(LCopy.Call('nyx_transaction', 'Shared recovery', LRequest).ToJSON = LRetry.ToJSON,
      'In-memory rollback preserves its immutable delivery receipt');
    LCopy.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LCopy.Revision)), NyxField('direction', NyxData('undo'))]));
    LCopy.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LCopy.Revision)), NyxField('direction', NyxData('redo'))]));
    Check(Observe(LCopy).Field('project').AsText = LBefore.Field('project').AsText,
      'Copied Redo publishes the exact retained files and unfinished buffer');
    Check(Observe(LPrimary).Field('project').AsText = LBefore.Field('project').AsText,
      'Rollback mutation never changes its origin');
    ClearDraft(LCopyRegistry.Find(LReference));
    Check(Observe(LRegistry.Find(LReference)).Field('project').AsText = LBefore.Field('project').AsText,
      'Copied child sessions remain independently owned');
    LRecovered := TNyxAgentSession.CreateRecovered(LFrame);
    LRecoveredRegistry := TNyxStudioWorkspaces.CreateRecovered(LRecovered, LRegistryFrame);
    Check(Observe(LRecovered).Field('project').AsText = LBefore.Field('project').AsText,
      'Recovered primary retains exact accepted pair and stale draft');
    Check(LRecovered.Revision = LPrimary.Revision, 'Recovery preserves semantic revisions');
    Check(Observe(LRecovered).Field('session').Field('selection').AsText = 'workspace',
      'Recovery preserves explicit selected control');
    Check(Observe(LRecoveredRegistry.Find(LReference)).Field('project').AsText = LBefore.Field('project').AsText,
      'Recovery retains ordinary project handles and exact child pair');
    LRefused := False;
    try
      LRecovered.Call('nyx_transaction', 'Shared recovery', LRequest);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Durable recovery expires old transport retry authority');
    { A caller changing its copied vector cannot erase admitted history owners. }
    LFrame.Session.Undo[0] := Default(TNyxStudioCheckpoint);
    LRecovered.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LRecovered.Revision)), NyxField('direction', NyxData('undo'))]));
    LRecovered.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LRecovered.Revision)), NyxField('direction', NyxData('redo'))]));
    LRecovered.Exchange(NyxObject([NyxField('op', NyxData('history')),
      NyxField('expectedRevision', NyxData(LRecovered.Revision)), NyxField('direction', NyxData('undo'))]));
    Check(Observe(LRecovered).Field('project').AsText = LSeedPacket,
      'Recovered paired Undo/Redo execute independently of copied frame arrays');
    LLocal := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LLocal.AddKind('button');
    LLocalFrame := LLocal.RecoveryFrame;
    Check(LLocalFrame.NextID > 0, 'Ordinary designer owns its automatic control serial');
    LRecoveredLocal := TNyxStudioSession.CreateRecovered(LLocalFrame);
    LLocal.AddKind('button');
    LRecoveredLocal.AddKind('button');
    Check(LLocal.Save = LRecoveredLocal.Save, 'Recovery preserves purpose/control naming sequence');
    Check(LLocal.Source = LRecoveredLocal.Source, 'Recovery preserves next ordinary generated source');
  finally
    LRecoveredLocal.Free;
    LLocal.Free;
    LRecoveredRegistry.Free;
    LRecovered.Free;
    LCopyRegistry.Free;
    LCopy.Free;
    LRegistry.Free;
    LPrimary.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' portable recovery ownership checks');
    {$IFDEF PAS2JS}
    document.body.setAttribute('data-nyx-recovery', 'passed');
    document.body.setAttribute('data-nyx-recovery-checks', IntToStr(GChecks));
    {$ENDIF}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-nyx-recovery', 'failed');
      document.body.setAttribute('data-nyx-recovery-error', LException.Message);
      {$ELSE}
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
end.
