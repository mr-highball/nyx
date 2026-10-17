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

unit nyx.studio.builddiagnostics;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.types, nyx.source, nyx.studio.session,
  nyx.studio.compiler, nyx.studio.builds, nyx.studio.editorbuild;

type
  { Inspection is transient editor state, independently owned by each bridge.
    Loading/Unavailable never mean compilation or source publication succeeded. }
  TNyxBuildDiagnosticPhase = (bdpHidden, bdpLoading, bdpReady, bdpUnavailable);
  TNyxBuildDiagnosticOrigin = (bdoUnknown, bdoAccepted, bdoProposed, bdoEarlier);

  { The authenticated status protocol grants navigation for these one-based
    Unicode scalar coordinates only when they belong to the current source.
    Compiler-pool mapping alone does not grant editor navigation authority.
    Values are returned by copy and retain no job, tree or widget references. }
  TNyxBuildDiagnosticItem = record
    FileName: TNyxText;
    MessageText: TNyxText;
    Severity: TNyxCompilerSeverity;
    Line: Integer;
    Column: Integer;
    Navigable: Boolean;
  end;

  { At most twenty immutable diagnostic values and an optional exact LOCAL
    accepted-source snapshot. It contains no COM interface, making copied browser
    and native presentation records portable. A failed proposal never acquires
    that snapshot; its locations cannot navigate the accepted editor. }
  TNyxBuildDiagnosticInspection = record
  private
    FPhase: TNyxBuildDiagnosticPhase;
    FOrigin: TNyxBuildDiagnosticOrigin;
    FJob: TNyxBuildJobRef;
    FState: TNyxBuildJobState;
    FAwaitingPublication: Boolean;
    FOffset: Integer;
    FTotal: Integer;
    FSource: TNyxText;
    FMessage: TNyxText;
    FItems: array of TNyxBuildDiagnosticItem;
    function GetCount: Integer;
  public
    function Item(AIndex: Integer): TNyxBuildDiagnosticItem;
    property Phase: TNyxBuildDiagnosticPhase read FPhase;
    property Origin: TNyxBuildDiagnosticOrigin read FOrigin;
    property Job: TNyxBuildJobRef read FJob;
    property State: TNyxBuildJobState read FState;
    { Successful compilation can precede a source job's paired publication.
      Keep observing its existing status until that separate phase settles. }
    property AwaitingPublication: Boolean read FAwaitingPublication;
    property Offset: Integer read FOffset;
    property Total: Integer read FTotal;
    property Count: Integer read GetCount;
    property MessageText: TNyxText read FMessage;
  end;

{ Typed pending selection, with no transport/source ownership. Invalid job or
  negative offsets refuse before any caller-visible state changes. }
function NyxBuildDiagnosticsLoading(const AJob: TNyxBuildJobRef;
  AOffset: Integer): TNyxBuildDiagnosticInspection;
{ Admit only this immutable job and requested page. Current-source borrowing
  requires authenticated currentness, the owning bridge's exact acknowledged
  frame and committed semantic publication. Otherwise locations stay read-only.
  A cancelled worker's explicitly empty report retires any selected later page
  to offset zero without creating a synchronization conflict. Other mismatched
  shapes, closed choices, counts or coordinates raise without a result. }
function ReadNyxBuildDiagnostics(const AStatus: TNyxDataValue;
  const AJob: TNyxBuildJobRef; AOffset: Integer;
  const AAcceptedSource: TNyxText; ASourceAcknowledged: Boolean): TNyxBuildDiagnosticInspection;

{ Exact editor-boundary metadata; these keys are not document properties. }
function NyxStudioInspectBuildKey: TNyxExtensionRef;
function NyxStudioInspectOffsetKey: TNyxExtensionRef;
function NyxStudioCloseBuildDiagnosticsKey: TNyxExtensionRef;
function NyxStudioBuildDiagnosticIndexKey: TNyxExtensionRef;

{ Owned ordinary Nyx composition. Borrows session/inspection only during build.
  Source navigation is enabled only for an exact synchronized accepted snapshot;
  view construction never changes selection, draft, source or history. }
function BuildNyxStudioBuildDiagnostics(ASession: TNyxStudioSession;
  const AInspection: TNyxBuildDiagnosticInspection;
  ACanInspect, ASourceSynchronized: Boolean): TNyxNode;
{ Recheck complete local source/draft, synchronization, exact job/page and item
  identity at input time. Old controls cannot navigate a different inspected page.
  Refusal raises ENyxModel without editing source or consuming paired history. }
function RouteNyxBuildDiagnostic(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const AInspection: TNyxBuildDiagnosticInspection;
  ASourceSynchronized: Boolean; out ADiagnostic: TNyxSourceDiagnostic): Boolean;

implementation

uses
  SysUtils, nyx.studio.agents;

const
  CIndexKey: TNyxText = 'studio.build-diagnostic-index';
  CPageKey: TNyxText = 'studio.build-diagnostic-page';
  CItemKey: TNyxText = 'studio.build-diagnostic-item';
  CSeparator: TNyxText = ' · ';

function TNyxBuildDiagnosticInspection.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TNyxBuildDiagnosticInspection.Item(AIndex: Integer): TNyxBuildDiagnosticItem;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxModel.Create('Compiler diagnostic index is outside this inspected page');
  end;
  Result := FItems[AIndex];
end;

{ Exact bounded local row identity. Severity ordering can change while a build
  runs; an older control at the same job/page/index must not navigate a newer
  message. This is editor metadata, never document or transport authority. }
function DiagnosticIdentity(const AItem: TNyxBuildDiagnosticItem): TNyxText;
begin
  Result := NyxObject([NyxField('file', NyxData(AItem.FileName)),
    NyxField('message', NyxData(AItem.MessageText)),
    NyxField('severity', NyxData(Ord(AItem.Severity))),
    NyxField('line', NyxData(AItem.Line)), NyxField('column', NyxData(AItem.Column)),
    NyxField('navigable', NyxData(AItem.Navigable))]).ToJSON;
end;

function NyxBuildDiagnosticsLoading(const AJob: TNyxBuildJobRef;
  AOffset: Integer): TNyxBuildDiagnosticInspection;
begin

  if (AJob.ID = '') or (AOffset < 0) then
  begin
    raise ENyxModel.Create('Compiler inspection requires an exact job and nonnegative offset');
  end;
  Result := Default(TNyxBuildDiagnosticInspection);
  Result.FPhase := bdpLoading;
  Result.FJob := AJob;
  Result.FOffset := AOffset;
  Result.FMessage := 'Reading this job''s diagnostics...';
end;

function ReadNyxBuildDiagnostics(const AStatus: TNyxDataValue;
  const AJob: TNyxBuildJobRef; AOffset: Integer;
  const AAcceptedSource: TNyxText; ASourceAcknowledged: Boolean): TNyxBuildDiagnosticInspection;
var
  LPage: TNyxDataValue;
  LItems: TNyxDataValue;
  LValue: TNyxDataValue;
  LIndex: Integer;
  LSeverity: TNyxCompilerSeverity;
  LFound: Boolean;
  LName: TNyxText;
  LProposed: Boolean;
  LRetiredPage: Boolean;
begin
  Result := NyxBuildDiagnosticsLoading(AJob, AOffset);

  if AStatus.Field('state').AsText = 'rejected' then
  begin
    Result.FPhase := bdpUnavailable;
    Result.FMessage := AStatus.Field('error').AsText;
    Exit;
  end;

  if AStatus.Field('job').AsText <> AJob.ID then
  begin
    raise ENyxModel.Create('Compiler diagnostics belong to a different job');
  end;
  Result.FState := ParseNyxBuildJobState(AStatus.Field('state').AsText);
  LPage := AStatus.Field('diagnostics');
  LItems := LPage.Field('items');
  Result.FOffset := LPage.Field('offset').AsInteger;
  Result.FTotal := LPage.Field('total').AsInteger;
  LRetiredPage := (Result.FState = bjsCancelled) and (LItems.Kind = ndArray) and
    (LItems.Count = 0) and (Result.FTotal = 0) and (Result.FOffset = AOffset);

  if LRetiredPage then
  begin
    Result.FOffset := 0;
  end;

  if (LItems.Kind <> ndArray) or (LItems.Count > 20) or
    ((Result.FOffset <> AOffset) and not LRetiredPage) or (Result.FTotal < 0) or
    (Result.FTotal > 512) or (Result.FOffset > Result.FTotal) or
    (LItems.Count > Result.FTotal - Result.FOffset) then
  begin
    raise ENyxModel.Create('Compiler diagnostics require the exact bounded page');
  end;
  LProposed := False;

  if NyxAgentHas(AStatus, 'purpose') then
  begin

    if AStatus.Field('purpose').AsText <> 'transaction' then
    begin
      raise ENyxModel.Create('Unknown compiler diagnostic purpose');
    end;
    LName := AStatus.Field('publication').Field('state').AsText;

    if (LName <> 'pending') and (LName <> 'committed') and
      (LName <> 'refused') and (LName <> 'reviewed') then
    begin
      raise ENyxModel.Create('Unknown compiler diagnostic publication');
    end;
    LProposed := LName <> 'committed';
    Result.FAwaitingPublication := LName = 'pending';

    if (LName = 'committed') and ((Result.FState <> bjsSucceeded) or
      (AStatus.Field('publication').Field('revision').AsInteger < 1)) then
    begin
      raise ENyxModel.Create('Published diagnostics require successful construction at an exact revision');
    end;
  end;
  Result.FOrigin := bdoEarlier;

  if LProposed then
  begin
    Result.FOrigin := bdoProposed;
  end
  else if ASourceAcknowledged and (AAcceptedSource <> '') and
    NyxAgentHas(AStatus, 'currentSource') and AStatus.Field('currentSource').AsBoolean then
  begin
    Result.FOrigin := bdoAccepted;
    Result.FSource := AAcceptedSource;
  end;
  SetLength(Result.FItems, LItems.Count);

  if (Result.FOffset < Result.FTotal) and (LItems.Count = 0) then
  begin
    raise ENyxModel.Create('Compiler diagnostic pages must make bounded progress');
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LValue := LItems.Item(LIndex);
    { The service deliberately replaces its internal `mapped` field with
      `navigable` after checking currentness/publication. Consume that public
      authority rather than admitting a raw compiler-pool report. }
    NyxAgentFields(LValue, '|file|severity|message|line|column|navigable|');
    Result.FItems[LIndex].FileName := LValue.Field('file').AsText;
    Result.FItems[LIndex].MessageText := LValue.Field('message').AsText;
    LName := LValue.Field('severity').AsText;
    LFound := False;
    for LSeverity := Low(TNyxCompilerSeverity) to High(TNyxCompilerSeverity) do
    begin

      if LowerCase(NyxCompilerSeverityName(LSeverity)) = LName then
      begin
        Result.FItems[LIndex].Severity := LSeverity;
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxModel.Create('Unknown compiler diagnostic severity');
    end;
    Result.FItems[LIndex].Line := LValue.Field('line').AsInteger;
    Result.FItems[LIndex].Column := LValue.Field('column').AsInteger;
    Result.FItems[LIndex].Navigable := LValue.Field('navigable').AsBoolean;

    if (Result.FItems[LIndex].Line < 0) or (Result.FItems[LIndex].Column < 0) or
      (Result.FItems[LIndex].Navigable and ((Result.FItems[LIndex].Line < 1) or
      (Result.FItems[LIndex].Column < 1))) then
    begin
      raise ENyxModel.Create('Compiler diagnostic coordinates are invalid');
    end;
  end;
  Result.FPhase := bdpReady;
  Result.FMessage := AStatus.Field('error').AsText;
end;

function NyxStudioInspectBuildKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.inspect-build');
end;

function NyxStudioInspectOffsetKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.inspect-build-offset');
end;

function NyxStudioCloseBuildDiagnosticsKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.close-build-diagnostics');
end;

function NyxStudioBuildDiagnosticIndexKey: TNyxExtensionRef;
begin
  Result := NyxExtension(CIndexKey);
end;

function CurrentInspection(ASession: TNyxStudioSession;
  const AInspection: TNyxBuildDiagnosticInspection; ASynchronized: Boolean): Boolean;
begin
  Result := (ASession <> nil) and ASynchronized and
    (AInspection.FOrigin = bdoAccepted) and (AInspection.FSource <> '') and
    (ASession.Source = AInspection.FSource) and
    (ASession.DraftSource = AInspection.FSource);
end;

function BuildNyxStudioBuildDiagnostics(ASession: TNyxStudioSession;
  const AInspection: TNyxBuildDiagnosticInspection;
  ACanInspect, ASourceSynchronized: Boolean): TNyxNode;
var
  LIndex: Integer;
  LItem: TNyxBuildDiagnosticItem;
  LMessage: TNyxText;
  LButton: TNyxNode;
  LCanNavigate: Boolean;

  procedure QueryButton(const AID, ACaption: TNyxText; AOffset: Integer);
  begin
    LButton := TNyxNode.Create(nkButton, AID);
    Result.Add(LButton);
    LButton.Configure.Text(ACaption).Enabled(ACanInspect).Done;
    LButton.Extensions.SetValue(NyxStudioInspectBuildKey, NyxData(AInspection.Job.ID));
    LButton.Extensions.SetValue(NyxStudioInspectOffsetKey, NyxData(AOffset));
  end;

begin

  if (ASession = nil) or (AInspection.Phase = bdpHidden) then
  begin
    raise ENyxModel.Create('Compiler diagnostic view requires a session and selected job');
  end;
  Result := TNyxNode.Create(nkCard, 'studio-job-diagnostics');
  try
    Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Surface(True).Done;
    Result.Add(TNyxNode.Create(nkHeading, 'studio-job-diagnostics-title').Configure
      .Text('Compiler diagnostics').Done);
    LButton := TNyxNode.Create(nkButton, 'studio-job-diagnostics-close');
    Result.Add(LButton);
    LButton.Configure.Text('Back to jobs').Done;
    LButton.Extensions.SetValue(NyxStudioCloseBuildDiagnosticsKey, NyxData(True));

    if AInspection.Phase <> bdpReady then
    begin
      Result.Add(TNyxNode.Create(nkLabel, 'studio-job-diagnostics-status').Configure
        .Text(AInspection.MessageText).Done);
      Exit;
    end;
    case AInspection.Origin of
      bdoAccepted:
        begin
          LMessage := 'Diagnostics for accepted Pascal. Navigation requires the unchanged source and draft.';
        end;
      bdoProposed:
        begin
          LMessage := 'Diagnostics for proposed Pascal. Accepted code is retained; these locations cannot navigate it.';
        end;
      bdoEarlier:
        begin
          LMessage := 'Diagnostics for a retained source snapshot. Current code and draft are retained.';
        end;
      bdoUnknown:
        begin
          raise ENyxModel.Create('Ready compiler diagnostics require a known source origin');
        end;
    end;
    Result.Add(TNyxNode.Create(nkLabel, 'studio-job-diagnostics-origin').Configure.Text(LMessage).Done);
    Result.Add(TNyxNode.Create(nkLabel, 'studio-job-diagnostics-summary').Configure
      .Text(NyxBuildJobStateName(AInspection.State) + CSeparator +
        TNyxText(IntToStr(AInspection.Total)) + ' messages').Done);

    if AInspection.MessageText <> '' then
    begin
      Result.Add(TNyxNode.Create(nkLabel, 'studio-job-diagnostics-error').Configure
        .Text(AInspection.MessageText).Done);
    end;
    LCanNavigate := CurrentInspection(ASession, AInspection, ASourceSynchronized);
    for LIndex := 0 to AInspection.Count - 1 do
    begin
      LItem := AInspection.Item(LIndex);
      LMessage := NyxCompilerSeverityName(LItem.Severity) + ' / ' + LItem.FileName;

      if LItem.Line > 0 then
      begin
        LMessage := LMessage + ':' + TNyxText(IntToStr(LItem.Line)) + ':' +
          TNyxText(IntToStr(LItem.Column));
      end;
      LMessage := LMessage + ' / ' + LItem.MessageText;
      Result.Add(TNyxNode.Create(nkLabel, 'studio-job-diagnostic-' + TNyxText(IntToStr(LIndex)))
        .Configure.Text(LMessage).Done);

      if LItem.Navigable then
      begin
        LButton := TNyxNode.Create(nkButton, 'studio-job-diagnostic-go-' +
          TNyxText(IntToStr(LIndex)));
        Result.Add(LButton);
        LButton.Configure.Text('Go to ' + TNyxText(IntToStr(LItem.Line)) + ':' +
          TNyxText(IntToStr(LItem.Column))).Enabled(LCanNavigate).Done;
        LButton.Extensions.SetValue(NyxStudioInspectBuildKey, NyxData(AInspection.Job.ID));
        LButton.Extensions.SetValue(NyxStudioBuildDiagnosticIndexKey, NyxData(LIndex));
        LButton.Extensions.SetValue(NyxExtension(CPageKey), NyxData(AInspection.Offset));
        LButton.Extensions.SetValue(NyxExtension(CItemKey), NyxData(DiagnosticIdentity(LItem)));
      end;
    end;

    if AInspection.Count = 0 then
    begin
      Result.Add(TNyxNode.Create(nkLabel, 'studio-job-diagnostics-empty').Configure
        .Text('No diagnostics in this page. Refresh to check compiler progress.').Done);
    end;
    QueryButton('studio-job-diagnostics-refresh', 'Refresh diagnostics', AInspection.Offset);

    if AInspection.Offset > 0 then
    begin
      LIndex := AInspection.Offset - 20;

      if LIndex < 0 then
      begin
        LIndex := 0;
      end;
      QueryButton('studio-job-diagnostics-previous', 'Previous messages', LIndex);
    end;

    if AInspection.Offset + AInspection.Count < AInspection.Total then
    begin
      QueryButton('studio-job-diagnostics-next', 'More messages',
        AInspection.Offset + AInspection.Count);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function RouteNyxBuildDiagnostic(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const AInspection: TNyxBuildDiagnosticInspection;
  ASourceSynchronized: Boolean; out ADiagnostic: TNyxSourceDiagnostic): Boolean;
var
  LItem: TNyxBuildDiagnosticItem;
begin
  ADiagnostic := Default(TNyxSourceDiagnostic);
  Result := (ATrigger = ntClick) and (ANode <> nil) and
    ANode.Extensions.Has(NyxStudioBuildDiagnosticIndexKey);

  if not Result then
  begin
    Exit;
  end;

  if (AInspection.Phase <> bdpReady) or
    (ANode.Extensions.Value(NyxStudioInspectBuildKey).AsText <> AInspection.Job.ID) or
    (ANode.Extensions.Value(NyxExtension(CPageKey)).AsInteger <> AInspection.Offset) or
    not CurrentInspection(ASession, AInspection, ASourceSynchronized) then
  begin
    raise ENyxModel.Create('This compiler location does not match the unchanged accepted source and inspected page');
  end;
  LItem := AInspection.Item(ANode.Extensions.Value(NyxStudioBuildDiagnosticIndexKey).AsInteger);

  if not ANode.Extensions.Has(NyxExtension(CItemKey)) or
    (ANode.Extensions.Value(NyxExtension(CItemKey)).AsText <> DiagnosticIdentity(LItem)) then
  begin
    raise ENyxModel.Create('This compiler message changed; inspect the current row before navigating');
  end;

  if not LItem.Navigable then
  begin
    raise ENyxModel.Create('This compiler location has no source navigation authority');
  end;
  ADiagnostic.Defined := True;
  ADiagnostic.Line := LItem.Line;
  ADiagnostic.Column := LItem.Column;
  ADiagnostic.Message := NyxCompilerSeverityName(LItem.Severity) + ': ' + LItem.MessageText;
  ASession.ActiveSourceFile := ASession.SourceFile(0);
end;

end.
