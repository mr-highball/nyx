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

program nyx_build_activity_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}Classes,{$endif}
  nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.studio.builds,
  nyx.studio.buildview;

const
  { Bounded public rows exported by the real maintained semantic/compiler and
    native observing-control consumer. They are presentation inputs, never
    publication or transport authority. Native reads a supplied owned directory;
    the browser loads the identical files beside its compiled fixture. }
  CFiles: array[0..4] of TNyxText = ('callbacks-pending.json',
    'callbacks-committed.json', 'handlers-pending.json',
    'handlers-committed.json', 'handlers-refused.json');
  CLegacyCaption: TNyxText = 'Queued · application · browser';

var
  GInputs: array[0..4] of TNyxDataValue;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  GStep: Integer;
  GDocument: TNyxDocument;
  GRenderer: TNyxBrowserRenderer;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Replace one wire-boundary value in an owning copy. Unchanged fields retain
  their exact values; this cannot mutate the independent real producer input. }
function Changed(const AValue: TNyxDataValue; const AName: TNyxText;
  const AReplacement: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LCount: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  LCount := AValue.Count;
  SetLength(LFields, LCount + 1);
  LFound := False;
  for LIndex := 0 to LCount - 1 do
  begin

    if AValue.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AReplacement);
      LFound := True;
    end
    else
    begin
      LFields[LIndex] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));
    end;
  end;

  if LFound then
  begin
    SetLength(LFields, LCount);
  end
  else
  begin
    LFields[LCount] := NyxField(AName, AReplacement);
  end;
  Result := NyxObject(LFields);
end;

function Envelope(const AItem: TNyxDataValue): TNyxDataValue;
var
  LState: TNyxBuildJobState;
begin
  LState := ParseNyxBuildJobState(AItem.Field('state').AsText);
  Result := NyxObject([NyxField('running', NyxData(Ord(LState = bjsRunning))),
    NyxField('queued', NyxData(Ord(LState = bjsQueued))),
    NyxField('cancelling', NyxData(Ord(LState = bjsCancelling))),
    NyxField('items', NyxArray([AItem]))]);
end;

procedure Refuses(const AItem: TNyxDataValue; const AReason: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    ReadNyxStudioBuildActivity(AItem);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

procedure Qualify;
var
  LIndex: Integer;
  LActivity: TNyxStudioBuildActivity;
  LPending: TNyxDataValue;
  LRefused: TNyxDataValue;
  LLegacy: TNyxDataValue;
  LRoot: TNyxNode;
  LButton: TNyxNode;
  LOriginal: TNyxText;
begin
  for LIndex := 0 to High(GInputs) do
  begin
    LOriginal := GInputs[LIndex].ToJSON;
    LActivity := ReadNyxStudioBuildActivity(GInputs[LIndex]);

    if LIndex < 2 then
    begin
      Check(LActivity.Kind = sbakCallbacks, 'real callback row has its specialized activity kind');
    end
    else
    begin
      Check(LActivity.Kind = sbakHandlers, 'real body row has its specialized activity kind');
    end;

    if LIndex in [1, 3] then
    begin
      Check((LActivity.Publication = sbpCommitted) and
        (LActivity.PublishedRevision = GInputs[LIndex].Field('publication')
          .Field('revision').AsInteger) and (Pos('Applied', LActivity.Caption) = 1),
        'only exact committed construction is presented as applied');
      Check(Pos('one Undo', LActivity.Detail) > 0,
        'applied semantic activity explains paired history');
    end
    else
    begin
      Check((LActivity.PublishedRevision = 0) and
        (Pos('Applied', LActivity.Caption) = 0) and (Pos('Complete', LActivity.Caption) = 0),
        'pending or refused changes never claim active-source success');
    end;
    LRoot := BuildNyxStudioBuildJobs(Envelope(GInputs[LIndex]), True);
    try
      Check(LRoot.Find('studio-build-0-title').Prop('text') = LActivity.Caption,
        'ordinary reusable component consumes the typed activity presentation');
      Check(LRoot.Find('studio-build-0-publication').Prop('text') = LActivity.Detail,
        'ordinary component explains the distinct publication state');
      Check(LRoot.Find('studio-build-0-earlier') = nil,
        'uncommitted source does not produce a misleading earlier-source warning');
      LButton := LRoot.Find('studio-build-0-cancel');
      Check((LButton.Prop('text') = 'Cancel changes') and
        (LButton.Extensions.Value(NyxStudioCancelBuildKey).AsText =
          GInputs[LIndex].Field('job').AsText),
        'cancel identity comes from the exact immutable job rather than its row index');
      Check((LButton.Prop('enabled') = 'true') = GInputs[LIndex].Field('canCancel').AsBoolean,
        'worker cancellation availability survives presentation');
    finally
      LRoot.Free;
    end;
    Check(GInputs[LIndex].ToJSON = LOriginal,
      'presentation does not modify the real semantic producer receipt');
  end;

  { Deliberately model the narrow joined-but-unsealed interval not guaranteed
    to occur in a timer-driven capture. The owning pool tests establish actual
    retention; this boundary test establishes honest wording on both adapters. }
  LPending := Changed(GInputs[0], 'state', NyxData('succeeded'));
  LPending := Changed(LPending, 'canCancel', NyxData(False));
  LActivity := ReadNyxStudioBuildActivity(LPending);
  Check((LActivity.Publication = sbpPending) and
    (Pos('Awaiting publication', LActivity.Caption) = 1),
    'successful construction remains pending until editor publication');
  LRefused := Changed(LPending, 'publication', NyxObject([
    NyxField('state', NyxData('refused')), NyxField('revision', NyxNull)]));
  LActivity := ReadNyxStudioBuildActivity(LRefused);
  Check((LActivity.Publication = sbpRefused) and
    (Pos('Not applied', LActivity.Caption) = 1) and
    (Pos('retained', LActivity.Detail) > 0),
    'successful construction can be refused without claiming applied changes');
  Refuses(Changed(GInputs[1], 'state', NyxData('failed')),
    'committed presentation refuses a failed compiler state');
  Refuses(Changed(GInputs[1], 'publication', NyxObject([
    NyxField('state', NyxData('committed')), NyxField('revision', NyxData(0))])),
    'committed presentation requires a positive exact revision');
  Refuses(Changed(GInputs[1], 'publication', NyxObject([
    NyxField('state', NyxData('future')), NyxField('revision', NyxNull)])),
    'unknown publication choices refuse at the wire boundary');
  Refuses(Changed(GInputs[1], 'operation', NyxData('future')),
    'unknown semantic operations refuse at the wire boundary');
  Refuses(Changed(GInputs[1], 'purpose', NyxData('future')),
    'unknown compiler purposes refuse at the wire boundary');

  LLegacy := NyxObject([NyxField('state', NyxData('queued')),
    NyxField('scope', NyxData('application')), NyxField('target', NyxData('browser'))]);
  LActivity := ReadNyxStudioBuildActivity(LLegacy);
  Check((LActivity.Kind = sbakOutput) and (LActivity.Publication = sbpNone) and
    (LActivity.Caption = CLegacyCaption),
    'older ordinary output metadata remains compatible without semantic fields');
  LActivity := ReadNyxStudioBuildActivity(Changed(GInputs[0], 'operation',
    NyxData('callbacks')));
  Check(LActivity.PublishedRevision = 0,
    'presentation owns no proposed source-location authority');
end;

{$ifdef PAS2JS}
{ Fixture-local identity prefixing uses the public authoring contract while
  the tree still belongs to this caller. Metadata/job identities stay exact. }
procedure PrefixIDs(ANode: TNyxNode; const APrefix: TNyxText);
var
  LIndex: Integer;
begin
  ANode.Named(APrefix + ANode.ID);
  for LIndex := 0 to ANode.Count - 1 do
  begin
    PrefixIDs(ANode.Children[LIndex], APrefix);
  end;
end;

procedure ReleaseView(AEvent: TJSEvent);
begin
  FreeAndNil(GRenderer);
  FreeAndNil(GDocument);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

procedure Present;
var
  LPage: TNyxNode;
  LSection: TNyxNode;
  LPanel: TNyxNode;
  LIndex: Integer;
  LActivity: TNyxStudioBuildActivity;
  LPending: TNyxDataValue;
  LHost: TJSHTMLElement;
begin
  GDocument := TNyxDocument.Create;
  LPage := TNyxNode.Create(nkPage, 'activity-workshop');
  LPage.Configure.Layout(nlColumn).Padding(18).Gap(12).Done;
  GDocument.AddPage(LPage);
  LPage.Add(TNyxNode.Create(nkHeading, 'workshop-title').Configure
    .Text('Following agent changes').Done);
  LPage.Add(TNyxNode.Create(nkLabel, 'workshop-description').Configure
    .Text('Real compiler receipts, with separate editor publication feedback.').Done);
  for LIndex := 0 to High(GInputs) do
  begin
    LSection := TNyxNode.Create(nkColumn, 'receipt-' + IntToStr(LIndex));
    LPanel := BuildNyxStudioBuildJobs(Envelope(GInputs[LIndex]), True);
    { Each borrowed row is composed into an independent node tree. Prefix the
      local composition IDs so this qualification page can show several jobs
      without introducing collisions; production shows one owning panel. }
    PrefixIDs(LPanel, 'receipt-' + IntToStr(LIndex) + '-');
    LSection.Add(LPanel);
    LPage.Add(LSection);
  end;
  LPending := Changed(GInputs[0], 'state', NyxData('succeeded'));
  LPending := Changed(LPending, 'canCancel', NyxData(False));
  LPanel := BuildNyxStudioBuildJobs(Envelope(LPending), True);
  PrefixIDs(LPanel, 'joined-');
  LPage.Add(LPanel);
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.Render(GDocument, LPage, LHost);
  window.addEventListener('pagehide', @ReleaseView);
  for LIndex := 0 to High(GInputs) do
  begin
    LActivity := ReadNyxStudioBuildActivity(GInputs[LIndex]);
    Check(Find('receipt-' + IntToStr(LIndex) + '-studio-build-0-title').textContent =
      LActivity.Caption, 'actual browser paints the exact real receipt caption');
    Check(TJSHTMLButtonElement(Find('receipt-' + IntToStr(LIndex) +
      '-studio-build-0-cancel')).disabled = not GInputs[LIndex].Field('canCancel').AsBoolean,
      'actual browser button follows the real job cancellation state');
  end;
  Check(Pos('Awaiting publication', Find('joined-studio-build-0-title').textContent) = 1,
    'actual browser paints joined construction as awaiting publication');
  Check(TJSHTMLButtonElement(Find('joined-studio-build-0-cancel')).disabled,
    'actual browser offers no worker cancellation after construction has joined');
  document.body.setAttribute('data-nyx-activity', 'passed');
  document.body.setAttribute('data-nyx-activity-checks', IntToStr(GChecks));
  document.body.setAttribute('data-nyx-activity-width', IntToStr(window.innerWidth));
end;

procedure Failed(const AMessage: TNyxText);
begin
  document.body.setAttribute('data-nyx-activity', 'failed');
  document.body.setAttribute('data-nyx-activity-error', AMessage);
end;

procedure LoadNext; forward;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  try
    Check(GRequest.status = 200, 'real bounded compiler receipt loads over HTTP');
    GInputs[GStep] := TNyxDataValue.ParseJSON(GRequest.responseText);
    Inc(GStep);
    LoadNext;
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

function LoadFailed(AEvent: TJSProgressEvent): Boolean;
begin
  Result := False;
  Failed('Bounded activity input could not load');
end;

procedure LoadNext;
begin

  if GStep = Length(CFiles) then
  begin
    Qualify;
    Present;
    Exit;
  end;
  GRequest := TJSXMLHttpRequest.new;
  GRequest.onload := @Loaded;
  GRequest.onerror := @LoadFailed;
  GRequest.open('GET', CFiles[GStep], True);
  GRequest.send;
end;
{$else}
function ReadInput(const APath: TNyxText): TNyxDataValue;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 65536) then
    begin
      raise Exception.Create('Activity input must be bounded public metadata');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := TNyxDataValue.ParseJSON(NyxDecodeUTF8(LBytes));
  finally
    LStream.Free;
  end;
end;

var
  LIndex: Integer;
{$endif}

begin
  {$ifdef PAS2JS}
  LoadNext;
  {$else}

  if ParamCount <> 1 then
  begin
    raise Exception.Create('Supply the owned real-producer public receipt directory');
  end;
  for LIndex := 0 to High(GInputs) do
  begin
    GInputs[LIndex] := ReadInput(IncludeTrailingPathDelimiter(ParamStr(1)) + CFiles[LIndex]);
  end;
  Qualify;
  WriteLn('PASS ', GChecks, ' typed compiler/publication presentation checks');
  {$endif}
end.
