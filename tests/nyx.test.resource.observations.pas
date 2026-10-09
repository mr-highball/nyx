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

unit nyx.test.resource.observations;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Actual ordinary controllers consume an independent real semantic session via
  the maintained suspended editor exchange. It qualifies selection, delayed
  replies, proposal/focus retention and historical capability negotiation, not
  network authentication, trusted input, accessibility or installed rollout. }
procedure RunNyxResourceObservationQualification;

implementation

uses SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.resources, nyx.resources.editor, nyx.resources.browser,
  nyx.resources.runtime.view, nyx.application.resources, nyx.scheduler,
  nyx.resources.workspace,
  nyx.studio.agents, nyx.studio.session, nyx.studio.projects, nyx.studio.workspaces,
  nyx.studio.agentbridge, nyx.studio.exchange, nyx.studio.resourceedits,
  nyx.test.editor.exchange,
  {$ifdef PAS2JS}
  JS, Web, nyx.studio.browser
  {$else}
  Interfaces, Forms, StdCtrls, Controls, nyx.studio.lcl, nyx.test.capture.lcl
  {$endif};

type
  TTestStudio = class({$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif})
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

  { Each owner is explicit. The controller owns its exchange, which only borrows
    Core. Runtime snapshots are immutable and do not own this Studio/document. }
  TJourney = class
  private
    FCore: TNyxAgentSession;
    FStudio: TTestStudio;
    FRuntime: INyxApplicationResources;
    FScheduler: INyxScheduler;
    FObservation: TNyxStudioResourceObservation;
    FBefore: TNyxText;
    FStage: Integer;
    FChecks: Integer;
    FPreludeReplies: Integer;
    FFinished: Boolean;
    FStarted: {$ifdef PAS2JS}Double{$else}QWord{$endif};
    FRetained: {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};
    {$ifndef PAS2JS}
    FWindow: TForm;
    procedure CaptureDetail;
    {$endif}
    procedure Check(AValue: Boolean; const AReason: TNyxText);
    procedure Click(const AID: TNyxText);
    procedure SelectRow(AIndex: Integer);
    function Text(const AID: TNyxText): TNyxText;
    function Visible(const AID: TNyxText): Boolean;
    procedure SetProposal;
    procedure QualifyQueries;
    procedure QualifyLegacy;
    procedure Finish;
  public
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    property Finished: Boolean read FFinished;
  end;

var
  GCore: TNyxAgentSession; { weak factory context, revoked before its owner }
  GExchange: TNyxTestEditorExchange; { borrowed controller-owned transport }
  GJourney: TJourney;

function TTestStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TNyxTestEditorExchange.Create(GCore);
  Result := GExchange;
end;

procedure TJourney.Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Resource observations: ' + AReason);
  end;
  Inc(FChecks);
  {$ifndef PAS2JS}
  WriteLn('Resource observations / ', FChecks, ' / ', AReason);
  Flush(Output);
  {$endif}
end;

procedure TJourney.Click(const AID: TNyxText);
{$ifndef PAS2JS}
var
  LControl: TControl;
{$endif}
begin
  Check(FStudio.ShellView.Root.Find(AID) <> nil, 'mounted command ' + AID);
  {$ifdef PAS2JS}
  TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]')).click;
  {$else}
  LControl := FStudio.ShellView.ControlFor(AID);
  Check(Assigned(LControl.OnClick), 'ordinary callback ' + AID);
  LControl.OnClick(LControl);
  {$endif}
end;

procedure TJourney.SelectRow(AIndex: Integer);
{$ifndef PAS2JS}
var
  LList: TListBox;
{$endif}
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(document.querySelector('[data-node="studio-resource-browser-list"]')
    .querySelectorAll('[data-nyx-item]')[AIndex]).click;
  {$else}
  LList := TListBox(FStudio.ShellView.ControlFor('studio-resource-browser-list'));
  Check(LList.Items.Count > AIndex, 'actual catalog row exists');
  LList.ItemIndex := AIndex;
  LList.OnSelectionChange(LList, True);
  {$endif}
end;

function TJourney.Text(const AID: TNyxText): TNyxText;
begin
  Check(FStudio.ShellView.Root.Find(AID) <> nil, 'mounted status ' + AID);
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]')).textContent;
  {$else}
  Result := TNyxText(RawByteString(TLabel(FStudio.ShellView.ControlFor(AID)).Caption));
  {$endif}
end;

procedure TJourney.SetProposal;
begin
  { Incomplete resource content is presentation, not a design commit. A runtime
    status refresh must preserve this exact input and its physical identity. }
  {$ifdef PAS2JS}
  FRetained := TJSHTMLElement(document.querySelector(
    '[data-node="studio-resource-editor-content"] textarea'));
  Check(FRetained <> nil, 'ordinary content input');
  TJSHTMLTextAreaElement(FRetained).value := 'Unfinished proposal 🌙';
  FRetained.dispatchEvent(TJSEvent.new('input'));
  FRetained.focus;
  {$else}
  FRetained := FStudio.ShellView.InputFor(NyxResourceEditorFieldID(
    'studio-resource-editor', refContent));
  Check(FRetained is TCustomMemo, 'ordinary content input');
  TCustomMemo(FRetained).Text := 'Unfinished proposal 🌙';
  TWinControl(FRetained).SetFocus;
  {$endif}
end;

{$ifndef PAS2JS}
procedure TJourney.CaptureDetail;
var
  LTarget: TControl;
  LAncestor: TWinControl;
begin
  { The workspace owns an independent editor scroll pane. Reveal the actual
    detail through each owning viewport; scrolling only the outer Resources
    container would leave the card below that inner pane's visible allocation. }
  LTarget := FStudio.ShellView.ControlFor('studio-resource-runtime-0-selection');
  LAncestor := LTarget.Parent;
  while LAncestor <> nil do
  begin

    if LAncestor is TScrollingWinControl then
    begin
      TScrollingWinControl(LAncestor).ScrollInView(LTarget);
    end;
    LAncestor := LAncestor.Parent;
  end;
  FWindow.Repaint;
  SaveNyxNativeCapture(FWindow, TNyxText(ParamStr(1)), ncmPrint);
end;
{$endif}

function TJourney.Visible(const AID: TNyxText): Boolean;
begin
  {$ifdef PAS2JS}
  Result := window.getComputedStyle(TJSHTMLElement(document.querySelector(
    '[data-node="' + AID + '"]'))).getPropertyValue('display') <> 'none';
  {$else}
  Result := FStudio.ShellView.ControlFor(AID).Visible;
  {$endif}
end;

procedure TJourney.QualifyQueries;
var
  LReply: TNyxDataValue;
  LMissing: TNyxDataValue;
  LSchema: TNyxDataValue;
  LIndex: Integer;
  LFound: Boolean;
  LRejected: Boolean;
  LItem: TNyxDataValue;
  function ReplaceField(const AItem: TNyxDataValue; const AName: TNyxText;
    const AValue: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LFieldIndex: Integer;
  begin
    SetLength(LFields, AItem.Count);
    for LFieldIndex := 0 to AItem.Count - 1 do
    begin
      LFields[LFieldIndex] := NyxField(AItem.Key(LFieldIndex), AItem.Field(AItem.Key(LFieldIndex)));

      if AItem.Key(LFieldIndex) = AName then
      begin
        LFields[LFieldIndex] := NyxField(AName, AValue);
      end;
    end;
    Result := NyxObject(LFields);
  end;
  procedure RejectDetail(const AName: TNyxText; const AValue: TNyxDataValue);
  var
    LRefused: Boolean;
  begin
    LRefused := False;
    try
      TNyxResourceRuntimeDetail.FromData(ReplaceField(LItem, AName, AValue));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'detail boundary refuses invalid ' + AName);
  end;
  function Query(const AReference, ALocale: TNyxText; ASequence: Integer): TNyxDataValue;
  begin
    Result := FCore.Call('nyx_resources', 'resource qualification', NyxObject([
      NyxField('mode', NyxData('runtime')), NyxField('expectedRevision', NyxData(1)),
      NyxField('run', NyxData('selected-resource-run')),
      NyxField('expectedSequence', NyxData(ASequence)),
      NyxField('reference', NyxData(AReference)), NyxField('locale', NyxData(ALocale))]));
  end;
begin
  LReply := Query('welcome', '', 1).Field('selection');
  Check((LReply.Count = 3) and (LReply.Field('entry').Count = 15) and
    (LReply.Field('entry').Field('locale').AsText = ''),
    'exact bounded semantic read returns one status item without resource bytes');
  Check(TNyxResourceRuntimeDetail.FromData(LReply.Field('entry')).Entry.Reference.Name = 'welcome',
    'strict public detail decodes the trusted real snapshot');
  LItem := LReply.Field('entry').Copy;
  RejectDetail('kind', NyxData('unknown'));
  RejectDetail('phase', NyxData('unknown'));
  RejectDetail('attemptOrigin', NyxNull);
  RejectDetail('publishedOrigin', NyxData('failed'));
  RejectDetail('cacheRead', NyxData('unknown'));
  RejectDetail('policy', NyxObject([]));
  RejectDetail('error', NyxData(TNyxText(StringOfChar('x', 514))));
  Check(LItem.ToJSON = LReply.Field('entry').ToJSON,
    'refused detached candidates preserve the accepted observation');
  LMissing := Query('welcome', 'fr', 1);
  Check(LMissing.Field('selection').Field('entry').Kind = ndNull,
    'missing locale does not substitute default or fallback');
  LMissing := Query('absent', '', 1);
  Check(LMissing.Field('selection').Field('entry').Kind = ndNull,
    'missing reference remains distinct from another declaration');
  LReply := Query('welcome', 'en-US', 1);
  Check(TNyxResourceRuntimeDetail.FromData(LReply.Field('selection').Field('entry')).Entry.Locale.Name =
    'en-US', 'localized exact read retains typed variant identity');
  LRejected := False;
  try
    Query('welcome', '', 2);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'stale sequence refuses before a selected result');
  LRejected := False;
  try
    FCore.Call('nyx_resources', 'resource qualification', NyxObject([
      NyxField('mode', NyxData('runtime')), NyxField('expectedRevision', NyxData(1)),
      NyxField('run', NyxData('selected-resource-run')), NyxField('expectedSequence', NyxData(1)),
      NyxField('reference', NyxData('welcome')), NyxField('limit', NyxData(1))]));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'semantic admission refuses mixed page and exact selection');
  LSchema := NyxResourceAgentSchema.Field('oneOf');
  LFound := False;
  for LIndex := 0 to LSchema.Count - 1 do
  begin

    if LSchema.Item(LIndex).Field('properties').Field('mode').Field('const').AsText = 'runtime' then
    begin
      LFound := Pos('"reference"', LSchema.Item(LIndex).ToJSON) > 0;
      Check(LSchema.Item(LIndex).Field('allOf').Count = 2,
        'public MCP schema separates exact selection from paging and requires reference for locale');
    end;
  end;
  Check(LFound, 'public MCP schema advertises the implemented selector');
  Check(EncodeNyxProject(FCore.ReviewSeed(1)) = FBefore,
    'all exact reads preserve paired source, history and selection');
end;

procedure TJourney.QualifyLegacy;
var
  LSession: TNyxStudioSession;
  LBridge: TNyxStudioAgentBridge;
  LExchange: TNyxTestEditorExchange;
begin
  LSession := TNyxStudioSession.Create(FCore.ReviewSeed(1));
  LExchange := TNyxTestEditorExchange.Create(FCore);
  LExchange.LegacyResourceObservations := True;
  LBridge := TNyxStudioAgentBridge.Create(LSession, nil, NyxPrimaryWorkspace, LExchange);
  try
    LBridge.ObserveResource(NyxResourceRef('welcome'), NyxDefaultLocale, True);
    LBridge.Connect;
    LExchange.Deliver;
    Check(LBridge.State.Connected and not LBridge.State.CanInspectResourceRuntime,
      'historical peer connects without selected-resource capability');
    LExchange.FireTick;
    Check(LExchange.PendingBody.Count = 2, 'historical observation sends no unknown selector');
    LExchange.Deliver;
    Check(LBridge.State.Connected and (LBridge.State.ResourceRuntimes.Count = 1),
      'historical peer keeps ordinary runtime summaries');
  finally
    LBridge.Free;
    LSession.Free;
  end;
end;

procedure TJourney.Start;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  {$ifdef PAS2JS}
  FStarted := TJSDate.now;
  {$else}
  FStarted := GetTickCount64;
  {$endif}
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxColumn('home').Add(
      NewNyxHeading('welcome-heading').WithText('Welcome to your workspace')).Node);
    LDocument.Resources.Define(NyxResourceRef('welcome'), NyxTextResource('Welcome'));
    LDocument.Resources.Define(NyxResourceRef('welcome'), NyxLocale('en-US'),
      NyxTextResource('Howdy'));
    LDocument.Resources.Define(NyxResourceRef('notes'), NyxTextResource('Your notes'));
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    FCore := TNyxAgentSession.Create(LPair);
    FBefore := EncodeNyxProject(FCore.ReviewSeed(1));
    FScheduler := NewNyxScheduler;
    FRuntime := NewNyxApplicationResources(LDocument.Resources, FScheduler,
      NyxApplicationResourceOptions);
    FObservation := FCore.ObserveResourceRuntime(1, NyxStudioRuntime('selected-resource-run'),
      srsApplication, {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif}, '',
      NyxApplicationResourceDiagnostics(FRuntime).CaptureRuntime);
    QualifyQueries;
    GCore := FCore;
    {$ifdef PAS2JS}
    FStudio := TTestStudio.Create;
    FStudio.Run(False);
    FStudio.ConnectAgents;
    {$else}
    FWindow := TForm.CreateNew(nil);
    FWindow.SetBounds(40, 40, 1240, 820);
    FWindow.Show;
    FStudio := TTestStudio.Create(FWindow, '');
    FStudio.Run;
    FStudio.ConnectService('', NyxPrimaryWorkspace);
    {$endif}
    GExchange.Deliver;
  finally
    LDocument.Free;
  end;
  FStage := 1;
  {$ifdef PAS2JS}
  window.setTimeout(@Next, 20);
  {$endif}
end;

procedure TJourney.Next;
var
  LBody: TNyxDataValue;
begin
  {$ifdef PAS2JS}
  try
  {$endif}

    if FFinished then
    begin
      Exit;
    end;

    if {$ifdef PAS2JS}TJSDate.now{$else}GetTickCount64{$endif} - FStarted > 120000 then
    begin
      raise Exception.Create('Selected-resource controller journey timed out at ' + IntToStr(FStage));
    end;

    if FStudio.PresentationPending or FStudio.SourceBusy then
    begin
      {$ifdef PAS2JS}
      window.setTimeout(@Next, 20);
      {$endif}
      Exit;
    end;
    case FStage of
      1:
        begin
          Check(FStudio.ShellView.Root.Find('welcome-heading') = nil,
            'design and editor shell remain separate owned trees');
          {$ifdef PAS2JS}

          if FStudio.ShellView.Root.Find('action-resources-toggle') = nil then
          begin
            { Compact Studio starts on Design. Open its ordinary Project tab
              before using that panel's Resources command, just as a caller
              would. This fixture does not invent a hidden direct editor API. }
            Click('action-panel-project');
            window.setTimeout(@Next, 20);
            Exit;
          end;
          {$endif}
          {$ifndef PAS2JS}
          { Native attachment opens the separate Agents activity panel. Close
            that presentation before qualifying Resources status-only retention;
            activity-list growth/full-frame recovery has its own shared owner. }
          Click('action-agents');
          {$endif}
          Click('action-resources-toggle');
          FStage := 2;
        end;
      2:
        begin
          SelectRow(0);
          FStage := 3;
        end;
      3:
        begin
          Click(NyxResourceBrowserActionID('studio-resource-browser', rbaOpen));
          FStage := 4;
        end;
      4:
        begin
          GExchange.FireTick;
          Check(GExchange.RequestPending, 'ordinary controller schedules selected inspection');
          LBody := GExchange.PendingBody;
          { Ordinary persistence/refresh may already own an earlier summary
            request. The bridge must finish that serialized request; it cannot
            retarget its in-flight bytes when Open selects another variant. }

          if LBody.Count = 2 then
          begin
            Inc(FPreludeReplies);
            Check((LBody.Field('op').AsText = 'observe') and (FPreludeReplies <= 2),
              'finish an already owned summary request before exact inspection');
            GExchange.Deliver;
            {$ifdef PAS2JS}
            window.setTimeout(@Next, 20);
            {$endif}
            Exit;
          end;
          {$ifndef PAS2JS}
          WriteLn('OBSERVER / op=', LBody.Field('op').AsText, ' / fields=', LBody.Count,
            ' / capability=', FStudio.Agents.CanInspectResourceRuntime,
            ' / connected=', FStudio.Agents.Connected, ' / conflict=', FStudio.Agents.Conflict,
            ' / local selection=', FStudio.Session.SelectedID, ' / authoritative selection=',
            FCore.Call('nyx_session', 'resource qualification', NyxObject([])).Field('selection').AsText,
            ' / same pair=',
            EncodeNyxProject(FStudio.Session.ProjectSnapshot) = EncodeNyxProject(FCore.ReviewSeed(1)));
          Flush(Output);
          {$endif}
          Check((LBody.Field('resource').Field('reference').AsText = 'welcome') and
            (LBody.Field('resource').Field('locale').AsText = ''),
            'ordinary controller sends exact default variant at the current revision');
          GExchange.Deliver;
          FStage := 5;
        end;
      5:
        begin
          Check(Text('studio-resource-runtime-0-selection-title') = 'welcome',
            'ordinary selected detail reaches actual target control');
          Check(Text('studio-resource-runtime-0-selection-displayed') =
            'Displayed content: Authored defaults', 'authored content does not masquerade as a loaded publication');
          GExchange.FireTick;
          GExchange.PrepareReply;
          {$ifdef PAS2JS}

          if TJSHTMLElement(document.querySelector('[data-node="' +
            NyxResourceWorkspaceActionID('studio-resource-workspace', rwpFiles) + '"]'))
            .getBoundingClientRect.height > 0 then
          begin
            { Compact Files/Edit panes remain mounted, but hidden catalog rows
              correctly refuse interaction. Navigate through the public pane
              contract before selecting the next row in this delayed-reply test. }
            Click(NyxResourceWorkspaceActionID('studio-resource-workspace', rwpFiles));
            FStage := 51;
            window.setTimeout(@Next, 20);
            Exit;
          end;
          {$endif}
          SelectRow(1);
          FStage := 6;
        end;
      {$ifdef PAS2JS}
      51:
        begin
          SelectRow(1);
          FStage := 6;
        end;
      {$endif}
      6:
        begin
          Click(NyxResourceBrowserActionID('studio-resource-browser', rbaOpen));
          FStage := 7;
        end;
      7:
        begin
          GExchange.Deliver;
          FStage := 8;
        end;
      8:
        begin
          Check(not Visible('studio-resource-runtime-0-selection-attempt') and
            Visible('studio-resource-runtime-0-selection-awaiting'),
            'late default-variant reply cannot paint as the newly selected locale');
          GExchange.FireTick;
          LBody := GExchange.PendingBody;
          Check(LBody.Field('resource').Field('locale').AsText = 'en-US',
            'next bounded observation follows the new locale');
          GExchange.Deliver;
          FStage := 9;
        end;
      9:
        begin
          Check(Text('studio-resource-runtime-0-selection-locale') = 'en-US',
            'locale change refreshes detail without a document revision or activity change');
          SetProposal;
          FCore.RetireResourceRuntime(FObservation);
          GExchange.FireTick;
          GExchange.Deliver;
          FStage := 10;
        end;
      10:
        begin
          Check(Text('studio-resource-runtime-0-retired') = 'Observation retired',
            'runtime retirement refreshes the observing ordinary editor');
          {$ifdef PAS2JS}
          Check(TJSHTMLTextAreaElement(FRetained).value = 'Unfinished proposal 🌙',
            'runtime-only refresh retains incomplete content');
          Check(document.activeElement = FRetained, 'runtime-only refresh retains physical focus');
          Check(FRetained = document.querySelector(
            '[data-node="studio-resource-editor-content"] textarea'),
            'runtime-only refresh retains actual input identity');
          {$else}
          { Validate this borrowed pointer before reading it. A replacement is
            a qualification failure, never permission to dereference a freed
            widget as though it were still owned by the controller. }
          Check(FRetained = FStudio.ShellView.InputFor(NyxResourceEditorFieldID(
            'studio-resource-editor', refContent)), 'runtime-only refresh retains actual input identity');
          Check(TNyxText(RawByteString(TCustomMemo(FRetained).Text)) = TNyxText('Unfinished proposal 🌙'),
            'runtime-only refresh retains incomplete content');
          Check(FWindow.ActiveControl = FRetained, 'runtime-only refresh retains physical focus');
          CaptureDetail;
          {$endif}
          Check(EncodeNyxProject(FCore.ReviewSeed(1)) = FBefore,
            'selection, proposal and runtime refresh create no paired editor history');
          QualifyLegacy;
          {$ifdef PAS2JS}
          TJSHTMLElement(document.querySelector(
            '[data-node="studio-resource-runtime-0-selection"]')).scrollIntoView;
          {$endif}
          Finish;
        end;
    end;
    {$ifdef PAS2JS}

    if not FFinished then
    begin
      window.setTimeout(@Next, 20);
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-resource-observations', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      FFinished := True;
      FreeAndNil(FStudio);
      GExchange := nil;
    end;
  end;
  {$endif}
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-resource-observations', 'passed');
  document.body.setAttribute('data-checks', IntToStr(FChecks));
  {$else}
  WriteLn('PASS / resource observations / ', FChecks);
  {$endif}
end;

destructor TJourney.Destroy;
begin
  FStudio.Free;
  GExchange := nil;
  GCore := nil;
  {$ifndef PAS2JS}
  FWindow.Free;
  {$endif}

  if FRuntime <> nil then
  begin
    FRuntime.Stop;
  end;
  FRuntime := nil;

  if FScheduler <> nil then
  begin
    FScheduler.Shutdown;
  end;
  FScheduler := nil;
  FCore.Free;
  inherited Destroy;
end;

procedure RunNyxResourceObservationQualification;
begin
  GJourney := TJourney.Create;
  {$ifdef PAS2JS}
  GJourney.Start;
  {$else}
  try
    GJourney.Start;
    while not GJourney.Finished do
    begin
      CheckSynchronize(0);
      Application.ProcessMessages;
      GJourney.Next;
      Sleep(1);
    end;
  finally
    FreeAndNil(GJourney);
    CheckSynchronize(0);
    Application.ProcessMessages;
  end;
  {$endif}
end;

end.
