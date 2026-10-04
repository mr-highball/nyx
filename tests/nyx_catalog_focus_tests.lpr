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
program nyx_catalog_focus_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.root.types, nyx.contract,
  nyx.catalog, nyx.schema, nyx.behavior, nyx.events, nyx.scheduler,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, LMessages, nyx.render.lcl;{$endif}

type
  TPhaseCounts = array[TNyxTrigger] of Integer;
  { A ledger owns scalar observations, never borrowed controls or interfaces in
    a pas2js record. Runtime nodes are borrowed only while the renderer is live. }
  TFace = record
    Node: TNyxNode;
    First: TPhaseCounts;
    Second: TPhaseCounts;
  end;
  TFocusProbe = class(TNyxEventCallback)
  public
    Ordinal: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TControlAccess = class(TWinControl);
  {$endif}

var
  GDocument: TNyxDocument;
  GCatalog: TNyxCatalog;
  GRenderer: TRenderer;
  GFaces: array of TFace;
  GTokens: array of INyxEventSubscription;
  GFirst: INyxEventCallback;
  GSecond: INyxEventCallback;
  GCase: Integer;
  GPolicy: Integer;
  GChecks: Integer;
  GFaceTotal: Integer;
  GFailure: TNyxText;
  GPageID: TNyxText;
  GKind: TNyxText;
  GSample: TNyxNode;
  GPeerPhase: Integer;
  {$ifdef PAS2JS}GHost: TJSHTMLElement;
  {$else}GHost: TForm;{$endif}

procedure Require(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    GFailure := GKind + ': ' + AReason;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-catalog-result', 'failed');
    document.body.setAttribute('data-catalog-error', GFailure);
    {$endif}
    raise ENyxModel.Create(GFailure);
  end;
  Inc(GChecks);
end;

function ObservedTrigger(ATrigger: TNyxTrigger): Boolean;
begin
  Result := NyxIsKeyboardTrigger(ATrigger) or
    (ATrigger in [ntAfterEnter, ntAfterExit]);
end;

function HasEvent(ANode: TNyxNode; ATrigger: TNyxTrigger): Boolean;
var
  LSchema: TNyxEventSchemas;
  LIndex: Integer;
begin
  Result := False;
  LSchema := NyxEventsMetadata(ANode);
  for LIndex := 0 to High(LSchema) do
  begin

    if LSchema[LIndex].Trigger = ATrigger then
    begin
      Require((LSchema[LIndex].Browser <> ncMissing) and
        (LSchema[LIndex].Native <> ncMissing), 'Published focus/key bridge is supported');
      Exit(True);
    end;
  end;
end;

procedure TFocusProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GFaces) do
  begin

    if GFaces[LIndex].Node.ID = AEvent.OriginID then
    begin

      if AEvent.HasKeyboard and (AEvent.Keyboard.Key = nkTabKey) then
      begin
        { Host Tab is navigation, not the neutral actuation being counted. It
          must retain its platform default while the same event router observes it. }
        Exit;
      end;

      if NyxIsKeyboardTrigger(AEvent.Trigger) then
      begin
        Require(AEvent.HasKeyboard and (AEvent.Keyboard.Key = nkF8Key),
          'Keyboard phase owns the exact physical key');
      end;

      if Ordinal = 1 then
      begin
        Inc(GFaces[LIndex].First[AEvent.Trigger]);
        Require((GPolicy = 3) or
          (GFaces[LIndex].First[AEvent.Trigger] = GFaces[LIndex].Second[AEvent.Trigger] + 1),
          'First independent registration executes before the second');
      end
      else
      begin
        Inc(GFaces[LIndex].Second[AEvent.Trigger]);
        Require(GFaces[LIndex].First[AEvent.Trigger] = GFaces[LIndex].Second[AEvent.Trigger],
          'Second independent registration executes exactly once in order');
      end;
      Exit;
    end;
  end;
  Require(False, 'A retired or unknown origin reached the current probes');
end;

procedure Collect(ANode: TNyxNode);
var
  LIndex: Integer;
  LFace: Integer;
  LTrigger: TNyxTrigger;
  LStream: INyxEventStream;
  {$ifdef PAS2JS}LControl: TJSHTMLElement;{$endif}
begin
  { Compound metadata aggregates descendant routes; a column does not thereby
    acquire a physical focus surface. Inspect every expanded leaf separately. }

  if (ANode.Prop('compound') <> 'true') and HasEvent(ANode, ntKeyDown) then
  begin
    LFace := Length(GFaces);
    SetLength(GFaces, LFace + 1);
    GFaces[LFace].Node := ANode;
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if ObservedTrigger(LTrigger) then
      begin
        Require(HasEvent(ANode, LTrigger), 'Every physical face publishes the complete focus/key family');
        LStream := GRenderer.Events.On(NyxControlEvents(ANode.ID, niRuntime), LTrigger);
        SetLength(GTokens, Length(GTokens) + 2);
        GTokens[Length(GTokens) - 2] := LStream.Subscribe(GFirst);
        GTokens[Length(GTokens) - 1] := LStream.Subscribe(GSecond);
      end;
    end;
    {$ifdef PAS2JS}
    LControl := GRenderer.FocusFor(ANode.ID, niRuntime);
    Require(LControl <> nil, 'Published browser face is exposed by the public adapter contract');
    LControl.setAttribute('data-catalog-face', IntToStr(LFace));
    {$endif}
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Collect(ANode.Children[LIndex]);
  end;
end;

procedure ResetCounts;
var
  LIndex: Integer;
  LTrigger: TNyxTrigger;
begin
  for LIndex := 0 to High(GFaces) do
  begin
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin
      GFaces[LIndex].First[LTrigger] := 0;
      GFaces[LIndex].Second[LTrigger] := 0;
    end;
  end;
end;

procedure ReleaseSubscriptions;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GTokens) do
  begin
    GTokens[LIndex].Cancel;
    Require(not GTokens[LIndex].Active, 'Navigation cancels each independent lease');
    GTokens[LIndex] := nil;
  end;
  SetLength(GTokens, 0);
end;

procedure PrepareCase;
begin
  ReleaseSubscriptions;
  SetLength(GFaces, 0);
  GKind := GCatalog[GCase].Kind;
  GPageID := 'catalog-' + GKind;
  GPolicy := 0;
  Require(GDocument.FindRoot(NyxPageRoot(GPageID)) <> nil,
    'The semantic companion includes this catalog case');
  GRenderer.Render(GDocument, GDocument.FindRoot(NyxPageRoot(GPageID)), GHost);
  { Reusable references receive qualified runtime identities. The semantic
    fixture's before/sample/after placement survives that independent expansion. }
  Require(GRenderer.Root.Count = 3, 'The unchanged compiled sample placement is realized');
  GSample := GRenderer.Root.Children[1];
  Collect(GSample);
  Inc(GFaceTotal, Length(GFaces));
  ResetCounts;
end;

procedure Policy;
var
  LIndex: Integer;
begin
  Inc(GPolicy);
  Require(GPolicy <= 3, 'Each case has one read-only/disabled/re-enabled cycle');
  GSample.Configure.ReadOnly(GPolicy = 1).Enabled(GPolicy <> 2).Done;

  if GPolicy = 3 then
  begin
    { Dropping a lease alone does not remove a callback. Explicit cancellation
      must remove only its registration, leaving its ordered sibling live. }
    for LIndex := 0 to High(GTokens) do
    begin

      if Odd(LIndex) then
      begin
        GTokens[LIndex].Cancel;
        Require(not GTokens[LIndex].Active, 'Only the second registration is cancelled');
      end;
    end;
  end;
  GRenderer.Sync;
  ResetCounts;
end;

procedure CheckFace(AIndex: Integer; ARequireExit: Boolean);
var
  LTrigger: TNyxTrigger;
  LExpected: Integer;
begin
  LExpected := 2;

  if GPolicy = 3 then
  begin
    LExpected := 1;
  end;
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin

    if NyxIsKeyboardTrigger(LTrigger) or (LTrigger = ntAfterEnter) or
      (ARequireExit and (LTrigger = ntAfterExit)) then
    begin
      Require(GFaces[AIndex].First[LTrigger] = 1,
        GFaces[AIndex].Node.ID + ' delivers one ' + NyxTriggerTitle(LTrigger));
      Require(GFaces[AIndex].Second[LTrigger] = LExpected - 1,
        'Independent sibling count agrees with cancellation');
    end;
  end;
end;

procedure PreparePeers;
begin
  ReleaseSubscriptions;
  SetLength(GFaces, 0);
  GKind := 'radio peers';
  GPeerPhase := 0;
  GRenderer.Render(GDocument, GDocument.FindRoot(NyxPageRoot('catalog-radio-peers')), GHost);
  GSample := GRenderer.Root.Find('radio-peers');
  Require(GSample <> nil, 'Radio peer fixture is compiled from the semantic companion');
end;

function PeerEntry: TNyxText;
begin
  Result := 'radio-first';

  if GPeerPhase in [1, 3, 4, 7] then
  begin
    Result := 'radio-middle';
  end;

  if GPeerPhase = 6 then
  begin
    Result := 'radio-after';
  end;
end;

procedure PeerPolicy;
{$ifdef PAS2JS}
const
  PeerIDs: array[0..2] of TNyxText = ('radio-first', 'radio-middle', 'radio-last');
var
  LIndex: Integer;
{$endif}
begin
  Inc(GPeerPhase);
  Require(GPeerPhase <= 8, 'Radio peer policies advance once in the declared sequence');
  GSample.Configure.ReadOnly(GPeerPhase = 4).Enabled(GPeerPhase <> 6);
  GSample.Find('radio-middle').Configure.Value(True).Enabled(GPeerPhase <> 2)
    .Visible(GPeerPhase <> 5);
  {$ifdef PAS2JS}

  if GPeerPhase = 8 then
  begin
    { Explicit HTML interoperability through the borrowed input API. Empty
      names are independent native radio groups; this changes no Nyx design,
      accepted source or portable grouping claim. Qualify host Tab separately. }
    for LIndex := Low(PeerIDs) to High(PeerIDs) do
    begin
      TJSHTMLInputElement(GRenderer.InputFor(PeerIDs[LIndex])).name := '';
    end;
  end;
  {$endif}
  GRenderer.Sync;
end;

{$ifdef PAS2JS}
procedure Publish;
var
  LActive: TJSHTMLElement;
  LRoot: TJSHTMLElement;
  LIndex: Integer;
  LFocus: TNyxText;
begin

  if GFailure <> '' then
  begin
    Exit;
  end;
  LActive := TJSHTMLElement(document.activeElement);
  LRoot := TJSHTMLElement(LActive.closest('[data-runtime-id]'));
  LFocus := '';

  if LRoot <> nil then
  begin
    LFocus := LRoot.getAttribute('data-runtime-id');
  end;
  document.body.setAttribute('data-catalog-focus', LFocus);
  document.body.setAttribute('data-catalog-index', IntToStr(GCase));
  document.body.setAttribute('data-catalog-total', IntToStr(GCatalog.Count));
  document.body.setAttribute('data-catalog-kind', GKind);
  document.body.setAttribute('data-catalog-policy', IntToStr(GPolicy));
  document.body.setAttribute('data-catalog-face-count', IntToStr(Length(GFaces)));
  document.body.setAttribute('data-catalog-before', GPageID + '-before');
  document.body.setAttribute('data-catalog-after', GPageID + '-after');
  for LIndex := 0 to High(GFaces) do
  begin
    document.body.setAttribute('data-catalog-face-' + IntToStr(LIndex), GFaces[LIndex].Node.ID);
    document.body.setAttribute('data-catalog-projection-' + IntToStr(LIndex),
      GFaces[LIndex].Node.ProjectionKind);
  end;
  document.body.setAttribute('data-catalog-checks', IntToStr(GChecks));
  document.body.setAttribute('data-catalog-radio-phase', IntToStr(GPeerPhase));
  document.body.setAttribute('data-catalog-radio-entry', PeerEntry);
end;

function NextCase(AEvent: TJSMouseEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  try

    if GCase = GCatalog.Count then
    begin
      Require(GPeerPhase = 8, 'Radio peer policy and HTML interoperability journey completed before disposal');
      GRenderer.Unmount;
      document.body.setAttribute('data-catalog-result', 'passed');
      Publish;
      Exit;
    end;
    Require(GPolicy = 3, 'The host completed all policy transitions before navigation');
    for LIndex := 0 to High(GFaces) do
    begin
      CheckFace(LIndex, True);
    end;
    Inc(GCase);

    if GCase = GCatalog.Count then
    begin
      PreparePeers;
      document.body.setAttribute('data-catalog-result', 'radio');
      document.body.setAttribute('data-catalog-face-total', IntToStr(GFaceTotal));
    end
    else
    begin
      PrepareCase;
    end;
    Publish;
  except
    on LException: Exception do
    begin
      Require(False, LException.Message);
    end;
  end;
end;

function ChangePolicy(AEvent: TJSMouseEvent): Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  try

    if GCase = GCatalog.Count then
    begin
      PeerPolicy;
      Publish;
      Exit;
    end;

    if GPolicy <> 2 then
    begin
      for LIndex := 0 to High(GFaces) do
      begin
        CheckFace(LIndex, True);
      end;
    end;
    Policy;
    Publish;
  except
    on LException: Exception do
    begin
      Require(False, LException.Message);
    end;
  end;
end;
{$else}
function FaceControl(AIndex: Integer): TWinControl;
var
  LControl: TControl;
begin
  LControl := GRenderer.FocusFor(GFaces[AIndex].Node.ID, niRuntime);
  Require(LControl is TWinControl, 'Advertised native keyboard face is a window control');
  Result := TWinControl(LControl);
end;

procedure NativeCycle;
var
  LIndex: Integer;
  LControl: TWinControl;
  LBefore: TWinControl;
  LAfter: TWinControl;
begin
  LBefore := TWinControl(GRenderer.ControlFor(GPageID + '-before'));
  LAfter := TWinControl(GRenderer.ControlFor(GPageID + '-after'));
  LBefore.SetFocus;
  ResetCounts;
  for LIndex := 0 to High(GFaces) do
  begin
    LControl := FaceControl(LIndex);

    if GPolicy = 2 then
    begin
      Require(not LControl.CanFocus, 'Inherited disabled native face cannot receive focus');
      Continue;
    end;
    Require(LControl.CanFocus and LControl.TabStop, 'Advertised native face is keyboard reachable');
    LControl.SetFocus;
    Require(LControl.Focused, 'The actual native face owns focus');
    { LCL control messages exercise its installed adapter slots. This is native
      widget evidence, not a claim about a physical keyboard or another widgetset. }
    LControl.Perform(CN_KEYDOWN, $77, 0);
    LControl.Perform(CN_KEYUP, $77, 0);
    CheckFace(LIndex, False);
  end;
  LAfter.SetFocus;

  if GPolicy <> 2 then
  begin
    for LIndex := 0 to High(GFaces) do
    begin
      CheckFace(LIndex, True);
    end;
  end;
end;

procedure NativePeers;
const
  PeerIDs: array[0..2] of TNyxText = ('radio-first', 'radio-middle', 'radio-last');
var
  LIndex: Integer;
  LPhase: Integer;
  LControl: TWinControl;
  LEntry: TNyxText;
  LEntries: Integer;
begin
  PreparePeers;
  for LPhase := 0 to 7 do
  begin
    LEntry := PeerEntry;
    LEntries := 0;
    for LIndex := Low(PeerIDs) to High(PeerIDs) do
    begin
      LControl := GRenderer.FocusFor(PeerIDs[LIndex]);
      Require(LControl <> nil, 'Every radio peer exposes its borrowed focus face');
      Require(LControl.TabStop = (PeerIDs[LIndex] = LEntry),
        'Native radio group retains exactly its declared single entry');

      if LControl.TabStop then
      begin
        Inc(LEntries);
        Require(LControl.CanFocus, 'Chosen radio entry remains reachable');
        LControl.SetFocus;
        Require(LControl.Focused, 'Chosen radio peer owns actual native focus');
      end;
    end;
    Require(((LPhase = 6) and (LEntries = 0)) or
      ((LPhase <> 6) and (LEntries = 1)), 'Disabled radio group has no remaining Tab entry');

    if LPhase < 7 then
    begin
      PeerPolicy;
    end;
  end;
  WriteLn('PASS native radio peer entry / unchecked, checked, disabled, re-enabled, read-only and hidden');
end;
{$endif}

var
  LProbe: TFocusProbe;
begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GDocument := BuildNyxDocument;
    GCatalog := TNyxCatalog.Create;
    GRenderer := TRenderer.Create;
    LProbe := TFocusProbe.Create;
    LProbe.Ordinal := 1;
    GFirst := LProbe;
    LProbe := TFocusProbe.Create;
    LProbe.Ordinal := 2;
    GSecond := LProbe;
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.getElementById('catalog-host'));

    if window.location.search = '?peers=1' then
    begin
      GCase := GCatalog.Count;
      PreparePeers;
    end
    else
    begin
      PrepareCase;
    end;
    TJSHTMLElement(document.getElementById('catalog-next')).onclick := @NextCase;
    TJSHTMLElement(document.getElementById('catalog-policy')).onclick := @ChangePolicy;
    window.setInterval(@Publish, 10);
    Publish;

    if GCase = GCatalog.Count then
    begin
      document.body.setAttribute('data-catalog-result', 'radio');
    end
    else
    begin
      document.body.setAttribute('data-catalog-result', 'ready');
    end;
    {$else}
    GHost := TForm.CreateNew(nil);
    GHost.SetBounds(0, 0, 1000, 1000);
    GHost.Show;
    try
      for GCase := 0 to GCatalog.Count - 1 do
      begin
        PrepareCase;
        Application.ProcessMessages;
        NativeCycle;
        Policy;
        NativeCycle;
        Policy;
        NativeCycle;
        Policy;
        NativeCycle;
        WriteLn('PASS ', GKind, ' / ', Length(GFaces), ' native focus/key faces');
      end;
      ReleaseSubscriptions;
      NativePeers;
    finally
      GRenderer.Free;
      GFirst := nil;
      GSecond := nil;
      GHost.Free;
      GCatalog.Free;
      GDocument.Free;
    end;
    WriteLn('PASS ', GChecks, ' native catalog focus checks / ', GFaceTotal, ' faces');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-catalog-result', 'failed');
      document.body.setAttribute('data-catalog-error', LException.Message);
      {$else}
      WriteLn(StdErr, 'FAIL ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
