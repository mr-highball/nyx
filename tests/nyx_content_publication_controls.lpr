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
program nyx_content_publication_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  {$ifndef PAS2JS}Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Classes, Windows,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state,
  nyx.presentations, nyx.editing, nyx.events, nyx.scheduler, nyx.callbacks, nyx.behavior,
  nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.editing.browser, nyx.render.browser
  {$else}, nyx.gestures.lcl, nyx.render.lcl{$endif};

type
  TPublicationFault = (pfNone, pfPhysicalSync, pfFocus);
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  THost = TJSHTMLElement;
  { The matched RTL's base constructor omits options. Admit typed host
    notification options in this browser adapter fixture only. }
  TPointerNotification = class external name 'PointerEvent' (TJSPointerEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  THost = TForm;
  TAccess = class(TControl);
  { An actual native focus call fails after the candidate is shown and its
    observation hooks installed. The old input uses its ordinary LCL class. }
  TFocusMemo = class(TMemo)
  public
    procedure SetFocus; override;
  end;
  {$endif}

  { Managed callbacks retain owned snapshots and counters, never the renderer
    or its controls. This verifies the prepared observers' committed revision. }
  TProbe = class(TNyxEventCallback)
  public
    Counts: array[TNyxTrigger] of Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

  { Explicit typed instrumentation over the unchanged generated companion.
    This is a real adapter consumer, not an MCP-authored replacement project. }
  TReview = class
  private
    FRenderer: TRenderer;
    FHost: THost;
    FStore: TNyxState;
    FLease: INyxPresentationView;
    FProbe: TProbe;
    FProbeOwner: INyxEventCallback;
    FSubscriptions: array of INyxEventSubscription;
    FAccepted: TNyxNode;
    FFormerInput: TFace;
    FWideID: TNyxText;
    FCompactID: TNyxText;
    FSelection: TNyxTextSelection;
    FEventRevision: Integer;
    FStateRevision: Integer;
    FStage: Integer;
    FPolls: Integer;
    procedure Setup;
    function Ready(ACondition: Boolean): Boolean;
    procedure CheckRecovery(const AReason: TNyxText);
    procedure Request(AFault: TPublicationFault);
    procedure SelectRange(const AID: TNyxText; AStart, AFinish: Integer);
    { Reuse the public design-selection path, including actual native paint
      strips. No private renderer fields or portable synthetic events are used. }
    function DesignerSelectionVisible: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Step: Boolean;
  end;

const
  CDraft: TNyxText = 'A careful draft 🌙 with room to create.';

var
  GReview: TReview;
  GChecks: Integer;
  GFault: TPublicationFault;
  GSyncFailures: Integer;
  GFocusFailures: Integer;
  {$ifdef PAS2JS}
  { Borrowed only while the one memo factory/focus call is under review. Clear
    these explicit test references at teardown; no product callback retains them. }
  GFocusFace: TJSHTMLElement;
  GNativeFocus: TJSFunction;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Counts[AEvent.Trigger]);
end;

{$ifdef PAS2JS}
procedure AttemptFocus(AOptions: TJSObject);
begin

  if GFault = pfFocus then
  begin
    Inc(GFocusFailures);
    raise Exception.Create('Controlled physical focus failure');
  end;
  GNativeFocus.call(GFocusFace, AOptions);
end;
{$else}
procedure TFocusMemo.SetFocus;
begin

  if (GFault = pfFocus) and Showing then
  begin
    Inc(GFocusFailures);
    raise Exception.Create('Controlled physical focus failure');
  end;
  inherited SetFocus;
end;
{$endif}

function NewReviewMemo(ANode: TNyxNode
  {$ifndef PAS2JS}; AOwner: TComponent{$endif}): TFace;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('textarea'));
  GFocusFace := Result;
  GNativeFocus := TJSFunction(TJSObject(Result)['focus']);
  TJSObject(Result)['focus'] := @AttemptFocus;
  {$else}
  Result := TFocusMemo.Create(AOwner);
  TMemo(Result).ScrollBars := ssVertical;
  {$endif}
end;

procedure UpdateReviewMemo(ANode: TNyxNode; AFace: TFace);
var
  LShown: Boolean;
begin
  { The probe host is hidden; this fault specifically exercises synchronization
    in the visible physical host, after factories and allocation have succeeded.
    These raw values are confined to the explicit renderer extension boundary. }
  {$ifdef PAS2JS}
  LShown := document.body.contains(AFace) and
    (window.getComputedStyle(AFace).getPropertyValue('visibility') <> 'hidden');
  {$else}
  LShown := TWinControl(AFace).Showing;
  {$endif}

  if (GFault = pfPhysicalSync) and LShown then
  begin
    Inc(GSyncFailures);
    raise Exception.Create('Controlled physical synchronization failure');
  end;
  {$ifdef PAS2JS}TJSHTMLTextAreaElement(AFace).value := ANode.Prop('value');
  {$else}TMemo(AFace).Text := ANode.Prop('value');{$endif}
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := NyxBrowserInputText(AFace);
  {$else}Result := TCustomEdit(AFace).Text;{$endif}
end;

function Focused(AFace: TFace): Boolean;
begin
  {$ifdef PAS2JS}Result := document.activeElement = AFace;
  {$else}Result := Screen.ActiveControl = AFace;{$endif}
end;

constructor TReview.Create;
begin
  inherited Create;
  FRenderer := TRenderer.Create;
  FProbe := TProbe.Create;
  FProbeOwner := FProbe;
end;

destructor TReview.Destroy;
var
  LIndex: Integer;
begin
  GFault := pfNone;
  for LIndex := 0 to High(FSubscriptions) do
  begin
    FSubscriptions[LIndex] := nil;
  end;
  FRenderer.Free;
  FProbeOwner := nil;
  FStore.Free;
  {$ifdef PAS2JS}
  GFocusFace := nil;
  GNativeFocus := nil;

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}
  FHost.Free;
  {$endif}
  inherited Destroy;
end;

procedure TReview.SelectRange(const AID: TNyxText; AStart, AFinish: Integer);
begin
  FRenderer.SetTextSelection(AID, NyxTextSelection(
    TextOf(FRenderer.InputFor(AID)), AStart, AFinish, ntdUnknown));
  {$ifdef PAS2JS}FRenderer.InputFor(AID).dispatchEvent(TJSEvent.new('select'));
  {$else}Application.Idle(False);{$endif}
end;

procedure TReview.Setup;
var
  LDocument: TNyxDocument;
  LCompact: TNyxText;
begin
  LDocument := BuildNyxDocument;
  try
    LCompact := LDocument.Find('workspace').Content.Rule(1).Component.Name;
    LDocument.Find('wide-name').Configure.PartName(NyxPart('notes')).Done;
    LDocument.Find('compact-notes').Configure.PartName(NyxPart('notes')).Done;
    LDocument.Find('wide-form').Add(NewNyxButton('wide-action').WithText('Keep creating'));
    LDocument.Find(LCompact).Add(NewNyxButton('compact-action').WithText('Save idea'));
    FStore := LDocument.State.Clone;
    {$ifdef PAS2JS}
    FHost := TJSHTMLElement(document.createElement('div'));
    FHost.style.setProperty('width', '900px');
    FHost.style.setProperty('height', '700px');
    document.body.appendChild(FHost);
    {$else}
    FHost := TForm.Create(nil);
    FHost.ClientWidth := 900;
    FHost.ClientHeight := 700;
    FHost.Show;
    FHost.BringToFront;
    {$endif}
    FRenderer.RegisterFactory(NyxKindName(nkMemo), @NewReviewMemo, @UpdateReviewMemo);
    FRenderer.Render(LDocument, LDocument.Pages[0], FHost, False, FStore);
  finally
    LDocument.Free;
  end;
  FWideID := NyxQualifiedID('workspace', 'wide-name');
  FCompactID := NyxQualifiedID('workspace', 'compact-notes');
  FLease := FRenderer.Presentations;
  FAccepted := FRenderer.Root;
  FFormerInput := FRenderer.InputFor(FWideID);
  SetLength(FSubscriptions, 6);
  FSubscriptions[0] := FRenderer.Events.OnTextSelectionChange(NyxControlEvents(FWideID, niRuntime)).Subscribe(FProbeOwner);
  FSubscriptions[1] := FRenderer.Events.OnTextSelectionChange(NyxControlEvents(FCompactID, niRuntime)).Subscribe(FProbeOwner);
  FSubscriptions[2] := FRenderer.Events.On(NyxControlEvents(NyxQualifiedID('workspace', 'wide-action'), niRuntime), ntClick).Subscribe(FProbeOwner);
  FSubscriptions[3] := FRenderer.Events.On(NyxControlEvents(NyxQualifiedID('workspace', 'compact-action'), niRuntime), ntClick).Subscribe(FProbeOwner);
  FSubscriptions[4] := FRenderer.Events.OnScroll(NyxControlEvents(FCompactID, niRuntime)).Subscribe(FProbeOwner);
  FSubscriptions[5] := FRenderer.Events.OnPointerCaptureLost(NyxControlEvents(FCompactID, niRuntime)).Subscribe(FProbeOwner);
  {$ifdef PAS2JS}
  TJSHTMLInputElement(FFormerInput).value := CDraft;
  FFormerInput.focus;
  {$else}
  TCustomEdit(FFormerInput).Text := CDraft;
  TWinControl(FFormerInput).SetFocus;
  {$endif}
  SelectRange(FWideID, 2, 7);
  FSelection := FRenderer.TextSelectionFor(FWideID);
  FEventRevision := FRenderer.Events.ViewRevision;
  FStateRevision := FStore.Revision;
  Request(pfPhysicalSync);
end;

function TReview.Ready(ACondition: Boolean): Boolean;
begin
  Result := ACondition;

  if not Result then
  begin
    Inc(FPolls);

    if FPolls > 100 then
    begin
      raise Exception.Create('Publication review timed out / ' + FRenderer.LastContentError);
    end;
  end
  else
  begin
    FPolls := 0;
  end;
end;

procedure TReview.Request(AFault: TPublicationFault);
begin
  GFault := AFault;
  FLease.Select(NyxPresentation('focused'));
  Check(FRenderer.Root = FAccepted, 'A request preserves the accepted root until a queue turn');
end;

procedure TReview.CheckRecovery(const AReason: TNyxText);
var
  LBefore: Integer;
begin
  Check(FRenderer.Root = FAccepted, AReason + ': exact old model remains accepted');
  Check(FRenderer.InputFor(FWideID) = FFormerInput, AReason + ': exact old input remains mounted');
  Check(TextOf(FFormerInput) = CDraft, AReason + ': exact supplementary draft survives');
  Check(Focused(FFormerInput), AReason + ': old focus is restored');
  Check(FRenderer.TextSelectionFor(FWideID).SameRange(FSelection),
    AReason + ': old scalar selection is restored');
  Check(FStore.Revision = FStateRevision, AReason + ': no state defaults or edits are imported');
  Check(FRenderer.Events.ViewRevision = FEventRevision, AReason + ': router epoch stays accepted');
  Check(FLease.Connected and (FRenderer.Presentations = FLease) and
    not FLease.Selection.Reference.Defined, AReason + ': original capability and choice remain');
  LBefore := FProbe.Counts[ntClick];
  {$ifdef PAS2JS}
  FRenderer.ElementFor(NyxQualifiedID('workspace', 'wide-action')).click;
  Check(document.querySelector('[inert][data-nyx-theme]') = nil,
    AReason + ': discarded hidden candidate is removed');
  {$else}
  TAccess(FRenderer.ControlFor(NyxQualifiedID('workspace', 'wide-action'))).Click;
  Check(FHost.ControlCount = 1, AReason + ': discarded visible candidate and its hooks are removed');
  {$endif}
  Check(FProbe.Counts[ntClick] = LBefore + 1, AReason + ': old callbacks still dispatch exactly once / ' +
    IntToStr(LBefore) + TNyxText(' -> ') + IntToStr(FProbe.Counts[ntClick]) +
    TNyxText(' / ') + FRenderer.LastBindingError);
  LBefore := FProbe.Counts[ntTextSelectionChange];
  SelectRange(FWideID, 1, 4);
  Check(FProbe.Counts[ntTextSelectionChange] = LBefore + 1,
    AReason + ': old editing observer remains connected');
  SelectRange(FWideID, FSelection.Start, FSelection.Finish);
end;

function TReview.Step: Boolean;
var
  LBefore: Integer;
  LText: TNyxText;
  LLine: Integer;
  LFace: TFace;
  LDocument: TNyxDocument;
  {$ifdef PAS2JS}LOptions: TJSObject;{$endif}
begin
  Result := False;
  case FStage of
    0: Setup;
    1:
      begin

        if not Ready(FRenderer.LastContentError <> '') then
        begin
          Exit;
        end;
        Check(GSyncFailures = 1, 'The failure happens once in visible physical synchronization');
        CheckRecovery('Physical synchronization failure');
        Request(pfFocus);
      end;
    2:
      begin

        if not Ready(GFocusFailures > 0) then
        begin
          Exit;
        end;
        Check(GFocusFailures = 1, 'The failure happens once during actual candidate focus');
        CheckRecovery('Physical focus failure');
        Request(pfNone);
      end;
    3:
      begin

        if not Ready(FRenderer.Root <> FAccepted) then
        begin
          Exit;
        end;
        Check(FRenderer.LastContentError = '', 'A successful retry clears its admission error');
        Check(FLease.Connected and (FRenderer.Presentations = FLease) and
          (FLease.Selection.Reference.Name = 'focused'), 'Successful retry retains and publishes the original capability');
        Check(FRenderer.Events.ViewRevision <> FEventRevision, 'Successful commit advances its router epoch');
        LFace := FRenderer.InputFor(FCompactID);
        Check(TextOf(LFace) = CDraft, 'The new compatible input keeps its exact logical draft');
        Check(Focused(LFace), 'The new compatible input receives actual focus');
        Check(FRenderer.TextSelectionFor(FCompactID).SameRange(FSelection),
          'The new compatible input keeps scalar selection');
        Check(FStore.Revision = FStateRevision, 'Successful admission imports no source defaults');
        LBefore := FProbe.Counts[ntTextSelectionChange];
        SelectRange(FCompactID, 0, 3);
        Check(FProbe.Counts[ntTextSelectionChange] = LBefore + 1,
          'Prepared editing hooks dispatch at the committed epoch');
        LBefore := FProbe.Counts[ntClick];
        {$ifdef PAS2JS}FRenderer.ElementFor(NyxQualifiedID('workspace', 'compact-action')).click;
        {$else}TAccess(FRenderer.ControlFor(NyxQualifiedID('workspace', 'compact-action'))).Click;{$endif}
        Check(FProbe.Counts[ntClick] = LBefore + 1, 'New control callbacks dispatch once after commit');
        { Exercise real target scroll/capture observations rather than dispatching
          a portable event directly. Browser capture here is a host notification;
          this does not claim hardware pointer or assistive-input qualification. }
        LText := '';
        for LLine := 1 to 60 do
        begin
          LText := LText + 'A line to review.' + #10;
        end;
        FStore.SetValue(NyxTextState('notes'), LText);
        LBefore := FProbe.Counts[ntScroll];
        {$ifdef PAS2JS}
        LFace.scrollTop := 80;
        LFace.dispatchEvent(TJSEvent.new('scroll'));
        {$else}
        Windows.SendMessageW(TWinControl(LFace).Handle, EM_LINESCROLL, 0, 5);
        Application.Idle(False);
        {$endif}
        Check(FProbe.Counts[ntScroll] > LBefore, 'Prepared viewport observation dispatches after commit');
        LBefore := FProbe.Counts[ntPointerCaptureLost];
        {$ifdef PAS2JS}
        LOptions := TJSObject.new;
        LOptions['pointerId'] := 1;
        LFace.dispatchEvent(TPointerNotification.new('gotpointercapture', LOptions));
        LFace.dispatchEvent(TPointerNotification.new('lostpointercapture', LOptions));
        {$else}
        CaptureNyxLCLPointer(LFace);
        Application.Idle(False);
        ReleaseNyxLCLPointer(LFace);
        Application.Idle(False);
        {$endif}
        Check(FProbe.Counts[ntPointerCaptureLost] = LBefore + 1,
          'Prepared capture hooks dispatch once after commit');
        { The wide recipe contains a single-line field. Restore a value its
          declared domain accepts before requesting the reverse publication. }
        FStore.SetValue(NyxTextState('notes'), CDraft);
        FLease.Automatic;
      end;
    4:
      begin

        if not Ready(FRenderer.Root.Find(FWideID) <> nil) then
        begin
          Exit;
        end;
        Check(not FLease.Selection.Reference.Defined, 'Reverse publication restores automatic mode');
        Check(TextOf(FRenderer.InputFor(FWideID)) = FStore.GetValue(NyxTextState('notes')),
          'Reverse publication preserves the runtime store');
        FreeAndNil(FRenderer);
        Check(not FLease.Connected, 'Explicit teardown retires the stable capability');
        { A second independently owned mount exercises designer selection.
          Its source remains the exact compiled companion, with a custom target
          factory used solely to inject a physical publication failure. }
        LDocument := BuildNyxDocument;
        try
          FRenderer := TRenderer.Create;
          FRenderer.RegisterFactory(NyxKindName(nkMemo), @NewReviewMemo, @UpdateReviewMemo);
          FRenderer.Render(LDocument, LDocument.Pages[0], FHost, True, FStore);
        finally
          LDocument.Free;
        end;
        FLease := FRenderer.Presentations;
        FAccepted := FRenderer.Root;
        FRenderer.Select('workspace');
        Check(DesignerSelectionVisible, 'The initial designer has actual visible selection paint');
        Request(pfPhysicalSync);
      end;
    5:
      begin

        if not Ready(FRenderer.LastContentError <> '') then
        begin
          Exit;
        end;
        Check(FRenderer.Root = FAccepted, 'Failed designer publication preserves its exact accepted root');
        Check(DesignerSelectionVisible, 'Failed designer publication preserves visible selection paint');
        Check(FLease.Connected and not FLease.Selection.Reference.Defined,
          'Failed designer publication preserves its capability and automatic choice');
        Request(pfNone);
      end;
    6:
      begin

        if not Ready(FRenderer.Root <> FAccepted) then
        begin
          Exit;
        end;
        Check(DesignerSelectionVisible, 'Successful designer publication transfers prepared selection paint');
        Check(FLease.Selection.Reference.Name = 'focused', 'Designer publication admits its manual choice');
        FreeAndNil(FRenderer);
        Check(not FLease.Connected, 'Designer teardown retires its original capability');
        Result := True;
      end;
  end;
  Inc(FStage);
end;

function TReview.DesignerSelectionVisible: Boolean;
{$ifndef PAS2JS}
var
  LStrips: Integer;

  procedure CountStrips(AControl: TControl);
  var
    LIndex: Integer;
  begin

    if (AControl is TShape) and AControl.Visible then
    begin
      Inc(LStrips);
    end;

    if AControl is TWinControl then
    begin
      for LIndex := 0 to TWinControl(AControl).ControlCount - 1 do
      begin
        CountStrips(TWinControl(AControl).Controls[LIndex]);
      end;
    end;
  end;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := FRenderer.ElementFor('workspace').classList.contains('nyx-selected');
  {$else}
  LStrips := 0;
  CountStrips(FHost);
  Result := LStrips = 4;
  {$endif}
end;

procedure Drive;
begin
  try

    if GReview.Step then
    begin
      FreeAndNil(GReview);
      WriteLn('PASS ', GChecks, ' actual reversible publication checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'passed');
      document.body.setAttribute('data-projection-refresh-checks', IntToStr(GChecks));
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LError.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GReview := TReview.Create;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  while GReview <> nil do
  begin
    Drive;
    Application.ProcessMessages;
    CheckSynchronize(10);
  end;
  CheckSynchronize;
  {$endif}
end.
