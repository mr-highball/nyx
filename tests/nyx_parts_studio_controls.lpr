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

program nyx_parts_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,
  LCLType, LMessages, Types,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.composition, nyx.parts.editor, nyx.callbacks, nyx.source,
  nyx.disclosure, nyx.collections, nyx.collections.view, nyx.studio.hierarchy,
  nyx.studio.inspector,
  nyx.studio.menu, nyx.studio.projects, nyx.studio.session,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange{$else}nyx.studio.lcl{$endif};

type
  { Geometry copies use one screen/CSS-pixel coordinate space per target. No
    native control or DOM element survives the observation that supplies it. }
  TPartsBounds = record
    Left, Top, Right, Bottom: Double;
  end;
  { A nonempty face must fit the entire effective clip, not merely intersect
    it or report Visible. Unavailable observations never count as contained. }
  TPartsVisibility = (ppvUnavailable, ppvContained, ppvClipped);
  { The detached observation records actual host allocation and the Inspector's
    current scroll as well as every clipping ancestor's intersected client box. }
  TPartsGeometry = record
    Face, Clip: TPartsBounds;
    HostWidth, HostHeight, ScrollX, ScrollY: Double;
    Ancestors: Integer;
    Visibility: TPartsVisibility;
  end;
  { Initial clipping has a different owner from a newly clipped replacement.
    Both fail the observation; neither can be promoted to a passing focus check. }
  TPartsGeometryChange = (pgcContained, pgcInitialClipping, pgcReplacementClipping);
  { One ordinary action is submitted per settled turn. Copied paired snapshots
    bridge those turns; no selected node, widget or mounted collection survives. }
  TPayloadPhase = (ppOpenProperties, ppCustomize, ppCustomized, ppPalette,
    ppAdded, ppCaption, ppCaptioned, ppCaptionFace, ppEvents, ppAddCallback,
    ppCallbackAdded, ppUndo, ppRedo, ppReturn, ppCloseMenu, ppCloseSource,
    ppRestoreInspector, ppSelectOriginal, ppSelectedOriginal, ppDone);
  {$ifdef PAS2JS}
  { Only transport is replaced by the maintained asynchronous test exchange.
    Actual Studio controls and the shared paired source queue own every edit. }
  TPartsStudio = class(TNyxStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;
  {$else}
  { Expose the actual LCL control's ordinary click for this input consumer. }
  TControlAccess = class(TControl);
  { Read the current form's protected native Tab ordering for a one-step entry
    precondition. Navigation itself still goes through ordinary key messages. }
  TFormAccess = class(TForm);
  { Capture widgetset exceptions for the next assertion instead of displaying a
    modal error that would obscure the failed ordinary-controller journey. }
  TFailureObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

var
  {$ifdef PAS2JS}
  GStudio: TPartsStudio;
  GCore: TNyxAgentSession;
  GExchange: TNyxTestEditorExchange;
  GStarted: Double;
  {$else}
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GWidth: Integer;
  GDraft: TNyxText;
  GPartScrollAway: TPartsGeometry;
  {$endif}
  GStage, GChecks: Integer;
  GInitial, GHidden, GDefinition, GOther: TNyxText;
  GFinished: Boolean;
  GPartGeometryPath: TNyxPartRef;
  GPartGeometryAction: TNyxPartsEditorAction;
  GPartBeforeGeometry: TPartsGeometry;
  GPayloadPhase: TPayloadPhase;
  GPayloadPairs: array[0..4] of TNyxText;
  GPayloadDescriptorID: TNyxText;
  GPayloadID: TNyxText;
  GPayloadDefaultCaption: TNyxText;
  GPayloadHandler: TNyxHandlerRef;
  GPayloadCallback: TNyxCallbackRef;
  GPayloadHistoryAt: Integer;

const
  CPartsVisibility: array[TPartsVisibility] of TNyxText =
    ('unavailable', 'contained', 'clipped');
  CPartsGeometryChange: array[TPartsGeometryChange] of TNyxText =
    ('contained', 'initial-clipping', 'replacement-clipping');
  CPartsActions: array[TNyxPartsEditorAction] of TNyxText = ('hide', 'restore');
  CPayloadCaption: TNyxText = 'Review this note';

{$ifdef PAS2JS}
function TPartsStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TNyxTestEditorExchange.Create(GCore);
  Result := GExchange;
end;
{$else}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GObserver.Error <> '' then
  begin
    raise Exception.Create('Parts control callback: ' + GObserver.Error);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Parts Studio stage ' + IntToStr(GStage) + ': ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LDefinition: INyxCard;
  LActions: INyxRow;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    LDefinition := NewNyxCard('note');
    LDefinition.Add(NewNyxHeading('note-title').Configure
      .PartName(NyxPart('title')).Text('A reusable note').Done);
    LActions := NewNyxRow('note-actions');
    LActions.Configure.PartName(NyxPart('actions')).Done;
    LActions.Add(NewNyxButton('note-send').Configure
      .PartName(NyxPart('send')).Text('Send note').Done);
    LDefinition.Add(LActions);
    Result.AddComponent(LDefinition);
    LPage := NewNyxPage('home');
    Result.AddPage(LPage);
    LInstance := NewNyxComponent('first-note');
    LInstance.Reference := NyxComponent('note');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-note');
    LInstance.Reference := NyxComponent('note');
    LPage.Add(LInstance);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.ProjectSnapshot);
end;

procedure Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
begin
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check(LControl <> nil, 'mounted browser button / ' + AID);
  LControl.click;
end;
{$else}
var
  LControl: TControl;
begin

  if (GStudio.SourceView.Root <> nil) and (GStudio.SourceView.Root.Find(AID) <> nil) then
    LControl := GStudio.SourceView.ControlFor(AID)
  else
    LControl := GStudio.ShellView.ControlFor(AID);
  Check(LControl <> nil, 'mounted native button / ' + AID);
  TControlAccess(LControl).Click;
end;
{$endif}

{$ifndef PAS2JS}
{ Native contextual menus are separate public windows, not shell section views.
  Borrow their physically mounted English buttons through the same ordinary
  LCL input path as the existing Studio menu consumer. }
function MenuButton(AParent: TWinControl; const ACaption: String): TControl;
var
  LAt: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LAt := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LAt];

    if (LChild is TWinControl) and
      ((TControlAccess(LChild).Caption = ACaption) or
        (Pos(ACaption + ' ', TControlAccess(LChild).Caption) = 1)) then
    begin
      Exit(LChild);
    end;

    if LChild is TWinControl then
    begin
      Result := MenuButton(TWinControl(LChild), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure ClickMenu(const AWindow, ACaption: String);
var
  LAt: Integer;
  LButton: TControl;
begin
  Application.ProcessMessages;
  LButton := nil;
  for LAt := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LAt].Visible and (Screen.Forms[LAt].Caption = AWindow) then
    begin
      LButton := MenuButton(Screen.Forms[LAt], ACaption);
      Break;
    end;
  end;
  Check(LButton <> nil, 'actual native popup button / ' + ACaption);
  TControlAccess(LButton).Click;
  Application.ProcessMessages;
end;
{$endif}

{ The fixture has three named paths. Discover their typed copied proposals;
  row order and runtime captions do not become a test command vocabulary. }
function ActionID(const APath: TNyxText; AAction: TNyxPartsEditorAction): TNyxText;
var
  LIndex: Integer;
  LButton, LRoot: TNyxNode;
  LDocument: TNyxDocument;
  LProposal: TNyxPartsEditorProposal;
begin
  Result := '';
  LRoot := GStudio.ShellView.RootFor(NyxStudioPartsEditorID);
  Check(LRoot <> nil, 'the public Parts editor is mounted');
  LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
  try
    for LIndex := 0 to 7 do
    begin
      LButton := LRoot.Find(NyxPartsEditorActionID(NyxStudioPartsEditorID, LIndex, AAction));

      if (LButton <> nil) and CaptureNyxPartsEditor(LButton, LRoot, LDocument, LProposal) and
        (LProposal.Path.Name = APath) and (LProposal.Action = AAction) then
      begin
        Exit(LButton.ID);
      end;
    end;
  finally
    LDocument.Free;
  end;
  raise Exception.Create('No actual part action for ' + APath);
end;

{ Intersect axes independently because browser overflow may clip only one axis.
  Native child windows clip both axes at every actual parent client rectangle. }
procedure IntersectPartsBounds(var AClip: TPartsBounds; const ABounds: TPartsBounds;
  AX, AY: Boolean);
begin

  if AX then
  begin

    if ABounds.Left > AClip.Left then
      AClip.Left := ABounds.Left;

    if ABounds.Right < AClip.Right then
      AClip.Right := ABounds.Right;
  end;

  if AY then
  begin

    if ABounds.Top > AClip.Top then
      AClip.Top := ABounds.Top;

    if ABounds.Bottom < AClip.Bottom then
      AClip.Bottom := ABounds.Bottom;
  end;
end;

function CapturePartGeometry(const AID: TNyxText): TPartsGeometry;
var
  LBounds: TPartsBounds;
  LInspectorSeen, LHostSeen, LVisible: Boolean;
  {$ifdef PAS2JS}
  LFace, LAncestor, LInspector, LViewport: TJSHTMLElement;
  LBox: TJSDOMRect;
  LStyle: TJSCSSStyleDeclaration;
  LOverflowX, LOverflowY: String;

  function Clips(const AValue: String): Boolean;
  begin
    Result := (AValue = 'auto') or (AValue = 'scroll') or
      (AValue = 'hidden') or (AValue = 'clip');
  end;
  {$else}
  LFace, LAncestor: TWinControl;
  LInspector: TControl;
  LOrigin: TPoint;
  LClient: TRect;
  {$endif}
begin
  Result := Default(TPartsGeometry);
  LInspectorSeen := False;
  LHostSeen := False;
  LFace := GStudio.ShellView.FocusFor(AID);
  {$ifdef PAS2JS}
  LInspector := GStudio.ShellView.ElementFor('studio-right');
  LViewport := TJSHTMLElement(document.documentElement);

  if (LFace = nil) or (LInspector = nil) or (LViewport = nil) or
    not document.body.contains(LFace) then
    raise Exception.Create('Parts geometry requires its mounted browser face and Inspector');
  LBox := LFace.getBoundingClientRect;
  Result.Face.Left := LBox.left;
  Result.Face.Top := LBox.top;
  Result.Face.Right := LBox.right;
  Result.Face.Bottom := LBox.bottom;
  Result.HostWidth := LViewport.clientWidth;
  Result.HostHeight := LViewport.clientHeight;
  Result.Clip.Right := Result.HostWidth;
  Result.Clip.Bottom := Result.HostHeight;
  LStyle := window.getComputedStyle(LFace);
  LVisible := (LStyle.getPropertyValue('display') <> 'none') and
    (LStyle.getPropertyValue('visibility') = 'visible');
  LAncestor := TJSHTMLElement(LFace.parentElement);
  while LAncestor <> nil do
  begin
    Inc(Result.Ancestors);
    LStyle := window.getComputedStyle(LAncestor);
    LVisible := LVisible and (LStyle.getPropertyValue('display') <> 'none') and
      (LStyle.getPropertyValue('visibility') = 'visible');
    LOverflowX := LStyle.getPropertyValue('overflow-x');
    LOverflowY := LStyle.getPropertyValue('overflow-y');
    LBox := LAncestor.getBoundingClientRect;
    LBounds.Left := LBox.left + LAncestor.clientLeft;
    LBounds.Top := LBox.top + LAncestor.clientTop;
    LBounds.Right := LBounds.Left + LAncestor.clientWidth;
    LBounds.Bottom := LBounds.Top + LAncestor.clientHeight;
    IntersectPartsBounds(Result.Clip, LBounds, Clips(LOverflowX), Clips(LOverflowY));

    if LAncestor = LInspector then
    begin
      LInspectorSeen := True;
      Result.ScrollX := LAncestor.scrollLeft;
      Result.ScrollY := LAncestor.scrollTop;
    end;

    if LAncestor = LViewport then
      LHostSeen := True;
    LAncestor := TJSHTMLElement(LAncestor.parentElement);
  end;
  {$else}
  LInspector := GStudio.ShellView.ControlFor('studio-right');

  if (LFace = nil) or (LFace.Parent = nil) or (LInspector = nil) then
    raise Exception.Create('Parts geometry requires its mounted native face and Inspector');
  { BoundsRect belongs to the parent client space; the button's own client
    origin excludes its border and cannot measure the full activation face. }
  LOrigin := LFace.Parent.ClientToScreen(Point(LFace.Left, LFace.Top));
  Result.Face.Left := LOrigin.X;
  Result.Face.Top := LOrigin.Y;
  Result.Face.Right := LOrigin.X + LFace.Width;
  Result.Face.Bottom := LOrigin.Y + LFace.Height;
  Result.HostWidth := GForm.ClientWidth;
  Result.HostHeight := GForm.ClientHeight;
  LClient := GForm.ClientRect;
  LOrigin := GForm.ClientToScreen(LClient.TopLeft);
  Result.Clip.Left := LOrigin.X;
  Result.Clip.Top := LOrigin.Y;
  Result.Clip.Right := LOrigin.X + LClient.Right - LClient.Left;
  Result.Clip.Bottom := LOrigin.Y + LClient.Bottom - LClient.Top;
  LVisible := LFace.Visible;
  LAncestor := LFace.Parent;
  while LAncestor <> nil do
  begin
    Inc(Result.Ancestors);
    LVisible := LVisible and LAncestor.Visible;
    LClient := LAncestor.ClientRect;
    LOrigin := LAncestor.ClientToScreen(LClient.TopLeft);
    LBounds.Left := LOrigin.X;
    LBounds.Top := LOrigin.Y;
    LBounds.Right := LOrigin.X + LClient.Right - LClient.Left;
    LBounds.Bottom := LOrigin.Y + LClient.Bottom - LClient.Top;
    IntersectPartsBounds(Result.Clip, LBounds, True, True);

    if LAncestor = LInspector then
    begin
      LInspectorSeen := True;

      if LAncestor is TScrollingWinControl then
      begin
        Result.ScrollX := TScrollingWinControl(LAncestor).HorzScrollBar.Position;
        Result.ScrollY := TScrollingWinControl(LAncestor).VertScrollBar.Position;
      end;
    end;

    if LAncestor = GForm then
    begin
      LHostSeen := True;
      Break;
    end;
    LAncestor := LAncestor.Parent;
  end;
  {$endif}

  if not LInspectorSeen or not LHostSeen then
    raise Exception.Create('Parts geometry did not reach its actual Inspector and host');
  Result.Visibility := ppvClipped;

  if LVisible and (Result.Face.Right > Result.Face.Left) and
    (Result.Face.Bottom > Result.Face.Top) and
    (Result.Clip.Right > Result.Clip.Left) and (Result.Clip.Bottom > Result.Clip.Top) and
    (Result.Face.Left >= Result.Clip.Left) and (Result.Face.Top >= Result.Clip.Top) and
    (Result.Face.Right <= Result.Clip.Right) and (Result.Face.Bottom <= Result.Clip.Bottom) then
    Result.Visibility := ppvContained;
end;

function PartsBoundsValue(const ABounds: TPartsBounds): TNyxDataValue;
begin
  Result := NyxObject([NyxField('left', NyxData(ABounds.Left)),
    NyxField('top', NyxData(ABounds.Top)), NyxField('right', NyxData(ABounds.Right)),
    NyxField('bottom', NyxData(ABounds.Bottom))]);
end;

procedure ReportPartGeometry(const APhase, APath: TNyxText;
  AAction: TNyxPartsEditorAction; const AGeometry: TPartsGeometry;
  const AChange: TNyxText);
var
  LValue: TNyxDataValue;
begin
  LValue := NyxObject([NyxField('phase', NyxData(APhase)),
    NyxField('path', NyxData(APath)), NyxField('action', NyxData(CPartsActions[AAction])),
    NyxField('visibility', NyxData(CPartsVisibility[AGeometry.Visibility])),
    NyxField('change', NyxData(AChange)), NyxField('face', PartsBoundsValue(AGeometry.Face)),
    NyxField('clip', PartsBoundsValue(AGeometry.Clip)),
    NyxField('hostWidth', NyxData(AGeometry.HostWidth)),
    NyxField('hostHeight', NyxData(AGeometry.HostHeight)),
    NyxField('scrollX', NyxData(AGeometry.ScrollX)), NyxField('scrollY', NyxData(AGeometry.ScrollY)),
    NyxField('ancestors', NyxData(AGeometry.Ancestors))]);
  {$ifdef PAS2JS}
  document.body.setAttribute('data-parts-geometry-' + APhase, LValue.ToJSON);
  {$else}
  WriteLn('Parts geometry ', LValue.ToJSON);
  Flush(Output);
  {$endif}
end;

{ Retain only the measured face/clip and typed path across source publication.
  A replacement is never revealed or focused as part of this observation. }
procedure CheckReplacementGeometry(const APath: TNyxText; AAction: TNyxPartsEditorAction);
var
  LGeometry: TPartsGeometry;
  LChange: TPartsGeometryChange;
begin
  Check((GPartGeometryPath.Name = APath) and (GPartGeometryAction <> AAction),
    'the geometry baseline belongs to the same path and opposite Parts action');
  LGeometry := CapturePartGeometry(ActionID(APath, AAction));
  LChange := pgcContained;

  if GPartBeforeGeometry.Visibility <> ppvContained then
    LChange := pgcInitialClipping
  else if LGeometry.Visibility <> ppvContained then
    LChange := pgcReplacementClipping;
  ReportPartGeometry('after-' + CPartsActions[GPartGeometryAction], APath, AAction,
    LGeometry, CPartsGeometryChange[LChange]);
  Check(LChange <> pgcInitialClipping,
    'the initial focused action was already clipped before activation; replacement causality is unproved');
  Check(LChange <> pgcReplacementClipping,
    'a previously contained focused action became clipped after same-path replacement');
end;

{ Resolve the current physical action only after typed path discovery. Native
  key messages enter the ordinary LCL button path; browser click is a focused
  deterministic activation, not evidence of trusted browser keyboard input. }
procedure ActivatePartAction(const APath: TNyxText; AAction: TNyxPartsEditorAction);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LControl: TJSHTMLElement;
  {$else}
  LControl, LPredecessor: TWinControl;
  {$endif}
begin
  LID := ActionID(APath, AAction);
  LControl := GStudio.ShellView.FocusFor(LID);
  Check(LControl <> nil, 'the current Parts action has a physical keyboard face');
  {$ifdef PAS2JS}
  LControl.focus;
  Check(document.activeElement = LControl, 'the current browser Parts action owns focus before activation');
  {$else}
  Check(LControl.CanSetFocus and LControl.TabStop, 'the current native Parts action is keyboard reachable');

  if GStage = 1 then
  begin
    { Enter the first Hide action through the same form-level successor lookup
      used by LCL Tab, rather than assigning focus to the target. This observes
      native key-message traversal only; no physical keyboard is implied. }
    LPredecessor := TFormAccess(GForm).FindNextControl(LControl, False, True, False);
    Check((LPredecessor <> nil) and (LPredecessor <> LControl),
      'the first Parts action has a distinct actual native Tab predecessor');
    Check(TFormAccess(GForm).FindNextControl(LPredecessor, True, True, False) = LControl,
      'the actual native forward Tab successor is exactly the current Parts action');
    LPredecessor.SetFocus;
    Check(Screen.ActiveControl = LPredecessor,
      'the actual native Tab predecessor owns focus before entry');

    if LPredecessor.Perform(CN_KEYDOWN, VK_TAB, 0) = 0 then
    begin
      LPredecessor.Perform(LM_KEYDOWN, VK_TAB, 0);
    end;
    Check(Screen.ActiveControl = LControl,
      'ordinary native Tab key messages enter the exact current Parts action');
    { Key release belongs to the newly focused face, as it does after an actual
      Tab transition. No source edit or explicit scrolling precedes Space. }

    if LControl.Perform(CN_KEYUP, VK_TAB, 0) = 0 then
    begin
      LControl.Perform(LM_KEYUP, VK_TAB, 0);
    end;
  end
  else
    LControl.SetFocus;
  Check(Screen.ActiveControl = LControl, 'the current native Parts action owns focus before activation');
  {$endif}

  if (GStage = 1) or (GStage = 4) then
  begin
    GPartGeometryPath := NyxPart(APath);
    GPartGeometryAction := AAction;
    GPartBeforeGeometry := CapturePartGeometry(LID);
    ReportPartGeometry('before-' + CPartsActions[AAction], APath, AAction,
      GPartBeforeGeometry, 'unobserved');
  end;
  {$ifdef PAS2JS}
  LControl.click;
  {$else}

  if LControl.Perform(CN_KEYDOWN, VK_SPACE, 0) = 0 then
  begin
    LControl.Perform(LM_KEYDOWN, VK_SPACE, 0);
  end;

  if LControl.Perform(CN_KEYUP, VK_SPACE, 0) = 0 then
  begin
    LControl.Perform(LM_KEYUP, VK_SPACE, 0);
  end;
  {$endif}
end;

{ Never retain a face across source/presentation publication. The replacement
  is found again through its exact current typed part and action. }
function PartActionFocused(const APath: TNyxText;
  AAction: TNyxPartsEditorAction): Boolean;
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LControl: TJSHTMLElement;
  {$else}
  LControl: TWinControl;
  {$endif}
begin
  LID := ActionID(APath, AAction);
  LControl := GStudio.ShellView.FocusFor(LID);
  Result := (LControl <> nil) and
    ({$ifdef PAS2JS}document.activeElement{$else}Screen.ActiveControl{$endif} = LControl);
end;

{$ifndef PAS2JS}
{ Deliberately scroll the existing Inspector away after the two contained
  replacement checks. Copy numbers only; the upcoming Undo owns publication. }
procedure ScrollFocusedPartAway;
var
  LInspector: TControl;
  LBefore: TPartsGeometry;
  LID: TNyxText;
begin
  LID := ActionID('actions', npaHide);
  LInspector := GStudio.ShellView.ControlFor('studio-right');
  Check(LInspector is TScrollingWinControl,
    'the current native Inspector has an actual scroll owner');
  LBefore := CapturePartGeometry(LID);
  Check(LBefore.Visibility = ppvContained,
    'the same-path Hide action is contained before deliberate scrolling');
  TScrollingWinControl(LInspector).HorzScrollBar.Position := 0;
  TScrollingWinControl(LInspector).VertScrollBar.Position := 0;
  GPartScrollAway := CapturePartGeometry(LID);
  ReportPartGeometry('deliberate-scroll-away', 'actions', npaHide,
    GPartScrollAway, 'deliberate-scroll');
  Check((GPartScrollAway.ScrollX <> LBefore.ScrollX) or
    (GPartScrollAway.ScrollY <> LBefore.ScrollY),
    'deliberate native Inspector scrolling changes the actual viewport offset');
  Check(GPartScrollAway.Visibility = ppvClipped,
    'deliberate scrolling moves the focused Parts action outside its clip');
  Check(PartActionFocused('actions', npaHide),
    'scrolling away leaves the same actual Parts face focused');
  Check(Pair = GInitial, 'deliberate viewport intent does not edit the accepted pair');
end;

{ The existing Undo is real queued source/history work. Its fresh replacement
  may regain focus while the caller's explicitly chosen viewport stays put. }
procedure CheckQueuedScrollIntent;
var
  LGeometry: TPartsGeometry;
begin
  Check(PartActionFocused('actions', npaRestore),
    'queued Undo retains same-path focus after deliberate Inspector scrolling');
  LGeometry := CapturePartGeometry(ActionID('actions', npaRestore));
  ReportPartGeometry('after-undo-scroll-away', 'actions', npaRestore,
    LGeometry, 'scroll-retention-observation');
  Check((LGeometry.ScrollX = GPartScrollAway.ScrollX) and
    (LGeometry.ScrollY = GPartScrollAway.ScrollY),
    'routine queued publication preserves the deliberate Inspector viewport offset');
  Check(LGeometry.Visibility = ppvClipped,
    'focus replay does not pull an intentionally clipped action back into view');
end;
{$endif}

{ The Project panel is mounted alongside Parts only in the wide layout. Compact
  mode deliberately omits it; its title field is not a compact redirect target. }
function WideHost: Boolean;
begin
  Result := {$ifdef PAS2JS}window.innerWidth{$else}GWidth{$endif} >= 900;
end;

procedure FocusProjectTitle;
var
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TWinControl;
  {$endif}
begin
  LInput := {$ifndef PAS2JS}TWinControl({$endif}
    GStudio.ShellView.InputFor('project-title'){$ifndef PAS2JS}){$endif};
  Check(LInput <> nil, 'the wide layout has its ordinary Project title input');
  {$ifdef PAS2JS}LInput.focus;{$else}LInput.SetFocus;{$endif}
  Check({$ifdef PAS2JS}document.activeElement{$else}Screen.ActiveControl{$endif} = LInput,
    'later focus moves to the ordinary Project title input before completion');
end;

function ProjectTitleFocused: Boolean;
begin
  Result := {$ifdef PAS2JS}document.activeElement{$else}Screen.ActiveControl{$endif} =
    GStudio.ShellView.InputFor('project-title');
end;

procedure CheckParts(const APath: TNyxText; AHidden: Boolean);
var
  LDocument: TNyxDocument;
  LRuntime: TNyxNode;
  LFound: Boolean;
  LSaved: TNyxProjectPair;
begin
  LSaved := DecodeNyxProject(Pair);
  Check(EncodeNyxProject(LSaved) = Pair, 'the complete saved pair round trips exactly');
  LDocument := TNyxCodec.Decode(LSaved.Design);
  try
    Check(TNyxCodec.NodeSnapshot(LDocument.Find('note')).ToJSON = GDefinition,
      'definition remains byte-for-byte unchanged');
    Check(TNyxCodec.NodeSnapshot(LDocument.Find('second-note')).ToJSON = GOther,
      'sibling instance remains byte-for-byte unchanged');
    Check(LSaved.Source = TNyxCodegen.Generate(LDocument),
      'accepted Pascal describes the complete accepted design');
    LRuntime := RealizeNyxView(LDocument, LDocument.Find('first-note'));
    try
      LFound := True;
      try
        LRuntime.Part(NyxPart(APath));
      except
        on E: ENyxModel do
        begin

          if Pos('Compound part not found: ', E.Message) <> 1 then
          begin
            raise;
          end;
          LFound := False;
        end;
      end;
      Check(LFound <> AHidden, 'selected instance realizes the intended part membership');
    finally
      LRuntime.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

{ Resolve only the currently mounted shell. Compact panels intentionally omit
  the other owners; finding a parked/previous field cannot authorize input. }
function ShellNode(const AID: TNyxText): TNyxNode;
var
  LRoot: TNyxNode;
begin
  Result := nil;
  LRoot := GStudio.ShellView.RootFor(AID);

  if LRoot <> nil then
  begin
    Result := LRoot.Find(AID);
  end;
end;

{$ifdef PAS2JS}
{ A retained DOM face in the hidden parking owner is not an available control.
  Check the complete current parent chain without changing layout or scrolling. }
function BrowserFaceVisible(AFace: TJSHTMLElement): Boolean;
var
  LAncestor: TJSElement;
  LStyle: TJSCSSStyleDeclaration;
  LBounds: TJSDOMRect;
begin
  Result := False;

  if (AFace = nil) or not document.documentElement.contains(AFace) then
  begin
    Exit;
  end;
  LBounds := AFace.getBoundingClientRect;

  if (LBounds.width <= 0) or (LBounds.height <= 0) then
  begin
    Exit;
  end;
  LAncestor := AFace;
  while LAncestor <> nil do
  begin
    LStyle := window.getComputedStyle(LAncestor);

    if (LStyle.getPropertyValue('display') = 'none') or
      (LStyle.getPropertyValue('visibility') = 'hidden') or
      (LStyle.getPropertyValue('visibility') = 'collapse') then
    begin
      Exit;
    end;
    LAncestor := LAncestor.parentElement;
  end;
  Result := True;
end;
{$endif}

procedure CheckShellFace(const AID: TNyxText);
var
  {$ifdef PAS2JS}
  LFace: TJSHTMLElement;
  {$else}
  LFace: TControl;
  {$endif}
begin
  Check(ShellNode(AID) <> nil, 'authoring control belongs to the mounted shell / ' + AID);
  {$ifdef PAS2JS}
  LFace := GStudio.ShellView.ElementFor(AID);
  Check(BrowserFaceVisible(LFace), 'authoring browser control is visible / ' + AID);
  {$else}
  LFace := GStudio.ShellView.ControlFor(AID);
  Check((LFace <> nil) and LFace.IsVisible,
    'authoring native control is visible through its current parents / ' + AID);
  {$endif}
end;

{ The public tree supplies the selected authored identity on both targets. Never
  infer it from a generated descriptor number or call Session.Select for cleanup. }
function HierarchySelection: TNyxText;
var
  LView: INyxCollectionView;
begin
  Check(ShellNode(NyxStudioHierarchyID) <> nil, 'the current Inspector owns its hierarchy');
  LView := GStudio.ShellView.CollectionView(NyxStudioHierarchyID);
  Check((LView.Selection.Count = 1) and LView.Selection.Focus.Defined,
    'the current hierarchy reports one selected authored identity');
  Result := LView.Selection.Focus.ID;
end;

{ Customize buttons carry exact part paths as command data. Their ordinal IDs
  remain presentation details and are deliberately rediscovered after paint. }
function CustomizeActionsID: TNyxText;
var
  LMatches: Integer;

  procedure Visit(ANode: TNyxNode);
  var
    LIndex: Integer;
  begin

    if (ANode.ProjectionKind = NyxKindName(nkButton)) and
      (ANode.Prop('override-path') = 'actions') then
    begin
      Inc(LMatches);
      Result := ANode.ID;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin
  Result := '';
  LMatches := 0;
  Check(ShellNode('instance-part-properties') <> nil,
    'the ordinary instance customization disclosure is mounted');
  Visit(ShellNode('instance-part-properties'));
  Check(LMatches = 1, 'one actual Customize command owns the exact actions path');
end;

function PayloadClick(ANode: TNyxNode): TNyxAuthoredEventInfo;
var
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
begin
  Result := Default(TNyxAuthoredEventInfo);
  LEvents := NyxAuthoredEvents(ANode);
  for LIndex := 0 to High(LEvents) do
  begin

    if LEvents[LIndex].Trigger = ntClick then
    begin
      Exit(LEvents[LIndex]);
    end;
  end;
end;

{ Every accepted authoring/history result reconstructs the full paired source
  through the existing declarative owner. Callback implementations remain inert
  Pascal text; exact paired history, not bare code generation, retains the TODO. }
procedure CheckPayloadState(AState: Integer);
var
  LSaved: TNyxProjectPair;
  LDocument: TNyxDocument;
  LReplayed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LInstance: TNyxNode;
  LRule: TNyxNode;
  LPayload: TNyxNode;
  LRuntime: TNyxNode;
  LActions: TNyxNode;
  LClick: TNyxAuthoredEventInfo;
begin
  LSaved := DecodeNyxProject(Pair);
  Check(not LSaved.Pending, 'payload authoring publishes an accepted pair without a draft');
  Check(EncodeNyxProject(LSaved) = Pair, 'payload design/source pair round trips exactly');
  LDocument := TNyxCodec.Decode(LSaved.Design);
  LReplayed := nil;
  LWorkspace := nil;
  try
    Check(TNyxCodec.NodeSnapshot(LDocument.Find('note')).ToJSON = GDefinition,
      'payload publication preserves the exact reusable definition');
    Check(TNyxCodec.NodeSnapshot(LDocument.Find('second-note')).ToJSON = GOther,
      'payload publication preserves the exact sibling instance');
    LReplayed := TNyxSourceWorkspace.PrepareDraft(LSaved.Source, LWorkspace);
    Check(TNyxCodec.Encode(LReplayed) = LSaved.Design,
      'the entire accepted Pascal reconstructs the exact payload design');
    Check(LWorkspace.Capture.Source = LSaved.Source,
      'source reconstruction preserves every accepted Pascal byte');
    LInstance := LDocument.Find('first-note');

    if AState = 0 then
    begin
      Check(LInstance.Count = 0, 'original instance has no local customization descriptor');
    end
    else
    begin
      Check(LInstance.Count = 1, 'only the selected instance owns one local descriptor');
      LRule := LInstance.Children[0];
      Check((LRule.ID = GPayloadDescriptorID) and
        (LRule.Kind = NyxKindName(nkSlotOverride)) and
        (LRule.Prop(NyxAttributeName(atPath)) = 'actions'),
        'the copied descriptor identity belongs to the exact named actions path');

      if AState = 1 then
      begin
        Check((LRule.Prop(NyxAttributeName(atOverrideMode)) = NyxOverrideName(noProperties)) and
          (LRule.Count = 0), 'Customize creates an empty properties descriptor');
      end
      else
      begin
        Check((LRule.Prop(NyxAttributeName(atOverrideMode)) = NyxOverrideName(noAppend)) and
          (LRule.Count = 1), 'palette insertion promotes properties to one appended payload');
        LPayload := LRule.Children[0];
        Check((LPayload.ID = GPayloadID) and
          (LPayload.Kind = NyxKindName(nkButton)),
          'the added button retains its own copied authored identity');

        if AState = 2 then
        begin
          Check(LPayload.Prop(NyxAttributeName(atText)) = GPayloadDefaultCaption,
            'Undo preserves the palette button''s original caption');
        end
        else
        begin
          Check(LPayload.Prop(NyxAttributeName(atText)) = CPayloadCaption,
            'the English caption belongs only to the authored payload');
        end;
        LClick := PayloadClick(LPayload);

        if AState = 4 then
        begin
          Check((Length(LClick.Callbacks) = 1) and
            (LClick.Callbacks[0].Handler.Name = GPayloadHandler.Name) and
            (LClick.Callbacks[0].ID.Name = GPayloadCallback.Name),
            'the payload owns exactly the admitted Click registration');
          Check(Pos('TODO: implement ' + GPayloadHandler.Name + '.',
            ReadNyxHandlerSource(LSaved.Source, GPayloadHandler).Code) > 0,
            'the exact payload handler owns the retained TODO implementation');
        end
        else
        begin
          Check(Length(LClick.Callbacks) = 0,
            'earlier payload states contain no later Click registration');
        end;
      end;
    end;
    LRuntime := RealizeNyxView(LDocument, LInstance);
    try
      LActions := LRuntime.Part(NyxPart('actions'));
      Check(LRuntime.Part(NyxPart('actions/send')).Prop(NyxAttributeName(atText)) =
        'Send note', 'the inherited named action remains unchanged');

      if AState < 2 then
      begin
        Check(LActions.Count = 1, 'the earlier action row contains only its inherited button');
      end
      else
      begin
        Check((LActions.Count = 2) and (LActions.Children[1].DesignID = GPayloadID),
          'the selected realization appends the actual authored button after inherited content');
        Check(LActions.Children[1].Prop(NyxAttributeName(atText)) =
          LInstance.Children[0].Children[0].Prop(NyxAttributeName(atText)),
          'the realized payload caption follows its authored value');
      end;
    finally
      LRuntime.Free;
    end;
    LRuntime := RealizeNyxView(LDocument, LDocument.Find('second-note'));
    try
      Check(LRuntime.Part(NyxPart('actions')).Count = 1,
        'the sibling realization receives no added action or callback');
    finally
      LRuntime.Free;
    end;
  finally
    LWorkspace.Free;
    LReplayed.Free;
    LDocument.Free;
  end;
end;

procedure CheckPayloadCanvas(const ACaption: TNyxText);
var
  {$ifdef PAS2JS}
  LFace: TJSHTMLElement;
  {$else}
  LFace: TControl;
  {$endif}
begin
  CheckShellFace('studio-canvas');
  {$ifdef PAS2JS}
  LFace := GStudio.CanvasView.ElementFor(GPayloadID, niDesign);
  Check(BrowserFaceVisible(LFace) and (TNyxText(LFace.textContent) = ACaption),
    'the actual Design canvas shows the payload''s current English caption');
  {$else}
  LFace := GStudio.CanvasView.ControlFor(GPayloadID, niDesign);
  Check((LFace <> nil) and LFace.IsVisible and
    (TNyxText(TControlAccess(LFace).Caption) = ACaption),
    'the actual Design canvas shows the payload''s current English caption');
  {$endif}

  if not WideHost then
  begin
    Check((ShellNode('studio-left') = nil) and (ShellNode('studio-right') = nil),
      'compact Design mounts the canvas without parked Project or Inspector fields');
  end;
end;

procedure WritePayloadCaption;
var
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TControl;
  {$endif}
begin
  CheckShellFace('inspector-text');
  LInput := GStudio.ShellView.InputFor('inspector-text');
  {$ifdef PAS2JS}
  Check(LInput <> nil, 'caption editing uses the mounted Inspector input');
  LInput.focus;
  TJSHTMLInputElement(LInput).value := CPayloadCaption;
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  Check(LInput is TCustomEdit, 'caption editing uses the mounted native Inspector input');
  TWinControl(LInput).SetFocus;
  TCustomEdit(LInput).Text := CPayloadCaption;
  {$endif}
end;

function PayloadCodeShowing: Boolean;
var
  {$ifdef PAS2JS}
  LCode: TJSHTMLElement;
  {$else}
  LCode: TControl;
  {$endif}
begin
  {$ifdef PAS2JS}
  LCode := TJSHTMLElement(document.querySelector('textarea[data-node="studio-code"]'));
  Result := BrowserFaceVisible(LCode);
  {$else}
  Result := False;

  if (GStudio.CodeView.Root = nil) or (GStudio.CodeView.Root.Find('studio-code') = nil) then
  begin
    Exit;
  end;
  LCode := GStudio.CodeView.InputFor('studio-code');
  Result := (LCode is TWinControl) and LCode.IsVisible and TWinControl(LCode).Showing;
  {$endif}
end;

{ Callback completion must itself navigate the independent source owner. A
  manual Code/Design action here would hide a failed ordinary Add contract. }
procedure CheckPayloadSourceFocus;
var
  LSaved: TNyxProjectPair;
  LIntent: TNyxStudioEventIntent;
  LOwner: TNyxText;
  LView: TNyxText;
  LHandler: TNyxHandlerRef;
  LLine: Integer;
  {$ifdef PAS2JS}
  LCode: TJSHTMLTextAreaElement;
  LIndex: Integer;
  LCaretLine: Integer;
  LText: TNyxText;
  {$else}
  LCode: TControl;
  {$endif}
begin
  LSaved := GStudio.ProjectSnapshot;
  Check(GStudio.SourceCommands.CompletedEvent(LIntent, LOwner, LView, LHandler) and
    (LIntent.Action = seaAdd) and (LOwner = GPayloadID) and (LView = 'home') and
    (LHandler.Name = GPayloadHandler.Name),
    'ordinary callback completion retains the exact authored payload and view owner');
  Check(PayloadCodeShowing, 'callback Add automatically mounts visible Pascal source');
  LLine := NyxHandlerSourceLine(LSaved.Source, GPayloadHandler);
  {$ifdef PAS2JS}
  LCode := TJSHTMLTextAreaElement(
    document.querySelector('textarea[data-node="studio-code"]'));
  Check((document.activeElement = LCode) and (TNyxText(LCode.value) = LSaved.Source),
    'automatic source navigation focuses the current independent browser editor');
  LText := LCode.value;
  LCaretLine := 1;
  for LIndex := 1 to Integer(LCode.selectionStart) do
  begin

    if LText[LIndex] = #10 then
    begin
      Inc(LCaretLine);
    end;
  end;
  Check(LCaretLine = LLine, 'browser source caret names the exact payload implementation line');
  {$else}
  LCode := GStudio.CodeView.InputFor('studio-code');
  Check((LCode is TMemo) and (Screen.ActiveControl = LCode) and
    (StringReplace(TNyxText(TMemo(LCode).Text), #13#10, #10,
      [rfReplaceAll]) = LSaved.Source),
    'automatic source navigation focuses the current independent native editor');
  Check((TMemo(LCode).CaretPos.Y + 1 = LLine) and
    (GStudio.Session.CallbackLine(GPayloadHandler) = LLine) and
    (GStudio.Session.SelectedID = GPayloadID),
    'native caret and unchanged payload selection name the exact implementation line');
  {$endif}

  if not WideHost then
  begin
    Check((ShellNode('studio-canvas') <> nil) and (ShellNode('studio-right') = nil),
      'compact callback completion automatically selects Design/source ownership');
  end;
end;

{ Continue the original Parts fixture through four ordinary authored mutations.
  The existing driver settles each turn; history cleanup also uses actual UI
  actions, then the current public hierarchy restores the original selection. }
function PayloadAuthoring: Boolean;
var
  LDisclosure: TNyxNode;
  LCommand: TNyxText;
  LDocument: TNyxDocument;
  LRule: TNyxNode;
  LPayload: TNyxNode;
  LClick: TNyxAuthoredEventInfo;
  LHierarchy: INyxCollectionView;
  LItem: TNyxItemRef;
begin
  Result := False;
  {$ifndef PAS2JS}
  WriteLn('Parts payload phase ', Ord(GPayloadPhase));
  Flush(Output);
  {$endif}
  case GPayloadPhase of
    ppOpenProperties:
      begin
        GPayloadPairs[0] := GInitial;
        Check(HierarchySelection = 'first-note',
          'payload authoring starts on the selected instance');
        LDisclosure := ShellNode('instance-part-properties');
        Check(LDisclosure <> nil, 'the existing part-properties disclosure is available');

        if not NyxDisclosureExpanded(LDisclosure) then
        begin
          Click(LDisclosure.Part(NyxPart('header')).ID);
        end;
        GPayloadPhase := ppCustomize;
      end;
    ppCustomize:
      begin
        Check(NyxDisclosureExpanded(ShellNode('instance-part-properties')),
          'the ordinary disclosure opens its actual customization controls');
        Check(Pair = GPayloadPairs[0], 'opening customization changes no accepted pair');
        LCommand := CustomizeActionsID;
        CheckShellFace(LCommand);
        Click(LCommand);
        GPayloadPhase := ppCustomized;
      end;
    ppCustomized:
      begin
        GPayloadDescriptorID := HierarchySelection;
        GPayloadPairs[1] := Pair;
        Check(GPayloadPairs[1] <> GPayloadPairs[0], 'Customize is one admitted paired edit');
        CheckPayloadState(1);

        if not WideHost then
        begin
          Click('action-panel-project');
        end;
        GPayloadPhase := ppPalette;
      end;
    ppPalette:
      begin
        Check(Pair = GPayloadPairs[1], 'Project navigation preserves the customized pair');
        CheckShellFace('palette-button');

        if not WideHost then
        begin
          Check((ShellNode('studio-left') <> nil) and (ShellNode('studio-right') = nil),
            'compact palette input belongs to the actually mounted Project panel');
        end;
        {$ifndef PAS2JS}
        Check(GStudio.Session.SelectedID = GPayloadDescriptorID,
          'Project navigation retains the selected part descriptor');
        {$endif}
        Click('palette-button');
        GPayloadPhase := ppAdded;
      end;
    ppAdded:
      begin
        LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
        try
          LRule := LDocument.Find(GPayloadDescriptorID);
          Check((LRule <> nil) and (LRule.Count = 1),
            'the actual palette result belongs to the selected descriptor');
          LPayload := LRule.Children[0];
          GPayloadID := LPayload.ID;
          GPayloadDefaultCaption := LPayload.Prop(NyxAttributeName(atText));
        finally
          LDocument.Free;
        end;
        GPayloadPairs[2] := Pair;
        Check(GPayloadPairs[2] <> GPayloadPairs[1],
          'palette insertion is one admitted paired edit');
        Check(GPayloadDefaultCaption <> CPayloadCaption,
          'the chosen English caption will change the actual palette default');
        CheckPayloadState(2);
        CheckPayloadCanvas(GPayloadDefaultCaption);

        if not WideHost then
        begin
          Click('action-panel-inspector');
        end;
        GPayloadPhase := ppCaption;
      end;
    ppCaption:
      begin
        Check(HierarchySelection = GPayloadID,
          'palette completion selects the new authored payload in the current Inspector');
        WritePayloadCaption;
        Check(GStudio.SourceBusy and (Pair = GPayloadPairs[2]),
          'ordinary caption input prepares without replacing the accepted pair');
        GPayloadPhase := ppCaptioned;
      end;
    ppCaptioned:
      begin
        GPayloadPairs[3] := Pair;
        Check(GPayloadPairs[3] <> GPayloadPairs[2], 'caption editing is one admitted paired edit');
        CheckPayloadState(3);

        if not WideHost then
        begin
          Click('action-panel-design');
        end;
        GPayloadPhase := ppCaptionFace;
      end;
    ppCaptionFace:
      begin
        Check(Pair = GPayloadPairs[3], 'Design navigation preserves the caption pair');
        CheckPayloadCanvas(CPayloadCaption);

        if not WideHost then
        begin
          Click('action-panel-inspector');
        end;
        GPayloadPhase := ppEvents;
      end;
    ppEvents:
      begin
        Check(HierarchySelection = GPayloadID,
          'returning to Inspector retains the selected authored payload');
        CheckShellFace(NyxInspectorEventsID);
        Click(NyxInspectorEventsID);
        GPayloadPhase := ppAddCallback;
      end;
    ppAddCallback:
      begin
        CheckShellFace('event-click-add');
        Check(ShellNode('event-click-add').Prop(NyxStudioEventOwnerKey) = GPayloadID,
          'the ordinary Click Add command belongs to the new payload, not an inherited button');
        Check(Pair = GPayloadPairs[3], 'Events navigation retains the accepted caption pair');
        Click('event-click-add');
        Check(GStudio.SourceBusy and (Pair = GPayloadPairs[3]),
          'Click Add retains the accepted pair while its TODO source is preparing');
        GPayloadPhase := ppCallbackAdded;
      end;
    ppCallbackAdded:
      begin
        LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
        try
          LClick := PayloadClick(LDocument.Find(GPayloadID));
          Check(Length(LClick.Callbacks) = 1, 'ordinary Add publishes one payload Click callback');
          GPayloadHandler := LClick.Callbacks[0].Handler;
          GPayloadCallback := LClick.Callbacks[0].ID;
        finally
          LDocument.Free;
        end;
        GPayloadPairs[4] := Pair;
        Check(GPayloadPairs[4] <> GPayloadPairs[3],
          'callback creation is one admitted paired edit');
        CheckPayloadState(4);
        CheckPayloadSourceFocus;
        GPayloadHistoryAt := 3;
        Click('action-undo');
        GPayloadPhase := ppUndo;
      end;
    ppUndo:
      begin
        Check(Pair = GPayloadPairs[GPayloadHistoryAt],
          'each Undo restores the exact previous source/design pair');
        CheckPayloadState(GPayloadHistoryAt);

        if GPayloadHistoryAt > 0 then
        begin
          Dec(GPayloadHistoryAt);
          Click('action-undo');
        end
        else
        begin
          GPayloadHistoryAt := 1;
          Click('action-redo');
          GPayloadPhase := ppRedo;
        end;
      end;
    ppRedo:
      begin
        Check(Pair = GPayloadPairs[GPayloadHistoryAt],
          'each Redo restores the exact admitted source/design pair');
        CheckPayloadState(GPayloadHistoryAt);

        if GPayloadHistoryAt < 4 then
        begin
          Inc(GPayloadHistoryAt);
          Click('action-redo');
        end
        else
        begin
          GPayloadHistoryAt := 3;
          Click('action-undo');
          GPayloadPhase := ppReturn;
        end;
      end;
    ppReturn:
      begin
        Check(Pair = GPayloadPairs[GPayloadHistoryAt],
          'history cleanup returns through the same exact paired states');
        CheckPayloadState(GPayloadHistoryAt);

        if GPayloadHistoryAt > 0 then
        begin
          Dec(GPayloadHistoryAt);
          Click('action-undo');
        end
        else
        begin
          Check(PayloadCodeShowing, 'callback source remains visible before explicit cleanup');

          if WideHost then
          begin
            Click('action-code');
          end
          else
          begin
            Click(NyxStudioActionMenuID);
          end;
          GPayloadPhase := ppCloseMenu;
        end;
      end;
    ppCloseMenu:
      begin

        if not WideHost then
        begin
          {$ifdef PAS2JS}
          Click('studio-menu-view');
          {$else}
          ClickMenu('Component actions', 'View');
          {$endif}
        end;
        GPayloadPhase := ppCloseSource;
      end;
    ppCloseSource:
      begin

        if not WideHost then
        begin
          {$ifdef PAS2JS}
          Click('studio-menu-code');
          {$else}
          ClickMenu('View', 'Pascal source');
          {$endif}
        end;
        GPayloadPhase := ppRestoreInspector;
      end;
    ppRestoreInspector:
      begin
        Check(not PayloadCodeShowing,
          'ordinary Pascal action restores the original closed source pane');
        Check(Pair = GInitial, 'presentation cleanup leaves the original complete pair exact');

        if not WideHost then
        begin
          Click('action-panel-inspector');
        end;
        GPayloadPhase := ppSelectOriginal;
      end;
    ppSelectOriginal:
      begin
        CheckShellFace(NyxStudioHierarchyID);
        LHierarchy := GStudio.ShellView.CollectionView(NyxStudioHierarchyID);
        LItem := NyxItem(LHierarchy.Spec.Key, 'first-note');
        Check(LHierarchy.Snapshot.IndexOf(LItem) >= 0,
          'the current hierarchy contains the original instance after authored Undo');
        LHierarchy.Select(LItem);
        GPayloadPhase := ppSelectedOriginal;
      end;
    ppSelectedOriginal:
      begin
        Check(HierarchySelection = 'first-note',
          'the actual hierarchy selection route restores the original instance');
        Check(Pair = GInitial, 'hierarchy navigation adds no source/design history');
        CheckPayloadState(0);
        Check(ActionID('actions', npaHide) <> '',
          'all original Parts actions remain available for the stale/draft journey');
        GPayloadPhase := ppDone;
        Result := True;
      end;
    ppDone:
      begin
        Result := True;
      end;
  end;
end;

procedure Stage;
var
  LQueued: TNyxStudioDesignEdit;
  LAction: TNyxText;
  LDocument: TNyxDocument;
{$ifndef PAS2JS}
  LInput: TWinControl;
{$endif}
begin
  case GStage of
    0:
      begin
        GInitial := Pair;
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
        {$else}

        if GWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
        {$endif}
      end;
    1: ActivatePartAction('actions', npaHide);
    2:
      begin
        CheckParts('actions', True);
        Check(PartActionFocused('actions', npaRestore),
          'Hide preserves keyboard focus on the same part''s replacement Restore action');
        CheckReplacementGeometry('actions', npaRestore);
        GHidden := Pair;
        Check(ActionID('actions', npaRestore) <> '', 'a hidden part retains its Restore action');
        Click('action-undo');
      end;
    3:
      begin
        Check(Pair = GInitial, 'one Undo restores the exact original pair');
        Click('action-redo');
      end;
    4:
      begin
        Check(Pair = GHidden, 'one Redo restores the exact hidden pair');
        ActivatePartAction('actions', npaRestore);
      end;
    5:
      begin
        Check(PartActionFocused('actions', npaHide),
          'Restore preserves keyboard focus on the same part''s replacement Hide action');
        CheckReplacementGeometry('actions', npaHide);
        Check(Pair = GInitial, 'Restore original removes only the local descriptor');
        CheckParts('actions', False);
        {$ifndef PAS2JS}
        ScrollFocusedPartAway;
        {$endif}
        Click('action-undo');
      end;
    6:
      begin
        Check(Pair = GHidden, 'Restore is one independently undoable operation');
        {$ifndef PAS2JS}
        CheckQueuedScrollIntent;
        {$endif}
        Click('action-redo');
      end;
    7:
      begin
        Check(Pair = GInitial, 'Restore Redo retains the complete pair');
        ActivatePartAction('actions/send', npaHide);

        if WideHost then
        begin
          Check(GStudio.SourceBusy, 'nested Hide is still preparing before the later input focus');
          FocusProjectTitle;
        end;
      end;
    8:
      begin

        if WideHost then
        begin
          Check(ProjectTitleFocused, 'accepted Hide preserves the later wide-layout input focus');
        end;
        CheckParts('actions/send', True);
        CheckParts('actions', False);
        Click(ActionID('actions/send', npaRestore));
      end;
    9:
      begin

        if GPayloadPhase = ppOpenProperties then
        begin
          Check(Pair = GInitial, 'nested Restore retains parent and definition');

          if WideHost then
          begin
            Check(ProjectTitleFocused, 'Restore cannot reclaim Parts focus from the later input');
          end;
        end;

        if not PayloadAuthoring then
        begin
          Exit;
        end;
        LAction := ActionID('actions', npaHide);
        LQueued := Default(TNyxStudioDesignEdit);
        LQueued.Action := sdaTitle;
        LQueued.Selection := 'first-note';
        LQueued.View := 'home';
        LQueued.Value := 'Earlier queued edit';
        GStudio.SourceCommands.Edit(LQueued);
        Check(GStudio.SourceBusy, 'earlier ordinary edit is still preparing at Parts capture');
        Click(LAction);
      end;
    10:
      begin
        CheckParts('actions', False);
        LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
        try
          Check(LDocument.Title = 'Earlier queued edit', 'the earlier queued edit publishes normally');
        finally
          LDocument.Free;
        end;
        Click('action-undo');
      end;
    11:
      begin
        Check(Pair = GInitial, 'a stale queued Parts proposal adds no history or removal');
        {$ifdef PAS2JS}
        GFinished := True;
        {$else}
        { Pending-source refusal is exercised through the actual native memo.
          Compact mode reaches Pascal through the ordinary public Actions menu. }

        if GWidth < 900 then
        begin
          Click(NyxStudioActionMenuID);
          ClickMenu('Component actions', 'View');
          ClickMenu('View', 'Pascal source');
        end
        else
        begin
          Click('action-code');
        end;
        {$endif}
      end;
    {$ifndef PAS2JS}
    12:
      begin
        Click('action-source-tab');
        LInput := TWinControl(GStudio.CodeView.InputFor('studio-code'));
        Check(LInput is TMemo, 'actual Pascal editor is available');
        TMemo(LInput).Text := GStudio.ProjectSnapshot.Source + #10 + '// Unfinished parts draft';
      end;
    13:
      begin
        Check(Screen.ActiveControl = GStudio.CodeView.InputFor('studio-code'),
          'explicit source-tab navigation wins over earlier Parts or title focus');
        Check(GStudio.ProjectSnapshot.Pending, 'Pascal memo retains an unaccepted draft');
        GDraft := Pair;

        if GWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
      end;
    14: Click(ActionID('actions', npaHide));
    15:
      begin
        Check(Pair = GDraft, 'pending Pascal refuses part edits without losing draft or history');
        GFinished := True;
      end;
    {$endif}
  end;
  Inc(GStage);
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try

    if GExchange.RequestPending then
    begin
      GExchange.Deliver;
    end
    else if GExchange.TickPending then
    begin
      GExchange.FireTick;
    end;

    if TJSDate.now - GStarted > 180000 then
    begin
      raise Exception.Create('Parts Studio timed out at stage ' + IntToStr(GStage));
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending or not GStudio.SharedSourceSynchronized then
    begin
      window.setTimeout(@Drive, 20);
      Exit;
    end;
    Stage;

    if GFinished then
    begin
      FreeAndNil(GStudio);
      GExchange := nil;
      FreeAndNil(GCore);
      document.body.setAttribute('data-parts-studio', 'passed');
      document.body.setAttribute('data-parts-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-parts-studio', 'failed');
      document.body.setAttribute('data-parts-error', E.Message);
      document.body.setAttribute('data-parts-stage', IntToStr(GStage));
      FreeAndNil(GStudio);
      GExchange := nil;
      FreeAndNil(GCore);
    end;
  end;
end;
{$else}
procedure AwaitChanges;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Application.ProcessMessages;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise Exception.Create('Parts Studio timeout at stage ' + IntToStr(GStage));
    end;

    if GStudio.SourceCommands.Busy or GStudio.PresentationPending then
    begin
      Sleep(1);
    end;
  until not GStudio.SourceCommands.Busy and not GStudio.PresentationPending;
end;
{$endif}

var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
begin
  LDocument := nil;
  try
    LDocument := Fixture;
    GDefinition := TNyxCodec.NodeSnapshot(LDocument.Find('note')).ToJSON;
    GOther := TNyxCodec.NodeSnapshot(LDocument.Find('second-note')).ToJSON;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    FreeAndNil(LDocument);
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    GCore := TNyxAgentSession.Create(LPair);
    GCore.Call('nyx_select', 'Parts workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('parts-select')),
      NyxField('id', NyxData('first-note')), NyxField('activate', NyxData(False))]), 'parts-workshop');
    GStudio := TPartsStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    Drive;
    {$else}

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply an independent project directory and host width');
    end;
    GWidth := StrToInt(ParamStr(2));
    GObserver := TFailureObserver.Create;
    Application.Initialize;
    Application.OnException := GObserver.Failed;
    GForm := TForm.Create(nil);
    GForm.SetBounds(30, 30, GWidth, 900);
    GForm.ClientWidth := GWidth;
    GForm.Show;
    Check(GForm.ClientWidth = GWidth, 'actual native host width matches the requested allocation');
    GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
    GStudio.LoadProject(LPair);
    GStudio.Session.Select('first-note');
    GStudio.Run;
    while not GFinished do
    begin
      AwaitChanges;
      WriteLn('Parts Studio stage ', GStage, ' width=', GForm.ClientWidth);
      Flush(Output);
      Stage;
    end;
    WriteLn('PASS ', GChecks, ' ordinary native Parts checks / width=', GForm.ClientWidth);
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-parts-studio', 'failed');
      document.body.setAttribute('data-parts-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' stage=', GStage, ' checks=', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  FreeAndNil(GStudio);
  FreeAndNil(GForm);
  FreeAndNil(GObserver);
  {$endif}
  LDocument.Free;
end.
