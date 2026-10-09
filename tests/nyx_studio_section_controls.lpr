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
program nyx_studio_section_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls, ExtCtrls,
    nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.contract,
  nyx.theme, nyx.behavior, nyx.events, nyx.scheduler, nyx.content.mount,
  nyx.studio.session,
  nyx.studio.view, nyx.studio.sections, nyx.studio.section.views
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser{$endif};

type
  {$ifndef PAS2JS}
  { An ordinary embedding host can refuse insertion before frame ownership is
    assigned. The local proposal must be freed, with the old frame untouched. }
  TRefusalPanel = class(TPanel)
  public
    procedure InsertControl(AControl: TControl; AIndex: Integer); override;
  end;
  {$endif}
  { This receiver borrows no view. Its event token is canceled before the
    receiver is released; the facade/configurer is destroyed before its theme. }
  TScenario = class(TNyxEventCallback)
  public
    Clicks: Integer;
    Configurations: Integer;
    Views: TNyxStudioSectionViews; { borrowed; revoked before facade retirement }
    Continuity: TNyxContentFaceStates;
    BusyCaptureRefused: Boolean;
    BusyRestoreRefused: Boolean;
    procedure Configure(ARenderer: TNyxStudioSectionRenderer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GFailBuild: Boolean;
  GFailPreview: Boolean;
  GProjectPreviewed: Boolean;
  GFailInsertion: Boolean;
  GInsertionRefusals: Integer;
  {$ifdef PAS2JS}
  { Borrowed only during the one embedding-host refusal. Restore the original
    method before clearing these handles; no production object retains them. }
  GAppendParent: TJSNode;
  GOriginalAppend: TJSFunction;
  {$endif}

{$ifdef PAS2JS}
function AppendFrame(AChild: TJSNode): TJSNode;
begin
  Result := TJSNode(GOriginalAppend.call(GAppendParent, AChild));

  if GFailInsertion and (AChild is TJSHTMLElement) and
    TJSHTMLElement(AChild).hasAttribute('data-nyx-shell-frame') then
  begin
    GFailInsertion := False;
    Inc(GInsertionRefusals);
    { The browser insertion succeeded before the extension refused. Retiring
      the unassigned candidate must remove this actual attached DOM proposal. }
    raise ENyxModel.Create('Intentional frame host insertion refusal');
  end;
end;
{$else}
procedure TRefusalPanel.InsertControl(AControl: TControl; AIndex: Integer);
begin

  if GFailInsertion and (AControl is TPanel) then
  begin
    GFailInsertion := False;
    Inc(GInsertionRefusals);
    raise ENyxModel.Create('Intentional frame host insertion refusal');
  end;
  inherited InsertControl(AControl, AIndex);
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Studio section recovery: ' + AReason);
  end;
  Inc(GChecks);
  WriteLn('PASS ', GChecks, ' / ', AReason);
end;

{ Refusal occurs only once the candidate leaves its detached parking host.
  Native visible ancestors and browser inert ancestors establish that physical
  boundary. The earlier Project preview must already have succeeded. Neither
  helper patches production publication or substitutes a model-only event. }
function LiveFace(AFace: TNyxStudioSectionControl): Boolean;
{$ifndef PAS2JS}
var
  LParent: TControl;
{$endif}
begin
  {$ifdef PAS2JS}
  Result := (AFace <> nil) and document.body.contains(AFace) and
    (AFace.closest('[inert]') = nil);
  {$else}
  Result := False;
  LParent := AFace;
  while LParent <> nil do
  begin

    if not LParent.Visible then
    begin
      Exit;
    end;
    LParent := LParent.Parent;
  end;
  Result := AFace <> nil;
  {$endif}
end;

{$ifdef PAS2JS}
function BadgeFace(ANode: TNyxNode): TJSHTMLElement;
{$else}
function BadgeFace(ANode: TNyxNode; AOwner: TComponent): TControl;
{$endif}
begin

  if GFailBuild and (ANode.ID = 'recovery-inspector-badge') then
  begin
    raise ENyxModel.Create('Intentional Studio Inspector factory refusal');
  end;
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('button'));
  Result.className := 'nyx-button';
  Result.textContent := ANode.Prop('text');
  {$else}
  Result := TButton.Create(AOwner);
  TButton(Result).Caption := ANode.Prop('text');
  {$endif}
end;

procedure UpdateBadge(ANode: TNyxNode; AFace: TNyxStudioSectionControl);
begin

  if GFailPreview and LiveFace(AFace) then
  begin

    if ANode.ID = 'recovery-project-badge' then
    begin
      GProjectPreviewed := True;
    end
    else if ANode.ID = 'recovery-inspector-badge' then
    begin
      GFailPreview := False;
      raise ENyxModel.Create('Intentional Studio Inspector physical preview refusal');
    end;
  end;
  {$ifdef PAS2JS}
  AFace.textContent := ANode.Prop('text');
  {$else}
  TButton(AFace).Caption := ANode.Prop('text');
  {$endif}
end;

procedure TScenario.Configure(ARenderer: TNyxStudioSectionRenderer);
begin
  Inc(Configurations);
  ARenderer.RegisterFactory(NyxKindName(nkBadge),
    {$ifdef PAS2JS}@{$endif}BadgeFace, {$ifdef PAS2JS}@{$endif}UpdateBadge);
end;

procedure TScenario.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LCopied: TNyxContentFaceStates;
begin

  if (AEvent.Trigger = ntClick) and (AExecution <> nil) then
  begin
    Inc(Clicks);
    BusyCaptureRefused := not Views.SectionView(nssChrome).CaptureInteraction(LCopied);
    BusyRestoreRefused := not Views.SectionView(nssChrome).RestoreInteraction(Continuity);
  end;
end;

function InputText(AFace: TNyxStudioSectionControl): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AFace).value;
  {$else}
  Result := TCustomEdit(AFace).Text;
  {$endif}
end;

procedure Draft(AFace: TNyxStudioSectionControl; const AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AText;
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := AText;
  {$endif}
end;

procedure Click(AFace: TNyxStudioSectionControl);
begin
  {$ifdef PAS2JS}
  AFace.click;
  {$else}
  TButton(AFace).Click;
  {$endif}
end;

function Shell(ASession: TNyxStudioSession): TNyxDocument;
var
  LInput: INyxInput;
begin
  Result := BuildNyxStudioView(ASession, DefaultNyxStudioViewState);
  try
    { A creator-supplied numeric field retains its physical draft until editing
      completes on both targets. Its accepted value is independently observable;
      a retained control alone does not establish preservation of that draft. }
    LInput := NewNyxInput('recovery-budget');
    LInput.Text := 'Draft budget';
    LInput.Configure.InputType(niNumber).Value(10.0).Done;
    LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
    Result.Pages[0].Add(LInput);
    Result.Pages[0].Add(NewNyxButton('recovery-action').WithText('Check callbacks'));
    Result.Pages[0].Find('studio-right').Add(
      NewNyxBadge('recovery-inspector-badge').WithText('Inspector extension'));
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 15);
end;
{$endif}

procedure Idle(AViews: TNyxStudioSectionViews); {$ifdef PAS2JS}async;{$endif}
var
  LTurns: Integer;
begin
  LTurns := 0;
  repeat
    {$ifdef PAS2JS}
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));
    {$else}
    Application.ProcessMessages;
    Sleep(10);
    {$endif}
    Inc(LTurns);

    if LTurns > 2000 then
    begin
      raise ENyxModel.Create('Studio section input boundary did not settle');
    end;
  until not AViews.Dispatching;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LSession: TNyxStudioSession;
  LTheme: TNyxTheme;
  LViews: TNyxStudioSectionViews;
  LBase: TNyxDocument;
  LNext: TNyxDocument;
  LFull: TNyxDocument;
  LScenario: TScenario;
  LLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LInput: TNyxStudioSectionControl;
  LAction: TNyxStudioSectionControl;
  LTitle: TNyxStudioSectionControl;
  LBeforeInputs: TNyxContentFaceStates;
  LBadInputs: TNyxContentFaceStates;
  LIndex: Integer;
  LInputIndex: Integer;
  LTitleText: TNyxText;
  LRoots: array[TNyxStudioSection] of TNyxNode;
  LRole: TNyxStudioSection;
  LRefused: Boolean;
  LMessage: TNyxText;
  LHostChildren: Integer;
  LRootsRetained: Boolean;
  {$ifdef PAS2JS}
  LStarted: Double;
  LHost: TJSHTMLElement;
  LStyle: TJSHTMLElement;
  {$else}
  LWindow: TForm;
  LHost: TPanel;
  {$endif}
begin
  LSession := TNyxStudioSession.Create;
  LTheme := TNyxTheme.Create;
  LTheme.Accent := '#227766';
  LTheme.FontSize := 17;
  LScenario := TScenario.Create;
  LLease := LScenario;
  LViews := TNyxStudioSectionViews.Create(LTheme, nscEditorOwnedHierarchy,
    {$ifdef PAS2JS}@{$endif}LScenario.Configure);
  LScenario.Views := LViews;
  LBase := nil;
  LNext := nil;
  LFull := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.getElementById('studio-sections'));
  LStyle := TJSHTMLElement(document.createElement('style'));
  LStyle.textContent := NyxStudioBrowserCSS;
  document.head.appendChild(LStyle);
  {$else}
  Application.Initialize;
  LWindow := TForm.CreateNew(nil);
  LWindow.Caption := 'Nyx Studio section recovery';
  LWindow.SetBounds(20, 20, 1240, 820);
  LHost := TRefusalPanel.Create(LWindow);
  LHost.BevelOuter := bvNone;
  LHost.Parent := LWindow;
  LHost.Align := alClient;
  LWindow.Show;
  {$endif}
  try
    LBase := Shell(LSession);
    LViews.Render(LBase, LBase.Pages[0], LHost);
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LInput := LViews.InputFor('recovery-budget');
    LAction := LViews.ControlFor('recovery-action');
    LTitle := LViews.InputFor('project-title');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LRoots[LRole] := LViews.SectionRoot(LRole);
    end;
    LToken := LViews.Events.On(NyxControlEvents('recovery-action', niRuntime),
      ntClick).Subscribe(LLease);
    Draft(LInput, '12.5');
    Check((InputText(LInput) = '12.5') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '10'),
      'unfinished numeric edit is separate from accepted model text');
    Draft(LTitle, TNyxText('A retained thought / 🌙'));
    LTitleText := InputText(LTitle);
    {$ifdef PAS2JS}
    LTitle.focus;
    TJSHTMLInputElement(LTitle).setSelectionRange(2, 7);
    {$else}
    TCustomEdit(LTitle).SetFocus;
    TCustomEdit(LTitle).SelStart := 2;
    TCustomEdit(LTitle).SelLength := 5;
    {$endif}
    Check(LViews.SectionView(nssChrome).CaptureInteraction(LBeforeInputs),
      'idle renderer exposes copied continuity without borrowing its controls');
    LScenario.Continuity := LBeforeInputs;
    LInputIndex := -1;
    SetLength(LBadInputs, Length(LBeforeInputs) + 1);
    for LIndex := 0 to High(LBeforeInputs) do
    begin
      LBadInputs[LIndex] := LBeforeInputs[LIndex];

      if LBeforeInputs[LIndex].Identity.RuntimeID = 'recovery-budget' then
      begin
        LInputIndex := LIndex;
      end;
    end;
    Check(LInputIndex >= 0, 'continuity contains copied identity for the unfinished field');
    LBadInputs[High(LBadInputs)] := LBeforeInputs[LInputIndex];
    LRefused := False;
    try
      LViews.SectionView(nssChrome).RestoreInteraction(LBadInputs);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (InputText(LInput) = '12.5'),
      'ambiguous continuity refuses before changing physical drafts');
    LBadInputs := nil;
    LNext := LBase.Clone;
    LNext.Pages[0].Find('recovery-budget').Configure.Value(11.0).Done;
    LNext.Pages[0].Find('project-title').Configure.Text('Changed candidate title').Done;
    GFailBuild := True;
    LRefused := False;
    LMessage := '';
    try
      LViews.TryRefresh(LNext, LNext.Pages[0]);
    except
      on LException: Exception do
      begin
        LRefused := True;
        LMessage := LException.Message;
      end;
    end;
    GFailBuild := False;
    Check(LRefused and (Pos('Intentional Studio Inspector factory refusal',
      LMessage) > 0), 'later Inspector preparation actually refuses');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      Check(LViews.SectionRoot(LRole) = LRoots[LRole],
        'preparation refusal preserves exact root / ' + NyxStudioSectionRootID(LRole));
    end;
    Check((LViews.InputFor('recovery-budget') = LInput) and
      (LViews.ControlFor('recovery-action') = LAction),
      'retained Chrome input and action identities survive refusal');
    Check((LViews.Root.Find('recovery-budget').Prop('value') = '10') and
      (InputText(LInput) = '12.5'), 'preparation rollback preserves exact unfinished draft');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7) and
      (InputText(LTitle) = LTitleText), 'preparation rollback preserves focused text/range');
    {$else}
    Check((Screen.ActiveControl = LTitle) and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5) and (InputText(LTitle) = LTitleText),
      'preparation rollback preserves focused text/range');
    {$endif}
    Click(LAction);
    Check(LToken.Active and (LScenario.Clicks = 1),
      'original registered callback remains live after refusal');
    Check(LScenario.BusyCaptureRefused and LScenario.BusyRestoreRefused and
      (InputText(LInput) = '12.5'),
      'actual borrowed-input callback refuses capture/restore without touching its draft');
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LNext.Pages[0].Find('studio-left').Add(
      NewNyxBadge('recovery-project-badge').WithText('Project extension'));
    GFailPreview := True;
    GProjectPreviewed := False;
    LRefused := False;
    LMessage := '';
    try
      LViews.TryRefresh(LNext, LNext.Pages[0]);
    except
      on LException: Exception do
      begin
        LRefused := True;
        LMessage := LException.Message;
      end;
    end;
    GFailPreview := False;
    Check(LRefused and GProjectPreviewed and
      (Pos('Intentional Studio Inspector physical preview refusal', LMessage) > 0),
      'second physical publication refuses after first Project preview');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      Check(LViews.SectionRoot(LRole) = LRoots[LRole],
        'grouped preview rollback preserves exact root / ' + NyxStudioSectionRootID(LRole));
    end;
    Check((InputText(LInput) = '12.5') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '10') and LToken.Active,
      'grouped preview rollback preserves draft/model/callback');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7) and
      (InputText(LTitle) = LTitleText), 'grouped rollback preserves focused text/range');
    {$else}
    Check((Screen.ActiveControl = LTitle) and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5) and (InputText(LTitle) = LTitleText),
      'grouped rollback preserves focused text/range');
    {$endif}
    Check(LViews.Root.Find('recovery-project-badge') = nil,
      'refused candidate extension never joins the mounted lookup forest');
    Click(LAction);
    Check(LScenario.Clicks = 2, 'retained callback still executes once after grouped rollback');
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    Check(LViews.TryRefresh(LNext, LNext.Pages[0]),
      'fresh later grouped refresh succeeds after both failure paths');
    Check((LViews.SectionRoot(nssChrome) = LRoots[nssChrome]) and
      (LViews.InputFor('recovery-budget') = LInput) and
      (LViews.SectionRoot(nssProject) <> LRoots[nssProject]) and
      (LViews.SectionRoot(nssInspector) <> LRoots[nssInspector]),
      'successful group replaces only Project and Inspector');
    Check((InputText(LInput) = '11') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '11'),
      'explicit successful authored value supersedes the unfinished draft');
    Check(LScenario.Configurations >= 8,
      'creator configurator runs for freshly staged replacement renderers');
    {$ifdef PAS2JS}
    Check(Trim(window.getComputedStyle(LViews.ControlFor('recovery-project-badge'))
      .getPropertyValue('--nyx-accent')) = LTheme.Accent,
      'borrowed custom theme reaches the replacement Project view');
    {$else}
    Check(TCustomEdit(LViews.InputFor('project-title')).Font.Height = -LTheme.FontSize,
      'replacement Project input has its physical themed font');
    {$endif}
    Click(LAction);
    Check(LToken.Active and (LScenario.Clicks = 3), 'retained callback survives successful group');
    Check(LViews.SectionView(nssChrome).RestoreInteraction(LBeforeInputs) and
      (InputText(LInput) = '11') and
      (LViews.Root.Find('recovery-budget').Prop('value') = '11'),
      'stale copied draft cannot override a newly admitted authored value');
    { Exercise the complete owner, rather than only independent section swaps.
      A new Chrome descendant changes membership of its physical hierarchy. }
    LTitle := LViews.InputFor('project-title');
    Draft(LInput, '11.75');
    Draft(LTitle, TNyxText('A complete-frame thought / 🌙'));
    LTitleText := InputText(LTitle);
    {$ifdef PAS2JS}
    LTitle.focus;
    TJSHTMLInputElement(LTitle).setSelectionRange(2, 7);
    {$else}
    TCustomEdit(LTitle).SetFocus;
    TCustomEdit(LTitle).SelStart := 2;
    TCustomEdit(LTitle).SelLength := 5;
    {$endif}
    {$ifdef PAS2JS}await(Idle(LViews));{$else}Idle(LViews);{$endif}
    LFull := LNext.Clone;
    { The ordinary string field admits input to its private model immediately;
      the numeric domain above defers admission. Forward the accepted string
      baseline as a real composer would, rather than request an explicit reset. }
    LFull.Pages[0].Find('project-title').SetProp('value',
      LViews.Root.Find('project-title').Prop('value'));
    LFull.Pages[0].Add(NewNyxButton('recovery-new-chrome').WithText('New chrome'));
    Check(not LViews.TryRefresh(LFull, LFull.Pages[0]),
      'structural Chrome change requests complete frame admission');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LRoots[LRole] := LViews.SectionRoot(LRole);
    end;
    LRefused := False;
    GFailBuild := True;
    try
      LViews.Render(LFull, LFull.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRefused := True;
        LMessage := LException.Message;
      end;
    end;
    GFailBuild := False;
    Check(LRefused and (Pos('Intentional Studio Inspector factory refusal', LMessage) > 0),
      'later complete-frame factory failure refuses before old ownership retires');
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      Check(LViews.SectionRoot(LRole) = LRoots[LRole],
        'complete refusal preserves exact root / ' + NyxStudioSectionRootID(LRole));
    end;
    Check((LViews.InputFor('recovery-budget') = LInput) and
      (LViews.InputFor('project-title') = LTitle) and LToken.Active and
      (InputText(LInput) = '11.75') and (InputText(LTitle) = LTitleText),
      'complete refusal recovers original controls, drafts and callback scope');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7),
      'complete refusal recovers physical focus and range');
    {$else}
    Check(TCustomEdit(LTitle).Focused and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5),
      'complete refusal recovers physical focus and range');
    {$endif}
    { Refuse at the embedding host boundary, earlier than any component
      factory. This covers locally owned allocation as well as frame recovery. }
    {$ifdef PAS2JS}
    GAppendParent := LHost.parentNode;
    GOriginalAppend := TJSFunction(TJSObject(GAppendParent)['appendChild']);
    LHostChildren := GAppendParent.childNodes.length;
    TJSObject(GAppendParent)['appendChild'] := @AppendFrame;
    {$else}
    LHostChildren := LHost.ControlCount;
    {$endif}
    GFailInsertion := True;
    LRefused := False;
    try
      try
        LViews.Render(LFull, LFull.Pages[0], LHost);
      except
        on LException: Exception do
        begin
          LRefused := True;
          LMessage := LException.Message;
        end;
      end;
    finally
      GFailInsertion := False;
      {$ifdef PAS2JS}
      TJSObject(GAppendParent)['appendChild'] := GOriginalAppend;
      GOriginalAppend := nil;
      GAppendParent := nil;
      {$endif}
    end;
    Check(LRefused and (GInsertionRefusals = 1) and
      (Pos('Intentional frame host insertion refusal', LMessage) > 0),
      'embedding host refusal reaches the actual unassigned frame boundary');
    LRootsRetained := True;
    for LRole := Low(TNyxStudioSection) to High(TNyxStudioSection) do
    begin
      LRootsRetained := LRootsRetained and (LViews.SectionRoot(LRole) = LRoots[LRole]);
    end;
    Check(LRootsRetained and (LViews.InputFor('project-title') = LTitle) and LToken.Active,
      'host refusal preserves exact prior roots, input and callback scope');
    {$ifdef PAS2JS}
    Check(LHost.parentNode.childNodes.length = LHostChildren,
      'host refusal detaches the actual local candidate');
    Check((document.activeElement = LTitle) and (InputText(LTitle) = LTitleText) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7),
      'host refusal preserves physical Unicode text, focus and range');
    {$else}
    Check(LHost.ControlCount = LHostChildren,
      'host refusal retains only the original host children');
    Check(TCustomEdit(LTitle).Focused and (InputText(LTitle) = LTitleText) and
      (TCustomEdit(LTitle).SelStart = 2) and (TCustomEdit(LTitle).SelLength = 5),
      'host refusal preserves physical Unicode text, focus and range');
    {$endif}
    LViews.Render(LFull, LFull.Pages[0], LHost, False, nil,
      [nssChrome, nssProject, nssInspector]);
    LInput := LViews.InputFor('recovery-budget');
    LTitle := LViews.InputFor('project-title');
    Check(InputText(LInput) = '11.75',
      'complete frame retains explicitly selected numeric draft / actual=' + InputText(LInput));
    Check(InputText(LTitle) = LTitleText,
      'complete frame retains explicitly selected Unicode draft / actual=' + InputText(LTitle));
    Check((LViews.SectionRoot(nssChrome) <> LRoots[nssChrome]) and
      (LViews.ControlFor('recovery-new-chrome') <> nil) and not LToken.Active and
      (LViews.Root.Find('recovery-budget').Prop('value') = '11'),
      'complete admission changes physical owners and revokes old callbacks without editing model values');
    {$ifdef PAS2JS}
    Check((document.activeElement = LTitle) and
      (TJSHTMLInputElement(LTitle).selectionStart = 2) and
      (TJSHTMLInputElement(LTitle).selectionEnd = 7),
      'complete admission transfers physical focus and range');
    {$else}
    Check(TCustomEdit(LTitle).Focused and (TCustomEdit(LTitle).SelStart = 2) and
      (TCustomEdit(LTitle).SelLength = 5),
      'complete admission transfers physical focus and range');
    {$endif}
    LViews.Render(LFull, LFull.Pages[0], LHost);
    LInput := LViews.InputFor('recovery-budget');
    LTitle := LViews.InputFor('project-title');
    Check((InputText(LInput) = '11') and
      (InputText(LTitle) = LFull.Pages[0].Find('project-title').Prop('value')),
      'default complete admission discards unqualified physical drafts');
    Draft(LInput, '11.875');
    LFull.Pages[0].Find('recovery-budget').SetProp('value', '13');
    LViews.Render(LFull, LFull.Pages[0], LHost, False, nil, [nssChrome]);
    LInput := LViews.InputFor('recovery-budget');
    Check(InputText(LInput) = '13',
      'forward continuity cannot overwrite a changed accepted value');
    LToken := LViews.Events.On(NyxControlEvents('recovery-action', niRuntime),
      ntClick).Subscribe(LLease);
    Click(LViews.ControlFor('recovery-action'));
    Check(LToken.Active and (LScenario.Clicks = 4),
      'new complete-frame callback scope executes once');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-capture-checkpoint', 'studio-recovery-live');
    LStarted := window.performance.now;
    repeat
      await(TJSPromise.resolve(TJSPromise.new(@Pause)));

      if window.performance.now - LStarted > 30000 then
      begin
        raise ENyxModel.Create('Studio recovery capture acknowledgment timed out');
      end;
    until document.body.getAttribute('data-capture-observed') = 'studio-recovery-live';
    {$else}
    Application.ProcessMessages;

    if ParamCount > 0 then
    begin
      SaveNyxNativeCapture(LWindow, ParamStr(1), ncmPrint);
    end;
    {$endif}
    LScenario.Views := nil;
    LViews.Free;
    LViews := nil;
    Check(not LToken.Active, 'facade retirement revokes the retained event token');
    WriteLn('PASS ', GChecks, ' actual Studio section recovery checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-section-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
  finally

    if LToken <> nil then
    begin
      LToken.Cancel;
    end;
    LToken := nil;
    LScenario.Views := nil;
    LViews.Free;
    LLease := nil;
    LFull.Free;
    LNext.Free;
    LBase.Free;
    LTheme.Free;
    LSession.Free;
    {$ifdef PAS2JS}LStyle.remove;{$endif}
    {$ifndef PAS2JS}LWindow.Free;{$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-section-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
