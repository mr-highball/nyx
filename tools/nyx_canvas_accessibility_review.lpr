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
program nyx_canvas_accessibility_review;

{$mode delphi}{$H+}{$codepage utf8}

{ Review actual LCL properties/DOM attributes, not accessibility-tree or
  screen-reader output.
  Exit 2 reports observed product gaps; exit 1 is a broken review/host. No image,
  HTTP service, operator project or enrollment is involved. All names are applied
  through existing typed authoring/binding contracts. Width is 800 or 390. }
uses
  Classes, SysUtils,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state, nyx.codec, nyx.data,
  nyx.canvas.scene, nyx.canvas.workspace
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.canvas.browser
  {$else}, Interfaces, Forms, Controls, nyx.render.lcl, nyx.canvas.lcl{$endif};

const
  { Dedicated supplementary/multiscript input uses the portable text type so
    native comparisons never introduce an implicit ANSI/Unicode conversion. }
  UnicodeReviewName: TNyxText = 'Planning 🌙 / 東京';
  { Authored English help must override generic navigation help without losing
    its ordinary tooltip on the outer control. }
  PlanningHint: TNyxText = 'Pan and zoom through the planning board.';

type
  { Own the complete review and independent runtime store. Target surfaces and
    parent controls are borrowed only while Renderer/View remain connected.
    Native Application borrows the exception receiver until its host retires; no
    document/control retains this owner, and no user state is read or written. }
  TReview = class
  private
    FDocument: TNyxDocument;
    FState: TNyxState;
    {$ifdef PAS2JS}
    FHost: TJSHTMLElement;
    FReserved: TJSHTMLElement;
    FRenderer: TNyxBrowserRenderer;
    {$else}
    FHost: TForm;
    FRenderer: TNyxLCLRenderer;
    FOriginalException: TExceptionEvent;
    {$endif}
    FError: TNyxText;
    FBefore: TNyxText;
    FGaps: Integer;
    {$ifndef PAS2JS}
    procedure Failed(ASender: TObject; AError: Exception);
    {$endif}
    procedure Pump;
    procedure Require(ACondition: Boolean; const AMessage: TNyxText);
    function Inspect(const AID, AExpected: TNyxText): TNyxDataValue;
  public
    constructor Create(AWidth: Integer);
    destructor Destroy; override;
    function Run: TNyxDataValue;
    property Gaps: Integer read FGaps;
  end;

{$ifndef PAS2JS}
procedure TReview.Failed(ASender: TObject; AError: Exception);
begin
  FError := TNyxText(AError.ClassName) + ': ' + TNyxText(AError.Message);
end;
{$endif}

procedure TReview.Pump;
begin
  {$ifndef PAS2JS}
  Application.ProcessMessages;
  {$endif}
end;

procedure TReview.Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition or (FError <> '') then
  begin
    raise Exception.Create(AMessage + ' / ' + FError);
  end;
end;

constructor TReview.Create(AWidth: Integer);
var
  LPage: INyxPage;
  LStatic: INyxCanvas;
  LBound: INyxCanvas;
  LDefault: INyxCanvas;
begin
  inherited Create;
  {$ifndef PAS2JS}
  FOriginalException := Application.OnException;
  Application.OnException := Failed;
  {$endif}
  FDocument := TNyxDocument.Create;
  FDocument.State.SetValue(NyxTextState('board-name'), 'Session board');
  LPage := NewNyxPage('home');
  FDocument.AddPage(LPage);
  LStatic := NewNyxCanvas('planning-board');
  LStatic.Configure.Height(120).AccessibleName('Planning board').Hint(PlanningHint).Done;
  LPage.Add(LStatic);
  LBound := NewNyxCanvas('session-board');
  LBound.Configure.Height(120).Done;
  LBound.Binds.AccessibleName(NyxTextState('board-name'));
  LPage.Add(LBound);
  LDefault := NewNyxCanvas('default-board');
  LDefault.Configure.Height(120).Done;
  LPage.Add(LDefault);
  FDocument.Validate;
  FBefore := TNyxCodec.Encode(FDocument);
  FState := FDocument.State.Clone;
  {$ifdef PAS2JS}
  Require(document.getElementById('nyx-canvas-description-1') = nil,
    'review requires its own fresh browser document');
  FReserved := TJSHTMLElement(document.createElement('span'));
  FReserved.id := 'nyx-canvas-description-1';
  FReserved.setAttribute('hidden', '');
  FReserved.textContent := 'Caller description';
  document.body.appendChild(FReserved);
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.setProperty('width', IntToStr(AWidth) + 'px');
  FHost.style.setProperty('height', '800px');
  document.body.appendChild(FHost);
  FRenderer := TNyxBrowserRenderer.Create;
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.Caption := 'Canvas accessibility review';
  FHost.ClientWidth := AWidth;
  FHost.ClientHeight := 800;
  FHost.Show;
  Pump;
  FRenderer := TNyxLCLRenderer.Create;
  {$endif}
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost, False, FState);
  Pump;
  Require(True, 'ordinary target mount must succeed');
end;

function TReview.Inspect(const AID, AExpected: TNyxText): TNyxDataValue;
var
  {$ifdef PAS2JS}
  LView: INyxBrowserCanvasWorkspaceView;
  LDescription: TJSElement;
  {$else}
  LView: INyxLCLCanvasWorkspaceView;
  {$endif}
  LOuter: TNyxText;
  LSurface: TNyxText;
  LMatches: Boolean;
  LCanFocus: Boolean;
  LHelp: TNyxText;
  LHint: TNyxText;
begin
  {$ifdef PAS2JS}
  LView := FRenderer.CanvasView(AID, niRuntime) as INyxBrowserCanvasWorkspaceView;
  LOuter := '';

  if FRenderer.ElementFor(AID, niRuntime).hasAttribute('aria-label') then
  begin
    LOuter := FRenderer.ElementFor(AID, niRuntime).getAttribute('aria-label');
  end;
  LHint := '';

  if FRenderer.ElementFor(AID, niRuntime).hasAttribute('title') then
  begin
    LHint := FRenderer.ElementFor(AID, niRuntime).getAttribute('title');
  end;
  LSurface := LView.Surface.getAttribute('aria-label');
  LDescription := document.getElementById(LView.Surface.getAttribute('aria-describedby'));
  Require((LDescription <> nil) and LView.Surface.contains(LDescription) and
    LDescription.hasAttribute('hidden'), 'description belongs to this retained surface');
  LHelp := LDescription.textContent;
  LCanFocus := (LView.Surface.tabIndex >= 0) and document.body.contains(LView.Surface);
  {$else}
  LView := FRenderer.CanvasView(AID, niRuntime) as INyxLCLCanvasWorkspaceView;
  LOuter := TNyxText(FRenderer.ControlFor(AID, niRuntime).AccessibleName);
  LSurface := TNyxText(LView.Surface.AccessibleName);
  LHelp := TNyxText(LView.Surface.AccessibleDescription);
  LHint := TNyxText(FRenderer.ControlFor(AID, niRuntime).Hint);
  LCanFocus := LView.Surface.CanFocus;
  {$endif}
  LMatches := LSurface = AExpected;

  if not LMatches then
  begin
    Inc(FGaps);
  end;
  Result := NyxObject([
    NyxField('control', NyxData(AID)),
    NyxField('expectedName', NyxData(AExpected)),
    NyxField('outerName', NyxData(LOuter)),
    NyxField('surfaceName', NyxData(LSurface)),
    NyxField('surfaceCanFocus', NyxData(LCanFocus)),
    NyxField('description', NyxData(LHelp)),
    NyxField('outerHint', NyxData(LHint)),
    {$ifdef PAS2JS}NyxField('descriptionID', NyxData(TNyxText(LDescription.id))),{$endif}
    NyxField('nameMatches', NyxData(LMatches))]);
end;

function TReview.Run: TNyxDataValue;
var
  LStatic: TNyxDataValue;
  LInitial: TNyxDataValue;
  LUpdated: TNyxDataValue;
  LDefault: TNyxDataValue;
  LUnicode: TNyxDataValue;
  LReplacement: TNyxDataValue;
  {$ifdef PAS2JS}
  LPrevious: INyxBrowserCanvasWorkspaceView;
  LSurface: TJSHTMLElement;
  {$else}
  LPrevious: INyxLCLCanvasWorkspaceView;
  LSurface: TWinControl;
  {$endif}
  LWidth: Integer;
  LMetadata: INyxCanvasAccessibilityView;
  LRefused: Boolean;
  LCustomHelp: Boolean;
begin
  LStatic := Inspect('planning-board', 'Planning board');
  Require(LStatic.Field('outerName').AsText = 'Planning board',
    'typed accessible name must reach its ordinary outer control');
  Require(LStatic.Field('outerHint').AsText = PlanningHint,
    'the authored hint must actually reach its ordinary control');
  LCustomHelp := LStatic.Field('description').AsText = PlanningHint;

  if not LCustomHelp then
  begin
    Inc(FGaps);
  end;
  LInitial := Inspect('session-board', 'Session board');
  Require(LInitial.Field('outerName').AsText = 'Session board',
    'the initial typed name binding must actually run');
  {$ifdef PAS2JS}
  Require((LStatic.Field('descriptionID').AsText <> LInitial.Field('descriptionID').AsText) and
    (LStatic.Field('descriptionID').AsText <> FReserved.id) and
    (FReserved.textContent = 'Caller description'),
    'independent descriptions neither collide with nor overwrite caller DOM');
  {$endif}
  LSurface := (FRenderer.CanvasView('planning-board', niRuntime) as
    {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif}).Surface;
  {$ifdef PAS2JS}
  LSurface.focus;
  {$else}
  LSurface.SetFocus;
  {$endif}
  FState.SetValue(NyxTextState('board-name'), 'Updated session board');
  Pump;
  LUpdated := Inspect('session-board', 'Updated session board');
  Require(LUpdated.Field('outerName').AsText = 'Updated session board',
    'the runtime name update must actually reach the outer control');
  LPrevious := FRenderer.CanvasView('planning-board', niRuntime) as
    {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  Require((LSurface = LPrevious.Surface) and
    {$ifdef PAS2JS}(document.activeElement = LSurface){$else}LSurface.Focused{$endif},
    'name-only state publication must retain the same focused surface');
  LDefault := Inspect('default-board', 'Canvas workspace');
  Require(LDefault.Field('nameMatches').AsBoolean,
    'an unnamed canvas must retain its existing usable default');
  Require(LDefault.Field('description').AsText = NyxCanvasKeyboardHelp,
    'a concise name must retain separate keyboard help');
  { Non-English text belongs to this dedicated qualification input, not a demo. }
  FState.SetValue(NyxTextState('board-name'), UnicodeReviewName);
  Pump;
  LUnicode := Inspect('session-board', UnicodeReviewName);
  Require(LUnicode.Field('outerName').AsText = UnicodeReviewName,
    'Unicode must survive the existing binding before assessing the surface');
  FRenderer.Render(FDocument, FDocument.Pages[0], FHost, False, FState);
  Pump;
  Require(not LPrevious.Connected, 'a replaced view must revoke its old target authority');
  Require(Supports(LPrevious, INyxCanvasAccessibilityView, LMetadata),
    'default views expose the optional typed metadata bridge');
  LRefused := False;
  try
    LMetadata.SetAccessibleMetadata('Stale name', 'Stale description');
  except
    on ENyxCanvasScene do
    begin
      LRefused := True;
    end;
  end;
  Require(LRefused, 'revoked metadata authority refuses before touching retired controls');
  LReplacement := Inspect('session-board', UnicodeReviewName);
  Require(TNyxCodec.Encode(FDocument) = FBefore,
    'runtime review must preserve its complete authored document');
  {$ifdef PAS2JS}LWidth := FHost.clientWidth;{$else}LWidth := FHost.ClientWidth;{$endif}
  Result := NyxObject([
    NyxField('scope', NyxData(TNyxText(
      {$ifdef PAS2JS}'DOM attribute review; not accessibility-tree/assistive-technology qualification'
      {$else}'LCL property review; not assistive-technology qualification'{$endif}))),
    NyxField('width', NyxData(LWidth)),
    NyxField('staticName', LStatic),
    NyxField('initialBinding', LInitial),
    NyxField('updatedBinding', LUpdated),
    NyxField('defaultName', LDefault),
    NyxField('unicodeBinding', LUnicode),
    NyxField('replacement', LReplacement),
    NyxField('focusedSurfaceRetained', NyxData(True)),
    NyxField('previousViewRevoked', NyxData(True)),
    NyxField('revokedMetadataRefused', NyxData(LRefused)),
    NyxField('customHelpMatches', NyxData(LCustomHelp)),
    NyxField('authoredDocumentUnchanged', NyxData(True)),
    NyxField('gaps', NyxData(FGaps))]);
end;

destructor TReview.Destroy;
begin
  FRenderer.Free;
  FState.Free;
  FDocument.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;

  if FReserved <> nil then
  begin
    FReserved.remove;
  end;
  {$else}
  FHost.Free;
  Application.OnException := FOriginalException;
  {$endif}
  inherited Destroy;
end;

var
  LReview: TReview;
  LWidth: Integer;
  LResult: TNyxDataValue;
begin
  LReview := nil;
  try
    LWidth := 800;
    {$ifdef PAS2JS}

    if window.innerWidth <= 480 then
    begin
      LWidth := 390;
    end;
    {$else}

    if ParamCount > 0 then
    begin

      if (ParamCount <> 1) or not TryStrToInt(ParamStr(1), LWidth) or
        ((LWidth <> 390) and (LWidth <> 800)) then
      begin
        raise Exception.Create('Supply width 800 or 390');
      end;
    end;
    Application.Initialize;
    {$endif}
    LReview := TReview.Create(LWidth);
    LResult := LReview.Run;
    WriteLn(LResult.ToJSON);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-review', LResult.ToJSON);
    {$endif}

    if LReview.Gaps > 0 then
    begin
      WriteLn('PARTIAL: authored canvas names do not all reach the focusable surface');
      ExitCode := 2;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  LReview.Free;
end.
