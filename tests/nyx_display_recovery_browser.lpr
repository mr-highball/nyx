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
program nyx_display_recovery_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.view.recovery, nyx.studio.browser,
  nyx.studio.view, nyx.studio.projects;

type
  { Standard browser input options are confined to this actual input/file
    harness. No production source accessor or editor-only test hook is added. }
  TInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

var
  GStudio: TNyxStudio;
  GChecks: Integer;
  GRefuse: Boolean;
  GRefusals: Integer;
  GCanvasHost: TJSHTMLElement; { borrowed only while its host method is intercepted }
  GOriginalAppend: TJSFunction;
  GExport: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Browser display recovery: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise ENyxModel.Create('Missing ordinary control: ' + AID);
  end;
end;

procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 15);
end;

procedure Idle; async;
var
  LStarted: Double;
begin
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));

    if window.performance.now - LStarted > 60000 then
    begin
      raise ENyxModel.Create('Ordinary browser presentation did not settle');
    end;
  until not GStudio.SourceBusy and not GStudio.PresentationPending;
end;

procedure Action(const AID, ABranch, ACommand: TNyxText); async;
var
  LControl: TJSHTMLElement;
begin
  LControl := Find(AID);

  if LControl.getBoundingClientRect.height > 0 then
  begin
    LControl.click;
    await(Idle);
    Exit;
  end;
  { Managed menus publish after the borrowed input callback returns. Traverse
    only after that real UI turn, as the maintained workspace consumer does. }
  Find('action-actions').click;
  await(Idle);

  if ABranch <> '' then
  begin
    Find('studio-menu-' + ABranch).click;
    await(Idle);
  end;
  Find('studio-menu-' + ACommand).click;
  await(Idle);
end;

procedure Source(const AText: TNyxText);
var
  LInput: TJSHTMLTextAreaElement;
begin
  LInput := TJSHTMLTextAreaElement(Find('studio-code'));
  LInput.value := AText;
  LInput.dispatchEvent(TInputEvent.new('input', New(['bubbles', True])));
  { Programmatic click does not perform a physical focus/blur transition. Deliver
    the real textarea change notification explicitly, as the maintained source
    input harness does, before asking the ordinary Apply command to read it. }
  LInput.dispatchEvent(TInputEvent.new('change', New(['bubbles', True])));
end;

function Exported(AEvent: TJSEvent): Boolean;
var
  LAnchor: TJSHTMLAnchorElement;
begin
  Result := True;

  if not (AEvent.target is TJSHTMLAnchorElement) then
  begin
    Exit;
  end;
  LAnchor := TJSHTMLAnchorElement(AEvent.target);

  if LAnchor.download = 'project.nyxproject' then
  begin
    { Observe the real portable backup, not Save's separate storage operation.
      Suppress only this owned test download; no product method is replaced. }
    AEvent.preventDefault;
    GExport := decodeURIComponent(Copy(LAnchor.href, Pos(',', LAnchor.href) + 1, MaxInt));
  end;
end;

function ExportPair: TNyxProjectPair; async;
var
  LPanel: TJSHTMLElement;
begin
  LPanel := TJSHTMLElement(document.querySelector('[data-node="action-panel-project"]'));

  if (LPanel <> nil) and (LPanel.getBoundingClientRect.height > 0) then
  begin
    LPanel.click;
    await(Idle);
  end;

  if document.querySelector('[data-node="studio-project-files"]') = nil then
  begin
    { Inspect disclosure only after its owning compact panel is mounted. An
      unmounted Project does not mean its retained Files preference is closed. }
    await(Action('action-import', 'project', 'open'));
    await(Idle);
  end;
  GExport := '';
  Find('action-project-export').click;
  await(Idle);
  Check(GExport <> '', 'ordinary backup exports the real complete project pair');
  Result := DecodeNyxProject(GExport);

  LPanel := TJSHTMLElement(document.querySelector('[data-node="action-panel-design"]'));

  if (LPanel <> nil) and (LPanel.getBoundingClientRect.height > 0) then
  begin
    LPanel.click;
    await(Idle);
  end;
end;

procedure Capture(const AName: TNyxText); async;
var
  LStarted: Double;
begin
  document.body.setAttribute('data-capture-checkpoint', AName);
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));

    if window.performance.now - LStarted > 60000 then
    begin
      raise ENyxModel.Create('Live display recovery capture was not acknowledged');
    end;
  until document.body.getAttribute('data-capture-observed') = AName;
end;

function AppendCanvas(AChild: TJSNode): TJSNode;
begin

  if GRefuse and ((AChild.nodeType = 1) or (AChild.nodeType = 11)) and
    (TJSHTMLElement(AChild).querySelector('[data-node="display-title-next"]') <> nil) then
  begin
    Inc(GRefusals);
    { A real embedding host refuses the new display, while allowing recovery of
      the old distinct children. No renderer method or model owner is patched. }
    raise ENyxModel.Create('Intentional browser display refusal');
  end;
  Result := TJSNode(GOriginalAppend.call(GCanvasHost, AChild));
end;

procedure Run; async;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LSeed: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LChanged: TNyxText;
  LIndex: Integer;
begin
  GStudio := TNyxStudio.Create;
  try
    GStudio.Run(False);
    await(Idle);
    await(Action('action-code', 'view', 'code'));
    await(Idle);
    document.addEventListener('click', @Exported);
    LDocument := TNyxDocument.Create;
    try
      LDocument.Title := 'Display recovery review';
      LPage := NewNyxPage('home');
      LDocument.AddPage(LPage);
      LPage.Add(NewNyxHeading('display-title').WithText('Make something wonderful.'));
      LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LPage := nil;
      LDocument.Free;
    end;
    { Semantic full-source import/Apply is not available through the current MCP
      workflow. This maintained physical harness exercises the actual source
      worker/editor path; it does not replace primary semantic design authoring. }
    Source(LSeed.Source);
    Find('action-apply-source').click;
    await(Idle);
    LBefore := await(ExportPair);
    Check(not LBefore.Pending, 'actual Apply has no remaining source proposal / ' +
      Find('studio-source-status').textContent);
    LIndex := 1;
    while (LIndex <= Length(LBefore.Source)) and (LIndex <= Length(LSeed.Source)) and
      (LBefore.Source[LIndex] = LSeed.Source[LIndex]) do
    begin
      Inc(LIndex);
    end;
    Check(LBefore.Source = LSeed.Source,
      'actual Apply retains the exact owned source baseline / lengths=' +
      IntToStr(Length(LBefore.Source)) + '/' + IntToStr(Length(LSeed.Source)) +
      ' / first difference=' + IntToStr(LIndex) + ' / observed=' +
      Copy(LBefore.Source, LIndex, 50) + ' / expected=' + Copy(LSeed.Source, LIndex, 50));
    Check(Find('display-title').textContent = 'Make something wonderful.',
      'actual Apply displays the exact owned heading baseline');
    GCanvasHost := TJSHTMLElement(Find('home').parentElement);
    GOriginalAppend := TJSFunction(TJSObject(GCanvasHost)['appendChild']);
    TJSObject(GCanvasHost)['appendChild'] := @AppendCanvas;
    LChanged := StringReplace(LBefore.Source, 'Make something wonderful.',
      'A refreshed design.', [rfReplaceAll]);
    LChanged := StringReplace(LChanged, '''display-title''', '''display-title-next''', [rfReplaceAll]);
    GRefuse := True;
    Source(LChanged);
    Find('action-apply-source').click;
    await(Idle);
    Check((GRefusals > 0) and (Find('display-title').textContent = 'Make something wonderful.') and
      (Find(NyxStudioDisplayRecoveryID).getBoundingClientRect.height > 0),
      'actual host refusal retains the old display and exposes the Nyx recovery notice');
    Check(Find('display-title').closest('[inert]') <> nil,
      'stale canvas faces are inert while accepted content needs display recovery');
    Check(Pos('Pascal applied', Find('studio-source-status').textContent) = 1,
      'source status reports successful admission independently of failed display');
    await(Capture('display-recovery-stale'));
    Find('action-actions').click;
    await(Idle);
    Check((document.querySelector('[data-node="studio-menu-undo"]') <> nil) and
      (document.querySelector('[data-node="studio-menu-project"]') <> nil),
      'current Chrome still opens its public action menu after canvas refusal');
    Find('action-actions').click;
    await(Idle);
    Find(NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID)).click;
    await(Idle);
    Check(not TJSHTMLButtonElement(Find(NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID))).disabled and
      (Find(NyxStudioDisplayRecoveryID).getBoundingClientRect.height > 0),
      'a refused retry restores its action without throwing out of the input handler');
    GRefuse := False;
    Find(NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID)).click;
    await(Idle);
    Check((Find('display-title-next').textContent = 'A refreshed design.') and
      (Find(NyxStudioDisplayRecoveryID).getBoundingClientRect.height = 0) and
      (Find('display-title-next').closest('[inert]') = nil),
      'Retry displays the accepted design and restores ordinary canvas input');
    LAfter := await(ExportPair);
    Check((LAfter.Source = LChanged) and not LAfter.Pending,
      'recovered display exports the successfully admitted exact Pascal pair');
    await(Action('action-undo', '', 'undo'));
    await(Idle);
    LAfter := await(ExportPair);
    Check(EncodeNyxProject(LAfter) = EncodeNyxProject(LBefore),
      'one actual Undo restores the complete exact source baseline');
    { Capture actual ordinary Studio before releasing its live owners. }
    await(Capture('display-recovery-live'));
  finally
    GRefuse := False;

    if GOriginalAppend <> nil then
    begin
      TJSObject(GCanvasHost)['appendChild'] := GOriginalAppend;
    end;

    document.removeEventListener('click', @Exported);
    GOriginalAppend := nil;
    GCanvasHost := nil;
    GExport := '';
    GStudio.Free;
    GStudio := nil;
  end;
  document.body.setAttribute('data-display-recovery', 'passed');
  document.body.setAttribute('data-display-recovery-checks', IntToStr(GChecks));
end;

procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-display-recovery', 'failed');
      document.body.setAttribute('data-display-recovery-error', LException.Message);
      document.body.setAttribute('data-event-error', LException.Message);
    end;
  end;
end;

begin
  Start;
end.
