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

program nyx_date_policy_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Graphics, IntfGraphics,
  FPWritePNG, nyx.render.lcl, nyx.studio.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.dates.editor,
  nyx.model, nyx.contract, nyx.codec, nyx.generated.view, nyx.behavior,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.view, nyx.studio.inspector, nyx.test.datepolicy;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TControlAccess = class(TControl);
  TEditAccess = class(TCustomEdit);
  {$endif}
  { This actual consumer extracts the ordinary Inspector's public Nyx compound
    and sends physical control events through the ordinary source command queue.
    It runs the real Pascal browser worker, not a scripted processor substitute.
    The hosts are owned fixture controls, never an observing user's Studio. }
  TReview = class
  private
    FHost: THost;
    FPreviewHost: THost;
    FRenderer: TRenderer;
    FPreview: TRenderer;
    FShell: TNyxDocument;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FStage: Integer;
    FBefore: TNyxText;
    FAfter: TNyxText;
    FError: TNyxText;
    procedure Refresh;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Change(AField: TNyxDateDomainEditorField; const AValue: TNyxText);
    procedure Click(AField: TNyxDateDomainEditorField);
    function Pair: TNyxText;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GStarted: {$ifdef PAS2JS}Double{$else}QWord{$endif};
  {$ifdef PAS2JS}
  GRequest: TJSXMLHttpRequest;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure SaveText(const AName: TNyxText; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Capture(AForm: TForm; const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AForm.ClientWidth, AForm.ClientHeight);
    AForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(2)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

constructor TReview.Create(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
  LSeed: TNyxProjectPair;
begin
  inherited Create;
  LDocument := BuildNyxDocument;
  try
    LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), ASource);
    GChecks := RunNyxDatePolicyTests(LSeed);
    FSession := TNyxStudioSession.Create(LSeed);
  finally
    LDocument.Free;
  end;
  FSession.Select('first-arrival');
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FPreview := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'max-width:640px;';
  FPreviewHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(FHost);
  document.body.appendChild(FPreviewHost);
  document.body.setAttribute('data-date-policy-width', IntToStr(window.innerWidth));
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(12, 12, 600, 820);
  FHost.Show;
  FPreviewHost := TForm.CreateNew(nil);
  FPreviewHost.SetBounds(630, 12, 850, 600);
  FPreviewHost.Show;
  {$endif}
  Refresh;
end;

destructor TReview.Destroy;
begin
  FCommands.Free;
  FPreview.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}
  FPreviewHost.remove;
  FHost.remove;
  {$else}
  FPreviewHost.Free;
  FHost.Free;
  {$endif}
  inherited Destroy;
end;

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
begin
  LState := DefaultNyxStudioViewState;
  LState.InspectorTab := nitProperties;
  LShell := BuildNyxStudioView(FSession, LState);
  try
    Check(LShell.Find('inspector-date-domain') <> nil,
      'The ordinary Inspector composes the public Nyx date-domain editor');
    FRenderer.Render(LShell, LShell.Find('inspector-date-domain'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
    FPreview.Render(FSession.Document, FSession.Document.Pages[0], FPreviewHost);
  finally
    LShell.Free;
  end;
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState = nssFailed then
  begin
    FError := AMessage;
  end;

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent.Trigger, FRenderer.Root);
end;

procedure TReview.Change(AField: TNyxDateDomainEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TCustomEdit;
  {$endif}
begin
  LID := NyxDateDomainEditorFieldID('inspector-date-domain', AField);
  {$ifdef PAS2JS}
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'Actual policy input is mounted');
  TJSHTMLInputElement(LInput).value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput := TCustomEdit(FRenderer.InputFor(LID));
  Check(LInput <> nil, 'Actual policy input is mounted');
  LInput.Text := AValue;
  TEditAccess(LInput).OnChange(LInput);
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue,
    'Physical policy input retains its exact draft');
end;

procedure TReview.Click(AField: TNyxDateDomainEditorField);
var
  LID: TNyxText;
begin
  LID := NyxDateDomainEditorFieldID('inspector-date-domain', AField);
  {$ifdef PAS2JS}
  FRenderer.ElementFor(LID).click;
  {$else}
  TControlAccess(FRenderer.ControlFor(LID)).Click;
  {$endif}
end;

function TReview.Step: Boolean;
var
  LDomain: TNyxValueDomain;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LCopy: TNyxStudioDesignRequest;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$endif}
begin
  Result := False;

  if FError <> '' then
  begin
    raise Exception.Create(FError);
  end;

  if FCommands.Busy then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        FBefore := Pair;
        Change(ndfMinimum, '2026-10-01');
        Change(ndfMaximum, '2026-10-31');
        Change(ndfChoices, '2026-10-06' + #10 + '2026-10-09' + #10 + '(empty)');
        Check(Pair = FBefore, 'Policy input drafts do not mutate accepted project/history');
        Check(CaptureNyxDateDomainInspector(FSession,
          FRenderer.Root.Find(NyxDateDomainEditorFieldID('inspector-date-domain', ndfApply)),
          FRenderer.Root, LEdit), 'Inspector captures the shared typed command');
        LRequest := FSession.PrepareDesignRequest(LEdit, 1);
        LCopy := ReadNyxStudioDesignRequest(LRequest.ToData);
        Check((LRequest.ToData.Field('version').AsInteger = 11) and
          LRequest.SameRequest(LCopy), 'Exact copied domain command survives the worker ticket');
        FStage := 1;
        Click(ndfApply);
      end;
    1:
      begin
        Check(FCommands.State = nssApplied, 'Ordinary source queue admits the actual Apply event');
        Check(FSession.Selected.Contract.FindValue(LDomain) and LDomain.CalendarDate,
          'Selected authored override now owns typed date constraints');
        Check(LDomain.ToData.Field('min').AsText = '2026-10-01',
          'Queued policy retains exact inclusive minimum');
        Check(Pos('.Value(NyxDateDomain.Range(NyxDate(2026, 10, 1)', FSession.Source) > 0,
          'Near-real-time accepted Pascal contains the crafted typed policy');
        FAfter := Pair;
        {$ifdef PAS2JS}
        LInput := TJSHTMLInputElement(FPreview.InputFor(
          FPreview.Root.Find(NyxQualifiedID('first-trip', 'trip-dates'))
            .Part(NyxPart('start')).ID, niRuntime));
        Check((LInput <> nil) and (LInput.getAttribute('min') = '2026-10-01') and
          (LInput.getAttribute('max') = '2026-10-31'), 'Live preview receives the admitted bounds');
        document.body.setAttribute('data-capture-checkpoint', 'date-constraints');
        {$else}
        SaveText('nyx.generated.view.pas', FSession.Source);
        SaveText('design.nyx.json', FSession.Save);
        Capture(FHost, 'date-constraints-native');
        Capture(FPreviewHost, 'date-preview-native');
        {$endif}
        FStage := 2;
      end;
    2:
      begin
        {$ifdef PAS2JS}
        if (window.location.search = '?capture=1') and
          (document.body.getAttribute('data-capture-observed') <> 'date-constraints') then
        begin
          Exit;
        end;
        {$endif}
        FSession.Undo;
        Check(Pair = FBefore, 'One ordinary Undo restores the exact policy/source pair');
        FSession.Redo;
        Check(Pair = FAfter, 'One ordinary Redo restores the exact accepted pair');
        Refresh;
        Change(ndfMinimum, '2026-10-08');
        Change(ndfChoices, '');
        FStage := 3;
        Click(ndfApply);
      end;
    3:
      begin
        Check((FCommands.State = nssRejected) and (Pair = FAfter),
          'Bounds excluding the authored default refuse without losing pair/history');
        Refresh;
        Change(ndfChoices, '2026-02-30');
        LRefused := False;
        try
          CaptureNyxDateDomainInspector(FSession,
            FRenderer.Root.Find(NyxDateDomainEditorFieldID('inspector-date-domain', ndfApply)),
            FRenderer.Root, LEdit);
        except
          on LException: ENyxContract do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (Pair = FAfter), 'Impossible choice draft refuses before enqueue');
        Refresh;
        FStage := 4;
        Click(ndfInherit);
      end;
    4:
      begin
        Check(FCommands.State = nssApplied, 'Physical Restore uses the same paired command path');
        Check(not FSession.Selected.Contract.FindValue(LDomain),
          'Restore removes only the exact local declaration');
        {$ifdef PAS2JS}
        Check(TJSHTMLButtonElement(FRenderer.ElementFor(
          NyxDateDomainEditorFieldID('inspector-date-domain', ndfInherit))).disabled,
          'Actual Restore button is disabled after inheritance is restored');
        {$else}
        Check(not FRenderer.ControlFor(
          NyxDateDomainEditorFieldID('inspector-date-domain', ndfInherit)).Enabled,
          'Actual Restore button is disabled after inheritance is restored');
        {$endif}
        Result := True;
      end;
  end;
end;

procedure Drive;
begin
  try

    if GReview.Step then
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-date-policy-checks', IntToStr(GChecks));
      document.body.setAttribute('data-date-policy-controls', 'passed');
      {$else}
      WriteLn('PASS ', GChecks, ' actual Studio date-policy checks');
      {$endif}
      Exit;
    end;
    {$ifdef PAS2JS}
    if TJSDate.now - GStarted > 120000 then
    {$else}
    if GetTickCount64 - GStarted > 120000 then
    {$endif}
    begin
      raise Exception.Create('Date-constraint queue did not finish within 120 seconds');
    end;
    {$ifdef PAS2JS}
    window.setTimeout(@Drive, 20);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-date-policy-controls', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

{$ifndef PAS2JS}
{ Exercise the full ordinary controller as well as the shared compound above.
  The project directory, form and controller belong exclusively to this fixture.
  Actual Nyx callbacks must reach its native source queue and retained code view. }
procedure RunNativeStudio(const ASource: TNyxText);
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LCode: TControl;
  LDomain: TNyxValueDomain;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Ordinary native Studio did not finish: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Button(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
  end;

  procedure Change(AField: TNyxDateDomainEditorField; const AValue: TNyxText);
  var
    LEdit: TCustomEdit;
  begin
    LEdit := TCustomEdit(LStudio.ShellView.InputFor(
      NyxDateDomainEditorFieldID('inspector-date-domain', AField)));
    LEdit.Text := AValue;
    TEditAccess(LEdit).OnChange(LEdit);
  end;

begin
  LForm := TForm.CreateNew(nil);
  LStudio := nil;
  try
    LForm.SetBounds(20, 20, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm,
      IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2))) + 'studio-projects');
    LDocument := BuildNyxDocument;
    try
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('first-arrival');
    LStudio.Run;
    Ready;
    Check(LStudio.ShellView.Root.Find('inspector-date-domain') <> nil,
      'Full ordinary Studio mounts the public date-constraint editor');
    Button('action-code');
    LCode := LStudio.CodeView.InputFor('studio-code');
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Change(ndfMinimum, '2026-10-01');
    Change(ndfMaximum, '2026-10-31');
    Change(ndfChoices, '2026-10-06' + #10 + '2026-10-09' + #10 + '(empty)');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Full Studio drafts leave the accepted pair unchanged before Apply');
    Button(NyxDateDomainEditorFieldID('inspector-date-domain', ndfApply));
    Check((LStudio.SourceCommands.State = nssApplied) and
      LStudio.Session.Selected.Contract.FindValue(LDomain) and LDomain.CalendarDate,
      'Full native controller admits the typed policy through its own source queue');
    Check((LStudio.CodeView.InputFor('studio-code') = LCode) and
      (Pos('.Value(NyxDateDomain.Range(', TMemo(LCode).Text) > 0),
      'Full Studio updates its retained Pascal editor with typed fluent constraints');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Ordinary Undo restores the exact accepted pair');
    Button('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'Ordinary Redo restores the exact typed policy pair');
    Capture(LForm, 'studio-date-policy-desktop');
    LForm.ClientWidth := 390;
    Ready;
    Button('action-panel-inspector');
    Capture(LForm, 'studio-date-policy-compact');
    Check(LStudio.Session.SelectedID = 'first-arrival',
      'Compact ordinary Inspector retains the exact policy owner');
    Button(NyxDateDomainEditorFieldID('inspector-date-domain', ndfInherit));
    Check((LStudio.SourceCommands.State = nssApplied) and
      not LStudio.Session.Selected.Contract.FindValue(LDomain),
      'Ordinary Restore removes only this local policy through paired admission');
  finally
    LStudio.Free;
    LForm.Free;
  end;
  WriteLn('PASS ', GChecks, ' date-policy checks including full ordinary native Studio');
end;
{$endif}

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;
  try

    if GRequest.status <> 200 then
    begin
      raise Exception.Create('The exact semantic seed did not load');
    end;
    GReview := TReview.Create(GRequest.responseText);
    GStarted := TJSDate.now;
    window.setTimeout(@Drive, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-date-policy-controls', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
    end;
  end;
end;
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  Application.Initialize;
  ForceDirectories(ParamStr(2));
  LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LSource, LStream.Size);
    LStream.ReadBuffer(LSource[1], Length(LSource));
  finally
    LStream.Free;
  end;
  try
    GReview := TReview.Create(LSource);
    GStarted := GetTickCount64;
    while GReview <> nil do
    begin
      Drive;
      Application.ProcessMessages;
      CheckSynchronize(10);
    end;

    if ExitCode = 0 then
    begin
      RunNativeStudio(LSource);
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      FreeAndNil(GReview);
    end;
  end;
  {$endif}
end.
