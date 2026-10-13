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

program nyx_tabs_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.tabs, nyx.tabs.editor, nyx.behavior, nyx.generated.view,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.view, nyx.studio.inspector
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TButtonAccess = class(TButton);
  {$endif}
  { Owns an exact compiled semantic companion, the ordinary Nyx-built inspector
    and real paired source queue. No live project, service or profile is edited. }
  TReview = class
  private
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FRenderer: TRenderer;
    FHost: THost;
    FShell: TNyxDocument;
    FState: TNyxStudioViewState;
    FStage: Integer;
    FStale: TNyxStudioDesignEdit;
    FBefore: TNyxText;
    FAfter: TNyxText;
    procedure Refresh;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Change(AField: TNyxTabsEditorField; const AValue: TNyxText);
    procedure Click(AField: TNyxTabsEditorField);
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GPolls: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TReview.Create(const ASource: TNyxText);
var
  LDoc: TNyxDocument;
begin
  inherited Create;
  LDoc := BuildNyxDocument;
  try
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDoc), ASource));
  finally
    LDoc.Free;
  end;
  FSession.Activate('ideas');
  FSession.Select('idea-sections');
  FState := DefaultNyxStudioViewState;
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:900px;overflow:auto;max-width:680px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(0, 0, 700, 940);
  FHost.Show;
  {$endif}
  Refresh;
  {$ifdef PAS2JS}
  FRenderer.ElementFor('inspector-tab-policy').scrollIntoView;
  document.body.setAttribute('data-capture-checkpoint', 'tab-policy-form');
  {$endif}
end;

destructor TReview.Destroy;
begin
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}FHost.remove;{$else}FHost.Free;{$endif}
  inherited Destroy;
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
  FState.TabsEditorDraft.Capture('inspector-tab-policy', FRenderer.Root);
  LShell := BuildNyxStudioView(FSession, FState);
  try
    FState.TabsEditorDraft.Restore(LShell.Pages[0]);
    FRenderer.Render(LShell, LShell.Find('studio-right'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Change(AField: TNyxTabsEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := NyxTabsEditorFieldID('inspector-tab-policy', AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'typed policy field is physically mounted');
  {$ifdef PAS2JS}

  if AField = nteWrap then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TCheckBox then
  begin
    TCheckBox(LInput).Checked := AValue = 'true';
    TCheckBox(LInput).OnChange(LInput);
  end
  else
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue, 'adapter retains the exact disposable field value');
end;

procedure TReview.Click(AField: TNyxTabsEditorField);
var
  LID: TNyxText;
begin
  LID := NyxTabsEditorFieldID('inspector-tab-policy', AField);
  {$ifdef PAS2JS}FRenderer.ElementFor(LID).click;{$else}
  TButtonAccess(FRenderer.ControlFor(LID)).Click;{$endif}
end;

function TReview.Step: Boolean;
var
  LPolicy: TNyxTabsOptions;
begin
  Result := False;

  if FCommands.Busy then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}

  if (FStage = 0) and (document.body.getAttribute('data-capture-observed') <> 'tab-policy-form') then
  begin
    Exit;
  end;
  {$endif}
  case FStage of
    0:
      begin
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        Change(nteActivation, 'On Enter or Space');
        Change(nteOrientation, 'Vertical');
        Change(nteWrap, 'false');
        { An unrelated shell refresh retains this form draft independently of
          accepted source. The real controls are reconstructed from public parts. }
        Refresh;
        Check(FRenderer.Root.Find(NyxTabsEditorFieldID('inspector-tab-policy', nteOrientation)).Prop('value') = 'Vertical',
          'ordinary refresh preserves parked form choices');
        Check(CaptureNyxTabsInspector(FSession,
          FRenderer.Root.Find(NyxTabsEditorFieldID('inspector-tab-policy', nteApply)),
          FRenderer.Root, FStale), 'inspector captures a copied baseline before publication');
        Click(nteApply);
      end;
    1:
      begin
        Check(FCommands.State = nssApplied, 'real paired worker publishes tab policy');
        LPolicy := NyxTabsOptions(FSession.Document.Find('idea-sections'));
        Check((LPolicy.ActivationMode = ntaManual) and (LPolicy.OrientationMode = ntoVertical) and
          not LPolicy.WrapFocus, 'ordinary inspector publishes closed choices and Boolean');
        FAfter := EncodeNyxProject(FSession.ProjectSnapshot);
        FSession.Undo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore, 'one Undo restores complete design/source pair');
        FSession.Redo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FAfter, 'one Redo restores exact complete policy pair');
        FCommands.Edit(FStale);
      end;
    2:
      begin
        Check((FCommands.State = nssRejected) and
          (EncodeNyxProject(FSession.ProjectSnapshot) = FAfter), 'stale form worker refuses without paired mutation');
        FSession.SetSourceDraft(FSession.Source + #10 + '// unfinished application work');
        Click(nteApply);
      end;
    3:
      begin
        Check((FCommands.State = nssRejected) and FSession.SourceDraftPending,
          'ordinary form respects unfinished source and refuses publication');
        Check(FSession.ProjectSnapshot.Draft = FSession.Source + #10 + '// unfinished application work',
          'rejected form retains exact pending Pascal');
        FSession.DiscardSourceDraft;
        Click(nteInherit);
      end;
    4:
      begin
        Check(FCommands.State = nssApplied, 'real paired worker removes only the local policy');
        Check(not FSession.Document.Find('idea-sections').Extensions.Has(NyxExtension(NyxTabsOptionsKey)),
          'inherited policy remains distinct from explicit defaults');
        Result := True;
      end;
  end;
  Inc(FStage);
end;

procedure Drive;
begin
  try
    Inc(GPolls);

    if GPolls > 2000 then
    begin
      raise Exception.Create('Tab authoring worker did not finish its bounded journey');
    end;

    if GReview.Step then
    begin
      WriteLn('PASS ', GChecks, ' actual Studio tab inspector/paired queue checks');
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-tab-authoring', 'passed');
      document.body.setAttribute('data-nyx-tab-authoring-checks', IntToStr(GChecks));
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-tab-authoring', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}DumpExceptionBackTrace(Output); ExitCode := 1;{$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-nyx-tab-authoring', 'failed');
    Exit;
  end;
  GReview := TReview.Create(GRequest.responseText);
  GRequest := nil;
  Drive;
end;
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'nyx.generated.view.pas');
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  try
    Application.Initialize;
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LSource, LStream.Size);
      LStream.ReadBuffer(LSource[1], Length(LSource));
    finally
      LStream.Free;
    end;
    GReview := TReview.Create(LSource);
    while GReview <> nil do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Drive;
      Sleep(25);
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL startup / ', LException.Message);
      DumpExceptionBackTrace(Output);
      FreeAndNil(GReview);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
