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

program nyx_canvas_inspector_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.canvas.types, nyx.canvas.editor, nyx.disclosure, nyx.behavior,
  nyx.generated.view, nyx.studio.projects, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.studio.view, nyx.studio.inspector,
  nyx.studio.inspector.groups
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.widgets.lcl, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TButtonAccess = class(TNyxLCLButton);
  {$endif}
  { Owns only its semantic companion, public Studio inspector and ordinary
    paired worker. No live service, user project or machine profile is changed. }
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
    procedure Change(const AValue: TNyxText);
    procedure Click;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GPolls: Integer;
  {$ifdef PAS2JS}GRequest: TJSXMLHttpRequest;{$endif}

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
  FSession.Activate('home');
  FSession.Select('board');
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
  FHost.SetBounds(20, 20, 700, 900);
  FHost.Show;
  {$endif}
  Refresh;
end;

destructor TReview.Destroy;
begin
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;
  {$else}
  FHost.Free;
  {$endif}
  inherited Destroy;
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
  FState.InspectorGroups := FState.InspectorGroups.Captured(nigComponent, FRenderer.Root);
  LShell := BuildNyxStudioView(FSession, FState);
  try
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
  {$ifdef PAS2JS}
  document.body.setAttribute('data-canvas-command-state', IntToStr(Ord(AState)));
  document.body.setAttribute('data-canvas-command-message', AMessage);
  {$endif}

  if AState in [nssApplied, nssIdle] then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Change(const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := 'inspector-canvas-scene-choice';
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'typed appearance choice is physically mounted');
  {$ifdef PAS2JS}
  TJSHTMLSelectElement(LInput).value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
  TComboBox(LInput).OnChange(LInput);
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue, 'real adapter retains the selected disposable choice');
end;

procedure TReview.Click;
var
  LID: TNyxText;
begin
  LID := 'inspector-canvas-scene-apply';
  {$ifdef PAS2JS}
  FRenderer.ElementFor(LID).click;
  {$else}
  TButtonAccess(FRenderer.ControlFor(LID)).Click;
  {$endif}
end;

function TReview.Step: Boolean;
var
  LButton: TNyxNode;
begin
  Result := False;

  if FCommands.State = nssPreparing then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        Change('Empty workspace');
        LButton := FRenderer.Root.Find('inspector-canvas-scene-apply');
        Check(CaptureNyxCanvasInspector(FSession, LButton, FRenderer.Root, FStale),
          'public scene picker captures a sealed Studio command');
        Click;
      end;
    1:
      begin
        Check((FCommands.State = nssApplied) and
          not FSession.Document.Find('board').CanvasScene.Defined,
          'ordinary Studio scene choice publishes through its paired worker');
        FAfter := EncodeNyxProject(FSession.ProjectSnapshot);
        FSession.Undo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
          'one inspector Undo restores the exact scene design and Pascal');
        FSession.Redo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FAfter,
          'one inspector Redo restores the exact detached scene pair');
        FCommands.Edit(FStale);
      end;
    2:
      begin
        Check((FCommands.State = nssRejected) and
          (EncodeNyxProject(FSession.ProjectSnapshot) = FAfter),
          'queued stale scene picker baseline refuses the exact current pair');
        Refresh;
        Change('Scene / workshop 🐾');
        Click;
      end;
    3:
      begin
        Check((FCommands.State = nssApplied) and
          FSession.Document.Find('board').CanvasScene.Defined,
          'ordinary Studio restores an exact saved scene reference');
        Refresh;
        FSession.SetSourceDraft(FSession.Source + #10 + '// unfinished canvas behavior');
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        Change('Empty workspace');
        Click;
      end;
    4:
      begin
        Check((FCommands.State = nssRejected) and FSession.SourceDraftPending and
          (EncodeNyxProject(FSession.ProjectSnapshot) = FBefore),
          'scene choice preserves unfinished Pascal, accepted pair and history');
        Result := True;
      end;
  end;
  Inc(FStage);
end;

procedure Drive;
begin
  try
    Inc(GPolls);

    if GPolls >= 2400 then
    begin
      raise Exception.Create('Owned ordinary source queue did not settle; inspect its receipts');
    end;

    if GReview.Step then
    begin
      {$ifndef PAS2JS}
      WriteLn('PASS ', GChecks, ' actual Studio canvas picker/paired queue checks');
      {$else}
      document.body.setAttribute('data-canvas-inspector', 'passed');
      document.body.setAttribute('data-canvas-inspector-checks', IntToStr(GChecks));
      {$endif}
      { Keep the ordinary browser controls mounted for selective visual review.
        Page teardown owns their final release; no further timer is scheduled. }
      {$ifndef PAS2JS}
      FreeAndNil(GReview);
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on GError: Exception do
    begin
      {$ifndef PAS2JS}
      WriteLn('FAIL ', GError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$else}
      document.body.setAttribute('data-canvas-inspector', 'failed');
      document.body.setAttribute('data-canvas-inspector-error', GError.Message);
      {$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-canvas-inspector', 'failed');
    document.body.setAttribute('data-canvas-inspector-error',
      'Exact compiler companion was not served successfully');
    GRequest := nil;
    Exit;
  end;
  GReview := TReview.Create(GRequest.responseText);
  GRequest := nil;
  Drive;
end;
{$else}
var
  LFile: TFileStream;
  LSource: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'nyx.generated.view.pas');
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  Application.Initialize;
  try
    LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LSource, LFile.Size);
      LFile.ReadBuffer(LSource[1], Length(LSource));
    finally
      LFile.Free;
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
    on GError: Exception do
    begin
      WriteLn('FAIL startup / ', GError.Message);
      DumpExceptionBackTrace(Output);
      FreeAndNil(GReview);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
