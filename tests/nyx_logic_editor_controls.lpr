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

program nyx_logic_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.flow.types, nyx.flow, nyx.flow.declarations, nyx.logic, nyx.logic.editor, nyx.disclosure, nyx.behavior,
  nyx.generated.view, nyx.studio.projects, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.studio.view, nyx.studio.inspector,
  nyx.studio.inspector.groups
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, nyx.widgets.lcl, nyx.render.lcl{$endif};

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
    procedure Change(const AName, AValue: TNyxText);
    procedure Click(const AName: TNyxText);
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GPolls: Integer;
  {$ifndef PAS2JS}
  GBitmap: TBitmap;
  GPNG: TPortableNetworkGraphic;
  {$endif}
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
  FSession.Select('source-memo');
  FState := DefaultNyxStudioViewState;
  FState.LogicVisible := True;
  FState.LogicReference := NyxFlowRef('Profile');
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
  FState.LogicDraft.Capture('studio-logic-editor', FRenderer.Root);
  LShell := BuildNyxStudioView(FSession, FState);
  FState.LogicDraft.Restore('studio-logic-editor', LShell.Pages[0]);
  try
    FRenderer.Render(LShell, LShell.Find('studio-logic'), FHost);
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
  document.body.setAttribute('data-logic-command-state', IntToStr(Ord(AState)));
  document.body.setAttribute('data-logic-command-message', AMessage);
  {$endif}

  if AState in [nssApplied, nssIdle] then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  if HandleNyxLogicEditor(ANode, FRenderer.Root) then
  begin
    FRenderer.Sync;
    Exit;
  end;
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Change(const AName, AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := 'studio-logic-editor-' + AName;
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'Logic field is physically mounted');
  {$ifdef PAS2JS}
  if LInput is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('change'));
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
    LInput.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}
  if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue, 'real adapter retains exact field input');
end;

procedure TReview.Click(const AName: TNyxText);
var
  LID: TNyxText;
begin
  LID := 'studio-logic-editor-' + AName;
  {$ifdef PAS2JS}
  FRenderer.ElementFor(LID).click;
  {$else}
  TButtonAccess(FRenderer.ControlFor(LID)).Click;
  {$endif}
end;

function TReview.Step: Boolean;
var
  LValue: TNyxFlowRef;
  LItems: TNyxDataValue;
  LIndex: Integer;
  LSource: TNyxText;
  LTarget: TNyxText;
  LResult: INyxFlowResult;
begin
  Result := False;

  if FCommands.State = nssPreparing then
  begin
    Exit;
  end;
  LValue := NyxFlowRef('Profile');
  case FStage of
    0:
      begin
        Check(FRenderer.Root.Find('studio-logic-editor') <> nil, 'ordinary Studio consumes the public Logic workbench');
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        Change('search', 'format');
        Check(FRenderer.Root.Find('studio-logic-editor-tool-0').Prop('visible') = 'false',
          'search filters the mounted collection without changing the graph');
        Change('search', '');
        Change('operation', 'json-format');
        Click('defaults');
        Change('step-name', 'extra');
        Change('indent', '4');
        Check(CaptureNyxLogicInspector(FSession, FRenderer.Root.Find('studio-logic-editor-add'),
          FRenderer.Root, FStale), 'ordinary source command captures exact typed Logic proposal');
        Click('add');
      end;
    1:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Flows.Definition(LValue).StepCount = 6), 'real Add button publishes saved typed operation');
        FAfter := EncodeNyxProject(FSession.ProjectSnapshot);
        FSession.Undo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore, 'one physical editor Undo restores exact source and graph');
        FSession.Redo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FAfter, 'physical editor Redo restores complete operation');
        Refresh;
        LItems := FRenderer.Root.Find('studio-logic-editor').Extensions
          .Value(NyxExtension('nyx.logic-editor')).Field('endpoints');
        LSource := '';
        LTarget := '';
        for LIndex := 0 to LItems.Count - 1 do
        begin

          if (LItems.Item(LIndex).Field('step').AsText = 'parse') and
            (LItems.Item(LIndex).Field('port').AsText = 'json') then
          begin
            LSource := LItems.Item(LIndex).Field('label').AsText;
          end;

          if (LItems.Item(LIndex).Field('step').AsText = 'extra') and
            (LItems.Item(LIndex).Field('port').AsText = 'json') then
          begin
            LTarget := LItems.Item(LIndex).Field('label').AsText;
          end;
        end;
        Change('source', LSource);
        Change('target', LTarget);
        Click('connect');
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Flows.Definition(LValue).WireCount = 5),
          'real port choices create a typed connection through paired publication');
        LResult := FSession.Document.Flows.Definition(LValue).Build.Evaluate([
          NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('parse'), NyxLogicPort(nlpText)),
            NyxFlowText('{"profile":{"name":"English demo","number":9007199254740993}}')),
          NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('summary'), NyxLogicPort(nlpLeft)), NyxFlowText('Profile'))]);
        Check(Pos('9007199254740993', LResult.Value(
          NyxFlowEndpoint(NyxFlowStep('extra'), NyxLogicPort(nlpText))).AsText) > 0,
          'the exact UI-authored chain produces real precision-preserving output');
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        FCommands.Edit(FStale);
      end;
    3:
      begin
        Check((FCommands.State in [nssFailed, nssRejected]) and
          (EncodeNyxProject(FSession.ProjectSnapshot) = FBefore),
          'stale form baseline refuses without changing accepted source or graph / ' +
          TNyxText(IntToStr(Ord(FCommands.State))) + ' / ' + FCommands.Message);
        Refresh;
        Change('step', 'extra');
        Change('confirm', 'Confirm removal');
        Click('remove-step');
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Flows.Definition(LValue).StepCount = 5) and
          (FSession.Document.Flows.Definition(LValue).WireCount = 4),
          'confirmed removal retires the exact step and its wires together');
        FSession.Undo;
        Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
          'one Undo restores removed configuration and connections exactly');
        Refresh;
        FSession.SetSourceDraft(FSession.Source + #10 + '// unfinished Logic behavior');
        FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
        Change('step-name', 'pending');
        Click('add');
      end;
    5:
      begin
        Check((FCommands.State = nssRejected) and
          (EncodeNyxProject(FSession.ProjectSnapshot) = FBefore) and FSession.SourceDraftPending,
          'pending Pascal preserves full draft/base/history through actual editor input');
        FSession.DiscardSourceDraft;
        Refresh;
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
      WriteLn('PASS ', GChecks, ' actual Studio Logic/paired queue checks');
      {$else}
      document.body.setAttribute('data-logic-editor', 'passed');
      document.body.setAttribute('data-logic-editor-checks', IntToStr(GChecks));
      {$endif}
      { Keep the ordinary browser controls mounted for selective visual review.
        Page teardown owns their final release; no further timer is scheduled. }
      {$ifndef PAS2JS}
      if ParamCount = 2 then
      begin
        GBitmap := GReview.FHost.GetFormImage;
        GPNG := TPortableNetworkGraphic.Create;
        try
          GPNG.Assign(GBitmap);
          GPNG.SaveToFile(ParamStr(2));
        finally
          GPNG.Free;
          GBitmap.Free;
        end;
      end;
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
      document.body.setAttribute('data-logic-editor', 'failed');
      document.body.setAttribute('data-logic-editor-error', GError.Message);
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
    document.body.setAttribute('data-logic-editor', 'failed');
    document.body.setAttribute('data-logic-editor-error',
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
