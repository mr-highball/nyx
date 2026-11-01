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
program nyx_choice_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.data, nyx.model,
  nyx.codec, nyx.choices, nyx.choices.editor, nyx.behavior, nyx.generated.view,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.view, nyx.studio.inspector
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl{$endif};

type
  { The actual ordinary target renderer is owned by the finite harness below. }
  TRenderer = {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
  { Borrowed browser element or owned native window; no platform types escape
    into the editor/library contract being qualified. }
  THost = {$ifdef PAS2JS}TJSHTMLElement{$else}TForm{$endif};
  {$ifndef PAS2JS}
  { Exposes the installed standard LCL button input without replacing its event. }
  TButtonInput = class(TButton);
  {$endif}
  { Owns one compiled semantic companion, ordinary Inspector and paired queue.
    Real widgets feed the existing routing; no accepted model is edited by a
    fixture shortcut. The source returned at the end belongs to this journey. }
  TReview = class
  private
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FRenderer: TRenderer;
    FHost: THost;
    FShell: TNyxDocument;
    FState: TNyxStudioViewState;
    FStage: Integer;
    FBefore: TNyxText;
    FAfter: TNyxText;
    FStale: TNyxStudioDesignEdit;
    procedure Refresh;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Change(AField: TNyxChoiceEditorField; const AValue: TNyxText);
    procedure Click(AAction: TNyxChoiceEditorAction);
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
    function Pair: TNyxProjectPair;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GStarted: Double;
  GContinue: Boolean;
  {$ifdef PAS2JS}
  GLoader: TJSXMLHttpRequest;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TReview.Create(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
begin
  inherited Create;
  LDocument := BuildNyxDocument;
  try
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
  finally
    LDocument.Free;
  end;
  FSession.Select('operation');
  FState := DefaultNyxStudioViewState;
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.ClientWidth := StrToIntDef(ParamStr(2), 1280);
  FHost.ClientHeight := 900;
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
  {$ifdef PAS2JS}FHost.remove;{$else}FHost.Free;{$endif}
  inherited Destroy;
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
  FState.ChoicesEditorDraft.Capture(NyxStudioChoicesEditorID, FRenderer.Root);
  LShell := BuildNyxStudioView(FSession, FState);
  try
    FState.ChoicesEditorDraft.Restore(LShell.Pages[0]);
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

  if AState in [nssApplied, nssIdle] then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Change(AField: TNyxChoiceEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TControl;
  LIndex: Integer;
  LNode: TNyxNode;
  {$endif}
begin
  LID := NyxChoiceEditorFieldID(NyxStudioChoicesEditorID, AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'the ordinary choice field is mounted');
  {$ifdef PAS2JS}
  case AField of
    ncefSelected: TJSHTMLSelectElement(LInput).value := AValue;
    ncefEscaped: TJSHTMLInputElement(LInput).checked := AValue = 'true';
    ncefKey, ncefCaption: TJSHTMLInputElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  case AField of
    ncefSelected:
      begin
        LNode := FRenderer.Root.Find(LID);
        LIndex := LNode.ChoiceItems.IndexOf(NyxChoice(AValue));
        Check(LIndex >= 0, 'the navigation key has a physical native option');
        TComboBox(LInput).ItemIndex := LIndex;
        TComboBox(LInput).OnChange(LInput);
      end;
    ncefEscaped:
      begin
        TCheckBox(LInput).Checked := AValue = 'true';
        TCheckBox(LInput).OnChange(LInput);
      end;
    ncefKey, ncefCaption:
      begin
        TEdit(LInput).Text := String(AValue);
        TEdit(LInput).OnChange(LInput);
      end;
  end;
  {$endif}
end;

procedure TReview.Click(AAction: TNyxChoiceEditorAction);
var
  LID: TNyxText;
begin
  LID := NyxChoiceEditorActionID(NyxStudioChoicesEditorID, AAction);
  {$ifdef PAS2JS}
  FRenderer.ElementFor(LID).click;
  {$else}
  TButtonInput(FRenderer.ControlFor(LID)).Click;
  {$endif}
end;

function TReview.Pair: TNyxProjectPair;
begin
  Result := FSession.ProjectSnapshot;
end;

function TReview.Step: Boolean;
var
  LChoices: INyxChoiceItems;
begin
  Result := False;

  if FCommands.Busy then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        FBefore := EncodeNyxProject(Pair);
        Change(ncefSelected, '2');
        Change(ncefCaption, 'Built with Nyx 🌙');
        Refresh;
        Check(FRenderer.Root.Find(NyxChoiceEditorFieldID(NyxStudioChoicesEditorID, ncefCaption))
          .Prop('value') = TNyxText('Built with Nyx 🌙'), 'actual field draft survives ordinary Inspector reconstruction');
        Check(EncodeNyxProject(Pair) = FBefore, 'unfinished form changes do not publish source or history');
        Check(CaptureNyxChoicesInspector(FSession, FRenderer.Root.Find(
          NyxChoiceEditorActionID(NyxStudioChoicesEditorID, nceaSave)), FRenderer.Root, FStale),
          'ordinary inspector captures an exact future stale proposal');
        Click(nceaSave);
      end;
    1:
      begin
        Check(FCommands.State = nssApplied, 'the real paired source worker publishes the choice edit');
        LChoices := FSession.Document.Find('operation').ChoiceItems;
        Check((LChoices.Item(2).Key.Value = TNyxText('second/🌙')) and
          (LChoices.Item(2).Caption = TNyxText('Built with Nyx 🌙')),
          'ordinary caption editing preserves the stable Unicode key');
        FAfter := EncodeNyxProject(Pair);
        FSession.Undo;
        Check(EncodeNyxProject(Pair) = FBefore, 'one Undo restores the whole pre-edit pair');
        FSession.Redo;
        Check(EncodeNyxProject(Pair) = FAfter, 'one Redo restores the exact accepted pair');
        FCommands.Edit(FStale);
      end;
    2:
      begin
        Check((FCommands.State = nssRejected) and (EncodeNyxProject(Pair) = FAfter),
          'the paired queue refuses the old captured form without mutation');
        Refresh;
        Change(ncefSelected, '-1');
        Change(ncefKey, 'new/🌙');
        Change(ncefCaption, 'Another friendly choice');
        Click(nceaAdd);
      end;
    3:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Find('operation').ChoiceItems.Count = 5), 'ordinary Add creates one admitted choice');
        Change(ncefSelected, '4');
        Click(nceaUp);
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Find('operation').ChoiceItems.Item(3).Key.Value = TNyxText('new/🌙')),
          'ordinary Move up reorders the exact new choice');
        Change(ncefSelected, '3');
        Click(nceaDown);
      end;
    5:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Find('operation').ChoiceItems.Item(4).Key.Value = TNyxText('new/🌙')),
          'ordinary Move down restores that exact order');
        Change(ncefSelected, '4');
        Click(nceaRemove);
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Find('operation').ChoiceItems.Count = 4), 'ordinary Remove leaves the other choices exact');
        Click(nceaEmpty);
      end;
    7:
      begin
        Check((FCommands.State = nssApplied) and FSession.Document.Find('operation').HasChoiceItems and
          (FSession.Document.Find('operation').ChoiceItems.Count = 0), 'No choices is an explicit empty declaration');
        FSession.Undo;
        Refresh;
        Click(nceaInherit);
      end;
    8:
      begin
        Check((FCommands.State = nssApplied) and not FSession.Document.Find('operation').HasChoiceItems and
          (FSession.Document.Find('operation').Prop('items') = TNyxText('Legacy first' + #10 + 'Legacy second')),
          'Use inherited choices removes only the local set and retains legacy Items');
        FSession.Undo;
        Refresh;
        Check(FSession.Document.Find('operation').ChoiceItems.Item(2).Caption = TNyxText('Built with Nyx 🌙'),
          'the final accepted pair retains the ordinary caption edit');
        Exit(True);
      end;
  end;
  Inc(FStage);
end;

procedure Drive;
{$ifndef PAS2JS}
var
  LBytes: TNyxBytes;
  LFile: TFileStream;
{$endif}
begin
  try

    if not GContinue then
    begin
      {$ifdef PAS2JS}window.setTimeout(@Drive, 50);{$endif}
      Exit;
    end;

    if (Now - GStarted) * 86400 > 180 then
    begin
      raise Exception.Create('Choice Inspector journey exceeded its deadline');
    end;

    if GReview.Step then
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-editor', 'passed');
      document.body.setAttribute('data-choice-checks', IntToStr(GChecks));
      {$else}
      LBytes := NyxEncodeUTF8(GReview.Pair.Source);
      LFile := TFileStream.Create(ParamStr(3), fmCreate);
      try

        if Length(LBytes) > 0 then
        begin
          LFile.WriteBuffer(LBytes[0], Length(LBytes));
        end;
      finally
        LFile.Free;
      end;
      WriteLn('PASS ', GChecks, ' actual choice Inspector checks');
      {$endif}
      FreeAndNil(GReview);
    end
    {$ifdef PAS2JS}
    else
    begin
      window.setTimeout(@Drive, 25);
    end
    {$endif};
  except
    on LException: Exception do
    begin
      FreeAndNil(GReview);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-editor', 'failed');
      document.body.setAttribute('data-choice-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
function BeginChecks(AEvent: TJSMouseEvent): Boolean;
begin
  GStarted := Now;
  GContinue := True;
  Result := True;
end;

function Loaded(AEvent: TJSProgressEvent): Boolean;
var
  LButton: TJSHTMLButtonElement;
begin
  Result := True;

  if GLoader.status <> 200 then
  begin
    document.body.setAttribute('data-choice-editor', 'failed');
    Exit;
  end;
  GReview := TReview.Create(GLoader.responseText);
  GLoader := nil;
  LButton := TJSHTMLButtonElement(document.createElement('button'));
  LButton.textContent := 'Run choice Inspector checks';
  LButton.onclick := @BeginChecks;
  document.body.insertBefore(LButton, document.body.firstChild);
  document.body.setAttribute('data-choice-editor', 'ready');
  Drive;
end;
{$else}
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
{$endif}

begin
  GStarted := Now;
  {$ifdef PAS2JS}
  GLoader := TJSXMLHttpRequest.new;
  GLoader.open('GET', 'nyx.generated.view.pas');
  GLoader.onload := @Loaded;
  GLoader.send;
  {$else}
  try
    Application.Initialize;
    LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LBytes, LFile.Size);

      if Length(LBytes) > 0 then
      begin
        LFile.ReadBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LFile.Free;
    end;
    GReview := TReview.Create(NyxDecodeUTF8(LBytes));
    GContinue := True;
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
      FreeAndNil(GReview);
      WriteLn('FAIL startup / ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
