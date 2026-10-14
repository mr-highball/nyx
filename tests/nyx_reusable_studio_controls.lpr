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

program nyx_reusable_studio_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.codegen, nyx.controls,
  nyx.data, nyx.reusable.editor, nyx.disclosure, nyx.studio.inspector,
  nyx.studio.inspector.groups, nyx.studio.menu, nyx.studio.projects, nyx.generated.view,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange{$else}nyx.studio.lcl{$endif};

{$ifdef PAS2JS}
type
  { Only the asynchronous transport is supplied. All controls, source commands,
    presentation and history use the ordinary stock Studio controller. }
  TReusableStudio = class(TNyxStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;
{$else}
type
  TControlAccess = class(TControl);
  { Widgetset exceptions are retained for the next assertion, never obscured
    behind a modal error dialog during an automated ordinary-control journey. }
  TFailureObserver = class
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

var
  {$ifdef PAS2JS}
  GStudio: TReusableStudio;
  GCore: TNyxAgentSession;
  GExchange: TNyxTestEditorExchange; { Borrowed until Studio destruction. }
  GStarted: Double;
  GCapture: Boolean;
  GSourceMenuOpened: Boolean;
  {$else}
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  {$endif}
  GStage: Integer;
  GChecks: Integer;
  GBefore: TNyxText;
  GCreated: TNyxText;
  GDraft: TNyxText;
  GFinished: Boolean;

{$ifdef PAS2JS}
function TReusableStudio.CreateEditorExchange: TNyxStudioEditorExchange;
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
    raise Exception.Create('Ordinary reusable callback: ' + GObserver.Error);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Ordinary reusable Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.ProjectSnapshot);
end;

function FieldID(AField: TNyxReusableEditorField): TNyxText;
begin
  Result := NyxReusableEditorFieldID(NyxStudioReusableEditorID, AField);
end;

procedure Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
begin
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check(LControl <> nil, 'actual button exists / ' + AID);
  LControl.click;
end;
{$else}
var
  LControl: TControl;
begin

  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  Check(LControl <> nil, 'actual button exists / ' + AID);
  TControlAccess(LControl).Click;
end;
{$endif}

procedure WriteField(const AID, AValue: TNyxText);
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
begin
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check(LControl <> nil, 'actual field exists / ' + AID);

  if (LControl.tagName <> 'INPUT') and (LControl.tagName <> 'TEXTAREA') then
  begin
    LControl := TJSHTMLElement(LControl.querySelector('input,textarea'));
  end;
  Check(LControl <> nil, 'actual input exists / ' + AID);
  LControl.focus;
  TJSHTMLTextAreaElement(LControl).value := AValue;
  LControl.dispatchEvent(TJSEvent.new('change'));
end;
{$else}
var
  LControl: TWinControl;
begin

  if AID = 'studio-code' then
  begin
    LControl := TWinControl(GStudio.CodeView.InputFor(AID));
  end
  else
  begin
    LControl := TWinControl(GStudio.ShellView.InputFor(AID));
  end;
  Check(LControl <> nil, 'actual input exists / ' + AID);

  if LControl is TMemo then
  begin
    TMemo(LControl).Text := AValue;
  end
  else
  begin
    TEdit(LControl).Text := AValue;
  end;
end;
{$endif}

function ReadName: TNyxText;
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
begin
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + FieldID(nrfName) + '"]'));
  Check(LControl <> nil, 'current name control remains mounted');

  if LControl.tagName <> 'INPUT' then
  begin
    LControl := TJSHTMLElement(LControl.querySelector('input'));
  end;
  Check(LControl <> nil, 'current name input remains mounted');
  Result := TJSHTMLInputElement(LControl).value;
end;
{$else}
begin
  Result := TNyxText(TEdit(GStudio.ShellView.InputFor(FieldID(nrfName))).Text);
end;
{$endif}

procedure Checkpoint(const AName: TNyxText);
begin
  {$ifdef PAS2JS}

  if GCapture then
  begin
    GStudio.ShellView.ControlFor(FieldID(nrfSummary)).scrollIntoView(False);
    document.body.setAttribute('data-capture-checkpoint', AName);
  end;
  {$endif}
end;

{ Each stage issues at most one ordinary mutation. The owning target loop waits
  for both source preparation and deferred presentation before proceeding. }
procedure Stage;
var
  LDocument: TNyxDocument;
  LGroup: TNyxNode;
  LSnapshot: TNyxProjectPair;
begin
  case GStage of
    0:
      begin
        GBefore := Pair;
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
        {$endif}
      end;
    1:
      begin
        Click(NyxInspectorPropertiesID);
      end;
    2:
      begin
        Check(GStudio.ShellView.Root.Find(NyxStudioReusableEditorID) <> nil,
          'stock Inspector mounts the public reusable editor');
        WriteField(FieldID(nrfName), 'note');
      end;
    3:
      begin
        Click(FieldID(nrfPreview));
      end;
    4:
      begin
        Check(Pos('note-title-2', GStudio.ShellView.Root.Find(FieldID(nrfSummary)).Prop('text')) > 0,
          'physical preview displays the collision-free purpose-based identity');
        Check(Pair = GBefore, 'preview preserves exact accepted design/Pascal');
        Checkpoint('reusable-name-preview');
      end;
    5:
      begin
        LGroup := GStudio.ShellView.Root.Find(NyxInspectorGroupID(nigComponent));
        Click(LGroup.Part(NyxPart('header')).ID);
      end;
    6:
      begin
        LGroup := GStudio.ShellView.Root.Find(NyxInspectorGroupID(nigComponent));
        Check(not NyxDisclosureExpanded(LGroup), 'component options collapse through the shared action');
        {$ifdef PAS2JS}
        Check(document.activeElement = GStudio.ShellView.ControlFor(LGroup.Part(NyxPart('header')).ID),
          'queued collapse safely recovers focus from the hidden name input / actual ' +
          TJSHTMLElement(document.activeElement).getAttribute('data-node') + ' / expected ' +
          LGroup.Part(NyxPart('header')).ID);
        {$endif}
        Click(LGroup.Part(NyxPart('header')).ID);
      end;
    7:
      begin
        Check(ReadName = 'note', 'name draft survives collapse and restoration');
        Check(Pair = GBefore, 'presentation adds no paired history');
        Click(FieldID(nrfCreate));
      end;
    8:
      begin
        LSnapshot := GStudio.ProjectSnapshot;
        LDocument := TNyxCodec.Decode(LSnapshot.Design);
        try
          Check(LDocument.FindComponent('note') <> nil, 'ordinary Create publishes the named definition');
          Check(LDocument.Find('reply').Parent.ID = 'home', 'original subtree remains on its page');
          Check(LDocument.Find('note-title-2') <> nil, 'full copy uses the previewed collision-free ID');
          Check(LDocument.FindComponent('note').Part(NyxPart('actions/send')).Prop('text') = 'Send reply',
            'nested named parts remain usable in the copy');
        finally
          LDocument.Free;
        end;
        Check(Pos('INyxMemo', LSnapshot.Source) > 0, 'generated source retains specialized interfaces');
        Check(Pos('NewNyxMemo', LSnapshot.Source) > 0, 'generated source retains typed factories');
        GCreated := Pair;
        Checkpoint('reusable-copy-created');
      end;
    9:
      begin
        Click('action-undo');
      end;
    10:
      begin
        Check(Pair = GBefore, 'one ordinary Undo restores the exact accepted pair');
        Click('action-redo');
      end;
    11:
      begin
        Check(Pair = GCreated, 'one ordinary Redo restores the exact named copy');
        WriteField(FieldID(nrfName), 'note');
      end;
    12:
      begin
        Click(FieldID(nrfCreate));
      end;
    13:
      begin
        Check(Pair = GCreated, 'occupied definition refuses without another history entry');
        Check(ReadName = 'note', 'refusal retains the entered component name');
        WriteField(FieldID(nrfName), 'pending-copy');
      end;
    14:
      begin
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-design');
        end;
        {$endif}
      end;
    15:
      begin

        {$ifdef PAS2JS}

        if document.querySelector('[data-node="studio-code"]') = nil then
        begin

          if window.innerWidth < 900 then
          begin

            if not GSourceMenuOpened then
            begin
              Click(NyxStudioActionMenuID);
              GSourceMenuOpened := True;
              Exit;
            end;
            Click('studio-menu-view');
            Click('studio-menu-code');
            GSourceMenuOpened := False;
          end
          else
          begin
            Click('action-code');
          end;
        end;
        {$else}

        if GStudio.CodeView.Root = nil then
        begin
          Click('action-code');
        end;
        {$endif}
      end;
    16:
      begin
        WriteField('studio-code', GStudio.ProjectSnapshot.Source + #10 + '// Unfinished reusable draft');
      end;
    17:
      begin
        Check(GStudio.ProjectSnapshot.Pending, 'ordinary source control retains an unfinished draft');
        GDraft := Pair;
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
        {$endif}
      end;
    18:
      begin
        Check(ReadName = 'pending-copy', 'independent name survives source/chrome refresh');
        Click(FieldID(nrfPreview));
      end;
    19:
      begin
        Check(Pos('pending-copy', GStudio.ShellView.Root.Find(FieldID(nrfSummary)).Prop('text')) > 0,
          'read-only preview remains available with unfinished Pascal');
        Check(Pair = GDraft, 'preview preserves exact unfinished draft/base');
        Click(FieldID(nrfCreate));
      end;
    20:
      begin
        Check(Pair = GDraft, 'Create refuses pending Pascal without losing accepted/draft pair');
        Checkpoint('reusable-pending-source');
      end;
    21:
      begin
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-design');
        end;
        {$endif}
      end;
    22:
      begin
        Click('action-reset-source');
      end;
    23:
      begin
        Check(not GStudio.ProjectSnapshot.Pending, 'explicit Restore accepted resolves the draft');
        {$ifdef PAS2JS}

        if document.querySelector('[data-node="action-component"]') = nil then
        begin
          Click('action-panel-inspector');
          Exit;
        end;
        {$endif}
        Click('action-component');
      end;
    24:
      begin
        LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
        try
          Check(LDocument.ComponentCount = 2, 'existing quick Make reusable shortcut remains available');
        finally
          LDocument.Free;
        end;
        GFinished := True;
      end;
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
      raise Exception.Create('Reusable editor timeout at stage ' + IntToStr(GStage));
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending or
      not GStudio.SharedSourceSynchronized then
    begin
      window.setTimeout(@Drive, 20);
      Exit;
    end;

    if GCapture and (document.body.getAttribute('data-capture-checkpoint') <> '') and
      (document.body.getAttribute('data-capture-observed') <>
      document.body.getAttribute('data-capture-checkpoint')) then
    begin
      window.setTimeout(@Drive, 50);
      Exit;
    end;
    Stage;

    if GFinished then
    begin
      FreeAndNil(GStudio);
      GExchange := nil;
      FreeAndNil(GCore);
      document.body.setAttribute('data-reusable-studio', 'passed');
      document.body.setAttribute('data-reusable-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-reusable-studio', 'failed');
      document.body.setAttribute('data-reusable-error', LException.Message);
      document.body.setAttribute('data-reusable-failed-stage', IntToStr(GStage));
      { A capture owns the failing tab until the host has preserved its actual
        UI frame. Other fixture runs retire their independent owners immediately. }

      if not GCapture then
      begin
        FreeAndNil(GStudio);
        GExchange := nil;
        FreeAndNil(GCore);
      end;
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
      raise Exception.Create('Native reusable timeout at stage ' + IntToStr(GStage));
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
    LDocument := BuildNyxDocument;
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    FreeAndNil(LDocument);
    {$ifdef PAS2JS}
    GStarted := TJSDate.now;
    GCapture := window.location.search = '?capture=1';
    GCore := TNyxAgentSession.Create(LPair);
    GCore.Call('nyx_select', 'Reusable workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('workshop-open-page')),
      NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]), 'reusable-workshop');
    GCore.Call('nyx_select', 'Reusable workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('workshop-select')),
      NyxField('id', NyxData('reply')), NyxField('activate', NyxData(False))]), 'reusable-workshop');
    GStudio := TReusableStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    Drive;
    {$else}

    if ParamCount <> 1 then
    begin
      raise Exception.Create('Supply an independent native Studio project directory');
    end;
    GObserver := TFailureObserver.Create;
    Application.Initialize;
    Application.OnException := GObserver.Failed;
    GForm := TForm.Create(nil);
    GForm.SetBounds(30, 30, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
    GStudio.LoadProject(LPair);
    GStudio.Session.Select('reply');
    GStudio.Run;
    while not GFinished do
    begin
      AwaitChanges;
      WriteLn('Ordinary reusable stage ', GStage);
      Flush(Output);
      Stage;
    end;
    WriteLn('PASS ', GChecks, ' ordinary native reusable checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-reusable-studio', 'failed');
      document.body.setAttribute('data-reusable-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
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
