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
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.composition, nyx.parts.editor, nyx.studio.inspector,
  nyx.studio.menu, nyx.studio.projects, nyx.studio.session,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange{$else}nyx.studio.lcl{$endif};

type
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
  {$endif}
  GStage, GChecks: Integer;
  GInitial, GHidden, GDefinition, GOther: TNyxText;
  GFinished: Boolean;

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
    1: Click(ActionID('actions', npaHide));
    2:
      begin
        CheckParts('actions', True);
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
        Click(ActionID('actions', npaRestore));
      end;
    5:
      begin
        Check(Pair = GInitial, 'Restore original removes only the local descriptor');
        CheckParts('actions', False);
        Click('action-undo');
      end;
    6:
      begin
        Check(Pair = GHidden, 'Restore is one independently undoable operation');
        Click('action-redo');
      end;
    7:
      begin
        Check(Pair = GInitial, 'Restore Redo retains the complete pair');
        Click(ActionID('actions/send', npaHide));
      end;
    8:
      begin
        CheckParts('actions/send', True);
        CheckParts('actions', False);
        Click(ActionID('actions/send', npaRestore));
      end;
    9:
      begin
        Check(Pair = GInitial, 'nested Restore retains parent and definition');
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
        LInput := TWinControl(GStudio.CodeView.InputFor('studio-code'));
        Check(LInput is TMemo, 'actual Pascal editor is available');
        TMemo(LInput).Text := GStudio.ProjectSnapshot.Source + #10 + '// Unfinished parts draft';
      end;
    13:
      begin
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
