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

program nyx_program_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef NYX_CONTROL_ROUTES_STUDIO}{$define NYX_PROGRAM_LOGIC_STUDIO}{$endif}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec,
  nyx.studio.projects, nyx.studio.sourcejobs, nyx.studio.menu,
  nyx.programs.types, nyx.programs.contract, nyx.programs.declarations
  {$ifdef NYX_PROGRAM_LOGIC_STUDIO}, nyx.codegen, nyx.flow.types,
    nyx.flow.declarations, nyx.test.programs.logic{$endif}
  {$ifdef NYX_CONTROL_ROUTES_STUDIO}, nyx.types, nyx.controls, nyx.flow.routes,
    nyx.flow.route.model, nyx.flow.controls.editor, nyx.studio.edits,
    nyx.studio.transactions, nyx.studio.agents, nyx.studio.hierarchy,
    nyx.collections, nyx.bytes{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, nyx.studio.lcl{$endif};

const
  CProgramName: TNyxText = 'Welcome behavior';
  CSource: TNyxText = 'begin Caption := ''Hello, '' + Name end.';
  CInvalid: TNyxText = 'begin Missing := 1 end.';

{$ifndef PAS2JS}
type
  { Borrows a mounted control only for its inherited Click input. Studio owns
    every widget and may replace it after the callback returns. }
  TControlInput = class(TControl);

  { Fixture-owned exception receiver prevents Lazarus modal error dialogs from
    hiding failed callbacks. It outlives Studio and never retains a control. }
  TCallbackFailure = class
  public
    Error: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GBefore: TNyxText;
  GAfter: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GDone: Boolean;
  {$ifndef PAS2JS}
  GForm: TForm;
  GFailure: TCallbackFailure;
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TCallbackFailure.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

function Node(const AID: TNyxText): TNyxNode;
var
  LRoot: TNyxNode;
begin
  LRoot := GStudio.ShellView.RootFor(AID);

  if LRoot = nil then
  begin
    raise Exception.Create('Missing mounted program section: ' + AID);
  end;
  Result := LRoot.Find(AID);

  if Result = nil then
  begin
    raise Exception.Create('Missing mounted program control: ' + AID);
  end;
end;

procedure Click(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  GStudio.ShellView.ElementFor(AID).click;
  {$else}
  TControlInput(GStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

procedure Change(const AName, AText: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LControl: TJSHTMLElement;{$else}LControl: TControl;{$endif}
begin
  {$ifdef NYX_PROGRAM_LOGIC_STUDIO}
  LID := 'studio-logic-editor-' + AName;
  {$else}
  LID := 'studio-program-editor-' + AName;
  {$endif}
  LControl := GStudio.ShellView.InputFor(LID);
  Check(LControl <> nil, 'Program proposal uses a mounted target input');
  {$ifdef PAS2JS}

  if LControl is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LControl).value := AText;
    LControl.dispatchEvent(TJSEvent.new('change'));
  end
  else if LControl is TJSHTMLTextAreaElement then
  begin
    TJSHTMLTextAreaElement(LControl).value := AText;
    LControl.dispatchEvent(TJSEvent.new('input'));
  end
  else
  begin
    TJSHTMLInputElement(LControl).value := AText;
    LControl.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}

  if LControl is TComboBox then
  begin
    TComboBox(LControl).ItemIndex := TComboBox(LControl).Items.IndexOf(AText);
    TComboBox(LControl).OnChange(LControl);
  end
  else if LControl is TMemo then
  begin
    TMemo(LControl).Text := AText;
  end
  else
  begin
    TEdit(LControl).Text := AText;
  end;
  {$endif}
  Check(Node(LID).Prop('value') = AText, 'Physical input retains exact program proposal text');
end;

procedure History(ARedo: Boolean);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LFace: TJSHTMLElement;{$endif}
begin
  LID := 'action-undo';

  if ARedo then
  begin
    LID := 'action-redo';
  end;
  {$ifdef PAS2JS}
  LFace := GStudio.ShellView.ElementFor(LID);

  if (LFace = nil) or (LFace.getBoundingClientRect.height = 0) then
  begin
    Click(NyxStudioActionMenuID);

    if ARedo then
    begin
      Click('studio-menu-redo');
    end
    else
    begin
      Click('studio-menu-undo');
    end;
    Exit;
  end;
  {$endif}
  Click(LID);
end;

{ A target resize may mount compact navigation after the initial frame. Follow
  the actual Project input and wait for its resulting section, rather than
  assuming the first frame has the eventual viewport or clicking a hidden face. }
function OpenPrograms: Boolean;
const
  CAction = {$ifdef NYX_PROGRAM_LOGIC_STUDIO}'action-logic-toggle'{$else}'action-programs-toggle'{$endif};
begin
  Result := False;

  if GStudio.ShellView.Root.Find(CAction) = nil then
  begin

    if GStudio.ShellView.Root.Find('action-panel-project') <> nil then
    begin
      Click('action-panel-project');
    end;
    Exit;
  end;
  Click(CAction);
  Result := True;
end;

{$ifdef NYX_CONTROL_ROUTES_STUDIO}
{$I nyx.test.control.routes.studio.inc}
{$else}
{$ifdef NYX_PROGRAM_LOGIC_STUDIO}
{$I nyx.test.program.logic.studio.inc}
{$else}
procedure Step;
var
  LDocument: TNyxDocument;
  LDefinition: INyxProgramDefinition;
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if Clock - GStarted > 120000 then
  begin
    raise Exception.Create('Actual Studio program input deadline at stage ' + IntToStr(GStage));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  case GStage of
    0:
    begin
      {$ifdef PAS2JS}

      if document.querySelector('[data-node="action-panel-project"]') <> nil then
      begin
        Click('action-panel-project');
      end;
      {$endif}
    end;
    1:
    begin
      GBefore := EncodeNyxProject(GStudio.ProjectSnapshot);

      if not OpenPrograms then
      begin
        Exit;
      end;
    end;
    2:
    begin
      Check(Node('studio-program-editor-source').Prop('value') <> '',
        'Ordinary Studio consumes the public Nyx program editor');
      Change('name', CProgramName);
      Change('port-name', 'Name');
      Click('studio-program-editor-add-port');
      Change('port-name', 'Caption');
      Change('port-direction', 'Output');
      Click('studio-program-editor-add-port');
      Change('source', CSource);
      Click('studio-program-editor-check');
      Check((Pos('Program checked.', Node('studio-program-editor-feedback').Prop('text')) = 1) and
        (EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore),
        'Actual Check compiles the local proposal without running or publishing');
      {$ifdef PAS2JS}

      if GStudio.ShellView.Root.Find('action-panel-design') <> nil then
      begin
        Click('action-panel-design');
      end
      else
      {$endif}
      begin
        Click('action-programs-close');
      end;
    end;
    3:
    begin

      if not OpenPrograms then
      begin
        Exit;
      end;
    end;
    4:
    begin
      Check((Node('studio-program-editor-source').Prop('value') = CSource) and
        (Node('studio-program-editor-name').Prop('value') = CProgramName) and
        (Pos('Caption', Node('studio-program-editor-ports').Prop('text')) > 0),
        'Closing and reopening retains unfinished source, name and typed ports');
      Click('studio-program-editor-apply');
    end;
    5:
    begin
      Check(GStudio.SourceCommands.State = nssApplied,
        'Actual Save admits through ordinary source preparation: ' + GStudio.SourceCommands.Message);
      GAfter := EncodeNyxProject(GStudio.ProjectSnapshot);
      LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
      try
        LDefinition := LDocument.Programs.Definition(NyxProgramRef(CProgramName));
        Check((LDefinition.Source = CSource) and (LDefinition.Contract.PortCount = 2),
          'Saved program source and contract belong to the active design');
        Check(LDefinition.Compile.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Name'),
          NyxProgramValue('Ada'))).Outputs.Value(NyxProgramSymbol('Caption')).AsText = 'Hello, Ada',
          'Explicit invocation of the UI-authored saved definition produces real typed output');
      finally
        LDocument.Free;
      end;
      Check((GAfter <> GBefore) and
        (Pos('NewNyxProgramDefinition(', GStudio.ProjectSnapshot.Source) > 0),
        'The same Save produces crafted typed Pascal alongside the design');
      History(False);
    end;
    6:
    begin
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
        'One actual Undo restores the complete accepted pair before program creation');
      History(True);
    end;
    7:
    begin
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
        'One actual Redo restores exact source and saved program metadata');
      Change('source', CInvalid);
      Click('studio-program-editor-check');
      Check(Pos('Line ', Node('studio-program-editor-feedback').Prop('text')) = 1,
        'Actual local Check exposes an invalid program diagnostic beside its source');
      Click('studio-program-editor-apply');
    end;
    8:
    begin
      Check((EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter) and
        (Node('studio-program-editor-source').Prop('value') = CInvalid),
        'Rejected Save retains the exact accepted pair and invalid local proposal');
      Click('studio-program-editor-restore');
      Check(Node('studio-program-editor-source').Prop('value') = CSource,
        'Actual Restore returns the proposal to its accepted definition');
      Click('studio-program-editor-remove');
    end;
    9:
    begin
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
        'Unconfirmed removal preserves both accepted artifacts and history');
      {$ifdef PAS2JS}
      GStudio.ShellView.InputFor('studio-program-editor-confirm').click;
      {$else}
      TCheckBox(GStudio.ShellView.InputFor('studio-program-editor-confirm')).Checked := True;
      {$endif}
      Click('studio-program-editor-remove');
    end;
    10:
    begin
      Check(GStudio.SourceCommands.State = nssApplied,
        'Confirmed removal enters the ordinary paired command');
      LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
      try
        Check(not LDocument.Programs.Contains(NyxProgramRef(CProgramName)),
          'Confirmed actual input removes the exact saved declaration');
      finally
        LDocument.Free;
      end;
      History(False);
    end;
    11:
    begin
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
        'One actual Undo restores the removed definition and Pascal exactly');
      GDone := True;
    end;
  end;
  Inc(GStage);
end;
{$endif}
{$endif}

procedure Drive;
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
{$endif}
begin
  try
    Step;

    if GDone then
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-studio', 'passed');
      document.body.setAttribute('data-program-studio-checks', IntToStr(GChecks));
      {$else}
      WriteLn('PASS ', GChecks, ' actual Studio program/paired input checks');

      if ParamCount = 2 then
      begin
        LBitmap := GForm.GetFormImage;
        LPNG := TPortableNetworkGraphic.Create;
        try
          LPNG.Assign(LBitmap);
          LPNG.SaveToFile(ParamStr(2));
        finally
          LPNG.Free;
          LBitmap.Free;
        end;
      end;
      {$endif}
      Exit;
    end;
    {$ifdef PAS2JS}window.setTimeout(@Drive, 25);{$endif}
  except
    on E: Exception do
    begin
      GDone := True;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-studio', 'failed');
      document.body.setAttribute('data-program-studio-error', E.Message);
      document.body.setAttribute('data-program-studio-stage', IntToStr(GStage));
      {$else}
      WriteLn('FAIL stage ', GStage, ' / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  {$ifndef PAS2JS}

  if (ParamCount < 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned Studio project directory');
  end;
  Application.Initialize;
  GFailure := TCallbackFailure.Create;
  Application.OnException := GFailure.Failed;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(20, 20, 1280, 900);
  GForm.Show;
  GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
  GStudio.Run;
  {$else}
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  {$endif}
  GStarted := Clock;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  try
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Drive;
      Sleep(25);
    end;
  finally
    GStudio.Free;
    GForm.Free;
    Application.OnException := nil;
    GFailure.Free;
  end;
  {$endif}
end.
