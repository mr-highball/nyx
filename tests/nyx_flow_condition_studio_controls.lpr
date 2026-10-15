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

program nyx_flow_condition_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.codec,
  nyx.codegen, nyx.flow.types, nyx.flow.editing, nyx.flow.declarations,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.menu,
  nyx.test.flow.conditionfixture
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.studio.lcl{$endif};

{$ifndef PAS2JS}
type
  { Borrows one renderer-owned widget to expose its inherited click. The real
    Studio adapter still owns dispatch, model updates and widget retirement. }
  TControlInput = class(TControl);

  { Outlives the owned Studio and records callback failures without opening a
    blocking Lazarus exception dialog. It owns no widgets or exception objects. }
  TCallbackFailure = class
  public
    Error: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

const
  CForm = 'studio-logic-editor-condition-editor';

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GSeed: TNyxProjectPair;
  GFalse: TNyxProjectPair;
  GTrue: TNyxProjectPair;
  GClear: TNyxProjectPair;
  GPending: TNyxProjectPair;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GStepStarted: Double;
  GDone: Boolean;
  {$ifndef PAS2JS}
  GHost: TForm;
  GFailure: TCallbackFailure;
  {$endif}

{$ifndef PAS2JS}
procedure TCallbackFailure.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

procedure Check(ACondition: Boolean; const AMessage: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

function Seed: TNyxProjectPair;
var
  LDoc: TNyxDocument;
  LAgent: TNyxAgentSession;
begin
  LDoc := ConditionalFileDocument;
  LAgent := nil;
  try
    { This explicit owned in-process semantic context removes only the condition
      that ordinary controls will author. It cannot address the observing LAN. }
    LAgent := TNyxAgentSession.Create(NyxProjectPair(TNyxCodec.Encode(LDoc),
      TNyxCodegen.Generate(LDoc)));
    LAgent.Call('nyx_transaction', 'Condition editor input', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('prepare-condition-editor')),
      NyxField('operations', NyxArray([NyxClearFlowCondition(NyxFlowRef('Read report'),
        NyxFlowStep('decode')).Expect(NyxFlowBaseline(LDoc.Flows,
          NyxFlowRef('Read report'))).ToData]))]));
    Result := LAgent.PreviewPair(LAgent.Revision, 'home');
  finally
    LAgent.Free;
    LDoc.Free;
  end;
end;

function Node(const AID: TNyxText): TNyxNode;
var
  LRoot: TNyxNode;
begin
  LRoot := GStudio.ShellView.RootFor(AID);

  if LRoot = nil then
  begin
    raise Exception.Create('Unmapped Studio section: ' + String(AID));
  end;
  Result := LRoot.Find(AID);

  if Result = nil then
  begin
    raise Exception.Create('Unmapped Studio control: ' + String(AID));
  end;
end;

procedure Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LFace := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if LFace = nil then
  begin
    raise Exception.Create('Missing browser input: ' + String(AID));
  end;
  LFace.scrollIntoView(TJSObject.new);
  LFace.click;
  {$else}
  TControlInput(GStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

procedure History(AUndo: Boolean);
begin
  {$ifdef PAS2JS}
  { Compact chrome deliberately hides toolbar history faces. Use its real
    managed menu; this is software host input, never a hardware-input claim. }
  Click(NyxStudioActionMenuID);

  if AUndo then
  begin
    Click('studio-menu-undo');
  end
  else
  begin
    Click('studio-menu-redo');
  end;
  {$else}

  if AUndo then
  begin
    Click(NyxStudioMenuActionTarget(smaUndo));
  end
  else
  begin
    Click(NyxStudioMenuActionTarget(smaRedo));
  end;
  {$endif}
end;

procedure ShowProject;
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LFace := TJSHTMLElement(document.querySelector('[data-node="action-panel-project"]'));

  if LFace <> nil then
  begin
    Click('action-panel-project');
  end;
  {$endif}
end;

procedure Choose(const AField, AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLSelectElement;
  {$else}
  LInput: TComboBox;
  {$endif}
begin
  LID := CForm + '-' + AField;
  LInput := {$ifdef PAS2JS}TJSHTMLSelectElement{$else}TComboBox{$endif}(
    GStudio.ShellView.InputFor(LID));
  Check(LInput <> nil, 'actual condition selector is mounted');
  {$ifdef PAS2JS}
  LInput.value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput.ItemIndex := LInput.Items.IndexOf(AValue);
  LInput.OnChange(LInput);
  {$endif}
  Check(Node(LID).Prop('value') = AValue, 'ordinary adapter retains exact selector input');
end;

procedure Toggle(const AField: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$else}
  LInput: TCheckBox;
  {$endif}
begin
  LID := CForm + '-' + AField;
  LInput := {$ifdef PAS2JS}TJSHTMLInputElement{$else}TCheckBox{$endif}(
    GStudio.ShellView.InputFor(LID));
  Check(LInput <> nil, 'actual condition checkbox is mounted');
  {$ifdef PAS2JS}
  LInput.click;
  {$else}
  LInput.Checked := not LInput.Checked;
  {$endif}
end;

function DecodeCondition(const APair: TNyxProjectPair): TNyxDataValue;
var
  LDoc: TNyxDocument;
  LItems: TNyxDataValue;
  LIndex: Integer;
begin
  Result := NyxNull;
  LDoc := TNyxCodec.Decode(APair.Design);
  try
    LItems := LDoc.Flows.Definition(NyxFlowRef('Read report')).ToData.Field('conditions');
    for LIndex := 0 to LItems.Count - 1 do
    begin

      if LItems.Item(LIndex).Field('step').AsText = 'decode' then
      begin
        Exit(LItems.Item(LIndex));
      end;
    end;
  finally
    LDoc.Free;
  end;
end;

procedure Step;
var
  LCondition: TNyxDataValue;
  {$ifndef PAS2JS}
  LFile: TFileStream;
  LBytes: TNyxBytes;
  {$endif}
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if (Clock - GStarted > 600000) or (Clock - GStepStarted > 120000) then
  begin
    raise Exception.Create('Condition Studio deadline at stage ' + IntToStr(GStage));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-condition-studio-stage', IntToStr(GStage));
  {$else}
  WriteLn('Condition Studio stage ', GStage, ' / elapsed milliseconds ',
    Round(Clock - GStarted));
  Flush(Output);
  {$endif}
  case GStage of
    0:
      begin
        GStudio.SourceCommands.OpenProject(GSeed);
      end;
    1:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSeed),
          'ordinary compiler-independent Open retains the exact semantic seed');
        ShowProject;
      end;
    2:
      begin
        Click('action-logic-toggle');
      end;
    3:
      begin
        Choose('step', '2 / decode');
        Click(CForm + '-load');
      end;
    4:
      begin
        Check(Pos('No explicit condition', Node(CForm + '-summary').Prop('text')) > 0,
          'ordinary form displays the actual saved absence');
        Choose('source', '1 / choose / read');
        Toggle('expected');
        Click(CForm + '-set');
      end;
    5:
      begin
        GFalse := GStudio.ProjectSnapshot;
        LCondition := DecodeCondition(GFalse);
        Check((LCondition.Kind <> ndNull) and not LCondition.Field('expected').AsBoolean,
          'real controls publish the typed opposite condition');
        Check(Pos('False)', GFalse.Source) > 0, 'ordinary publication updates adjacent crafted Pascal');
        History(True);
      end;
    6:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSeed),
          'one actual Undo restores the exact seed pair');
        History(False);
      end;
    7:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GFalse),
          'one actual Redo restores the exact condition pair');
        Choose('step', '2 / decode');
        Click(CForm + '-load');
        Toggle('expected');
        Click(CForm + '-set');
      end;
    8:
      begin
        GTrue := GStudio.ProjectSnapshot;
        Check(DecodeCondition(GTrue).Field('expected').AsBoolean,
          'ordinary checkbox can deliberately select the successful read branch');
        Choose('step', '2 / decode');
        Click(CForm + '-load');
        Click(CForm + '-clear');
      end;
    9:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GTrue),
          'unconfirmed clear preserves the complete accepted source and design');
        Check(Node(CForm + '-source').Prop('value') = '1 / choose / read',
          'refused clearing preserves unfinished form choices');
        Toggle('confirm-clear');
        Click(CForm + '-clear');
      end;
    10:
      begin
        GClear := GStudio.ProjectSnapshot;
        Check(DecodeCondition(GClear).Kind = ndNull,
          'explicit confirmation permits one ordinary condition removal');
        History(True);
      end;
    11:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GTrue),
          'condition removal Undo restores the exact pair');
        History(False);
      end;
    12:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GClear),
          'condition removal Redo restores the exact pair');
        History(True);
      end;
    13:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GTrue),
          'the final useful report graph remains the UI-authored accepted pair');
        Choose('step', '2 / decode');
        Click(CForm + '-load');
        Toggle('expected');
        Click('action-logic-close');
      end;
    14:
      begin
        Click('action-logic-toggle');
      end;
    15:
      begin
        Check(Node(CForm + '-expected').Prop('value') = 'false',
          'hiding and reopening Logic retains unfinished Boolean intent');
        Check(Pos('Saved condition: Runs when choose / read is True.',
          Node(CForm + '-summary').Prop('text')) > 0,
          'the caption distinguishes accepted meaning from unfinished input');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GTrue),
          'local form presentation never changes the accepted pair');
        {$ifndef PAS2JS}
        LBytes := NyxEncodeUTF8(GTrue.Source);
        LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
          'nyx.generated.view.pas', fmCreate);
        try
          LFile.WriteBuffer(LBytes[0], Length(LBytes));
        finally
          LFile.Free;
        end;
        {$endif}
        GPending := GTrue;
        GPending.Pending := True;
        GPending.DraftBase := GTrue.Source;
        GPending.Draft := GTrue.Source + TNyxText(#10 + '// Unfinished report 🌙');
        GStudio.SourceCommands.OpenProject(GPending);
      end;
    16:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GPending),
          'ordinary Open admits and retains the separate unfinished source buffer');
        ShowProject;
      end;
    17:
      begin

        if GStudio.ShellView.RootFor(CForm) = nil then
        begin
          Click('action-logic-toggle');
        end;
      end;
    18:
      begin
        Choose('step', '2 / decode');
        Click(CForm + '-load');
        Toggle('expected');
        Click(CForm + '-set');
      end;
    19:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GPending),
          'pending source refuses a condition edit without losing pair/draft/base');
        {$ifdef PAS2JS}
        TJSHTMLElement(document.querySelector('[data-node="' + CForm + '-title"]'))
          .scrollIntoView(TJSObject.new);
        {$endif}
        GDone := True;
      end;
  end;
  Inc(GStage);
  GStepStarted := Clock;
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-condition-studio', 'passed');
      document.body.setAttribute('data-condition-studio-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 30);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-condition-studio', 'failed');
      document.body.setAttribute('data-condition-studio-error', E.Message);
      document.body.setAttribute('data-event-error', E.Message);
      document.body.setAttribute('data-condition-studio-stage', IntToStr(GStage));
    end;
  end;
end;
{$endif}

begin
  GSeed := Seed;
  {$ifndef PAS2JS}

  if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned Studio output/project directory');
  end;
  Application.Initialize;
  GFailure := TCallbackFailure.Create;
  Application.OnException := GFailure.Failed;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1280, 900);
  GHost.Show;
  GStudio := TNyxNativeStudio.Create(GHost, ParamStr(1));
  GStudio.Run;
  {$else}
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  {$endif}
  GStarted := Clock;
  GStepStarted := GStarted;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  try
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Step;
      Sleep(30);
    end;
    WriteLn('PASS ', GChecks, ' actual ordinary Studio condition checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GHost.Free;
  Application.OnException := nil;
  GFailure.Free;
  {$endif}
end.
