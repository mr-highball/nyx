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

program nyx_computed_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.codegen, nyx.controls,
  nyx.data, nyx.state, nyx.binding.types, nyx.expressions, nyx.expressions.editor,
  nyx.disclosure, nyx.studio.inspector, nyx.studio.inspector.groups,
  nyx.studio.authoring, nyx.studio.menu, nyx.studio.projects, nyx.generated.view,
  {$ifdef PAS2JS}nyx.studio.browser, nyx.studio.agents, nyx.studio.exchange,
  nyx.test.editor.exchange{$else}nyx.studio.lcl{$endif};

{$ifdef PAS2JS}
type
  { Only asynchronous transport is injected. Stock Nyx Studio controls, source
    commands, history and presentation are exercised with an independent core. }
  TExpressionStudio = class(TNyxStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;
{$else}
type
  TControlAccess = class(TControl);
  TFailureObserver = class
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

var
  {$ifdef PAS2JS}
  GStudio: TExpressionStudio;
  GCore: TNyxAgentSession;
  GExchange: TNyxTestEditorExchange;
  GStarted: Double;
  GCapture: Boolean;
  GMenuOpened: Boolean;
  GInspectorOpened: Boolean;
  {$else}
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  {$endif}
  GStage: Integer;
  GChecks: Integer;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GPending: TNyxText;
  GDraft: TNyxText;
  GFinished: Boolean;

{$ifdef PAS2JS}
function TExpressionStudio.CreateEditorExchange: TNyxStudioEditorExchange;
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
    raise Exception.Create('Ordinary expression callback: ' + GObserver.Error);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Ordinary expression Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.ProjectSnapshot);
end;

function FieldID(AField: TNyxExpressionEditorField): TNyxText;
begin
  Result := NyxExpressionEditorFieldID(NyxStudioExpressionEditorID, AField);
end;

procedure Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
  LBounds: TJSDOMRect;
begin
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  Check(LControl <> nil, 'actual button exists / ' + AID);
  LBounds := LControl.getBoundingClientRect;
  Check((LBounds.width > 0) and (LBounds.height > 0), 'actual button is displayed / ' + AID);
  LControl.scrollIntoView(False);
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
  Check((LControl <> nil) and LControl.Visible, 'actual native button exists / ' + AID);
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

  if LControl.tagName <> 'TEXTAREA' then
  begin
    LControl := TJSHTMLElement(LControl.querySelector('textarea'));
  end;
  Check(LControl <> nil, 'actual writable source field exists / ' + AID);
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
  Check((LControl <> nil) and (LControl is TMemo), 'actual native source memo exists / ' + AID);
  TMemo(LControl).Text := AValue;
end;
{$endif}

function ReadSource: TNyxText;
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
begin
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + FieldID(eefSource) + '"]'));
  Check(LControl <> nil, 'expression source remains mounted');

  if LControl.tagName <> 'TEXTAREA' then
  begin
    LControl := TJSHTMLElement(LControl.querySelector('textarea'));
  end;
  Result := TJSHTMLTextAreaElement(LControl).value;
end;
{$else}
var
  LLines: TNyxStrings;
begin
  { Native memo storage uses CRLF; the portable editing contract already admits
    LF. Preserve UTF-8 through Nyx's text collection rather than ANSI RTL replace. }
  LLines := TNyxStrings.Create;
  try
    LLines.Text := TNyxText(TMemo(GStudio.ShellView.InputFor(FieldID(eefSource))).Text);
    Result := LLines.Text;

    if (Result <> '') and (Result[Length(Result)] = #10) then
    begin
      Delete(Result, Length(Result), 1);
    end;
  finally
    LLines.Free;
  end;
end;
{$endif}

procedure Checkpoint(const AName: TNyxText);
begin
  {$ifdef PAS2JS}

  if GCapture then
  begin
    TJSHTMLElement(document.querySelector('[data-node="' + FieldID(eefSource) + '"]'))
      .scrollIntoView(False);
    document.body.setAttribute('data-capture-checkpoint', AName);
  end;
  {$endif}
end;

procedure Stage;
var
  LGroup: TNyxNode;
  LDocument: TNyxDocument;
  LSpec: TNyxBindingSpec;
begin
  case GStage of
    0:
      begin
        GBefore := Pair;
        {$ifdef PAS2JS}

        if (window.innerWidth < 900) and not GInspectorOpened then
        begin
          Click('action-panel-inspector');
          { Pane publication is queued. Wait for the ordinary presentation gate
            before addressing controls which belong to that pane. }
          GInspectorOpened := True;
          Exit;
        end;
        {$endif}
        Click(NyxInspectorPropertiesID);
      end;
    1:
      begin
        Click(NyxStudioBindingsToggleID);
      end;
    2:
      begin
        Check(GStudio.ShellView.Root.Find(NyxStudioExpressionEditorID) <> nil,
          'stock Inspector mounts the specialized public expression form');
        GDraft := TNyxCodegen.Expression(NyxTextExpression('Subtotal: ').Append(
          NyxIntegerExpression(NyxIntegerState('quantity')).Times(NyxIntegerExpression(3)).AsText));
        WriteField(FieldID(eefSource), GDraft);
      end;
    3:
      begin
        Click(FieldID(eefPreview));
      end;
    4:
      begin
        Check(Pos('Default preview: Subtotal: 9',
          GStudio.ShellView.Root.Find(FieldID(eefSummary)).Prop('text')) > 0,
          'ordinary Preview evaluates real typed defaults');
        Check(Pair = GBefore, 'Preview preserves the accepted pair');
        Check(ReadSource = GDraft, 'Preview preserves the source draft / actual ' +
          NyxData(ReadSource).ToJSON + ' / expected ' + NyxData(GDraft).ToJSON);
        LGroup := GStudio.ShellView.Root.Find(NyxInspectorGroupID(nigData));
        Click(LGroup.Part(NyxPart('header')).ID);
      end;
    5:
      begin
        LGroup := GStudio.ShellView.Root.Find(NyxInspectorGroupID(nigData));
        Check(not NyxDisclosureExpanded(LGroup), 'data disclosure collapses the form');
        Click(LGroup.Part(NyxPart('header')).ID);
      end;
    6:
      begin
        Check(ReadSource = GDraft, 'collapse/reopen retains the exact expression draft');
        Click(FieldID(eefApply));
      end;
    7:
      begin
        LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
        try
          Check(LDocument.Find('order-total').FindBinding(bpText, LSpec) and
            (LSpec.Source = bsExpression), 'ordinary Apply publishes a typed computed binding');
          Check((LSpec.Expression as INyxTextExpression)
            .Evaluate(CaptureNyxExpressionInputs(LDocument.State)) = 'Subtotal: 9',
            'applied expression retains its computed meaning');
        finally
          LDocument.Free;
        end;
        Check(Pos('.Times(', GStudio.ProjectSnapshot.Source) > 0,
          'ordinary Apply publishes readable fluent Pascal alongside the design');
        GAfter := Pair;
        Checkpoint('expression-applied');
      end;
    8:
      begin
        Click('action-undo');
      end;
    9:
      begin
        Check(Pair = GBefore, 'one ordinary Undo restores the exact previous pair');
        Click('action-redo');
      end;
    10:
      begin
        Check(Pair = GAfter, 'one ordinary Redo restores the complete computed pair');
        WriteField(FieldID(eefSource), 'NyxTextExpression(''unfinished');
      end;
    11:
      begin
        Click(FieldID(eefApply));
      end;
    12:
      begin
        Check((Pair = GAfter) and (ReadSource = 'NyxTextExpression(''unfinished'),
          'invalid Apply retains accepted pair and exact expression input');
        GDraft := TNyxCodegen.Expression(NyxTextExpression('A pending caption 🌙'));
        WriteField(FieldID(eefSource), GDraft);
      end;
    13:
      begin
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-design');
        end;
        {$endif}
      end;
    14:
      begin
        {$ifdef PAS2JS}

        if document.querySelector('[data-node="studio-code"]') = nil then
        begin

          if window.innerWidth < 900 then
          begin

            if not GMenuOpened then
            begin
              Click(NyxStudioActionMenuID);
              GMenuOpened := True;
              Exit;
            end;
            Click('studio-menu-view');
            Click('studio-menu-code');
            GMenuOpened := False;
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
    15:
      begin
        WriteField('studio-code', GStudio.ProjectSnapshot.Source + #10 +
          TNyxText('// Unfinished expression workshop 🌙'));
      end;
    16:
      begin
        Check(GStudio.ProjectSnapshot.Pending, 'ordinary Pascal field retains its unfinished draft');
        GPending := Pair;
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
        {$endif}
      end;
    17:
      begin
        Check(ReadSource = GDraft, 'expression input survives unrelated source/pane reconstruction');
        Click(FieldID(eefPreview));
      end;
    18:
      begin
        Check((Pair = GPending) and (Pos('A pending caption',
          GStudio.ShellView.Root.Find(FieldID(eefSummary)).Prop('text')) > 0),
          'readonly Preview retains accepted application pair and pending Pascal');
        Click(FieldID(eefApply));
      end;
    19:
      begin
        Check((Pair = GPending) and (ReadSource = GDraft),
          'pending Pascal refuses expression Apply without losing either buffer');
        Checkpoint('expression-pending-source');
        Click(NyxStudioBindingsToggleID);
      end;
    20:
      begin
        Click(NyxStudioBindingsToggleID);
      end;
    21:
      begin
        Check((Pair = GPending) and (ReadSource = GDraft),
          'closing/reopening Bindings restores its parked source draft');
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
        Check(not GStudio.ProjectSnapshot.Pending, 'ordinary Restore resolves application Pascal');
        {$ifdef PAS2JS}

        if window.innerWidth < 900 then
        begin
          Click('action-panel-inspector');
        end;
        {$endif}
      end;
    24:
      begin
        Check(ReadSource = GDraft, 'application Restore retains the independent expression draft');
        GAfter := Pair;
        Click('binding-clear');
      end;
    25:
      begin
        LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
        try
          Check(not LDocument.Find('order-total').FindBinding(bpText, LSpec),
            'ordinary Unbind removes effective computed behavior');
        finally
          LDocument.Free;
        end;
        Click('action-undo');
      end;
    26:
      begin
        Check(Pair = GAfter, 'Unbind is independently undoable with its exact Pascal');
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
      raise Exception.Create('Expression Studio timeout at stage ' + IntToStr(GStage));
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending or not GStudio.SharedSourceSynchronized then
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
      document.body.setAttribute('data-computed-studio', 'passed');
      document.body.setAttribute('data-computed-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-computed-studio', 'failed');
      document.body.setAttribute('data-computed-error', LException.Message);
      document.body.setAttribute('data-computed-stage', IntToStr(GStage));

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
      raise Exception.Create('Native expression timeout at stage ' + IntToStr(GStage));
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
    GCore.Call('nyx_select', 'Expression workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('expression-open-page')),
      NyxField('id', NyxData('home')), NyxField('activate', NyxData(True))]));
    GCore.Call('nyx_select', 'Expression workshop', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('expression-select-label')),
      NyxField('id', NyxData('order-total')), NyxField('activate', NyxData(False))]));
    GStudio := TExpressionStudio.Create;
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
    GStudio.Session.Select('order-total');
    GStudio.Run;
    while not GFinished do
    begin
      AwaitChanges;
      WriteLn('Ordinary expression stage ', GStage);
      Flush(Output);
      Stage;
    end;
    WriteLn('PASS ', GChecks, ' ordinary native expression checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-computed-studio', 'failed');
      document.body.setAttribute('data-computed-error', LException.Message);
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
