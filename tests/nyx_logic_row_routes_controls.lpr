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

program nyx_logic_row_routes_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.behavior,
  nyx.codec, nyx.codegen, nyx.root.types, nyx.flow.routes, nyx.flow.route.model,
  nyx.flow.declarations, nyx.flow.rows.editor, nyx.logic.editor,
  nyx.studio.projects, nyx.studio.session, nyx.studio.inspector,
  nyx.studio.sourcejobs, nyx.studio.edits, nyx.test.logic.table
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser{$else},
  Interfaces, Forms, Controls, StdCtrls, Graphics, IntfGraphics, FPWritePNG,
  nyx.widgets.lcl, nyx.render.lcl{$endif};

type
  { Ordinary source commands receive the same value-only capture as Studio.
    A callback only changes fields or enqueues a proposal; rebuilds wait for a
    later actual UI turn after publication. }
  TReceiver = class
  public
    Session: TNyxStudioSession;
    Commands: TNyxSourceCommands;
    Failure: TNyxText;
    Captured: Boolean;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
  end;

var
  GReceiver: TReceiver;
  GForms: TNyxDocument;
  GDocument: TNyxDocument;
  GBefore: TNyxProjectPair;
  GAfter: TNyxProjectPair;
  GPending: TNyxProjectPair;
  GBuffered: TNyxText;
  GChecks: Integer;
  GStage: Integer;
  GStarted: Double;
  GDone: Boolean;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  {$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure TReceiver.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LEdit: TNyxStudioDesignEdit;
begin
  try

    if HandleNyxFlowRowsEditor(ANode, GRenderer.Root, Session.Document, 0) then
    begin
      GRenderer.Sync;
    end;

    if CaptureNyxLogicInspector(Session, ANode, GRenderer.Root, LEdit) then
    begin
      Captured := True;
      Commands.Edit(LEdit);
    end;
  except
    on E: Exception do
    begin
      Failure := E.Message;
      GRenderer.Sync;
    end;
  end;
end;

procedure TReceiver.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin
  { Completion is checked on a subsequent turn. }
end;

procedure Change(const AName, AValue: TNyxText);
var
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LInput := GRenderer.InputFor('editor-rows-' + AName);
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
  Check(GRenderer.Root.Find('editor-rows-' + AName).Prop('value') = AValue,
    'actual typed row field input: ' + AName);
end;

procedure Click(const AName: TNyxText);
begin
  {$ifdef PAS2JS}GRenderer.ElementFor('editor-rows-' + AName).click;{$else}
  TNyxLCLButton(GRenderer.ControlFor('editor-rows-' + AName)).Click;{$endif}
end;

procedure Render;
var
  LPage: INyxPage;
  LScroll: INyxScroll;
begin
  FreeAndNil(GRenderer);
  FreeAndNil(GForms);
  GForms := TNyxDocument.Create;
  LPage := NewNyxPage('configuration');
  LScroll := NewNyxScroll('rows-scroll');
  LScroll.Configure.Height(720).Padding(12).Gap(12).Done;
  LScroll.Add(NewNyxFlowRowsEditor('editor-rows', GReceiver.Session.Document, NyxControl('load'), 0));
  LPage.Add(LScroll);
  GForms.AddPage(LPage);
  {$ifdef PAS2JS}GRenderer := TNyxBrowserRenderer.Create;{$else}
  GRenderer := TNyxLCLRenderer.Create;{$endif}
  GRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}GReceiver.Event;
  GRenderer.Render(GForms, GForms.Pages[0], GHost);
end;

procedure Capture(const AName: TNyxText);
{$ifndef PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
{$endif}
begin
  {$ifndef PAS2JS}
  if ParamCount <> 1 then
  begin
    Exit;
  end;
  LBitmap := GHost.GetFormImage;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + String(AName) + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
  {$endif}
end;

procedure Step;
var
  LRoute: TNyxFlowRoute;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
begin

  if Clock - GStarted > 90000 then
  begin
    raise Exception.Create('Row route form deadline at stage ' + IntToStr(GStage));
  end;
  case GStage of
    0:
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-capture-checkpoint', 'mapping-form');

      if document.body.getAttribute('data-capture-observed') <> 'mapping-form' then
      begin
        Exit;
      end;
      {$endif}
      Capture('row-form-native');
      Change('scope', 'Field');
      Change('field', 'rank');
      Check(GRenderer.Root.Find('editor-rows-projection').Prop('value') = 'Decimal text',
        'selecting a field displays its admitted specialized projection');
      Change('field', 'name');
      Click('clear-path');
      Change('part', 'id');
      Click('append');
      Change('numbers', 'Approximate');
      Click('apply');
      Check(GReceiver.Failure = '', 'valid row input captures without UI failure');
      Check(GReceiver.Captured, 'ordinary Studio inspector queues the paired proposal');
    end;
    1:
    begin

      if GReceiver.Commands.Busy then
      begin
        Exit;
      end;
      Check(GReceiver.Commands.State = nssApplied, 'matching source worker publishes the row mapping');
      GAfter := GReceiver.Session.ProjectSnapshot;
      LRoute := NyxControlFlowRoutes(GReceiver.Session.Document.Find('load')).Route(0);
      Check(LRoute.RowOutput(0).Rows.FieldPath(0).ToData.ToJSON = '["id"]',
        'public controls publish the exact structural name path');
      Check(Pos('.OutputRows(', GAfter.Source) > 0, 'paired source keeps crafted typed row authoring');
      GReceiver.Session.Undo;
      Check(EncodeNyxProject(GReceiver.Session.ProjectSnapshot) = EncodeNyxProject(GBefore),
        'one ordinary Undo restores the complete accepted pair');
      GReceiver.Session.Redo;
      Check(EncodeNyxProject(GReceiver.Session.ProjectSnapshot) = EncodeNyxProject(GAfter),
        'one ordinary Redo restores the complete authored pair');
      Click('apply');
      Check((GReceiver.Failure <> '') and
        (EncodeNyxProject(GReceiver.Session.ProjectSnapshot) = EncodeNyxProject(GAfter)),
        'old mounted form refuses after accepted row mapping changes');
      GReceiver.Failure := '';
      Render;
      Change('scope', 'Field');
      Change('field', 'name');
      Change('segment', 'Item');
      Change('part', 'unfinished index');
      Click('append');
      Check((GReceiver.Failure <> '') and
        (GRenderer.Root.Find('editor-rows-part').Prop('value') = 'unfinished index'),
        'malformed actual input remains visible without publication');
      Check(EncodeNyxProject(GReceiver.Session.ProjectSnapshot) = EncodeNyxProject(GAfter),
        'invalid actual input retains complete accepted source/design');
      GBuffered := GAfter.Source + #10 + '// Unfinished Table mapping note';
      GReceiver.Session.SetSourceDraft(GBuffered);
      GPending := GReceiver.Session.ProjectSnapshot;
      Change('numbers', 'Canonical');
      Click('apply');
      {$ifdef PAS2JS}
      GRenderer.InputFor('editor-rows-numbers').scrollIntoView(True);
      {$else}
      TScrollBox(GRenderer.ControlFor('rows-scroll')).VertScrollBar.Position := 600;
      {$endif}
    end;
    2:
    begin
      if GReceiver.Commands.Busy then
      begin
        Exit;
      end;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-capture-checkpoint', 'mapping-paths');

      if document.body.getAttribute('data-capture-observed') <> 'mapping-paths' then
      begin
        Exit;
      end;
      {$endif}
      { The capture acknowledgement may take several turns. Count each semantic
        assertion once, after that barrier rather than once per polling turn. }
      Check(GReceiver.Commands.State = nssRejected, 'pending Pascal draft explicitly rejects grouped row edits');
      Check((GReceiver.Session.DraftSource = GBuffered) and
        (GReceiver.Session.SourceDraftBase = GAfter.Source) and
        (EncodeNyxProject(GReceiver.Session.ProjectSnapshot) = EncodeNyxProject(GPending)),
        'draft rejection preserves complete pair, unfinished text and base');
      GReceiver.Session.DiscardSourceDraft;
      Capture('row-paths-native');
      {$ifndef PAS2JS}
      if ParamCount = 1 then
      begin
        LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
          'nyx.generated.view.pas', fmCreate);
        try
          LFile.WriteBuffer(GAfter.Source[1], Length(GAfter.Source));
        finally
          LFile.Free;
        end;
      end;
      {$endif}
      GDone := True;
    end;
  end;
  Inc(GStage);
end;

procedure Retire;
{$ifndef PAS2JS}
var
  LUntil: Double;
{$endif}
begin
  FreeAndNil(GRenderer);
  FreeAndNil(GForms);
  GReceiver.Commands.Free;
  GReceiver.Session.Free;
  FreeAndNil(GReceiver);
  FreeAndNil(GDocument);
  {$ifndef PAS2JS}
  LUntil := Clock + 150;
  while Clock < LUntil do
  begin
    Application.ProcessMessages;
    CheckSynchronize;
    Sleep(1);
  end;
  FreeAndNil(GHost);
  {$endif}
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      document.body.setAttribute('data-row-form', 'passed');
      document.body.setAttribute('data-row-form-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 5);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-row-form', 'failed');
      document.body.setAttribute('data-row-form-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GDocument := LogicTableRoutedDocument;
    GBefore := NyxProjectPair(TNyxCodec.Encode(GDocument), TNyxCodegen.Generate(GDocument));
    GReceiver := TReceiver.Create;
    GReceiver.Session := TNyxStudioSession.Create(GBefore);
    GReceiver.Session.Select('load');
    GReceiver.Commands := TNyxSourceCommands.Create(GReceiver.Session,
      {$ifdef PAS2JS}@{$endif}GReceiver.Changed);
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(GHost);
    {$else}
    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
    end;
    GHost := TForm.CreateNew(nil);
    GHost.SetBounds(15, 15, 780, 800);
    GHost.Show;
    {$endif}
    Render;
    GStarted := Clock;
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Step;
      Sleep(1);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' actual row-route/source-command checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-row-form', 'failed');
      document.body.setAttribute('data-row-form-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
