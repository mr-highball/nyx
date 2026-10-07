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

program nyx_menu_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Spin, Graphics, IntfGraphics,
  FPWritePNG, nyx.render.lcl,
  nyx.studio.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.menu.editor,
  nyx.menu.declarations, nyx.menu.types, nyx.popover.types, nyx.typeahead,
  nyx.data, nyx.codec, nyx.schema, nyx.behavior, nyx.studio.projects, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.studio.view, nyx.studio.inspector,
  nyx.generated.view;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TControlAccess = class(TControl);
  {$endif}

  { The exact MCP companion is compiled unchanged. The harness consumes Studio's
    ordinary shared inspector and paired queue; actual target controls provide
    input. It owns its entire pair/host and never imports into a user's service. }
  TReview = class
  private
    FHost: THost;
    FRenderer: TRenderer;
    FShell: TNyxDocument;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FState: TNyxStudioViewState;
    FStage: Integer;
    FBefore: TNyxText;
    FAfter: TNyxText;
    procedure Refresh;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Change(const APrefix: TNyxText; AField: TNyxMenuEditorField;
      const AValue: TNyxText);
    procedure Click(AAction: TNyxMenuEditorAction; const APrefix: TNyxText = 'inspector-menu');
    function Pair: TNyxText;
    procedure History;
    procedure CaptureRefused(AAction: TNyxMenuEditorAction);
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GPolls: Integer;

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
  FSession.Select('open-actions');
  FState := DefaultNyxStudioViewState;
  FState.MenuEditorReference := NyxMenuRef('actions');
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:900px;overflow:auto;max-width:680px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(10, 10, 700, 940);
  FHost.Show;
  {$endif}
  {$ifndef PAS2JS}WriteLn('Compose inspector'); Flush(Output);{$endif}
  Refresh;
  {$ifndef PAS2JS}WriteLn('Inspector mounted'); Flush(Output);{$endif}
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

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
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

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if RouteNyxMenuInspectorChoice(FSession, ANode, FRenderer.Root,
    AEvent.Trigger, FState.MenuEditorReference) then
  begin
    Refresh;
    Exit;
  end;
  FCommands.Route(ANode, AEvent.Trigger, FRenderer.Root);
end;

procedure TReview.Change(const APrefix: TNyxText; AField: TNyxMenuEditorField;
  const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := NyxMenuEditorFieldID(APrefix, AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'Menu field is physically mounted: ' + LID);
  {$ifdef PAS2JS}

  if FRenderer.Root.Find(LID).ProjectionKind = 'checkbox' then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TSpinEdit then
  begin
    TSpinEdit(LInput).Value := StrToInt(AValue);
    TSpinEdit(LInput).OnChange(LInput);
  end
  else if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else if LInput is TCheckBox then
  begin
    TCheckBox(LInput).Checked := AValue = 'true';
    TCheckBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
    TEdit(LInput).OnChange(LInput);
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue, 'Target input retains its exact typed field');
end;

procedure TReview.Click(AAction: TNyxMenuEditorAction; const APrefix: TNyxText);
var
  LID: TNyxText;
begin
  LID := NyxMenuEditorActionID(APrefix, AAction);
  {$ifdef PAS2JS}FRenderer.ElementFor(LID).click;{$else}
  TControlAccess(FRenderer.ControlFor(LID)).Click;{$endif}
end;

procedure TReview.History;
begin
  FAfter := Pair;
  FSession.Undo;
  Check(Pair = FBefore, 'One Undo restores both exact accepted files');
  FSession.Redo;
  Check(Pair = FAfter, 'One Redo restores both exact candidate files');
  Refresh;
end;

procedure TReview.CaptureRefused(AAction: TNyxMenuEditorAction);
var
  LEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;
  LBefore: TNyxText;
begin
  LBefore := Pair;
  LRefused := False;
  try
    CaptureNyxMenuInspector(FSession,
      FRenderer.Root.Find(NyxMenuEditorActionID('inspector-menu', AAction)), FRenderer.Root, LEdit);
  except
    on LError: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (Pair = LBefore), 'Refusal preserves accepted design, source and history');
end;

function TReview.Step: Boolean;
var
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := False;

  if FCommands.Busy then
  begin
    {$ifdef PAS2JS}
    document.body.setAttribute('data-stage', IntToStr(FStage));
    document.body.setAttribute('data-command-state', IntToStr(Ord(FCommands.State)));
    document.body.setAttribute('data-command-message', FCommands.Message);
    {$endif}
    Exit;
  end;
  {$ifndef PAS2JS}WriteLn('Menu editor stage ', FStage); Flush(Output);{$endif}
  case FStage of
    0:
      begin
        Check(FRenderer.Root.Find('inspector-menu') <> nil, 'Studio consumes the public menu editor');
        LMetadata := NyxEventsMetadata(FSession.Selected, FSession.Document);
        LFound := False;
        for LIndex := 0 to High(LMetadata) do
        begin

          if (LMetadata[LIndex].Trigger = ntNamed) and
            (LMetadata[LIndex].Name.Name = NyxSemantic(nseActivate).Name) then
          begin
            LFound := not LMetadata[LIndex].DeclaredProducer and
              (LMetadata[LIndex].Browser = ncAvailable) and (LMetadata[LIndex].Native = ncAvailable);
          end;
        end;
        Check(LFound, 'Declared invoker advertises its real menu completion without granting Emit');
        FBefore := Pair;
        Change('inspector-menu', nmfTitle, 'Thoughtful choices');
        Change('inspector-menu', nmfWidth, '352');
        Change('inspector-menu', nmfSide, 'Above');
        Change('inspector-menu', nmfSizing, 'Fixed allocation');
        Change('inspector-menu', nmfSearchMatch, 'Exact');
        Change('inspector-menu', nmfSearchWindow, '1800');
        Check(CaptureNyxMenuInspector(FSession,
          FRenderer.Root.Find(NyxMenuEditorActionID('inspector-menu', nmeSave)), FRenderer.Root, LEdit),
          'Actual form captures a complete typed menu intent');
        LRequest := FSession.PrepareDesignRequest(LEdit, NyxSchemaRevision);
        Check((LRequest.ToData.Field('version').AsInteger = 12) and
          LRequest.SameRequest(ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON))),
          'Menu intent survives its exact worker boundary');
        { Both jobs capture the same registry. The second refuses after the
          first publishes; this must not become a second history step. }
        FCommands.Edit(LEdit);
        FCommands.Edit(LEdit);
      end;
    1:
      begin
        Check(FCommands.State = nssRejected, 'Stale queued menu baseline refuses: ' + FCommands.Message);
        Check((FSession.Document.Menus.Definition(NyxMenuRef('actions')).Options.Placement.Title = 'Thoughtful choices') and
          (FSession.Document.Menus.Definition(NyxMenuRef('actions')).Options.Placement.Width = 352) and
          (FSession.Document.Menus.Definition(NyxMenuRef('actions')).Options.Placement.Side = npsAbove) and
          (FSession.Document.Menus.Definition(NyxMenuRef('actions')).Options.Search.MatchMode = ntmExact),
          'Earlier whole-form change is retained with typed policies');
        Check(Pos('Thoughtful choices', FSession.Source) > 0, 'Adjacent Pascal carries the accepted menu policy');
        History;
        FBefore := Pair;
        Click(nmeMoveDown, NyxMenuEditorItemID('inspector-menu', 0));
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and
          (FSession.Document.Menus.Definition(NyxMenuRef('actions')).Item(0).Kind = nmiCheck),
          'Physical reorder replaces the complete typed plan');
        History;
        FBefore := Pair;
        Click(nmeMask);
      end;
    3:
      begin
        Check(FSession.Selected.HasMenu and (FSession.Selected.MenuReference.Name = ''),
          'Physical suppress installs an explicit local mask');
        History;
        FBefore := Pair;
        Click(nmeInherit);
      end;
    4:
      begin
        Check(not FSession.Selected.HasMenu, 'Physical inherit removes only the local declaration');
        History;
        FBefore := Pair;
        Change('inspector-menu', nmfDefinition, '1 / actions');
        Click(nmeAttach);
      end;
    5:
      begin
        Check(FSession.Selected.MenuReference.Name = 'actions', 'Physical attach uses the exact selected menu');
        History;
        CaptureRefused(nmeRemove);
        Change('inspector-menu', nmfConfirm, 'true');
        FBefore := Pair;
        Click(nmeRemove);
      end;
    6:
      begin
        Check((FCommands.State = nssRejected) and (Pair = FBefore),
          'Confirmed removal still refuses retained invoker/submenu dependencies');
        Change('inspector-menu', nmfDefinition, 'Choose a menu');
        FBefore := Pair;
        Click(nmeChoose);
        Check((Pair = FBefore) and (FState.MenuEditorReference.Name = ''),
          'Opening a new definition is presentation only');
        Change('inspector-menu', nmfName, 'reading-actions');
        Change('inspector-menu', nmfRoot, '2 / component / actions-content');
        Change('inspector-menu-new-item', nmfPart, 'copy');
        Change('inspector-menu-new-item', nmfCommand, 'copy-reading');
        Click(nmeAddItem);
      end;
    7:
      begin
        Check((FCommands.State = nssApplied) and FSession.Document.Menus.Contains(NyxMenuRef('reading-actions')),
          'Physical new-name/root/item authoring creates an independent menu definition');
        History;
        Change('inspector-menu', nmfDefinition, '3 / reading-actions');
        FBefore := Pair;
        Click(nmeChoose);
        Check((Pair = FBefore) and (FState.MenuEditorReference.Name = 'reading-actions'),
          'Open follows the exact saved definition without history');
        CaptureRefused(nmeRemove);
        Change('inspector-menu', nmfConfirm, 'true');
        FBefore := Pair;
        Click(nmeRemove);
      end;
    8:
      begin
        Check((FCommands.State = nssApplied) and not FSession.Document.Menus.Contains(NyxMenuRef('reading-actions')),
          'Explicitly reviewed unreferenced definition removal is admitted');
        History;
        Change('inspector-menu', nmfDefinition, '1 / actions');
        Click(nmeChoose);
        { A corrupt closed value cannot be selected by a real select. Exercise
          the explicit capture boundary with malformed owned field metadata. }
        FRenderer.Root.Find(NyxMenuEditorFieldID('inspector-menu', nmfSide))
          .SetProp('value', 'Sideways');
        CaptureRefused(nmeSave);
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
      raise Exception.Create('Menu editor paired queue exceeded stage ' + IntToStr(GReview.FStage) +
        ' / ' + GReview.FCommands.Message);
    end;

    if GReview.Step then
    begin
      WriteLn('PASS ', GChecks, ' actual menu editor/paired queue checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'passed');
      document.body.setAttribute('data-checks', IntToStr(GChecks));
      {$else}FreeAndNil(GReview);{$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LError.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      FreeAndNil(GReview);
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
    document.body.setAttribute('data-result', 'failed');
    document.body.setAttribute('data-error', 'Exact semantic source export is unavailable');
    Exit;
  end;
  GReview := TReview.Create(GRequest.responseText);
  Drive;
end;

function ReviewClosed(AEvent: TJSEvent): Boolean;
begin
  FreeAndNil(GReview);
  Result := True;
end;
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;

{ Qualify the ordinary controller as well as the shared inspector consumer.
  Only the explicitly supplied ignored project directory receives recovery data. }
procedure RunNativeStudio(const ASource: TNyxText);
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LChoice: TComboBox;
  LTitle: TEdit;

  procedure Capture(const AName: TNyxText);
  var
    LBitmap: TBitmap;
    LImage: TLazIntfImage;
    LWriter: TFPWriterPNG;
  begin

    if ParamCount < 3 then
    begin
      Exit;
    end;
    ForceDirectories(ParamStr(3));
    LBitmap := TBitmap.Create;
    LImage := nil;
    LWriter := nil;
    try
      LBitmap.SetSize(LForm.ClientWidth, LForm.ClientHeight);
      LForm.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LWriter := TFPWriterPNG.Create;
      LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(3)) + AName + '.png', LWriter);
    finally
      LWriter.Free;
      LImage.Free;
      LBitmap.Free;
    end;
  end;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 60000 then
      begin
        raise Exception.Create('Ordinary Studio menu edit exceeded its bound: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Button(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
  end;

begin

  if ParamCount < 2 then
  begin
    Exit;
  end;
  LForm := TForm.CreateNew(nil);
  LStudio := nil;
  LDocument := nil;
  try
    LForm.SetBounds(20, 20, 1280, 940);
    LForm.Show;
    WriteLn('Ordinary Studio construct'); Flush(Output);
    LStudio := TNyxNativeStudio.Create(LForm, ParamStr(2));
    LDocument := BuildNyxDocument;
    WriteLn('Ordinary Studio load exact pair'); Flush(Output);
    LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
    LStudio.Session.Select('open-actions');
    WriteLn('Ordinary Studio run'); Flush(Output);
    LStudio.Run;
    Ready;
    WriteLn('Ordinary Studio ready'); Flush(Output);
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    LChoice := TComboBox(LStudio.ShellView.InputFor(NyxMenuEditorFieldID('inspector-menu', nmfDefinition)));
    Check(LChoice <> nil, 'Ordinary Studio physically mounts public menu choice');
    LChoice.ItemIndex := LChoice.Items.IndexOf('1 / actions');
    LChoice.OnChange(LChoice);
    Button(NyxMenuEditorActionID('inspector-menu', nmeChoose));
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Ordinary Studio opens a definition without changing either accepted file');
    LTitle := TEdit(LStudio.ShellView.InputFor(NyxMenuEditorFieldID('inspector-menu', nmfTitle)));
    LTitle.Text := 'Studio menu choices';
    LTitle.OnChange(LTitle);
    Button(NyxMenuEditorActionID('inspector-menu', nmeSave));
    Check(LStudio.Session.Document.Menus.Definition(NyxMenuRef('actions')).Options.Placement.Title = 'Studio menu choices',
      'Actual ordinary Studio save uses the public typed menu candidate');
    Check(Pos('Studio menu choices', LStudio.Session.Source) > 0,
      'Ordinary Studio regenerates its adjacent Pascal');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Ordinary Studio Undo restores the exact pair');
    Button('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'Ordinary Studio Redo restores the exact candidate');
    Capture('studio-menu-desktop');
    LForm.ClientWidth := 390;
    Ready;
    Button('action-panel-inspector');
    Check(LStudio.ShellView.InputFor(NyxMenuEditorFieldID('inspector-menu', nmfTitle)) <> nil,
      'Compact ordinary Studio retains its public editor');
    Capture('studio-menu-compact');
  finally
    LStudio.Free;
    LDocument.Free;
    LForm.Free;
  end;
  WriteLn('PASS ', GChecks, ' including ordinary native Studio menu authoring');
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := @Loaded;
  GRequest.send;
  window.addEventListener('pagehide', @ReviewClosed);
  {$else}
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
    Drive;
    Application.ProcessMessages;
    CheckSynchronize(10);
  end;
  CheckSynchronize;

  if ExitCode = 0 then
  begin
    RunNativeStudio(LSource);
  end;
  {$endif}
end.
