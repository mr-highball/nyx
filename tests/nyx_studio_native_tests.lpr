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

program nyx_studio_native_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.source, nyx.contract,
  nyx.editing, nyx.editing.lcl, nyx.callbacks, nyx.controls, nyx.widgets.lcl, nyx.render.lcl,
  nyx.studio.lcl, nyx.studio.projects, nyx.studio.projectstore,
  nyx.studio.inspector, nyx.studio.palette, nyx.generated.view;

type
  TControlAccess = class(TControl);
  { Modal widgetset exception dialogs would obscure a failed editor journey.
    Retain failures for the next explicit check, outside notification lifetime. }
  TFailureObserver = class
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GChecks: Integer;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if GObserver.Error <> '' then
  begin
    raise ENyxModel.Create('Native editor callback: ' + GObserver.Error);
  end;

  if not ACondition then
  begin
    raise ENyxModel.Create('Native Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Pump;
var
  LIndex: Integer;
begin
  for LIndex := 1 to 4 do
  begin
    Application.ProcessMessages;
  end;
end;

{ The fixture presses the real public Nyx button. Painting must still be queued
  when its callback returns, rather than destroying that button mid-notification. }
procedure AwaitEditorChanges;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Pump;

    if GetTickCount64 - LStarted > 20000 then
    begin
      raise ENyxModel.Create('Native editor preparation did not retire / ' + GStudio.Status);
    end;

    if GStudio.SourceCommands.Busy then
    begin
      Sleep(1);
    end;
  until not GStudio.SourceCommands.Busy;
  Pump;
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
  LPaints: Integer;
begin
  { Source actions live in a retained independent Nyx view, which can also be
    mounted inside the expanded editor. Other editor chrome stays in ShellView. }
  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    LControl := GStudio.ShellView.ControlFor(AID);
  end
  else
  begin
    LControl := GStudio.SourceView.ControlFor(AID);
  end;
  WriteLn('Editor action ', AID);
  Flush(Output);
  LPaints := GStudio.PaintCount;
  TControlAccess(LControl).Click;
  Check(GStudio.PaintCount = LPaints, 'paint deferred past ' + AID);
  AwaitEditorChanges;
end;

procedure WriteField(ARenderer: TNyxLCLRenderer; const AID, AValue: TNyxText);
var
  LInput: TWinControl;
begin
  LInput := TWinControl(ARenderer.InputFor(AID));

  if LInput is TMemo then
  begin
    TMemo(LInput).Text := AValue;
  end
  else if LInput is TComboBox then
  begin
    TComboBox(LInput).Text := AValue;
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
  end;
  AwaitEditorChanges;
end;

function ReadBytes(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

function FindFace(ANode: TNyxNode; const AOwner: TNyxText;
  AKind: TNyxKind): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;

  if (ANode.DesignID = AOwner) and (ANode.ProjectionKind = NyxKindName(AKind)) then
  begin
    Exit(ANode);
  end;
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Result := FindFace(ANode.Children[LIndex], AOwner, AKind);

    if Result <> nil then
    begin
      Exit;
    end;
  end;
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(2)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

function DefinitionSnapshot: TNyxText;
var
  LDocument: TNyxDocument;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('definition-check'));
    LDocument.AddComponent(GStudio.Session.Document.Components[0].Clone);
    Result := TNyxCodec.Encode(LDocument);
  finally
    LDocument.Free;
  end;
end;

function Registrations: Integer;
var
  LEvents: TNyxAuthoredEventInfos;
begin
  LEvents := NyxAuthoredEvents(GStudio.Session.Document.Find('designer-reply-memo'));
  Result := Length(LEvents[0].Callbacks);
end;


{ A short actual-editor journey qualifies visibility after a retained view was
  parked by compact navigation. Object identity alone cannot prove that the
  canvas remains within a native viewport or paints its editable descendants. }
procedure Geometry;
var
  LControl: TWinControl;
  LCanvasHost: TWinControl;
  LPoint: TPoint;
  LParent: TControl;
  LSource: TMemo;
  LBefore: TNyxText;
begin
  { Optional service attachment must not make an unimplemented build appear
    available or suggest that connecting again would execute it. }
  LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
  Click('action-build-view');
  Check((GStudio.ShellView.Root.Find('studio-status').Prop('text') =
    'Native build requests are unavailable') and
    (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
    'unavailable view build reports accurately and retains the exact project');
  Click('action-build-app');
  Check((GStudio.ShellView.Root.Find('studio-status').Prop('text') =
    'Native build requests are unavailable') and
    (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
    'unavailable application build retains the exact project');
  Click('view-component-0');
  Click('action-code');
  WriteField(GStudio.ShellView, 'project-title', 'Native title / 🌙 漢字');
  LSource := TMemo(GStudio.CodeView.InputFor('studio-code'));
  Check((GStudio.Session.Document.Title = TNyxText('Native title / 🌙 漢字')) and
    (StringReplace(TNyxText(LSource.Text), #13#10, #10, [rfReplaceAll]) = GStudio.Session.Source),
    'admitted title edit updates the retained mounted Pascal source');
  { Keep initial review screenshots in English after qualifying exact Unicode
    title input. The pending draft below still exercises the full text contract. }
  WriteField(GStudio.ShellView, 'project-title', 'A reusable reply');
  Check(GStudio.Session.Document.Title = 'A reusable reply',
    'review presentation returns to English without narrowing title input');
  WriteField(GStudio.CodeView, 'studio-code', GStudio.Session.Source + #10 +
    TNyxText('{ Retained geometry draft / 🌙 漢字 }') + #10);
  LSource := TMemo(GStudio.CodeView.InputFor('studio-code'));
  LSource.SetFocus;
  SelectNyxLCLText(LSource, NyxTextSelection(TNyxText(LSource.Text), 4, 7));
  GStudio.RequestRefresh;
  Pump;
  Click('action-code');
  Click('action-code');
  GForm.ClientWidth := 390;
  Pump;
  Click('action-panel-project');
  Click('action-panel-inspector');
  Click('action-panel-design');
  LControl := TWinControl(GStudio.CanvasView.InputFor('designer-reply-memo'));
  LCanvasHost := GStudio.ShellView.ControlFor('studio-canvas') as TWinControl;
  LParent := LControl;
  while LParent <> nil do
  begin
    WriteLn('Canvas ancestor ', LParent.ClassName, ' ', LParent.Left, ',', LParent.Top,
      ' ', LParent.Width, 'x', LParent.Height, ' visible=', LParent.Visible);

    if LParent is TScrollBox then
    begin
      WriteLn('Scroll position ', TScrollBox(LParent).VertScrollBar.Position);
    end;
    LParent := LParent.Parent;
  end;
  Capture('native-editor-geometry');
  LPoint := LCanvasHost.ScreenToClient(LControl.ClientToScreen(Point(0, 0)));
  Check(LCanvasHost.ClientHeight > 100, 'retained canvas has a useful compact split viewport');
  Check(LControl.CanFocus and (LControl.Height > 50), 'retained memo is a visible native editor');
  Check((LPoint.Y + LControl.Height > 0) and (LPoint.Y < LCanvasHost.ClientHeight),
    'retained memo intersects its native canvas viewport');
  Check(GStudio.Session.ProjectSnapshot.Pending, 'geometry moves retain the exact pending source');
end;

var
  LSeed: TNyxDocument;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LDefinition: TNyxText;
  LPage: TNyxText;
  LBadge: TNyxText;
  LDraft: TNyxText;
  LSource: TMemo;
  LFace: TNyxNode;
  LReply: TMemo;
  LSelection: TNyxTextSelection;
  LPaints: Integer;
  LStore: TNyxProjectStore;
  LPacket: TNyxText;
  LRevision: TNyxText;
  LWrittenRevision: TNyxText;
  LRemote: TNyxText;
  LProject: TNyxText;
  LIdentity: TGUID;
begin
  GStudio := nil;
  GForm := nil;
  GObserver := TFailureObserver.Create;
  LStore := nil;
  LSeed := nil;
  try

    if not (ParamCount in [2, 3]) then
    begin
      raise ENyxModel.Create('Supply exact MCP source directory and owned editor artifact directory');
    end;

    if (ParamCount = 3) and (ParamStr(3) <> 'geometry') then
    begin
      raise ENyxModel.Create('Optional editor qualification mode must be geometry');
    end;
    ForceDirectories(ParamStr(2));
    Application.Initialize;
    Application.OnException := GObserver.Failed;
    GForm := TForm.Create(nil);
    GForm.SetBounds(30, 30, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm,
      IncludeTrailingPathDelimiter(ParamStr(2)) + 'projects');
    LSeed := BuildNyxDocument;
    LPair := NyxProjectPair(TNyxCodec.Encode(LSeed),
      ReadBytes(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas'));
    LSeed.Free;
    LSeed := nil;
    GStudio.LoadProject(LPair);
    GStudio.Run;
    Pump;
    Check(GStudio.CanvasView.DesignMode and
      (GStudio.Session.ActiveViewID = 'designer-review'), 'standalone full editor admits MCP pair');
    Check(GStudio.ShellView.ControlFor('studio-canvas').Height > 250,
      'native canvas receives useful viewport height');
    Check(GStudio.ShellView.ControlFor('studio-left') is TScrollBox,
      'palette uses the shared public scroll view');
    Check(GStudio.ShellView.ControlFor('studio-left').Height < GForm.ClientHeight,
      'sidebar scroll is contained within the editor viewport');
    Capture('native-editor-desktop');

    if ParamCount = 3 then
    begin
      Geometry;
    end
    else
    begin
      LDefinition := DefinitionSnapshot;
      LBefore := GStudio.Session.Source;
      LFace := FindFace(GStudio.CanvasView.Root, 'designer-first', nkMemo);
      Check(LFace <> nil, 'ordinary canvas exposes inherited memo');
      LReply := TMemo(GStudio.CanvasView.InputFor(LFace.ID));
      LReply.SetFocus;
      TControlAccess(GStudio.CanvasView.ControlFor(LFace.ID)).Click;
      Pump;
      Check((GStudio.Session.SelectedID = 'designer-first') and
        (GStudio.CanvasView.InputFor(LFace.ID) = LReply),
        'selecting a reusable field retains its physical native control');
      Check(Screen.ActiveControl = LReply, 'selection chrome retains actual memo focus');
      WriteField(GStudio.CanvasView, LFace.ID, 'Edited in native Studio / 🌙 漢字');
      Check(GStudio.Session.CanUndo and (GStudio.Session.Source <> LBefore),
        'physical memo editing publishes paired source/history / ' + GStudio.Status);
      Check(DefinitionSnapshot = LDefinition,
        'native instance edit preserves the reusable definition');
      Check(GStudio.CanvasView.InputFor(LFace.ID) = LReply,
        'accepted canvas value retains its editing control');
      Click('action-undo');
      Check(GStudio.Session.Source = LBefore, 'ordinary editor Undo restores exact source');
      Click('action-redo');
      Check(GStudio.Session.Source <> LBefore, 'ordinary editor Redo restores canvas edit');

      Click('action-add-page');
      LPage := GStudio.Session.ActiveViewID;
      Check((GStudio.Session.Document.Count = 2) and
        (GStudio.CanvasView.Root.DesignID = LPage), 'new page opens its full design editor');
      Click('palette-badge');
      LBadge := GStudio.Session.SelectedID;
      Check(GStudio.Session.Selected.Kind = 'badge', 'native palette inserts typed badge');
      WriteField(GStudio.ShellView, 'inspector-text', 'A reusable badge / 🌙');
      Check(GStudio.Session.Selected.Prop('text') = TNyxText('A reusable badge / 🌙'),
        'selected property edits exact supplementary Unicode');
      Click('action-component');
      Check((GStudio.Session.Document.ComponentCount = 2) and
        (GStudio.Session.ActiveViewID <> LPage), 'make reusable opens the new component editor');
      Click('view-page-1');
      Click('instance-component-1');
      Check((GStudio.Session.ActiveViewID = LPage) and
        (GStudio.Session.Selected.Kind = 'component'), 'reuse adds a separate instance on another page');
      Click('action-duplicate');
      Check(GStudio.Session.ActiveView.Count = 3, 'duplicate is an ordinary undoable editor command');
      Click('action-delete');
      Check(GStudio.Session.ActiveView.Count = 2, 'delete preserves remaining instances');
      Click('action-undo');
      Check(GStudio.Session.ActiveView.Count = 3, 'delete Undo restores the independent instance');

      Click('view-component-0');
      TControlAccess(GStudio.CanvasView.ControlFor('designer-reply-memo')).Click;
      Pump;
      Click(NyxInspectorEventsID);
      Click('event-before-key-press-add');
      Check(Pos('TODO', GStudio.Session.Source) > 0, 'event authoring creates a Pascal TODO handler');
      LSource := TMemo(GStudio.CodeView.InputFor('studio-code'));
      Check(LSource.CanFocus and (Screen.ActiveControl = LSource) and (LSource.SelStart > 0),
        'new callback opens and navigates the actual retained source control');
      Click('event-before-key-press-add');
      Check(Registrations = 2,
        'native event inspector permits multiple registrations');
      Click('event-before-key-press-callback-0-remove');
      Check(GStudio.ShellView.Root.Find('event-removal-confirm') <> nil,
        'removal exposes the shared exact-registration warning');
      Click('event-removal-confirm');
      Check(Registrations = 1,
        'explicit confirmation removes one registration');
      Click('action-undo');
      Check(Registrations = 2,
        'event removal shares paired Undo');

      LSource := TMemo(GStudio.CodeView.InputFor('studio-code'));
      LBefore := GStudio.Session.Save;
      LDraft := GStudio.Session.Source + #10 + TNyxText('{ Draft / 🌙 漢字 }') + #10;
      WriteField(GStudio.CodeView, 'studio-code', LDraft);
      LSource.SetFocus;
      LSelection := NyxTextSelection(TNyxText(LSource.Text), 4, 7);
      SelectNyxLCLText(LSource, LSelection);
      LPaints := GStudio.PaintCount;
      GStudio.RequestRefresh;
      GStudio.RequestRefresh;
      Check(GStudio.PaintCount = LPaints, 'external refresh also waits for the UI queue');
      Pump;
      Check(GStudio.PaintCount = LPaints + 1, 'related refresh requests coalesce');
      Check((GStudio.CodeView.InputFor('studio-code') = LSource) and
        (Screen.ActiveControl = LSource) and
        (CaptureNyxLCLSelection(LSource).Start = LSelection.Start) and
        (CaptureNyxLCLSelection(LSource).Finish = LSelection.Finish),
        'chrome replacement keeps the actual source object, focus and scalar range');
      Click('action-code');
      Click('action-code');
      Check((GStudio.CodeView.InputFor('studio-code') = LSource) and
        (GStudio.Session.DraftSource = LDraft),
        'hiding/reopening optional Pascal retains its actual control and exact draft');
      GForm.ClientWidth := 390;
      Pump;
      Check(GStudio.ShellView.Root.Find('action-panel-project') <> nil,
        'actual narrow native host uses shared compact navigation');
      Click('action-panel-project');
      Click('action-panel-inspector');
      Click('action-panel-design');
      Check((GStudio.CodeView.InputFor('studio-code') = LSource) and
        (GStudio.Session.DraftSource = LDraft) and (GStudio.Session.Save = LBefore),
        'full native panel navigation preserves paired design and pending source');
      Capture('native-editor-narrow');
      GForm.ClientWidth := 1280;
      Pump;
      Click('action-reset-source');
      Check(not GStudio.Session.ProjectSnapshot.Pending and (GStudio.Session.Save = LBefore),
        'restore accepted source retains the design');
      Click('view-page-0');
      Check(GStudio.Session.ActiveViewID = 'designer-review',
        'page return mounts the full original project after component work');

      Click('action-outputs');
      LBefore := GStudio.Session.Source;
      Click('output-browser');
      Click('output-lcl');
      Click('output-none');
      Check((GStudio.Session.Source = LBefore) and
        (GStudio.ShellView.Root.Find('output-none') <> nil),
        'output selection is optional and can change without compilers or source mutation');
      Click('action-outputs');

      Click('action-import');
      CreateGUID(LIdentity);
      LProject := 'editor-' + Copy(GUIDToString(LIdentity), 2, 36);
      WriteField(GStudio.ShellView, 'project-file-name', LProject);
      LDraft := GStudio.Session.Source + #10 + TNyxText('{ Saved pending draft / 🌙 漢字 }') + #10;
      WriteField(GStudio.CodeView, 'studio-code', LDraft);
      Click('action-project-save');
      LStore := TNyxProjectStore.Create(IncludeTrailingPathDelimiter(ParamStr(2)) + 'projects');
      LPacket := LStore.ReadProject(LProject, LRevision);
      Check((LPacket <> '') and (DecodeNyxProject(LPacket).Draft = LDraft),
        'real native Save persists accepted files and the exact pending source');
      LPair := DecodeNyxProject(LPacket);
      LPair.Source := LPair.Source + #10 + TNyxText('{ External editor / 🌙 }') + #10;
      Check(LStore.SaveProject(LProject, LRevision, LPair, LWrittenRevision, LRemote),
        'independent writer creates a genuine saved-file revision conflict');
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-project-save');
      Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore) and
        (GStudio.ShellView.Root.Find('action-project-use-remote') <> nil),
        'stale Save retains current pair and exposes explicit conflict resolution');
      Click('action-project-use-remote');
      Check((GStudio.Session.Source = LPair.Source) and
        (GStudio.Session.DraftSource = LDraft),
        'warned resolution backs up mine and admits the saved pair with its draft');
      Check(not GStudio.Session.CanUndo, 'paired open deliberately starts independent project history');
      Capture('native-editor-files');
    end;

    GStudio.RequestRefresh;
    GStudio.Free;
    GStudio := nil;
    Pump;
    Check(GObserver.Error = '', 'destroying queued native editor removes its pending paint');
    WriteLn('PASS ', GChecks, ' actual standalone native Studio editor checks');
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  Application.OnException := nil;
  GStudio.Free;
  GForm.Free;
  LStore.Free;
  LSeed.Free;
  GObserver.Free;
end.
