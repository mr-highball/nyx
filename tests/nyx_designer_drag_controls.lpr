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
program nyx_designer_drag_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Interfaces, Forms, Controls, StdCtrls,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.model, nyx.codec, nyx.render.lcl, nyx.designer.input, nyx.gestures,
  nyx.behavior, nyx.events, nyx.scheduler, nyx.types,
  nyx.studio.projects, nyx.studio.lcl, nyx.studio.drag, nyx.test.placement;

type
  TControlAccess = class(TControl);
  { Runtime callbacks are registered deliberately to prove that design drops
    remain on the designer port, never application subscriptions. }
  TProbe = class(TNyxEventCallback)
    Calls: Integer;
    Failure: TNyxText;
    DesignerCalls: Integer;
    Decision: INyxGestureDecision;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
    procedure Failed(ASender: TObject; AError: Exception);
    procedure Designer(const ATarget: TNyxDesignerTarget; const AEvent: TNyxEventInfo;
      const ADecision: INyxGestureDecision);
  end;

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GProbe: TProbe;
  GProbeOwner: INyxEventCallback;
  GToken: INyxEventSubscription;
  GChecks: Integer;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Calls);
end;

procedure TProbe.Failed(ASender: TObject; AError: Exception);
begin
  Failure := TNyxText(AError.Message);
end;

procedure TProbe.Designer(const ATarget: TNyxDesignerTarget; const AEvent: TNyxEventInfo;
  const ADecision: INyxGestureDecision);
begin
  Inc(DesignerCalls);
  Decision := ADecision;
  ADecision.AcceptDrop(ndoCopy);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Native designer drag: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Pump;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if GProbe.Failure <> '' then
    begin
      raise Exception.Create(GProbe.Failure);
    end;

    if not GStudio.PresentationPending and not GStudio.SourceCommands.Busy then
    begin
      Exit;
    end;

    if GetTickCount64 - LStart > 30000 then
    begin
      raise Exception.Create('Designer drag work did not finish');
    end;
    Sleep(1);
  until False;
end;

procedure ClickShell(const AID: TNyxText);
begin
  Check(GStudio.ShellView.ControlFor(AID) <> nil, 'shared shell action is mounted: ' + AID);
  TControlAccess(GStudio.ShellView.ControlFor(AID)).Click;
  Pump;
end;

procedure Position(const AValue: TNyxText);
var
  LInput: TComboBox;
begin
  LInput := TComboBox(GStudio.ShellView.InputFor(NyxStudioDropPositionID));
  Check(LInput <> nil, 'ordinary Nyx drop-position select is mounted');
  LInput.ItemIndex := LInput.Items.IndexOf(String(AValue));
  LInput.OnChange(LInput);
end;

{ Invoke real registered LCL source/target slots. End runs before queued
  publication can replace the source shell. This qualifies host callbacks,
  not a physical mouse, touch or another widgetset drag manager. }
procedure Drag(const ASource, ATarget: TNyxText; AAcceptExpected: Boolean);
var
  LSource: TControl;
  LTarget: TControl;
  LDrag: TDragObject;
  LAccept: Boolean;
  LBefore: TNyxProjectPair;
begin
  LSource := GStudio.ShellView.ControlFor(ASource);
  LTarget := GStudio.CanvasView.ControlFor(ATarget);
  Check((LSource <> nil) and (LTarget <> nil), 'source and design target are realized');
  LBefore := GStudio.Session.ProjectSnapshot;
  LDrag := nil;
  TControlAccess(LSource).OnStartDrag(LSource, LDrag);
  try
    Check(LDrag <> nil, 'public Nyx source constructs a native drag object');
    LAccept := False;
    TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 4, 5, dsDragEnter, LAccept);
    Check(LAccept = AAcceptExpected, 'actual enter negotiates the intended eligibility');
    Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'actual hover leaves both files untouched');
    TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 6, 7, dsDragMove, LAccept);
    Check(LAccept = AAcceptExpected, 'actual hover keeps the same eligibility');
    TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 6, 7, dsDragLeave, LAccept);
    Check(LAccept = AAcceptExpected, 'native final leave retains the eligible hover agreement');
    TControlAccess(LTarget).OnDragDrop(LTarget, LDrag, 6, 7);
    TControlAccess(LSource).OnEndDrag(LSource, LTarget, 6, 7);
  finally
    LDrag.Free;
  end;
  Pump;
end;

{ Independently mounted public adapter policy, with an actual Nyx source object.
  A disabled/read-only design target stays authorable through host callbacks;
  this does not qualify hardware hit-testing over disabled Win32 widgets. }
procedure AdapterPolicy;
var
  LHost: TForm;
  LRenderer: TNyxLCLRenderer;
  LDocument: TNyxDocument;
  LSource: TControl;
  LTarget: TControl;
  LDrag: TDragObject;
  LAccept: Boolean;
  LRefused: Boolean;
begin
  LHost := TForm.CreateNew(nil);
  LRenderer := TNyxLCLRenderer.Create;
  LDocument := TNyxCodec.Decode(NyxPlacementFixture.Design);
  LSource := GStudio.ShellView.ControlFor('palette-button');
  LDrag := nil;
  try
    LHost.SetBounds(0, 0, 640, 480);
    LHost.HandleNeeded;
    LRenderer.OnDesignerGesture := GProbe.Designer;
    LRenderer.Render(LDocument, LDocument.Find('home'), LHost, True);
    TControlAccess(LSource).OnStartDrag(LSource, LDrag);
    Check(LDrag <> nil, 'adapter fixture borrows a real Nyx source drag');
    LTarget := LRenderer.ControlFor('right-layout');
    LAccept := True;
    TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 2, 3, dsDragMove, LAccept);
    Check(not LAccept and (GProbe.DesignerCalls = 0), 'designer drops default to disabled');
    LRefused := False;
    try
      LRenderer.DesignerInput := NyxDesignerInput.Drops(True);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'mounted policy changes refuse without replacing the view');
    LRenderer.Unmount;
    LRenderer.DesignerInput := NyxDesignerInput.Drops(True);
    LDocument.Find('notes-editor').Configure.Enabled(False).ReadOnly(True).Done;
    LRenderer.Render(LDocument, LDocument.Find('home'), LHost, True);
    LTarget := LRenderer.ControlFor('notes-editor');
    TControlAccess(LTarget).OnDragOver(LTarget, LDrag, 2, 3, dsDragMove, LAccept);
    Check(LAccept and (GProbe.DesignerCalls = 1),
      'explicit designer input can author a disabled/read-only target');
    Check(not GProbe.Decision.CanRequest(ngcAcceptDrop),
      'the adapter seals a retained designer decision before returning');
    TControlAccess(LSource).OnEndDrag(LSource, LTarget, 2, 3);
  finally
    LDrag.Free;
    LRenderer.OnDesignerGesture := nil;
    LRenderer.Free;
    LDocument.Free;
    LHost.Free;
  end;
end;

procedure WriteBytes(const AName, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Capture;
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
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'designer-drag.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

var
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LEditor: TControl;
  LAdded: TNyxText;
begin
  try
    Application.Initialize;
    ForceDirectories(ParamStr(1));
    GProbe := TProbe.Create;
    GProbeOwner := GProbe;
    Application.OnException := GProbe.Failed;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(20, 20, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm, IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    try
      GStudio.LoadProject(NyxPlacementFixture);
      GStudio.Run;
      Pump;
      AdapterPolicy;
      ClickShell('action-code');
      LEditor := GStudio.CodeView.InputFor('studio-code');
      Check(LEditor <> nil, 'ordinary source editor is independently retained');
      GToken := GStudio.CanvasView.Events.OnDragOver(NyxControlEvents('right-layout'))
        .Subscribe(GProbeOwner);
      LBefore := GStudio.Session.ProjectSnapshot;
      GStudio.Session.Select('notes-editor');
      GStudio.RequestRefresh;
      Pump;
      Position('before');
      Drag(NyxStudioDragMoveID, 'send-button', True);
      Check((GStudio.Session.Document.Find('notes-editor').Parent.ID = 'right-layout') and
        (GStudio.Session.Document.Find('right-layout').Children[0].ID = 'notes-editor'),
        'actual physical move publishes exact nested relative placement');
      LAfter := GStudio.Session.ProjectSnapshot;
      Check(LAfter.Source <> LBefore.Source, 'accepted move updates adjacent crafted Pascal');
      Check(Pos('Keep this handwritten English source comment.', LAfter.Source) > 0,
        'physical edit preserves handwritten source');
      Check(GStudio.CodeView.InputFor('studio-code') = LEditor, 'source control survives physical move');
      ClickShell('action-undo');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'ordinary Undo restores both physical-move files together');
      ClickShell('action-redo');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'ordinary Redo restores the exact move pair');
      Position('inside');
      Drag('palette-labeled-button', 'right-layout', True);
      LAdded := GStudio.Session.SelectedID;
      Check((GStudio.Session.Selected.Kind = 'labeled-button') and
        (GStudio.Session.Selected.Parent.ID = 'right-layout'),
        'actual palette drop creates a specialized compound in the target layout');
      Check(GStudio.CanvasView.Root.Find(LAdded).Part('button') <> nil,
        'accepted compound realizes its reusable action part');
      Check(GProbe.Calls = 0, 'designer drops suppress runtime application subscriptions');
      LAfter := GStudio.Session.ProjectSnapshot;
      Drag('palette-button', 'notes-editor', False);
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'leaf-target refusal changes neither source nor design');
      Check(GStudio.CodeView.InputFor('studio-code') = LEditor,
        'hover/refusal retain editor identity');
      Capture;
      WriteBytes('design.nyx', LAfter.Design);
      WriteBytes('nyx.generated.view.pas', LAfter.Source);
      WriteBytes('project.nyxpair', EncodeNyxProject(LAfter));
      WriteLn('Native designer drag controls passed: ', GChecks);
    finally

      if GToken <> nil then
      begin
        GToken.Cancel;
      end;
      GToken := nil;
      GStudio.Free;
      GForm.Free;
      Application.OnException := nil;
      GProbeOwner := nil;
    end;
  except
    on LError: Exception do
    begin
      WriteLn(LError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
