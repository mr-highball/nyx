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

program nyx_placement_controls;
{$mode delphi}{$H+}{$codepage utf8}
uses
  Interfaces, SysUtils, Classes, Forms, Controls, StdCtrls, LCLType,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.model, nyx.codec, nyx.types, nyx.theme, nyx.render.lcl,
  nyx.studio.projects, nyx.studio.lcl, nyx.test.placement, nyx.generated.view;

type
  TControlAccess = class(TControl);
  TObserver = class
  public
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  LChecks: Integer;
  LObserver: TObserver;
  LForm: TForm;
  LStudio: TNyxNativeStudio;
  LCompiled: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LCompiledHost: TForm;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LSourceEditor: TControl;
  LStream: TFileStream;
  LExpected: TNyxText;

procedure TObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := TNyxText(AException.Message);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Native placement: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if LObserver.Failure <> '' then
    begin
      raise Exception.Create(LObserver.Failure);
    end;

    if not LStudio.PresentationPending and not LStudio.SourceCommands.Busy then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 10000 then
    begin
      raise Exception.Create('Owned placement work did not finish');
    end;
    Sleep(1);
  until False;
end;

procedure Stage(const AName: TNyxText);
begin
  WriteLn('Native placement stage / ', AName);
  Flush(Output);
end;

{ Invoke actual widget hooks, never Session.Place or document mutations. The
  renderer/controller/isolated worker owns each ordinary authoring action. }
procedure ClickShell(const AID: TNyxText);
var
  LControl: TControl;
begin
  Stage('click shell ' + AID);
  LControl := LStudio.ShellView.ControlFor(AID);

  if LControl = nil then
  begin
    raise Exception.Create('Missing actual shell control: ' + AID);
  end;
  TControlAccess(LControl).Click;
  Pump;
end;

procedure ClickCanvas(const AID: TNyxText);
var
  LControl: TControl;
begin
  Stage('click canvas ' + AID);
  LControl := LStudio.CanvasView.ControlFor(AID);

  if LControl = nil then
  begin
    raise Exception.Create('Missing actual canvas control: ' + AID);
  end;
  TControlAccess(LControl).Click;
  Pump;
end;

{ A selective visual artifact of the ordinary public Nyx inspector, captured
  from the focused fixture only. No user's desktop/project is photographed. }
procedure CapturePlacement;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(LForm.ClientWidth, LForm.ClientHeight);
    LForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    ForceDirectories(ParamStr(1));
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'placement.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

begin
  try
    Application.Initialize;
    Stage('initialized');
    LObserver := TObserver.Create;
    Application.OnException := LObserver.Failed;
    LForm := TForm.CreateNew(nil);
    LForm.SetBounds(20, 20, 1280, 900);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm,
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    Stage('studio created');
    try
      LStudio.LoadProject(NyxPlacementFixture);
      LStudio.Run;
      Pump;
      Stage('initial canvas');
      ClickShell('action-code');
      LSourceEditor := LStudio.CodeView.InputFor('studio-code');
      Check(LSourceEditor <> nil, 'retained ordinary source editor is mounted');
      LBefore := LStudio.Session.ProjectSnapshot;
      ClickCanvas('notes-editor');
      Check(LStudio.Session.SelectedID = 'notes-editor', 'actual canvas selects source control');
      ClickShell('action-place-start');
      Stage('source armed');
      Check(LStudio.Session.PlacementSource.ID = 'notes-editor', 'actual inspector arms copied source');
      Check(not LStudio.Session.CanUndo, 'arming alone does not create project history');
      ClickCanvas('send-button');
      Check(LStudio.Session.SelectedID = 'send-button', 'actual canvas selects exact destination');
      Check(LStudio.ShellView.Root.Find('action-place-before') <> nil, 'shared inspector shows pending placement');
      CapturePlacement;
      ClickShell('action-place-before');
      Stage('placement published');
      Check(LStudio.Session.Document.Find('notes-editor').Parent.ID = 'right-layout',
        'real queued placement changes nested ownership');
      Check(LStudio.Session.Document.Find('right-layout').Children[0].ID = 'notes-editor',
        'relative ordering reaches actual published canvas');
      Check(LStudio.Session.SelectedID = 'notes-editor', 'publication selects the moved control');
      Check(LStudio.Session.PlacementSource.ID = '', 'publication retires pending move');
      Check(LStudio.CodeView.InputFor('studio-code') = LSourceEditor,
        'moving retains the source widget identity');
      LAfter := LStudio.Session.ProjectSnapshot;
      ClickShell('action-undo');
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'actual Undo restores both files together');
      ClickShell('action-redo');
      Stage('paired history');
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'actual Redo restores the exact accepted pair');
      ClickShell('action-place-start');
      ClickShell('action-place-cancel');
      Check((LStudio.Session.PlacementSource.ID = '') and
        (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter)),
        'actual cancel changes only editor presentation');

      { Compile the semantic export unchanged, then mount real Win32 controls.
        This establishes target consumption beyond serialization or source
        admission. The browser companion uses the same exported Pascal unit. }
      LCompiled := BuildNyxDocument;
      Stage('compiled companion');
      LStream := TFileStream.Create(ParamStr(2), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(LExpected, LStream.Size);

        if LExpected <> '' then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      Check(TNyxCodec.Encode(LCompiled) = LExpected,
        'unchanged compiled companion reproduces the exact semantic design');
      LCompiledHost := TForm.CreateNew(nil);
      LRenderer := TNyxLCLRenderer.Create;
      try
        LCompiledHost.SetBounds(40, 40, 900, 700);
        LCompiledHost.Show;
        LRenderer.Render(LCompiled, LCompiled.Find('home'), LCompiledHost, False);
        Check(TMemo(LRenderer.InputFor('reusable-instance/notes-editor')).Text = 'Keep these notes.',
          'unchanged compiled reusable payload reaches the actual native memo');
        Check(LRenderer.ControlFor('reply-action') <> nil, 'unchanged compound mounts native controls');
        LRenderer.Render(LCompiled, LCompiled.Find('archive'), LCompiledHost, False);
        Check(TButton(LRenderer.ControlFor('send-button')).Caption = 'Post reply',
          'unchanged cross-page placement mounts on its owning output');
      finally
        LRenderer.Free;
        LCompiledHost.Free;
        LCompiled.Free;
      end;
    finally
      LStudio.Free;
      LForm.Free;
      Application.OnException := nil;
      LObserver.Free;
    end;
    WriteLn('PASS ', LChecks, ' actual native placement checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
