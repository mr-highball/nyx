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


program nyx_source_workspace_controls;
{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, SysUtils, Classes, Forms, Controls, StdCtrls, LCLType,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.studio.projects, nyx.studio.lcl, nyx.modal.lcl, nyx.studio.source;

type
  TControlAccess = class(TControl);
  TObserver = class
  public
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GForm: TForm;
  GStudio: TNyxNativeStudio;
  GObserver: TObserver;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LPair: TNyxProjectPair;
  LSource: TMemo;
  LDraft: TNyxText;
  LChecks: Integer;
  LKey: Word;
  LNormalHeight: Integer;
  LExpandedHeight: Integer;
  LExpandedWidth: Integer;

procedure TObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := TNyxText(AException.Message);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Source workspace: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure Pump;
var
  LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 8000;
  repeat
    Application.ProcessMessages;

    if GObserver.Failure <> '' then
    begin
      raise Exception.Create(GObserver.Failure);
    end;

    if not GStudio.PresentationPending and not GStudio.SourceCommands.Busy then
    begin
      Break;
    end;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Check(not GStudio.PresentationPending and not GStudio.SourceCommands.Busy,
    'owned editor work reaches its terminal presentation');
end;

procedure Click(const AID: TNyxText);
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
  TControlAccess(LControl).Click;
  Pump;
end;

procedure Capture(AForm: TForm; const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AForm.ClientWidth, AForm.ClientHeight);
    AForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

begin
  try
    ForceDirectories(ParamStr(1));
    GObserver := TObserver.Create;
    Application.Initialize;
    Application.OnException := GObserver.Failed;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(30, 30, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm,
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    LDocument := TNyxDocument.Create;
    try
      LDocument.Title := 'A comfortable Pascal workspace';
      LPage := NewNyxPage('home');
      LDocument.AddPage(LPage);
      LPage.Add(NewNyxHeading('welcome').Configure.Text('Make something wonderful.').Done);
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LPage := nil;
      LDocument.Free;
    end;
    GStudio.LoadProject(LPair);
    GStudio.Run;
    Pump;
    Click('action-code');
    LSource := TMemo(GStudio.CodeView.InputFor('studio-code'));
    Check((ColorToRGB(LSource.Color) = RGBToColor(23, 27, 41)) and
      (ColorToRGB(LSource.Font.Color) = RGBToColor(203, 213, 237)),
      'ordinary native source consumes its independent typed readable palette');
    LNormalHeight := LSource.Height;
    WriteLn('Source viewport / memo ', LSource.Width, ' x ', LNormalHeight,
      ' / split ', GStudio.ShellView.ControlFor('studio-split').Height,
      ' / pane ', GStudio.SourceView.ControlFor('studio-source-pane').Height);
    Capture(GForm, 'source-sizing');
    Check(LNormalHeight > 120,
      'ordinary source gains useful space from the viewport instead of a fixed desktop split');
    Click('action-outputs');
    Check((LSource.Height >= 120) and
      (GStudio.SourceView.ControlFor('studio-source-pane').Height >= 280),
      'source remains readable beside open Outputs through public pane minima');
    Capture(GForm, 'source-with-outputs');
    LDraft := LPair.Source + #10 + '{ This draft belongs to the same retained editor. }' + #10;
    LSource.Text := LDraft;
    Pump;
    LSource.SelStart := 10;
    LSource.SelLength := 6;
    Check(GStudio.Session.DraftSource = LDraft, 'actual memo input retains the exact unsubmitted draft');
    Click('action-messages-tab');
    Check(not GStudio.SourceView.ControlFor('studio-code-host').Visible and
      GStudio.SourceView.ControlFor('studio-source-messages').Visible,
      'messages occupy their own view and cannot squeeze the source');
    Click('action-source-tab');
    Check((GStudio.CodeView.InputFor('studio-code') = LSource) and
      (LSource.SelStart = 10) and (LSource.SelLength = 6),
      'source/message switching preserves the actual editor and selection');
    Capture(GForm, 'source-desktop');
    Click('action-expand-source');
    Check(GStudio.SourceModal.IsOpen and not GForm.Enabled,
      'expanded native host isolates background input');
    Check((GStudio.CodeView.InputFor('studio-code') = LSource) and
      (LSource.Height > LNormalHeight + 200) and (LSource.SelStart = 10),
      'the same source editor receives an expanded viewport and keeps its range');
    Capture(TForm(GStudio.SourceModal.Control), 'source-expanded-desktop');
    LExpandedHeight := LSource.Height;
    LExpandedWidth := LSource.Width;
    TForm(GStudio.SourceModal.Control).ClientHeight :=
      TForm(GStudio.SourceModal.Control).ClientHeight - 140;
    TForm(GStudio.SourceModal.Control).ClientWidth :=
      TForm(GStudio.SourceModal.Control).ClientWidth - 180;
    Pump;
    Check((LSource.Height < LExpandedHeight - 100) and
      (LSource.Width < LExpandedWidth - 140) and
      (GStudio.CodeView.InputFor('studio-code') = LSource) and
      (GStudio.Session.DraftSource = LDraft) and (LSource.SelStart = 10),
      'native modal resize follows available space without losing editor ownership or input');
    { Source diagnostics change the workspace's control structure. Both rejection
      and admission must remain usable inside the modal and retain the memo. }
    LSource.Text := LDraft + #10 + '''unfinished';
    Click('action-apply-source');
    Check(GStudio.SourceModal.IsOpen and GStudio.Session.SourceDiagnostic.Defined and
      (GStudio.SourceView.Root.Find(NyxStudioDiagnosticGoID) <> nil) and
      (GStudio.CodeView.InputFor('studio-code') = LSource) and
      (GStudio.Session.Source = LPair.Source),
      'expanded Apply shows a real source diagnostic without losing accepted source or the editor');
    Click('action-reset-source');
    Check(not GStudio.Session.ProjectSnapshot.Pending and GStudio.SourceModal.IsOpen,
      'expanded Restore removes rejected diagnostics without retiring the editor');
    LSource.Text := LDraft;
    LSource.SelStart := 10;
    LSource.SelLength := 6;
    Click('action-apply-source');
    Check((GStudio.Session.Source = LDraft) and not GStudio.Session.ProjectSnapshot.Pending and
      GStudio.Session.CanUndo and GStudio.SourceModal.IsOpen and
      (GStudio.CodeView.InputFor('studio-code') = LSource),
      'expanded Apply admits one ordinary paired source edit through the retained editor');
    Click('action-expand-source');
    WriteLn('Return / modal ', GStudio.SourceModal.IsOpen, ' / owner ', GForm.Enabled,
      ' / draft ', GStudio.Session.DraftSource = LDraft, ' / selection ', LSource.SelStart,
      ':', LSource.SelLength, ' / same memo ', GStudio.CodeView.InputFor('studio-code') = LSource,
      ' / status ', GStudio.Status);
    Check(not GStudio.SourceModal.IsOpen and GForm.Enabled and
      (GStudio.Session.DraftSource = LDraft) and (LSource.SelStart = 10),
      'Close returns to the split editor with its exact draft/range and owner enabled');
    GForm.ClientWidth := 390;
    Pump;
    Click('action-expand-source');
    Check(GStudio.SourceModal.IsOpen and (LSource.Width > 250),
      'narrow expanded editor uses the available native viewport width');
    Capture(TForm(GStudio.SourceModal.Control), 'source-expanded-narrow');
    LKey := VK_ESCAPE;
    TForm(GStudio.SourceModal.Control).OnKeyDown(GStudio.SourceModal.Control, LKey, []);
    Pump;
    Check((LKey = 0) and not GStudio.SourceModal.IsOpen and GForm.Enabled and
      (GStudio.CodeView.InputFor('studio-code') = LSource) and
      (GStudio.Session.DraftSource = LDraft), 'Escape returns without replacing the editor or draft');
    Click('action-undo');
    Click('action-reset-source');
    Check((GStudio.Session.Source = LPair.Source) and not GStudio.Session.ProjectSnapshot.Pending and
      (GStudio.Session.Save = LPair.Design) and not GStudio.Session.CanUndo,
      'presentation changes spend no document history and ordinary Restore remains usable');
    WriteLn('PASS ', LChecks, ' actual native source workspace checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GForm.Free;
  GObserver.Free;
  Application.OnException := nil;
  Application.ProcessMessages;
end.
