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

program nyx_source_compilation_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, SysUtils, Classes, Forms, Controls, StdCtrls,
  Graphics, IntfGraphics, FPWritePNG, nyx.text, nyx.bytes, nyx.data, nyx.model,
  nyx.studio.lcl, nyx.studio.outputs, nyx.studio.directories,
  nyx.studio.sourcecompilation.native, nyx.studio.buildexecutor;

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
  GChecks: Integer;

procedure TObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled source controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReadText(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size < 1) or (LFile.Size > 4 * 1024 * 1024) then
    begin
      raise Exception.Create('Owned qualification input requires bounded bytes');
    end;
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    Result := NyxDecodeUTF8(LBytes);
  finally
    LFile.Free;
  end;
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if GObserver.Failure <> '' then
    begin
      raise Exception.Create(GObserver.Failure);
    end;

    if not GStudio.SourceBusy and not GStudio.PresentationPending then
    begin
      Exit;
    end;
    Sleep(1);
  until GetTickCount64 - LStarted > 180000;
  raise Exception.Create('The live native source/presentation command exceeded its budget');
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
  Check(LControl <> nil, 'real mounted action ' + AID);
  TControlAccess(LControl).Click;
  Pump;
end;

procedure Capture(const APath: TNyxText);
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
    GForm.PaintTo(LBitmap.Canvas.Handle, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(APath, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

var
  LDirectories: TNyxStudioDirectories;
  LProfile: TNyxOutputConfiguration;
  LTools: TNyxDataValue;
  LSource: TNyxText;
  LExpected: TNyxText;
  LBeforeSource: TNyxText;
  LBeforeDesign: TNyxText;
  LInput: TMemo;
begin
  LProfile := nil;
  try

    if (ParamCount <> 4) or DirectoryExists(ParamStr(4)) or FileExists(ParamStr(4)) then
    begin
      raise Exception.Create('Supply repository, toolchain, compiled fixture home and NEW evidence home');
    end;
    ForceDirectories(ParamStr(4));
    LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
      .RunningIn(IncludeTrailingPathDelimiter(ParamStr(4)) + 'runtime');
    LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
    LProfile := TNyxOutputConfiguration.Create;
    LProfile.SetField('fpc', LTools.Field('FPC').AsText);
    LSource := ReadText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'web/source.pas');
    LExpected := ReadText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'web/expected.nyx');
    Application.Initialize;
    GObserver := TObserver.Create;
    Application.OnException := GObserver.Failed;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(30, 30, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm, LDirectories.Projects,
      NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode, TNyxCompilerLimits.Default));
    GStudio.Run;
    Pump;
    LBeforeSource := GStudio.Session.Source;
    LBeforeDesign := GStudio.Session.Save;
    Click('action-code');
    LInput := TMemo(GStudio.CodeView.InputFor('studio-code'));
    Check(LInput <> nil, 'ordinary native Pascal editor is mounted');
    LInput.Text := LSource;
    Pump;
    Check(GStudio.Session.DraftSource = LSource, 'physical memo captures exact complete Pascal');
    Click('action-apply-source');
    Check((GStudio.Session.Source = LSource) and (GStudio.Session.Save = LExpected) and
      not GStudio.Session.SourceDraftPending, 'real Apply compiles/evaluates helpers and loops');
    Check((GStudio.Session.ActiveViewID = 'notebook-1') and
      (GStudio.CanvasView.Root <> nil), 'compiled root replacement mounts its actual native view');
    Check(GStudio.Session.CanUndo and not GStudio.Session.CanRedo,
      'physical Apply owns one paired Undo');
    Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + 'compiled-apply.png');
    Click('action-undo');
    Check((GStudio.Session.Source = LBeforeSource) and (GStudio.Session.Save = LBeforeDesign),
      'real Undo restores the complete initial pair');
    Click('action-redo');
    Check((GStudio.Session.Source = LSource) and (GStudio.Session.Save = LExpected),
      'real Redo restores the exact compiled pair');
    Check(GStudio.CodeView.InputFor('studio-code') = LInput,
      'compiler publication/history retains the actual source input');
    WriteLn('PASS ', GChecks, ' actual native compiler source controls');
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
  LProfile.Free;
  GObserver.Free;
  Application.OnException := nil;
  Application.ProcessMessages;
end.
