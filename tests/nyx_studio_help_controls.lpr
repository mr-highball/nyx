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

program nyx_studio_help_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls,
  nyx.text, nyx.model, nyx.codec, nyx.studio.projects, nyx.studio.lcl,
  nyx.studio.help, nyx.generated.view;

type
  TControlAccess = class(TWinControl);

function ReadSource(const APath: String): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size = 0) or (LFile.Size > 1048576) then
    begin
      raise Exception.Create('Companion exceeds source fixture budget');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

function HelpWindow: TForm;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LIndex].Visible and
      (Screen.Forms[LIndex].Caption = 'About this component') then
    begin
      Exit(Screen.Forms[LIndex]);
    end;
  end;
end;

procedure Require(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

var
  LForm: TForm;
  LStudio: TNyxNativeStudio;
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LBefore: TNyxText;
  LStarted: QWord;
  LHelp: TForm;
begin
  LForm := nil;
  LStudio := nil;
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Use native Studio help <exact companion.pas> <fixture directory>');
    end;
    Application.Initialize;
    LForm := TForm.CreateNew(nil);
    LForm.SetBounds(30, 30, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm, ParamStr(2));
    LDocument := BuildNyxDocument;
    try
      LSource := ReadSource(ParamStr(1));
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), LSource));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('notes-memo');
    LStudio.Run;
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Native Studio did not finish its initial presentation');
      end;
      Sleep(1);
    until not LStudio.PresentationPending;
    Require(LStudio.ShellView.ControlFor(NyxStudioComponentHelpID) <> nil,
      'Ordinary Inspector mounts its public help action');
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioComponentHelpID)).Click;
    Application.ProcessMessages;
    LHelp := HelpWindow;
    Require((LHelp <> nil) and LForm.Enabled, 'Actual native Studio opens nonmodal help');
    Require((Screen.ActiveControl <> nil) and
      (TControlAccess(Screen.ActiveControl).Caption = 'Close'),
      'Initial focus reaches the ordinary Nyx close control');
    TControlAccess(Screen.ActiveControl).Click;
    Application.ProcessMessages;
    Require(HelpWindow = nil, 'Typed semantic dismiss closes actual Studio help');
    Require(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Help preserves the exact accepted project/source pair');
    WriteLn('PASS 5 ordinary native Studio component-help checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  LStudio.Free;
  LForm.Free;
end.
