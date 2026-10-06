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


program nyx_constraints_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Interfaces, Forms, Controls, StdCtrls, Spin,
  nyx.text, nyx.types, nyx.model, nyx.studio.projects, nyx.studio.lcl,
  nyx.studio.inspector, nyx.studio.sourcejobs, nyx.test.constraints;

type
  TControlAccess = class(TControl);
  TObserver = class
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;

var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LObserver: TObserver;
  LChecks: Integer;
  LBefore, LAfter: TNyxProjectPair;
  LCode: TControl;

procedure TObserver.Failed(ASender: TObject; AError: Exception);
begin
  Failure := TNyxText(AError.Message);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual Studio size constraints: ' + AReason);
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

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Size authoring work did not finish');
    end;
    Sleep(1);
  until False;
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
begin
  WriteLn('Studio size constraints / click ', AID);
  Flush(Output);
  LControl := LStudio.ShellView.ControlFor(AID);
  Check(LControl <> nil, 'ordinary Nyx action is mounted: ' + AID);
  TControlAccess(LControl).Click;
  Pump;
end;

procedure WriteBound(const AKey: TNyxText; AValue: Integer);
var
  LInput: TSpinEdit;
begin
  WriteLn('Studio size constraints / field ', AKey, ' = ', AValue);
  Flush(Output);
  LInput := TSpinEdit(LStudio.ShellView.InputFor('inspector-' + AKey));
  Check(LInput <> nil, 'published integer field is mounted: ' + AKey);
  LInput.Value := AValue;
  LInput.OnChange(LInput);
  Pump;
  WriteLn('Studio size constraints / status ', LStudio.Status);
  Flush(Output);
end;

begin
  try
    Application.Initialize;
    LObserver := TObserver.Create;
    Application.OnException := LObserver.Failed;
    LForm := TForm.CreateNew(nil);
    LForm.SetBounds(20, 20, 1280, 900);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm,
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    try
      LStudio.LoadProject(NyxConstraintsFixture);
      LStudio.Run;
      Pump;
      TControlAccess(LStudio.CanvasView.ControlFor('notes-editor')).Click;
      Pump;
      Check(LStudio.Session.SelectedID = 'notes-editor', 'actual canvas selects its constrained editor');
      Click('action-code');
      LCode := LStudio.CodeView.InputFor('studio-code');
      Check(LCode <> nil, 'retained Pascal editor is mounted');
      WriteBound('max-width', 300);
      Check(LStudio.Session.Selected.Prop('max-width') = '300', 'ordinary field admits a typed maximum');
      Check(Pos('.MaximumWidth(300)', LStudio.Session.ProjectSnapshot.Source) > 0,
        'ordinary field synchronizes typed adjacent source');
      LBefore := LStudio.Session.ProjectSnapshot;
      WriteBound('max-width', 170);
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'inverted field proposal retains the exact accepted pair');
      Check((LStudio.SourceCommands.State = nssRejected) and
        (LStudio.Status = LStudio.SourceCommands.Message) and
        (LStudio.ShellView.Root.Find('inspector-max-width').Prop('value') = '300'),
        'rejection reports its diagnostic and restores the accepted inspector value');
      Click('inspector-unset-max-width');
      Check(LStudio.Session.Selected.Prop('max-width') = '', 'unset differs from an explicit zero');
      Check(Pos('.Clear(atMaximumWidth)', LStudio.Session.ProjectSnapshot.Source) > 0,
        'unset generates a typed clear');
      LAfter := LStudio.Session.ProjectSnapshot;
      Click('action-undo');
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'unset has one paired Undo');
      Click('action-redo');
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'unset has exact paired Redo');
      WriteBound('max-height', 0);
      Check(LStudio.Session.Selected.Prop('max-height') = '0', 'explicit zero reaches the actual editor workflow');
      Click('inspector-unset-max-height');
      Check(LStudio.Session.Selected.Prop('max-height') = '', 'zero can be unset independently');
      Check(LStudio.CodeView.InputFor('studio-code') = LCode, 'size authoring retains source editor identity');
      Check(Pos('Keep this handwritten English comment.', LStudio.Session.ProjectSnapshot.Source) > 0,
        'size authoring retains handwritten source');
      WriteLn('PASS ', LChecks, ' actual Studio size constraint checks');
    finally
      LStudio.Free;
      LForm.Free;
      Check(LObserver.Failure = '', 'teardown reports no deferred shell failure: ' + LObserver.Failure);
      Application.OnException := nil;
      LObserver.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
