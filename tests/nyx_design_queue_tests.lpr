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

program nyx_design_queue_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs;

type
  { Presentation is a host extension boundary. These intentional failures prove
    that its exception cannot consume accepted history or strand later intent.
    The real native scheduler and CheckSynchronize deliver both candidates. }
  TPresentation = class
  public
    ThrowPreparing: Boolean;
    ThrowApplied: Boolean;
    Applied: Integer;
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TPresentation.Changed(AState: TNyxSourceCommandState;
  const AMessage: TNyxText);
begin

  if (AState = nssPreparing) and ThrowPreparing then
  begin
    ThrowPreparing := False;
    raise ENyxModel.Create('Intentional preparing presentation failure');
  end;

  if AState = nssApplied then
  begin
    Inc(Applied);

    if ThrowApplied then
    begin
      ThrowApplied := False;
      raise ENyxModel.Create('Intentional applied presentation failure');
    end;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LPresentation: TPresentation;
  LEdit: TNyxStudioDesignEdit;
  LBefore: TNyxProjectPair;
  LStarted: QWord;
  LRefused: Boolean;
  LSave: TNyxNode;
begin
  LDocument := TNyxDocument.Create;
  LDocument.Title := 'Queue presentation review';
  LDocument.AddPage(NewNyxPage('home').Add(NewNyxLabel('caption').WithText('English caption')));
  LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  LDocument.Free;
  LSession := TNyxStudioSession.Create(LBefore);
  LPresentation := TPresentation.Create;
  LCommands := TNyxSourceCommands.Create(LSession, LPresentation.Changed);
  try
    LBefore := LSession.ProjectSnapshot;
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Selection := 'home';
    LEdit.View := 'home';
    LEdit.Value := 'First title';
    LPresentation.ThrowPreparing := True;
    LPresentation.ThrowApplied := True;
    LRefused := False;
    try
      LCommands.Edit(LEdit);
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LCommands.Busy,
      'Preparing callback exception leaves admitted intent scheduled');
    LEdit.Value := 'Second title';
    LCommands.Edit(LEdit);
    LSave := TNyxNode.Create(nkButton, 'action-save');
    try
      Check(LCommands.Route(LSave, ntClick) and LCommands.Busy and
        (LSession.Source = LBefore.Source),
        'Save cannot announce an earlier accepted pair while visible edits remain pending');
    finally
      LSave.Free;
    end;
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise ENyxModel.Create('Presentation exception stranded the design queue');
      end;
      Sleep(1);
    until not LCommands.Busy;
    Check(LPresentation.Applied = 2,
      'Applied presentation exception cannot strand the next FIFO command');
    Check(LSession.Document.Title = 'Second title',
      'Both real worker results publish their ordered meaning');
    LSession.Undo;
    Check(LSession.Document.Title = 'First title',
      'One Undo restores the command whose presentation raised');
    LSession.Undo;
    Check((LSession.Save = LBefore.Design) and (LSession.Source = LBefore.Source) and
      not LSession.CanUndo, 'Second Undo restores the exact initial pair');
    Check(LSession.CanRedo, 'Presentation failures preserve paired Redo');

    LEdit.Value := 'Retired active title';
    LCommands.Edit(LEdit);
    LEdit.Action := sdaProperty;
    LEdit.Selection := 'caption';
    LEdit.Name := 'text';
    LEdit.Value := 'Retired waiting caption';
    LCommands.Edit(LEdit);
    LSession.LoadProject(LBefore);
    Check(not LCommands.Busy and
      (Length(LCommands.PendingDesign.Fields) = 0) and
      not LCommands.PendingDesign.TitleDefined,
      'Earlier-load work cannot claim or paint pending input in a newly opened project');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTitle;
    LEdit.Selection := 'home';
    LEdit.View := 'home';
    LEdit.Value := 'New project title';
    LCommands.Edit(LEdit);
    LStarted := GetTickCount64;
    repeat
      CheckSynchronize;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise ENyxModel.Create('New-load command did not retire');
      end;
      Sleep(1);
    until not LCommands.Busy;
    Check((LSession.Document.Title = 'New project title') and
      (LSession.Document.Find('caption').Prop('text') = 'English caption'),
      'Waiting old intent cannot retarget identical IDs in another project load');
    LSession.Undo;
    Check((LSession.Source = LBefore.Source) and (LSession.Save = LBefore.Design) and
      not LSession.CanUndo, 'Only the new-load command contributes paired history');
  finally
    LCommands.Free;
    LPresentation.Free;
    LSession.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' real native queue/presentation checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
