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



program nyx_state_source_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Types, Forms, Controls, StdCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.types, nyx.model, nyx.state,
  nyx.binding.types, nyx.studio.authoring, nyx.studio.projects, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.studio.lcl, nyx.test.agent.state;

type
  TControlAccess = class(TControl);
  { Real LCL exception dispatch must fail the journey, including worker retirement.
    This observer owns neither the editor nor any of its presentation controls. }
  TFailureObserver = class
  public
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
  Error := AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition or (GObserver.Error <> '') then
  begin
    raise ENyxState.Create('State controls: ' + AReason + ' / ' + GObserver.Error);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  CheckSynchronize;
  Application.ProcessMessages;
end;

procedure Ready;
var
  LStart: QWord;
begin
  LStart := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStart > 30000 then
    begin
      raise ENyxState.Create('State command did not retire / ' + GStudio.Status);
    end;
    Sleep(1);
  until not GStudio.SourceCommands.Busy;
  Pump;
end;

procedure Click(const AID: TNyxText; AWait: Boolean = True);
var
  LControl: TControl;
begin
  LControl := GStudio.ShellView.ControlFor(AID);
  Check(LControl <> nil, 'Actual command exists / ' + AID);
  TControlAccess(LControl).Click;

  if AWait then
  begin
    Ready;
  end;
end;

procedure Text(const AID, AValue: TNyxText);
var
  LInput: TControl;
begin
  LInput := GStudio.ShellView.InputFor(AID);
  Check(LInput is TCustomEdit, 'Actual text editor exists / ' + AID);
  TCustomEdit(LInput).Text := AValue;
end;

procedure Choice(const AID, AValue: TNyxText);
var
  LCombo: TComboBox;
  LIndex: Integer;
begin
  LCombo := TComboBox(GStudio.ShellView.InputFor(AID));
  Check(LCombo <> nil, 'Actual choice editor exists / ' + AID);
  LIndex := LCombo.Items.IndexOf(AValue);
  Check(LIndex >= 0, 'Closed choice exists / ' + AValue);
  LCombo.ItemIndex := LIndex;
  LCombo.OnChange(LCombo);
end;

function PairText: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
end;

procedure Select(const AID: TNyxText);
begin
  GStudio.Session.Select(AID);
  GStudio.RequestRefresh;
  Pump;
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
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure CaptureReview;
var
  LScroll: TScrollBox;
  LRow: TControl;
  LPosition: TPoint;

  procedure RevealStateRow;
  begin
    { This is the actual nested Project scrollbar, independent of the canvas's
      logical viewport. Root Reveal cannot establish nested sidebar scrolling. }
    LScroll := TScrollBox(GStudio.ShellView.ControlFor('studio-left'));
    LRow := GStudio.ShellView.ControlFor('state-row-0');
    LPosition := LScroll.ScreenToClient(LRow.ClientToScreen(Point(0, 0)));
    LScroll.VertScrollBar.Position := LScroll.VertScrollBar.Position + LPosition.Y;
    Pump;
  end;

begin
  RevealStateRow;
  Capture('state-inspector-desktop');
  GForm.ClientWidth := 390;
  GStudio.RequestRefresh;
  Pump;
  Click('action-panel-project');
  RevealStateRow;
  Capture('state-inspector-390');
end;

procedure Run;
var
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LBeforeSource: TNyxText;
  LSpec: TNyxBindingSpec;
  LPending: TNyxStudioPendingDesign;
  LValue: TNyxText;
  LFailed: Boolean;
  LEdit: TNyxStudioDesignEdit;
begin
  Application.Initialize;
  ForceDirectories(ParamStr(1));
  GObserver := TFailureObserver.Create;
  Application.OnException := GObserver.Failed;
  GForm := TForm.Create(nil);
  GForm.ClientWidth := 1100;
  GForm.ClientHeight := 800;
  GStudio := TNyxNativeStudio.Create(GForm,
    IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
  try
    LSeed := CreateNyxAgentStateSeed;
    GStudio.LoadProject(LSeed);
    GStudio.Run;
    GForm.Show;
    Pump;
    Click(NyxStudioStateToggleID);

    if ParamStr(2) = 'capture' then
    begin
      { Bounded English painting after qualified behavior. This optional mode
        makes no source/history, runtime input or omitted-journey pass claim. }
      CaptureReview;
      Exit;
    end;
    LBefore := PairText;
    Text('state-name-0', 'r');
    Text('state-name-0', 'res');
    Text('state-name-0', 'response');
    Check((PairText = LBefore) and not GStudio.SourceCommands.Busy and
      not GStudio.Session.CanUndo, 'Rapid name typing is presentation only');
    GStudio.RequestRefresh;
    Pump;
    Check(TCustomEdit(GStudio.ShellView.InputFor('state-name-0')).Text = 'response',
      'Unrelated shell painting retains the full uncommitted name draft');
    Click(NyxStudioStateToggleID);
    Click(NyxStudioStateToggleID);
    Check(TCustomEdit(GStudio.ShellView.InputFor('state-name-0')).Text = 'response',
      'Panel navigation retains the project-owned name draft');

    Text('state-name-0', 'checked');
    Click('state-rename-0', False);
    Check(GStudio.SourceCommands.Busy and (PairText = LBefore),
      'Explicit Rename queues intent without synchronously changing source');
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaRemoveStateDefault;
    LEdit.Selection := GStudio.Session.SelectedID;
    LEdit.View := GStudio.Session.ActiveViewID;
    LEdit.Name := 'reply';
    LFailed := False;
    try
      GStudio.SourceCommands.Edit(LEdit);
    except
      on ENyxState do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Pre-paint row guard refuses a stale identity while Rename is pending');
    Ready;
    Check((PairText = LBefore) and
      (TCustomEdit(GStudio.ShellView.InputFor('state-name-0')).Text = 'checked'),
      'Rejected colliding name retains exact files/history and the editable draft');
    Text('state-name-0', 'response');
    Click('state-rename-0');
    Check(GStudio.Session.Document.State.Has('response') and
      (GStudio.Session.Document.Find('definition-editor').Bindings[0].StateName = 'response') and
      (GStudio.Session.Document.Find('review-label').Bindings[0].StateName = 'response'),
      'Real Rename migrates all authored references and generated source');
    GStudio.Session.Undo;
    Check(PairText = LBefore, 'One paired Undo restores the exact original files');
    GStudio.Session.Redo;
    GStudio.RequestRefresh;
    Pump;

    Text('state-default-0', 'First reply');
    Text('state-default-0', 'Second reply');
    Text('state-default-0', 'Latest reply');
    LPending := GStudio.SourceCommands.PendingDesign;
    Check(LPending.StateValue('response', nskText, LValue) and (LValue = 'Latest reply'),
      'Latest rapid input stays visible above earlier independent preparation');
    Ready;
    Check(GStudio.Session.Document.State.Value('response').TextValue = 'Latest reply',
      'Queued defaults admit the latest full text');
    GStudio.Session.Undo;
    Check(GStudio.Session.Document.State.Value('response').TextValue = 'First reply',
      'Adjacent waiting values coalesce while the active command keeps its paired history');
    GStudio.Session.Redo;
    GStudio.RequestRefresh;
    Pump;

    Choice('state-default-1', 'true');
    Ready;
    Text('state-default-2', '-2147483648');
    Ready;
    Check(GStudio.Session.Document.State.Value('checked').BooleanValue and
      (GStudio.Session.Document.State.Value('quantity').IntegerValue = Low(Integer)),
      'Actual Boolean and Integer editors retain exact scalar families');
    LBefore := PairText;
    { Retained Studio sections own independent renderers. Reveal through the
      exact mounted owner so real focus/scroll checks use its current control. }
    GStudio.ShellView.ViewFor('state-default-3').Reveal('state-default-3');
    TCustomEdit(GStudio.ShellView.InputFor('state-default-3')).SetFocus;
    Text('state-default-3', '-');
    Ready;
    Check((PairText = LBefore) and
      (TCustomEdit(GStudio.ShellView.InputFor('state-default-3')).Text = '0.125'),
      'Invalid partial Number restores its accepted physical editor without publication');
    Check(Screen.ActiveControl = GStudio.ShellView.InputFor('state-default-3'),
      'Rejected Number retains focus on its exact scalar editor');
    Text('state-default-3', '-');
    Text('state-default-3', '0.875');
    LPending := GStudio.SourceCommands.PendingDesign;
    Check(LPending.StateValue('ratio', nskNumber, LValue) and (LValue = '0.875'),
      'A later valid Number remains visible while an earlier partial input is rejected');
    Ready;
    Check((GStudio.Session.Document.State.Value('ratio').NumberValue = 0.875) and
      (TCustomEdit(GStudio.ShellView.InputFor('state-default-3')).Text = '0.875'),
      'Rejected earlier input cannot overwrite newer admitted numeric text');
    Check(Screen.ActiveControl = GStudio.ShellView.InputFor('state-default-3'),
      'Later coalesced Number keeps the focused editor through background completion');

    Text(NyxStudioNewStateNameID, 'exactText');
    Choice(NyxStudioNewStateInputID, NyxStudioStateInputName(ssiEscapedText));
    Pump;
    Text(NyxStudioNewStateValueID, '"Hello 🌙\u0000world"');
    Click(NyxStudioAddStateID, False);
    Click(NyxStudioAddStateID, False);
    Ready;
    Check((GStudio.Session.Document.State.Count = 5) and
      (GStudio.Session.Document.State.Value('exactText').TextValue = TNyxText('Hello 🌙' + #0 + 'world')),
      'One form submission creates an exact supplementary/NUL default despite a rapid second click');
    Check(TCustomEdit(GStudio.ShellView.InputFor(NyxStudioNewStateNameID)).Text = '',
      'Only successful creation clears the exact submitted form name');
    Text('state-default-4', '"Next 🌙\u0000reply"');
    Text('state-default-4', '"Next plain reply"');
    Text('state-default-4', '"Latest plain reply"');
    Ready;
    Check(GStudio.Session.Document.State.Value('exactText').TextValue = 'Latest plain reply',
      'Escaped notation remains attached to later queued text across earlier publication');
    Click('state-remove-4');
    Check(not GStudio.Session.Document.State.Has('exactText'),
      'Actual removal updates the accepted pair for an unused default');

    Select('reply-memo');
    Click(NyxStudioBindingsToggleID);
    Click('binding-state-0', False);
    Select('reply-label');
    Ready;
    Check((GStudio.Session.SelectedID = 'reply-label') and
      GStudio.Session.Document.Find('reply-memo').FindBinding(bpValue, LSpec) and
      (LSpec.StateName = 'response') and
      (GStudio.Session.Document.Find('reply-label').BindingCount = 0),
      'Selection while binding preparation runs cannot retarget its authored owner');
    Select('reply-memo');
    Choice(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdFromState));
    Choice(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdTwoWay));
    Choice(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdFromState));
    Ready;
    Check(GStudio.Session.Selected.FindBinding(bpValue, LSpec) and (LSpec.Direction = bdFromState),
      'Rapid flow choices use the pending descriptor and retain the latest direction');
    Click('binding-clear');
    Check(not GStudio.Session.Selected.FindBinding(bpValue, LSpec),
      'Actual Unbind queues the typed local clear');

    Select('first-editor');
    Click('binding-clear');
    Check(GStudio.Session.Selected.BindingCount = 1, 'Clear owns an independent reusable override');
    Click('binding-inherit');
    Check(GStudio.Session.Selected.BindingCount = 0, 'Inherit removes only that local descriptor');

    LBeforeSource := GStudio.Session.Source;
    GStudio.Session.SetSourceDraft(LBeforeSource + #10 + '{ User draft remains independent }');
    LBefore := PairText;
    Text('state-default-0', 'Edited beside Pascal draft');
    Ready;
    Check((PairText <> LBefore) and (GStudio.Session.Source <> LBeforeSource) and
      GStudio.Session.ProjectSnapshot.Pending and
      (GStudio.Session.ProjectSnapshot.DraftBase = LBeforeSource) and
      (GStudio.Session.DraftSource = LBeforeSource + #10 + '{ User draft remains independent }') and
      (GStudio.Session.Document.State.Value('response').TextValue = 'Edited beside Pascal draft'),
      'Ordinary visual editing preserves the exact Pascal draft and its original stale base');
    GStudio.Session.DiscardSourceDraft;
    GStudio.RequestRefresh;
    Pump;

    Text('state-name-0', 'Old project draft');
    Text('state-default-0', 'Old active value');
    Text('state-default-0', 'Old waiting value');
    { Opening is now an ordinary admitted project command. Pending visual work
      must retire before replacement; refusing it preserves the exact owner. }
    LBefore := PairText;
    LFailed := False;
    try
      GStudio.LoadProject(LSeed);
    except
      on LException: ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (PairText = LBefore) and GStudio.SourceCommands.Busy,
      'Opening another project refuses while exact visual work is pending');
    Ready;
    GStudio.LoadProject(LSeed);
    Text('state-default-0', 'Old mounted input after replacement');
    Ready;
    Check(GStudio.Session.Source = LSeed.Source,
      'Input from the old mounted shell cannot replay into the admitted project');
    Check(not GStudio.SourceCommands.Busy and
      not GStudio.SourceCommands.PendingDesign.StateName('reply', nskText, LValue),
      'A new load cannot inherit name drafts or pending input with identical names');
    Pump;
    Text('state-default-0', 'Fresh project reply');
    Ready;
    Check(GStudio.Session.Document.State.Value('reply').TextValue = 'Fresh project reply',
      'Retired workers cannot replay their state names into the new load');

    GStudio.LoadProject(LSeed);
    Ready;
    Pump;
    CaptureReview;
    Text('state-default-0', 'Compact English reply');
    Ready;
    Check(GStudio.Session.Document.State.Value('reply').TextValue = 'Compact English reply',
      'Actual compact Project control uses the same isolated admission');
    Text('state-default-0', 'Retire while preparing');
    Check(GStudio.SourceCommands.Busy, 'Editor retirement starts with actual preparation');
    FreeAndNil(GStudio);
    Pump;
    Check(GObserver.Error = '', 'Detached state work cannot call freed editor or controls');
  finally
    GStudio.Free;
    GStudio := nil;
    GForm.Free;
    Application.OnException := nil;
    GObserver.Free;
  end;
end;

begin
  try
    Run;
    if ParamStr(2) = 'capture' then
    begin
      WriteLn('PASS English desktop/390 state inspector painting only');
    end
    else
    begin
      WriteLn('PASS ', GChecks, ' actual native queued state/binding checks');
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
