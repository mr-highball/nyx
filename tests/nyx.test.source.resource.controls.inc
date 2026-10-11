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

{ The maintained full Studio consumer uses actual Nyx/LCL controls against its
  own handwritten project. The same journey runs locally and through the shared
  provider/backend seam. Only transport is replaced in the shared case; compiler
  execution, guarded publication, observing acknowledgement and paired history
  remain ordinary product behavior. No primary listener or project is touched. }
procedure ResourceControls(const ALabel: TNyxText);
const
  CEditor: TNyxText = 'studio-resource-editor';
  CName: TNyxText = 'crafted-copy';
  CInitial: TNyxText = 'A resource-backed notebook';
  CUpdated: TNyxText = 'A brighter resource-backed notebook';
var
  LInitial: TNyxProjectPair;
  LCreated: TNyxProjectPair;
  LUpdated: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LSourceInput: TControl;
  LSelection: TNyxText;
  LView: TNyxText;
  LBinding: TNyxBindingSpec;
  LBindInput: TCheckBox;
  LBeforeRefusal: TNyxSourceCommandState;
  LSelectedControl: TControl;

  procedure Ready(AHistoryReplies: Integer = 0);
  var
    LStarted: QWord;
    LState: TNyxDataValue;
    LPair: TNyxProjectPair;
  begin
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GSharedExchange = nil then
      begin
        Exit;
      end;

      if GStudio.Agents.Conflict then
      begin
        raise Exception.Create('Resource observation conflict: ' + GStudio.Agents.Status);
      end;
      LState := GSharedEngine.EditorExchange(GSharedExchange.Token,
        NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
      LPair := DecodeNyxProject(LState.Field('project').AsText);

      if (GSharedExchange.HistoryReplies >= AHistoryReplies) and
        not Assigned(GSharedExchange.Reply) and not GStudio.Agents.Busy and
        not GStudio.SourceBusy and not GStudio.PresentationPending and
        (LState.Field('session').Field('selection').AsText = GStudio.Session.SelectedID) and
        (LState.Field('session').Field('view').AsText = GStudio.Session.ActiveViewID) and
        (EncodeNyxProject(LPair) = EncodeNyxProject(GStudio.Session.ProjectSnapshot)) then
      begin
        Exit;
      end;
      Sleep(1);
    until GetTickCount64 - LStarted > 60000;
    raise Exception.Create('Resource pair was not acknowledged within sixty seconds');
  end;

  procedure Action(const AID: TNyxText);
  var
    LReplies: Integer;
  begin
    LReplies := 0;

    if (GSharedExchange <> nil) and ((AID = 'action-undo') or (AID = 'action-redo')) then
    begin
      LReplies := GSharedExchange.HistoryReplies + 1;
    end;
    Click(AID);
    Ready(LReplies);
  end;

  procedure TextField(AField: TNyxResourceEditorField; const AValue: TNyxText);
  var
    LInput: TControl;
  begin
    LInput := GStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, AField));
    Check(LInput is TCustomEdit, ALabel + ' actual resource text input is mounted');
    TCustomEdit(LInput).Text := AValue;
    Ready;
  end;

  procedure Choice(AField: TNyxResourceEditorField; const AValue: TNyxText);
  var
    LInput: TComboBox;
  begin
    LInput := TComboBox(GStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, AField)));
    Check(LInput <> nil, ALabel + ' actual resource choice is mounted');
    LInput.ItemIndex := LInput.Items.IndexOf(AValue);
    Check(LInput.ItemIndex >= 0, ALabel + ' resource choice exists: ' + AValue);
    LInput.OnChange(LInput);
    Ready;
  end;

  procedure BoundCaption(const ACaption: TNyxText);
  var
    LControl: TControl;
  begin
    Check(GStudio.Session.Document.Find('heading-1').FindBinding(bpText, LBinding) and
      (LBinding.Source = bsResource) and (LBinding.ResourceValue.Reference.Name = CName),
      ALabel + ' accepted heading has the exact typed resource binding');
    Check(GStudio.CanvasView.Root.Find('heading-1').Prop('text') = ACaption,
      ALabel + ' realized canvas receives the edited resource value');
    LControl := GStudio.CanvasView.ControlFor('heading-1');
    Check((LControl <> nil) and (TNyxText(TControlAccess(LControl).Caption) = ACaption),
      ALabel + ' actual native heading displays the resource value');
    Check(GStudio.CodeView.InputFor('studio-code') = LSourceInput,
      ALabel + ' resource compilation retains the independent source input');
    Check(Pos('NyxTextResource(TNotebookCards.Caption)', GStudio.Session.Source) > 0,
      ALabel + ' unrelated helper-defined resource stays authored');
  end;

  procedure OpenResource;
  var
    LList: TListBox;
  begin
    { Apply leaves a fresh proposal. Reopen the accepted exact variant through
      ordinary catalog selection rather than editing a blank New form or
      interpreting the previous command's Applied status as fresh publication. }
    LList := TListBox(GStudio.ShellView.ControlFor(
      NyxResourceBrowserListID('studio-resource-browser')));
    Check(LList <> nil, ALabel + ' actual resource catalog is mounted');
    LList.ItemIndex := LList.Items.IndexOf('Notebook copy (crafted-copy)');
    Check(LList.ItemIndex >= 0, ALabel + ' exact admitted resource is listed');
    LList.OnSelectionChange(LList, True);
    Ready;
    Action(NyxResourceBrowserActionID('studio-resource-browser', rbaOpen));
    Check(TCustomEdit(GStudio.ShellView.InputFor(
      NyxResourceEditorFieldID(CEditor, refName))).Text = CName,
      ALabel + ' catalog Open edits the exact retained variant');
  end;

begin
  LInitial := GStudio.Session.ProjectSnapshot;
  LSelection := GStudio.Session.SelectedID;
  LView := GStudio.Session.ActiveViewID;
  LSourceInput := GStudio.CodeView.InputFor('studio-code');
  { Selection is part of the shared acknowledged frame. Take the ordinary
    canvas route, which publishes navigation; direct session selection would
    leave the bridge correctly waiting even when both project pairs match. }
  LSelectedControl := GStudio.CanvasView.ControlFor('heading-1');
  Check(LSelectedControl <> nil, ALabel + ' resource binding target has an actual canvas control');
  TControlAccess(LSelectedControl).Click;
  Ready;
  Check(GStudio.Session.SelectedID = 'heading-1', ALabel + ' actual canvas selects the resource owner');
  Action('action-resources-toggle');
  Action(NyxResourceEditorActionID(CEditor, reaNew));
  TextField(refName, CName);
  Choice(refKind, 'JSON');
  TextField(refContent, '{"title":"' + CInitial + '"}');
  TextField(refTitle, 'Notebook copy');
  TextField(refDescription, 'A caption supplied by packed project data.');
  Action(NyxResourceEditorActionID(CEditor, reaPreview));
  Choice(refTarget, NyxBindingPropertyTitle(bpText));
  Choice(refPath, 'Root["title"] / text');
  LBindInput := TCheckBox(GStudio.ShellView.InputFor(
    NyxResourceEditorFieldID(CEditor, refBind)));
  Check(LBindInput <> nil, ALabel + ' actual resource binding toggle is mounted');
  LBindInput.Checked := True;
  LBindInput.OnChange(LBindInput);
  Ready;
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LInitial),
    ALabel + ' real resource proposal/preview leaves the accepted pair unchanged');
  Action(NyxResourceEditorActionID(CEditor, reaApply));
  Check(GStudio.SourceCommands.State = nssApplied,
    ALabel + ' actual resource Apply compiles and publishes: ' + GStudio.Status);
  LCreated := GStudio.Session.ProjectSnapshot;
  Action('action-resources-close');
  BoundCaption(CInitial);
  Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + ALabel + '-resource-created.png');
  Action('action-undo');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LInitial),
    ALabel + ' one physical Undo restores the exact pre-resource pair');
  Action('action-redo');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LCreated),
    ALabel + ' one physical Redo restores the exact resource pair');
  BoundCaption(CInitial);
  Action('action-resources-toggle');
  OpenResource;
  TextField(refContent, '{"title":"' + CUpdated + '"}');
  Action(NyxResourceEditorActionID(CEditor, reaPreview));
  Action(NyxResourceEditorActionID(CEditor, reaApply));
  Check(GStudio.SourceCommands.State = nssApplied,
    ALabel + ' actual resource update publishes: ' + GStudio.Status);
  LUpdated := GStudio.Session.ProjectSnapshot;
  Check((LUpdated.Source <> LCreated.Source) and
    (GStudio.Session.Document.Resources.Definition(NyxResourceRef(CName), NyxDefaultLocale)
      .Data.Field('title').AsText = CUpdated),
    ALabel + ' resource update is a fresh complete publication');
  Action('action-resources-close');
  BoundCaption(CUpdated);
  Action('action-undo');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LCreated),
    ALabel + ' resource update owns one exact paired Undo');
  Action('action-redo');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LUpdated),
    ALabel + ' resource update owns one exact paired Redo');
  { Deliberate unfinished code is entered through the real independent memo.
    A grouped resource action must report a useful rejection and retain it. }
  TMemo(LSourceInput).Text := LUpdated.Source + #10 + '{ A still unfinished resource idea }';
  Ready;
  LPending := GStudio.Session.ProjectSnapshot;
  Action('action-resources-toggle');
  OpenResource;
  LBeforeRefusal := GStudio.SourceCommands.State;
  Action(NyxResourceEditorActionID(CEditor, reaApply));
  Check((GStudio.SourceCommands.State = LBeforeRefusal) and
    (Pos('Resolve the pending Pascal draft', GStudio.Status) > 0) and
    (Pos('unavailable', GStudio.Status) = 0),
    ALabel + ' editor draft guard refuses before queue admission with useful guidance');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LPending),
    ALabel + ' rejected resource input retains exact draft/base/design/source');
  Action('action-resources-close');
  Capture(IncludeTrailingPathDelimiter(ParamStr(4)) + ALabel + '-resource-draft-refused.png');
  Action('action-reset-source');
  Check(not GStudio.Session.SourceDraftPending,
    ALabel + ' explicit Restore accepted resolves the unfinished draft');
  Action('action-undo');
  Action('action-undo');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LInitial),
    ALabel + ' fixture restores only its original owned pair');
  Check(GStudio.Session.ActiveViewID = LView, ALabel + ' resource journey retains the original view');
  LSelectedControl := GStudio.CanvasView.ControlFor(LSelection);
  Check(LSelectedControl <> nil, ALabel + ' original selection still has its owned canvas control');
  TControlAccess(LSelectedControl).Click;
  Ready;
  Check(GStudio.Session.SelectedID = LSelection, ALabel + ' physical selection restores original navigation');
end;
