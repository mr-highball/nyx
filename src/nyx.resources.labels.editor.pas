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
unit nyx.resources.labels.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.resources, nyx.model, nyx.controls;

type
  TNyxResourceLabelsEditorPurpose = (rlepAssign, rlepFilter);
  TNyxResourceLabelsEditorField = (rlefInput, rlefSelection);
  TNyxResourceLabelsEditorAction = (rleaAdd, rleaRemove);

  { Copied presentation state, independent of a resource or target control.
    Input deliberately allows incomplete names. Selection is undefined or an
    exact member of Labels. Admission checks the complete candidate before any
    mounted proposal changes; no document, widget or interface is retained. }
  TNyxResourceLabelsEditorState = record
    Labels: TNyxResourceLabels;
    Input: TNyxText;
    Selection: TNyxResourceLabelRef;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResourceLabelsEditorState; static;
  end;

{ Fixed ordinary Nyx descendants keep their identities as labels change. The
  returned card owns its controls; the host owns synchronization and event
  subscriptions. Purpose changes disclosure only, not label semantics. }
function NewNyxResourceLabelsEditor(const AID: TNyxText;
  const ALabels: TNyxResourceLabels;
  APurpose: TNyxResourceLabelsEditorPurpose = rlepAssign): INyxCard;
function NyxResourceLabelsEditorFieldID(const AID: TNyxText;
  AField: TNyxResourceLabelsEditorField): TNyxText;
function NyxResourceLabelsEditorActionID(const AID: TNyxText;
  AAction: TNyxResourceLabelsEditorAction): TNyxText;
{ Borrow a complete mounted compound. Restore normalizes a detached candidate;
  malformed labels or a foreign selection refuse before writing properties.
  Synchronize the owning view afterward; accepted resources remain untouched. }
function ReadNyxResourceLabelsEditor(AEditor: TNyxNode): TNyxResourceLabelsEditorState;
procedure RestoreNyxResourceLabelsEditor(AEditor: TNyxNode;
  const AState: TNyxResourceLabelsEditorState);
{ Recognize exact owned descendants, never IDs copied onto a foreign node.
  Add admits the whole typed name/set, selects it and clears Input on success.
  Duplicate Add is idempotent. Remove requires an existing exact selection.
  Invalid input raises with the old proposal intact; unrelated actions return
  False. No control is retired while its callback is being delivered. }
function NyxResourceLabelsEditorInput(ANode, AEditor: TNyxNode): Boolean;
function HandleNyxResourceLabelsEditorAction(AButton, AEditor: TNyxNode): Boolean;

implementation

uses nyx.layout.policy;

const
  CLabels = 'nyx.resource-labels.editor.labels';
  CFields: array[TNyxResourceLabelsEditorField] of TNyxText = ('input', 'selection');
  CActions: array[TNyxResourceLabelsEditorAction] of TNyxText = ('add', 'remove');

function NyxResourceLabelsEditorFieldID(const AID: TNyxText;
  AField: TNyxResourceLabelsEditorField): TNyxText;
begin
  Result := AID + TNyxText('-') + CFields[AField];
end;

function NyxResourceLabelsEditorActionID(const AID: TNyxText;
  AAction: TNyxResourceLabelsEditorAction): TNyxText;
begin
  Result := AID + TNyxText('-') + CActions[AAction];
end;

function TNyxResourceLabelsEditorState.ToData: TNyxDataValue;
var
  LLabels: TNyxResourceLabels;
  LSelection: TNyxDataValue;
begin
  LLabels := Labels.Copy;
  LSelection := NyxNull;

  if Selection.Defined then
  begin

    if not LLabels.Contains(Selection) then
    begin
      raise ENyxResource.Create('Tag selection must belong to its proposal');
    end;
    LSelection := NyxData(NyxResourceLabel(Selection.Name).Name);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('labels', LLabels.ToData), NyxField('input', NyxData(Input)),
    NyxField('selection', LSelection)]);
end;

class function TNyxResourceLabelsEditorState.FromData(
  const AData: TNyxDataValue): TNyxResourceLabelsEditorState;
var
  LState: TNyxResourceLabelsEditorState;
  LSelection: TNyxDataValue;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 4) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxResource.Create('Tag editor state requires its exact version and fields');
  end;
  LState := Default(TNyxResourceLabelsEditorState);
  LState.Labels := TNyxResourceLabels.FromData(AData.Field('labels'));
  LState.Input := AData.Field('input').AsText;
  LSelection := AData.Field('selection');

  if LSelection.Kind <> ndNull then
  begin
    LState.Selection := NyxResourceLabel(LSelection.AsText);

    if not LState.Labels.Contains(LState.Selection) then
    begin
      raise ENyxResource.Create('Tag selection must belong to its proposal');
    end;
  end;
  Result := LState;
end;

function Complete(AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceLabelsEditorField;
  LAction: TNyxResourceLabelsEditorAction;
begin
  Result := False;

  if (AEditor = nil) or (AEditor.Props.IndexOfName(CLabels) < 0) then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceLabelsEditorField) to High(TNyxResourceLabelsEditorField) do
  begin

    if AEditor.Find(NyxResourceLabelsEditorFieldID(AEditor.ID, LField)) = nil then
    begin
      Exit;
    end;
  end;
  for LAction := Low(TNyxResourceLabelsEditorAction) to High(TNyxResourceLabelsEditorAction) do
  begin

    if AEditor.Find(NyxResourceLabelsEditorActionID(AEditor.ID, LAction)) = nil then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function ReadNyxResourceLabelsEditor(AEditor: TNyxNode): TNyxResourceLabelsEditorState;
var
  LSelection: TNyxText;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxResource.Create('Tags require a complete editor');
  end;
  Result := Default(TNyxResourceLabelsEditorState);
  Result.Labels := TNyxResourceLabels.FromData(TNyxDataValue.ParseJSON(AEditor.Prop(CLabels)));
  Result.Input := AEditor.Find(NyxResourceLabelsEditorFieldID(AEditor.ID, rlefInput)).Prop('value');
  LSelection := AEditor.Find(NyxResourceLabelsEditorFieldID(AEditor.ID, rlefSelection)).Prop('value');

  if LSelection <> '' then
  begin
    Result.Selection := NyxResourceLabel(LSelection);

    if not Result.Labels.Contains(Result.Selection) then
    begin
      raise ENyxResource.Create('Tag selection must belong to its proposal');
    end;
  end;
end;

procedure RestoreNyxResourceLabelsEditor(AEditor: TNyxNode;
  const AState: TNyxResourceLabelsEditorState);
var
  LState: TNyxResourceLabelsEditorState;
  LItems: TNyxText;
  LIndex: Integer;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxResource.Create('Tags require a complete editor');
  end;
  LState := TNyxResourceLabelsEditorState.FromData(AState.ToData);
  LItems := '';
  for LIndex := 0 to LState.Labels.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LItems := LItems + TNyxText(#10);
    end;
    { Label admission excludes line breaks, so the ordinary Select's item
      boundary preserves commas, spaces, punctuation and supplementary text. }
    LItems := LItems + LState.Labels.Item(LIndex).Name;
  end;
  AEditor.SetProp(CLabels, LState.Labels.ToData.ToJSON);
  AEditor.Find(NyxResourceLabelsEditorFieldID(AEditor.ID, rlefInput)).Configure
    .Value(LState.Input).Done;
  AEditor.Find(NyxResourceLabelsEditorFieldID(AEditor.ID, rlefSelection)).Configure
    .Items(LItems).Value(LState.Selection.Name).Enabled(LState.Labels.Count > 0).Done;
  AEditor.Find(NyxResourceLabelsEditorActionID(AEditor.ID, rleaRemove)).Configure
    .Enabled(LState.Selection.Defined).Done;
end;

function NewNyxResourceLabelsEditor(const AID: TNyxText;
  const ALabels: TNyxResourceLabels;
  APurpose: TNyxResourceLabelsEditorPurpose): INyxCard;
var
  LState: TNyxResourceLabelsEditorState;
  LTitle: TNyxText;
  LHelp: TNyxText;
begin

  if (Ord(APurpose) < Ord(Low(TNyxResourceLabelsEditorPurpose))) or
    (Ord(APurpose) > Ord(High(TNyxResourceLabelsEditorPurpose))) then
  begin
    raise ENyxResource.Create('Unsupported tag editor purpose');
  end;
  LState := Default(TNyxResourceLabelsEditorState);
  LState.Labels := ALabels.Copy;
  LTitle := 'Tags';
  LHelp := 'Describe how this resource is used. Tags are saved with the project.';

  if APurpose = rlepFilter then
  begin
    LTitle := 'Filter by tags';
    LHelp := 'Add exact tags to narrow this resource list.';
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Layout(TNyxLayoutPolicy.Column).Gap(8).Done;
  Result.Node.SetProp(CLabels, LState.Labels.ToData.ToJSON);
  Result.Add(NewNyxLabel(AID + TNyxText('-title')).WithText(LTitle));
  Result.Add(NewNyxLabel(AID + TNyxText('-help')).WithText(LHelp));
  Result.Add(NewNyxInput(NyxResourceLabelsEditorFieldID(AID, rlefInput)).Configure
    .Text('New tag').AccessibleName('New tag').Placeholder('For example, onboarding').Done);
  Result.Add(NewNyxButton(NyxResourceLabelsEditorActionID(AID, rleaAdd)).WithText('+ Add tag'));
  Result.Add(NewNyxSelect(NyxResourceLabelsEditorFieldID(AID, rlefSelection)).Configure
    .Text('Selected tag').AccessibleName('Selected tag').Done);
  Result.Add(NewNyxButton(NyxResourceLabelsEditorActionID(AID, rleaRemove)).WithText('Remove tag'));
  RestoreNyxResourceLabelsEditor(Result.Node, LState);
end;

function NyxResourceLabelsEditorInput(ANode, AEditor: TNyxNode): Boolean;
var
  LField: TNyxResourceLabelsEditorField;
begin
  Result := False;

  if (ANode = nil) or not Complete(AEditor) then
  begin
    Exit;
  end;
  for LField := Low(TNyxResourceLabelsEditorField) to High(TNyxResourceLabelsEditorField) do
  begin

    if AEditor.Find(NyxResourceLabelsEditorFieldID(AEditor.ID, LField)) = ANode then
    begin
      Exit(True);
    end;
  end;
end;

function HandleNyxResourceLabelsEditorAction(AButton, AEditor: TNyxNode): Boolean;
var
  LState: TNyxResourceLabelsEditorState;
  LLabel: TNyxResourceLabelRef;
  LAction: TNyxResourceLabelsEditorAction;
begin
  Result := False;

  if (AButton = nil) or not Complete(AEditor) then
  begin
    Exit;
  end;
  for LAction := Low(TNyxResourceLabelsEditorAction) to High(TNyxResourceLabelsEditorAction) do
  begin

    if AEditor.Find(NyxResourceLabelsEditorActionID(AEditor.ID, LAction)) = AButton then
    begin
      LState := ReadNyxResourceLabelsEditor(AEditor);
      case LAction of
        rleaAdd:
          begin
            LLabel := NyxResourceLabel(LState.Input);
            LState.Labels := LState.Labels.Add(LLabel);
            LState.Selection := LLabel;
            LState.Input := '';
          end;
        rleaRemove:
          begin

            if not LState.Selection.Defined then
            begin
              raise ENyxResource.Create('Choose a tag before removing it');
            end;
            LState.Labels := LState.Labels.Remove(LState.Selection);
            LState.Selection := Default(TNyxResourceLabelRef);
          end;
      end;
      RestoreNyxResourceLabelsEditor(AEditor, LState);
      Exit(True);
    end;
  end;
end;

end.
