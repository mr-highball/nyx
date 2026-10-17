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

unit nyx.pascal.files.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.controls, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.filechanges;

type
  { Closed form actions. Confirmation is presentation only: the embedding
    controller must retain/revalidate its exact owning removal ticket. }
  TNyxPascalFilesEditorAction = (npfeaNone, npfeaCreate, npfeaRemove,
    npfeaConfirmRemove, npfeaCancelRemove);

  { Copied unfinished form values, never project data or execution authority.
    Invalid names stay editable; only Creation validates a complete typed name.
    No form, controller, document, target or compiler is retained. }
  TNyxPascalFilesEditorState = record
  private
    FName: TNyxText;
    FKind: TNyxPascalInputKind;
    FMessage: TNyxText;
  public
    { Capture current visible fields without rebuilding them during typing. }
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Creation: TNyxPascalFileChange;
    { Clear the submitted name only while it still describes this exact result;
      newer form input must survive a delayed compiler completion. }
    function AcceptCreation(const AChange: TNyxPascalFileChange): Boolean;
    { Local validation feedback is presentation only, independent of a queued
      compiler's authoritative status. New fields/actions clear their old scope. }
    procedure Report(const AMessage: TNyxText);
    property Name: TNyxText read FName;
    property Kind: TNyxPascalInputKind read FKind;
  end;

{ Reusable Nyx-built source inventory form. Main cannot be removed. A non-main
  review names the exact confirmed target independently of current selection.
  The card owns its controls; it performs no file/compiler/history operation. }
function NewNyxPascalFilesEditor(const AID: TNyxText;
  const AState: TNyxPascalFilesEditorState;
  const ASelected, ARemoval: TNyxPascalSourceRef;
  const AStatus: TNyxText = ''): INyxCard;
function NyxPascalFilesEditorAction(const AID: TNyxText; ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxPascalFilesEditorAction;

implementation

uses SysUtils, nyx.pascal.names;

const
  CUnitCaption = 'Pascal unit (.pas)';
  CIncludeCaption = 'Include fragment (.inc)';

procedure TNyxPascalFilesEditorState.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LKind: TNyxText;
begin

  if ARoot = nil then
  begin
    raise ENyxPascalProject.Create('Source-file form is no longer mounted');
  end;
  LForm := ARoot.Find(AID);

  if LForm = nil then
  begin
    raise ENyxPascalProject.Create('Source-file form is no longer mounted');
  end;
  LKind := LForm.Find(AID + '-kind').Prop('value');

  if LKind = CUnitCaption then
  begin
    FKind := npiUnit;
  end
  else if LKind = CIncludeCaption then
  begin
    FKind := npiInclude;
  end
  else
  begin
    raise ENyxPascalProject.Create('Choose a unit or include fragment');
  end;
  FName := LForm.Find(AID + '-name').Prop('value');
  FMessage := '';
end;

function TNyxPascalFilesEditorState.Creation: TNyxPascalFileChange;
begin
  case Ord(FKind) of
    Ord(npiUnit):
      begin
        Result := TNyxPascalFileChange.CreateUnit(NyxPascalUnit(FName));
      end;
    Ord(npiInclude):
      begin
        Result := TNyxPascalFileChange.CreateInclude(NyxPascalInclude(FName));
      end;
    else
      begin
        raise ENyxPascalProject.Create('Unknown source-file kind');
      end;
  end;
end;

function TNyxPascalFilesEditorState.AcceptCreation(
  const AChange: TNyxPascalFileChange): Boolean;
var
  LName: TNyxText;
begin
  Result := AChange.IsDefined and (AChange.Kind = npfcCreate);

  if not Result then
  begin
    Exit;
  end;
  LName := AChange.FileReference.Name;

  if AChange.FileReference.Role = npsUnit then
  begin
    LName := Copy(LName, 1, Length(LName) - 4);
  end;
  Result := (FName = LName) and
    ((FKind = npiUnit) = (AChange.FileReference.Role = npsUnit));

  if Result then
  begin
    FName := '';
    FMessage := '';
  end;
end;

procedure TNyxPascalFilesEditorState.Report(const AMessage: TNyxText);
begin
  FMessage := AMessage;
end;

function NewNyxPascalFilesEditor(const AID: TNyxText;
  const AState: TNyxPascalFilesEditorState;
  const ASelected, ARemoval: TNyxPascalSourceRef;
  const AStatus: TNyxText): INyxCard;
var
  LKind: TNyxText;
  LReview: Boolean;
  LMessage: TNyxText;
begin
  ASelected.Validate;
  ARemoval.Validate;
  LReview := ARemoval.Role <> npsMain;
  LKind := CUnitCaption;

  if AState.Kind = npiInclude then
  begin
    LKind := CIncludeCaption;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Flex(1).Done;
  LMessage := AState.FMessage;

  if LMessage = '' then
  begin
    LMessage := AStatus;
  end;
  Result.Add(NewNyxLabel(AID + '-title').WithText('Source files'));
  Result.Add(NewNyxLabel(AID + '-message').Configure.Text(LMessage)
    .Visible(LMessage <> '').Done);
  Result.Add(NewNyxSelect(AID + '-kind').Configure.Text('New file type')
    .Items(CUnitCaption + #10 + CIncludeCaption).Value(LKind).Enabled(not LReview).Done);
  Result.Add(NewNyxInput(AID + '-name').Configure.Text('New file name')
    .Placeholder('project.utilities or rules.inc').Value(AState.Name)
    .Enabled(not LReview).Hint('Unit names omit .pas; include names end in .inc. No paths.').Done);
  Result.Add(NewNyxButton(AID + '-create').Configure.Text('Create file')
    .Enabled(not LReview).Variant(nvPrimary).Done);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'New files stay with the project. Add uses or include references where you need them.'));
  Result.Add(NewNyxButton(AID + '-remove').Configure.Text('Remove selected file')
    .Enabled((ASelected.Role <> npsMain) and not LReview).Done);

  if LReview then
  begin
    Result.Add(NewNyxLabel(AID + '-warning').WithText('Remove ' + ARemoval.Name +
      ' and its unfinished draft? Undo restores both. Dependencies must be fixed first.'));
    Result.Add(NewNyxButton(AID + '-remove-confirm').WithText('Remove ' + ARemoval.Name));
    Result.Add(NewNyxButton(AID + '-remove-cancel').WithText('Keep file'));
  end;
end;

function NyxPascalFilesEditorAction(const AID: TNyxText; ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxPascalFilesEditorAction;
begin
  Result := npfeaNone;

  if (ANode = nil) or (ATrigger <> ntClick) then
  begin
    Exit;
  end;

  if ANode.ID = AID + '-create' then
  begin
    Result := npfeaCreate;
  end
  else if ANode.ID = AID + '-remove' then
  begin
    Result := npfeaRemove;
  end
  else if ANode.ID = AID + '-remove-confirm' then
  begin
    Result := npfeaConfirmRemove;
  end
  else if ANode.ID = AID + '-remove-cancel' then
  begin
    Result := npfeaCancelRemove;
  end;
end;

end.
