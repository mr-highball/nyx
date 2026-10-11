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

unit nyx.studio.buildview;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.types, nyx.responsive,
  nyx.studio.builds, nyx.studio.editorbuild, nyx.studio.builddiagnostics;

type
  { Closed presentation choices for a compiler job's semantic purpose. JSON
    names are admitted only at ReadNyxStudioBuildActivity below; composing the
    ordinary editor uses these Pascal choices on both adapters. }
  TNyxStudioBuildActivityKind = (sbakOutput, sbakDesign, sbakCallbacks, sbakHandlers,
    sbakProjectImport, sbakPascal);
  TNyxStudioBuildPublication = (sbpNone, sbpPending, sbpCommitted, sbpRefused, sbpReviewed);

  { Owned bounded presentation, with no source, proposed locations, transport
    authority or session reference. Compiler completion and editor publication
    are independent: a successful constructor may still await publication or
    have its changes refused. Only committed has a published revision. }
  TNyxStudioBuildActivity = record
  private
    FKind: TNyxStudioBuildActivityKind;
    FState: TNyxBuildJobState;
    FPublication: TNyxStudioBuildPublication;
    FPublishedRevision: Integer;
    FCaption: TNyxText;
    FDetail: TNyxText;
  public
    property Kind: TNyxStudioBuildActivityKind read FKind;
    property State: TNyxBuildJobState read FState;
    property Publication: TNyxStudioBuildPublication read FPublication;
    property PublishedRevision: Integer read FPublishedRevision;
    property Caption: TNyxText read FCaption;
    property Detail: TNyxText read FDetail;
  end;

{ Decode only the bounded job fields needed for the editor. Older application
  rows need no purpose/publication fields. Unknown semantic choices and invalid
  committed state/revision refuse rather than presenting compiler success as
  applied work. Proposed source locations never grant UI navigation here. }
function ReadNyxStudioBuildActivity(const AItem: TNyxDataValue): TNyxStudioBuildActivity;

{ Exact execution identity is explicit editor metadata, never a document root
  or a list index. A mounted old row still refers to the same immutable job. }
function NyxStudioCancelBuildKey: TNyxExtensionRef;
{ Owned reusable ordinary Nyx composition. Borrows bounded metadata only while
  composing; nodes own descendants and retain no bridge/session references.
  ACanCancel additionally gates the operator's acknowledged project frame.
  No compiler, target selection or transport is required to render this panel. }
function BuildNyxStudioBuildJobs(const AJobs: TNyxDataValue;
  ACanCancel: Boolean): TNyxNode; overload;
{ Inspection availability is independent from synchronized mutation/cancellation:
  an unfinished draft may still inspect read-only compiler messages. }
function BuildNyxStudioBuildJobs(const AJobs: TNyxDataValue;
  ACanCancel, ACanInspect: Boolean): TNyxNode; overload;

implementation

uses nyx.studio.agents;

function ReadNyxStudioBuildActivity(const AItem: TNyxDataValue): TNyxStudioBuildActivity;
const
  CSeparator: TNyxText = ' · ';
  CUndoDetail: TNyxText = ' · one Undo restores the design and Pascal.';
  CStates: array[TNyxBuildJobState] of TNyxText =
    ('Queued', 'Compiling', 'Cancelling', 'Complete', 'Failed', 'Cancelled');
  CKinds: array[TNyxStudioBuildActivityKind] of TNyxText =
    ('Build', 'Design changes', 'Callbacks', 'Callback implementations', 'Project import',
      'Pascal edits');
var
  LName: TNyxText;
  LStage: TNyxText;
  LPublication: TNyxDataValue;
begin
  Result := Default(TNyxStudioBuildActivity);
  Result.FState := ParseNyxBuildJobState(AItem.Field('state').AsText);
  LStage := CStates[Result.FState];

  if not NyxAgentHas(AItem, 'purpose') then
  begin
    Result.FCaption := LStage + CSeparator + AItem.Field('scope').AsText +
      CSeparator + AItem.Field('target').AsText;
    Exit;
  end;

  if AItem.Field('purpose').AsText <> 'transaction' then
  begin
    raise ENyxModel.Create('Unknown semantic compiler job purpose');
  end;
  Result.FKind := sbakDesign;

  if NyxAgentHas(AItem, 'operation') then
  begin
    LName := AItem.Field('operation').AsText;

    if LName = 'callbacks' then
    begin
      Result.FKind := sbakCallbacks;
    end
    else if LName = 'handlers' then
    begin
      Result.FKind := sbakHandlers;
    end
    else if LName = 'project-import-review' then
    begin
      Result.FKind := sbakProjectImport;
    end
    else if LName = 'source-edit' then
    begin
      { The bounded receipt describes general source work. It grants no access
        to proposed text and shares the exact publication/cancellation contract. }
      Result.FKind := sbakPascal;
    end
    else
    begin
      raise ENyxModel.Create('Unknown semantic compiler job operation');
    end;
  end;
  LPublication := AItem.Field('publication');
  LName := LPublication.Field('state').AsText;

  if LName = 'pending' then
  begin
    Result.FPublication := sbpPending;
    Result.FDetail := 'Accepted design and Pascal stay active until these changes are applied.';

    if Result.FState = bjsSucceeded then
    begin
      LStage := 'Awaiting publication';

      if Result.FKind = sbakProjectImport then
      begin
        LStage := 'Awaiting review';
      end;
    end;
  end
  else if LName = 'committed' then
  begin
    Result.FPublication := sbpCommitted;
    Result.FPublishedRevision := LPublication.Field('revision').AsInteger;

    if (Result.FState <> bjsSucceeded) or (Result.FPublishedRevision < 1) then
    begin
      raise ENyxModel.Create('Applied compiler changes require successful construction and an exact revision');
    end;
    LStage := 'Applied';
    Result.FDetail := 'Applied at revision ' + TNyxText(IntToStr(Result.FPublishedRevision)) +
      CUndoDetail;
  end
  else if LName = 'refused' then
  begin
    Result.FPublication := sbpRefused;
    Result.FDetail := 'Changes were not applied. Accepted design and Pascal are retained.';

    if Result.FState = bjsSucceeded then
    begin
      LStage := 'Not applied';
    end;
  end
  else if (LName = 'reviewed') and (Result.FKind = sbakProjectImport) then
  begin

    if (Result.FState <> bjsSucceeded) or
      (LPublication.Field('revision').AsInteger < 1) then
    begin
      raise ENyxModel.Create('Project review requires successful construction at its retained revision');
    end;
    Result.FPublication := sbpReviewed;
    LStage := 'Review ready';
    Result.FDetail := 'Current project is retained. Apply its exact reviewed import separately.';
  end
  else
  begin
    raise ENyxModel.Create('Unknown compiler publication state');
  end;
  Result.FCaption := LStage + CSeparator + CKinds[Result.FKind];
end;

function NyxStudioCancelBuildKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.cancel-build');
end;

function BuildNyxStudioBuildJobs(const AJobs: TNyxDataValue; ACanCancel: Boolean): TNyxNode;
begin
  Result := BuildNyxStudioBuildJobs(AJobs, ACanCancel, ACanCancel);
end;

function BuildNyxStudioBuildJobs(const AJobs: TNyxDataValue;
  ACanCancel, ACanInspect: Boolean): TNyxNode;
const
  CRevision: TNyxText = ' · revision ';
  CCompiling: TNyxText = ' compiling · ';
  CQueued: TNyxText = ' queued · ';
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LList: TNyxNode;
  LRow: TNyxNode;
  LButton: TNyxNode;
  LID: TNyxText;
  LCaption: TNyxText;
  LActivity: TNyxStudioBuildActivity;
  LCancelCaption: TNyxText;
begin
  LItems := AJobs.Field('items');

  if LItems.Count > 16 then
  begin
    raise ENyxModel.Create('Build activity requires bounded job metadata');
  end;
  Result := TNyxNode.Create(nkCard, 'studio-builds');
  try
    Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Surface(True).Done;
    Result.Add(TNyxNode.Create(nkHeading, 'studio-builds-title').Configure.Text('Builds').Done);
    Result.Add(TNyxNode.Create(nkLabel, 'studio-builds-summary').Configure.Text(
      TNyxText(IntToStr(AJobs.Field('running').AsInteger)) + CCompiling +
      TNyxText(IntToStr(AJobs.Field('queued').AsInteger)) + CQueued +
      IntToStr(AJobs.Field('cancelling').AsInteger) + ' cancelling').Done);

    if LItems.Count = 0 then
    begin
      Result.Add(TNyxNode.Create(nkLabel, 'studio-builds-empty').Configure
        .Text('No compiler activity for this project.').Done);
    end
    else
    begin
      LList := TNyxNode.Create(nkScroll, 'studio-builds-list');
      LList.Configure.Layout(nlColumn).Gap(8).Height(180)
        .WhenViewport(TNyxViewportWidth.Below(640)).Height(130).Done;
      Result.Add(LList);
      { Service metadata preserves its stable query order. Present the newest
        retained activity first while keeping each exact original row identity. }
      for LIndex := LItems.Count - 1 downto 0 do
      begin
        LItem := LItems.Item(LIndex);
        LID := 'studio-build-' + IntToStr(LIndex);
        LActivity := ReadNyxStudioBuildActivity(LItem);
        LCaption := LActivity.Caption;
        LRow := TNyxNode.Create(nkColumn, LID);
        LRow.Configure.Gap(4).Done;
        LList.Add(LRow);
        LRow.Add(TNyxNode.Create(nkLabel, LID + '-title').Configure.Text(LCaption).Done);
        LRow.Add(TNyxNode.Create(nkLabel, LID + '-actor').Configure
          .Text(LItem.Field('actor').AsText + CRevision +
            IntToStr(LItem.Field('revision').AsInteger)).Done);

        if LActivity.Detail <> '' then
        begin
          LRow.Add(TNyxNode.Create(nkLabel, LID + '-publication').Configure
            .Text(LActivity.Detail).Done);
        end;

        { An uncommitted constructor intentionally does not represent active
          source. That is publication progress, not an earlier-source warning. }

        if ((LActivity.Kind = sbakOutput) or (LActivity.Publication = sbpCommitted)) and
          (not LItem.Field('currentSource').AsBoolean or
          not LItem.Field('currentOutput').AsBoolean) then
        begin
          LRow.Add(TNyxNode.Create(nkLabel, LID + '-earlier').Configure
            .Text('Earlier source or output settings').Done);
        end;
        LCancelCaption := 'Cancel build';

        if LActivity.Kind <> sbakOutput then
        begin
          LCancelCaption := 'Cancel changes';
        end;
        LButton := TNyxNode.Create(nkButton, LID + '-cancel');
        LButton.Configure.Text(LCancelCaption).Enabled(ACanCancel and
          LItem.Field('canCancel').AsBoolean)
          .Hint('Retire this compiler job; keep accepted Pascal, history and preview.').Done;
        LButton.Extensions.SetValue(NyxStudioCancelBuildKey,
          NyxData(NyxBuildJob(LItem.Field('job').AsText).ID));
        LRow.Add(LButton);
        LButton := TNyxNode.Create(nkButton, LID + '-inspect');
        LButton.Configure.Text('Inspect diagnostics').Enabled(ACanInspect)
          .Hint('Read this job''s messages without changing accepted code or a draft.').Done;
        LButton.Extensions.SetValue(NyxStudioInspectBuildKey,
          NyxData(NyxBuildJob(LItem.Field('job').AsText).ID));
        LButton.Extensions.SetValue(NyxStudioInspectOffsetKey, NyxData(0));
        LRow.Add(LButton);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
