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
  nyx.studio.builds, nyx.studio.editorbuild;

{ Exact execution identity is explicit editor metadata, never a document root
  or a list index. A mounted old row still refers to the same immutable job. }
function NyxStudioCancelBuildKey: TNyxExtensionRef;
{ Owned reusable ordinary Nyx composition. Borrows bounded metadata only while
  composing; nodes own descendants and retain no bridge/session references.
  ACanCancel additionally gates the operator's acknowledged project frame.
  No compiler, target selection or transport is required to render this panel. }
function BuildNyxStudioBuildJobs(const AJobs: TNyxDataValue; ACanCancel: Boolean): TNyxNode;

implementation

function NyxStudioCancelBuildKey: TNyxExtensionRef;
begin
  Result := NyxExtension('studio.cancel-build');
end;

function BuildNyxStudioBuildJobs(const AJobs: TNyxDataValue; ACanCancel: Boolean): TNyxNode;
const
  CSeparator: TNyxText = ' · ';
  CRevision: TNyxText = ' · revision ';
  CCompiling: TNyxText = ' compiling · ';
  CQueued: TNyxText = ' queued · ';
  CStates: array[TNyxBuildJobState] of TNyxText =
    ('Queued', 'Compiling', 'Cancelling', 'Complete', 'Failed', 'Cancelled');
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LList: TNyxNode;
  LRow: TNyxNode;
  LButton: TNyxNode;
  LID: TNyxText;
  LCaption: TNyxText;
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
        .Text('No active builds for this project.').Done);
    end
    else
    begin
      LList := TNyxNode.Create(nkScroll, 'studio-builds-list');
      LList.Configure.Layout(nlColumn).Gap(8).Height(180)
        .WhenViewport(TNyxViewportWidth.Below(640)).Height(130).Done;
      Result.Add(LList);
      for LIndex := 0 to LItems.Count - 1 do
      begin
        LItem := LItems.Item(LIndex);
        LID := 'studio-build-' + IntToStr(LIndex);
        LCaption := CStates[ParseNyxBuildJobState(LItem.Field('state').AsText)] + CSeparator +
          LItem.Field('scope').AsText + CSeparator + LItem.Field('target').AsText;
        LRow := TNyxNode.Create(nkColumn, LID);
        LRow.Configure.Gap(4).Done;
        LList.Add(LRow);
        LRow.Add(TNyxNode.Create(nkLabel, LID + '-title').Configure.Text(LCaption).Done);
        LRow.Add(TNyxNode.Create(nkLabel, LID + '-actor').Configure
          .Text(LItem.Field('actor').AsText + CRevision +
            IntToStr(LItem.Field('revision').AsInteger)).Done);

        if not LItem.Field('currentSource').AsBoolean or
          not LItem.Field('currentOutput').AsBoolean then
        begin
          LRow.Add(TNyxNode.Create(nkLabel, LID + '-earlier').Configure
            .Text('Earlier source or output settings').Done);
        end;
        LButton := TNyxNode.Create(nkButton, LID + '-cancel');
        LButton.Configure.Text('Cancel build').Enabled(ACanCancel and
          LItem.Field('canCancel').AsBoolean)
          .Hint('Retire this compiler job; keep accepted Pascal, history and preview.').Done;
        LButton.Extensions.SetValue(NyxStudioCancelBuildKey,
          NyxData(NyxBuildJob(LItem.Field('job').AsText).ID));
        LRow.Add(LButton);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
