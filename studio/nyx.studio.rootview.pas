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
unit nyx.studio.rootview;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.types, nyx.studio.session,
  nyx.studio.rootedits;

const
  NyxStudioReviewRootID = 'action-review-root';
  NyxStudioRemoveRootID = 'action-remove-root';
  NyxStudioCancelRootID = 'action-cancel-root';

type
  TNyxRootViewEffect = (nreNone, nreReview, nreCancel, nreRemoved);

{ Public Nyx controls present copied review metadata. No borrowed project nodes
  or widget references survive composition; the returned card is raw-owned. }
function BuildNyxRootRemovalCard(const AInspection: TNyxDataValue): TNyxNode;
{ Both controllers may route the same typed command. Review chooses the exact
  active root partition; confirm rechecks its immutable pair before one Undo
  publication. Refusal retains the review and all current user work. }
function RouteNyxRootRemoval(ASession: TNyxStudioSession; const AID: TNyxText;
  ATrigger: TNyxTrigger; var AReview: INyxRootRemoval): TNyxRootViewEffect;

implementation

uses
  SysUtils, nyx.root.types;

function BuildNyxRootRemovalCard(const AInspection: TNyxDataValue): TNyxNode;
var
  LRoots: TNyxDataValue;
  LRoot: TNyxDataValue;
  LIndex: Integer;
begin
  Result := TNyxNode.Create(nkCard, 'studio-root-removal');
  try
    Result.Configure.Surface(True).Padding(16).Gap(10).Done;
    Result.Add(TNyxNode.Create(nkHeading, 'root-removal-title').Configure
      .Text('Remove view?').Done);
    LRoots := AInspection.Field('roots');
    for LIndex := 0 to LRoots.Count - 1 do
    begin
      LRoot := LRoots.Item(LIndex);
      Result.Add(TNyxNode.Create(nkLabel, 'root-removal-name-' + IntToStr(LIndex)).Configure
        .Text(LRoot.Field('id').AsText).Done);
    end;
    Result.Add(TNyxNode.Create(nkLabel, 'root-removal-counts').Configure
      .Text(IntToStr(AInspection.Field('nodes').AsInteger) + ' components / ' +
        IntToStr(AInspection.Field('registrations').AsInteger) + ' callback registrations / ' +
        IntToStr(AInspection.Field('retainedReferences').AsInteger) + ' retained references').Done);
    Result.Add(TNyxNode.Create(nkLabel, 'root-removal-warning').Configure
      .Text(AInspection.Field('warning').AsText).Done);
    Result.Add(TNyxNode.Create(nkRow, 'root-removal-actions')
      .Add(TNyxNode.Create(nkButton, NyxStudioCancelRootID).Configure.Text('Keep view').Done)
      .Add(TNyxNode.Create(nkButton, NyxStudioRemoveRootID).Configure.Text('Remove view')
        .Enabled(AInspection.Field('ready').AsBoolean).Variant(nvDanger).Done));
  except
    Result.Free;
    raise;
  end;
end;

function RouteNyxRootRemoval(ASession: TNyxStudioSession; const AID: TNyxText;
  ATrigger: TNyxTrigger; var AReview: INyxRootRemoval): TNyxRootViewEffect;
var
  LRoot: TNyxRootRef;
begin
  Result := nreNone;

  if ATrigger <> ntClick then
  begin
    Exit;
  end;

  if AID = NyxStudioReviewRootID then
  begin
    LRoot := NyxPageRoot(ASession.ActiveViewID);

    if ASession.Document.FindRoot(LRoot) = nil then
    begin
      LRoot := NyxReusableRoot(ASession.ActiveViewID);
    end;
    AReview := ReviewNyxRootRemoval(ASession.ProjectSnapshot, [LRoot]);
    Result := nreReview;
  end
  else if AID = NyxStudioCancelRootID then
  begin
    AReview := nil;
    Result := nreCancel;
  end
  else if AID = NyxStudioRemoveRootID then
  begin
    ASession.RemoveRoots(AReview);
    AReview := nil;
    Result := nreRemoved;
  end;
end;

end.
