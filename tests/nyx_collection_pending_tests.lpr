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



program nyx_collection_pending_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.collections, nyx.collections.view.types,
  nyx.studio.projects, nyx.studio.session, nyx.studio.collections,
  nyx.studio.collectionintent, nyx.test.collection.queue
  {$ifdef PAS2JS}, Web{$endif};

var
  LSession: TNyxStudioSession;
  LPending: TNyxStudioPendingDesign;
  LProjection: TNyxNode;
  LRoot: TNyxNode;
  LBefore: TNyxText;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Pending collection presentation: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure PaintProposal;
begin
  LPending.Collections[0].Intent.Validate;
  LProjection := LSession.SelectedProjection;
  LRoot := TNyxNode.Create(nkColumn, 'pending-inspector');
  try
    AddNyxCollectionBindingPanel(LRoot, LSession, LProjection, LPending);
    Check((LRoot.Find('collection-column-1-title').Prop('value') = 'Task') and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'Unadmitted pending metadata cannot throw or replace the accepted typed view');
  finally
    LRoot.Free;
    LProjection.Free;
  end;
end;

begin
  LSession := nil;
  try
    LSession := TNyxStudioSession.Create(CreateNyxCollectionQueueSeed);
    LSession.Select('tasks-table');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LPending := Default(TNyxStudioPendingDesign);
    SetLength(LPending.Collections, 1);
    LPending.Collections[0].Owner := 'tasks-table';
    LPending.Collections[0].Intent.Action := scaTitle;
    LPending.Collections[0].Intent.Key := NyxCollection('tasks');
    LPending.Collections[0].Intent.Field :=
      TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField('caption'));
    LPending.Collections[0].Intent.Projection := cpTable;
    LPending.Collections[0].Intent.Value := 'Unadmitted title';
    PaintProposal;
    LPending.Collections[0].Intent.Field :=
      TNyxStudioCollectionFieldRef.Text(NyxTextField('absent'));
    PaintProposal;
    LPending.Collections[0].Intent := Default(TNyxStudioCollectionIntent);
    LPending.Collections[0].Intent.Action := scaBind;
    LPending.Collections[0].Intent.Key := NyxCollection('absent');
    LPending.Collections[0].Intent.Projection := cpTable;
    PaintProposal;
    LPending.Collections[0].Owner := 'another-owner';
    PaintProposal;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-pending', 'passed');
    document.body.setAttribute('data-collection-checks', IntToStr(LChecks));
    {$else}
    WriteLn('PASS ', LChecks, ' pending collection refusal/presentation checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-pending', 'failed');
      document.body.setAttribute('data-collection-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  LSession.Free;
end.
