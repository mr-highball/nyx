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
program nyx_collection_inspection_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.state, nyx.model, nyx.controls,
  nyx.types, nyx.codec, nyx.codegen, nyx.source, nyx.collections,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.inspection,
  nyx.collections.paging, nyx.collections.query
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Inspection authoring: ' + AReason);
  end;
end;

procedure Run;
const
  CExact: TNyxText = 'A careful reader 🌙' + #0 + #13#10 + 'still here';
var
  LPolicy: TNyxCollectionInspectionPolicy;
  LReadPolicy: TNyxCollectionInspectionPolicy;
  LBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LReadSpec: TNyxCollectionViewSpec;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LInspection: INyxCollectionInspection;
  LBefore: INyxCollectionSnapshot;
  LKey: TNyxCollectionRef;
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPage: INyxPage;
  LList: INyxList;
  LSource: TNyxText;
  LSaved: TNyxText;
  LWire: TNyxText;
  LRefused: Boolean;
  {$ifndef PAS2JS}
  LBytes: UTF8String;
  LStream: TFileStream;
  {$endif}
begin
  LPolicy := NyxInspectionPolicy.PausedBytes(High(Integer)).RemovedIdentities(0).Follow(ncfLastVisible);
  LReadPolicy := TNyxCollectionInspectionPolicy.FromData(LPolicy.ToData);
  Check((LReadPolicy.MaximumPausedBytes = High(Integer)) and (LReadPolicy.IdentityLimit = 0) and
    (LReadPolicy.DefaultFollow = ncfLastVisible), 'exact maximum/zero/enum values survive policy persistence');
  LReadPolicy := LPolicy;
  LRefused := False;
  try
    LReadPolicy := TNyxCollectionInspectionPolicy.FromData(TNyxDataValue.ParseJSON(
      '{"version":1,"follow":"last-visible","maximumPausedBytes":-1,"removedIdentityLimit":0}'));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LReadPolicy.ToData.ToJSON = LPolicy.ToData.ToJSON),
    'rejected policy decoding cannot replace the caller''s accepted value');
  LKey := NyxCollection('readers');
  LBase := NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name')
    .Paged(NyxCollectionPaging.PageSize(2)).Query(NyxCollectionQuery.OrderBy(NyxTextField('name')));
  LWire := LBase.ToData.ToJSON;
  LSpec := LBase.Inspection(LPolicy);
  Check((LSpec.ToData.Field('version').AsInteger = 13) and not LBase.InspectionPolicy.Defined,
    'inspection is an explicit detached authored choice');
  LReadSpec := TNyxCollectionViewSpec.FromData(LSpec.ToData);
  Check(LReadSpec.ToData.ToJSON = LSpec.ToData.ToJSON, 'descriptor thirteen preserves complete nullable policy/column state');
  Check(LReadSpec.UseDefaultInspection.ToData.ToJSON = LWire,
    'removing only inspection restores exact earlier descriptor bytes');
  LReadSpec := LSpec;
  LRefused := False;
  try
    LReadSpec := TNyxCollectionViewSpec.FromData(TNyxDataValue.ParseJSON(
      StringReplace(LSpec.ToData.ToJSON, '"inspection"', '"unexpected"', [])));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LReadSpec.ToData.ToJSON = LSpec.ToData.ToJSON),
    'unknown descriptor fields refuse without damaging the accepted binding');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), ''), [
    NyxCollectionItem(NyxItem(LKey, 'reader')).WithValue(NyxTextField('name'), CExact),
    NyxCollectionItem(NyxItem(LKey, 'tail')).WithValue(NyxTextField('name'), 'Zebra')]);
  LView := NewNyxCollectionView(LStore, LSpec, cpList);
  LPeer := NewNyxCollectionView(LStore, LBase, cpList);
  LInspection := NyxCollectionInspection(LView);
  Check(LInspection.CaptureInspection.Configured and
    (LInspection.CaptureInspection.FollowMode = ncfLastVisible) and
    not NyxCollectionInspection(LPeer).CaptureInspection.Configured,
    'ordinary runtime construction applies declared defaults to its own view only');
  LBefore := LView.Snapshot;
  LInspection.PauseInspection;
  LStore.Update(NyxCollectionItem(NyxItem(LKey, 'reader')).WithValue(NyxTextField('name'), 'New incoming value'));
  Check((LView.Snapshot = LBefore) and (LPeer.Snapshot <> LBefore),
    'saved retention defaults enable real independent pause while a shared peer advances');
  LInspection.ResumeInspection;
  LInspection.ConfigureInspection(NyxInspectionPolicy);
  Check((LView.Spec.ToData.ToJSON = LSpec.ToData.ToJSON) and
    (LView.Spec.InspectionPolicy.DefaultFollow = ncfLastVisible),
    'runtime overrides and paused frames never rewrite authored defaults');
  LRefused := False;
  try
    ValidateNyxCollectionViewProjection(LSpec.UseDefaultPaging, cpTree);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unsupported Tree follow-last refuses before target mounting');
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LDocument.Collections.Define(LKey, LStore.Snapshot.Schema, [
      NyxCollectionItem(NyxItem(LKey, 'reader')).WithValue(NyxTextField('name'), CExact)]);
    LPage := NewNyxPage('reading');
    LDocument.AddPage(LPage);
    LList := NewNyxList('reader-list');
    LPage.Add(LList);
    LList.Binds.Collection(LSpec).Done;
    LSaved := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('.Inspection(NyxInspectionPolicy', LSource) > 0) and
      (Pos('.Follow(ncfLastVisible)', LSource) > 0) and
      (Pos('nyx.collections.inspection,', LSource) > 0), 'generated Pascal uses typed, readable inspection defaults');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LSaved, 'source admission reconstructs exact authored defaults and supplementary data');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      Check(not FileExists(ParamStr(1)), 'emitted companion output must be fresh');
      LBytes := UTF8Encode(LSource);
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LStream.WriteBuffer(LBytes[1], Length(LBytes));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-inspection-authoring-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' inspection authoring checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-inspection-authoring-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
