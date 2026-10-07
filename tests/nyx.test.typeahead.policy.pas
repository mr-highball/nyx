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
unit nyx.test.typeahead.policy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

{ Start from the unchanged authenticated English review. This local enrichment
  adds only typed list/tree binding policies; it does not publish to a server.
  The caller owns the complete document, including failure cleanup. }
function CreateNyxSavedTypeAheadFixture: TNyxDocument;
{ Qualify portable saved-policy admission, source/history and wire ownership.
  Native callers may export the exact tested source into their own ignored
  output directory for a separate compiler/execution check. Browser callers
  run the same assertions without claiming compilation or filesystem access. }
function RunNyxTypeAheadPolicyTests(const AOutputDirectory: TNyxText = ''): Integer;
{ Verify the independently compiled builder, rather than another parser result.
  Borrows ADocument only during the call; it never changes that document. }
function VerifyNyxSavedTypeAheadFixture(ADocument: TNyxDocument): Integer;

implementation

uses
  SysUtils, nyx.data, nyx.types, nyx.codec, nyx.codegen, nyx.source,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.selection, nyx.typeahead,
  nyx.studio.session, nyx.studio.collectionintent, nyx.generated.view
  {$ifndef PAS2JS}, Classes{$endif};

function ReplaceFirst(const AText, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, AText);

  if LPosition = 0 then
  begin
    raise Exception.Create('Saved typeahead fixture text is missing: ' + ABefore);
  end;
  { TNyxText operations preserve exact native UTF-8 and browser Unicode. }
  Result := Copy(AText, 1, LPosition - 1) + AAfter +
    Copy(AText, LPosition + Length(ABefore), Length(AText));
end;

function WithField(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));

    if AData.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

function CreateNyxSavedTypeAheadFixture: TNyxDocument;
var
  LNode: TNyxNode;
begin
  Result := BuildNyxDocument;
  try
    LNode := Result.Find('destination-list');
    LNode.SetCollectionView(LNode.CollectionView.TypeAhead(
      NyxTypeAhead.Enabled(False).WindowMilliseconds(700)));
    LNode := Result.Find('destination-tree');
    LNode.SetCollectionView(LNode.CollectionView.TypeAhead(
      NyxTypeAhead.WindowMilliseconds(800).Match(ntmExact)));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function VerifyNyxSavedTypeAheadFixture(ADocument: TNyxDocument): Integer;
var
  LList: TNyxCollectionViewSpec;
  LTree: TNyxCollectionViewSpec;

  procedure Check(ACondition: Boolean; const AMessage: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Compiled saved typeahead: ' + AMessage);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  ADocument.Validate;
  LList := ADocument.Find('destination-list').CollectionView;
  LTree := ADocument.Find('destination-tree').CollectionView;
  Check(LList.HasTypeAhead and not LList.TypeAheadPolicy.IsEnabled,
    'disabled list policy is reconstructed');
  Check((LList.TypeAheadPolicy.WindowMS = 700) and
    (LList.TypeAheadPolicy.MatchMode = ntmFolded), 'exact list options');
  Check(LTree.HasTypeAhead and LTree.TypeAheadPolicy.IsEnabled and
    (LTree.TypeAheadPolicy.WindowMS = 800) and
    (LTree.TypeAheadPolicy.MatchMode = ntmExact), 'exact tree options');
  Check((LTree.ParentField = 'parent') and (LList.Count = 1) and
    (LList.ColumnAt(0).Title = 'Destination'), 'hierarchy and columns retain meaning');
  Check((ADocument.Collections.Snapshot(NyxCollection('destinations')).Count = 8) and
    (ADocument.Title = 'Find your next destination'), 'unchanged English semantic defaults');
end;

function RunNyxTypeAheadPolicyTests(const AOutputDirectory: TNyxText): Integer;
const
  CInvalidPolicies: array[0..9] of TNyxText = (
    '{"version":2,"enabled":true,"windowMS":700,"match":"exact"}',
    '{"version":1,"enabled":"true","windowMS":700,"match":"exact"}',
    '{"version":1,"enabled":true,"windowMS":"700","match":"exact"}',
    '{"version":1,"enabled":true,"windowMS":700.5,"match":"exact"}',
    '{"version":1,"enabled":true,"windowMS":0,"match":"exact"}',
    '{"version":1,"enabled":true,"windowMS":60001,"match":"exact"}',
    '{"version":1,"enabled":true,"windowMS":700,"match":"Exact"}',
    '{"version":1,"windowMS":700,"match":"exact"}',
    '{"version":1,"enabled":true,"windowMS":700,"match":"exact","prefix":"s"}',
    '{"version":1,"enabled":true,"enabled":false,"windowMS":700,"match":"exact"}');
var
  LBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LCopy: TNyxCollectionViewSpec;
  LOptions: TNyxTypeAheadOptions;
  LData: TNyxDataValue;
  LVersion: Integer;
  LIndex: Integer;
  LRejected: Boolean;
  LBefore: TNyxText;
  LBeforeSource: TNyxText;
  LAccepted: TNyxText;
  LSource: TNyxText;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCandidate: TNyxDocument;
  LSession: TNyxStudioSession;
  LTable: TNyxNode;
  LIntent: TNyxStudioCollectionIntent;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}

  procedure Check(ACondition: Boolean; const AMessage: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Saved typeahead: ' + AMessage);
    end;
    Inc(Result);
  end;

  procedure RejectDescriptor(const AData: TNyxDataValue);
  begin
    LRejected := False;
    try
      LCopy := TNyxCollectionViewSpec.FromData(AData);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'malformed binding descriptor refuses');
  end;

  procedure RejectSource(const ABefore, AAfter: TNyxText);
  begin
    LSession.SetSourceDraft(ReplaceFirst(LSession.Source, ABefore, AAfter));
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LAccepted) and
      (LSession.Source = LSource), 'bad source retains accepted model and Pascal');
    Check(LSession.SourceDraftPending and (LSession.DraftSource <> LSource) and
      LSession.SourceDiagnostic.Defined and (LSession.SourceDiagnostic.Line > 0) and
      (LSession.SourceDiagnostic.Column > 0),
      'refused source remains an editable draft with a usable source location');
    LSession.DiscardSourceDraft;
  end;

begin
  Result := 0;
  LBase := NyxCollectionView(NyxCollection('destinations'))
    .Column(NyxTextField('title'), 'Destination');
  Check(not LBase.HasTypeAhead and LBase.TypeAheadPolicy.IsEnabled and
    (LBase.TypeAheadPolicy.WindowMS = 1000), 'absent choice uses the library default');
  for LVersion := 1 to 3 do
  begin
    LSpec := LBase;

    if LVersion = 2 then
    begin
      LSpec := LSpec.Selection(nsmMultiple);
    end;

    if LVersion = 3 then
    begin
      LSpec := LSpec.Query(NyxCollectionQuery.OrderBy(NyxTextField('title')));
    end;
    LData := LSpec.ToData;
    LBefore := LData.ToJSON;
    LCopy := TNyxCollectionViewSpec.FromData(LData);
    Check((LData.Field('version').AsInteger = LVersion) and
      (LCopy.ToData.ToJSON = LBefore) and not LCopy.HasTypeAhead,
      'legacy descriptor is byte-compatible');
    LCopy := LSpec.TypeAhead(NyxTypeAhead.Enabled(False)).UseDefaultTypeAhead;
    Check(LCopy.ToData.ToJSON = LBefore, 'reset restores the exact legacy shape');
  end;
  LSpec := LBase.Parent(NyxTextField('parent')).Scoped(csInstance)
    .Selection(nsmMultiple).Query(NyxCollectionQuery.OrderBy(NyxTextField('title')))
    .Column(NyxTextField('parent'), TNyxText('Unicode caption / ') + NyxScalarText($1F319))
    .TypeAhead(NyxTypeAhead.Enabled(False).WindowMilliseconds(60000).Match(ntmExact));
  LData := LSpec.ToData;
  LBefore := LData.ToJSON;
  LCopy := TNyxCollectionViewSpec.FromData(LData);
  Check((LData.Field('version').AsInteger = 4) and (LData.Count = 8) and
    (LCopy.ToData.ToJSON = LBefore), 'full saved binding keeps exact Unicode and query');
  LCopy := LSpec.Copy.Column(NyxTextField('extra'), 'Independent copy');
  Check((LSpec.Count = 2) and (LCopy.Count = 3) and
    (LSpec.ToData.ToJSON = LBefore), 'fluent policy/binding copies are independent');
  LOptions := LSpec.TypeAheadPolicy.Enabled(True).Match(ntmFolded);
  Check(not LSpec.TypeAheadPolicy.IsEnabled and
    (LSpec.TypeAheadPolicy.MatchMode = ntmExact) and LOptions.IsEnabled,
    'policy getter returns an independent scalar value');
  LCopy := LBase.TypeAhead(NyxTypeAhead);
  Check(LCopy.HasTypeAhead and (LCopy.ToData.Field('query').Kind = ndNull),
    'explicit default is distinct from an absent choice');
  RejectDescriptor(WithField(LData, 'version', NyxData(5)));
  RejectDescriptor(WithField(LData, 'selection', NyxData('Multiple')));
  RejectDescriptor(WithField(LData, 'query', NyxData('')));
  RejectDescriptor(WithField(LData, 'typeAhead', NyxNull));
  for LIndex := Low(CInvalidPolicies) to High(CInvalidPolicies) do
  begin
    LRejected := False;
    try
      LOptions := TNyxTypeAheadOptions.FromData(
        TNyxDataValue.ParseJSON(CInvalidPolicies[LIndex]));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'strict wire policy refuses case ' + IntToStr(LIndex));
  end;
  LRejected := False;
  try
    LCopy := LSpec.TypeAhead(Default(TNyxTypeAheadOptions));
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LSpec.ToData.ToJSON = LBefore),
    'undefined policy refuses without changing the original binding');
  LRejected := False;
  try
    LCopy := Default(TNyxCollectionViewSpec).TypeAhead(NyxTypeAhead);
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'an absent collection binding cannot accept a policy');

  LDocument := CreateNyxSavedTypeAheadFixture;
  LDecoded := nil;
  LCandidate := nil;
  LSession := TNyxStudioSession.Create;
  try
    Inc(Result, VerifyNyxSavedTypeAheadFixture(LDocument));
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := NyxCollection('destinations');
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('title'));
    LIntent.Value := 'Place';
    LSpec := LDocument.Find('destination-list').CollectionView;
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec,
      LDocument.Collections.Snapshot(LIntent.Key).Schema);
    Check((LCopy.TypeAheadPolicy.ToData.ToJSON = LSpec.TypeAheadPolicy.ToData.ToJSON)
      and LCopy.HasTypeAhead and (LCopy.ColumnAt(0).Title = 'Place'),
      'ordinary Inspector column edits preserve the exact saved search policy');
    LBefore := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LDecoded) = LBefore, 'document persistence keeps the exact saved pair');
    LCandidate := LDocument.Clone;
    Check(TNyxCodec.Encode(LCandidate) = LBefore, 'cloning retains independent saved policies');
    LCandidate.Find('destination-list').SetCollectionView(LBase);
    Check(LDocument.Find('destination-list').CollectionView.HasTypeAhead,
      'changing a clone does not alter the accepted document');
    LTable := TNyxNode.Create(nkTable, 'unsupported-table');
    LCandidate.Pages[0].Add(LTable);
    LSpec := LBase.TypeAhead(NyxTypeAhead.Enabled(False));
    LTable.SetCollectionView(LSpec);
    LRejected := False;
    try
      LCandidate.Validate;
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore),
      'unsupported table behavior refuses document admission');
    LStore := NewNyxCollection(LDocument.Collections.Snapshot(NyxCollection('destinations')));
    LRejected := False;
    try
      LView := NewNyxCollectionView(LStore, LSpec, cpTable);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LView = nil), 'ordinary live table also refuses unsupported policy');

    LSession.Load(LBefore);
    LBeforeSource := LSession.Source;
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('.TypeAhead(NyxTypeAhead', LSource) > 0) and
      (Pos('.Match(ntmExact)', LSource) > 0) and
      (Pos('  nyx.typeahead,', LSource) > 0) and
      (Pos('LDestinationList: INyxList;', LSource) > 0),
      'generated Pascal uses specialized references and a typed fluent policy');
    Check(TNyxCodegen.Generate(LDocument) = LSource, 'source generation is deterministic');
    LSource := ReplaceFirst(LSource, '.WindowMilliseconds(700)', '.WindowMilliseconds(900)');
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LAccepted := LSession.Save;
    Check((LSession.Document.Find('destination-list').CollectionView.TypeAheadPolicy.WindowMS = 900)
      and (LSession.Source = LSource), 'typed source admits one exact accepted pair');
    RejectSource('.Match(ntmExact)', '.Match(False)');
    RejectSource('.WindowMilliseconds(900)', '.WindowMilliseconds(0)');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LBeforeSource),
      'one Undo restores both the policy and source');
    LSession.Redo;
    Check((LSession.Save = LAccepted) and (LSession.Source = LSource),
      'one Redo restores the exact accepted pair');
    LSession.SetSourceDraft(ReplaceFirst(LSource, '.Match(ntmFolded))',
      '.Match(ntmFolded)).UseDefaultTypeAhead'));
    LSession.ApplySourceDraft;
    Check(not LSession.Document.Find('destination-list').CollectionView.HasTypeAhead,
      'handwritten parameterless reset reconstructs the default');
    LSession.Undo;
    LSession.SetSourceDraft(ReplaceFirst(LSource, '.Match(ntmFolded))',
      '.Match(ntmFolded)).UseDefaultTypeAhead()'));
    LSession.ApplySourceDraft;
    Check(not LSession.Document.Find('destination-list').CollectionView.HasTypeAhead,
      'parenthesized parameterless reset reconstructs the default');
    {$ifndef PAS2JS}

    if AOutputDirectory <> '' then
    begin
      ForceDirectories(AOutputDirectory);
      LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.typeahead');
      LStream := TFileStream.Create(IncludeTrailingPathDelimiter(AOutputDirectory) +
        'nyx.generated.typeahead.pas', fmCreate);
      try

        if LSource <> '' then
        begin
          LStream.WriteBuffer(LSource[1], Length(LSource));
        end;
      finally
        LStream.Free;
      end;
    end;
    {$endif}
  finally
    LView := nil;
    LStore := nil;
    LSession.Free;
    LCandidate.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end;

end.
