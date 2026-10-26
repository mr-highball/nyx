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
program nyx_data_toolbar_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.source, nyx.data.toolbar, nyx.collections,
  nyx.collections.query, nyx.collections.view.types, nyx.composition, nyx.schema
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

const
  CBaseline: TNyxText = 'Original author''s order 🌙';
  CChoice: TNyxText = 'Name descending';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function Options(const ATarget: TNyxText): TNyxDataToolbarOptions;
begin
  Result := NyxDataToolbarOptions(NyxControl(ATarget), NyxTextField('name'))
    .SearchMode(ntsmStartsWith, nqtExact).Baseline(NyxToolbarSort(CBaseline))
    .Sort(NyxToolbarSort(CChoice), NyxCollectionQuery
      .OrderBy(NyxTextField('name'), nsdDescending)
      .ThenBy(NyxIntegerField('rank')))
    .Sort(NyxToolbarSort('Source order'), NyxCollectionQuery);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LWorkspace: INyxCard;
  LToolbar: INyxDataToolbar;
  LTable: INyxTable;
  LInstance: INyxComponent;
  LKey: TNyxCollectionRef;
  LRule: INyxControl;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'People worth finding';
    LKey := NyxCollection('people');
    Result.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('name'), '')
      .Integer(NyxIntegerField('rank'), 0), [
      NyxCollectionItem(NyxItem(LKey, 'ada')).WithValue(NyxTextField('name'), 'Ada')
        .WithValue(NyxIntegerField('rank'), 2),
      NyxCollectionItem(NyxItem(LKey, 'grace')).WithValue(NyxTextField('name'), 'Grace')
        .WithValue(NyxIntegerField('rank'), 1)]);
    LWorkspace := NewNyxCard('workspace-template');
    LWorkspace.Configure.Layout(nlColumn).Done;
    LToolbar := ConfigureNyxDataToolbar(NewNyxDataToolbar('tools'), Options('people-table'));
    LToolbar.Configure.PartName(NyxPart('tools')).Done;
    LToolbar.CreateButton.WithText('Invite a person');
    LWorkspace.Add(LToolbar);
    LTable := NewNyxTable('people-table');
    LTable.Configure.PartName(NyxPart('people')).Height(120).Done;
    LTable.Binds.Collection(NyxCollectionView(LKey).Column(NyxTextField('name'), 'People')
      .Scoped(csInstance));
    LWorkspace.Add(LTable);
    Result.AddComponent(LWorkspace);
    LPage := NewNyxPage('home');
    LInstance := NewNyxComponent('left');
    LInstance.Reference := NyxComponent('workspace-template');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('right');
    LInstance.Reference := NyxComponent('workspace-template');
    LRule := RetainNyxControl(LInstance.Node.OverridePart(NyxPart('tools'))
      .Named('right-tools-policy'));
    SetNyxDataToolbarIntent(LRule, TNyxDataToolbarIntent.Disabled);
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    LToolbar := ConfigureNyxDataToolbar(NewNyxDataToolbar('toolbar-template'), Options('unused'));
    Result.AddComponent(LToolbar);
    LPage := NewNyxPage('disabled-page');
    LInstance := NewNyxComponent('disabled-instance');
    LInstance.Reference := NyxComponent('toolbar-template');
    SetNyxDataToolbarIntent(LInstance, TNyxDataToolbarIntent.Disabled);
    LPage.Add(LInstance);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function ReplaceOnce(const AText, AOld, ANew: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  LIndex := Pos(AOld, AText);
  Check(LIndex > 0, 'source edit has an exact expected target');
  Result := Copy(AText, 1, LIndex - 1) + ANew + Copy(AText, LIndex + Length(AOld), MaxInt);
end;

procedure Refuses(AWorkspace: TNyxSourceWorkspace; ADocument: TNyxDocument;
  const ASource: TNyxText);
var
  LCandidate: TNyxDocument;
  LBefore: TNyxText;
  LSourceBefore: TNyxText;
  LRefused: Boolean;
begin
  LCandidate := nil;
  LBefore := TNyxCodec.Encode(ADocument);
  LSourceBefore := AWorkspace.Snapshot;
  LRefused := False;
  try
    try
      LCandidate := AWorkspace.Candidate(ADocument, ASource);
    except
      on ENyxSource do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LBefore) and
      (AWorkspace.Snapshot = LSourceBefore), 'bad typed source preserves the accepted design/source pair');
  finally
    LCandidate.Free;
  end;
end;

{$ifndef PAS2JS}
procedure SaveText(const AName: String; const AText: TNyxText);
var
  LBytes: TNyxBytes;
  LStream: TFileStream;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(AName, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure SourceCases;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LChanged: TNyxText;
  LRoot: TNyxNode;
  LControl: INyxControl;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LDocument := Fixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  LRoot := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.toolbar');
    Check((Pos('SetNyxDataToolbarIntent(', LSource) > 0) and
      (Pos('.SearchMode(ntsmStartsWith, nqtExact)', LSource) > 0) and
      (Pos('.Baseline(NyxToolbarSort(' + TNyxCodegen.TextLiteral(CBaseline), LSource) > 0) and
      (Pos('.Sort(NyxToolbarSort(''Name descending'')', LSource) > 0),
      'generated toolbar behavior uses readable typed fluent contracts');
    Check((Pos(': INyxDataToolbar;', LSource) > 0) and (Pos(': INyxSelect;', LSource) > 0) and
      (Pos('''nyx.data-toolbar''', LSource) = 0) and
      (Pos('ConfigureNyxDataToolbar(', LSource) = 0),
      'generation preserves specialized children and avoids raw intent or linked-tree replacement');
    LWorkspace.Accept(LDocument, LSource);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'concrete, reusable and named override intent survive exact source replay');
    LRoot := RealizeNyxView(LCandidate, LCandidate.Pages[0]);
    Check(GetNyxDataToolbarIntent(LRoot.Find(NyxQualifiedID('left', 'tools')))
      .Options.BaselineChoice.Name = CBaseline, 'supplementary Unicode and quotes remain exact');
    Check(GetNyxDataToolbarIntent(LRoot.Find(NyxQualifiedID('right', 'tools')))
      .Mode = ntiDisabled, 'named disabled override preserves the independent definition');
    FreeAndNil(LRoot);
    FreeAndNil(LCandidate);
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.SearchMode(ntsmStartsWith, nqtExact)',
      '.SearchMode(''starts-with'', nqtExact)'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, 'NyxControl(''people-table'')',
      'NyxPart(''people-table'')'));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, '.Sort(NyxToolbarSort(''Name descending'')',
      '.Sort(''Name descending'''));
    Refuses(LWorkspace, LDocument, ReplaceOnce(LSource, 'NyxTextField(''name''))' + #10 +
      '          .SearchMode', 'NyxIntegerField(''name''))' + #10 + '          .SearchMode'));
    LChanged := ReplaceOnce(LSource, '.SearchMode(ntsmStartsWith, nqtExact)',
      '.SearchMode(ntsmEqual, nqtAsciiInsensitive)');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(GetNyxDataToolbarIntent(LCandidate.Find('tools')).Options.MatchMode = ntsmEqual,
      'source edits submit a copied typed policy rather than executing a controller');
    FreeAndNil(LCandidate);
    LControl := RetainNyxControl(LDocument.Find('disabled-instance'));
    InheritNyxDataToolbarIntent(LControl);
    Check(not HasNyxDataToolbarIntent(LControl.Node), 'inherit removes only the local reusable mask');
    SetNyxDataToolbarIntent(LControl, TNyxDataToolbarIntent.Disabled);
    LBefore := TNyxCodec.Encode(LDocument);
    LRefused := False;
    try
      SetNyxDataToolbarIntent(RetainNyxControl(LDocument.Pages[0]), TNyxDataToolbarIntent.Disabled);
    except
      on ENyxDataToolbar do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'an unrelated host refuses intent before changing authored data');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      SaveText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.toolbar.pas', LSource);
      SaveText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'toolbar-design.json', LBefore);
    end;
    {$endif}
  finally
    LRoot.Free;
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure StaticPromotionCase;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPage: INyxPage;
  LToolbar: INyxDataToolbar;
  LSource: TNyxText;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LPage := NewNyxPage('home');
    LToolbar := NewNyxDataToolbar('static-tools');
    LPage.Add(LToolbar);
    LDocument.AddPage(LPage);
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.generated.toolbar.disabled');
    Check(Pos('  nyx.data.toolbar,', LSource) = 0, 'static absence adds no new intent dependency');
    LWorkspace.Accept(LDocument, LSource);
    SetNyxDataToolbarIntent(LToolbar, TNyxDataToolbarIntent.Disabled);
    LSource := LWorkspace.Render(LDocument);
    Check(Pos('  nyx.data.toolbar,', LSource) > 0,
      'promoting a static source frame carries its typed namespace');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'disabled intent needs no collection definitions or views');
    FreeAndNil(LCandidate);
    LBefore := TNyxCodec.Encode(LDocument);
    LToolbar.Extensions.SetValue(NyxExtension(NyxDataToolbarIntentKey),
      NyxObject([NyxField('version', NyxData(2)), NyxField('mode', NyxData('disabled'))]));
    LRefused := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'typed property admission refuses malformed known toolbar metadata');
    LCandidate := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    LRefused := False;
    try
      ValidateNyxDocumentProperties(LCandidate);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'decoded metadata undergoes the same typed admission as a Studio candidate');
    FreeAndNil(LCandidate);
    SetNyxDataToolbarIntent(LToolbar, TNyxDataToolbarIntent.Disabled);
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'metadata failure did not alter unrelated defaults');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      SaveText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.toolbar.disabled.pas', LSource);
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
    SourceCases;
    StaticPromotionCase;
    {$ifdef PAS2JS}
    Web.document.body.setAttribute('data-toolbar-source-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed DataToolbar source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      Web.document.body.setAttribute('data-toolbar-source-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
