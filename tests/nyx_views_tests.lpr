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

program nyx_views_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.controls, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.studio.projects, nyx.studio.session,
  nyx.studio.agents, nyx.studio.viewsedits
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

const
  CHeading: TNyxText = 'Ready for tomorrow 🌟';
  CStar: TNyxText = '🌟';
  CDraft: TNyxText = '// Pending user note 🌿';

var
  LAgent: TNyxAgentSession;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LHeading: INyxHeading;
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LReplacement: TNyxText;
  LResult: TNyxDataValue;
  LArguments: TNyxDataValue;
  LRevision: Integer;
  LChecks: Integer;
  LPair: TNyxProjectPair;
  LSession: TNyxStudioSession;
  LOwner: TNyxText;
  LOffset: Integer;
  LWindow: TNyxText;
  LScalar: Integer;
  LIndex: Integer;
  LPatch: INyxViewsPatch;
  LRefused: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Semantic views: ' + AReason);
  end;
  Inc(LChecks);
end;

{ Byte/storage-unit exact replacement in already admitted TNyxText. This avoids
  the native ANSI RTL StringReplace boundary for supplementary Unicode values. }
function ReplaceOnce(const AText, AOld, ANew: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  Check(LPosition > 0, 'Fixture replacement has its exact source anchor');
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld), MaxInt);
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := LAgent.Call(ATool, 'Scooty views workshop', AArguments, LOwner);
  LRevision := LAgent.Revision;
end;

function Edit(const AID, AExpected, ABuilder: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('edit-views')),
    NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData(AID)), NyxField('expected', NyxData(AExpected)),
    NyxField('builder', NyxData(ABuilder))]);
end;

function PairText: TNyxText;
begin
  { Trusted ordinary-editor observation, used only to compare complete paired
    invariants in this fixture. Public agent context remains bounded windows. }
  Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(0))])).Field('project').AsText;
end;

procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText);
var
  LOldPair: TNyxText;
  LOldRevision: Integer;
  LRefused: Boolean;
begin
  LOldPair := PairText;
  LOldRevision := LRevision;
  LRefused := False;
  try
    Call('nyx_pascal', AArguments);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
  Check((PairText = LOldPair) and (LRevision = LOldRevision),
    'Refusal retains exact paired files and revision');
end;

{$ifndef PAS2JS}
function PascalQuote(const AText: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '''';
  for LIndex := 1 to Length(AText) do
  begin
    Result := Result + AText[LIndex];

    if AText[LIndex] = '''' then
    begin
      Result := Result + '''';
    end;
  end;
  Result := Result + '''';
end;

procedure SavePair(const ADirectory: TNyxText; const APair: TNyxProjectPair);

  procedure Save(const AName, AText: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(ADirectory + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;

begin
  ForceDirectories(ADirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('design.nyx', APair.Design);
  Save('workshop.nyxproject', EncodeNyxProject(APair));
  Save('expected-design.inc', 'const ExpectedDesign: TNyxText = ' +
    PascalQuote(APair.Design) + ';' + #10);
end;
{$endif}

begin
  try
    LDocument := TNyxDocument.Create;
    try
      LDocument.Title := 'Views workshop';
      LPage := NewNyxPage('home');
      LDocument.AddPage(LPage);
      LPage.Configure.Layout(nlColumn).Gap(16).Padding(24).Done;
      LHeading := NewNyxHeading('workshop-heading');
      LPage.Add(LHeading);
      LHeading.Configure.Text('Build something thoughtful').Done;
      LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument),
        TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
    { Independent handwritten public helper and Unicode comments remain outside
      managed views. Their exact bytes are retained through every publication. }
    SplitNyxSourceFrame(LSeed.Source, LPrefix, LBody, LSuffix);
    LPrefix := ReplaceOnce(LPrefix, 'implementation',
      'function WorkshopNote: TNyxText;' + #10 + #10 + 'implementation' + #10 +
      '// Keep this application helper and its 🌿 comment exactly.' + #10 +
      'function WorkshopNote: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''A thoughtful day 🌿'';' + #10 + 'end;' + #10);
    LSeed.Source := LPrefix + LBody + LSuffix;
    LAgent := TNyxAgentSession.Create(LSeed);
    try
      LAgent.InheritPermission(apEdit);
      LOwner := 'views-owner-one';
      LRevision := LAgent.Revision;
      LBefore := PairText;

      { Inspect through the same semantic API a controller uses. Unicode-scalar
        pagination is deliberately small and crosses supplementary characters. }
      LOffset := 0;
      LWindow := '';
      repeat
        LResult := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('views')),
          NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(17))]));
        Check(LResult.Field('revision').AsInteger = LRevision,
          'Every bounded window belongs to the exact accepted revision');
        LWindow := LWindow + LResult.Field('text').AsText;
        LOffset := LResult.Field('nextOffset').AsInteger;
      until LOffset = LResult.Field('total').AsInteger;
      Check(LWindow = LBody, 'Paged context reconstructs exact managed builder');
      Check(not LResult.Field('pendingDraft').AsBoolean, 'Accepted seed has no draft');
      Check(LResult.Field('maximumEditCharacters').AsInteger = NyxMaximumViewsCharacters,
        'Portable proposal budget is discoverable');

      LReplacement := ReplaceOnce(LBody, '.Gap(16)', '.Gap(24)');
      LReplacement := ReplaceOnce(LReplacement, '''Build something thoughtful''',
        '''Ready for tomorrow 🌟''');
      LArguments := Edit('views-first', LBody, LReplacement);
      LResult := Call('nyx_pascal', LArguments);
      Check(LResult.Field('views').Field('replaced').AsBoolean,
        'One semantic receipt reports the source publication');
      LAfter := PairText;
      Check(LAfter <> LBefore, 'Typed builder changes the paired design and source');
      LPair := LAgent.ReviewSeed(LRevision);
      Check(LPair.Source = LPrefix + LReplacement + LSuffix,
        'Exact application helpers/imports/markers and builder survive admission');
      LDocument := TNyxCodec.Decode(LPair.Design);
      try
        Check(LDocument.Find('workshop-heading').Prop('text') = CHeading,
          'Source admission changes actual portable heading text');
        Check(LDocument.Find('home').Prop('gap') = '24',
          'Related typed layout change shares the publication');
      finally
        LDocument.Free;
      end;
      LResult := Call('nyx_pascal', LArguments);
      Check(PairText = LAfter, 'Exact transport-owner retry returns the original receipt');
      LResult := LAgent.Call('nyx_pascal', 'A renamed display actor', LArguments, LOwner);
      Check(PairText = LAfter, 'Display actor renaming retains the private owner receipt');
      Refuse(Edit('views-stale', LBody, LReplacement), 'Mismatched expected builder refuses');
      Refuse(Edit('views-unchanged', LReplacement, LReplacement), 'No-op builder refuses');
      Refuse(Edit('views-marker', LReplacement, LReplacement + NyxViewsEnd + #10),
        'Replacement cannot escape through a duplicate marker');
      Refuse(Edit('views-directive', LReplacement, '{$IFDEF SOMETHING}' + #10 + LReplacement),
        'Compiler directives cannot qualify the managed builder');
      Refuse(Edit('views-unsupported', LReplacement,
        ReplaceOnce(LReplacement, '.Gap(24)', '.ImaginaryGap(24)')),
        'Unsupported fluent source refuses before design publication');
      Refuse(Edit('views-first', LReplacement, LBody),
        'An operation ID cannot be reused with different current intent');

      LResult := Call('nyx_history', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('views-undo')), NyxField('direction', NyxData('undo'))]));
      Check(PairText = LBefore, 'One Undo restores exact design and handwritten source');
      LResult := Call('nyx_history', NyxObject([
        NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('views-redo')), NyxField('direction', NyxData('redo'))]));
      Check(PairText = LAfter, 'One Redo restores exact accepted source/design');

      { Read windows count Unicode scalars on UTF-8 and UTF-16, including 🌟. }
      LIndex := 1;
      LOffset := 0;
      while LIndex <= Length(LReplacement) do
      begin

        if not NyxNextScalar(LReplacement, LIndex, LScalar) then
        begin
          raise Exception.Create('Admitted builder is invalid Unicode');
        end;

        if LScalar = $1f31f then
        begin
          Break;
        end;
        Inc(LOffset);
      end;
      LResult := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('views')),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(1))]));
      Check(LResult.Field('text').AsText = CStar, 'Window retains one supplementary scalar');

      LAgent.InheritPermission(apReadOnly);
      Refuse(Edit('views-readonly', LReplacement, LBody), 'Read-only permission blocks mutation');
      LAgent.InheritPermission(apEdit);
      LOwner := 'views-owner-two';
      Refuse(LArguments, 'Identical display name cannot retry another transport owner receipt');
      LOwner := 'views-owner-one';

      { Pending author work must block an external intent, including when that
        intent was captured independently before the editor draft existed. }
      LSession := TNyxStudioSession.Create(LPair);
      try
        LSession.SetSourceDraft(LPair.Source + #10 + CDraft);
        LSeed := LSession.ProjectSnapshot;
      finally
        LSession.Free;
      end;
      LSession := nil;
      try
        NyxViewsPatch(LReplacement, LBody).Candidate(LSeed);
        Check(False, 'Pending draft must refuse');
      except
        on ENyxModel do
        begin
          Check(True, 'Pending draft retains independent author buffer');
        end;
      end;

      { The immutable contract enforces schema budgets independently, including
        callers which do not pass through the MCP JSON validator. }
      LRefused := False;
      try
        LPatch := NyxViewsPatch(LReplacement,
          TNyxText(StringOfChar('x', NyxMaximumViewsCharacters + 1)));
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Oversized Unicode-scalar proposal refuses at capture');
      LRefused := False;
      try
        {$ifdef PAS2JS}
        LPatch := NyxViewsPatch(LReplacement, TNyxText(#$d800));
        {$else}
        LPatch := NyxViewsPatch(LReplacement, TNyxText(AnsiChar($ff)));
        {$endif}
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Malformed target text cannot become a source proposal');
      LPatch := nil;

      {$ifndef PAS2JS}

      if ParamCount = 1 then
      begin
        SavePair(IncludeTrailingPathDelimiter(ParamStr(1)), LPair);
      end;
      WriteLn('PASS ', LChecks, ' guarded semantic views checks');
      {$else}
      document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' guarded semantic views checks';
      document.body.setAttribute('data-nyx-views', 'passed');
      {$endif}
    finally
      LAgent.Free;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-views', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
