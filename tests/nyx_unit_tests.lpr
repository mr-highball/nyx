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

program nyx_unit_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.controls, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.studio.projects, nyx.studio.session,
  nyx.studio.agents, nyx.studio.sourceedits
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

const
  CHeading: TNyxText = 'A thoughtful workspace 🌟';
  COldHeading: TNyxText = 'Build something thoughtful';
  CPreservedNote: TNyxText = '// Retain this 🌿 application note exactly.';
  CUnfinishedNote: TNyxText = '// Unfinished author note 🌿';
  CLeaf: TNyxText = '🌿';
  CHelper: TNyxText = 'function WorkshopNote: TNyxText;' + #10 +
    'begin' + #10 + '  Result := ''A thoughtful day 🌿'';' + #10 + 'end;';
  CClass: TNyxText = 'type' + #10 + '  { Handwritten theme, owned by the application. }' + #10 +
    '  TWorkshopTheme = class' + #10 + '  public' + #10 +
    '    class function Caption: TNyxText;' + #10 + '  end;' + #10 + #10;
  CNewHelper: TNyxText = 'class function TWorkshopTheme.Caption: TNyxText;' + #10 +
    'begin' + #10 + '  Result := ''A thoughtful workspace 🌟'';' + #10 + 'end;' + #10 + #10 +
    'function WorkshopNote: TNyxText;' + #10 +
    'begin' + #10 + '  Result := TWorkshopTheme.Caption;' + #10 + 'end;';

var
  LAgent: TNyxAgentSession;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LHeading: INyxHeading;
  LSeed: TNyxProjectPair;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSource: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LArguments: TNyxDataValue;
  LResult: TNyxDataValue;
  LRevision: Integer;
  LChecks: Integer;
  LOffset: Integer;
  LNext: Integer;
  LText: TNyxText;
  LOwner: TNyxText;
  LSession: TNyxStudioSession;
  LPatch: INyxSourcePatch;
  LEdits: array of TNyxSourceEdit;
  LRefused: Boolean;
  LClassOffset: Integer;
  LHelperOffset: Integer;
  LHeadingOffset: Integer;
  LHelperLine: Integer;
  LIndex: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Semantic Pascal unit: ' + AReason);
  end;
  Inc(LChecks);
end;

function ReplaceOnce(const AText, AOld, ANew: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(AOld, AText);
  Check(LPosition > 0, 'Exact fixture anchor exists');
  Result := Copy(AText, 1, LPosition - 1) + ANew +
    Copy(AText, LPosition + Length(AOld), MaxInt);
end;

{ Fixtures compute original scalar offsets rather than using platform storage
  indices. The public tool reports the same offsets through bounded windows. }
function OffsetOf(const ASource, AAnchor: TNyxText): Integer;
var
  LPosition: Integer;
  LCursor: Integer;
  LScalar: Integer;
begin
  LPosition := Pos(AAnchor, ASource);
  Check(LPosition > 0, 'Requested source anchor exists');
  LCursor := 1;
  Result := 0;
  while LCursor < LPosition do
  begin

    if not NyxNextScalar(ASource, LCursor, LScalar) then
    begin
      raise Exception.Create('Fixture source encoding is invalid');
    end;
    Inc(Result);
  end;
end;

function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
begin
  Result := LAgent.Call(ATool, 'Scooty source workshop', AArguments, LOwner);
  LRevision := LAgent.Revision;
end;

function Change(AOffset: Integer; const AExpected, AReplacement: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('offset', NyxData(AOffset)),
    NyxField('expected', NyxData(AExpected)), NyxField('replacement', NyxData(AReplacement))]);
end;

function Edit(const AID: TNyxText; const AChanges: array of TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('edit-unit')),
    NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData(AID)),
    NyxField('changes', NyxArray(AChanges))]);
end;

function PairText: TNyxText;
begin
  { The trusted editor exchange observes the actual accepted pair. Agent queries
    remain bounded and never return this whole-project observation packet. }
  Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(0))])).Field('project').AsText;
end;

procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText);
var
  LOriginal: TNyxText;
  LOldRevision: Integer;
  LDenied: Boolean;
begin
  LOriginal := PairText;
  LOldRevision := LRevision;
  LDenied := False;
  try
    Call('nyx_pascal', AArguments);
  except
    on Exception do
    begin
      LDenied := True;
    end;
  end;
  Check(LDenied, AReason);
  Check((PairText = LOriginal) and (LAgent.Revision = LOldRevision),
    'Refusal preserves the exact accepted pair and revision');
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
      LDocument.Title := 'Source workshop';
      LPage := NewNyxPage('home');
      LDocument.AddPage(LPage);
      LPage.Configure.Layout(nlColumn).Gap(16).Padding(24).Done;
      LHeading := NewNyxHeading('workshop-heading');
      LPage.Add(LHeading);
      LHeading.Configure.Text(COldHeading).Done;
      LSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
    SplitNyxSourceFrame(LSeed.Source, LPrefix, LBody, LSuffix);
    LPrefix := ReplaceOnce(LPrefix, 'implementation',
      CPreservedNote + #10 +
      'function WorkshopNote: TNyxText;' + #10 + #10 + 'implementation' + #10 + CHelper + #10);
    LSeed.Source := LPrefix + LBody + LSuffix;
    LAgent := TNyxAgentSession.Create(LSeed);
    try
      LAgent.InheritPermission(apEdit);
      LOwner := 'source-owner-one';
      LRevision := LAgent.Revision;
      LBefore := PairText;
      LSource := '';
      LOffset := 0;
      repeat
        LResult := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('unit')),
          NyxField('expectedRevision', NyxData(LRevision)), NyxField('offset', NyxData(LOffset)),
          NyxField('count', NyxData(4096))]));
        Check(LResult.Field('revision').AsInteger = LRevision, 'Bounded source is pinned');
        LSource := LSource + LResult.Field('text').AsText;
        LNext := LResult.Field('nextOffset').AsInteger;
        Check(LNext > LOffset, 'Nonterminal source page advances');
        LOffset := LNext;
      until LOffset = LResult.Field('total').AsInteger;
      Check(LSource = LSeed.Source, 'Bounded windows reconstruct exact accepted source');
      Check((LResult.Field('maximumChanges').AsInteger = 16) and
        (LResult.Field('maximumEditCharacters').AsInteger = 262144), 'Group budgets are discoverable');
      LOffset := OffsetOf(LSource, CLeaf);
      LResult := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(1))]));
      Check(LResult.Field('text').AsText = CLeaf, 'One supplementary scalar is not split');
      LClassOffset := OffsetOf(LSource, 'function WorkshopNote:');
      LHelperOffset := OffsetOf(LSource, CHelper);
      LHeadingOffset := OffsetOf(LSource, COldHeading);
      LHelperLine := 1;
      for LIndex := 1 to Pos(CHelper, LSource) - 1 do
      begin

        if LSource[LIndex] = #10 then
        begin
          Inc(LHelperLine);
        end;
      end;
      LResult := Call('nyx_pascal', NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('expectedRevision', NyxData(LRevision)), NyxField('line', NyxData(LHelperLine)),
        NyxField('count', NyxData(8))]));
      Check((LResult.Field('text').AsText = 'function') and
        (LResult.Field('offset').AsInteger = LHelperOffset) and
        (LResult.Field('line').AsInteger = LHelperLine),
        'One line query maps a known helper location to its exact scalar edit offset');
      LArguments := Edit('source-first', [
        Change(LClassOffset, '', CClass),
        Change(LHelperOffset, CHelper, CNewHelper),
        Change(LHeadingOffset, COldHeading, CHeading)]);
      LResult := Call('nyx_pascal', LArguments);
      Check(LResult.Field('sourceEdit').Field('changes').AsInteger = 3,
        'Class declaration, implementation and view share one small receipt');
      LAfter := PairText;
      LPair := DecodeNyxProject(LAfter);
      LText := ReplaceOnce(LSource, 'function WorkshopNote:', CClass + 'function WorkshopNote:');
      LText := ReplaceOnce(LText, CHelper, CNewHelper);
      LText := ReplaceOnce(LText, COldHeading, CHeading);
      Check(LPair.Source = LText, 'Every untouched byte and exact authored edit survives');
      Check(not LPair.Pending, 'Source admission leaves no pending draft');
      LDocument := TNyxCodec.Decode(LPair.Design);
      try
        Check(LDocument.Find('workshop-heading').Prop('text') = CHeading,
          'Grouped source admission reconstructs actual portable design');
      finally
        LDocument.Free;
      end;
      LResult := Call('nyx_pascal', LArguments);
      Check(PairText = LAfter, 'Exact retry returns original receipt without another edit');
      LResult := LAgent.Call('nyx_pascal', 'Another visible name', LArguments, LOwner);
      Check(PairText = LAfter, 'Transport ownership survives display-name changes');

      Refuse(Edit('source-first', [Change(0, '', '// different intent' + #10)]),
        'Changed request cannot reuse operation ID');
      Refuse(Edit('mismatch', [Change(LHelperOffset, CHelper, CNewHelper)]), 'Stale exact source range refuses');
      Refuse(Edit('outside', [Change(NyxMaximumSourceCharacters, '', '// outside')]), 'Outside source range refuses');
      Refuse(Edit('order', [Change(2, '', 'a'), Change(1, '', 'b')]), 'Descending ranges refuse');
      Refuse(Edit('overlap', [Change(0, 'unit', 'a'), Change(1, '', 'b')]), 'Overlapping ranges refuse');
      Refuse(Edit('insertion', [Change(0, '', 'a'), Change(0, '', 'b')]), 'Ambiguous same-position inserts refuse');
      Refuse(Edit('unchanged', [Change(0, '', '')]), 'No-op changes refuse');
      Refuse(Edit('missing-expected', [NyxObject([NyxField('offset', NyxData(0)),
        NyxField('replacement', NyxData('// note'))])]), 'Missing expected text refuses');
      Refuse(Edit('missing-replacement', [NyxObject([NyxField('offset', NyxData(0)),
        NyxField('expected', NyxData(''))])]), 'Missing replacement text refuses');
      Refuse(Edit('nested-context', [NyxObject([NyxField('offset', NyxData(0)),
        NyxField('expected', NyxData('')), NyxField('replacement', NyxData('// note')),
        NyxField('workspace', NyxData('foreign'))])]), 'Per-range context injection refuses');
      Refuse(Edit('malformed', [Change(OffsetOf(LPair.Source, CHeading), CHeading, '''')]),
        'Malformed managed source refuses after staging');
      Refuse(Edit('unsupported', [Change(OffsetOf(LPair.Source, '.Gap(16)'), '.Gap(16)', '.ImaginaryGap(16)')]),
        'Unsupported fluent source refuses without partial class changes');
      Refuse(Edit('atomic', [Change(OffsetOf(LPair.Source, CHeading), CHeading, 'Changed'),
        Change(OffsetOf(LPair.Source, 'end.'), 'WRONG', 'end.')]), 'Later mismatch rejects the complete group');
      Refuse(NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('expectedRevision', NyxData(LRevision - 1))]), 'Stale pinned read refuses');
      Refuse(NyxObject([NyxField('mode', NyxData('unit')), NyxField('line', NyxData(1)),
        NyxField('offset', NyxData(0))]), 'Competing line and scalar offset refuse');
      Refuse(NyxObject([NyxField('mode', NyxData('unit')), NyxField('line', NyxData(1000000))]),
        'Missing source line refuses');
      LAgent.InheritPermission(apReadOnly);
      Refuse(Edit('readonly', [Change(0, '', '// note' + #10)]), 'Read-only mode refuses source changes');
      LAgent.InheritPermission(apEdit);
      LOwner := 'source-owner-two';
      Refuse(LArguments, 'Another private owner cannot retry the original edit');
      LOwner := 'source-owner-one';

      Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('source-undo')), NyxField('direction', NyxData('undo'))]));
      Check(PairText = LBefore, 'One Undo restores exact original handwritten source and design');
      Call('nyx_history', NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
        NyxField('operationId', NyxData('source-redo')), NyxField('direction', NyxData('redo'))]));
      Check(PairText = LAfter, 'One Redo restores the complete authored pair');

      LSession := TNyxStudioSession.Create(LPair);
      try
        LSession.SetSourceDraft(LPair.Source + #10 + CUnfinishedNote);
        LSeed := LSession.ProjectSnapshot;
      finally
        LSession.Free;
      end;
      LRefused := False;
      try
        NyxSourcePatch([NyxSourceEdit(0, '', '// proposal' + #10)]).Candidate(LSeed);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and LSeed.Pending, 'Pending draft remains intact on candidate refusal');

      SetLength(LEdits, 1);
      LEdits[0] := NyxSourceEdit(0, '', '// detached 🌿' + #10);
      LPatch := NyxSourcePatch(LEdits);
      LEdits[0] := NyxSourceEdit(0, '', '// retargeted' + #10);
      LSeed := LPatch.Candidate(LPair);
      Check(Pos('// detached 🌿' + #10, LSeed.Source) = 1, 'Captured array owns detached intent');
      LPatch := nil;
      LRefused := False;
      try
        {$ifdef PAS2JS}
        NyxSourceEdit(0, '', TNyxText(#$d800));
        {$else}
        NyxSourceEdit(0, '', TNyxText(AnsiChar($ff)));
        {$endif}
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Malformed encoding refuses before admission');
      LRefused := False;
      try
        NyxSourcePatch([NyxSourceEdit(0, '', TNyxText(StringOfChar('x', 131073))),
          NyxSourceEdit(1, '', TNyxText(StringOfChar('y', 131072)))]);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Cumulative group budget applies across changes');

      {$ifndef PAS2JS}

      if ParamCount = 1 then
      begin
        SavePair(IncludeTrailingPathDelimiter(ParamStr(1)), LPair);
      end;
      WriteLn('PASS ', LChecks, ' coordinated semantic source checks');
      {$else}
      document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' coordinated semantic source checks';
      document.body.setAttribute('data-nyx-unit', 'passed');
      {$endif}
    finally
      LAgent.Free;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-unit', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
