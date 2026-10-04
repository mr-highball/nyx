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

unit nyx.test.source.contract;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxSourceContractTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.data,
  nyx.contract,
  nyx.codec,
  nyx.source,
  nyx.studio.session,
  nyx.test.core;

function ReplaceFirst(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  { Keep portable text typed throughout the native UTF-8 concatenation. }
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    raise Exception.Create('Source-contract fixture cannot find its edit');
  end;
  Result := Copy(ASource, 1, LPosition - 1) + AAfter +
    Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL source contract: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxSourceContractTests: Integer;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LDraft: TNyxText;
  LBaseline: TNyxText;
  LAccepted: TNyxText;
  LPayload: TNyxDataValue;
  LDomain: TNyxValueDomain;
  LEvent: TNyxEventContract;
  LRejected: Boolean;

  procedure Reject(const ABefore, AAfter, AReason: TNyxText);
  begin
    LDraft := ReplaceFirst(LSource, ABefore, AAfter);
    LCandidate := nil;
    LRejected := False;
    try
      try
        LCandidate := LWorkspace.Candidate(LDocument, LDraft);
      except
        on LException: Exception do
        begin
          LRejected := True;
          Check(LException.Message <> '', 'diagnostic: ' + AReason, Result);
        end;
      end;
    finally
      LCandidate.Free;
      LCandidate := nil;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBaseline) and
      (LWorkspace.Render(LDocument) = LSource), 'atomic refusal: ' + AReason, Result);
  end;

begin
  Result := 0;
  LDocument := CreateNyxPersistenceFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  LSession := nil;
  try
    LBaseline := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBaseline,
      'complete generated domains/metadata/nested extensions reconstruct exactly', Result);
    LCandidate.Free;
    LCandidate := nil;

    LDraft := ReplaceFirst(LSource, '.Range(0, 1).Choices([0.125, 0.5])',
      '.Range(0, 2).Choices([0.125, 0.5, 1])');
    LDraft := ReplaceFirst(LDraft,
      '.Field(NyxPart(''ratio''), NyxNumberDomain.Range(0, 1).Choices([0.125, 0.5]))',
      '.Field(NyxPart(''ratio''), NyxNumberDomain.Range(0, 2).Choices([0.125, 0.5, 1]))');
    LDraft := ReplaceFirst(LDraft,
      '.On(ntClick, NyxPartValue(NyxPart(''ratio'')), NyxNumberDomain.Range(0, 1))',
      '.On(ntClick, NyxPartValue(NyxPart(''ratio'')), NyxNumberDomain.Range(0, 2))' + #10 +
      '      .Signal(ntChange)');
    LDraft := ReplaceFirst(LDraft, 'NyxDecimal(''1.234567890123456789'')',
      'NyxDecimal(''9.876543210987654321'')');
    LDraft := ReplaceFirst(LDraft, '  except',
      '    Result.Extensions.SetValue(NyxExtension(''code.assets''), NyxObject([' + #10 +
      '      NyxField(''🌙'' + TNyxText(#0), NyxArray([NyxNull, NyxData(True),' + #10 +
      '        NyxData(7), NyxData(0.125), NyxData(''漢字'' + TNyxText(#10)),' + #10 +
      '        NyxData(NyxDecimal(''9007199254740993''))]))' + #10 +
      '    ]));' + #10 + '  except');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    Check(LCandidate.Find('fixture-ratio').Contract.FindValue(LDomain) and
      (LDomain.ReadWire('1').AsNumber = 1), 'Number domain widens Integer bounds/choices', Result);
    Check(LCandidate.Find('fixture-ratio').Contract.FieldAt(0).Domain.ReadWire('1').AsNumber = 1,
      'named field domain edits preserve exact scalar family', Result);
    Check(LCandidate.Find('fixture-ratio-commit').Contract.FindEvent(ntClick, LEvent) and
      (LEvent.Domain.ReadWire('2').AsNumber = 2) and
      (LEvent.ValueSource.Part = 'ratio'), 'typed event part source and range', Result);
    Check(LCandidate.Find('fixture-ratio-commit').Contract.FindEvent(ntChange, LEvent) and
      not LEvent.Domain.Defined, 'Signal remains distinct from a scalar payload', Result);
    Check(LCandidate.Extensions.Value(NyxExtension('studio.assets')).Field('tokens').Item(3).
      Field('ratio').AsDecimal.Text = '9.876543210987654321', 'edited decimal spelling is exact', Result);
    LPayload := LCandidate.Extensions.Value(NyxExtension('code.assets')).Field(TNyxText('🌙') + #0);
    Check((LPayload.Item(0).Kind = ndNull) and LPayload.Item(1).AsBoolean and
      (LPayload.Item(2).AsInteger = 7) and (LPayload.Item(3).AsNumber = 0.125) and
      (LPayload.Item(4).AsText = TNyxText('漢字') + #10) and
      (LPayload.Item(5).AsDecimal.Text = '9007199254740993'),
      'nested constructors preserve typed data, Unicode/NUL keys and exact integers', Result);
    Check(TNyxCodec.Encode(LDocument) = LBaseline, 'candidate owns edited data independently', Result);
    LWorkspace.Accept(LCandidate, LDraft);
    Check(LWorkspace.Render(LCandidate) = LDraft, 'accepted code retains crafted spelling', Result);
    LCandidate.Free;
    LCandidate := nil;
    LWorkspace.Reset;

    { Removal is replayed from an empty extension/declaration store rather than
      overlaid onto the original. A stale reader cache cannot hide omission. }
    LDraft := ReplaceFirst(LSource, '  except',
      '    Result.Extensions.Remove(NyxExtension(''studio.assets''));' + #10 +
      '    LFixtureRatioColumn.Extensions.Remove(NyxExtension(''nyx.contract''));' + #10 +
      '  except');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    Check(not LCandidate.Extensions.Has(NyxExtension('studio.assets')) and
      not LCandidate.Find('fixture-ratio').Contract.FindValue(LDomain),
      'typed removal invalidates domain readers and preserves other data', Result);
    LCandidate.Free;
    LCandidate := nil;

    Reject('NyxNumberDomain.Range(0, 1)', 'NyxIntegerDomain.Range(0, 1.0)', 'fractional Integer bound');
    Reject('NyxNumberDomain.Range(0, 1)', 'NyxNumberDomain.Range(''0'', 1)', 'text numeric bound');
    Reject('NyxNumberDomain.Range(0, 1)', 'NyxNumberDomain.Range(2, 1)', 'reversed range');
    Reject('NyxNumberDomain.Range(0, 1)', 'NyxTextDomain.Range(0, 1)', 'text domain has no Range');
    Reject('Choices([0.125, 0.5])', 'Choices([0.125, True])', 'wrong Number choice');
    Reject('Choices([0.125, 0.5])', 'Choices([0.125, 0.125])', 'duplicate choices');
    Reject('Choices([0.125, 0.5])', 'Choices([0.125, 0.25])', 'current default outside choices');
    Reject('NyxPart(''ratio'')', '''ratio''', 'raw field reference');
    Reject('NyxPartValue(NyxPart(''ratio''))', 'NyxPartValue(''ratio'')', 'raw event part source');
    Reject('NyxPartValue(NyxPart(''ratio''))', 'NyxNoEventValue', 'scalar event without source');
    Reject('.On(ntClick,', '.On(''click'',', 'raw event trigger');
    Reject('.On(ntClick,', '.On(ntDesignValue,', 'authoring trigger in runtime contract');
    Reject('.Value(NyxNumberDomain.Range(0, 1).Choices([0.125, 0.5]))',
      '.Value(''number'')', 'raw domain name');
    Reject('NyxExtension(''studio.assets'')', '''studio.assets''', 'raw extension reference');
    Reject('NyxExtension(''studio.assets'')', 'NyxExtension(''state'')', 'reserved document field');
    Reject('NyxDecimal(''9007199254740993'')', 'NyxDecimal(9007199254740993)', 'untyped exact decimal');
    Reject('NyxDecimal(''9007199254740993'')', 'NyxDecimal(''1e999'')', 'nonfinite decimal');
    Reject('NyxField(''tokens'',', 'NyxField(7,', 'nontext object key');
    Reject('NyxNull', '''null''', 'raw array member');
    Reject('NyxObject([', 'NyxArray([', 'object fields in data array');
    Reject('NyxField(''tokens'',', 'NyxField(''owner'',', 'duplicate object key');
    Reject('NyxField(''fields'', NyxArray([]))', 'NyxField(''fields'', NyxData(False))',
      'invalid imported contract metadata shape');
    Reject('LFixtureRatioColumn := NewNyxColumn(',
      'LFixtureRatioColumn.Contract.Value(NyxNumberDomain);' + #10 +
      '    LFixtureRatioColumn := NewNyxColumn(', 'contract before ownership');
    Reject('NyxIntegerDomain)', 'NyxIntegerDomain.Choices([1.5]))', 'fractional Integer choice');

    { Run the same candidate through Studio's real paired history boundary. }
    LSession := TNyxStudioSession.Create;
    LSession.Load(LBaseline);
    LAccepted := LSession.Source;
    LDraft := ReplaceFirst(LAccepted, '.Range(0, 1).Choices([0.125, 0.5])',
      '.Range(0, 2).Choices([0.125, 0.5, 1])');
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    Check(LSession.Document.Find('fixture-ratio').Contract.FindValue(LDomain) and
      (LDomain.ReadWire('1').AsNumber = 1), 'session admits typed contract code', Result);
    LSession.Undo;
    Check((LSession.Save = LBaseline) and (LSession.Source = LAccepted),
      'undo restores paired domain/source', Result);
    LSession.Redo;
    Check(LSession.Source = LDraft, 'redo restores exact accepted source', Result);
    LAccepted := LSession.Save;
    LDraft := ReplaceFirst(LSession.Source, '.Range(0, 2)', '.Range(2, 0)');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LAccepted) and (LSession.DraftSource = LDraft),
      'invalid domain retains accepted design and source draft', Result);

    { Exercise all closed choice families through the public constructors. }
    LSession.DiscardSourceDraft;
    LDraft := ReplaceFirst(LSession.Source, '  except',
      '    LProjectNameInput.Contract.Value(NyxTextDomain.Choices(['''', ''🌙 漢字'', ''READY'']));' + #10 +
      '    LEmailUpdatesCheckbox.Contract.Value(NyxBooleanDomain.Choices([False, True]));' + #10 +
      '    LCreateProjectButton.Contract.Signal(ntClick);' + #10 + '  except');
    LCandidate := LWorkspace.Candidate(LSession.Document, LDraft);
    Check(LCandidate.Find('project-name').Contract.FindValue(LDomain) and
      (LDomain.ReadWire('READY').AsText = 'READY'), 'Text choices stay text', Result);
    Check(LCandidate.Find('email-updates').Contract.FindValue(LDomain) and
      LDomain.ReadWire('true').AsBoolean, 'Boolean choices stay Boolean', Result);
    Check(LCandidate.Find('create-project').Contract.FindEvent(ntClick, LEvent) and
      not LEvent.Domain.Defined, 'new declaration on an owned existing button', Result);
  finally
    LSession.Free;
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

end.
