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

program nyx_pascal_completion_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.data, nyx.pascal.project, nyx.pascal.drafts,
  nyx.pascal.sources, nyx.pascal.completion, nyx.pascal.completion.wire, nyx.pascal.names
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

{ Rebuild one exact transport object field without ANSI string replacement or
  duplicate JSON keys. This mutates untrusted packets, never a source snapshot. }
function FieldWith(const AWire, AName: TNyxText;
  const AValue: TNyxDataValue): TNyxText;
var
  LObject: TNyxDataValue;
  LFields: array of TNyxDataField;
  LAt: Integer;
begin
  LObject := TNyxDataValue.ParseJSON(AWire);
  SetLength(LFields, LObject.Count);
  for LAt := 0 to LObject.Count - 1 do
  begin
    LFields[LAt] := NyxField(LObject.Key(LAt), LObject.Field(LObject.Key(LAt)));

    if LFields[LAt].Name = AName then
    begin
      LFields[LAt].Value := AValue;
    end;
  end;
  Result := NyxObject(LFields).ToJSON;
end;

procedure RefuseRequest(const AWire: TNyxText; const AReason: TNyxText);
var
  LID: TNyxText;
  LRejected: Boolean;
begin
  LRejected := False;
  try
    DecodeNyxCompletionRequest(AWire, LID);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LID = ''), AReason);
end;

procedure Run;
const
  CSource: TNyxText = 'unit assist; interface implementation end.';
  CPrefix: TNyxText = '// A friendly control 🚀 𐐷' + #10 + 'LBadge.';
  CExpected: TNyxText = 'WithTexx';
  CSuffix: TNyxText = '; // exact tail' + #0 + '𐐷';
  CDetail: TNyxText = 'function WithText(const AText: TNyxText): INyxBadge; 🚀';
var
  LProject: INyxPascalProject;
  LSources: INyxPascalSourceSnapshot;
  LPreview: INyxPascalSourceSnapshot;
  LChanged: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LBase: INyxPascalCompletionRequest;
  LDecoded: INyxPascalCompletionRequest;
  LResult: INyxPascalCompletionResult;
  LReceived: INyxPascalCompletionResult;
  LItem: TNyxPascalCompletionItem;
  LRows: array of TNyxPascalCompletionItem;
  LMain: TNyxPascalSourceRef;
  LHelper: TNyxPascalSourceRef;
  LUnit: TNyxPascalSourceRef;
  LStart: Integer;
  LCaret: Integer;
  LID: TNyxText;
  LWire: TNyxText;
  LRejected: Boolean;
  LAt: Integer;
begin
  LProject := NewNyxPascalProject(NyxPascalUnit('assist'), CSource)
    .WithInclude(NyxPascalInclude('details.inc'), 'const AValue = 2;')
    .WithUnit(NyxPascalUnit('assist.helper'), 'unit assist.helper; interface implementation end.');
  LMain := TNyxPascalSourceRef.Main;
  LHelper := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('details.inc'));
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('assist.helper'));
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LMain,
    CPrefix + CExpected + CSuffix);
  LStart := NyxTextScalarCount(CPrefix);
  LBase := NewNyxPascalCompletionRequest(LSources);
  LRequest := LBase.At(LMain, LStart + 3).Take(1).Skip(0);
  Check((LBase.Caret = 0) and (LBase.Limit = 20) and
    (LRequest.Caret = LStart + 3) and (LRequest.Limit = 1),
    'Fluent query configuration is immutable and scalar-based');
  LItem := NyxPascalCompletion('WithText', 'WithText', CDetail, pckMethod);
  LResult := NewNyxPascalCompletionResult(LRequest, pcsReady, '',
    LStart, CExpected, [LItem], 3);
  LPreview := LResult.Preview(0, LSources, LCaret);
  Check(LPreview.Text(LMain) = CPrefix + 'WithText' + CSuffix,
    'Completion replaces the whole identifier around a middle caret without duplicating its tail');
  Check(LCaret = LStart + 8, 'Proposed caret counts Unicode scalars after insertion');
  Check((LSources.Text(LMain) = CPrefix + CExpected + CSuffix) and
    (LPreview.AcceptedText(LMain) = CSource) and
    (LPreview.Text(LHelper) = LSources.Text(LHelper)),
    'Preview retains original/current/accepted and all independent helper buffers');
  Check((LResult.Total = 3) and (LResult.Count = 1) and
    (LResult.Item(0).Detail = CDetail), 'Bounded pages retain exact detached descriptors');

  LWire := EncodeNyxCompletionRequest(LRequest, 'owned-query');
  LDecoded := DecodeNyxCompletionRequest(LWire, LID);
  Check((LID = 'owned-query') and LDecoded.Matches(LSources) and
    LDecoded.FileReference.SameFile(LMain) and (LDecoded.Caret = LRequest.Caret),
    'Helper request transport preserves all exact source/caret context, including supplementary/NUL text');
  RefuseRequest(FieldWith(LWire, 'version', NyxData(2)),
    'Unknown transport versions refuse before exposing authority');
  RefuseRequest(FieldWith(LWire, 'origin', NyxData(42)),
    'Unknown source origins refuse instead of becoming editing snapshots');
  RefuseRequest(FieldWith(LWire, 'owner', NyxObject([
    NyxField('kind', NyxData('unit')), NyxField('name', NyxData('assist.helper.pas'))])),
    'A main request cannot hide a different translation-unit owner on the wire');
  RefuseRequest(FieldWith(LWire, 'file', NyxObject([
    NyxField('kind', NyxData('include')), NyxField('name', NyxData('../escape.inc'))])),
    'Private worker source references cannot carry filesystem paths');
  RefuseRequest(FieldWith(LWire, 'limit', NyxData(51)),
    'Untrusted row budgets refuse before provider dispatch');
  LDecoded := DecodeNyxCompletionRequest(EncodeNyxCompletionRequest(
    LBase.At(LUnit, 0), 'unit-query'), LID);
  Check(LDecoded.Owner.SameFile(LUnit) and LDecoded.FileReference.SameFile(LUnit),
    'A supporting unit automatically retains its own translation-unit context');
  LDecoded := DecodeNyxCompletionRequest(LWire, LID);
  LReceived := DecodeNyxCompletionReply(EncodeNyxCompletionReply(LResult, LID), LID, LDecoded);
  LPreview := LReceived.Preview(0, LSources, LCaret);
  Check((LPreview.Text(LMain) = CPrefix + 'WithText' + CSuffix) and
    (LReceived.Item(0).Kind = pckMethod), 'Reply correlation/typed descriptors preserve insertion semantics');

  LChanged := LSources.WithBuffer(LHelper, '{ Another incomplete helper');
  LRejected := False;
  try
    LResult.Preview(0, LChanged, LCaret);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCaret = 0) and
    (LChanged.Text(LMain) = LSources.Text(LMain)), 'A changed dependent helper atomically refuses stale insertion');
  LChanged := LSources.WithBuffer(LMain, 'A newer main');
  Check(not LRequest.Matches(LChanged), 'New current text cannot reuse an older request');
  LRejected := False;
  try
    DecodeNyxCompletionReply(EncodeNyxCompletionReply(LResult, 'foreign'), LID, LRequest);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A foreign helper reply cannot bind to this query');

  LRejected := False;
  try
    NewNyxPascalCompletionResult(LRequest, pcsReady, '', LStart, 'Wrong', [LItem], 1);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Provider ranges must match exact original text and contain the queried caret');
  LRejected := False;
  try
    NewNyxPascalCompletionResult(LRequest, pcsUnavailable, '', 0, '', [LItem], 1);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Unavailable states cannot smuggle insertion rows');

  LResult := NewNyxPascalCompletionResult(LRequest, pcsUnavailable,
    'No assistance provider is configured', 0, '', [], 0);
  LReceived := DecodeNyxCompletionReply(EncodeNyxCompletionReply(LResult, 'none'), 'none', LRequest);
  Check((LReceived.Status = pcsUnavailable) and (LReceived.Count = 0),
    'Missing compiler assistance remains a normal empty capability result');
  LRejected := False;
  try
    LBase.At(LMain, NyxTextScalarCount(LSources.Text(LMain)) + 1);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Caret bounds are validated before any provider dispatch');

  LRejected := False;
  try
    DecodeNyxCompletionRequest(LWire, LID, 10);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LID = ''), 'Encoded UTF-8 admission limits fail before exposing a query identity');

  SetLength(LRows, 50);
  for LAt := 0 to High(LRows) do
  begin
    LRows[LAt] := NyxPascalCompletion('Method' + IntToStr(LAt),
      StringOfChar('x', 2000), StringOfChar('y', 2048), pckMethod);
  end;
  LRejected := False;
  try
    NewNyxPascalCompletionResult(LBase.At(LMain, LStart).Take(50), pcsReady,
      '', LStart, '', LRows, 50);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A whole result has a real UTF-8 budget in addition to per-row limits');
  LProject := nil;
  LSources := nil;
  LDecoded := nil;
  Check((LRequest.Sources.Text(LMain) = CPrefix + CExpected + CSuffix) and
    (LRequest.Owner.Role = npsMain), 'Frozen request lifetime is independent of producer/project lifetimes');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' completion boundary checks';
    {$else}
    WriteLn('PASS ', GChecks, ' completion boundary checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
