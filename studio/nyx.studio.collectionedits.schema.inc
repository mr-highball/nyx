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



{ Included by nyx.studio.collectionedits. The advertised schema has the same
  closed choices, primitive families and budgets as the strict decoder. }

{$include nyx.studio.collectionedits.queryschema.inc}

function NyxCollectionAgentSchema: TNyxDataValue;
var
  LScalar: TNyxDataValue;
  LDomain: TNyxText;
  LDefinitions: array of TNyxDataValue;
  LCells: array of TNyxDataValue;
  LBranches: array of TNyxDataValue;
  LFields: TNyxText;
  LValues: TNyxText;
  LRows: TNyxText;
  LSpec: TNyxText;
  LSpecOne: TNyxText;
  LSpecTwo: TNyxText;
  LSpecThree: TNyxText;
  LProperties: TNyxText;
  LRequired: TNyxText;
  LKind: TNyxStateKind;
  LAction: TNyxStudioCollectionAction;
  LIndex: Integer;
  LNumber: TNyxText;

  procedure Branch(const AProperties, ARequired: TNyxText);
  begin
    SetLength(LBranches, Length(LBranches) + 1);
    LBranches[High(LBranches)] := TNyxDataValue.ParseJSON(
      '{"type":"object","properties":{' + AProperties + '},"required":[' +
      ARequired + '],"additionalProperties":false}');
  end;

begin
  SetLength(LDefinitions, 4);
  SetLength(LCells, 4);
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin
    LNumber := NyxStateKindName(LKind);

    if LKind = nskText then
    begin
      LNumber := 'string';
    end;
    LScalar := TNyxDataValue.ParseJSON('{"type":"' + LNumber + '"}');

    if LKind = nskInteger then
    begin
      LScalar := TNyxDataValue.ParseJSON(
        '{"type":"integer","minimum":-2147483648,"maximum":2147483647}');
    end;
    LDomain := '{"oneOf":[{"type":"null"},{"type":"object","properties":{' +
      '"type":{"const":"' + NyxStateKindName(LKind) + '"}';

    if LKind in [nskInteger, nskNumber] then
    begin
      LDomain := LDomain + ',"min":' + LScalar.ToJSON + ',"max":' + LScalar.ToJSON;
    end;
    LDomain := LDomain + ',"choices":{"type":"array","minItems":1,"maxItems":128,' +
      '"uniqueItems":true,"items":' + LScalar.ToJSON +
      '}},"required":["type"],"additionalProperties":false';

    if LKind in [nskInteger, nskNumber] then
    begin
      LDomain := LDomain + ',"anyOf":[{"not":{"anyOf":[{"required":["min"]},' +
        '{"required":["max"]}]}},{"required":["min","max"]}]';
    end;
    LDomain := LDomain + '}]}';
    LDefinitions[Ord(LKind)] := TNyxDataValue.ParseJSON(
      '{"type":"object","properties":{"name":{"type":"string","minLength":1},' +
      '"kind":{"const":"' + NyxStateKindName(LKind) + '"},"default":' + LScalar.ToJSON +
      ',"domain":' + LDomain + '},"required":["name","kind","default"],' +
      '"additionalProperties":false}');
    LCells[Ord(LKind)] := TNyxDataValue.ParseJSON(
      '{"type":"object","properties":{"field":{"type":"string","minLength":1},' +
      '"kind":{"const":"' + NyxStateKindName(LKind) + '"},"value":' + LScalar.ToJSON +
      '},"required":["field","kind","value"],"additionalProperties":false}');
  end;
  LFields := NyxObject([NyxField('oneOf', NyxArray(LDefinitions))]).ToJSON;
  LValues := '{"type":"array","maxItems":64,"items":' +
    NyxObject([NyxField('oneOf', NyxArray(LCells))]).ToJSON + '}';
  LRows := '{"type":"array","maxItems":16384,"items":{"type":"object","properties":{' +
    '"item":{"type":"string","minLength":1},"values":' + LValues +
    '},"required":["item","values"],"additionalProperties":false}}';
  LBranches := nil;
  Branch('"op":{"const":"define"},"key":{"type":"string","minLength":1},' +
    '"fields":{"type":"array","minItems":1,"maxItems":64,"items":' + LFields + '},' +
    '"rows":' + LRows, '"op","key","fields","rows"');
  Branch('"op":{"const":"field"},"key":{"type":"string","minLength":1},' +
    '"definition":' + LFields, '"op","key","definition"');
  Branch('"op":{"const":"remove-field"},"key":{"type":"string","minLength":1},' +
    '"field":{"type":"string","minLength":1},' +
    '"kind":{"enum":["text","boolean","integer","number"]}', '"op","key","field","kind"');
  Branch('"op":{"enum":["append","update-row"]},"key":{"type":"string","minLength":1},' +
    '"item":{"type":"string","minLength":1},"values":' + LValues,
    '"op","key","item","values"');
  Branch('"op":{"const":"move-row"},"key":{"type":"string","minLength":1},' +
    '"item":{"type":"string","minLength":1},' +
    '"index":{"type":"integer","minimum":0,"maximum":16383}', '"op","key","item","index"');

  LSpec := '{"type":"object","properties":{"version":{"const":1},' +
    '"key":{"type":"string","minLength":1},"scope":{"enum":["application","instance"]},' +
    '"parent":{"type":"string"},"columns":{"type":"array","minItems":1,"maxItems":64,' +
    '"items":{"type":"object","properties":{"field":{"type":"string","minLength":1},' +
    '"kind":{"enum":["text","boolean","integer","number"]},"title":{"type":"string"},' +
    '"editable":{"type":"boolean"}},"required":["field","kind","title","editable"],' +
    '"additionalProperties":false}}},"required":["version","key","scope","parent","columns"],' +
    '"additionalProperties":false}';
  { Preserve query-free versions; version 3 explicitly chooses selection and
    carries a nonempty typed query. Recursive references resolve at tool root. }
  LSpecOne := LSpec;
  LSpecTwo := StringReplace(StringReplace(StringReplace(LSpecOne,
    '"const":1', '"const":2', []), '"parent":{"type":"string"}',
    '"selection":{"const":"multiple"},"parent":{"type":"string"}', []),
    '"required":["version","key","scope","parent","columns"]',
    '"required":["version","key","scope","parent","columns","selection"]', []);
  LSpecThree := StringReplace(StringReplace(StringReplace(LSpecOne,
    '"const":1', '"const":3', []), '"parent":{"type":"string"}',
    '"selection":{"enum":["single","multiple"]},"query":{"$ref":"#/$defs/nyxQuery"},' +
    '"parent":{"type":"string"}', []),
    '"required":["version","key","scope","parent","columns"]',
    '"required":["version","key","scope","parent","columns","selection","query"]', []);
  LSpec := '{"oneOf":[' + LSpecOne + ',' + LSpecTwo + ',' + LSpecThree + ']}';
  Branch('"op":{"const":"bind"},"owner":{"type":"string","minLength":1},' +
    '"projection":{"enum":["list","table","tree"]},"spec":' + LSpec,
    '"op","owner","projection","spec"');

  for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
  begin
    LProperties := '"op":{"const":"intent"},"action":{"const":"' +
      CCollectionActions[LAction] + '"},"key":{"type":"string","minLength":1}';
    LRequired := '"op","action","key"';

    if LAction = scaQuery then
    begin
      LProperties := LProperties + ',"baseline":{"type":"string","minLength":1},' +
        '"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]}';
      LRequired := LRequired + ',"baseline","query"';
    end;

    if NyxStudioCollectionViewAction(LAction) then
    begin
      LProperties := LProperties + ',"owner":{"type":"string","minLength":1},' +
        '"projection":{"enum":["list","table","tree"]}';
      LRequired := LRequired + ',"owner","projection"';
    end;

    if LAction in [scaAddRow, scaRemoveRow, scaCell] then
    begin
      LProperties := LProperties + ',"item":{"type":"string","minLength":1}';
      LRequired := LRequired + ',"item"';
    end;

    if LAction in [scaDefault, scaCell, scaTitle, scaMode, scaRemoveColumn, scaAddColumn] then
    begin
      LProperties := LProperties + ',"field":{"type":"string","minLength":1},' +
        '"kind":{"enum":["text","boolean","integer","number"]}';
      LRequired := LRequired + ',"field","kind"';
    end;

    if LAction = scaAddField then
    begin
      LProperties := LProperties + ',"kind":{"enum":["text","boolean","integer","number"]}';
      LRequired := LRequired + ',"kind"';
    end;

    if LAction in [scaDefault, scaCell] then
    begin
      { A single typed cell object also constrains primitive family. Scalar
        agreement is checked at decode; JSON schema expresses the alternatives. }
      for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
      begin
        LScalar := LCells[Ord(LKind)].Field('properties').Field('value');
        Branch(StringReplace(LProperties,
          '"kind":{"enum":["text","boolean","integer","number"]}',
          '"kind":{"const":"' + NyxStateKindName(LKind) + '"}', []) +
          ',"value":' + LScalar.ToJSON, LRequired + ',"value"');
      end;
      Continue;
    end;

    if LAction = scaTitle then
    begin
      LProperties := LProperties + ',"title":{"type":"string"}';
      LRequired := LRequired + ',"title"';
    end;

    if LAction = scaMode then
    begin
      LProperties := LProperties + ',"editable":{"type":"boolean"}';
      LRequired := LRequired + ',"editable"';
    end;

    if LAction = scaParent then
    begin
      LProperties := LProperties + ',"parent":{"type":["string","null"],"minLength":1}';
      LRequired := LRequired + ',"parent"';
    end;

    if LAction = scaScope then
    begin
      LProperties := LProperties + ',"scope":{"enum":["application","instance"]}';
      LRequired := LRequired + ',"scope"';
    end;

    if LAction = scaSelection then
    begin
      LProperties := LProperties + ',"selection":{"enum":["single","multiple"]}';
      LRequired := LRequired + ',"selection"';
    end;
    Branch(LProperties, LRequired);
  end;
  Result := TNyxDataValue.ParseJSON('{"type":"object","$defs":' +
    QueryAgentDefinitions.ToJSON + ',"oneOf":[' +
    '{"properties":{"mode":{"const":"list"},"offset":{"type":"integer","minimum":0},' +
    '"limit":{"type":"integer","minimum":1,"maximum":50},"filter":{"type":"string"}},' +
    '"required":["mode"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"schema"},"key":{"type":"string","minLength":1},' +
    '"offset":{"type":"integer","minimum":0,"maximum":64},' +
    '"limit":{"type":"integer","minimum":1,"maximum":50}},' +
    '"required":["mode","key"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"rows"},"key":{"type":"string","minLength":1},' +
    '"offset":{"type":"integer","minimum":0,"maximum":16384},' +
    '"limit":{"type":"integer","minimum":1,"maximum":20},' +
    '"fields":{"type":"array","minItems":1,"maxItems":16,"uniqueItems":true,' +
    '"items":{"type":"string","minLength":1}}},' +
    '"required":["mode","key"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"value"},"key":{"type":"string","minLength":1},' +
    '"field":{"type":"string","minLength":1},"item":{"type":"string","minLength":1},' +
    '"choice":{"type":"integer","minimum":0,"maximum":127},' +
    '"offset":{"type":"integer","minimum":0,"maximum":8388608},' +
    '"count":{"type":"integer","minimum":1,"maximum":4096}},' +
    '"required":["mode","key","field"],"additionalProperties":false,' +
    '"not":{"required":["item","choice"]}},' +
    '{"properties":{"mode":{"const":"domain"},"key":{"type":"string","minLength":1},' +
    '"field":{"type":"string","minLength":1},"offset":{"type":"integer","minimum":0,"maximum":128},' +
    '"limit":{"type":"integer","minimum":1,"maximum":50}},' +
    '"required":["mode","key","field"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"bindings"},"owner":{"type":"string","minLength":1},' +
    '"offset":{"type":"integer","minimum":0,"maximum":64},' +
    '"limit":{"type":"integer","minimum":1,"maximum":50}},' +
    '"required":["mode","owner"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"column"},"owner":{"type":"string","minLength":1},' +
    '"field":{"type":"string","minLength":1},"source":{"enum":["local","effective","restorable"]},' +
    '"offset":{"type":"integer","minimum":0,"maximum":8388608},' +
    '"count":{"type":"integer","minimum":1,"maximum":4096}},' +
    '"required":["mode","owner","field","source"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"apply"},"expectedRevision":{"type":"integer","minimum":1},' +
    '"operationId":{"type":"string","minLength":1,"maxLength":120},' +
    '"changes":{"type":"array","minItems":1,"maxItems":32,"items":' +
      NyxObject([NyxField('oneOf', NyxArray(LBranches))]).ToJSON + '}},' +
    '"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false}]}');
end;
