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
{$include nyx.studio.collectionedits.formatschema.inc}

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
  LSpecFour: TNyxText;
  LSpecFive: TNyxText;
  LSpecSix: TNyxText;
  LSpecSeven: TNyxText;
  LSpecEight: TNyxText;
  LSpecNine: TNyxText;
  LColumnFormat: TNyxText;
  LTableQuery: TNyxText;
  LColumnQuery: TNyxText;
  LTableLayout: TNyxText;
  LColumnLayout: TNyxText;
  LLayoutFeatures: TNyxText;
  LPaging: TNyxText;
  LSizing: TNyxText;
  LTypeAhead: TNyxText;
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
  { This is external schema notation, not an authoring string API. Saved
    policies use the public scalar record's strict four-member descriptor. }
  LTypeAhead := '{"type":"object","properties":{"version":{"const":1},' +
    '"enabled":{"type":"boolean"},"windowMS":{"type":"integer","minimum":1,' +
    '"maximum":60000},"match":{"enum":["folded","exact"]}},' +
    '"required":["version","enabled","windowMS","match"],"additionalProperties":false}';
  LSpecFour := StringReplace(StringReplace(StringReplace(LSpecOne,
    '"const":1', '"const":4', []), '"parent":{"type":"string"}',
    '"selection":{"enum":["single","multiple"]},' +
    '"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]},' +
    '"typeAhead":' + LTypeAhead + ',"parent":{"type":"string"}', []),
    '"required":["version","key","scope","parent","columns"]',
    '"required":["version","key","scope","parent","columns","selection","query","typeAhead"]', []);
  LSizing := '{"type":"object","properties":{"version":{"const":1},' +
    '"mode":{"enum":["content","fixed","flex"]},"value":{"type":"integer","minimum":0,"maximum":100000},' +
    '"minimum":{"type":"integer","minimum":1,"maximum":100000},' +
    '"maximum":{"type":"integer","minimum":1,"maximum":100000}},' +
    '"required":["version","mode","value","minimum","maximum"],"additionalProperties":false,' +
    '"oneOf":[{"properties":{"mode":{"const":"content"},"value":{"const":0}}},' +
    '{"properties":{"mode":{"enum":["fixed","flex"]},"value":{"minimum":1}}}]}';
  LSpecFive := StringReplace(LSpecFour, '"const":4', '"const":5', []);
  LSpecFive := StringReplace(LSpecFive, '"typeAhead":' + LTypeAhead,
    '"typeAhead":{"anyOf":[{"type":"null"},' + LTypeAhead + ']}', []);
  LSpecFive := StringReplace(LSpecFive, '"editable":{"type":"boolean"}',
    '"editable":{"type":"boolean"},"sizing":' + LSizing, []);
  { At least one explicit sizing selects version five. Admission also checks
    this semantic condition and bound ordering through the portable contract. }
  LSpecFive := StringReplace(LSpecFive, '"maxItems":64,',
    '"maxItems":64,"contains":{"required":["sizing"]},', []);
  { Paging selects version six independently of column sizing. Keep the
    complete nullable query/typeahead shape and strict positive Integer policy. }
  LPaging := '{"type":"object","properties":{"version":{"const":1},' +
    '"pageSize":{"type":"integer","minimum":1,"maximum":2147483647},' +
    '"initialPage":{"type":"integer","minimum":1,"maximum":2147483647}},' +
    '"required":["version","pageSize","initialPage"],"additionalProperties":false}';
  LSpecSix := StringReplace(LSpecFour, '"const":4', '"const":6', []);
  LSpecSix := StringReplace(LSpecSix, '"typeAhead":' + LTypeAhead,
    '"typeAhead":{"anyOf":[{"type":"null"},' + LTypeAhead + ']}', []);
  LSpecSix := StringReplace(LSpecSix, '"editable":{"type":"boolean"}',
    '"editable":{"type":"boolean"},"sizing":' + LSizing, []);
  LSpecSix := StringReplace(LSpecSix, '"parent":{"type":"string"}',
    '"paging":' + LPaging + ',"parent":{"type":"string"}', []);
  LSpecSix := StringReplace(LSpecSix, '"selection","query","typeAhead"]',
    '"selection","query","typeAhead","paging"]', []);
  { Query-control descriptors share the public version-one scalar records.
    Family-specific operator admission remains with those typed records and
    the schema-qualified intent; this wire schema does not evaluate queries. }
  LTableQuery := '{"type":"object","properties":{"version":{"const":1},' +
    '"enabled":{"type":"boolean"},"filtering":{"type":"boolean"},' +
    '"sorting":{"type":"boolean"},"sortMode":{"enum":["single","multiple"]}},' +
    '"required":["version","enabled","filtering","sorting","sortMode"],"additionalProperties":false}';
  LColumnQuery := '{"type":"object","properties":{"version":{"const":1},' +
    '"filtering":{"type":"boolean"},"sorting":{"type":"boolean"},' +
    '"operators":{"anyOf":[{"type":"null"},{"type":"array","maxItems":9,' +
    '"uniqueItems":true,"items":{"enum":["equal","notEqual","less","atMost",' +
    '"greater","atLeast","contains","startsWith","endsWith"]}}]},' +
    '"textComparison":{"enum":["exact","asciiInsensitive"]}},' +
    '"required":["version","filtering","sorting","operators","textComparison"],' +
    '"additionalProperties":false,"if":{"properties":{"filtering":{"const":true}}},' +
    '"then":{"properties":{"operators":{"anyOf":[{"type":"null"},{"type":"array","minItems":1}]}}}}';
  LSpecSeven := StringReplace(LSpecSix, '"const":6', '"const":7', []);
  LSpecSeven := StringReplace(LSpecSeven, '"sizing":' + LSizing,
    '"sizing":{"anyOf":[{"type":"null"},' + LSizing + ']}', []);
  LSpecSeven := StringReplace(LSpecSeven, '"paging":' + LPaging,
    '"paging":{"anyOf":[{"type":"null"},' + LPaging + ']}', []);
  LSpecSeven := StringReplace(LSpecSeven, '"editable":{"type":"boolean"}',
    '"editable":{"type":"boolean"},"queryControls":{"anyOf":[{"type":"null"},' + LColumnQuery + ']}', []);
  LSpecSeven := StringReplace(LSpecSeven, '"required":["field","kind","title","editable"]',
    '"required":["field","kind","title","editable","sizing","queryControls"]', []);
  LSpecSeven := StringReplace(LSpecSeven, '"parent":{"type":"string"}',
    '"queryControls":{"anyOf":[{"type":"null"},' + LTableQuery + ']},"parent":{"type":"string"}', []);
  LSpecSeven := StringReplace(LSpecSeven, '"selection","query","typeAhead","paging"]',
    '"selection","query","typeAhead","paging","queryControls"]', []);
  { Layout is a distinct authored presence choice. Start with the complete
    version-seven envelope before its query-only declaration requirement. }
  LLayoutFeatures := '{"type":"array","maxItems":5,"uniqueItems":true,' +
    '"items":{"enum":["resize","auto-size","reorder","visibility","pinning"]}}';
  LColumnLayout := '{"type":"object","properties":{"version":{"const":1},' +
    '"visible":{"type":"boolean"},"pin":{"enum":["none","start","end"]},' +
    '"features":' + LLayoutFeatures + '},' +
    '"required":["version","visible","pin","features"],"additionalProperties":false}';
  LTableLayout := '{"type":"object","properties":{"version":{"const":1},' +
    '"enabled":{"type":"boolean"},"features":' + LLayoutFeatures + ',' +
    '"minimumCenter":{"type":"integer","minimum":1,"maximum":100000},' +
    '"autoSizeRows":{"type":"integer","minimum":1,"maximum":1024}},' +
    '"required":["version","enabled","features","minimumCenter","autoSizeRows"],' +
    '"additionalProperties":false}';
  LSpecEight := StringReplace(LSpecSeven, '"const":7', '"const":8', []);
  LSpecEight := StringReplace(LSpecEight, '"editable":{"type":"boolean"}',
    '"editable":{"type":"boolean"},"layoutControls":{"anyOf":[{"type":"null"},' + LColumnLayout + ']}', []);
  LSpecEight := StringReplace(LSpecEight,
    '"required":["field","kind","title","editable","sizing","queryControls"]',
    '"required":["field","kind","title","editable","sizing","queryControls","layoutControls"]', []);
  LSpecEight := StringReplace(LSpecEight, '"parent":{"type":"string"}',
    '"layoutControls":{"anyOf":[{"type":"null"},' + LTableLayout + ']},"parent":{"type":"string"}', []);
  LSpecEight := StringReplace(LSpecEight,
    '"selection","query","typeAhead","paging","queryControls"]',
    '"selection","query","typeAhead","paging","queryControls","layoutControls"]', []);
  LColumnFormat := ColumnFormatAgentSchema;
  LSpecNine := StringReplace(LSpecEight, '"const":8', '"const":9', []);
  LSpecNine := StringReplace(LSpecNine, '"editable":{"type":"boolean"}',
    '"editable":{"type":"boolean"},"display":{"anyOf":[{"type":"null"},' + LColumnFormat + ']}', []);
  LSpecNine := StringReplace(LSpecNine,
    '"required":["field","kind","title","editable","sizing","queryControls","layoutControls"]',
    '"required":["field","kind","title","editable","sizing","queryControls","layoutControls","display"]', []);
  Delete(LSpecNine, Length(LSpecNine), 1);
  LSpecNine := LSpecNine + ',"allOf":[{"properties":{"columns":{"contains":{' +
    '"properties":{"display":{"type":"object"}},"required":["display"]}}}}]}';
  Delete(LSpecEight, Length(LSpecEight), 1);
  LSpecEight := LSpecEight + ',"anyOf":[{"properties":{"layoutControls":{"type":"object"}}},' +
    '{"properties":{"columns":{"contains":{"properties":{"layoutControls":{"type":"object"}},' +
    '"required":["layoutControls"]}}}}]}';
  { Version seven requires at least one raw declaration. A null table choice
    with no column declaration stays an older version; defaults are not saved. }
  Delete(LSpecSeven, Length(LSpecSeven), 1);
  LSpecSeven := LSpecSeven + ',"anyOf":[{"properties":{"queryControls":{"type":"object"}}},' +
    '{"properties":{"columns":{"contains":{"properties":{"queryControls":{"type":"object"}},' +
    '"required":["queryControls"]}}}}]}';
  LSpec := '{"oneOf":[' + LSpecOne + ',' + LSpecTwo + ',' + LSpecThree + ',' +
    LSpecFour + ',' + LSpecFive + ',' + LSpecSix + ',' + LSpecSeven + ',' + LSpecEight + ',' + LSpecNine + ']}';
  Branch('"op":{"const":"bind"},"owner":{"type":"string","minLength":1},' +
    '"projection":{"enum":["list","table","tree"]},"spec":' + LSpec,
    '"op","owner","projection","spec"');
  Branch('"op":{"const":"query"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},' +
    '"projection":{"enum":["list","table","tree"]},' +
    '"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]}',
    '"op","owner","key","projection","query"');
  Branch('"op":{"const":"typeahead"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},"projection":{"enum":["list","tree"]},' +
    '"policy":{"oneOf":[{"type":"null"},' + LTypeAhead + ']}',
    '"op","owner","key","projection","policy"');

  { Named policy edits derive the form baseline inside each ordered candidate.
    Null resets only the declaration; defined default objects remain authored.
    These are table authoring commands, not restrictions on saved List metadata. }
  Branch('"op":{"const":"table-query"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},"projection":{"const":"table"},' +
    '"policy":{"oneOf":[{"type":"null"},' + LTableQuery + ']}',
    '"op","owner","key","projection","policy"');
  Branch('"op":{"const":"column-query"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},"projection":{"const":"table"},' +
    '"field":{"type":"string","minLength":1},' +
    '"kind":{"enum":["text","boolean","integer","number"]},' +
    '"policy":{"oneOf":[{"type":"null"},' + LColumnQuery + ']}',
    '"op","owner","key","projection","field","kind","policy"');

  Branch('"op":{"const":"table-layout"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},"projection":{"const":"table"},' +
    '"policy":{"oneOf":[{"type":"null"},' + LTableLayout + ']}',
    '"op","owner","key","projection","policy"');
  Branch('"op":{"const":"column-layout"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},"projection":{"const":"table"},' +
    '"field":{"type":"string","minLength":1},' +
    '"kind":{"enum":["text","boolean","integer","number"]},' +
    '"policy":{"oneOf":[{"type":"null"},' + LColumnLayout + ']}',
    '"op","owner","key","projection","field","kind","policy"');

  Branch('"op":{"const":"column-format"},"owner":{"type":"string","minLength":1},' +
    '"key":{"type":"string","minLength":1},"projection":{"const":"table"},' +
    '"field":{"type":"string","minLength":1},"kind":{"enum":["text","boolean","integer","number"]},' +
    '"policy":{"oneOf":[{"type":"null"},' + LColumnFormat + ']}',
    '"op","owner","key","projection","field","kind","policy"');
  for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
  begin
    LProperties := '"op":{"const":"intent"},"action":{"const":"' +
      CCollectionActions[LAction] + '"},"key":{"type":"string","minLength":1}';
    LRequired := '"op","action","key"';

    if LAction = scaQuery then
    begin
      LProperties := LProperties + ',"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]}';
      LRequired := LRequired + ',"query"';
    end;

    if LAction in [scaQuery, scaTableQuery, scaColumnQuery,
      scaDefaultTableQuery, scaDefaultColumnQuery, scaTableLayout,
      scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat] then
    begin
      LProperties := LProperties + ',"baseline":{"type":"string","minLength":1}';
      LRequired := LRequired + ',"baseline"';
    end;

    if LAction = scaTableQuery then
    begin
      LProperties := LProperties + ',"tableQuery":' + LTableQuery;
      LRequired := LRequired + ',"tableQuery"';
    end;

    if LAction = scaColumnQuery then
    begin
      LProperties := LProperties + ',"columnQuery":' + LColumnQuery;
      LRequired := LRequired + ',"columnQuery"';
    end;

    if LAction = scaTableLayout then
    begin
      LProperties := LProperties + ',"tableLayout":' + LTableLayout;
      LRequired := LRequired + ',"tableLayout"';
    end;

    if LAction = scaColumnLayout then
    begin
      LProperties := LProperties + ',"columnLayout":' + LColumnLayout;
      LRequired := LRequired + ',"columnLayout"';
    end;

    if LAction = scaColumnFormat then
    begin
      LProperties := LProperties + ',"columnFormat":' + LColumnFormat;
      LRequired := LRequired + ',"columnFormat"';
    end;

    if NyxStudioCollectionViewAction(LAction) then
    begin
      LProperties := LProperties + ',"owner":{"type":"string","minLength":1},' +
        '"projection":{"enum":["list","table","tree"]}';

      if LAction in [scaColumnContent, scaColumnFixed, scaColumnFlex,
        scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
        scaTableQuery, scaColumnQuery, scaDefaultTableQuery, scaDefaultColumnQuery,
        scaTableLayout, scaColumnLayout, scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat] then
      begin
        LProperties := StringReplace(LProperties,
          '"projection":{"enum":["list","table","tree"]}',
          '"projection":{"const":"table"}', []);
      end;
      LRequired := LRequired + ',"owner","projection"';

      if LAction in [scaPageSize, scaInitialPage, scaDefaultPaging] then
      begin
        LProperties := StringReplace(LProperties,
          '"projection":{"enum":["list","table","tree"]}',
          '"projection":{"enum":["list","table"]}', []);
      end;
    end;

    if LAction in [scaAddRow, scaRemoveRow, scaCell] then
    begin
      LProperties := LProperties + ',"item":{"type":"string","minLength":1}';
      LRequired := LRequired + ',"item"';
    end;

    if LAction in [scaDefault, scaCell, scaTitle, scaMode, scaRemoveColumn, scaAddColumn,
      scaColumnContent, scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
      scaColumnQuery, scaDefaultColumnQuery, scaColumnLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat] then
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

    if LAction in [scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum] then
    begin
      LProperties := LProperties + ',"value":{"type":"integer","minimum":1,"maximum":100000}';
      LRequired := LRequired + ',"value"';
    end;

    if LAction in [scaPageSize, scaInitialPage] then
    begin
      LProperties := LProperties + ',"value":{"type":"integer","minimum":1,"maximum":2147483647}';
      LRequired := LRequired + ',"value"';
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
    '"limit":{"type":"integer","minimum":1,"maximum":50},' +
    '"queryControls":{"type":"boolean","default":false},' +
    '"layoutControls":{"type":"boolean","default":false},' +
    '"formatControls":{"type":"boolean","default":false}},' +
    '"required":["mode","owner"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"column"},"owner":{"type":"string","minLength":1},' +
    '"field":{"type":"string","minLength":1},"source":{"enum":["local","effective","restorable"]},' +
    '"offset":{"type":"integer","minimum":0,"maximum":8388608},' +
    '"count":{"type":"integer","minimum":1,"maximum":4096},' +
    '"queryControls":{"type":"boolean","default":false},' +
    '"layoutControls":{"type":"boolean","default":false},' +
    '"formatControls":{"type":"boolean","default":false}},' +
    '"required":["mode","owner","field","source"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"query"},"owner":{"type":"string","minLength":1},' +
    '"source":{"enum":["local","effective","restorable"]},' +
    '"offset":{"type":"integer","minimum":0,"maximum":64},' +
    '"limit":{"type":"integer","minimum":1,"maximum":20}},' +
    '"required":["mode","owner","source"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"query-value"},"owner":{"type":"string","minLength":1},' +
    '"source":{"enum":["local","effective","restorable"]},' +
    '"path":{"type":"array","maxItems":15,"items":{"type":"integer","minimum":0,"maximum":1}},' +
    '"offset":{"type":"integer","minimum":0,"maximum":8388608},' +
    '"count":{"type":"integer","minimum":1,"maximum":4096}},' +
    '"required":["mode","owner","source","path"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"preview"},"owner":{"type":"string","minLength":1},' +
    '"source":{"enum":["local","effective","restorable"]},' +
    '"expectedRevision":{"type":"integer","minimum":1},"section":{"const":"summary"},' +
    '"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]}},' +
    '"required":["mode","owner","source","expectedRevision","section","query"],' +
    '"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"preview"},"owner":{"type":"string","minLength":1},' +
    '"source":{"enum":["local","effective","restorable"]},' +
    '"expectedRevision":{"type":"integer","minimum":1},"section":{"const":"rows"},' +
    '"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]},' +
    '"offset":{"type":"integer","minimum":0,"maximum":5000},' +
    '"limit":{"type":"integer","minimum":1,"maximum":20},' +
    '"fields":{"type":"array","minItems":1,"maxItems":16,"uniqueItems":true,' +
    '"items":{"type":"string","minLength":1}}},' +
    '"required":["mode","owner","source","expectedRevision","section","query"],' +
    '"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"preview"},"owner":{"type":"string","minLength":1},' +
    '"source":{"enum":["local","effective","restorable"]},' +
    '"expectedRevision":{"type":"integer","minimum":1},"section":{"const":"pascal"},' +
    '"query":{"anyOf":[{"type":"null"},{"$ref":"#/$defs/nyxQuery"}]},' +
    '"offset":{"type":"integer","minimum":0,"maximum":8388608},' +
    '"count":{"type":"integer","minimum":1,"maximum":4096}},' +
    '"required":["mode","owner","source","expectedRevision","section","query"],' +
    '"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"apply"},"expectedRevision":{"type":"integer","minimum":1},' +
    '"operationId":{"type":"string","minLength":1,"maxLength":120},' +
    '"changes":{"type":"array","minItems":1,"maxItems":32,"items":' +
      NyxObject([NyxField('oneOf', NyxArray(LBranches))]).ToJSON + '}},' +
    '"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false}]}');
end;
