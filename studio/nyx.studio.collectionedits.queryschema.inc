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

{ Included by nyx.studio.collectionedits. Recursive MCP query discovery mirrors
  the strict portable descriptor. JSON Schema constrains shape/families; total
  nodes, depth, canonical byte budget, exact duplicate field identity and finite
  scalar admission remain enforced by the ordinary Pascal candidate decoder. }
function QueryAgentDefinitions: TNyxDataValue;
var
  LLeaves: array of TNyxDataValue;
  LSorts: array of TNyxDataValue;
  LKind: TNyxStateKind;
  LType: TNyxText;
  LComparison: TNyxText;
  LMatch: TNyxText;
  LScalar: TNyxText;
  LCommon: TNyxText;
  LPredicate: TNyxDataValue;
begin
  SetLength(LLeaves, 6);
  SetLength(LSorts, 4);
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin
    LType := NyxStateKindName(LKind);
    LComparison := '["equal","notEqual","less","atMost","greater","atLeast"]';
    LMatch := '{"const":"exact"}';
    LScalar := '{"type":"' + LType + '"}';

    if LKind = nskText then
    begin
      LComparison := '["equal","notEqual","contains","startsWith","endsWith"]';
      LMatch := '{"enum":["exact","asciiInsensitive"]}';
      LScalar := '{"type":"string"}';
    end
    else if LKind = nskBoolean then
    begin
      LComparison := '["equal","notEqual"]';
    end
    else if LKind = nskInteger then
    begin
      LScalar := '{"type":"integer","minimum":-2147483648,"maximum":2147483647}';
    end;
    LCommon := '"field":{"type":"string","minLength":1},"kind":{"const":"' +
      LType + '"},"textComparison":' + LMatch;
    LLeaves[Ord(LKind)] := TNyxDataValue.ParseJSON(
      '{"type":"object","properties":{"op":{"const":"field"},' + LCommon +
      ',"comparison":{"enum":' + LComparison + '},"value":' + LScalar +
      '},"required":["op","field","kind","comparison","textComparison","value"],' +
      '"additionalProperties":false}');
    LSorts[Ord(LKind)] := TNyxDataValue.ParseJSON(
      '{"type":"object","properties":{' + LCommon +
      ',"direction":{"enum":["ascending","descending"]}},' +
      '"required":["field","kind","direction","textComparison"],"additionalProperties":false}');
  end;
  LLeaves[4] := TNyxDataValue.ParseJSON(
    '{"type":"object","properties":{"op":{"enum":["all","any"]},' +
    '"children":{"type":"array","minItems":2,"maxItems":2,' +
    '"items":{"$ref":"#/$defs/nyxQueryPredicate"}}},' +
    '"required":["op","children"],"additionalProperties":false}');
  LLeaves[5] := TNyxDataValue.ParseJSON(
    '{"type":"object","properties":{"op":{"const":"not"},' +
    '"children":{"type":"array","minItems":1,"maxItems":1,' +
    '"items":{"$ref":"#/$defs/nyxQueryPredicate"}}},' +
    '"required":["op","children"],"additionalProperties":false}');
  LPredicate := NyxObject([NyxField('oneOf', NyxArray(LLeaves))]);
  Result := NyxObject([
    NyxField('nyxQueryPredicate', LPredicate),
    NyxField('nyxQuerySort', NyxObject([NyxField('oneOf', NyxArray(LSorts))])),
    NyxField('nyxQuery', TNyxDataValue.ParseJSON(
      '{"type":"object","description":"Nonempty policy; at most 64 predicate nodes, ' +
      'depth 16, eight distinct sort fields and 65536 canonical UTF-8 bytes.",' +
      '"properties":{"version":{"const":1},"filter":{"anyOf":[{"type":"null"},' +
      '{"$ref":"#/$defs/nyxQueryPredicate"}]},"order":{"type":"array","maxItems":8,' +
      '"items":{"$ref":"#/$defs/nyxQuerySort"}}},"required":["version","filter","order"],' +
      '"additionalProperties":false,"anyOf":[{"properties":{"filter":{"type":"object"}}},' +
      '{"properties":{"order":{"minItems":1}}}]}'))]);
end;
