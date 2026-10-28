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

unit nyx.test.data;

{$mode delphi}{$H+}
{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

interface

uses
  nyx.model;

{ Shared exact-value fixture consumed by native-to-browser reconstruction and
  every isolated HTTP build. It contains nested types and non-Double precision. }
procedure AddNyxDataFixture(ADocument: TNyxDocument);
function RunNyxDataTests: Integer;

implementation

uses
  SysUtils,
  fpjson,
  {$ifdef PAS2JS}
  JS,
  {$endif}
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.json,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.studio.session;

procedure Check(ACondition: Boolean; const AReason: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxJSON.Create('Extension fixture: ' + AReason);
  end;
  Inc(ACount);
end;

procedure RejectData(const ASource: TNyxText; var ACount: Integer);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    TNyxDataValue.ParseJSON(ASource);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'invalid structured input must be refused', ACount);
end;

procedure AddNyxDataFixture(ADocument: TNyxDocument);
var
  LExact: TNyxText;
begin
  LExact := TNyxText('Café / 🌙 / 漢字') + NyxScalarText(0) + TNyxText('''asset''');
  ADocument.Extensions.SetValue(NyxExtension('studio.assets'), NyxObject([
    NyxField('schema', NyxData(1)),
    NyxField('owner', NyxData(LExact)),
    NyxField('tokens', NyxArray([
      NyxNull,
      NyxData(True),
      NyxData(NyxDecimal('9007199254740993')),
      NyxObject([
        NyxField('ratio', NyxData(NyxDecimal('1.234567890123456789'))),
        NyxField('negative-zero', NyxData(NyxDecimal('-0.000E+00')))
      ])
    ]))
  ]));
  ADocument.Extensions.SetValue(
    NyxExtension(TNyxText('extension/🌙=') + NyxScalarText(0)), NyxArray([]));
  ADocument.Find('project-name').Extensions.SetValue(NyxExtension('app.validation'), NyxObject([
    NyxField('required', NyxData(True)),
    NyxField('message', NyxData(LExact)),
    NyxField('metadata', NyxObject([]))
  ]));
  ADocument.FindComponent('welcome-card').Extensions.SetValue(
    NyxExtension('app.presentation'), NyxData('template'));
  ADocument.Find('welcome-instance').Extensions.SetValue(
    NyxExtension('app.presentation'), NyxData('instance'));
  ADocument.Find('fixture-title-override').Extensions.SetValue(
    NyxExtension('app.presentation'), NyxData('custom title'));
end;

{ Compare typed construction with the independent strict JSON reader, including
  formatter preferences, control escapes, supplementary text and both admission
  limits. Copies must own immutable values after the caller changes its text. }
procedure TypedTextValues(var ACount: Integer);
var
  LText: TNyxText;
  LValue: TNyxDataValue;
  LCopy: TNyxDataValue;
  LRead: TNyxDataValue;
  LPreviousStrict: Boolean;
  LStrict: Boolean;
  LIndex: Integer;
  LRejected: Boolean;
begin
  LPreviousStrict := TJSONString.StrictEscaping;
  try
    for LStrict := False to True do
    begin
      TJSONString.StrictEscaping := LStrict;
      LText := TNyxText('"/\ / Cafe / ') + NyxScalarText($1f319);
      for LIndex := 0 to 31 do
      begin
        LText := LText + NyxScalarText(LIndex);
      end;
      LValue := NyxData(LText);
      LRead := TNyxDataValue.ParseJSON(EncodeNyxJSONString(LText, LStrict));
      Check((LValue.Kind = ndText) and (LValue.AsText = LText) and
        (LValue.ToJSON = LRead.ToJSON),
        'typed text retains exact strict-reader meaning and wire spelling', ACount);
      LCopy := LValue.Copy;
      LText := 'changed caller';
      LValue := NyxData(LText);
      Check((LCopy.AsText = LRead.AsText) and (LCopy.ToJSON = LRead.ToJSON),
        'typed text copy remains independent from caller reassignment', ACount);
    end;
    LText := TNyxText(StringOfChar('x', NyxMaximumJSONBytes - 2));
    LValue := NyxData(LText);
    Check((Length(LValue.ToJSON) = NyxMaximumJSONBytes) and
      (LValue.AsText = LText), 'typed text admits the exact quoted byte limit', ACount);
    LRejected := False;
    try
      NyxData(LText + 'x');
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'typed text refuses one byte beyond the quoted limit', ACount);
    LText := TNyxText(StringOfChar('/', NyxMaximumJSONBytes div 2));
    LRejected := False;
    try
      NyxData(LText);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'typed text counts strict escaping before admission', ACount);
    SetLength(LText, 1);
    {$ifdef PAS2JS}
    LText[1] := Char($d800);
    {$else}
    LText[1] := Char($ff);
    {$endif}
    LRejected := False;
    try
      NyxData(LText);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'typed text refuses malformed platform Unicode storage', ACount);
  finally
    TJSONString.StrictEscaping := LPreviousStrict;
  end;
end;

{$ifdef PAS2JS}
type
  { Browser-only test bridge for erased ordinal arguments. The test owns this
    JavaScript object; its declared Integer slot may actually hold any Number. }
  TUntrustedOrdinal = class external name 'Object'(TJSObject)
    value: Integer;
  end;
{$endif}

{ Independent JSON-reader oracles preserve canonical spelling and exact scalar
  meaning. Invalid default references and erased browser domains retain refusal
  or fallback, including values beyond an ordinal's declared native range. }
procedure TypedScalarValues(var ACount: Integer);
const
  CIntegers: array[0..4] of Integer = (Low(Integer), -1, 0, 1, High(Integer));
var
  LValue: TNyxDataValue;
  LRead: TNyxDataValue;
  LBoolean: Boolean;
  LIndex: Integer;
  LRejected: Boolean;
  {$ifdef PAS2JS}
  LHost: TUntrustedOrdinal;
  {$endif}
begin
  LValue := NyxNull;
  Check((LValue.Kind = ndNull) and (LValue.ToJSON = 'null'),
    'typed null retains its exact canonical literal', ACount);
  for LBoolean := False to True do
  begin
    LValue := NyxData(LBoolean);
    LRead := TNyxDataValue.ParseJSON(LowerCase(BoolToStr(LBoolean, True)));
    Check((LValue.AsBoolean = LBoolean) and (LValue.ToJSON = LRead.ToJSON),
      'typed Boolean agrees with the strict reader', ACount);
  end;
  for LIndex := Low(CIntegers) to High(CIntegers) do
  begin
    LValue := NyxData(CIntegers[LIndex]);
    LRead := TNyxDataValue.ParseJSON(IntToStr(CIntegers[LIndex]));
    Check((LValue.AsInteger = CIntegers[LIndex]) and (LValue.ToJSON = LRead.ToJSON),
      'typed ordinal preserves signed boundaries and canonical spelling', ACount);
  end;
  LValue := NyxData(NyxDecimal('9007199254740993'));
  Check(LValue.AsDecimal.Text = '9007199254740993',
    'admitted decimal references preserve values beyond Double precision', ACount);
  LRejected := False;
  try
    NyxData(Default(TNyxDecimal));
  except
    on ENyxJSON do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'default decimal references retain strict admission refusal', ACount);
  {$ifdef PAS2JS}
  LHost := TUntrustedOrdinal(TJSObject.new);
  TJSObject(LHost)['value'] := 1.25;
  LValue := NyxData(LHost.value);
  Check(LValue.AsDecimal.Text = '1.25',
    'erased fractional ordinals retain the former numeric parser behavior', ACount);
  TJSObject(LHost)['value'] := 2147483648.0;
  LValue := NyxData(LHost.value);
  Check(LValue.AsDecimal.Text = '2147483648',
    'erased upper out-of-range ordinals retain their complete numeric spelling', ACount);
  TJSObject(LHost)['value'] := -2147483649.0;
  LValue := NyxData(LHost.value);
  Check(LValue.AsDecimal.Text = '-2147483649',
    'erased lower out-of-range ordinals retain their complete numeric spelling', ACount);
  for LIndex := 0 to 3 do
  begin
    case LIndex of
      0: TJSObject(LHost)['value'] := TJSNumber.NaN;
      1: TJSObject(LHost)['value'] := TJSNumber.POSITIVE_INFINITY;
      2: TJSObject(LHost)['value'] := TJSNumber.NEGATIVE_INFINITY;
      3: TJSObject(LHost)['value'] := 'not-a-number';
    end;
    LRejected := False;
    try
      NyxData(LHost.value);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'invalid erased numeric arguments retain strict refusal', ACount);
  end;
  {$endif}
end;

{ Extract scalars against independent strict-reader oracles, including after
  parent retirement and a formatter change. The largest child deliberately
  retains the former formatted read refusal rather than failing extraction. }
procedure IndexedScalarValues(var ACount: Integer);
const
  CLiterals: array[0..6] of TNyxText = ('"a/b"', '"\ud83c\udf19\u0000"',
    'null', 'true', 'false', '-0.000E+00', '9007199254740993');
var
  LOriginalStrict: Boolean;
  LStrict: Boolean;
  LIndex: Integer;
  LParent: TNyxDataValue;
  LSaved: TNyxDataValue;
  LChild: TNyxDataValue;
  LRead: TNyxDataValue;
  LRejected: Boolean;
begin
  LOriginalStrict := TJSONString.StrictEscaping;
  try
    TJSONString.StrictEscaping := False;
    LParent := TNyxDataValue.ParseJSON('["a/b","\ud83c\udf19\u0000",' +
      'null,true,false,-0.000E+00,9007199254740993]');
    LSaved := LParent.Copy;
    LParent := NyxNull;
    for LStrict := False to True do
    begin
      TJSONString.StrictEscaping := LStrict;
      for LIndex := Low(CLiterals) to High(CLiterals) do
      begin
        LRead := TNyxDataValue.ParseJSON(CLiterals[LIndex]);
        LChild := LSaved.Item(LIndex);
        Check((LChild.Kind = LRead.Kind) and (LChild.ToJSON = LRead.ToJSON),
          'indexed scalar matches independent admission under current formatter policy', ACount);

        if LChild.Kind = ndText then
        begin
          Check(LChild.AsText = LRead.AsText,
            'indexed text retains exact decoded supplementary/NUL/solidus meaning', ACount);
        end;
      end;
      LChild := LSaved.Copy;
      LChild := LChild.Item(1);
      Check(LChild.AsText = NyxScalarText($1f319) + NyxScalarText(0),
        'scalar extraction may replace its own parent receiver without losing text', ACount);
    end;
    TJSONString.StrictEscaping := False;
    LParent := TNyxDataValue.ParseJSON('["' + TNyxText(StringOfChar('/', 1024)) +
      TNyxText(StringOfChar('a', NyxMaximumJSONBytes - 1028)) + '"]');
    TJSONString.StrictEscaping := True;
    LChild := LParent.Item(0);
    Check((LChild.Kind = ndText) and (Length(LChild.ToJSON) > NyxMaximumJSONBytes),
      'changed formatter can still expand an admitted child beyond its read budget', ACount);
    LRejected := False;
    try
      LChild.AsText;
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'expanded child keeps its established read-time refusal', ACount);
  finally
    TJSONString.StrictEscaping := LOriginalStrict;
  end;
end;

{ A child container must equal independent full admission under every retained
  formatter policy, and own its immediate index after the parent is replaced.
  Mutable RTL formatting settings are restored even on a failed assertion. }
procedure IndexedContainerValues(var ACount: Integer);
const
  CSource: TNyxText = '[{"":[],"moon\ud83c\udf19\u0000":' +
    '{"path":"a/b","values":[null,true,-0.000E+00,9007199254740993]}},' +
    '["literal {}[]","a\"b\\c",[]],{}]';
var
  LOriginalStrict: Boolean;
  LOriginalCompressed: Boolean;
  LOriginalUnquoted: Boolean;
  LStrict: Boolean;
  LCompressed: Boolean;
  LParent: TNyxDataValue;
  LSaved: TNyxDataValue;
  LChild: TNyxDataValue;
  LRead: TNyxDataValue;
  LName: TNyxText;
  LRejected: Boolean;
begin
  LOriginalStrict := TJSONString.StrictEscaping;
  LOriginalCompressed := TJSONData.CompressedJSON;
  LOriginalUnquoted := TJSONObject.UnquotedMemberNames;
  try
    TJSONObject.UnquotedMemberNames := False;
    LName := TNyxText('moon') + NyxScalarText($1f319) + NyxScalarText(0);
    for LCompressed := False to True do
    begin
      for LStrict := False to True do
      begin
        TJSONData.CompressedJSON := LCompressed;
        TJSONString.StrictEscaping := LStrict;
        LParent := TNyxDataValue.ParseJSON('{"keep":' + CSource + ',"sibling":true}');
        LSaved := LParent.Copy;
        LChild := LParent.Field('keep');
        LParent := NyxNull;
        LRead := TNyxDataValue.ParseJSON(CSource);
        Check((LChild.ToJSON = LRead.ToJSON) and (LChild.Count = 3),
          'owned child container matches independent full admission and ordering', ACount);
        Check((LChild.Item(0).Key(1) = LName) and
          (LChild.Item(1).Item(1).AsText = 'a"b\c') and
          (LChild.Item(0).Field('').Count = 0) and (LChild.Item(2).Count = 0),
          'child index preserves escaped punctuation, exact keys and empty containers', ACount);
        LChild := LChild.Item(0).Field(LName);
        Check((LChild.Field('path').AsText = 'a/b') and
          (LChild.Field('values').Item(3).AsDecimal.Text = '9007199254740993'),
          'nested container may replace its own receiver without losing text or precision', ACount);
        Check(LSaved.Field('keep').ToJSON = LRead.ToJSON,
          'extraction never mutates a copied parent index', ACount);
      end;
    end;
    TJSONString.StrictEscaping := False;
    TJSONData.CompressedJSON := False;
    LParent := TNyxDataValue.ParseJSON('{"keep":' + CSource + '}');
    for LCompressed := False to True do
    begin
      for LStrict := False to True do
      begin
        TJSONData.CompressedJSON := LCompressed;
        TJSONString.StrictEscaping := LStrict;
        LRead := TNyxDataValue.ParseJSON(CSource);
        Check(LParent.Field('keep').ToJSON = LRead.ToJSON,
          'changed whitespace/escaping policies retain ordinary child reformatting', ACount);
      end;
    end;
    TJSONObject.UnquotedMemberNames := True;
    LParent := TNyxDataValue.ParseJSON('[{"key":1}]');
    TJSONObject.UnquotedMemberNames := False;
    LRejected := False;
    try
      LParent.Item(0);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unquoted parent formatter output grants no trusted child admission', ACount);
    LParent := TNyxDataValue.ParseJSON('[{"key":1}]');
    TJSONObject.UnquotedMemberNames := True;
    LRejected := False;
    try
      LParent.Item(0);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'current unquoted formatter retains its former child extraction refusal', ACount);
  finally
    TJSONString.StrictEscaping := LOriginalStrict;
    TJSONData.CompressedJSON := LOriginalCompressed;
    TJSONObject.UnquotedMemberNames := LOriginalUnquoted;
  end;
end;

function RunNyxDataTests: Integer;
var
  LValue: TNyxDataValue;
  LSnapshot: TNyxDataValue;
  LItems: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LStore: TNyxExtensions;
  LCopy: TNyxExtensions;
  LForeign: TNyxExtensions;
  LReference: TNyxExtensionRef;
  LDecimal: TNyxDecimal;
  LRejected: Boolean;
  LJSON: TJSONData;
  LJSONCopy: TJSONData;
  LDocument: TNyxDocument;
  LClone: TNyxDocument;
  LView: TNyxDocument;
  LNode: TNyxNode;
  LRuntime: TNyxNode;
  LSibling: TNyxNode;
  LDefinition: TNyxNode;
  LSession: TNyxStudioSession;
  LAccepted: TNyxDocument;
  LBaseline: TNyxText;
  LChanged: TNyxText;
  LSource: TNyxText;
  LParts: TNyxStrings;
  LIndex: Integer;

  { Container punctuation inside escaped keys/strings must never become an
    indexed boundary. Child snapshots keep exact meaning after every parent and
    caller record is reassigned; cached reads retain ordinary contract failures. }
  procedure IndexedReads;
  var
    LParent: TNyxDataValue;
    LSaved: TNyxDataValue;
    LChild: TNyxDataValue;
    LLeaf: TNyxDataValue;
    LName: TNyxText;
    LNested: TNyxText;
    LCanonical: TNyxText;
    LRejected: Boolean;
    LNumbers: array of TNyxDataValue;
    LNumber: Integer;
  begin
    LName := TNyxText('nul') + NyxScalarText(0) + TNyxText('moon🌙');
    LParent := TNyxDataValue.ParseJSON(
      '{"": [{"[]":"a\"b\\c{}[],","empty":{}},false,-0.000E+00],' +
      '"nul\u0000moon\ud83c\udf19":"🌙\u0000true", "A":null}');
    Check((LParent.Count = 3) and (LParent.Key(0) = '') and
      (LParent.Key(1) = LName) and (LParent.Key(2) = 'A'),
      'indexed object keys retain exact empty/NUL/supplementary identity/order', Result);
    LCanonical := LParent.ToJSON;
    Check(TNyxDataValue.ParseJSON(LCanonical).ToJSON = LCanonical,
      'indexed snapshot retains canonical interchange bytes', Result);
    LChild := LParent.Field('');
    Check((LChild.Count = 3) and (LChild.Item(0).Kind = ndObject) and
      not LChild.Item(1).AsBoolean and
      (LChild.Item(2).AsDecimal.Text = '-0.000E+00'),
      'indexed array retains mixed kinds and exact number spelling', Result);
    Check((LChild.Item(0).Field('[]').AsText = 'a"b\c{}[],') and
      (LChild.Item(0).Field('empty').Count = 0),
      'escaped quotes/slashes and container punctuation remain string data', Result);
    LLeaf := LParent.Field(LName);
    LSaved := LParent.Copy;
    LParent := NyxObject([]);
    Check((LSaved.ToJSON = LCanonical) and (LSaved.Count = 3) and
      (LChild.Item(0).Field('[]').AsText = 'a"b\c{}[],'),
      'copied index and child outlive reassigned parent', Result);
    LSaved := NyxNull;
    LChild := NyxArray([]);
    Check(LLeaf.Copy.AsText = TNyxText('🌙') + NyxScalarText(0) + TNyxText('true'),
      'cached scalar is independent of released containers and never coerces text', Result);
    LRejected := False;
    try
      LLeaf.Count;
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'cached scalar cannot supply a container count', Result);
    LRejected := False;
    try
      LParent.Field('missing');
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'indexed missing object member refuses', Result);
    LRejected := False;
    try
      LChild.Item(0);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'indexed empty array rejects its end index', Result);
    LRejected := False;
    try
      LChild.Item(-1);
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'indexed array rejects a negative index', Result);
    SetLength(LNumbers, 1024);
    for LNumber := 0 to High(LNumbers) do
    begin
      LNumbers[LNumber] := NyxData(LNumber - 512);
    end;
    LParent := NyxArray(LNumbers);
    LSaved := LParent.Copy;
    LNumbers[512] := NyxData('replaced caller');
    LParent := NyxNull;
    Check((LSaved.Count = 1024) and (LSaved.Item(0).AsInteger = -512) and
      (LSaved.Item(512).AsInteger = 0) and (LSaved.Item(1023).AsInteger = 511),
      'wide copied array retains first/middle/final offsets and caller independence', Result);
    LNested := '{"inside":["[]{}\"\\",{"end":true}]}';
    for LNumber := 1 to 20 do
    begin
      LNested := '[' + LNested + ']';
    end;
    LChild := TNyxDataValue.ParseJSON(LNested);
    for LNumber := 1 to 20 do
    begin
      LChild := LChild.Item(0);
    end;
    Check(LChild.Field('inside').Item(1).Field('end').AsBoolean,
      'nested container spans ignore escaped structural characters', Result);
  end;

  { Shared private indexes may outlive any number of receiver replacements.
    Exercise self-copy, re-admission and every store handoff through public
    contracts; no test can repair or inspect the private arrays directly. }
  procedure CopiedSnapshots;
  var
    LOriginal: TNyxDataValue;
    LSaved: TNyxDataValue;
    LArray: TNyxDataValue;
    LLeaf: TNyxDataValue;
    LStore: TNyxExtensions;
    LClone: TNyxExtensions;
    LAssigned: TNyxExtensions;
    LOverlay: TNyxExtensions;
    LKey: TNyxExtensionRef;
    LWire: TNyxText;
    LName: TNyxText;
    LIndex: Integer;
    LRejected: Boolean;
    LStrict: Boolean;
  begin
    LName := TNyxText('moon🌙') + NyxScalarText(0);
    LOriginal := NyxObject([
      NyxField(LName, NyxArray([NyxData('original'),
        NyxData(NyxDecimal('9007199254740993.0000000000000001'))])),
      NyxField('', NyxData(True))]);
    LWire := LOriginal.ToJSON;
    LSaved := LOriginal.Copy;
    LArray := LOriginal.Field(LName).Copy;
    LLeaf := LArray.Item(1).Copy;
    for LIndex := 1 to 8 do
    begin
      LOriginal := LOriginal.Copy;
      LArray := LArray.Copy;
    end;
    Check((LOriginal.ToJSON = LWire) and (LOriginal.Key(0) = LName) and
      (LArray.Count = 2) and (LArray.Item(0).AsText = 'original'),
      'repeated object/array self-copy retains full published indexes', Result);
    LOriginal := TNyxDataValue.ParseJSON('{"replacement":[false]}');
    LArray := TNyxDataValue.ParseJSON('[null]');
    Check((LSaved.ToJSON = LWire) and (LSaved.Count = 2) and
      (LSaved.Field(LName).Item(0).AsText = 'original') and
      (LLeaf.AsDecimal.Text = '9007199254740993.0000000000000001'),
      're-admission cannot rewrite a retained index or detached exact scalar', Result);
    LOriginal := NyxObject([]).Copy;
    LArray := NyxArray([]).Copy;
    Check((LOriginal.Kind = ndObject) and (LOriginal.Count = 0) and
      (LArray.Kind = ndArray) and (LArray.Count = 0),
      'empty copied containers preserve distinct kinds and no stale index', Result);
    LLeaf := NyxData(TNyxText('🌙') + NyxScalarText(0)).Copy;
    LLeaf := LLeaf.Copy;
    Check((LLeaf.AsText = TNyxText('🌙') + NyxScalarText(0)) and
      NyxData(False).Copy.AsBoolean = False,
      'copied cached text and Boolean remain exact without container metadata', Result);
    Check((NyxNull.Copy.Kind = ndNull) and (NyxData(-17).Copy.AsInteger = -17),
      'copied null and integer retain their admitted scalar families', Result);
    LRejected := False;
    LOriginal := Default(TNyxDataValue);
    try
      LOriginal.Copy;
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'default record cannot become valid through Copy', Result);

    LKey := NyxExtension('app.copy-lifetime');
    LStore := TNyxExtensions.Create(nesNode);
    LClone := nil;
    LAssigned := TNyxExtensions.Create(nesNode);
    LOverlay := TNyxExtensions.Create(nesNode);
    try
      LStore.SetValue(LKey, LSaved);
      LClone := LStore.Clone;
      LAssigned.Assign(LStore);
      LOverlay.SetValue(NyxExtension('app.first'), NyxData(1));
      LOverlay.Overlay(LStore);
      LStore.Assign(LStore);
      Check(LStore.Value(LKey).ToJSON = LWire,
        'store self-assignment retains the complete immutable value', Result);
      LStore.SetValue(LKey, NyxArray([NyxNull]));
      LSaved := NyxNull;
      LStore.Free;
      LStore := nil;
      Check((LClone.Value(LKey).ToJSON = LWire) and
        (LAssigned.Value(LKey).Field(LName).Count = 2) and
        (LOverlay.Value(LKey).Field('').AsBoolean) and
        (LOverlay.Key(0).Name = 'app.first'),
        'cloned/assigned/overlaid indexes outlive replacement and store retirement', Result);
      LSaved := LClone.Value(LKey);
      LClone.LoadJSON('{"app.replaced":true}');
      LAssigned.Remove(LKey);
      LOverlay.SetValue(LKey, NyxData('replaced'));
      Check((LSaved.ToJSON = LWire) and
        (LSaved.Field(LName).Item(1).AsDecimal.Text =
          '9007199254740993.0000000000000001'),
        'reader snapshot outlives import/removal/overlay-value replacement', Result);
    finally
      LOverlay.Free;
      LAssigned.Free;
      LClone.Free;
      LStore.Free;
    end;

    { Strict JSON admits these raw soliduses at exactly four MiB, while its
      optional strict formatter expands each solidus into a two-byte escape.
      Copy must retain the established formatted-read refusal, not rebuild or
      accidentally mark an over-budget cached scalar as readable. }
    LStrict := TJSONString.StrictEscaping;
    try
      TJSONString.StrictEscaping := True;
      LOriginal := TNyxDataValue.ParseJSON('"' +
        TNyxText(StringOfChar('/', 1024)) +
        TNyxText(StringOfChar('a', NyxMaximumJSONBytes - 1026)) + '"');
    finally
      TJSONString.StrictEscaping := LStrict;
    end;
    LSaved := LOriginal.Copy;
    LOriginal := NyxNull;
    Check(Length(LSaved.ToJSON) > NyxMaximumJSONBytes,
      'copied scalar retains admitted formatted wire beyond read budget', Result);
    LRejected := False;
    try
      LSaved.AsText;
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'copy retains formatted four MiB scalar read refusal', Result);
    LSaved := NyxNull;
    try
      TJSONString.StrictEscaping := True;
      LOriginal := TNyxDataValue.ParseJSON('["' +
        TNyxText(StringOfChar('/', 1024)) +
        TNyxText(StringOfChar('a', NyxMaximumJSONBytes - 1028)) + '"]');
    finally
      TJSONString.StrictEscaping := LStrict;
    end;
    LSaved := LOriginal.Copy;
    LOriginal := NyxNull;
    LRejected := False;
    try
      LSaved.Count;
    except
      on ENyxJSON do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Length(LSaved.ToJSON) > NyxMaximumJSONBytes),
      'copy retains formatted container read refusal without an index', Result);
  end;
begin
  Result := 0;
  TypedTextValues(Result);
  TypedScalarValues(Result);
  IndexedScalarValues(Result);
  IndexedContainerValues(Result);
  IndexedReads;
  CopiedSnapshots;
  LReference := Default(TNyxExtensionRef);
  LValue := NyxData(TNyxText('🌙漢字') + NyxScalarText(0) + TNyxText('true'));
  Check((LValue.Kind = ndText) and
    (LValue.AsText = TNyxText('🌙漢字') + NyxScalarText(0) + TNyxText('true')),
    'exact Unicode/NUL text', Result);
  Check(NyxData('true').Kind = ndText, 'text never infers Boolean behavior', Result);
  Check(NyxData(True).AsBoolean and not NyxData(False).AsBoolean,
    'typed Boolean constructors', Result);
  Check((NyxData(Low(Integer)).AsInteger = Low(Integer)) and
    (NyxData(High(Integer)).AsInteger = High(Integer)), 'signed integer boundaries', Result);
  Check(NyxData(0.125).AsNumber = 0.125, 'finite Double constructor', Result);
  Check(NyxNull.Kind = ndNull, 'explicit null value', Result);
  LDecimal := NyxDecimal('9007199254740993');
  Check(NyxData(LDecimal).ToJSON = '9007199254740993',
    'large exact decimal is never stored as approximate Double', Result);
  Check(NyxData(NyxDecimal('1.234567890123456789')).AsDecimal.Text =
    '1.234567890123456789', 'sub-mantissa decimal digits survive', Result);
  Check(NyxData(NyxDecimal('-0.000E+00')).ToJSON = '-0.000E+00',
    'signed zero/exponent spelling survives', Result);
  LRejected := False;
  try
    NyxData(NyxDecimal('1.0000000000000000001')).AsInteger;
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'integer conversion cannot round away fractional digits', Result);
  LRejected := False;
  try
    NyxData('false').AsBoolean;
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'wrong-kind reads reject instead of coercing text', Result);

  SetLength(LItems, 2);
  LItems[0] := LValue.Copy;
  LItems[1] := NyxNull;
  LSnapshot := NyxArray(LItems);
  LItems[0] := NyxData('changed caller array');
  Check((LSnapshot.Count = 2) and (LSnapshot.Item(0).AsText = LValue.AsText),
    'array snapshot does not borrow caller records', Result);
  SetLength(LFields, 2);
  LFields[0] := NyxField('caption', LValue);
  LFields[1] := NyxField('nested', LSnapshot);
  LSnapshot := NyxObject(LFields);
  LFields[0].Value := NyxData('changed caller field');
  Check((LSnapshot.Key(0) = 'caption') and
    (LSnapshot.Field('caption').AsText = LValue.AsText),
    'object snapshot does not borrow caller field records', Result);
  Check(LSnapshot.Field('nested').Item(1).Kind = ndNull,
    'nested object/array typed lookup', Result);
  Check((NyxObject([]).Count = 0) and (NyxArray([]).Count = 0),
    'empty structures retain distinct types', Result);
  RejectData('{"same":1,"\u0073ame":2}', Result);
  RejectData('{"x":[1,]}', Result);
  RejectData('1e-324', Result);
  RejectData('1e309', Result);

  { fpjson trees are still ordinary caller-owned mutable trees. Decimal Clone
    must preserve the spelling, while a deliberate numeric write must serialize
    its new value instead of a stale opaque representation. }
  LJSON := DecodeNyxJSON('{"id":9007199254740993,"fraction":1.234567890123456789}');
  try
    LJSONCopy := LJSON.Clone;
    try
      Check(LJSON.AsJSON = LJSONCopy.AsJSON, 'JSON number Clone retains exact decimal', Result);
      TJSONObject(LJSONCopy).Find('id').AsFloat := 17;
      LValue := TNyxDataValue.ParseJSON(TJSONObject(LJSONCopy).Find('id').AsJSON);
      Check((LValue.AsNumber = 17) and
        (TJSONObject(LJSON).Find('id').AsJSON = '9007199254740993'),
        'fpjson numeric mutation has independent current serialization', Result);
      TJSONObject(LJSONCopy).Find('fraction').AsFloat :=
        TJSONObject(LJSONCopy).Find('fraction').AsFloat;
      Check(TJSONObject(LJSONCopy).Find('fraction').AsJSON <>
        '1.234567890123456789', 'explicit approximate write replaces original precision', Result);
      TJSONObject(LJSONCopy).Find('id').AsString := '2.0';
      Check(TNyxDataValue.ParseJSON(TJSONObject(LJSONCopy).Find('id').AsJSON).AsNumber = 2,
        'fpjson string setter discards obsolete spelling', Result);
      TJSONObject(LJSONCopy).Find('id').AsBoolean := True;
      Check(TNyxDataValue.ParseJSON(TJSONObject(LJSONCopy).Find('id').AsJSON).AsNumber = 1,
        'fpjson Boolean setter discards obsolete spelling', Result);
      TJSONObject(LJSONCopy).Find('id').Clear;
      Check(TNyxDataValue.ParseJSON(TJSONObject(LJSONCopy).Find('id').AsJSON).AsNumber = 0,
        'fpjson clear discards obsolete spelling', Result);
    finally
      LJSONCopy.Free;
    end;
  finally
    LJSON.Free;
  end;

  LStore := TNyxExtensions.Create(nesDocument);
  LForeign := TNyxExtensions.Create(nesNode);
  try
    LStore.SetValue(NyxExtension('app.settings'), LSnapshot);
    LStore.SetValue(NyxExtension(''), NyxData(True));
    LStore.SetValue(NyxExtension(TNyxText('🌙=') + NyxScalarText(0)), NyxData(7));
    Check((LStore.Count = 3) and LStore.Value(NyxExtension('')).AsBoolean and
      (LStore.Value(NyxExtension(TNyxText('🌙=') + NyxScalarText(0))).AsInteger = 7),
      'opaque key identity preserves empty/equal/NUL/Unicode names', Result);
    LValue := LStore.Value(NyxExtension('app.settings'));
    LValue := NyxData('reassigned reader snapshot');
    Check(LStore.Value(NyxExtension('app.settings')).Kind = ndObject,
      'read snapshots cannot mutate the owned store', Result);
    LCopy := LStore.Clone;
    try
      LCopy.SetValue(NyxExtension('app.settings'), NyxData('copy'));
      Check(LStore.Value(NyxExtension('app.settings')).Kind = ndObject,
        'store clone mutations remain independent', Result);
      LCopy.Remove(NyxExtension(''));
      Check((LCopy.Count = 2) and (LStore.Count = 3),
        'clone removal preserves accepted ordering and membership', Result);
    finally
      LCopy.Free;
    end;
    LBaseline := LStore.ToJSON;
    LRejected := False;
    try
      LStore.SetValue(NyxExtension('state'), NyxNull);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.ToJSON = LBaseline),
      'root extensions cannot replace a standard state field', Result);
    LRejected := False;
    try
      LForeign.SetValue(NyxExtension('kind'), NyxData('button'));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LForeign.Count = 0), 'node fields are protected separately', Result);
    LForeign.SetValue(NyxExtension('state'), NyxNull);
    Check(LForeign.Count = 1, 'names protected only in their owner scope', Result);
    LRejected := False;
    try
      LStore.Assign(LForeign);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.ToJSON = LBaseline), 'cross-scope assignment is atomic', Result);
    LRejected := False;
    try
      LStore.SetValue(LReference, NyxNull);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.ToJSON = LBaseline), 'default reference requires construction', Result);
    LRejected := False;
    try
      LStore.LoadJSON('{"valid":true,"pages":[]}');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.ToJSON = LBaseline), 'invalid multi-field import preserves store', Result);
    LRejected := False;
    try
      LStore.LoadJSON('{"duplicate":true,"\u0064uplicate":false}');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.ToJSON = LBaseline), 'duplicate import preserves accepted store', Result);

    LParts := TNyxStrings.Create;
    try
      for LIndex := 0 to NyxMaximumJSONMembers - 5 do
      begin
        LParts.Add('"extra-' + IntToStr(LIndex) + '":null');
      end;
      LRejected := False;
      try
        LStore.LoadJSON('{' + LParts.Join(',') + '}');
      except
        on LException: Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LStore.ToJSON = LBaseline),
        'owner budget reserves all standard fields before publication', Result);
    finally
      LParts.Free;
    end;
    LStore.LoadJSON('{}');
    LStore.SetValue(NyxExtension('first'), NyxData(TNyxText(
      StringOfChar('a', NyxMaximumJSONBytes div 2))));
    LBaseline := LStore.ToJSON;
    LRejected := False;
    try
      LStore.SetValue(NyxExtension('second'), NyxData(TNyxText(
        StringOfChar('b', NyxMaximumJSONBytes div 2))));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.ToJSON = LBaseline),
      'aggregate payload byte budget is atomic, not just a per-value check', Result);
  finally
    LForeign.Free;
    LStore.Free;
  end;

  { Unfamiliar root/node fields are imported into the explicit extension store.
    Known fields remain strict and have their original version-1 shape. }
  LDocument := TNyxCodec.Decode(
    '{"version":1,"title":"Opaque","pages":[{"kind":"page","id":"p","props":{},' +
    '"children":[],"custom-node":{"number":9007199254740993,"text":"\u0000🌙"}}],' +
    '"components":[],"custom-root":{"enabled":true,"nothing":null,"rows":[1.234567890123456789]}}');
  try
    Check(LDocument.Extensions.Value(NyxExtension('custom-root')).Field('enabled').AsBoolean,
      'unknown root field is retained as typed data', Result);
    Check(LDocument.Pages[0].Extensions.Value(NyxExtension('custom-node')).Field('text').AsText =
      NyxScalarText(0) + TNyxText('🌙'), 'unknown node field retains NUL and Unicode', Result);
    LBaseline := TNyxCodec.Encode(LDocument);
    LClone := TNyxCodec.Decode(LBaseline);
    try
      Check(TNyxCodec.Encode(LClone) = LBaseline, 'canonical design extension round trip is stable', Result);
      LClone.Extensions.SetValue(NyxExtension('custom-root'), NyxNull);
      LClone.Pages[0].Extensions.SetValue(NyxExtension('custom-node'), NyxData(False));
      Check((LDocument.Extensions.Value(NyxExtension('custom-root')).Kind = ndObject) and
        (LDocument.Pages[0].Extensions.Value(NyxExtension('custom-node')).Kind = ndObject),
        'decoded data mutation does not alias its source document', Result);
    finally
      LClone.Free;
    end;
    LClone := LDocument.Clone;
    try
      LClone.Extensions.Remove(NyxExtension('custom-root'));
      Check(LDocument.Extensions.Count = 1, 'document Clone owns independent extension entries', Result);
    finally
      LClone.Free;
    end;
    LView := CloneNyxViewDocument(LDocument, LDocument.Pages[0]);
    try
      Check((LView.Extensions.ToJSON = LDocument.Extensions.ToJSON) and
        (LView.Pages[0].Extensions.ToJSON = LDocument.Pages[0].Extensions.ToJSON),
        'isolated view carries exact project and node data', Result);
    finally
      LView.Free;
    end;
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NyxObject([', LSource) > 0) and (Pos('NyxArray([', LSource) > 0) and
      (Pos('NyxDecimal(''9007199254740993'')', LSource) > 0) and
      (Pos('ParseJSON', LSource) = 0),
      'generator uses typed readable nested constructors instead of JSON blobs', Result);
  finally
    LDocument.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDefinition := TNyxNode.Create(nkColumn, 'definition');
    LDocument.AddComponent(LDefinition);
    LNode := TNyxNode.Create(nkLabel, 'caption');
    LDefinition.Add(LNode);
    LNode.Configure.PartName(NyxPart('caption')).Text('Shared caption');
    LDefinition.Extensions.SetValue(NyxExtension('presentation'), NyxObject([
      NyxField('theme', NyxData('shared')), NyxField('keep', NyxData(True))]));
    LNode.Extensions.SetValue(NyxExtension('validation'), NyxData('shared'));
    LNode := TNyxNode.Create(nkComponent, 'one');
    LDocument.AddPage(LNode);
    LNode.Configure.Component(NyxComponent('definition'));
    LNode.Extensions.SetValue(NyxExtension('presentation'), NyxObject([
      NyxField('theme', NyxData('one'))]));
    LNode.OverridePart('caption').Extensions.SetValue(NyxExtension('validation'), NyxData('one'));
    LSibling := TNyxNode.Create(nkComponent, 'two');
    LDocument.AddPage(LSibling);
    LSibling.Configure.Component(NyxComponent('definition'));
    LRuntime := RealizeNyxView(LDocument, LNode);
    try
      Check((LRuntime.Extensions.Value(NyxExtension('presentation')).Count = 1) and
        (LRuntime.Extensions.Value(NyxExtension('presentation')).Field('theme').AsText = 'one'),
        'instance overlay replaces whole values without implicit nested merging', Result);
      Check(LRuntime.Part('caption').Extensions.Value(NyxExtension('validation')).AsText = 'one',
        'part override has independent typed extension data', Result);
      LRuntime.Part('caption').Extensions.SetValue(NyxExtension('validation'), NyxData('runtime'));
      Check((LNode.Children[0].Extensions.Value(NyxExtension('validation')).AsText = 'one') and
        (LDefinition.Part('caption').Extensions.Value(NyxExtension('validation')).AsText = 'shared'),
        'runtime data edits preserve template and authored instance', Result);
    finally
      LRuntime.Free;
    end;
    LRuntime := RealizeNyxView(LDocument, LSibling);
    try
      Check((LRuntime.Extensions.Value(NyxExtension('presentation')).Count = 2) and
        (LRuntime.Part('caption').Extensions.Value(NyxExtension('validation')).AsText = 'shared'),
        'sibling retains all definition data', Result);
    finally
      LRuntime.Free;
    end;
  finally
    LDocument.Free;
  end;

  LSession := TNyxStudioSession.Create;
  try
    LBaseline := LSession.Save;
    LSession.SetExtension(seoDocument, NyxExtension('project.settings'), NyxData(True));
    LChanged := LSession.Save;
    Check(LChanged <> LBaseline, 'project extension command creates history', Result);
    LSession.Undo;
    Check(LSession.Save = LBaseline, 'project data undo restores exact design', Result);
    LAccepted := LSession.Document;
    LSession.RemoveExtension(seoDocument, NyxExtension('absent'));
    Check(LSession.Document = LAccepted, 'absent removal preserves accepted document handles', Result);
    LRejected := False;
    try
      LSession.SetExtension(seoDocument, NyxExtension('version'), NyxData(2));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted),
      'rejected command preserves document handles', Result);
    LSource := 'null';
    for LIndex := 1 to NyxMaximumJSONDepth do
    begin
      LSource := '[' + LSource + ']';
    end;
    LValue := TNyxDataValue.ParseJSON(LSource);
    LSession.Select('project-name');
    LRejected := False;
    try
      LSession.SetExtension(seoSelection, NyxExtension('too.deep'), LValue);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted),
      'whole-design nesting budget rejects a payload admitted in isolation', Result);
    LRejected := False;
    try
      LSession.Load('{"version":1,"title":"","pages":[],"components":[],' +
        '"custom":{"duplicate":1,"\u0064uplicate":2}}');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted),
      'invalid opaque import preserves accepted document/history', Result);
    LSession.Redo;
    Check(LSession.Save = LChanged, 'no-op/rejected extension commands and imports preserve redo', Result);
    LAccepted := LSession.Document;
    LSession.SetExtension(seoDocument, NyxExtension('project.settings'), NyxData(True));
    Check(LSession.Document = LAccepted, 'same-value command is a no-op', Result);
    LSession.Select('project-name');
    LSession.SetExtension(seoSelection, NyxExtension('control.settings'), NyxData(7));
    Check(LSession.Selected.Extensions.Value(NyxExtension('control.settings')).AsInteger = 7,
      'selected authored control receives an undoable typed value', Result);
    LSession.RemoveExtension(seoSelection, NyxExtension('control.settings'));
    Check(not LSession.Selected.Extensions.Has(NyxExtension('control.settings')),
      'selected extension removal', Result);
    LSession.Undo;
    Check(LSession.Selected.Extensions.Value(NyxExtension('control.settings')).AsInteger = 7,
      'selected removal undo restores typed data and identity', Result);
    Check(Pos('NyxExtension(''project.settings'')', LSession.Source) > 0,
      'portable session source preserves opaque project data', Result);
    LSession.SetProperty(NyxAttributeName(atHint), 'A useful control hint');
    Check(LSession.Document.Extensions.Value(NyxExtension('project.settings')).AsBoolean and
      (LSession.Selected.Extensions.Value(NyxExtension('control.settings')).AsInteger = 7),
      'ordinary Studio property edits retain project and node extension data', Result);
    LSession.Undo;
    Check((LSession.Selected.Prop(NyxAttributeName(atHint)) = '') and
      LSession.Document.Extensions.Value(NyxExtension('project.settings')).AsBoolean and
      (LSession.Selected.Extensions.Value(NyxExtension('control.settings')).AsInteger = 7),
      'ordinary property undo retains exact opaque data', Result);
  finally
    LSession.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := TNyxText(StringOfChar('t', NyxMaximumJSONBytes div 2));
    LDocument.Extensions.SetValue(NyxExtension('large'), NyxData(TNyxText(
      StringOfChar('e', NyxMaximumJSONBytes div 2))));
    LRejected := False;
    try
      TNyxCodec.Encode(LDocument);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'export refuses a combined design that cannot be imported', Result);
  finally
    LDocument.Free;
  end;
end;

end.
