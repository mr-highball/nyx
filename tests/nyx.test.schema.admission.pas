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

unit nyx.test.schema.admission;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Public behavior qualification for fresh property admission and complete owned
  inspector snapshots. The same fixture is compiled into native and browser
  source/editor suites; implementation caches or allocation counts are not API. }
function RunNyxSchemaAdmissionTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.schema,
  nyx.contract;

function PropertyAt(const AInfos: TNyxPropertyInfos;
  const AKey: TNyxText): TNyxPropertyInfo;
var
  LInfo: TNyxPropertyInfo;
begin
  for LInfo in AInfos do
  begin

    if LInfo.Key = AKey then
    begin
      Exit(LInfo);
    end;
  end;
  raise ENyxModel.Create('Missing property fixture: ' + AKey);
end;

function RunNyxSchemaAdmissionTests: Integer;
var
  LDocument: TNyxDocument;
  LDefinition: TNyxNode;
  LInstance: TNyxNode;
  LCustom: TNyxNode;
  LInfos: TNyxPropertyInfos;
  LPublished: TNyxPropertyInfos;
  LInfo: TNyxPropertyInfo;
  LIndex: Integer;
  LPlatform: TNyxPlatform;
  LKey: TNyxText;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Property admission: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Reject(ANode: TNyxNode; const AKey, AValue: TNyxText);
  var
    LCandidate: TNyxNode;
  begin
    LCandidate := ANode.Clone;
    try
      { Invalid text belongs at this explicit wire admission boundary. Typed
        authoring deliberately cannot construct these malformed arguments. }
      LCandidate.SetProp(AKey, AValue);
      LRejected := False;
      try
        ValidateNyxProperties(LCandidate, LDocument);
      except
        on LException: ENyxModel do
        begin
          LRejected := (Pos(AKey, LException.Message) > 0) and
            (Pos(ANode.ID, LException.Message) > 0);
        end;
      end;
      Check(LRejected, 'reject current ' + AKey);
    finally
      LCandidate.Free;
    end;
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LCustom := nil;
  try
    LDefinition := TNyxNode.Create(nkColumn, 'amount-definition');
    LDefinition.Contract.Value(NyxIntegerDomain.Range(1, 5));
    LDefinition.Configure.Compound(True).Value(2).Done;
    LDocument.AddComponent(LDefinition);
    LInstance := TNyxNode.Create(nkComponent, 'amount-instance');
    LInstance.Configure.Component(NyxComponent('amount-definition')).Done;
    LDocument.AddPage(LInstance);
    LInfos := NyxProperties(LInstance, LDocument);
    LInfo := PropertyAt(LInfos, 'value');
    Check((LInfo.ValueType = npInteger) and (LInfo.Minimum = 1) and
      (LInfo.Maximum = 5) and (LInfo.DefaultValue = '2'),
      'fresh reusable domain and inherited default');
    Reject(LInstance, 'value', '6');
    LDefinition.Contract.Value(NyxIntegerDomain.Range(8, 12));
    LDefinition.Configure.Value(9).Done;
    LInfo := PropertyAt(NyxProperties(LInstance, LDocument), 'value');
    Check((LInfo.Minimum = 8) and (LInfo.Maximum = 12) and
      (LInfo.DefaultValue = '9'), 'direct definition edits replace current facts');
    Reject(LInstance, 'value', '2');
    LInstance.Configure.Value(11).Done;
    ValidateNyxDocumentProperties(LDocument);
    Check(LInfos[0].Support.Defined and (PropertyAt(LInfos, 'value').DefaultValue = '2'),
      'previous owned snapshot survives definition admission');
    LInfos[0].Key := 'caller-changed';
    Check(NyxProperties(LInstance, LDocument)[0].Key <> 'caller-changed',
      'returned array cannot change a later query');

    { A large creator schema crosses ordinary built-in descriptor capacities.
      Replacing a built-in key must also replace its platform-scoped admission. }
    SetLength(LPublished, 132);
    for LIndex := 0 to 129 do
    begin
      LPublished[LIndex] := Default(TNyxPropertyInfo);
      LPublished[LIndex].Key := 'creator-value-' + IntToStr(LIndex);
      LPublished[LIndex].Title := 'Creator value ' + IntToStr(LIndex);
      LPublished[LIndex].ValueType := npInteger;
      LPublished[LIndex].DefaultValue := '1';
      LPublished[LIndex].Minimum := 1;
      LPublished[LIndex].Maximum := 7;
    end;
    LPublished[130] := Default(TNyxPropertyInfo);
    LPublished[130].Key := 'width';
    LPublished[130].Title := 'Creator width';
    LPublished[130].ValueType := npChoice;
    LPublished[130].DefaultValue := 'compact';
    LPublished[130].Choices := 'compact' + #10 + 'wide';
    LPublished[130].Support := NyxPropertySupport(npmCustom, ncCustom, ncCustom,
      'The creator selects a named width profile.');
    LPublished[131] := Default(TNyxPropertyInfo);
    LPublished[131].Key := 'creator-text';
    LPublished[131].Title := 'Creator text';
    LPublished[131].ValueType := npLines;
    LPublished[131].DefaultValue := TNyxText('Exact / 🌙 / 漢字') + #10 + 'Second line';
    RegisterNyxSchema(NyxCustomKind('property-admission-fixture'), LPublished, []);
    LPublished[0].Maximum := 100;
    LCustom := TNyxNode.Create(NyxCustomKind('property-admission-fixture'), 'creator');
    LCustom.Configure.ProjectAs(nkMemo).Done;
    LInfos := NyxProperties(LCustom);
    LInfo := PropertyAt(LInfos, 'creator-value-129');
    Check((LInfo.Minimum = 1) and (LInfo.Maximum = 7) and
      (LInfo.Title = 'Creator value 129') and LInfo.Support.Defined,
      'large creator metadata retains its last field and help');
    LInfo := PropertyAt(LInfos, 'creator-text');
    Check(LInfo.DefaultValue = TNyxText('Exact / 🌙 / 漢字') + #10 + 'Second line',
      'Unicode multiline defaults remain exact');
    ValidateNyxProperties(LCustom);
    Reject(LCustom, 'creator-value-0', '8');
    Reject(LCustom, 'creator-value-64', '0');
    Reject(LCustom, 'creator-value-129', '1.5');
    Reject(LCustom, 'creator-text', 'Bad' + #0 + 'text');
    Reject(LCustom, 'width', '12');
    for LPlatform := npfBrowser to npfNativeLCL do
    begin
      LKey := NyxPlatformKey(LPlatform, atWidth);
      LCustom.SetProp(LKey, 'wide');
      ValidateNyxProperties(LCustom);
      LInfo := PropertyAt(NyxProperties(LCustom), LKey);
      Check((LInfo.ValueType = npChoice) and (LInfo.Choices = 'compact' + #10 + 'wide') and
        (LInfo.DefaultValue = '') and LInfo.Advanced and LInfo.Support.Defined,
        'scoped creator replacement retains exact type and presentation');
      Reject(LCustom, LKey, '12');
      LCustom.Props.Delete(LCustom.Props.IndexOfName(LKey));
      LInfos := NyxProperties(LCustom);
      LRejected := False;
      try
        PropertyAt(LInfos, LKey);
      except
        on ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'removed scope is absent in the next fresh query');
    end;
    LInfo := PropertyAt(NyxProperties(LCustom), 'width');
    Check((LInfo.Title = 'Creator width') and
      (LInfo.Support.Description = 'The creator selects a named width profile.'),
      'ordinary creator help survives scoped queries');
  finally
    LCustom.Free;
    LDocument.Free;
  end;
end;

end.
