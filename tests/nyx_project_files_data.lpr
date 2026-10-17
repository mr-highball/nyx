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
program nyx_project_files_data;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.source,
  nyx.pascal.project, nyx.studio.projects, nyx.test.projectfiles
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
const
  CMain: TNyxText = 'unit Sample.Main;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface' + #10 + 'implementation' + #10 + '{$I values.inc}' + #10 + 'end.' + #10;
  CBody: TNyxText = '{ A rocket 🚀, a letter 𐐷, and é. }' + #10 +
    'const Caption = ''A thoughtful plan'';' + #10;
  CBase: TNyxText = 'Older baseline 🚀';
var
  LInputs: INyxPascalProject;
  LOpened: INyxPascalProject;
  LFiles: TNyxPascalProjectFiles;
  LParsed: TNyxPascalProjectFiles;
  LPair: TNyxProjectPair;
  LPacket: TNyxText;
  LLegacy: TNyxText;
  LFields: TNyxDataValue;
  LItems: TNyxDataValue;
  LRefused: Boolean;

  function Inventory(const ARole, AName, AText: TNyxText): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('kind', NyxData(ARole)),
      NyxField('name', NyxData(AName)),
      NyxField('text', NyxData(AText))]);
  end;

  function Envelope(const AItems: TNyxDataValue; ACount: Integer = 128): TNyxDataValue;
  begin
    Result := NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('main', NyxData('Sample.Main')),
      NyxField('maximumFiles', NyxData(ACount)),
      NyxField('maximumBytes', NyxData(32 * 1024 * 1024)),
      NyxField('files', AItems)]);
  end;

  procedure Refuses(const AData: TNyxDataValue; const AReason: String);
  begin
    LRefused := False;
    try
      LParsed := TNyxPascalProjectFiles.Parse(AData.ToJSON, CMain);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, AReason);
    Check(LFiles.Open(CMain).Input(0).Text = CBody,
      'malformed input leaves the earlier immutable value exact');
  end;

begin
  LFiles := Default(TNyxPascalProjectFiles);
  Check(LFiles.IsEmpty and (LFiles.StorageBytes = 0), 'default helper value is absent');
  Check(LFiles.Open('') = nil, 'absent data grants no project meaning');
  LInputs := NewNyxPascalProject(NyxPascalUnit('Sample.Main'), CMain)
    .WithInclude(NyxPascalInclude('values.inc'), CBody);
  LFiles := TNyxPascalProjectFiles.Capture(LInputs);
  LInputs := nil;
  Check(not LFiles.IsEmpty, 'captured helper inventory survives original project disposal');
  LOpened := LFiles.Open(CMain);
  Check((LOpened.Count = 1) and (LOpened.Source = CMain), 'Open reconstructs the entire input');
  Check(LOpened.Input(0).Text = CBody, 'supplementary and combining source text remains exact');
  LParsed := TNyxPascalProjectFiles.Parse(LFiles.Encoded, CMain);
  Check(LParsed.Encoded = LFiles.Encoded, 'data encoding and ordering are deterministic');
  LOpened := LOpened.WithInclude(NyxPascalInclude('values.inc'), 'const Caption = 2;');
  Check(LFiles.Open(CMain).Input(0).Text = CBody, 'later authored values never alias a checkpoint');

  LItems := NyxArray([Inventory('external', 'values.inc', CBody)]);
  Refuses(Envelope(LItems), 'unknown file roles refuse');
  Refuses(Envelope(NyxArray([Inventory('include', '../values.inc', CBody)])),
    'host paths cannot enter supporting input');
  Refuses(Envelope(NyxArray([Inventory('include', 'values.inc', CBody),
    Inventory('unit', 'Wrong.pas',
    'unit Another; {$mode delphi}{$codepage utf8} interface implementation end.')])),
    'declared helper namespace must match its typed leaf');
  Refuses(Envelope(NyxArray([Inventory('include', 'values.inc', CBody),
    Inventory('include', 'values.inc', CBody)])), 'duplicate identities cannot replace during decode');
  Refuses(Envelope(NyxArray([Inventory('include', 'values.inc', CBody),
    Inventory('include', 'VALUES.inc', CBody)])), 'case aliases refuse during decode');
  Refuses(Envelope(NyxArray([Inventory('include', 'values.inc', CBody)]), 1),
    'decoded file count observes its captured limits');
  Refuses(Envelope(NyxArray([Inventory('include', 'values.inc',
    '{$I missing.inc}')])), 'nested includes require actual captured membership');
  Refuses(Envelope(NyxArray([])), 'main include cannot disappear during decoding');
  Refuses(NyxObject([NyxField('version', NyxData(99))]), 'unknown file envelopes refuse');

  LPair := NyxProjectPair('{}', CMain);
  LLegacy := NyxObject([
    NyxField('version', NyxData(1)), NyxField('design', NyxData('{}')),
    NyxField('source', NyxData(CMain)), NyxField('draft', NyxData('')),
    NyxField('draftBase', NyxData('')), NyxField('pending', NyxData(False))]).ToJSON;
  Check(EncodeNyxProject(LPair) = LLegacy, 'legacy six-field packet bytes remain exact');
  LPair.Files := LFiles;
  LPair.Pending := True;
  LPair.Draft := '';
  LPair.DraftBase := CBase;
  LPacket := EncodeNyxProject(LPair);
  LPair := DecodeNyxProject(LPacket);
  Check(EncodeNyxProject(LPair) = LPacket, 'new complete packet round trip remains exact');
  Check(LPair.Pending and (LPair.Draft = '') and (LPair.DraftBase = CBase),
    'an empty independent pending draft remains distinguishable from absence');
  Check(LPair.Files.Open(LPair.Source).Input(0).Text = CBody,
    'project export retains complete helper text');
  LFields := TNyxDataValue.ParseJSON(LPacket);
  Check((LFields.Count = 7) and (LFields.Field('version').AsInteger = 2),
    'new member is explicit in version two');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-project-files-data', 'passed');
    document.body.setAttribute('data-project-files-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' supporting-file data and legacy packet checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-project-files-data', 'failed');
      document.body.setAttribute('data-project-files-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
