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

program nyx_project_opening_wire_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.data, nyx.model, nyx.source, nyx.pascal.project,
  nyx.source.preparation, nyx.studio.projects, nyx.studio.session,
  nyx.studio.sourcebuilds, nyx.studio.projectopening.shared;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Project Open wire: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Edit protocol data only, retaining every unrelated member and exact scalar.
  Product admission receives the complete result through its ordinary reader. }
function Field(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LFound: Boolean;
begin
  LCount := AObject.Count;
  SetLength(LFields, LCount + 1);
  LFound := False;
  for LIndex := 0 to LCount - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if AObject.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    LFields[LCount] := NyxField(AName, AValue);
    Inc(LCount);
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields);
end;

procedure Refuse(const AArguments: TNyxDataValue; const AReason: TNyxText);
var
  LPair: TNyxProjectPair;
  LResolution: TNyxProjectResolution;
  LRefused: Boolean;
begin
  LRefused := False;
  try
    ReadNyxProjectOpening(AArguments, LPair, LResolution);
  except
    on LException: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

procedure Run;
const
  CHelper: TNyxText = 'unit opening.helper;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + 'interface implementation end.';
var
  LSession: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LRead: TNyxProjectPair;
  LResolution: TNyxProjectResolution;
  LBase: TNyxDataValue;
  LArguments: TNyxDataValue;
  LOpening: TNyxDataValue;
  LProject: INyxPascalProject;
  LChangedProject: INyxPascalProject;
  LRefused: Boolean;
  LOriginal: TNyxText;
  LContext: TNyxStudioCommandContext;
begin
  LSession := TNyxStudioSession.Create;
  try
    LSession.SetTitle('A project with a moon 🌙');
    LOriginal := EncodeNyxProject(LSession.ProjectSnapshot);
    LPair := LSession.ProjectSnapshot;
    LPair.Pending := True;
    LPair.Draft := LPair.Source + #10 + TNyxText('{ Unfinished thought 🌙 }');
    LPair.DraftBase := LPair.Source + #10 + '{ Older base. }';
    LBase := NyxObject([
      NyxField('mode', NyxData('request')), NyxField('expectedRevision', NyxData(1)),
      NyxField('operationId', NyxData('project-open-fixture')),
      NyxField('source', NyxData(LPair.Source)), NyxField('publish', NyxData(True)),
      NyxField('issuer', NyxData('fixture-only-issuer'))]);
    LRead := LPair;
    Check(not ReadNyxProjectOpening(LBase, LRead, LResolution) and
      (LRead.Source = '') and not LRead.Pending,
      'absent extension retains old meaning and clears caller output values');
    LArguments := WithNyxProjectOpening(LBase, LPair, nprRequireMatch);
    Check(ReadNyxProjectOpening(LArguments, LRead, LResolution) and
      (EncodeNyxProject(LRead) = EncodeNyxProject(LPair)) and
      (LResolution = nprRequireMatch), 'complete Unicode pair/draft/base survive the wire');
    Check(ReadNyxProjectOpening(WithNyxProjectOpening(LBase, LPair, nprUsePascal),
      LRead, LResolution) and (LResolution = nprUsePascal),
      'explicit Pascal resolution remains a distinct closed choice');
    LOpening := LArguments.Field('opening');
    Refuse(Field(LArguments, 'opening', NyxNull), 'present null intent is not an absent extension');
    Refuse(Field(LArguments, 'opening', Field(LOpening, 'version', NyxData(2))),
      'unknown opening version refuses');
    Refuse(Field(LArguments, 'opening', Field(LOpening, 'resolution', NyxData('future'))),
      'unknown resolution refuses');
    Refuse(Field(LArguments, 'opening', Field(LOpening, 'executed', NyxData(True))),
      'serialized execution claim refuses rather than granting trust');
    Refuse(Field(LArguments, 'opening', Field(LOpening, 'project', NyxData('{}'))),
      'malformed imported project refuses');
    Refuse(Field(LArguments, 'publish', NyxData(False)), 'compile-only cannot claim project publication');
    Refuse(Field(LArguments, 'source', NyxData(LPair.Source + #10)),
      'substituted constructor bytes refuse before any execution');
    Refuse(Field(LArguments, 'source', NyxNull), 'missing typed source refuses');
    Refuse(Field(LArguments, 'visual', NyxObject([])), 'mixed visual and Open intent refuses');
    Refuse(Field(LArguments, 'sourceFile', NyxObject([])), 'mixed selected-file and Open intent refuses');
    LProject := NewNyxPascalProject(NyxPascalUnit(NyxCompanionUnitName(LPair.Source)),
      LPair.Source).WithUnit(NyxPascalUnit('opening.helper'), CHelper);
    LPair.Files := TNyxPascalProjectFiles.Capture(LProject);
    LArguments := WithNyxProjectOpening(WithNyxSourceProjectFiles(LBase, LPair.Files),
      LPair, nprRequireMatch);
    Check(ReadNyxProjectOpening(LArguments, LRead, LResolution) and
      (LRead.Files.Encoded = LPair.Files.Encoded), 'complete supporting files stay bound to the same inputs');
    Refuse(Field(LBase, 'opening', LArguments.Field('opening')),
      'absent required helper inventory refuses');
    Refuse(Field(LArguments, 'pascalFiles', NyxNull), 'null helper inventory refuses');
    LChangedProject := LProject.WithUnit(NyxPascalUnit('opening.helper'), CHelper + #10);
    Refuse(Field(LArguments, 'pascalFiles', TNyxDataValue.ParseJSON(
      TNyxPascalProjectFiles.Capture(LChangedProject).Encoded)),
      'substituted helper bytes refuse');
    LRefused := False;
    try
      WithNyxProjectOpening(LBase, LPair, nprUseDesign);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'design-only resolution cannot start a constructor path');
    Check(not NyxSharedProjectOpeningAvailable(nil), 'missing optional host reports no Open capability');
    LRefused := False;
    try
      StartNyxSharedProjectOpening(nil, Default(TNyxStudioProjectRequest), nil);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'unsupported host refuses before starting a compiler or retaining a port');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LOriginal,
      'all wire checks preserve the complete exact live project');
    LPair := LSession.ProjectSnapshot;
    LPair.Pending := True;
    LPair.Draft := LPair.Source + #10 + TNyxText('{ A literal imported draft 🌙 }');
    LPair.DraftBase := LPair.Source;
    LContext := LSession.CommandContext;
    LSession.OpenProject(LPair);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'ordinary literal Open retains its independent imported draft');
    Check(not LSession.MatchesCommandContext(LContext),
      'ordinary Open retires input captured in the previous load');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LOriginal,
      'one literal Open Undo restores the complete prior pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'one literal Open Redo restores the imported pair and draft');
  finally
    LSession.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' project Open wire checks';
    {$else}
    WriteLn('PASS ', GChecks, ' project Open wire checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
