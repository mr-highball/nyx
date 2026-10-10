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

unit nyx.studio.sourceconfiguration;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

type
  { Copied machine settings, never document properties or execution authority.
    Empty paths are valid hints while editing. Fluent changes return independent
    records; creating/decoding these values does no I/O and starts no compiler. }
  TNyxLocalSourceSettings = record
  private
    FLibraryRoot: TNyxText;
    FRuntimeRoot: TNyxText;
    FCompilerPath: TNyxText;
  public
    class function Empty: TNyxLocalSourceSettings; static;
    { Library source folder containing src/studio. This stores a hint only;
      the executing host checks layout/readiness when explicitly configured. }
    function WithLibrary(const APath: TNyxText): TNyxLocalSourceSettings;
    { Independent writable compiler workspace. The host's invocation owner
      admits fresh children; changing this hint neither moves nor deletes files. }
    function WithRuntime(const APath: TNyxText): TNyxLocalSourceSettings;
    { Existing FPC executable path, not command-line text or extra arguments.
      Paths allow Unicode/empty hints and reject controls or over 4096 text units. }
    function WithCompiler(const APath: TNyxText): TNyxLocalSourceSettings;
    { Versioned machine-file boundary. Enabled state is deliberately absent:
      restoring saved paths never authorizes execution or requires a compiler. }
    function Encode: TNyxText;
    class function Decode(const ASource: TNyxText): TNyxLocalSourceSettings; static;
    property LibraryRoot: TNyxText read FLibraryRoot;
    property RuntimeRoot: TNyxText read FRuntimeRoot;
    property CompilerPath: TNyxText read FCompilerPath;
  end;
  { Closed operations decoded at the editor UI boundary. }
  TNyxLocalSourceAction = (lsaUnknown, lsaConfigure, lsaDisable);

const
  NyxLocalSourceLibraryID = 'source-settings-library';
  NyxLocalSourceRuntimeID = 'source-settings-runtime';
  NyxLocalSourceCompilerID = 'source-settings-compiler';
  NyxLocalSourceConfigureID = 'action-source-configure';
  NyxLocalSourceDisableID = 'action-source-disable';

{ Build ordinary managed Nyx controls into the borrowed panel. The panel owns
  adopted descendants. This composition knows neither DOM nor LCL nor files. }
procedure AddNyxLocalSourceSettings(AParent: TNyxNode;
  const ASettings: TNyxLocalSourceSettings; AEnabled, APending: Boolean;
  const AMessage: TNyxText);
{ Route known field identities only; unknown controls leave the copied settings
  unchanged. Malformed known input raises before assigning the replacement. }
function SetNyxLocalSourceField(const AID, AValue: TNyxText;
  var ASettings: TNyxLocalSourceSettings): Boolean;
{ Decode fixed action identities; unknown returns lsaUnknown without effects. }
function NyxLocalSourceAction(const AID: TNyxText): TNyxLocalSourceAction;

implementation

uses SysUtils, nyx.data, nyx.controls;

procedure ValidatePathHint(const APath: TNyxText);
var
  LIndex: Integer;
begin

  if Length(APath) > 4096 then
  begin
    raise ENyxModel.Create('A local compiler path exceeds 4096 text units');
  end;
  for LIndex := 1 to Length(APath) do
  begin

    if Ord(APath[LIndex]) < 32 then
    begin
      raise ENyxModel.Create('A local compiler path contains a control separator');
    end;
  end;
end;

class function TNyxLocalSourceSettings.Empty: TNyxLocalSourceSettings;
begin
  Result := Default(TNyxLocalSourceSettings);
end;

function TNyxLocalSourceSettings.WithLibrary(const APath: TNyxText): TNyxLocalSourceSettings;
begin
  ValidatePathHint(APath);
  Result := Self;
  Result.FLibraryRoot := APath;
end;

function TNyxLocalSourceSettings.WithRuntime(const APath: TNyxText): TNyxLocalSourceSettings;
begin
  ValidatePathHint(APath);
  Result := Self;
  Result.FRuntimeRoot := APath;
end;

function TNyxLocalSourceSettings.WithCompiler(const APath: TNyxText): TNyxLocalSourceSettings;
begin
  ValidatePathHint(APath);
  Result := Self;
  Result.FCompilerPath := APath;
end;

function TNyxLocalSourceSettings.Encode: TNyxText;
begin
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('library', NyxData(FLibraryRoot)),
    NyxField('runtime', NyxData(FRuntimeRoot)),
    NyxField('compiler', NyxData(FCompilerPath))]).ToJSON;
end;

class function TNyxLocalSourceSettings.Decode(const ASource: TNyxText): TNyxLocalSourceSettings;
var
  LData: TNyxDataValue;
begin

  if Length(ASource) > 65536 then
  begin
    raise ENyxModel.Create('Local source settings exceed 64 KiB');
  end;
  LData := TNyxDataValue.ParseJSON(ASource);

  if (LData.Kind <> ndObject) or (LData.Count <> 4) or
    (LData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported local source settings');
  end;
  Result := Empty.WithLibrary(LData.Field('library').AsText)
    .WithRuntime(LData.Field('runtime').AsText)
    .WithCompiler(LData.Field('compiler').AsText);
end;

procedure AddNyxLocalSourceSettings(AParent: TNyxNode;
  const ASettings: TNyxLocalSourceSettings; AEnabled, APending: Boolean;
  const AMessage: TNyxText);
var
  LPanel: INyxCard;
  LStatus: TNyxText;
begin

  if AParent = nil then
  begin
    raise ENyxModel.Create('Local source settings require an owning panel');
  end;
  LPanel := NewNyxCard('studio-local-source-settings');
  LPanel.Configure.Padding(16).Gap(12).Done;
  AParent.Add(LPanel);
  LPanel.Add(NewNyxHeading('source-settings-title').WithText('Pascal source execution'));
  LPanel.Add(NewNyxLabel('source-settings-help').WithText(
    'Configure local compilation for Apply and Open. Application output can stay unchosen.'));
  LPanel.Add(NewNyxInput(NyxLocalSourceCompilerID).Configure.Text('FPC compiler')
    .Value(ASettings.CompilerPath).Placeholder('Choose your existing FPC executable').Done);
  LPanel.Add(NewNyxInput(NyxLocalSourceLibraryID).Configure.Text('Nyx library location')
    .Value(ASettings.LibraryRoot).Placeholder('Folder containing src and studio').Done);
  LPanel.Add(NewNyxInput(NyxLocalSourceRuntimeID).Configure.Text('Compiler workspace')
    .Value(ASettings.RuntimeRoot).Placeholder('Private writable folder for compiler jobs').Done);
  LStatus := 'Local source compilation is off. Design and edit without compiler tools.';

  if AEnabled then
  begin
    LStatus := 'Local source compilation is on.';

    if APending then
    begin
      LStatus := LStatus + ' Apply settings to use your changed paths.';
    end;
  end;
  LPanel.Add(NewNyxLabel('source-settings-status').WithText(LStatus));

  if AMessage <> '' then
  begin
    LPanel.Add(NewNyxLabel('source-settings-message').WithText(AMessage));
  end;
  LPanel.Add(NewNyxButton(NyxLocalSourceConfigureID).WithText('Use for Pascal source'));
  LPanel.Add(NewNyxButton(NyxLocalSourceDisableID).Configure
    .Text('Turn off source compilation').Enabled(AEnabled).Done);
  LPanel.Add(NewNyxLabel('source-settings-privacy').WithText(
    'Paths stay on this machine. Saved settings load as hints; compilation starts only on Apply or Open.'));
end;

function SetNyxLocalSourceField(const AID, AValue: TNyxText;
  var ASettings: TNyxLocalSourceSettings): Boolean;
begin
  Result := True;

  if AID = NyxLocalSourceLibraryID then
  begin
    ASettings := ASettings.WithLibrary(AValue);
  end
  else if AID = NyxLocalSourceRuntimeID then
  begin
    ASettings := ASettings.WithRuntime(AValue);
  end
  else if AID = NyxLocalSourceCompilerID then
  begin
    ASettings := ASettings.WithCompiler(AValue);
  end
  else
  begin
    Result := False;
  end;
end;

function NyxLocalSourceAction(const AID: TNyxText): TNyxLocalSourceAction;
begin
  Result := lsaUnknown;

  if AID = NyxLocalSourceConfigureID then
  begin
    Result := lsaConfigure;
  end
  else if AID = NyxLocalSourceDisableID then
  begin
    Result := lsaDisable;
  end;
end;

end.
