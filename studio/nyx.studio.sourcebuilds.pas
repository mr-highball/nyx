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

unit nyx.studio.sourcebuilds;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.filechanges,
  nyx.studio.sourceprojection;

const
  { JSON escaping can expand the admitted 1 MiB UTF-8 source by six. This HTTP
    envelope bound precedes JSON parsing; source admission still enforces 1 MiB. }
  NyxSourceBuildMaximumRequestBytes = 6 * 1024 * 1024 + 8192;
  { A native reply may contain an independently bounded executed construction
    and compiler diagnostics. It never advertises a downloadable executable. }
  NyxNativeSourceBuildMaximumReplyBytes = 2 * NyxProjectionMaximumResultBytes + 8192;

{ Private authenticated compiler receipt, not project persistence or admission.
  Source stays with its caller; compiler diagnostics omit the duplicated source.
  Only compiled browser results and nonexecuted failures cross this boundary.
  The worker receiver remains the sole browser execution/admission path. }
function EncodeNyxBrowserSourceBuild(
  const ABuild: INyxSourceProjectionBuild): TNyxDataValue;
{ ASource is the immutable source belonging to this exact authenticated job.
  Unknown fields/states, executed claims, oversized diagnostics and substituted
  worker paths refuse before an operation can publish. }
function DecodeNyxBrowserSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue): INyxSourceProjectionBuild; overload;
{ Bind only the inventory captured with this exact authenticated request.
  Server admission must understand pascalFiles; older servers refuse that field.
  The reply carries no file/execution claim and never supplies this argument. }
function DecodeNyxBrowserSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild; overload;

{ Native-only private service boundary. Encode borrows the service's joined
  producer result; Decode receives it only from the captured authenticated job,
  never from a design/file/MCP import. Executed construction is re-admitted into
  independent ownership with its exact producer identity/target. Failure receipts
  contain diagnostics but no design. This grants no editor publication authority. }
function EncodeNyxNativeSourceBuild(
  const ABuild: INyxSourceProjectionBuild): TNyxDataValue;
function DecodeNyxNativeSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue): INyxSourceProjectionBuild; overload;
function DecodeNyxNativeSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild; overload;

{ Existing private request extension. Empty inventory preserves exact old bytes;
  present inventory validates against source before append. Reader admits the
  complete captured value, never paths, compiler flags or execution provenance. }
function WithNyxSourceProjectFiles(const AArguments: TNyxDataValue;
  const AFiles: TNyxPascalProjectFiles): TNyxDataValue;
function ReadNyxSourceProjectFiles(const AArguments: TNyxDataValue): TNyxPascalProjectFiles;
{ Closed private inventory intent. New text is read only from its proposed
  captured member; the backend independently reconstructs all retained inputs. }
function ReadNyxSourceFileChange(const AArguments: TNyxDataValue): TNyxPascalFileChange;
function WithNyxSourceFileChange(const AArguments: TNyxDataValue;
  const AChange: TNyxPascalFileChange): TNyxDataValue;

{ Private selected-helper intent. Absence preserves the old main-Apply wire;
  the typed exact leaf identifies acknowledged editor data, never a host path. }
function WithNyxSourceFile(const AArguments: TNyxDataValue;
  const AFile: TNyxPascalSourceRef): TNyxDataValue;
function ReadNyxSourceFile(const AArguments: TNyxDataValue): TNyxPascalSourceRef;

implementation

uses nyx.bytes, nyx.model, nyx.source, nyx.studio.builds, nyx.studio.compiler,
  nyx.studio.projectinputs, nyx.pascal.names;

{ Both public overloads share strict receipt admission. Captured inputs remain
  caller-owned; a wire record never supplies their text or execution authority. }
function DecodeNativeSourceBuildData(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild; forward;
{ Browser receipts use the same copied diagnostic context while retaining their
  separate compiled-only rule and owned worker artifact validation. }
function DecodeBrowserSourceBuildData(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild; forward;

{ Legacy reports keep their exact rows. Rich reports carry only typed helper
  identities beside rows; the caller already owns every submitted file byte. }
function EncodeSourceDiagnostics(const AReport: INyxCompilerReport): TNyxDataValue;
var
  LReport: TNyxDataValue;
  LSources: TNyxDataValue;
  LEntries: array of TNyxDataValue;
  LIndex: Integer;
begin
  Result := NyxNull;

  if AReport = nil then
  begin
    Exit;
  end;
  LReport := TNyxDataValue.ParseJSON(AReport.Encode);

  if LReport.Field('version').AsInteger = 1 then
  begin
    Exit(LReport.Field('items'));
  end;

  if LReport.Field('version').AsInteger <> 2 then
  begin
    raise ENyxModel.Create('Unknown compiler report context version');
  end;
  LSources := LReport.Field('sources');
  SetLength(LEntries, LSources.Count);
  for LIndex := 0 to LSources.Count - 1 do
  begin
    LEntries[LIndex] := NyxObject([
      NyxField('role', LSources.Item(LIndex).Field('role')),
      NyxField('name', LSources.Item(LIndex).Field('name'))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(2)),
    NyxField('items', LReport.Field('items')), NyxField('sources', NyxArray(LEntries))]);
end;

{ Rebuild used helper context only from the immutable captured request. Unknown
  roles/members/fields or a missing local inventory refuse before any report is
  exposed. The full report decoder independently verifies all coordinates. }
function DecodeSourceDiagnostics(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxCompilerReport;
var
  LSources: TNyxDataValue;
  LEntry: TNyxDataValue;
  LEntries: array of TNyxDataValue;
  LInputs: INyxPascalProject;
  LFile: TNyxPascalSourceRef;
  LIndex: Integer;
  LName: TNyxText;
begin
  Result := nil;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if AData.Kind = ndArray then
  begin
    Exit(DecodeNyxCompilerReport(NyxObject([
      NyxField('version', NyxData(1)), NyxField('source', NyxData(ASource)),
      NyxField('items', AData)]).ToJSON));
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 3) or
    (AData.Field('version').AsInteger <> 2) or AFiles.IsEmpty then
  begin
    raise ENyxModel.Create('Helper diagnostics require their captured caller inputs');
  end;
  LSources := AData.Field('sources');

  if (LSources.Kind <> ndArray) or (LSources.Count < 1) or
    (LSources.Count > NyxCompilerMaximumDiagnostics) then
  begin
    raise ENyxModel.Create('Invalid compiler helper identity list');
  end;
  LInputs := AFiles.Open(ASource);
  SetLength(LEntries, LSources.Count);
  for LIndex := 0 to LSources.Count - 1 do
  begin
    LEntry := LSources.Item(LIndex);

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> 2) then
    begin
      raise ENyxModel.Create('Private compiler diagnostics cannot supply helper text');
    end;
    LName := LEntry.Field('name').AsText;
    case LEntry.Field('role').AsInteger of
      Ord(npsUnit):
        begin

          if Copy(LName, Length(LName) - 3, 4) <> '.pas' then
          begin
            raise ENyxModel.Create('Compiler unit identity requires its .pas leaf');
          end;
          LFile := TNyxPascalSourceRef.UnitFile(
            NyxPascalUnit(Copy(LName, 1, Length(LName) - 4)));
        end;
      Ord(npsInclude):
        begin
          LFile := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LName));
        end;
      else
        begin
          raise ENyxModel.Create('Unknown compiler helper identity role');
        end;
    end;
    LEntries[LIndex] := NyxObject([
      NyxField('role', LEntry.Field('role')), NyxField('name', NyxData(LName)),
      NyxField('text', NyxData(LFile.Text(LInputs)))]);
  end;
  Result := DecodeNyxCompilerReport(NyxObject([
    NyxField('version', NyxData(2)), NyxField('source', NyxData(ASource)),
    NyxField('items', AData.Field('items')), NyxField('sources', NyxArray(LEntries))]).ToJSON);
end;

function ReadNyxSourceFile(const AArguments: TNyxDataValue): TNyxPascalSourceRef;
var
  LIndex: Integer;
  LFile: TNyxDataValue;
  LName: TNyxText;
begin
  Result := TNyxPascalSourceRef.Main;
  for LIndex := 0 to AArguments.Count - 1 do
  begin

    if AArguments.Key(LIndex) = 'sourceFile' then
    begin
      LFile := AArguments.Field('sourceFile');

      if (LFile.Kind <> ndObject) or ((LFile.Count <> 2) and (LFile.Count <> 3)) then
      begin
        raise ENyxModel.Create('Selected source file requires its exact kind and name');
      end;

      if (LFile.Count = 3) and (LFile.Field('operation').AsText <> 'create') and
        (LFile.Field('operation').AsText <> 'remove') then
      begin
        raise ENyxModel.Create('Unknown source-file inventory operation');
      end;
      LName := LFile.Field('name').AsText;

      if LFile.Field('kind').AsText = 'unit' then
      begin

        if Copy(LName, Length(LName) - 3, 4) <> '.pas' then
        begin
          raise ENyxModel.Create('Selected unit requires its .pas leaf');
        end;
        Result := TNyxPascalSourceRef.UnitFile(
          NyxPascalUnit(Copy(LName, 1, Length(LName) - 4)));
      end
      else if LFile.Field('kind').AsText = 'include' then
      begin
        Result := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LName));
      end
      else
      begin
        raise ENyxModel.Create('Unknown selected source file kind');
      end;
      Result.Validate;
      Exit;
    end;
  end;
end;

function ReadNyxSourceFileChange(const AArguments: TNyxDataValue): TNyxPascalFileChange;
var
  LFile: TNyxPascalSourceRef;
  LData: TNyxDataValue;
  LProject: INyxPascalProject;
begin
  Result := Default(TNyxPascalFileChange);
  LFile := ReadNyxSourceFile(AArguments);

  if LFile.Role = npsMain then
  begin
    Exit;
  end;
  LData := AArguments.Field('sourceFile');

  if LData.Count = 2 then
  begin
    Exit;
  end;

  if LData.Field('operation').AsText = 'remove' then
  begin
    Exit(TNyxPascalFileChange.Remove(LFile));
  end;
  LProject := ReadNyxSourceProjectFiles(AArguments).Open(AArguments.Field('source').AsText);

  if LFile.Role = npsUnit then
  begin
    Result := TNyxPascalFileChange.CreateUnit(
      NyxPascalUnit(Copy(LFile.Name, 1, Length(LFile.Name) - 4)));
  end
  else
  begin
    Result := TNyxPascalFileChange.CreateInclude(NyxPascalInclude(LFile.Name));
  end;
  Result := Result.WithText(LFile.Text(LProject));
end;

function WithNyxSourceFileChange(const AArguments: TNyxDataValue;
  const AChange: TNyxPascalFileChange): TNyxDataValue;
var
  LArguments: TNyxDataValue;
  LFile: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LOperation: TNyxText;
begin
  AChange.Validate;
  LArguments := WithNyxSourceFile(AArguments, AChange.FileReference);
  LFile := LArguments.Field('sourceFile');
  LOperation := 'create';

  if AChange.Kind = npfcRemove then
  begin
    LOperation := 'remove';
  end;
  SetLength(LFields, LArguments.Count);
  for LIndex := 0 to LArguments.Count - 1 do
  begin

    if LArguments.Key(LIndex) = 'sourceFile' then
    begin
      LFields[LIndex] := NyxField('sourceFile', NyxObject([
        NyxField('kind', LFile.Field('kind')), NyxField('name', LFile.Field('name')),
        NyxField('operation', NyxData(LOperation))]));
    end
    else
    begin
      LFields[LIndex] := NyxField(LArguments.Key(LIndex),
        LArguments.Field(LArguments.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

function WithNyxSourceFile(const AArguments: TNyxDataValue;
  const AFile: TNyxPascalSourceRef): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LKind: TNyxText;
begin
  AFile.Validate;
  Result := AArguments.Copy;

  if AFile.Role = npsMain then
  begin
    Exit;
  end;
  LKind := 'unit';

  if AFile.Role = npsInclude then
  begin
    LKind := 'include';
  end;
  SetLength(LFields, AArguments.Count + 1);
  for LIndex := 0 to AArguments.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AArguments.Key(LIndex), AArguments.Field(AArguments.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('sourceFile', NyxObject([
    NyxField('kind', NyxData(LKind)), NyxField('name', NyxData(AFile.Name))]));
  Result := NyxObject(LFields);
end;

function ReadNyxSourceProjectFiles(const AArguments: TNyxDataValue): TNyxPascalProjectFiles;
var
  LIndex: Integer;
  LSource: TNyxText;
begin
  Result := Default(TNyxPascalProjectFiles);
  LSource := AArguments.Field('source').AsText;
  ValidateNyxProjectionSource(LSource);
  for LIndex := 0 to AArguments.Count - 1 do
  begin

    if AArguments.Key(LIndex) = 'pascalFiles' then
    begin
      Exit(TNyxPascalProjectFiles.Parse(AArguments.Field('pascalFiles').ToJSON, LSource));
    end;
  end;
  NyxCompanionUnitName(LSource);
end;

function WithNyxSourceProjectFiles(const AArguments: TNyxDataValue;
  const AFiles: TNyxPascalProjectFiles): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  Result := AArguments.Copy;

  if AFiles.IsEmpty then
  begin
    Exit;
  end;
  AFiles.Open(AArguments.Field('source').AsText);
  SetLength(LFields, AArguments.Count + 1);
  for LIndex := 0 to AArguments.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AArguments.Key(LIndex),
      AArguments.Field(AArguments.Key(LIndex)));
  end;
  LFields[High(LFields)] := NyxField('pascalFiles', TNyxDataValue.ParseJSON(AFiles.Encoded));
  Result := NyxObject(LFields);
end;

function DecodeNyxBrowserSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild;
begin
  Result := DecodeBrowserSourceBuildData(ASource, AData, AFiles);

  if not AFiles.IsEmpty then
  begin
    Result := BindNyxProjectSourceBuild(Result, AFiles.Open(ASource));
  end;
end;

function DecodeNyxNativeSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild;
begin
  Result := DecodeNativeSourceBuildData(ASource, AData, AFiles);

  if not AFiles.IsEmpty then
  begin
    Result := BindNyxProjectSourceBuild(Result, AFiles.Open(ASource));
  end;
end;

function DecodeNyxNativeSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue): INyxSourceProjectionBuild;
begin
  Result := DecodeNativeSourceBuildData(ASource, AData, Default(TNyxPascalProjectFiles));
end;

function DecodeNyxBrowserSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue): INyxSourceProjectionBuild;
begin
  Result := DecodeBrowserSourceBuildData(ASource, AData, Default(TNyxPascalProjectFiles));
end;

function EncodeNyxNativeSourceBuild(
  const ABuild: INyxSourceProjectionBuild): TNyxDataValue;
var
  LItems: TNyxDataValue;
  LPacket: TNyxDataValue;
  LDocument: TNyxDocument;
begin

  if (ABuild = nil) or (ABuild.Projection = nil) or
    (ABuild.Projection.Target <> btNativeLCL) or
    (ABuild.Projection.State = spsCompiled) or (ABuild.Artifact <> '') then
  begin
    raise ENyxModel.Create('Native source receipt requires its owned executed result or failure');
  end;
  LItems := NyxNull;
  LPacket := NyxNull;

  if ABuild.Projection.Report <> nil then
  begin
    LItems := EncodeSourceDiagnostics(ABuild.Projection.Report);
  end;

  if ABuild.Projection.State = spsExecuted then
  begin
    LDocument := ABuild.Projection.CopyDocument;
    try
      LPacket := CaptureNyxSourceProjection(LDocument, ABuild.Reference, btNativeLCL);
    finally
      LDocument.Free;
    end;
  end;
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('reference', NyxData(ABuild.Reference.Name)),
    NyxField('target', NyxData(NyxBuildTargetName(btNativeLCL))),
    NyxField('state', NyxData(NyxSourceProjectionStateName(ABuild.Projection.State))),
    NyxField('message', NyxData(ABuild.Projection.Message)),
    NyxField('diagnostics', LItems), NyxField('projection', LPacket)]);

  if NyxUTF8ByteCount(Result.ToJSON) > NyxNativeSourceBuildMaximumReplyBytes then
  begin
    raise ENyxModel.Create('Native source compiler receipt exceeds its reply budget');
  end;
end;

function DecodeNativeSourceBuildData(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild;
var
  LState: TNyxSourceProjectionState;
  LMatched: Boolean;
  LItems: TNyxDataValue;
  LPacket: TNyxDataValue;
  LReference: TNyxSourceProjectionRef;
  LReport: INyxCompilerReport;
  LProjection: INyxSourceProjection;
  LIndex: Integer;
begin
  ValidateNyxProjectionSource(ASource);

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    (AData.Field('version').ToJSON <> '1') or
    (AData.Field('target').AsText <> NyxBuildTargetName(btNativeLCL)) or
    (NyxUTF8ByteCount(AData.ToJSON) > NyxNativeSourceBuildMaximumReplyBytes) then
  begin
    raise ENyxModel.Create('Invalid private native source compiler receipt');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if (AData.Key(LIndex) <> 'version') and (AData.Key(LIndex) <> 'reference') and
      (AData.Key(LIndex) <> 'target') and (AData.Key(LIndex) <> 'state') and
      (AData.Key(LIndex) <> 'message') and (AData.Key(LIndex) <> 'diagnostics') and
      (AData.Key(LIndex) <> 'projection') then
    begin
      raise ENyxModel.Create('Native source compiler receipt contains an unknown field');
    end;
  end;
  LReference := NyxSourceProjectionRef(AData.Field('reference').AsText);
  LMatched := False;
  for LState := Low(TNyxSourceProjectionState) to High(TNyxSourceProjectionState) do
  begin

    if AData.Field('state').AsText = NyxSourceProjectionStateName(LState) then
    begin
      LMatched := True;
      Break;
    end;
  end;

  if not LMatched or (LState = spsCompiled) then
  begin
    raise ENyxModel.Create('Native compiler receipt requires execution or a closed failure');
  end;
  LItems := AData.Field('diagnostics');
  LReport := nil;

  if LItems.Kind <> ndNull then
  begin
    LReport := DecodeSourceDiagnostics(ASource, LItems, AFiles);
  end;
  LPacket := AData.Field('projection');

  if LState = spsExecuted then
  begin

    if (LPacket.Kind <> ndObject) or (AData.Field('message').AsText <> '') then
    begin
      raise ENyxModel.Create('Executed native receipt requires its complete producer packet');
    end;
    LProjection := ReceiveNyxSourceProjection(ASource, LReference, btNativeLCL,
      LPacket.ToJSON, LReport);

    if LProjection.State <> spsExecuted then
    begin
      raise ENyxModel.Create('Native compiler producer packet refused: ' + LProjection.Message);
    end;
  end
  else
  begin

    if LPacket.Kind <> ndNull then
    begin
      raise ENyxModel.Create('Failed native receipt cannot carry a construction');
    end;
    LProjection := NyxSourceProjectionFailure(ASource, btNativeLCL, LState,
      AData.Field('message').AsText, LReport);
  end;
  Result := NewNyxSourceProjectionBuild(LReference, LProjection, '', '');
end;

function EncodeNyxBrowserSourceBuild(
  const ABuild: INyxSourceProjectionBuild): TNyxDataValue;
var
  LItems: TNyxDataValue;
begin

  if (ABuild = nil) or (ABuild.Projection = nil) or
    (ABuild.Projection.Target <> btBrowser) or
    (ABuild.Projection.State = spsExecuted) then
  begin
    raise ENyxModel.Create('Source service requires a compiled browser receipt or failure');
  end;
  LItems := NyxNull;

  if ABuild.Projection.Report <> nil then
  begin
    LItems := EncodeSourceDiagnostics(ABuild.Projection.Report);
  end;
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('reference', NyxData(ABuild.Reference.Name)),
    NyxField('state', NyxData(NyxSourceProjectionStateName(ABuild.Projection.State))),
    NyxField('artifact', NyxData(ABuild.Artifact)),
    NyxField('message', NyxData(ABuild.Projection.Message)),
    NyxField('diagnostics', LItems)]);

  if NyxUTF8ByteCount(Result.ToJSON) > NyxProjectionMaximumResultBytes then
  begin
    raise ENyxModel.Create('Source compiler receipt exceeds its reply budget');
  end;
end;

function DecodeBrowserSourceBuildData(const ASource: TNyxText;
  const AData: TNyxDataValue; const AFiles: TNyxPascalProjectFiles): INyxSourceProjectionBuild;
var
  LState: TNyxSourceProjectionState;
  LMatched: Boolean;
  LItems: TNyxDataValue;
  LReport: INyxCompilerReport;
begin
  ValidateNyxProjectionSource(ASource);

  if (AData.Kind <> ndObject) or (AData.Count <> 6) or
    (AData.Field('version').AsInteger <> 1) or
    (NyxUTF8ByteCount(AData.ToJSON) > NyxProjectionMaximumResultBytes) then
  begin
    raise ENyxModel.Create('Invalid private source compiler receipt');
  end;
  LMatched := False;
  for LState := Low(TNyxSourceProjectionState) to High(TNyxSourceProjectionState) do
  begin

    if AData.Field('state').AsText = NyxSourceProjectionStateName(LState) then
    begin
      LMatched := True;
      Break;
    end;
  end;

  if not LMatched or (LState = spsExecuted) then
  begin
    raise ENyxModel.Create('Source compiler receipt cannot claim browser execution');
  end;
  LItems := AData.Field('diagnostics');
  LReport := nil;

  if LItems.Kind <> ndNull then
  begin
    LReport := DecodeSourceDiagnostics(ASource, LItems, AFiles);
  end;
  Result := NewNyxSourceProjectionBuild(
    NyxSourceProjectionRef(AData.Field('reference').AsText),
    NyxSourceProjectionFailure(ASource, btBrowser, LState,
      AData.Field('message').AsText, LReport), AData.Field('artifact').AsText, '');
end;

end.
