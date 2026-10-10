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

uses nyx.text, nyx.data, nyx.studio.sourceprojection;

const
  { JSON escaping can expand the admitted 1 MiB UTF-8 source by six. This HTTP
    envelope bound precedes JSON parsing; source admission still enforces 1 MiB. }
  NyxSourceBuildMaximumRequestBytes = 6 * 1024 * 1024 + 8192;

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
  const AData: TNyxDataValue): INyxSourceProjectionBuild;

implementation

uses nyx.bytes, nyx.model, nyx.studio.builds, nyx.studio.compiler;

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
    LItems := TNyxDataValue.ParseJSON(ABuild.Projection.Report.Encode).Field('items');
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

function DecodeNyxBrowserSourceBuild(const ASource: TNyxText;
  const AData: TNyxDataValue): INyxSourceProjectionBuild;
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
    LReport := DecodeNyxCompilerReport(NyxObject([
      NyxField('version', NyxData(1)), NyxField('source', NyxData(ASource)),
      NyxField('items', LItems)]).ToJSON);
  end;
  Result := NewNyxSourceProjectionBuild(
    NyxSourceProjectionRef(AData.Field('reference').AsText),
    NyxSourceProjectionFailure(ASource, btBrowser, LState,
      AData.Field('message').AsText, LReport), AData.Field('artifact').AsText, '');
end;

end.
