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


unit nyx.http.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.resource.sources;

type
  { Common choices plus an explicit typed extension name for future protocols.
    HTTP method tokens are case-sensitive. Browser-forbidden CONNECT/TRACE/TRACK
    refuse at the portable request boundary rather than silently changing them. }
  TNyxHTTPMethod = (nhmGet, nhmHead, nhmPost, nhmPut, nhmPatch, nhmDelete, nhmOptions);
  TNyxHTTPMethodRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { Ordered immutable header values. Append preserves duplicate fields; Set
    replaces all occurrences at their first position. Names are lowercase ASCII
    tokens. Values preserve HTTP Latin-1 byte scalars with horizontal tab,
    excluding CR/LF/control bytes and DEL. Explicit scalar admission avoids
    native/browser implicit character conversions.
    Target-filtered response headers remain the target's observable subset. }
  TNyxHTTPHeaders = record
  private
    FData: TNyxDataValue;
  public
    function Append(const AName, AValue: TNyxText): TNyxHTTPHeaders;
    function SetValue(const AName, AValue: TNyxText): TNyxHTTPHeaders;
    function Count: Integer;
    function Name(AIndex: Integer): TNyxText;
    function Value(AIndex: Integer): TNyxText;
    function First(const AName: TNyxText): TNyxText;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxHTTPHeaders; static;
  end;
  { Owned request description, with no transport, credentials, callbacks or
    executable authority. Every fluent call returns an independent value.
    Body returns detached bytes. WholeRequest includes queue/I/O time; response
    maximum and request bytes use the existing portable packed-byte ceiling.
    GET/HEAD bodies refuse for Fetch parity; HTTP response status is never
    fabricated from successful transport completion. WithURL derives one
    independently validated target while retaining method, bytes and limits. }
  TNyxHTTPRequest = record
  private
    FURL: TNyxResourceURL;
    FMethod: TNyxHTTPMethodRef;
    FHeaders: TNyxHTTPHeaders;
    FBody: TNyxText;
    FDeadline: Integer;
    FMaximum: Integer;
  public
    function WithURL(const AValue: TNyxResourceURL): TNyxHTTPRequest;
    function Method(AValue: TNyxHTTPMethod): TNyxHTTPRequest; overload;
    function Method(const AValue: TNyxHTTPMethodRef): TNyxHTTPRequest; overload;
    function Header(const AName, AValue: TNyxText): TNyxHTTPRequest;
    { Append retains an additional ordered field, including duplicate names.
      Header replaces every matching occurrence at its original first position. }
    function AppendHeader(const AName, AValue: TNyxText): TNyxHTTPRequest;
    function WithBody(const ABytes: TNyxBytes): TNyxHTTPRequest;
    function WholeRequest(AMilliseconds: Integer): TNyxHTTPRequest;
    function MaximumBytes(ABytes: Integer): TNyxHTTPRequest;
    function Body: TNyxBytes;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxHTTPRequest; static;
    property URL: TNyxResourceURL read FURL;
    property RequestMethod: TNyxHTTPMethodRef read FMethod;
    property Headers: TNyxHTTPHeaders read FHeaders;
    property DeadlineMS: Integer read FDeadline;
    property ByteLimit: Integer read FMaximum;
  end;

function NyxHTTPMethod(const AName: TNyxText): TNyxHTTPMethodRef; overload;
function NyxHTTPMethod(AValue: TNyxHTTPMethod): TNyxHTTPMethodRef; overload;
function NyxHTTPHeaders: TNyxHTTPHeaders;
function NyxHTTPRequest(const AURL: TNyxResourceURL): TNyxHTTPRequest;
{ Refuse headers managed/forbidden by ordinary Fetch. Explicit custom adapters
  may expose a broader host-specific contract separately; the portable browser
  contract never claims a forbidden header was actually sent. }
procedure ValidateNyxHTTPRequestHeader(const AName, AValue: TNyxText);

implementation

function Token(const AText: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := AText <> '';
  for LIndex := 1 to Length(AText) do
  begin

    if not (AText[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '!', '#', '$', '%',
      '&', #39, '*', '+', '-', '.', '^', '_', #96, '|', '~']) then
    begin
      Exit(False);
    end;
  end;
end;

function NyxHTTPMethod(const AName: TNyxText): TNyxHTTPMethodRef;
begin

  if not Token(AName) or (Length(AName) > 64) then
  begin
    raise ENyxBytes.Create('HTTP method requires a bounded protocol token');
  end;

  if (UpperCase(AName) = 'CONNECT') or (UpperCase(AName) = 'TRACE') or
    (UpperCase(AName) = 'TRACK') then
  begin
    raise ENyxBytes.Create('HTTP method is unavailable to the portable Fetch contract');
  end;
  Result.FName := AName;

  if (UpperCase(AName) = 'GET') or (UpperCase(AName) = 'HEAD') or
    (UpperCase(AName) = 'POST') or (UpperCase(AName) = 'PUT') or
    (UpperCase(AName) = 'DELETE') or (UpperCase(AName) = 'OPTIONS') then
  begin
    Result.FName := UpperCase(AName);
  end;
end;

function NyxHTTPMethod(AValue: TNyxHTTPMethod): TNyxHTTPMethodRef;
const
  CNames: array[TNyxHTTPMethod] of TNyxText = ('GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS');
begin

  if (Ord(AValue) < Ord(Low(TNyxHTTPMethod))) or (Ord(AValue) > Ord(High(TNyxHTTPMethod))) then
  begin
    raise ENyxBytes.Create('Unknown HTTP method choice');
  end;
  Result := NyxHTTPMethod(CNames[AValue]);
end;

function HeaderLength(const AValue: TNyxText): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AValue) do
  begin

    if not NyxNextScalar(AValue, LIndex, LScalar) or
      ((LScalar < 32) and (LScalar <> 9)) or (LScalar = 127) or (LScalar > 255) then
    begin
      raise ENyxBytes.Create('HTTP header values require Latin-1 scalars without line breaks');
    end;
    Inc(Result);
  end;
end;

procedure HeaderValue(const AName, AValue: TNyxText);
begin

  if not Token(AName) or (Length(AName) > 256) or (HeaderLength(AValue) > 8192) then
  begin
    raise ENyxBytes.Create('HTTP header name/value exceeds its portable contract');
  end;
end;

procedure ValidateNyxHTTPRequestHeader(const AName, AValue: TNyxText);
var
  LName: TNyxText;
const
  CForbidden = '|accept-charset|accept-encoding|access-control-request-headers|' +
    'access-control-request-method|connection|content-length|cookie|cookie2|date|dnt|' +
    'expect|host|keep-alive|origin|referer|set-cookie|te|trailer|transfer-encoding|' +
    'upgrade|via|permissions-policy|';
begin
  HeaderValue(AName, AValue);
  LName := LowerCase(AName);

  if (Pos('|' + LName + '|', CForbidden) > 0) or
    (Copy(LName, 1, 6) = 'proxy-') or (Copy(LName, 1, 4) = 'sec-') then
  begin
    raise ENyxBytes.Create('HTTP request header is managed by the portable host: ' + LName);
  end;
  { An explicit typed extension method is the portable mechanism for overrides;
    this contract does not silently send a host-dependent override header. }

  if (LName = 'x-http-method') or (LName = 'x-http-method-override') or
    (LName = 'x-method-override') then
  begin
    { A typed extension method is the explicit portable mechanism instead of a
      hidden override that could become forbidden after value-list parsing. }
    raise ENyxBytes.Create('Use an explicit typed HTTP method instead of a method override header');
  end;
end;

function NyxHTTPHeaders: TNyxHTTPHeaders;
begin
  Result := Default(TNyxHTTPHeaders);
end;

function TNyxHTTPHeaders.ToData: TNyxDataValue;
begin
  Result := NyxArray([]);

  if FData.Defined then
  begin
    Result := FData.Copy;
  end;
end;

function TNyxHTTPHeaders.Count: Integer;
begin
  Result := 0;

  if FData.Defined then
  begin
    Result := FData.Count;
  end;
end;

function TNyxHTTPHeaders.Name(AIndex: Integer): TNyxText;
begin
  Result := ToData.Item(AIndex).Item(0).AsText;
end;

function TNyxHTTPHeaders.Value(AIndex: Integer): TNyxText;
begin
  Result := ToData.Item(AIndex).Item(1).AsText;
end;

function TNyxHTTPHeaders.First(const AName: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to Count - 1 do
  begin

    if Name(LIndex) = LowerCase(AName) then
    begin
      Exit(Value(LIndex));
    end;
  end;
end;

function TNyxHTTPHeaders.Append(const AName, AValue: TNyxText): TNyxHTTPHeaders;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
  LBytes: Integer;
  LCount: Integer;
begin
  HeaderValue(AName, AValue);
  LCount := Count;

  if LCount >= 64 then
  begin
    raise ENyxBytes.Create('HTTP headers exceed the portable field-count budget');
  end;
  { One admitted Latin-1 scalar is one HTTP field byte on both targets; native
    UTF-8 storage length must not silently narrow the browser byte budget. }
  LBytes := Length(AName) + HeaderLength(AValue);
  SetLength(LValues, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LValues[LIndex] := ToData.Item(LIndex);
    Inc(LBytes, Length(Name(LIndex)) + HeaderLength(Value(LIndex)));
  end;

  if LBytes > 16384 then
  begin
    raise ENyxBytes.Create('HTTP headers exceed the portable byte budget');
  end;
  LValues[LCount] := NyxArray([NyxData(LowerCase(AName)), NyxData(AValue)]);
  Result.FData := NyxArray(LValues);
end;

function TNyxHTTPHeaders.SetValue(const AName, AValue: TNyxText): TNyxHTTPHeaders;
var
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := NyxHTTPHeaders;
  LFound := False;
  for LIndex := 0 to Count - 1 do
  begin

    if Name(LIndex) = LowerCase(AName) then
    begin

      if not LFound then
      begin
        Result := Result.Append(AName, AValue);
        LFound := True;
      end;
    end
    else
    begin
      Result := Result.Append(Name(LIndex), Value(LIndex));
    end;
  end;

  if not LFound then
  begin
    Result := Result.Append(AName, AValue);
  end;
end;

class function TNyxHTTPHeaders.FromData(const AData: TNyxDataValue): TNyxHTTPHeaders;
var
  LIndex: Integer;
begin

  if AData.Kind <> ndArray then
  begin
    raise ENyxBytes.Create('HTTP header persistence requires an array');
  end;
  Result := NyxHTTPHeaders;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if (AData.Item(LIndex).Kind <> ndArray) or (AData.Item(LIndex).Count <> 2) then
    begin
      raise ENyxBytes.Create('HTTP header persistence requires exact name/value pairs');
    end;
    Result := Result.Append(AData.Item(LIndex).Item(0).AsText, AData.Item(LIndex).Item(1).AsText);
  end;
end;

function NyxHTTPRequest(const AURL: TNyxResourceURL): TNyxHTTPRequest;
begin
  Result := Default(TNyxHTTPRequest);
  Result.FURL := NyxResourceURL(AURL.Address);
  Result.FMethod := NyxHTTPMethod(nhmGet);
  Result.FDeadline := 15000;
  Result.FMaximum := NyxMaximumPackedBytes;
end;

function TNyxHTTPRequest.WithURL(const AValue: TNyxResourceURL): TNyxHTTPRequest;
begin
  Result := Self;
  Result.FURL := NyxResourceURL(AValue.Address);
  Result.Validate;
end;

function TNyxHTTPRequest.Method(AValue: TNyxHTTPMethod): TNyxHTTPRequest;
begin
  Result := Method(NyxHTTPMethod(AValue));
end;

function TNyxHTTPRequest.Method(const AValue: TNyxHTTPMethodRef): TNyxHTTPRequest;
begin
  Result := Self;
  Result.FMethod := NyxHTTPMethod(AValue.Name);
  Result.Validate;
end;

function TNyxHTTPRequest.Header(const AName, AValue: TNyxText): TNyxHTTPRequest;
begin
  ValidateNyxHTTPRequestHeader(AName, AValue);
  Result := Self;
  Result.FHeaders := FHeaders.SetValue(AName, AValue);
end;

function TNyxHTTPRequest.WithBody(const ABytes: TNyxBytes): TNyxHTTPRequest;
begin
  Result := Self;
  Result.FBody := NyxEncodeBase64(ABytes);
  Result.Validate;
end;

function TNyxHTTPRequest.AppendHeader(const AName, AValue: TNyxText): TNyxHTTPRequest;
begin
  ValidateNyxHTTPRequestHeader(AName, AValue);
  Result := Self;
  Result.FHeaders := FHeaders.Append(AName, AValue);
end;

function TNyxHTTPRequest.Body: TNyxBytes;
begin
  Result := NyxDecodeBase64(FBody);
end;

function TNyxHTTPRequest.WholeRequest(AMilliseconds: Integer): TNyxHTTPRequest;
begin
  Result := Self;
  Result.FDeadline := AMilliseconds;
  Result.Validate;
end;

function TNyxHTTPRequest.MaximumBytes(ABytes: Integer): TNyxHTTPRequest;
begin
  Result := Self;
  Result.FMaximum := ABytes;
  Result.Validate;
end;

procedure TNyxHTTPRequest.Validate;
var
  LIndex: Integer;
begin
  NyxResourceURL(FURL.Address);
  NyxHTTPMethod(FMethod.Name);

  if (FDeadline < 1) or (FDeadline > 600000) or
    (FMaximum < 1) or (FMaximum > NyxMaximumPackedBytes) then
  begin
    raise ENyxBytes.Create('HTTP deadline/response budget is outside its portable range');
  end;
  NyxDecodeBase64(FBody);

  if ((FMethod.Name = 'GET') or (FMethod.Name = 'HEAD')) and (FBody <> '') then
  begin
    raise ENyxBytes.Create('GET/HEAD request bodies are unavailable in the portable Fetch contract');
  end;
  for LIndex := 0 to FHeaders.Count - 1 do
  begin
    ValidateNyxHTTPRequestHeader(FHeaders.Name(LIndex), FHeaders.Value(LIndex));
  end;
end;

function TNyxHTTPRequest.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('url', NyxData(FURL.Address)),
    NyxField('method', NyxData(FMethod.Name)), NyxField('headers', FHeaders.ToData),
    NyxField('body', NyxData(FBody)), NyxField('deadline', NyxData(FDeadline)),
    NyxField('maximum', NyxData(FMaximum))]);
end;

class function TNyxHTTPRequest.FromData(const AData: TNyxDataValue): TNyxHTTPRequest;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    (AData.Field('version').ToJSON <> '1') then
  begin
    raise ENyxBytes.Create('Unknown HTTP request persistence');
  end;
  Result := NyxHTTPRequest(NyxResourceURL(AData.Field('url').AsText));
  Result.FMethod := NyxHTTPMethod(AData.Field('method').AsText);
  Result.FHeaders := TNyxHTTPHeaders.FromData(AData.Field('headers'));
  Result.FBody := AData.Field('body').AsText;
  Result.FDeadline := AData.Field('deadline').AsInteger;
  Result.FMaximum := AData.Field('maximum').AsInteger;
  Result.Validate;
end;

end.
