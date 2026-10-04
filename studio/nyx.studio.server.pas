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

unit nyx.studio.server;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.json,
  Classes,
  SysUtils,
  fphttpserver,
  httpdefs,
  fpjson,
  Process,
  nyx.model,
  nyx.schema,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.callbacks,
  nyx.scheduler,
  nyx.studio.builds,
  nyx.studio.compiler,
  nyx.studio.mcp,
  nyx.studio.projects,
  nyx.studio.projectstore,
  nyx.composition,
  nyx.studio.outputs;

type
  { Older supported FPC exposes Address as protected. Publish it in our local
    adapter so the launcher can select loopback or LAN interfaces explicitly. }
  TNyxHTTPServer = class(TFPHTTPServer)
  public
    property Address;
  end;

  { Local development service. Owns generated build artifacts beneath build/ and
    explicitly saved paired projects beneath .local/projects/. Compiler configuration comes from
    optional machine-local profiles. A separate configuration route updates
    explicit path fields; build requests carry only designs and target/scope,
    never shell commands or arbitrary compiler argument text. }
  TNyxStudioServer = class
  private
    FHTTP: TNyxHTTPServer;
    FRepository: TNyxText;
    FWebRoot: TNyxText;
    FJobRoot: TNyxText;
    FBindAddress: TNyxText;
    FPort: Integer;
    FSerial: Integer;
    FOutputs: TNyxOutputConfiguration;
    FConfigurationPath: TNyxText;
    FProjects: TNyxProjectStore;
    FMCP: TNyxStudioMCP;
    procedure LoadOutputs;
    procedure SaveOutputs(AOutputs: TNyxOutputConfiguration);
    procedure CheckOutput(const ATarget: TNyxText);
    procedure HandleRequest(ASender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure ServeFile(const APath: TNyxText; AResponse: TFPHTTPConnectionResponse);
    function Build(ADocument: TNyxDocument;
      const ATarget, AScope, APage, ACompanion: TNyxText): TJSONObject;
    function RunCompiler(const AExecutable, ADirectory: TNyxText;
      AArguments: TStrings; out ALog: TNyxText): Boolean;
  public
    { ABindAddress selects the listening interface. Loopback remains the default;
      0.0.0.0 also admits devices on the machine's connected IPv4 networks. }
    constructor Create(const ARepository: TNyxText; APort: Integer;
      const ABindAddress: TNyxText = '127.0.0.1'; AMCPPort: Integer = 0;
      const AWebRoot: TNyxText = '');
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses
  nyx.data;

function RequestText(ARequest: TFPHTTPConnectionRequest): TNyxText;
var
  LBytes: RawByteString;
begin
  { The HTTP RTL stores body bytes in an ANSI-tagged String. The request protocol
    is UTF-8: relabel its raw bytes before entering the typed Nyx API. Converting
    the wrongly tagged String would double-encode the original network bytes. }
  LBytes := ARequest.Content;
  SetCodePage(LBytes, CP_UTF8, False);
  Result := LBytes;
end;

function QueryText(ARequest: TFPHTTPConnectionRequest;
  const AName: String): TNyxText;
var
  LBytes: RawByteString;
begin
  { QueryFields has already percent-decoded the URL into raw bytes, but its
    Strings retain the RTL's ANSI tag. View IDs use the same UTF-8 contract as
    request bodies, including names containing slash or supplementary scalars. }
  LBytes := ARequest.QueryFields.Values[AName];
  SetCodePage(LBytes, CP_UTF8, False);
  Result := LBytes;
end;

procedure RespondUTF8(AResponse: TFPHTTPConnectionResponse; const AText: TNyxText);
var
  LStream: TMemoryStream;
begin
  { Content setters in older HTTP RTL versions use the system ANSI type. A
    response stream keeps source/JSON bytes intact and transfers ownership to
    the HTTP response, which releases it after sending the body. }
  LStream := TMemoryStream.Create;
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
    LStream.Position := 0;
    AResponse.ContentStream := LStream;
    AResponse.FreeContentStream := True;
  except
    LStream.Free;
    raise;
  end;
end;

function ReadFile(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, LStream.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LStream.Size > 0 then
    begin
      LStream.ReadBuffer(Result[1], LStream.Size);
    end;
  finally
    LStream.Free;
  end;
end;

procedure WriteFile(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

function HostHTML(const AProgram: TNyxText): TNyxText;
begin
  { Runtime and compiled Pascal are served from the same admitted artifact root.
    The tiny bootstrap is the only handwritten target glue: all product behavior
    is compiled Pascal. Relative URLs work under both / and /builds/job-N/. }
  Result := '<!doctype html><html lang="en"><head><meta charset="utf-8">' +
    '<meta name="viewport" content="width=device-width,initial-scale=1">' +
    '<title>Nyx</title></head><body style="margin:0">' +
    '<script src="rtl.js"></script><script src="' + AProgram + '.js"></script>' +
    '<script>rtl.run();</script></body></html>';
end;

constructor TNyxStudioServer.Create(const ARepository: TNyxText; APort: Integer;
  const ABindAddress: TNyxText; AMCPPort: Integer; const AWebRoot: TNyxText);
begin
  inherited Create;

  if (APort < 1024) or (APort > 65535) then
  begin
    raise Exception.Create('Port must be in 1024..65535');
  end;
  FPort := APort;

  if Trim(ABindAddress) = '' then
  begin
    raise Exception.Create('A Studio bind address is required');
  end;
  FBindAddress := ABindAddress;
  FRepository := IncludeTrailingPathDelimiter(ExpandFileName(ARepository));
  FWebRoot := FRepository + 'build' + PathDelim + 'browser' + PathDelim;
  { A launcher may serve a staged frontend for integration checks without
    changing the live editor's files. HTTP requests cannot change this root. }

  if AWebRoot <> '' then
  begin
    FWebRoot := IncludeTrailingPathDelimiter(ExpandFileName(AWebRoot));
  end;
  FJobRoot := FRepository + 'build' + PathDelim + 'studio' + PathDelim + 'jobs' + PathDelim;
  ForceDirectories(FWebRoot);
  ForceDirectories(FJobRoot);
  FConfigurationPath := FRepository + '.local' + PathDelim + 'studio-outputs.nyx';
  LoadOutputs;
  FProjects := TNyxProjectStore.Create(FRepository + '.local' + PathDelim + 'projects');
  { Editor/LAN and agent ports are separate. Only the latter is loopback-bound;
    its per-launch document endpoint is registered in local Codex configuration. }

  if AMCPPort = 0 then
  begin
    AMCPPort := APort + 1;
  end;
  FMCP := TNyxStudioMCP.Create(FRepository, APort, AMCPPort);
  FHTTP := TNyxHTTPServer.Create(nil);
  FHTTP.Address := FBindAddress;
  FHTTP.Port := FPort;
  { Serial requests keep compiler admission and job state simple in this first
    slice. Compiler work is bounded; a worker queue is a separate later boundary. }
  FHTTP.Threaded := False;
  FHTTP.OnRequest := HandleRequest;
end;

destructor TNyxStudioServer.Destroy;
begin
  FMCP.Free;
  FHTTP.Free;
  FOutputs.Free;
  FProjects.Free;
  inherited Destroy;
end;

procedure TNyxStudioServer.LoadOutputs;
var
  LLoaded: TNyxOutputConfiguration;
begin
  { Environment values are optional initial hints. A saved Studio profile wins
    on later launches, including intentionally empty fields. Bad private config
    must not make the designer unavailable; retain hints and report the warning. }
  FOutputs := TNyxOutputConfiguration.Create;
  FOutputs.SetField('pas2js', GetEnvironmentVariable('NYX_PAS2JS'));
  FOutputs.SetField('runtime', GetEnvironmentVariable('NYX_PAS2JS_RUNTIME'));
  FOutputs.SetField('fpc', GetEnvironmentVariable('NYX_FPC'));
  FOutputs.SetField('lazarus', GetEnvironmentVariable('NYX_LAZARUS'));
  FOutputs.SetField('platform', GetEnvironmentVariable('NYX_LCL_PLATFORM'));
  FOutputs.SetField('widgetset', GetEnvironmentVariable('NYX_LCL_WIDGETSET'));

  if FileExists(FConfigurationPath) then
  begin
    try
      LLoaded := TNyxOutputConfiguration.Decode(ReadFile(FConfigurationPath));
      FOutputs.Free;
      FOutputs := LLoaded;
    except
      on LException: Exception do
      begin
        WriteLn('Output configuration was not restored: ', LException.Message);
      end;
    end;
  end;
end;

procedure TNyxStudioServer.SaveOutputs(AOutputs: TNyxOutputConfiguration);
var
  LTemporary: TNyxText;
  LBackup: TNyxText;
  LHadPrevious: Boolean;
begin
  { Save a complete candidate before replacing the accepted profile. The backup
    allows a failed rename to restore the prior disk state. The request handler
    swaps its in-memory profile only after this succeeds. }
  ForceDirectories(ExtractFilePath(FConfigurationPath));
  LTemporary := FConfigurationPath + '.tmp';
  LBackup := FConfigurationPath + '.bak';
  WriteFile(LTemporary, AOutputs.Encode);
  LHadPrevious := FileExists(FConfigurationPath);

  if FileExists(LBackup) and not DeleteFile(LBackup) then
  begin
    raise Exception.Create('Cannot replace the previous output configuration backup');
  end;

  if LHadPrevious and not RenameFile(FConfigurationPath, LBackup) then
  begin
    raise Exception.Create('Cannot preserve the previous output configuration');
  end;

  if not RenameFile(LTemporary, FConfigurationPath) then
  begin

    if LHadPrevious then
    begin
      RenameFile(LBackup, FConfigurationPath);
    end;
    raise Exception.Create('Cannot save output configuration');
  end;

  if LHadPrevious then
  begin
    DeleteFile(LBackup);
  end;
end;

function IsOutputIdentifier(const AText: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := (AText <> '') and (Length(AText) <= 80);
  for LIndex := 1 to Length(AText) do
  begin

    if not (AText[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
    begin
      Exit(False);
    end;
  end;
end;

procedure TNyxStudioServer.CheckOutput(const ATarget: TNyxText);
var
  LRoot: TNyxText;
  LPlatform: TNyxText;
  LWidgetset: TNyxText;
begin
  ValidateNyxOutputTarget(ATarget);

  if ATarget = '' then
  begin
    raise ENyxModel.Create('Choose an output in Studio Target / output before building');
  end;

  if ATarget = 'browser' then
  begin

    if not FileExists(FOutputs.Field('pas2js')) then
    begin
      raise ENyxModel.Create('Browser output: configure the pas2js compiler in Target / output');
    end;

    if not FileExists(FOutputs.Field('runtime')) then
    begin
      raise ENyxModel.Create('Browser output: configure matching rtl.js in Target / output');
    end;
  end
  else
  begin

    if not FileExists(FOutputs.Field('fpc')) then
    begin
      raise ENyxModel.Create('Native LCL output: configure the FPC compiler in Target / output');
    end;
    LRoot := IncludeTrailingPathDelimiter(FOutputs.Field('lazarus'));
    LPlatform := FOutputs.Field('platform');
    LWidgetset := FOutputs.Field('widgetset');

    if not DirectoryExists(LRoot + 'lcl') then
    begin
      raise ENyxModel.Create('Native LCL output: configure the Lazarus root in Target / output');
    end;

    if not IsOutputIdentifier(LPlatform) or not IsOutputIdentifier(LWidgetset) then
    begin
      raise ENyxModel.Create('Native LCL output: configure platform and widgetset in Target / output');
    end;

    if not DirectoryExists(LRoot + 'lcl/units/' + LPlatform + '/' + LWidgetset) then
    begin
      raise ENyxModel.Create('Native LCL output: matching Lazarus platform/widgetset units are missing');
    end;
  end;
end;

procedure TNyxStudioServer.Run;
begin
  FMCP.Start;
  WriteLn('Nyx Studio listening on ', FBindAddress, ':', FPort);
  WriteLn('This machine: http://127.0.0.1:', FPort, '/');
  WriteLn('Nyx agent endpoint: ', FMCP.Endpoint);
  FHTTP.Active := True;
end;

procedure TNyxStudioServer.ServeFile(const APath: TNyxText;
  AResponse: TFPHTTPConnectionResponse);
var
  LExtension: TNyxText;
begin

  if not FileExists(APath) then
  begin
    AResponse.Code := 404;
    AResponse.Content := 'Artifact not found';
    Exit;
  end;
  LExtension := LowerCase(ExtractFileExt(APath));
  AResponse.ContentType := 'application/octet-stream';

  if LExtension = '.html' then
  begin
    AResponse.ContentType := 'text/html; charset=utf-8';
  end
  else if LExtension = '.js' then
  begin
    AResponse.ContentType := 'text/javascript; charset=utf-8';
  end
  else if LExtension = '.pas' then
  begin
    AResponse.ContentType := 'text/plain; charset=utf-8';
  end;
  AResponse.ContentStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  AResponse.FreeContentStream := True;
end;

function TNyxStudioServer.RunCompiler(const AExecutable, ADirectory: TNyxText;
  AArguments: TStrings; out ALog: TNyxText): Boolean;
var
  LProcess: TProcess;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LChunk: TNyxText;
  LStarted: QWord;
begin

  if (AExecutable = '') or not FileExists(AExecutable) then
  begin
    ALog := 'Compiler executable is missing; configure NYX_PAS2JS or NYX_FPC.';
    Exit(False);
  end;
  LProcess := TProcess.Create(nil);
  try
    LProcess.Executable := AExecutable;
    LProcess.CurrentDirectory := ADirectory;
    LProcess.Parameters.Assign(AArguments);
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    ALog := '';
    LProcess.Execute;
    LStarted := GetTickCount64;
    repeat
      { Drain pipes while the compiler runs. Waiting for exit before reading can
        deadlock a compiler that fills its pipe with warnings/diagnostics. }
      while LProcess.Output.NumBytesAvailable > 0 do
      begin
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
          ALog := ALog + LChunk;
        end;

        if Length(ALog) > 1024 * 1024 then
        begin
          LProcess.Terminate(1);
          ALog := Copy(ALog, 1, 1024 * 1024) + #10 + 'Compiler log budget exceeded.';
          Exit(False);
        end;
      end;

      if GetTickCount64 - LStarted > 60000 then
      begin
        LProcess.Terminate(1);
        ALog := ALog + #10 + 'Compiler exceeded 60-second time budget.';
        Exit(False);
      end;

      if LProcess.Running then
      begin
        Sleep(10);
      end;
    until not LProcess.Running and (LProcess.Output.NumBytesAvailable = 0);
    Result := LProcess.ExitStatus = 0;
  finally
    LProcess.Free;
  end;
end;

function TNyxStudioServer.Build(ADocument: TNyxDocument;
  const ATarget, AScope, APage, ACompanion: TNyxText): TJSONObject;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LIndex: Integer;
  LJob: TNyxText;
  LDirectory: TNyxText;
  LArguments: TStringList;
  LSource: TNyxText;
  LUnitName: TNyxText;
  LCompanionSource: TNyxText;
  LLog: TNyxText;
  LExecutable: TNyxText;
  LRuntime: TNyxText;
  LLazarus: TNyxText;
  LPlatform: TNyxText;
  LWidgetset: TNyxText;
  LOK: Boolean;
  LReport: INyxCompilerReport;
  LSubmittedSource: TNyxText;

  procedure AdmitBrowserPolicies(ANode: TNyxNode);
  var
    LEvents: TNyxAuthoredEventInfos;
    LEventIndex: Integer;
    LChildIndex: Integer;
  begin
    LEvents := NyxAuthoredEvents(ANode);
    for LEventIndex := 0 to High(LEvents) do
    begin

      if LEvents[LEventIndex].Policy = neThreaded then
      begin
        raise ENyxModel.Create('Browser output cannot execute threaded callbacks on ' +
          ANode.ID + '; choose asynchronous or a native output');
      end;
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      AdmitBrowserPolicies(ANode.Children[LChildIndex]);
    end;
  end;

begin

  ValidateNyxOutputTarget(ATarget);

  if (AScope <> 'view') and (AScope <> 'application') then
  begin
    raise ENyxModel.Create('Scope must be view or application');
  end;
  ValidateNyxDocumentProperties(ADocument);
  CheckOutput(ATarget);
  LDocument := nil;
  LArguments := TStringList.Create;
  try

    if AScope = 'view' then
    begin
      LRoot := ADocument.Find(APage);

      if LRoot = nil then
      begin
        raise ENyxModel.Create('Requested view is missing');
      end;
      { The public view-cloning contract preserves authored identity and takes
        only reachable definitions. A maximum-length or definition-root preview
        therefore needs neither a prefix nor a duplicate definition root. }
      LDocument := CloneNyxViewDocument(ADocument, LRoot);
    end
    else
    begin
      { Use the complete ownership contract. Copying trees/state manually lost
        collection defaults and produced a saved job design differing from its
        companion. Clone carries every admitted document feature independently. }
      LDocument := ADocument.Clone;
    end;

    if LDocument.Count = 0 then
    begin
      raise ENyxModel.Create('Add a page before building an application');
    end;

    if ATarget = 'browser' then
    begin
      for LIndex := 0 to LDocument.Count - 1 do
      begin
        AdmitBrowserPolicies(LDocument.Pages[LIndex]);
      end;
      for LIndex := 0 to LDocument.ComponentCount - 1 do
      begin
        AdmitBrowserPolicies(LDocument.Components[LIndex]);
      end;
    end;
    { Companion admission precedes job allocation and compilation. Application
      builds retain the exact accepted source; view builds replace only the
      managed builder and preserve real imports, helpers and handwritten code. }
    LUnitName := 'nyx.generated.view';
    LCompanionSource := TNyxCodegen.Generate(LDocument);

    if ACompanion <> '' then
    begin
      LUnitName := NyxCompanionUnitName(ACompanion);
      LCompanionSource := PrepareNyxCompanion(ADocument, LDocument, ACompanion,
        AScope = 'view');
    end;
    LSubmittedSource := ACompanion;

    if LSubmittedSource = '' then
    begin
      LSubmittedSource := LCompanionSource;
    end;
    Inc(FSerial);
    { Timestamp avoids overwriting a prior session's admitted artifact. The serial
      disambiguates builds within this process; clients receive only this ID. }
    LJob := 'job-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '-' + IntToStr(FSerial);
    LDirectory := FJobRoot + LJob + PathDelim;
    ForceDirectories(LDirectory + 'units');
    WriteFile(LDirectory + LUnitName + '.pas', LCompanionSource);
    WriteFile(LDirectory + 'design.nyx', TNyxCodec.Encode(LDocument));
    LArguments.Add('-Mdelphi');
    LArguments.Add('-B');
    { Both providers support full diagnostic paths. Bare filenames cannot
      establish that an error belongs to this admitted companion rather than
      a same-named dependency, so navigation requires this fixed option. }
    LArguments.Add('-vb');
    LArguments.Add('-Fu' + FRepository + 'src');
    LArguments.Add('-Fu' + LDirectory);
    LArguments.Add('-FE' + LDirectory);

    if ATarget = 'browser' then
    begin
      LExecutable := FOutputs.Field('pas2js');
      LRuntime := FOutputs.Field('runtime');

      LSource := 'program nyx_preview;' + #10 + '{$mode delphi}{$H+}' + #10 +
        '{$codepage utf8}' + #10 +
        'uses Web, nyx.model, nyx.application.browser, ' + LUnitName + ';' + #10 +
        'var LDocument: TNyxDocument; LApplication: TNyxBrowserApplication;' + #10 +
        'begin' + #10 +
        '  LDocument := BuildNyxDocument;' + #10 +
        '  LApplication := TNyxBrowserApplication.Create;' + #10 +
        '  LApplication.Run(LDocument, TJSHTMLElement(document.body));' + #10 +
        '  document.body.setAttribute(''data-nyx-ready'', ''true'');' + #10 +
        'end.' + #10;
      WriteFile(LDirectory + 'nyx_preview.lpr', LSource);
      LArguments.Add(LDirectory + 'nyx_preview.lpr');
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog);
      WriteFile(LDirectory + 'rtl.js', ReadFile(LRuntime));
      WriteFile(LDirectory + 'index.html', HostHTML('nyx_preview'));
    end
    else
    begin
      LExecutable := FOutputs.Field('fpc');
      LLazarus := IncludeTrailingPathDelimiter(FOutputs.Field('lazarus'));

      LArguments.Add('-FU' + LDirectory + 'units');
      LPlatform := FOutputs.Field('platform');
      LWidgetset := FOutputs.Field('widgetset');

      LArguments.Add('-Fu' + LLazarus + 'lcl/units/' + LPlatform);
      LArguments.Add('-Fu' + LLazarus + 'lcl/units/' + LPlatform + '/' + LWidgetset);
      LArguments.Add('-Fu' + LLazarus + 'components/lazutils/lib/' + LPlatform);
      LArguments.Add('-Fu' + LLazarus + 'packager/units/' + LPlatform);
      LSource := 'program nyx_native;' + #10 + '{$mode delphi}{$H+}' + #10 +
        '{$codepage utf8}' + #10 +
        'uses Interfaces, Forms, nyx.model, nyx.application.lcl, ' + LUnitName + ';' + #10 +
        'var LDocument: TNyxDocument; LApplication: TNyxLCLApplication;' + #10 +
        'begin' + #10 + '  Application.Initialize;' + #10 +
        '  LDocument := BuildNyxDocument;' + #10 +
        '  LApplication := TNyxLCLApplication.Create;' + #10 +
        '  try' + #10 +
        '    LApplication.Run(LDocument);' + #10 +
        '  finally' + #10 + '    LApplication.Free;' + #10 +
        '    LDocument.Free;' + #10 +
        '  end;' + #10 + 'end.' + #10;
      WriteFile(LDirectory + 'nyx_native.lpr', LSource);
      LArguments.Add(LDirectory + 'nyx_native.lpr');
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog);
    end;
    WriteFile(LDirectory + 'compiler.log', LLog);
    LReport := ReadNyxCompilerReport(LSubmittedSource, LCompanionSource,
      LDirectory + LUnitName + '.pas', LLog);
    Result := TJSONObject.Create;
    Result.Add('ok', LOK);
    Result.Add('build', LJob);
    Result.Add('target', ATarget);
    Result.Add('scope', AScope);
    Result.Add('log', LLog);
    Result.Add('diagnostics', DecodeNyxJSON(LReport.Encode));

    if ATarget = 'browser' then
    begin
      Result.Add('artifact', 'builds/' + LJob + '/index.html');
    end
    else
    begin
      Result.Add('artifact', 'builds/' + LJob + '/nyx_native.exe');
    end;
    Result.Add('source', 'builds/' + LJob + '/' + LUnitName + '.pas');
    Result.Add('companion', ACompanion <> '');
  finally
    LArguments.Free;
    LDocument.Free;
  end;
end;

procedure TNyxStudioServer.HandleRequest(ASender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  LPath: TNyxText;
  LRelative: TNyxText;
  LFile: TNyxText;
  LOrigin: TNyxText;
  LHost: TNyxText;
  LDocument: TNyxDocument;
  LCompanion: TNyxText;
  LResult: TJSONObject;
  LOutputs: TNyxOutputConfiguration;
  LMessage: TNyxDataValue;
  LPair: TNyxProjectPair;
  LRevision: TNyxText;
  LRemote: TNyxText;
begin
  AResponse.CustomHeaders.Values['Cache-Control'] := 'no-store';
  AResponse.CustomHeaders.Values['X-Content-Type-Options'] := 'nosniff';
  try
    LPath := ARequest.PathInfo;
    LOrigin := ARequest.GetFieldByName('Origin');
    LHost := ARequest.GetFieldByName('Host');

    { Browsers send the destination authority in Host and the requesting page's
      authority in Origin. Compare them rather than hard-coding loopback: a LAN
      page is admitted, while a different website cannot write/build through it.
      Non-browser clients may omit Origin, as before. No CORS grant is emitted. }

    if (LOrigin <> '') and ((LHost = '') or
      not SameText(LOrigin, 'http://' + LHost)) then
    begin
      AResponse.Code := 403;
      AResponse.Content := 'Origin does not match the Studio service address';
      Exit;
    end;

    if (LPath = '/api/agents/connect') or (LPath = '/api/agents') then
    begin

      if (ARequest.Method <> 'POST') or (LOrigin = '') then
      begin
        AResponse.Code := 403;
        AResponse.Content := 'Use the same-origin Studio editor connection';
        Exit;
      end;
      LMessage := TNyxDataValue.ParseJSON(RequestText(ARequest));

      if LPath = '/api/agents/connect' then
      begin
        LMessage := FMCP.ConnectEditor(LMessage);
      end
      else
      begin
        LMessage := FMCP.EditorExchange(ARequest.GetFieldByName('X-Nyx-Editor'), LMessage);
      end;
      RespondUTF8(AResponse, LMessage.ToJSON);
      AResponse.ContentType := 'application/json; charset=utf-8';
      Exit;
    end;

    if LPath = '/api/agents/preview' then
    begin

      if ARequest.Method <> 'GET' then
      begin
        AResponse.Code := 405;
        Exit;
      end;
      LRemote := FMCP.PreviewData(QueryText(ARequest, 'token'));

      if LRemote = '' then
      begin
        AResponse.Code := 404;
        Exit;
      end;
      AResponse.ContentType := 'application/json; charset=utf-8';
      RespondUTF8(AResponse, LRemote);
      Exit;
    end;

    if LPath = '/api/health' then
    begin
      AResponse.ContentType := 'application/json';
      AResponse.Content := '{"ok":true,"service":"nyx-studio-server","protocol":1}';
      Exit;
    end;

    if LPath = '/api/configuration' then
    begin
      AResponse.ContentType := 'application/json; charset=utf-8';

      if ARequest.Method = 'GET' then
      begin
        RespondUTF8(AResponse, FOutputs.Encode);
      end
      else if ARequest.Method = 'POST' then
      begin
        LOutputs := TNyxOutputConfiguration.Decode(RequestText(ARequest));
        try
          SaveOutputs(LOutputs);
          FOutputs.Free;
          FOutputs := LOutputs;
          LOutputs := nil;
          RespondUTF8(AResponse, FOutputs.Encode);
        finally
          LOutputs.Free;
        end;
      end
      else
      begin
        AResponse.Code := 405;
        AResponse.Content := 'GET or POST a versioned local output configuration';
      end;
      Exit;
    end;

    if LPath = '/api/project' then
    begin
      AResponse.ContentType := 'application/json; charset=utf-8';

      if ARequest.Method = 'GET' then
      begin
        LRemote := FProjects.ReadProject(QueryText(ARequest, 'name'), LRevision);

        if LRemote = '' then
        begin
          AResponse.Code := 404;
        end;
      end
      else if ARequest.Method = 'POST' then
      begin
        LMessage := TNyxDataValue.ParseJSON(RequestText(ARequest));

        if (LMessage.Kind <> ndObject) or (LMessage.Count <> 3) or
          (LMessage.Field('version').AsInteger <> 1) then
        begin
          raise ENyxModel.Create('POST version, expected revision and project packet');
        end;
        LPair := DecodeNyxProject(LMessage.Field('project').AsText);

        if not FProjects.SaveProject(QueryText(ARequest, 'name'),
          LMessage.Field('expected').AsText, LPair, LRevision, LRemote) then
        begin
          AResponse.Code := 409;
        end;
      end
      else
      begin
        AResponse.Code := 405;
        Exit;
      end;
      RespondUTF8(AResponse, NyxObject([
        NyxField('revision', NyxData(LRevision)),
        NyxField('project', NyxData(LRemote))
      ]).ToJSON);
      Exit;
    end;

    if (LPath = '/api/generate') or (LPath = '/api/build') then
    begin

      if ARequest.Method <> 'POST' then
      begin
        AResponse.Code := 405;
        AResponse.Content := 'POST a versioned Nyx design';
        Exit;
      end;
      LCompanion := '';

      if (LPath = '/api/build') and (QueryText(ARequest, 'source') = 'companion') then
      begin
        DecodeNyxBuildRequest(RequestText(ARequest), LDocument, LCompanion);
      end
      else
      begin
        LDocument := TNyxCodec.Decode(RequestText(ARequest));
      end;
      try

        if LPath = '/api/generate' then
        begin
          AResponse.ContentType := 'text/plain; charset=utf-8';
          RespondUTF8(AResponse, TNyxCodegen.Generate(LDocument));
        end
        else
        begin
          LResult := Build(LDocument, QueryText(ARequest, 'target'),
            QueryText(ARequest, 'scope'), QueryText(ARequest, 'page'), LCompanion);
          try
            AResponse.ContentType := 'application/json';
            RespondUTF8(AResponse, LResult.AsJSON);
          finally
            LResult.Free;
          end;
        end;
      finally
        LDocument.Free;
      end;
      Exit;
    end;

    if ARequest.Method <> 'GET' then
    begin
      AResponse.Code := 405;
      Exit;
    end;

    if (Pos('..', LPath) > 0) or (Pos('\', LPath) > 0) or
      (Pos('%', LPath) > 0) or (Pos(':', LPath) > 0) then
    begin
      AResponse.Code := 400;
      AResponse.Content := 'Invalid artifact path';
      Exit;
    end;

    if (LPath = '/') or (LPath = '') then
    begin
      LPath := '/index.html';
    end;

    if Pos('/builds/', LPath) = 1 then
    begin
      LRelative := Copy(LPath, 9, MaxInt);
      LFile := ExpandFileName(FJobRoot + LRelative);

      if Pos(FJobRoot, LFile) <> 1 then
      begin
        raise Exception.Create('Artifact path escaped its build root');
      end;
    end
    else
    begin
      LFile := ExpandFileName(FWebRoot + Copy(LPath, 2, MaxInt));

      if Pos(FWebRoot, LFile) <> 1 then
      begin
        raise Exception.Create('Artifact path escaped its web root');
      end;
    end;
    ServeFile(LFile, AResponse);
  except
    on LException: Exception do
    begin
      LResult := TJSONObject.Create;
      try
        LResult.Add('ok', False);
        LResult.Add('error', LException.Message);

        if LException is ENyxSource then
        begin
          LResult.Add('line', ENyxSource(LException).Line);
          LResult.Add('column', ENyxSource(LException).Column);
        end;
        AResponse.Code := 400;
        AResponse.ContentType := 'application/json';
        RespondUTF8(AResponse, LResult.AsJSON);
      finally
        LResult.Free;
      end;
    end;
  end;
end;

end.
