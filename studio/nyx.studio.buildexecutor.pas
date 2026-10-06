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

unit nyx.studio.buildexecutor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Process, SyncObjs, fpjson, nyx.text, nyx.model, nyx.studio.outputs,
  nyx.studio.directories, nyx.studio.builds;

type
  { A monotonic, thread-safe request to retire an owned compiler. Holding this
    interface never retains a job, document or transport. Cancellation of an
    HTTP request alone is deliberately not a compiler cancellation request. }
  INyxBuildCancellation = interface
    ['{D1C91834-5DA4-48F2-A6D1-B719A29A5F38}']
    { Idempotent and irreversible for this invocation. Safe from any thread. }
    procedure Cancel;
    function Cancelled: Boolean;
    { Raises ENyxBuildCancelled when retirement was requested. }
    procedure Check;
  end;

  ENyxBuildCancelled = class(ENyxModel);

  { Trusted host policy, copied by value. Wire requests cannot change budgets.
    Defaults preserve the existing 60-second / 1-MiB compiler limits. }
  TNyxCompilerLimits = record
  private
    FTimeMilliseconds: Cardinal;
    FLogBytes: Integer;
  public
    class function Default: TNyxCompilerLimits; static;
    { Return copied policies, retaining the other axis. Admission rejects zero
      and values above 60000 milliseconds / 1048576 bytes with ENyxModel. }
    function TimeMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
    function LogBytes(AValue: Integer): TNyxCompilerLimits;
    procedure Validate;
  end;

  { One independent compiler invocation. Owns an immutable machine profile and
    borrows the caller's detached document only during Build. Both HTTP and MCP
    use this fixed-argument implementation; requests cannot supply shell commands,
    compiler options, paths or environment overrides. Artifacts are confined to
    the admitted runtime Jobs root. Compiler sources remain borrowed read-only;
    repository-mode callers retain build/studio/jobs. No editor/model lock is held here. }
  TNyxBuildExecutor = class
  private
    FDirectories: TNyxStudioDirectories;
    FJobRoot: TNyxText;
    FOutputs: TNyxOutputConfiguration;
    FLimits: TNyxCompilerLimits;
    FFailure: TNyxCompilerFailure;
    procedure CheckOutput(const ATarget: TNyxText);
    function RunCompiler(const AExecutable, ADirectory: TNyxText;
      AArguments: TStrings; out ALog: TNyxText;
      const ACancellation: INyxBuildCancellation): Boolean;
  public
    constructor Create(const ARepository, AProfile: TNyxText); overload;
    { Retains the admitted directory value separately from the immutable profile.
      Compiler units are borrowed read-only; artifacts belong to runtime Jobs. }
    constructor Create(const ADirectories: TNyxStudioDirectories;
      const AProfile: TNyxText); overload;
    destructor Destroy; override;
    { Readiness is diagnostic only; absent compilers never prevent authoring.
      Empty means ready. Contains no machine path values. }
    function Readiness(const ATarget: TNyxText): TNyxText;
    { Called by the trusted host before delegation; never during Build. }
    procedure ConfigureLimits(const ALimits: TNyxCompilerLimits);
    function Build(ADocument: TNyxDocument;
      const ATarget, AScope, APage, ACompanion: TNyxText;
      const ACancellation: INyxBuildCancellation = nil): TJSONObject;
  end;

{ Returns a fresh reference-counted cancellation lifetime, initially uncancelled.
  It owns only its native event, never the caller's job or editor. }
function NewNyxBuildCancellation: INyxBuildCancellation;

implementation

uses
  nyx.json, nyx.schema, nyx.codec, nyx.codegen, nyx.source, nyx.callbacks,
  nyx.scheduler, nyx.composition, nyx.studio.compiler, nyx.editing;

type
  TNyxBuildCancellation = class(TInterfacedObject, INyxBuildCancellation)
  private
    FCancelled: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    function Cancelled: Boolean;
    procedure Check;
  end;

constructor TNyxBuildCancellation.Create;
begin
  inherited Create;
  FCancelled := TEvent.Create(nil, True, False, '');
end;

destructor TNyxBuildCancellation.Destroy;
begin
  FCancelled.Free;
  inherited Destroy;
end;

procedure TNyxBuildCancellation.Cancel;
begin
  FCancelled.SetEvent;
end;

function TNyxBuildCancellation.Cancelled: Boolean;
begin
  Result := FCancelled.WaitFor(0) = wrSignaled;
end;

procedure TNyxBuildCancellation.Check;
begin

  if Cancelled then
  begin
    raise ENyxBuildCancelled.Create('Compiler cancellation requested');
  end;
end;

function NewNyxBuildCancellation: INyxBuildCancellation;
begin
  Result := TNyxBuildCancellation.Create;
end;

class function TNyxCompilerLimits.Default: TNyxCompilerLimits;
begin
  Result.FTimeMilliseconds := 60000;
  Result.FLogBytes := 1024 * 1024;
end;

function TNyxCompilerLimits.TimeMilliseconds(AValue: Cardinal): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FTimeMilliseconds := AValue;
  Result.Validate;
end;

function TNyxCompilerLimits.LogBytes(AValue: Integer): TNyxCompilerLimits;
begin
  Result := Self;
  Result.FLogBytes := AValue;
  Result.Validate;
end;

procedure TNyxCompilerLimits.Validate;
begin

  if (FTimeMilliseconds < 1) or (FTimeMilliseconds > 60000) or
    (FLogBytes < 1) or (FLogBytes > 1024 * 1024) then
  begin
    raise ENyxModel.Create('Compiler budgets are 1..60000 ms and 1..1048576 log bytes');
  end;
end;

procedure TNyxBuildExecutor.ConfigureLimits(const ALimits: TNyxCompilerLimits);
begin
  ALimits.Validate;
  FLimits := ALimits;
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

constructor TNyxBuildExecutor.Create(const ARepository, AProfile: TNyxText);
begin
  Create(TNyxStudioDirectories.ForRepository(ARepository), AProfile);
end;

constructor TNyxBuildExecutor.Create(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText);
begin
  inherited Create;
  ADirectories.Validate;
  FDirectories := ADirectories;
  FJobRoot := FDirectories.Jobs;
  FOutputs := TNyxOutputConfiguration.Decode(AProfile);
  FLimits := TNyxCompilerLimits.Default;
end;

destructor TNyxBuildExecutor.Destroy;
begin
  FOutputs.Free;
  inherited Destroy;
end;

function TNyxBuildExecutor.Readiness(const ATarget: TNyxText): TNyxText;
begin
  Result := '';
  try
    CheckOutput(ATarget);
  except
    on LException: ENyxModel do
    begin
      Result := LException.Message;
    end;
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

procedure TNyxBuildExecutor.CheckOutput(const ATarget: TNyxText);
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

function TNyxBuildExecutor.RunCompiler(const AExecutable, ADirectory: TNyxText;
  AArguments: TStrings; out ALog: TNyxText;
  const ACancellation: INyxBuildCancellation): Boolean;
var
  LProcess: TProcess;
  LBuffer: array[0..4095] of Byte;
  LRead: Integer;
  LChunk: TNyxText;
  LStarted: QWord;
  LExecuted: Boolean;

  function LogPrefix: TNyxText;
  var
    LIndex: Integer;
    LEnd: Integer;
    LScalar: Integer;
  begin
    { The byte budget may land inside a UTF-8 scalar. Keep only complete
      scalars at this diagnostic boundary; never cut a supplementary character
      into malformed text. Invalid compiler bytes end the readable prefix. }
    LIndex := 1;
    LEnd := 0;
    while LIndex <= FLimits.FLogBytes do
    begin

      if not NyxNextScalar(ALog, LIndex, LScalar) or
        (LIndex > FLimits.FLogBytes + 1) then
      begin
        Break;
      end;
      LEnd := LIndex - 1;
    end;
    Result := Copy(ALog, 1, LEnd);
  end;

  procedure CheckCancellation;
  begin

    if ACancellation <> nil then
    begin
      ACancellation.Check;
    end;
  end;

  function InTime: Boolean;
  begin
    Result := GetTickCount64 - LStarted < FLimits.FTimeMilliseconds;

    if not Result then
    begin
      FFailure := bcfTimeBudget;
      ALog := ALog + #10 + 'Compiler time budget exceeded.';
    end;
  end;
begin
  CheckCancellation;
  FLimits.Validate;

  if (AExecutable = '') or not FileExists(AExecutable) then
  begin
    ALog := 'Compiler executable is missing; configure NYX_PAS2JS or NYX_FPC.';
    Exit(False);
  end;
  LProcess := TProcess.Create(nil);
  LExecuted := False;
  try
    LProcess.Executable := AExecutable;
    LProcess.CurrentDirectory := ADirectory;
    LProcess.Parameters.Assign(AArguments);
    LProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    ALog := '';
    LProcess.Execute;
    LExecuted := True;
    LStarted := GetTickCount64;
    repeat
      { Drain pipes while the compiler runs. Waiting for exit before reading can
        deadlock a compiler that fills its pipe with warnings/diagnostics. }
      while LProcess.Output.NumBytesAvailable > 0 do
      begin
        { Check within the drain loop: a continuously writing compiler must not
          starve cancellation or the deadline by keeping its pipe nonempty. }
        CheckCancellation;

        if not InTime then
        begin
          Exit(False);
        end;
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));
        SetLength(LChunk, LRead);

        if LRead > 0 then
        begin
          Move(LBuffer[0], LChunk[1], LRead);
          ALog := ALog + LChunk;
        end;

        if Length(ALog) > FLimits.FLogBytes then
        begin
          FFailure := bcfLogBudget;
          ALog := LogPrefix + #10 + 'Compiler log budget exceeded.';
          Exit(False);
        end;
      end;

      CheckCancellation;

      if not InTime then
      begin
        Exit(False);
      end;

      if LProcess.Running then
      begin
        Sleep(10);
      end;
    until not LProcess.Running and (LProcess.Output.NumBytesAvailable = 0);
    Result := LProcess.ExitStatus = 0;

    if not Result then
    begin
      FFailure := bcfCompiler;
    end;
  finally
    { Every exit (including cancellation, read failure, deadline/log failure)
      joins this exact owned process. A failed OS retirement keeps the worker
      active; never release its slot, advertise an artifact or orphan a child.
      No editor lock is held. The budget bounds compiler execution, not an OS
      that cannot reap its process. Normal Windows retirement is qualified. }

    if LExecuted then
    begin

      if LProcess.Running then
      begin
        LProcess.Terminate(1);
      end;
      while not LProcess.WaitOnExit(25) do
      begin

        if LProcess.Running then
        begin
          LProcess.Terminate(1);
        end;
      end;
    end;
    LProcess.Free;
  end;
end;

function TNyxBuildExecutor.Build(ADocument: TNyxDocument;
  const ATarget, AScope, APage, ACompanion: TNyxText;
  const ACancellation: INyxBuildCancellation): TJSONObject;
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
  LID: TGUID;

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
  FFailure := bcfNone;

  if ACancellation <> nil then
  begin
    ACancellation.Check;
  end;

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
    CreateGUID(LID);
    { A per-invocation identity also separates concurrent HTTP/MCP workers. }
    LJob := 'job-' + Copy(GUIDToString(LID), 2, 36);
    LDirectory := FJobRoot + LJob + PathDelim;

    if not ForceDirectories(LDirectory + 'units') then
    begin
      raise ENyxModel.Create('Cannot prepare the admitted compiler job directory');
    end;
    WriteFile(LDirectory + LUnitName + '.pas', LCompanionSource);
    WriteFile(LDirectory + 'design.nyx', TNyxCodec.Encode(LDocument));
    LArguments.Add('-Mdelphi');
    LArguments.Add('-B');
    { Both providers support full diagnostic paths. Bare filenames cannot
      establish that an error belongs to this admitted companion rather than
      a same-named dependency, so navigation requires this fixed option. }
    LArguments.Add('-vb');
    LArguments.Add('-Fu' + FDirectories.CompilerUnits);
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
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation);
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
      LOK := RunCompiler(LExecutable, LDirectory, LArguments, LLog, ACancellation);
    end;

    if ACancellation <> nil then
    begin
      ACancellation.Check;
    end;
    WriteFile(LDirectory + 'compiler.log', LLog);
    LReport := ReadNyxCompilerReport(LSubmittedSource, LCompanionSource,
      LDirectory + LUnitName + '.pas', LLog);
    Result := TJSONObject.Create;
    Result.Add('ok', LOK);
    Result.Add('failure', NyxCompilerFailureName(FFailure));
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

end.
