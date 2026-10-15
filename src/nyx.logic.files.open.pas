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

unit nyx.logic.files.open;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.files.open, nyx.logic,
  nyx.flow, nyx.flow.types, nyx.scheduler;

type
  { Closed open/read ports map to ordinary typed graph references. Bytes and
    metadata describe the same completed snapshot, never an OS file capability. }
  TNyxOpenFilePort = (nofpTrigger, nofpBytes, nofpMetadata, nofpName,
    nofpByteCount, nofpRead, nofpCancelled, nofpOutcome, nofpError,
    nofpActivationRequired);

  { Managed immutable nonvisual selection/read intent. Saved options carry no
    host authority. User cancel/failure/unavailability are usable complete frames;
    run cancellation revokes publication. Every invocation has its own token. }
  INyxOpenFile = interface(INyxLogicOperation)
    ['{60CAFF21-C6E4-43D0-8855-940000000014}']
    function GetOptions: TNyxFileOpenOptions;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const AScope: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
    property Options: TNyxFileOpenOptions read GetOptions;
  end;

function NewNyxOpenFile(const AOptions: TNyxFileOpenOptions;
  const AHost: INyxFileOpenHost = nil): INyxOpenFile;
function NyxOpenFilePort(AValue: TNyxOpenFilePort): TNyxFlowPortRef;
{ Explicit runtime capability composes with save/HTTP/creator resolvers.
  Neither wire data nor selecting a toolbox operation grants file access. }
function NewNyxOpenFileLogicResolver(const AHost: INyxFileOpenHost;
  const ANext: INyxLogicResolver = nil): INyxLogicResolver;

implementation

uses nyx.bytes;

type
  { UI-owned continuation leases the completion and one host request only while
    pending. Reentrant/inline completion revokes those references before client
    publication. Cancellation revokes graph output before cancelling host input. }
  TOpenInvocation = class(TInterfacedObject, INyxLogicInvocation)
  private
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    FCompletion: INyxLogicCompletion;
    FRequest: INyxFileOpenRequest;
    FLease: INyxLogicInvocation;
    FMaximumBytes: Integer;
    procedure Opened(AOutcome: TNyxFileOpenOutcome; const AFile: INyxFileSnapshot;
      const AError: TNyxText);
  public
    procedure Start(const AOptions: TNyxFileOpenOptions; const AHost: INyxFileOpenHost);
    procedure Cancel;
  end;

  { Immutable operation retains copied options and an explicitly injected host.
    Execute refuses because selection/read completion uses asynchronous invocation. }
  TOpen = class(TNyxLogicOperation, INyxOpenFile, INyxAsyncLogicOperation)
  private
    FOptions: TNyxFileOpenOptions;
    FHost: INyxFileOpenHost;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxFileOpenOptions; const AHost: INyxFileOpenHost);
    function GetOptions: TNyxFileOpenOptions;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const AScope: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
  end;

  { Independent runtime resolver with no document/editor back reference.
    Unknown kinds delegate without changing another host's authority. }
  TOpenResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Host: INyxFileOpenHost;
    Next: INyxLogicResolver;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

function NyxOpenFilePort(AValue: TNyxOpenFilePort): TNyxFlowPortRef;
const
  CNames: array[TNyxOpenFilePort] of TNyxText =
    ('trigger', 'bytes', 'metadata', 'name', 'byte-count', 'read', 'cancelled',
     'outcome', 'error', 'activation-required');
begin

  if (Ord(AValue) < Ord(Low(TNyxOpenFilePort))) or
    (Ord(AValue) > Ord(High(TNyxOpenFilePort))) then
  begin
    raise ENyxFlow.Create('Unknown open-file port');
  end;
  Result := NyxFlowPort(CNames[AValue]);
end;

constructor TOpen.Create(const AOptions: TNyxFileOpenOptions; const AHost: INyxFileOpenHost);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions.Copy;
  FHost := AHost;
  FConfiguration := FOptions.ToData;
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('file-open'), 'Open file',
    'Choose and read one file as owned bytes and portable metadata. Runtime host access, user activation and cancellation remain explicit.',
    nlgFiles, nlaUI)
    .Input(NyxOpenFilePort(nofpTrigger), nftSignal, 'Explicit selection request')
    .Output(NyxOpenFilePort(nofpBytes), nftBytes, 'Complete owned file bytes')
    .Output(NyxOpenFilePort(nofpMetadata), nftJSON, 'Portable snapshot facts, or null when unread')
    .Output(NyxOpenFilePort(nofpName), nftText, 'Selected portable leaf, or empty when unread')
    .Output(NyxOpenFilePort(nofpByteCount), nftInteger, 'Complete read byte count')
    .Output(NyxOpenFilePort(nofpRead), nftBoolean, 'File snapshot was read completely')
    .Output(NyxOpenFilePort(nofpCancelled), nftBoolean, 'User cancelled selection')
    .Output(NyxOpenFilePort(nofpOutcome), nftInteger, 'TNyxFileOpenOutcome ordinal')
    .Output(NyxOpenFilePort(nofpError), nftText, 'Host outcome diagnostic')
    .Output(NyxOpenFilePort(nofpActivationRequired), nftBoolean, 'A current user action is needed')
    .Labelled('files').Labelled('open').Labelled('read').Labelled('import')
    .Labelled('bytes').Labelled('metadata');
end;

function TOpen.GetOptions: TNyxFileOpenOptions;
begin
  Result := FOptions.Copy;
end;

function TOpen.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues;
  raise ENyxFlow.Create('File opening requires asynchronous invocation and an explicit host');
end;

function TOpen.BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
  const AScope: INyxCancellationScope; const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
var
  LInvocation: TOpenInvocation;
begin

  if (AScheduler = nil) or (ACompletion = nil) or (FHost = nil) then
  begin
    raise ENyxFlow.Create('File opening requires a scheduler, completion and explicit host');
  end;
  AScheduler.RequireUI;
  ValidateNyxLogicFrame(FDescriptor, nfdInput, AInputs);

  if (AScope <> nil) and AScope.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('File open cancelled before admission');
  end;
  LInvocation := TOpenInvocation.Create;
  Result := LInvocation;
  LInvocation.FScheduler := AScheduler;
  LInvocation.FScope := AScope;
  LInvocation.FCompletion := ACompletion;
  LInvocation.Start(FOptions, FHost);
end;

procedure TOpenInvocation.Start(const AOptions: TNyxFileOpenOptions;
  const AHost: INyxFileOpenHost);
var
  LRequest: INyxFileOpenRequest;
begin
  FLease := Self;
  FMaximumBytes := AOptions.ByteLimit;
  try
    LRequest := AHost.BeginOpen(AOptions, FScheduler, FScope, Opened);

    if FCompletion = nil then
    begin

      if LRequest <> nil then
      begin
        LRequest.Cancel;
      end;
    end
    else
    begin

      if LRequest = nil then
      begin
        raise ENyxFlow.Create('File-open host returned no pending request token');
      end;
      FRequest := LRequest;
    end;
  except
    Cancel;
    raise;
  end;
end;

procedure TOpenInvocation.Cancel;
var
  LLease: INyxLogicInvocation;
  LRequest: INyxFileOpenRequest;
begin
  FScheduler.RequireUI;
  LLease := Self;
  FCompletion := nil;
  FScope := nil;
  LRequest := FRequest;
  FRequest := nil;
  FLease := nil;

  if LRequest <> nil then
  begin
    LRequest.Cancel;
  end;
end;

procedure TOpenInvocation.Opened(AOutcome: TNyxFileOpenOutcome;
  const AFile: INyxFileSnapshot; const AError: TNyxText);
var
  LLease: INyxLogicInvocation;
  LCompletion: INyxLogicCompletion;
  LFile: INyxFileSnapshot;
  LOutputs: TNyxFlowValues;
  LBytes: TNyxBytes;
  LMetadata: TNyxDataValue;
  LName: TNyxText;
  LCount: Integer;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if (FCompletion = nil) or ((FScope <> nil) and FScope.Cancelled) then
  begin
    Cancel;
    Exit;
  end;
  try
    LFile := AdmitNyxFileOpenResult(AOutcome, AFile, AError, FMaximumBytes);
    LBytes := nil;
    LMetadata := NyxNull;
    LName := '';
    LCount := 0;

    if LFile <> nil then
    begin
      LBytes := LFile.Bytes;
      LMetadata := LFile.Metadata.ToData;
      LName := LFile.Metadata.Name;
      LCount := Length(LBytes);
    end;
    LOutputs := NyxFlowValues
      .WithValue(NyxOpenFilePort(nofpBytes), NyxFlowBytes(LBytes))
      .WithValue(NyxOpenFilePort(nofpMetadata), NyxFlowJSON(LMetadata))
      .WithValue(NyxOpenFilePort(nofpName), NyxFlowText(LName))
      .WithValue(NyxOpenFilePort(nofpByteCount), NyxFlowInteger(LCount))
      .WithValue(NyxOpenFilePort(nofpRead), NyxFlowBoolean(AOutcome = nfooRead))
      .WithValue(NyxOpenFilePort(nofpCancelled), NyxFlowBoolean(AOutcome = nfooCancelled))
      .WithValue(NyxOpenFilePort(nofpOutcome), NyxFlowInteger(Ord(AOutcome)))
      .WithValue(NyxOpenFilePort(nofpError), NyxFlowText(AError))
      .WithValue(NyxOpenFilePort(nofpActivationRequired), NyxFlowBoolean(AOutcome = nfooActivationRequired));
  except
    { Foreign snapshot getters may revoke this invocation before they raise.
      Failure must obey the same receiver/scope guard as successful publication. }
    LCompletion := nil;

    if (FCompletion <> nil) and ((FScope = nil) or not FScope.Cancelled) then
    begin
      LCompletion := FCompletion;
    end;
    Cancel;

    if LCompletion <> nil then
    begin
      LCompletion.Fail('The file-open host returned an invalid or over-budget outcome');
    end;
    Exit;
  end;

  if (FCompletion = nil) or ((FScope <> nil) and FScope.Cancelled) then
  begin
    Cancel;
    Exit;
  end;
  LCompletion := FCompletion;
  FCompletion := nil;
  FScope := nil;
  FRequest := nil;
  FLease := nil;
  LCompletion.Succeed(LOutputs);
end;

function NewNyxOpenFile(const AOptions: TNyxFileOpenOptions;
  const AHost: INyxFileOpenHost): INyxOpenFile;
begin
  Result := TOpen.Create(AOptions, AHost);
end;

function TOpenResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin

  if AKind.Name = 'file-open' then
  begin
    Exit(NewNyxOpenFile(TNyxFileOpenOptions.FromData(AConfiguration), Host));
  end;

  if Next <> nil then
  begin
    Exit(Next.Resolve(AKind, AConfiguration));
  end;
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

function NewNyxOpenFileLogicResolver(const AHost: INyxFileOpenHost;
  const ANext: INyxLogicResolver): INyxLogicResolver;
var
  LResolver: TOpenResolver;
begin
  LResolver := TOpenResolver.Create;
  Result := LResolver;
  LResolver.Host := AHost;
  LResolver.Next := ANext;
end;

end.
