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

unit nyx.logic.files;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.files.save, nyx.logic,
  nyx.flow, nyx.flow.types, nyx.scheduler;

type
  { Typed identities for file-operation graph ports. Trigger and owned bytes
    are inputs; the remaining ports expose distinct portable receipt fields.
    NyxFilePort maps these choices to persistence names without file authority. }
  TNyxFilePort = (nfpTrigger, nfpBytes, nfpOutcome, nfpName, nfpWritten,
    nfpDelivered, nfpCancelled, nfpError, nfpByteCount, nfpActivationRequired);

  { Explicit byte conversion keeps encoding separate from file authority. }
  INyxUTF8Encode = interface(INyxLogicOperation)
    ['{60CAFF21-C6E4-43D0-8855-940000000003}']
    function Encode(const AText: TNyxText): TNyxBytes;
  end;
  { Immutable nonvisual save intent. Serialized options contain suggestions and
    policies only. Each invocation requires an explicitly injected file host.
    User cancellation/activation/unavailability are usable outcome data. Explicit
    run cancellation suppresses output; external file effects are not rolled back.
    Written and Delivered retain their distinct host meanings. }
  INyxSaveFile = interface(INyxLogicOperation)
    ['{60CAFF21-C6E4-43D0-8855-940000000004}']
    function GetOptions: TNyxFileSaveOptions;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
    property Options: TNyxFileSaveOptions read GetOptions;
  end;

function NewNyxUTF8Encode: INyxUTF8Encode;
function NewNyxSaveFile(const AOptions: TNyxFileSaveOptions;
  const AHost: INyxFileSaveHost = nil): INyxSaveFile;
function NyxFilePort(APort: TNyxFilePort): TNyxFlowPortRef;
{ Chain explicit runtime authority without changing document/graph data. Nil
  Next uses the ordinary data-only built-ins; HTTP can supply an explicit next. }
function NewNyxFileLogicResolver(const AHost: INyxFileSaveHost;
  const ANext: INyxLogicResolver = nil): INyxLogicResolver;

implementation

type
  { Stateless, reference-counted UTF-8 conversion operation. Encode returns
    owned bytes from exact portable text; it retains no input or file host. }
  TEncode = class(TNyxLogicOperation, INyxUTF8Encode)
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create;
    function Encode(const AText: TNyxText): TNyxBytes;
  end;

  { One UI-thread bridge from a host reply to a complete graph output frame.
    Retains the scheduler, cancellation scope and completion while pending,
    plus its host request token. A self-lease covers inline/late completion;
    Cancel revokes graph completion before asking the host to retire its reply. }
  TSaveInvocation = class(TInterfacedObject, INyxLogicInvocation)
  private
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    FCompletion: INyxLogicCompletion;
    FRequest: INyxFileSaveRequest;
    FLease: INyxLogicInvocation;
    FByteCount: Integer;
    procedure Saved(const AResult: TNyxFileSaveResult);
  public
    constructor Create(const AScheduler: INyxScheduler;
      const AScope: INyxCancellationScope; const ACompletion: INyxLogicCompletion);
    procedure Start(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AHost: INyxFileSaveHost);
    procedure Cancel;
  end;

  { Immutable save operation retaining copied options and an injected runtime
    host through managed interfaces. Synchronous Execute refuses; BeginInvoke
    creates an independent invocation. Persisted configuration contains options
    only, so data-only reconstruction cannot manufacture permission to save. }
  TSave = class(TNyxLogicOperation, INyxSaveFile, INyxAsyncLogicOperation)
  private
    FOptions: TNyxFileSaveOptions;
    FHost: INyxFileSaveHost;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxFileSaveOptions; const AHost: INyxFileSaveHost);
    function GetOptions: TNyxFileSaveOptions;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
  end;

  { Reference-counted runtime resolver retaining an explicitly supplied save
    host and optional next resolver. Only file-save receives this authority;
    other kinds delegate to Next or the ordinary data-only factory. }
  TFileResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Host: INyxFileSaveHost;
    Next: INyxLogicResolver;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

function NyxFilePort(APort: TNyxFilePort): TNyxFlowPortRef;
const
  CNames: array[TNyxFilePort] of TNyxText = ('trigger', 'bytes', 'outcome', 'name',
    'written', 'delivered', 'cancelled', 'error', 'byte-count', 'activation-required');
begin

  if (Ord(APort) < Ord(Low(TNyxFilePort))) or (Ord(APort) > Ord(High(TNyxFilePort))) then
  begin
    raise ENyxFlow.Create('Unknown file port');
  end;
  Result := NyxFlowPort(CNames[APort]);
end;

constructor TEncode.Create;
begin
  inherited Create;
  FConfiguration := NyxObject([]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('utf8-encode'), 'Encode UTF-8',
    'Encode exact Unicode text as owned UTF-8 bytes without machine codepage conversion.', nlgText)
    .Input(NyxLogicPort(nlpText), nftText, 'Unicode text')
    .Output(NyxFilePort(nfpBytes), nftBytes, 'Owned UTF-8 bytes')
    .Labelled('encoding').Labelled('bytes').Labelled('unicode');
end;

function TEncode.Encode(const AText: TNyxText): TNyxBytes;
begin
  Result := NyxEncodeUTF8(AText);
end;

function TEncode.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxFilePort(nfpBytes),
    NyxFlowBytes(Encode(AInputs.Value(NyxLogicPort(nlpText)).AsText)));
end;

constructor TSave.Create(const AOptions: TNyxFileSaveOptions; const AHost: INyxFileSaveHost);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions.Copy;
  FHost := AHost;
  FConfiguration := FOptions.ToData;
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('file-save'), 'Save file',
    'Save owned bytes through an explicitly supplied host. Write, download request, cancellation and activation are distinct outcomes.',
    nlgFiles, nlaUI)
    .Input(NyxFilePort(nfpTrigger), nftSignal, 'Explicit invocation')
    .Input(NyxFilePort(nfpBytes), nftBytes, 'Complete owned file bytes')
    .Output(NyxFilePort(nfpOutcome), nftInteger, 'TNyxFileSaveOutcome ordinal')
    .Output(NyxFilePort(nfpName), nftText, 'Selected or suggested portable leaf name')
    .Output(NyxFilePort(nfpWritten), nftBoolean, 'Completed host write and close')
    .Output(NyxFilePort(nfpDelivered), nftBoolean, 'Write completed or download requested')
    .Output(NyxFilePort(nfpCancelled), nftBoolean, 'User cancelled the picker')
    .Output(NyxFilePort(nfpError), nftText, 'Host outcome diagnostic')
    .Output(NyxFilePort(nfpByteCount), nftInteger, 'Written/requested byte count')
    .Output(NyxFilePort(nfpActivationRequired), nftBoolean, 'A current user action is needed')
    .Labelled('files').Labelled('save').Labelled('download').Labelled('export');
end;

function TSave.GetOptions: TNyxFileSaveOptions;
begin
  Result := FOptions.Copy;
end;

function TSave.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues;
  raise ENyxFlow.Create('File saving requires asynchronous invocation and an explicit host');
end;

function TSave.BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
var
  LInvocation: TSaveInvocation;
begin

  if (AScheduler = nil) or (ACompletion = nil) or (FHost = nil) then
  begin
    raise ENyxFlow.Create('File saving requires a scheduler, completion and explicit host');
  end;
  AScheduler.RequireUI;
  ValidateNyxLogicFrame(FDescriptor, nfdInput, AInputs);

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('File save cancelled before admission');
  end;
  FOptions.Admit(AInputs.Value(NyxFilePort(nfpBytes)).AsBytes);
  LInvocation := TSaveInvocation.Create(AScheduler, ACancellation, ACompletion);
  Result := LInvocation;
  LInvocation.Start(FOptions, AInputs.Value(NyxFilePort(nfpBytes)).AsBytes, FHost);
end;

constructor TSaveInvocation.Create(const AScheduler: INyxScheduler;
  const AScope: INyxCancellationScope; const ACompletion: INyxLogicCompletion);
begin
  inherited Create;
  FScheduler := AScheduler;
  FScope := AScope;
  FCompletion := ACompletion;
end;

procedure TSaveInvocation.Start(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AHost: INyxFileSaveHost);
var
  LRequest: INyxFileSaveRequest;
begin
  FLease := Self;
  FByteCount := Length(ABytes);
  try
    LRequest := AHost.BeginSave(AOptions, ABytes, FScheduler, FScope, Saved);

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
        raise ENyxFlow.Create('File-save host returned no pending request token');
      end;
      FRequest := LRequest;
    end;
  except
    Cancel;
    raise;
  end;
end;

procedure TSaveInvocation.Cancel;
var
  LLease: INyxLogicInvocation;
  LRequest: INyxFileSaveRequest;
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

procedure TSaveInvocation.Saved(const AResult: TNyxFileSaveResult);
var
  LLease: INyxLogicInvocation;
  LCompletion: INyxLogicCompletion;
  LOutputs: TNyxFlowValues;
  LError: TNyxText;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if (FCompletion = nil) or ((FScope <> nil) and FScope.Cancelled) then
  begin
    Cancel;
    Exit;
  end;
  LError := '';
  try
    ValidateNyxFileSaveResult(AResult, FByteCount);
  except
    on E: Exception do
    begin
      LError := 'The file-save host returned an invalid outcome';
    end;
  end;

  if LError <> '' then
  begin
    LCompletion := FCompletion;
    Cancel;
    LCompletion.Fail(LError);
    Exit;
  end;
  LOutputs := NyxFlowValues
    .WithValue(NyxFilePort(nfpOutcome), NyxFlowInteger(Ord(AResult.Outcome)))
    .WithValue(NyxFilePort(nfpName), NyxFlowText(AResult.Name))
    .WithValue(NyxFilePort(nfpWritten), NyxFlowBoolean(NyxFileSaveWritten(AResult)))
    .WithValue(NyxFilePort(nfpDelivered), NyxFlowBoolean(NyxFileSaveDelivered(AResult)))
    .WithValue(NyxFilePort(nfpCancelled), NyxFlowBoolean(AResult.Outcome = nfsoCancelled))
    .WithValue(NyxFilePort(nfpError), NyxFlowText(AResult.Error))
    .WithValue(NyxFilePort(nfpByteCount), NyxFlowInteger(AResult.ByteCount))
    .WithValue(NyxFilePort(nfpActivationRequired),
      NyxFlowBoolean(AResult.Outcome = nfsoActivationRequired));
  LCompletion := FCompletion;
  FCompletion := nil;
  FScope := nil;
  FRequest := nil;
  FLease := nil;
  LCompletion.Succeed(LOutputs);
end;

function NewNyxUTF8Encode: INyxUTF8Encode;
begin
  Result := TEncode.Create;
end;

function NewNyxSaveFile(const AOptions: TNyxFileSaveOptions;
  const AHost: INyxFileSaveHost): INyxSaveFile;
begin
  Result := TSave.Create(AOptions, AHost);
end;

function TFileResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin

  if AKind.Name = 'file-save' then
  begin
    Exit(NewNyxSaveFile(TNyxFileSaveOptions.FromData(AConfiguration), Host));
  end;

  if Next <> nil then
  begin
    Exit(Next.Resolve(AKind, AConfiguration));
  end;
  Result := NyxLogicFromData(AKind, AConfiguration);
end;

function NewNyxFileLogicResolver(const AHost: INyxFileSaveHost;
  const ANext: INyxLogicResolver): INyxLogicResolver;
var
  LResolver: TFileResolver;
begin
  LResolver := TFileResolver.Create;
  Result := LResolver;
  LResolver.Host := AHost;
  LResolver.Next := ANext;
end;

end.
