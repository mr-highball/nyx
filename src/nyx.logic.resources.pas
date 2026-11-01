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
unit nyx.logic.resources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.resources, nyx.resources.loader,
  nyx.application.resources, nyx.logic, nyx.flow, nyx.flow.types, nyx.scheduler;

const
  NyxJSONResourceLogicKind = 'json-resource';

type
  { Closed ports keep resource identity, exact data and diagnostic evidence
    separate. Reference text can select an existing declaration; it grants no URL
    or transport authority. Cache/origin ordinals use the public loader enums. }
  TNyxJSONResourcePort = (njrpTrigger, njrpReference, njrpJSON, njrpSucceeded,
    njrpError, njrpOrigin, njrpCacheWarning, njrpCacheRead, njrpCacheWrite);

  { Immutable authoring intent. The nominal resource reference and whole-request
    deadline are portable data. Locale and cache policy belong to the captured
    application/resource declarations, never duplicated in a graph configuration. }
  TNyxJSONResourceOptions = record
  private
    FReference: TNyxResourceRef;
    FLoad: TNyxResourceLoadOptions;
  public
    function WholeRequest(AMilliseconds: Integer): TNyxJSONResourceOptions;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxJSONResourceOptions; static;
    property Reference: TNyxResourceRef read FReference;
    property LoadOptions: TNyxResourceLoadOptions read FLoad;
  end;

  { Managed JSON resource operation shares the application's resolver/cache.
    Capture selects the authored locale/fallback at invocation. No runtime entry
    is substituted for that declaration. Ordinary load failure is a complete
    frame (JSON null, Succeeded=False and diagnostics); cancellation publishes
    nothing. Missing capability, reference or wrong resource kind refuses before
    I/O. Direct Pascal callers use the same asynchronous contract as graphs. }
  INyxJSONResource = interface(INyxLogicOperation)
    ['{BAB24C62-891B-4FBC-802B-2E5E67F7A7B6}']
    function GetOptions: TNyxJSONResourceOptions;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
    property Options: TNyxJSONResourceOptions read GetOptions;
  end;

function NyxJSONResourceOptions(const AReference: TNyxResourceRef): TNyxJSONResourceOptions;
function NyxJSONResourcePort(APort: TNyxJSONResourcePort): TNyxFlowPortRef;
function NewNyxJSONResource(const AOptions: TNyxJSONResourceOptions;
  const ASource: INyxResourceReadSource = nil): INyxJSONResource;
{ Runtime authority is explicitly composed around existing creator/file/HTTP
  resolvers. Saved data and catalog discovery do not supply a source capability. }
function NewNyxResourceLogicResolver(const ASource: INyxResourceReadSource;
  const ANext: INyxLogicResolver = nil): INyxLogicResolver;

implementation

type
  { Owns one pending load and completion. The method receiver is borrowed by the
    loader only until completion/cancellation; a temporary self lease spans that
    interval. Retirement disconnects output before invoking external Cancel. }
  TJSONResourceInvocation = class(TInterfacedObject, INyxLogicInvocation)
  private
    FScheduler: INyxScheduler;
    FScope: INyxCancellationScope;
    FCompletion: INyxLogicCompletion;
    FLoad: INyxResourceLoad;
    FLease: INyxLogicInvocation;
    procedure Loaded(const AResult: TNyxResourceLoadResult);
  public
    procedure Start(const APlan: INyxResourceReadPlan;
      const AOptions: TNyxResourceLoadOptions);
    procedure Cancel;
  end;

  { Immutable configured component retains its independent read source facet,
    never an editor, control or application. Each invocation captures a new plan. }
  TJSONResource = class(TNyxLogicOperation, INyxJSONResource, INyxAsyncLogicOperation)
  private
    FOptions: TNyxJSONResourceOptions;
    FSource: INyxResourceReadSource;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const AOptions: TNyxJSONResourceOptions;
      const ASource: INyxResourceReadSource);
    function GetOptions: TNyxJSONResourceOptions;
    function BeginInvoke(const AInputs: TNyxFlowValues; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
  end;

  { Retains independent host authority and the next resolver, without acquiring
    authority for unrecognized kinds or changing their configured schemas. }
  TResourceLogicResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Source: INyxResourceReadSource;
    Next: INyxLogicResolver;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

function NyxJSONResourceOptions(const AReference: TNyxResourceRef): TNyxJSONResourceOptions;
begin
  Result.FReference := NyxResourceRef(AReference.Name);
  Result.FLoad := NyxResourceLoadOptions;
end;

function TNyxJSONResourceOptions.WholeRequest(
  AMilliseconds: Integer): TNyxJSONResourceOptions;
begin
  Result := Self;
  Result.FLoad := FLoad.WholeRequest(AMilliseconds);
end;

function TNyxJSONResourceOptions.ToData: TNyxDataValue;
begin
  NyxResourceRef(FReference.Name);
  FLoad.Validate;
  Result := NyxObject([NyxField('resource', NyxData(FReference.Name)),
    NyxField('deadline', NyxData(FLoad.DeadlineMS))]);
end;

class function TNyxJSONResourceOptions.FromData(
  const AData: TNyxDataValue): TNyxJSONResourceOptions;
var
  LDeadline: Integer;
begin
  ValidateNyxFlowFields(AData, ['resource', 'deadline']);
  { Check the bounded exact integer before narrowing it on either compiler. }

  if (AData.Field('deadline').AsInteger < 1) or
    (AData.Field('deadline').AsInteger > 120000) then
  begin
    raise ENyxFlow.Create('Resource deadline requires 1..120000 milliseconds');
  end;
  LDeadline := AData.Field('deadline').AsInteger;
  Result := NyxJSONResourceOptions(NyxResourceRef(AData.Field('resource').AsText))
    .WholeRequest(LDeadline);
end;

function NyxJSONResourcePort(APort: TNyxJSONResourcePort): TNyxFlowPortRef;
const
  CNames: array[TNyxJSONResourcePort] of TNyxText =
    ('trigger', 'reference', 'json', 'succeeded', 'error', 'origin',
     'cache-warning', 'cache-read', 'cache-write');
begin

  if (Ord(APort) < Ord(Low(TNyxJSONResourcePort))) or
    (Ord(APort) > Ord(High(TNyxJSONResourcePort))) then
  begin
    raise ENyxFlow.Create('Unknown JSON resource port');
  end;
  Result := NyxFlowPort(CNames[APort]);
end;

procedure TJSONResourceInvocation.Start(const APlan: INyxResourceReadPlan;
  const AOptions: TNyxResourceLoadOptions);
var
  LLoad: INyxResourceLoad;
  LLease: INyxLogicInvocation;
begin
  FScheduler.RequireUI;
  LLease := Self;
  FLease := Self;
  try
    LLoad := APlan.Load(AOptions, Loaded);

    if FCompletion = nil then
    begin

      if LLoad <> nil then
      begin
        LLoad.Cancel;
      end;
    end
    else
    begin

      if LLoad = nil then
      begin
        raise ENyxFlow.Create('Resource loader returned no pending load token');
      end;
      FLoad := LLoad;
    end;
  except
    Cancel;
    raise;
  end;
end;

procedure TJSONResourceInvocation.Cancel;
var
  LLease: INyxLogicInvocation;
  LLoad: INyxResourceLoad;
begin
  FScheduler.RequireUI;
  LLease := Self;
  FCompletion := nil;
  FScope := nil;
  LLoad := FLoad;
  FLoad := nil;
  FLease := nil;

  if LLoad <> nil then
  begin
    LLoad.Cancel;
  end;
end;

procedure TJSONResourceInvocation.Loaded(const AResult: TNyxResourceLoadResult);
var
  LLease: INyxLogicInvocation;
  LCompletion: INyxLogicCompletion;
  LDefinition: INyxResourceDefinition;
  LJSON: TNyxDataValue;
  LOutputs: TNyxFlowValues;
begin
  FScheduler.RequireUI;
  LLease := Self;

  if (FCompletion = nil) or ((FScope <> nil) and FScope.Cancelled) then
  begin
    Cancel;
    Exit;
  end;
  LCompletion := FCompletion;
  FCompletion := nil;
  FLoad := nil;
  FScope := nil;
  FLease := nil;
  try
    LJSON := NyxNull;

    if AResult.Succeeded then
    begin
      LDefinition := AResult.Definition;

      if (LDefinition = nil) or (LDefinition.Kind <> nrkJSON) then
      begin
        raise ENyxFlow.Create('Resource loader returned a non-JSON definition');
      end;
      LJSON := LDefinition.Data;
    end;
    LOutputs := NyxFlowValues
      .WithValue(NyxJSONResourcePort(njrpJSON), NyxFlowJSON(LJSON))
      .WithValue(NyxJSONResourcePort(njrpSucceeded), NyxFlowBoolean(AResult.Succeeded))
      .WithValue(NyxJSONResourcePort(njrpError), NyxFlowText(AResult.Error))
      .WithValue(NyxJSONResourcePort(njrpOrigin), NyxFlowInteger(Ord(AResult.Origin)))
      .WithValue(NyxJSONResourcePort(njrpCacheWarning), NyxFlowText(AResult.CacheWarning))
      .WithValue(NyxJSONResourcePort(njrpCacheRead), NyxFlowInteger(Ord(AResult.CacheRead)))
      .WithValue(NyxJSONResourcePort(njrpCacheWrite), NyxFlowInteger(Ord(AResult.CacheWrite)));
  except
    on LException: Exception do
    begin
      LCompletion.Fail(LException.Message);
      Exit;
    end;
  end;
  LCompletion.Succeed(LOutputs);
end;

constructor TJSONResource.Create(const AOptions: TNyxJSONResourceOptions;
  const ASource: INyxResourceReadSource);
begin
  inherited Create;
  FConfiguration := AOptions.ToData;
  FOptions := TNyxJSONResourceOptions.FromData(FConfiguration);
  FSource := ASource;
  FDescriptor := NyxLogicDescriptor(NyxLogicKind(NyxJSONResourceLogicKind),
    'Read JSON resource',
    'Load declared embedded or hosted JSON with locale, cache and fallback policy. ' +
    'Keep exact data, loading errors and cache evidence available to connected controls.',
    nlgData, nlaUI)
    .Input(NyxJSONResourcePort(njrpTrigger), nftSignal, 'Explicit invocation trigger')
    .OptionalInput(NyxJSONResourcePort(njrpReference), nftText,
      'Declared resource name for this invocation; absence keeps the configured reference')
    .Output(NyxJSONResourcePort(njrpJSON), nftJSON, 'Exact JSON; null after failed loading')
    .Output(NyxJSONResourcePort(njrpSucceeded), nftBoolean, 'Admitted data, including allowed fallback')
    .Output(NyxJSONResourcePort(njrpError), nftText, 'Loading failure; retained after stale/fallback success')
    .Output(NyxJSONResourcePort(njrpOrigin), nftInteger, 'TNyxResourceLoadOrigin evidence')
    .Output(NyxJSONResourcePort(njrpCacheWarning), nftText, 'Storage warning independent of load success')
    .Output(NyxJSONResourcePort(njrpCacheRead), nftInteger, 'TNyxResourceCacheUse for successful reads')
    .Output(NyxJSONResourcePort(njrpCacheWrite), nftInteger, 'TNyxResourceCacheUse for successful writes')
    .Labelled('resource').Labelled('json').Labelled('hosted').Labelled('embedded')
    .Labelled('cache').Labelled('data');
end;

function TJSONResource.GetOptions: TNyxJSONResourceOptions;
begin
  Result := FOptions;
end;

function TJSONResource.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues;
  raise ENyxFlow.Create('Resource loading requires BeginInvoke on the UI scheduler');
end;

function TJSONResource.BeginInvoke(const AInputs: TNyxFlowValues;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  const ACompletion: INyxLogicCompletion): INyxLogicInvocation;
var
  LReference: TNyxResourceRef;
  LPlan: INyxResourceReadPlan;
  LDefinition: INyxResourceDefinition;
  LInvocation: TJSONResourceInvocation;
begin

  if (AScheduler = nil) or (ACompletion = nil) or (FSource = nil) then
  begin
    raise ENyxFlow.Create('Resource invocation requires scheduler, completion and read capability');
  end;
  AScheduler.RequireUI;
  ValidateNyxLogicFrame(FDescriptor, nfdInput, AInputs);

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('Resource invocation cancelled before admission');
  end;
  LReference := FOptions.Reference;

  if AInputs.Has(NyxJSONResourcePort(njrpReference)) then
  begin
    LReference := NyxResourceRef(AInputs.Value(NyxJSONResourcePort(njrpReference)).AsText);
  end;
  LPlan := FSource.CaptureRead(LReference);

  if LPlan = nil then
  begin
    raise ENyxFlow.Create('JSON resource source returned no read plan');
  end;
  LDefinition := LPlan.Definition;

  if (LDefinition = nil) or (LDefinition.Kind <> nrkJSON) then
  begin
    raise ENyxFlow.Create('JSON resource invocation requires a declared JSON resource');
  end;

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('Resource invocation cancelled during admission');
  end;
  LInvocation := TJSONResourceInvocation.Create;
  Result := LInvocation;
  LInvocation.FScheduler := AScheduler;
  LInvocation.FScope := ACancellation;
  LInvocation.FCompletion := ACompletion;
  LInvocation.Start(LPlan, FOptions.LoadOptions);
end;

function NewNyxJSONResource(const AOptions: TNyxJSONResourceOptions;
  const ASource: INyxResourceReadSource): INyxJSONResource;
begin
  Result := TJSONResource.Create(AOptions, ASource);
end;

function TResourceLogicResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
begin

  if AKind.Name = NyxJSONResourceLogicKind then
  begin
    Result := NewNyxJSONResource(TNyxJSONResourceOptions.FromData(AConfiguration), Source);
  end
  else if Next <> nil then
  begin
    Result := Next.Resolve(AKind, AConfiguration);
  end
  else
  begin
    Result := NyxLogicFromData(AKind, AConfiguration);
  end;
end;

function NewNyxResourceLogicResolver(const ASource: INyxResourceReadSource;
  const ANext: INyxLogicResolver): INyxLogicResolver;
var
  LResolver: TResourceLogicResolver;
begin

  if ASource = nil then
  begin
    raise ENyxFlow.Create('Resource Logic resolver requires a read capability');
  end;
  LResolver := TResourceLogicResolver.Create;
  Result := LResolver;
  LResolver.Source := ASource;
  LResolver.Next := ANext;
end;

end.
