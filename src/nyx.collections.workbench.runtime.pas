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
unit nyx.collections.workbench.runtime;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.clipboard, nyx.collections.clipboard,
  nyx.collections.clipboard.actions, nyx.collections.cell.actions,
  nyx.collections.mount, nyx.collections.workbench.policy, nyx.events;

type
  { Runtime services are independent of saved table intent. A renderer retains
    one configured service provider; each mounted command captures its actual
    host and bounds once. Providers must not retain that renderer, its mounted
    tree or controls. Foreign getters may reenter and are fenced against the
    original input incarnation before configuration can continue. }
  INyxTableWorkbenchServices = interface(IInterface)
    ['{8272E2F7-BF30-482D-97CE-7635790A6355}']
    function GetClipboardHost: INyxClipboardWriteHost;
    function GetCopyLimits: TNyxCollectionClipboardLimits;
    property ClipboardHost: INyxClipboardWriteHost read GetClipboardHost;
    property CopyLimits: TNyxCollectionClipboardLimits read GetCopyLimits;
  end;

{ Copies and validates runtime bounds; nil host becomes an explicit Unavailable
  adapter. This never infers OS/browser access from an authored policy. }
function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost = nil):
  INyxTableWorkbenchServices; overload;
function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ALimits: TNyxCollectionClipboardLimits): INyxTableWorkbenchServices; overload;

{ Closed authoring choices map explicitly to the established runtime options.
  Runtime limits are supplied independently and do not enter the saved policy. }
function NyxTableWorkbenchClipboardOptions(const APolicy: TNyxTableWorkbenchPolicy;
  const ALimits: TNyxCollectionClipboardLimits): TNyxCollectionClipboardOptions;

{ Configures one fresh table mount through existing optional contracts. The
  supplied fresh target factory belongs only to this attempt. Preflight refuses
  before changing the mount; after configuration begins, failure disconnects
  that exact partial incarnation and revokes its factory. Callers publish only
  on success. Staged input is allowed; readiness still belongs to its renderer's
  enclosing publication gate. This routine performs no clipboard write. }
procedure ConfigureNyxTableWorkbench(const AMount: INyxCollectionMount;
  const APolicy: TNyxTableWorkbenchPolicy;
  const AServices: INyxTableWorkbenchServices;
  const AFactory: INyxCollectionCellPresenterFactory;
  const AEvents: INyxEvents);

implementation

uses SysUtils, nyx.clipboard.unavailable, nyx.collections.range.commands,
  nyx.scheduler;

type
  { Owned immutable service values. Keeping only host authority and value-only
    work bounds prevents saved documents from retaining application services. }
  TWorkbenchServices = class(TInterfacedObject, INyxTableWorkbenchServices)
  private
    FClipboard: INyxClipboardWriteHost;
    FLimits: TNyxCollectionClipboardLimits;
  public
    constructor Create(const AClipboard: INyxClipboardWriteHost;
      const ALimits: TNyxCollectionClipboardLimits);
    function GetClipboardHost: INyxClipboardWriteHost;
    function GetCopyLimits: TNyxCollectionClipboardLimits;
  end;

constructor TWorkbenchServices.Create(const AClipboard: INyxClipboardWriteHost;
  const ALimits: TNyxCollectionClipboardLimits);
begin
  inherited Create;
  ALimits.Validate;
  FClipboard := AClipboard;

  if FClipboard = nil then
  begin
    FClipboard := NewNyxUnavailableClipboardWriteHost;
  end;
  FLimits := ALimits;
end;

function TWorkbenchServices.GetClipboardHost: INyxClipboardWriteHost;
begin
  Result := FClipboard;
end;

function TWorkbenchServices.GetCopyLimits: TNyxCollectionClipboardLimits;
begin
  Result := FLimits;
end;

function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost):
  INyxTableWorkbenchServices;
begin
  Result := TWorkbenchServices.Create(AClipboard,
    TNyxCollectionClipboardOptions.Defaults.WorkLimits);
end;

function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ALimits: TNyxCollectionClipboardLimits): INyxTableWorkbenchServices;
begin
  Result := TWorkbenchServices.Create(AClipboard, ALimits);
end;

function NyxTableWorkbenchClipboardOptions(const APolicy: TNyxTableWorkbenchPolicy;
  const ALimits: TNyxCollectionClipboardLimits): TNyxCollectionClipboardOptions;
var
  LActions: TNyxCollectionClipboardActions;
  LPolicy: TNyxTableWorkbenchPolicy;
begin
  LPolicy := APolicy.Copy;
  LPolicy.Validate;
  LActions := [];

  if LPolicy.AllowsCopy(nwcaCell) then
  begin
    Include(LActions, nccaCell);
  end;

  if LPolicy.AllowsCopy(nwcaSelectedSource) then
  begin
    Include(LActions, nccaSelectedSource);
  end;

  if LPolicy.AllowsCopy(nwcaSelectedPage) then
  begin
    Include(LActions, nccaSelectedPage);
  end;

  if LPolicy.AllowsCopy(nwcaCellRange) then
  begin
    Include(LActions, nccaCellRange);
  end;
  Result := TNyxCollectionClipboardOptions.Defaults.AllowOnly(LActions)
    .Headers(LPolicy.IncludeHeaders).Limits(ALimits);
  case LPolicy.FieldChoice of
    nwcfVisiblePresentation:
    begin
      Result := Result.Fields(nccfVisiblePresentation);
    end;
    nwcfAllArrangement:
    begin
      Result := Result.Fields(nccfAllArrangement);
    end;
  end;
  case LPolicy.Lines of
    nwleLF:
    begin
      Result := Result.LineEnding(nccleLF);
    end;
    nwleCRLF:
    begin
      Result := Result.LineEnding(nccleCRLF);
    end;
  end;
end;

procedure ConfigureNyxTableWorkbench(const AMount: INyxCollectionMount;
  const APolicy: TNyxTableWorkbenchPolicy;
  const AServices: INyxTableWorkbenchServices;
  const AFactory: INyxCollectionCellPresenterFactory;
  const AEvents: INyxEvents);
var
  LMount: INyxCollectionMount;
  LInputAdmission: INyxCollectionInputAdmission;
  LInput: INyxCollectionInputLifetime;
  LClipboard: INyxCollectionClipboardMount;
  LRange: INyxCollectionCellRangeMount;
  LActions: INyxCollectionCellActionOptionsMount;
  LServices: INyxTableWorkbenchServices;
  LHost: INyxClipboardWriteHost;
  LFactory: INyxCollectionCellPresenterFactory;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LPolicy: TNyxTableWorkbenchPolicy;
  LLimits: TNyxCollectionClipboardLimits;
  LOptions: TNyxCollectionClipboardOptions;
  LEpoch: Integer;

  procedure RequireCurrent;
  begin

    if (LInput = nil) or not LInput.Current or (LInput.Epoch <> LEpoch) or
      not LMount.Connected or not LInput.Current or (LInput.Epoch <> LEpoch) then
    begin
      raise ENyxCollectionClipboard.Create('Table retired while preparing its authored actions');
    end;
  end;

  procedure RequireFresh;
  var
    LExisting: INyxCollectionClipboardCommands;
  begin
    LExisting := LClipboard.ClipboardCommands;
    RequireCurrent;

    if LExisting <> nil then
    begin
      raise ENyxCollectionClipboard.Create('Authored table actions require an unconfigured clipboard mount');
    end;
  end;

begin
  LMount := AMount;
  LFactory := AFactory;
  LEvents := AEvents;
  LServices := AServices;
  LPolicy := APolicy.Copy;
  LPolicy.Validate;

  if (LMount = nil) or (LFactory = nil) or (LEvents = nil) or
    not Supports(LMount, INyxCollectionInputAdmission, LInputAdmission) then
  begin
    raise ENyxCollectionClipboard.Create('Authored table actions require a fresh managed mount');
  end;
  LInput := LInputAdmission.GetInputLifetime;

  if LInput = nil then
  begin
    raise ENyxCollectionClipboard.Create('Authored table actions require original input ownership');
  end;
  LEpoch := LInput.Epoch;
  RequireCurrent;

  if not Supports(LMount, INyxCollectionClipboardMount, LClipboard) or
    not Supports(LMount, INyxCollectionCellRangeMount, LRange) or
    not Supports(LMount, INyxCollectionCellActionOptionsMount, LActions) then
  begin
    raise ENyxCollectionClipboard.Create('This table target does not support authored workbench actions');
  end;
  RequireCurrent;
  RequireFresh;

  if LServices = nil then
  begin
    LServices := NewNyxTableWorkbenchServices;
  end;
  LHost := LServices.ClipboardHost;
  RequireCurrent;
  LLimits := LServices.CopyLimits;
  RequireCurrent;
  LOptions := NyxTableWorkbenchClipboardOptions(LPolicy, LLimits);

  if LHost = nil then
  begin
    LHost := NewNyxUnavailableClipboardWriteHost;
  end;
  LScheduler := LEvents.Scheduler;
  RequireCurrent;

  if LScheduler = nil then
  begin
    raise ENyxCollectionClipboard.Create('Authored table actions require a UI scheduler');
  end;
  LScheduler.RequireUI;
  RequireCurrent;
  RequireFresh;
  try
    LClipboard.ConfigureClipboard(LHost, LEvents, LScheduler, LOptions);
    { Clipboard admission may synchronize an already disabled legacy table's
      interaction epoch. That deliberate own transition precedes our remaining
      one-time configuration; it does not admit a captured physical input. }
    LEpoch := LInput.Epoch;
    RequireCurrent;
    LRange.ConfigureCellRange(LPolicy.RangeSelectionEnabled);
    RequireCurrent;
    LActions.ConfigureCellActionsWithOptions(LFactory, LScheduler,
      TNyxCollectionCellActionOptions.Defaults.FocusedDraft(LPolicy.HasFocusedDraft));
    RequireCurrent;
  except
    try
      LMount.Disconnect;
    except
      { Preserve the original admission failure after revoking partial input. }
    end;
    try
      LFactory.Revoke;
    except
      { Factory cleanup cannot replace the original failure. }
    end;
    raise;
  end;
end;

end.
