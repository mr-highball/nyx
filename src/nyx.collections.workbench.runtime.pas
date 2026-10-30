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
  nyx.collections.mount, nyx.collections.workbench.policy, nyx.events,
  nyx.collections.export, nyx.collections.export.actions, nyx.files.save;

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

  { Optional file authority extends a runtime service provider without changing
    its original clipboard ABI. Host/options are captured once before partial
    mount configuration. Providers must not retain a renderer, mount or tree.
    Absence keeps legacy behavior; authored export without this facet uses an
    explicit Unavailable host. Presence requires an explicit host
    and configures that exact export controller BEFORE managed cell actions.
    Foreign getters may reenter, so each is followed by the original input fence. }
  INyxTableWorkbenchExportServices = interface(IInterface)
    ['{0F5D81F3-CBEA-4D91-A15C-E85CD5D3D2A5}']
    function GetFileSaveHost: INyxFileSaveHost;
    function GetExportOptions: TNyxCollectionExportOptions;
    property FileSaveHost: INyxFileSaveHost read GetFileSaveHost;
    property ExportOptions: TNyxCollectionExportOptions read GetExportOptions;
  end;

{ Copies and validates runtime bounds; nil host becomes an explicit Unavailable
  adapter. This never infers OS/browser access from an authored policy. }
function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost = nil):
  INyxTableWorkbenchServices; overload;
function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ALimits: TNyxCollectionClipboardLimits): INyxTableWorkbenchServices; overload;

{ Export opt-in supplies real runtime authority independently of saved policy.
  Options are validated/deep-copied. Nil SaveHost refuses before any mount changes;
  omit this facet to keep exports absent, or inject a host reporting Unavailable.
  Clipboard=nil retains its established explicit Unavailable adapter behavior. }
function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ASaveHost: INyxFileSaveHost; const AExportOptions: TNyxCollectionExportOptions):
  INyxTableWorkbenchServices; overload;
function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ASaveHost: INyxFileSaveHost; const AExportOptions: TNyxCollectionExportOptions;
  const ACopyLimits: TNyxCollectionClipboardLimits): INyxTableWorkbenchServices; overload;

{ Closed authoring choices map explicitly to the established runtime options.
  Runtime limits are supplied independently and do not enter the saved policy. }
function NyxTableWorkbenchClipboardOptions(const APolicy: TNyxTableWorkbenchPolicy;
  const ALimits: TNyxCollectionClipboardLimits): TNyxCollectionClipboardOptions;

{ Overlay authored representation on copied runtime options. Host save hints
  and byte/work bounds survive unchanged. Membership permissions intersect the
  runtime ceiling; a saved document cannot broaden caller-supplied authority. }
function NyxTableWorkbenchExportOptions(const APolicy: TNyxTableExportPolicy;
  const AOptions: TNyxCollectionExportOptions): TNyxCollectionExportOptions;

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

{ Configure readonly recovery presentation for a table without authored Copy
  actions. No clipboard/export/range service is installed. The fresh factory
  and original input lifetime have the same publication/retirement rules as
  the complete workbench; ordinary source edits keep their existing ownership. }
procedure ConfigureNyxTableRecoveryReview(const AMount: INyxCollectionMount;
  const AFactory: INyxCollectionCellPresenterFactory; const AEvents: INyxEvents);

implementation

uses SysUtils, nyx.collections, nyx.clipboard.unavailable, nyx.files.save.unavailable,
  nyx.collections.range.commands,
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

  { Owned optional export services retain only explicit hosts and copied values.
    The base keeps clipboard behavior identical; no policy or document is owned. }
  TWorkbenchExportServices = class(TWorkbenchServices, INyxTableWorkbenchExportServices)
  private
    FSaveHost: INyxFileSaveHost;
    FExportOptions: TNyxCollectionExportOptions;
  public
    constructor Create(const AClipboard: INyxClipboardWriteHost;
      const ASaveHost: INyxFileSaveHost; const AExportOptions: TNyxCollectionExportOptions;
      const ACopyLimits: TNyxCollectionClipboardLimits);
    function GetFileSaveHost: INyxFileSaveHost;
    function GetExportOptions: TNyxCollectionExportOptions;
  end;

constructor TWorkbenchExportServices.Create(const AClipboard: INyxClipboardWriteHost;
  const ASaveHost: INyxFileSaveHost; const AExportOptions: TNyxCollectionExportOptions;
  const ACopyLimits: TNyxCollectionClipboardLimits);
begin
  inherited Create(AClipboard, ACopyLimits);

  if ASaveHost = nil then
  begin
    raise ENyxCollectionExport.Create('Export services require an explicit file-save host');
  end;
  FSaveHost := ASaveHost;
  FExportOptions := AExportOptions.Copy;
end;

function TWorkbenchExportServices.GetFileSaveHost: INyxFileSaveHost;
begin
  Result := FSaveHost;
end;

function TWorkbenchExportServices.GetExportOptions: TNyxCollectionExportOptions;
begin
  Result := FExportOptions.Copy;
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

function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ASaveHost: INyxFileSaveHost; const AExportOptions: TNyxCollectionExportOptions):
  INyxTableWorkbenchServices;
begin
  Result := TWorkbenchExportServices.Create(AClipboard, ASaveHost, AExportOptions,
    TNyxCollectionClipboardOptions.Defaults.WorkLimits);
end;

function NewNyxTableWorkbenchServices(const AClipboard: INyxClipboardWriteHost;
  const ASaveHost: INyxFileSaveHost; const AExportOptions: TNyxCollectionExportOptions;
  const ACopyLimits: TNyxCollectionClipboardLimits): INyxTableWorkbenchServices;
begin
  Result := TWorkbenchExportServices.Create(AClipboard, ASaveHost, AExportOptions, ACopyLimits);
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

function NyxTableWorkbenchExportOptions(const APolicy: TNyxTableExportPolicy;
  const AOptions: TNyxCollectionExportOptions): TNyxCollectionExportOptions;
var
  LAllowed: TNyxCollectionExportMemberships;
begin
  APolicy.Validate;
  LAllowed := [];

  if APolicy.Allows(nweaCurrentPage) then
  begin
    Include(LAllowed, ncerCurrentPage);
  end;

  if APolicy.Allows(nweaAllFiltered) then
  begin
    Include(LAllowed, ncerAllFiltered);
  end;
  Result := AOptions.Copy.AllowRows(LAllowed * AOptions.AllowedRows)
    .Headers(APolicy.IncludeHeaders);
  case APolicy.FieldChoice of
    nwcfVisiblePresentation:
      begin
        Result := Result.Fields(nccfVisiblePresentation);
      end;
    nwcfAllArrangement:
      begin
        Result := Result.Fields(nccfAllArrangement);
      end;
  end;
  case APolicy.Lines of
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
  LInputPreparation: INyxCollectionInputPreparation;
  LInput: INyxCollectionInputLifetime;
  LClipboard: INyxCollectionClipboardMount;
  LRange: INyxCollectionCellRangeMount;
  LActions: INyxCollectionCellActionOptionsMount;
  LServices: INyxTableWorkbenchServices;
  LExportServices: INyxTableWorkbenchExportServices;
  LExport: INyxCollectionExportMount;
  LSaveHost: INyxFileSaveHost;
  LExportOptions: TNyxCollectionExportOptions;
  LHost: INyxClipboardWriteHost;
  LFactory: INyxCollectionCellPresenterFactory;
  LEvents: INyxEvents;
  LScheduler: INyxScheduler;
  LPolicy: TNyxTableWorkbenchPolicy;
  LLimits: TNyxCollectionClipboardLimits;
  LOptions: TNyxCollectionClipboardOptions;
  LEpoch: Integer;
  LWantExport: Boolean;

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
    LExistingExport: INyxCollectionExportCommands;
    LExportMount: INyxCollectionExportMount;
  begin
    LExisting := LClipboard.ClipboardCommands;
    RequireCurrent;

    if LExisting <> nil then
    begin
      raise ENyxCollectionClipboard.Create('Authored table actions require an unconfigured clipboard mount');
    end;

    if ((LExport <> nil) or LPolicy.ExportPolicy.Defined) and
      Supports(LMount, INyxCollectionExportMount, LExportMount) then
    begin
      RequireCurrent;
      LExistingExport := LExportMount.ExportCommands;
      RequireCurrent;

      if LExistingExport <> nil then
      begin
        raise ENyxCollectionExport.Create('Authored export services require an unconfigured export mount');
      end;
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
  { Synchronize only privately owned input flags before reserving authority.
    This makes an initially disabled legacy table legitimate without adopting a
    newer epoch after any foreign getter or scheduler has had a chance to run.
    Older providers may omit this facet, but still must keep the original epoch. }

  if Supports(LMount, INyxCollectionInputPreparation, LInputPreparation) then
  begin
    LInputPreparation.PrepareInputTracking;
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

  LWantExport := not LPolicy.ExportPolicy.Defined or (LPolicy.ExportPolicy.Actions <> []);

  if LWantExport and Supports(LServices, INyxTableWorkbenchExportServices, LExportServices) then
  begin
    RequireCurrent;

    if not Supports(LMount, INyxCollectionExportMount, LExport) then
    begin
      raise ENyxCollectionExport.Create('This table target does not support export services');
    end;
    RequireCurrent;
    LSaveHost := LExportServices.FileSaveHost;
    RequireCurrent;
    LExportOptions := LExportServices.ExportOptions.Copy;
    RequireCurrent;

    if LSaveHost = nil then
    begin
      raise ENyxCollectionExport.Create('Export services require an explicit file-save host');
    end;
  end;

  if LPolicy.ExportPolicy.Defined and LWantExport then
  begin

    if LExport = nil then
    begin

      if not Supports(LMount, INyxCollectionExportMount, LExport) then
      begin
        raise ENyxCollectionExport.Create('This table target does not support authored export');
      end;
      RequireCurrent;
      LSaveHost := NewNyxUnavailableFileSaveHost;
      LExportOptions := TNyxCollectionExportOptions.Defaults;
    end;
    LExportOptions := NyxTableWorkbenchExportOptions(LPolicy.ExportPolicy, LExportOptions);
  end;
  RequireCurrent;

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
    RequireCurrent;
    LRange.ConfigureCellRange(LPolicy.RangeSelectionEnabled);
    RequireCurrent;

    if LExport <> nil then
    begin
      LExport.ConfigureExport(LSaveHost, LEvents, LScheduler, LExportOptions);
      RequireCurrent;
    end;
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

procedure ConfigureNyxTableRecoveryReview(const AMount: INyxCollectionMount;
  const AFactory: INyxCollectionCellPresenterFactory; const AEvents: INyxEvents);
var
  LMount: INyxCollectionMount;
  LFactory: INyxCollectionCellPresenterFactory;
  LAdmission: INyxCollectionInputAdmission;
  LPreparation: INyxCollectionInputPreparation;
  LInput: INyxCollectionInputLifetime;
  LActions: INyxCollectionCellActionOptionsMount;
  LScheduler: INyxScheduler;
  LEpoch: Integer;

  procedure RequireCurrent;
  begin

    if not LMount.Connected or not LInput.Current or (LInput.Epoch <> LEpoch) then
    begin
      raise ENyxCollection.Create('Table retired while configuring recovery review');
    end;
  end;

begin
  LMount := AMount;
  LFactory := AFactory;

  if (LMount = nil) or (LFactory = nil) or (AEvents = nil) or
    not Supports(LMount, INyxCollectionInputAdmission, LAdmission) or
    not Supports(LMount, INyxCollectionCellActionOptionsMount, LActions) then
  begin
    raise ENyxCollection.Create('Recovery review requires a fresh managed table');
  end;

  if Supports(LMount, INyxCollectionInputPreparation, LPreparation) then
  begin
    LPreparation.PrepareInputTracking;
  end;
  LInput := LAdmission.GetInputLifetime;

  if LInput = nil then
  begin
    raise ENyxCollection.Create('Recovery review requires original input ownership');
  end;
  LEpoch := LInput.Epoch;
  RequireCurrent;
  LScheduler := AEvents.Scheduler;
  RequireCurrent;

  if LScheduler = nil then
  begin
    raise ENyxCollection.Create('Recovery review requires a UI scheduler');
  end;
  LScheduler.RequireUI;
  RequireCurrent;
  try
    LActions.ConfigureCellActionsWithOptions(LFactory, LScheduler,
      TNyxCollectionCellActionOptions.Defaults);
    RequireCurrent;
  except
    try
      LMount.Disconnect;
    except
      { Preserve original admission failure while retiring this partial owner. }
    end;
    try
      LFactory.Revoke;
    except
      { Target cleanup cannot replace the original failure. }
    end;
    raise;
  end;
end;

end.
