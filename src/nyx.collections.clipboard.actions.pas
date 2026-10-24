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
unit nyx.collections.clipboard.actions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.collections, nyx.collections.arrangement,
  nyx.collections.clipboard, nyx.collections.representation,
  nyx.clipboard, nyx.scheduler, nyx.events;

type
  { Closed accepted-raw commands distinguish complete source membership from
    the explicit current page. CellRange is append-only and explicitly opt-in;
    no command implicitly copies an editor draft or changes row selection. }
  TNyxCollectionClipboardAction = (nccaCell, nccaSelectedSource, nccaSelectedPage,
    nccaCellRange);

  { Runtime opt-in permissions, independent of data editability and authored
    layout features. Read-only data can still be inspected/copied. }
  TNyxCollectionClipboardActions = set of TNyxCollectionClipboardAction;

  { Runtime configuration contains no authority or saved document mutation.
    Defaults permit all three commands, visible presentation fields, field-name
    headers and LF rows. Fields/headers/line endings apply only to row commands. }
  TNyxCollectionClipboardOptions = record
  private
    FDefined: Boolean;
    FActions: TNyxCollectionClipboardActions;
    FFields: TNyxCollectionCopyFields;
    FHeaders: Boolean;
    FLines: TNyxCollectionCopyLineEnding;
    FLimits: TNyxCollectionClipboardLimits;
  public
    class function Defaults: TNyxCollectionClipboardOptions; static;
    function AllowOnly(AValue: TNyxCollectionClipboardActions): TNyxCollectionClipboardOptions;
    function Fields(AValue: TNyxCollectionCopyFields): TNyxCollectionClipboardOptions;
    function Headers(AValue: Boolean): TNyxCollectionClipboardOptions;
    function LineEnding(AValue: TNyxCollectionCopyLineEnding): TNyxCollectionClipboardOptions;
    function Limits(const AValue: TNyxCollectionClipboardLimits): TNyxCollectionClipboardOptions;
    procedure Validate;
    property Actions: TNyxCollectionClipboardActions read FActions;
    property FieldChoice: TNyxCollectionCopyFields read FFields;
    property IncludeHeaders: Boolean read FHeaders;
    property Lines: TNyxCollectionCopyLineEnding read FLines;
    property WorkLimits: TNyxCollectionClipboardLimits read FLimits;
  end;

  { Pending is an entered host operation, not a confirmed clipboard effect.
    Cancelled is silent receiver withdrawal; it cannot undo an OS write. Only
    Completed has a host result, which may itself be unavailable/denied/failed. }
  TNyxCollectionClipboardPhase = (nccpIdle, nccpPending, nccpCompleted, nccpCancelled);

  { A borrowed mounted owner supplies a newly captured callback-free final fence
    for each command. The owner remains alive while its canonical input Current
    is true and revokes this callback before retiring. Each returned fence may
    retain the mount only for synchronous preparation/dispatch; it must never
    enter the asynchronous host cancellation scope. }
  TNyxCollectionClipboardReadinessFactory = function: INyxCancellationReadiness of object;

  { A single-use pre-canonical command. Begin reserves original input/intent;
    Prepare copies model/selection/page/typed payload BEFORE semantic callbacks.
    Current admits defaults; DispatchCurrent preserves remaining canonical
    phases after benign edits while refusing a retired original incarnation.
    Before Prepare, Current admits physical entry only. A disallowed action
    still permits canonical key delivery, without capturing data or default.
    Complete=True means claimed, including a consumed/stale default. It never
    recaptures a newer row, selection, model or host. Retire is flags-only and
    never cancels an already transferred asynchronous clipboard operation. }
  INyxCollectionClipboardCommand = interface(IInterface)
    ['{BCC77504-54AC-4C5E-9819-940F5E898CBC}']
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetPayload: INyxCollectionClipboardText;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
    property Current: Boolean read GetCurrent;
    property DispatchCurrent: Boolean read GetDispatchCurrent;
    property Payload: INyxCollectionClipboardText read GetPayload;
  end;

  { Physical adapters can reserve original input/intent BEFORE an outward
    snapshot item getter. ResolveCell supplies identities once, without taking
    a new epoch, serial or mount fence. A stale reservation stays denied. }
  INyxCollectionClipboardCellCommand = interface(INyxCollectionClipboardCommand)
    ['{E3A8915D-0F15-429B-A2E5-CAC76ED02DAB}']
    procedure ResolveCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity);
  end;

  { Optional physical-entry capability preserves the ordinary controller API.
    Beginning reserves no source getter and never cancels an earlier write. }
  INyxCollectionClipboardCellInput = interface(IInterface)
    ['{1131C387-EE2A-4B06-A06F-CFE2469DBBDB}']
    function BeginCellInput: INyxCollectionClipboardCellCommand;
  end;

  { Optional detached original cancellation for an already prepared immutable
    payload. Readiness is callback-free and retains no mount, model, command or
    physical host. Context inspectors may retain this exact snapshot authority;
    its epoch never grants access to a replacement input incarnation. }
  INyxCollectionClipboardPreparedScope = interface(IInterface)
    ['{34FC120A-0877-49DE-A6C8-9D9595C75491}']
    function GetReadiness: INyxCancellationReadiness;
    property Readiness: INyxCancellationReadiness read GetReadiness;
  end;

  { Snapshot actions share the SAME pending-write arbitration as physical copy
    keys. Payload/readiness are captured from one admitted prepared command;
    retaining them does not retain a mount. The per-call reply runs after the
    ordinary controller observer only while this operation remains current.
    ReplyOwner explicitly leases its borrowed receiver through inline/reentrant
    completion. The returned cancellation affects only this exact write, never
    a later keyboard/menu command. It cannot undo an already-entered platform
    promise/effect or guarantee which physical write finishes last. No new
    clipboard authority or physical serialization is introduced. }
  INyxCollectionClipboardCapturedWrite = interface(IInterface)
    ['{5A91F21F-5E73-4EAE-9A8B-69A9AB18D648}']
    function WriteCaptured(const APayload: INyxCollectionClipboardText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
  end;

  { Optional original metadata captured during Prepare, before canonical input
    callbacks. Reading it later never queries the live column or schema. }
  INyxCollectionClipboardDisplayCapture = interface(IInterface)
    ['{A7F0B926-C43B-4BC0-AC74-A26405873070}']
    function GetDisplayPolicy: TNyxCollectionCellDisplayPolicy;
    property DisplayPolicy: TNyxCollectionCellDisplayPolicy read GetDisplayPolicy;
  end;

  { Borrowed represented-output observer sees actual outbound text and its
    accepted RAW provenance. It never runs through the established RAW observer. }
  TNyxCollectionRepresentationObserver = procedure(
    const AValue: INyxCollectionCellText;
    const AResult: TNyxClipboardWriteResult) of object;

  { Optional explicit represented transfer shares the SAME host/serial/receipt
    slot as RAW copy. Newer accepted writes cancel only the older receiver, not
    an already-entered platform effect. RAW observer/phase/result stay RAW-only;
    this port exposes represented output separately and checks its actual bytes.
    All entry methods are UI-only. Foreign value getters are copied/admitted
    before cancellation or effects, with original serial/readiness rechecked. }
  INyxCollectionClipboardRepresentationWrite = interface(IInterface)
    ['{82C47ABC-676A-4233-91CE-252832292DD9}']
    function WriteRepresented(const AValue: INyxCollectionCellText;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    procedure ObserveRepresentation(AObserver: TNyxCollectionRepresentationObserver);
    function GetRepresentationPhase: TNyxCollectionClipboardPhase;
    function GetLastRepresentationResult: TNyxClipboardWriteResult;
    function GetLastRepresentation: INyxCollectionCellText;
    property RepresentationPhase: TNyxCollectionClipboardPhase read GetRepresentationPhase;
    property LastRepresentationResult: TNyxClipboardWriteResult read GetLastRepresentationResult;
    property LastRepresentation: INyxCollectionCellText read GetLastRepresentation;
  end;

  { Borrowed terminal observer is detached before cancellation/retirement.
    Payload and result are the original immutable provenance and typed host
    receipt. The receiver may release the controller or raise; inline failures
    propagate once after terminal state and must never cause a second reply. }
  TNyxCollectionClipboardObserver = procedure(
    const APayload: INyxCollectionClipboardText;
    const AResult: TNyxClipboardWriteResult) of object;

  { Managed per-mount runtime authority with one current asynchronous operation.
    A newer accepted copy cancels the older receiver before host entry. Merely
    beginning/consuming a key does not cancel an existing write. Revoke is
    terminal; CancelPending is silent and leaves future fresh commands enabled.
    Menus may retain prepared tokens and complete them through ordinary OnInvoke;
    tokens preserve the exact original table/cell even after menu callbacks. }
  INyxCollectionClipboardCommands = interface(IInterface)
    ['{95D2A540-F207-4DBD-AE37-C52463C2A055}']
    function BeginCell(const AItem: TNyxItemRef;
      const AField: TNyxColumnIdentity): INyxCollectionClipboardCommand;
    function BeginRows(AMembership: TNyxCollectionCopyMembership): INyxCollectionClipboardCommand;
    procedure Observe(AObserver: TNyxCollectionClipboardObserver);
    procedure CancelPending;
    procedure Revoke;
    function GetPhase: TNyxCollectionClipboardPhase;
    function GetLastResult: TNyxClipboardWriteResult;
    function GetLastPayload: INyxCollectionClipboardText;
    property Phase: TNyxCollectionClipboardPhase read GetPhase;
    property LastResult: TNyxClipboardWriteResult read GetLastResult;
    property LastPayload: INyxCollectionClipboardText read GetLastPayload;
  end;

  { Optional runtime registration preserves the original mount GUID/vtable.
    Clipboard authority is injected explicitly, never found through saved data.
    Staged mounts may configure, but input must be accepted before commands run.
    The mount revokes commands before target/renderer teardown. }
  INyxCollectionClipboardMount = interface(IInterface)
    ['{50D84AAC-9194-4AE7-B0EB-259B7E1FE258}']
    procedure ConfigureClipboard(const AHost: INyxClipboardWriteHost;
      const AEvents: INyxEvents; const AScheduler: INyxScheduler;
      const AOptions: TNyxCollectionClipboardOptions);
    function GetClipboardCommands: INyxCollectionClipboardCommands;
    property ClipboardCommands: INyxCollectionClipboardCommands read GetClipboardCommands;
  end;

implementation

class function TNyxCollectionClipboardOptions.Defaults: TNyxCollectionClipboardOptions;
begin
  Result := Default(TNyxCollectionClipboardOptions);
  Result.FDefined := True;
  Result.FActions := [nccaCell, nccaSelectedSource, nccaSelectedPage];
  Result.FFields := nccfVisiblePresentation;
  Result.FHeaders := True;
  Result.FLines := nccleLF;
  Result.FLimits := TNyxCollectionClipboardLimits.Defaults;
end;

procedure TNyxCollectionClipboardOptions.Validate;
begin

  if not FDefined or
    (Ord(FFields) < Ord(Low(TNyxCollectionCopyFields))) or
    (Ord(FFields) > Ord(High(TNyxCollectionCopyFields))) or
    (Ord(FLines) < Ord(Low(TNyxCollectionCopyLineEnding))) or
    (Ord(FLines) > Ord(High(TNyxCollectionCopyLineEnding))) then
  begin
    raise ENyxCollectionClipboard.Create('Collection clipboard options require known initialized choices');
  end;
  FLimits.Validate;
end;

function TNyxCollectionClipboardOptions.AllowOnly(
  AValue: TNyxCollectionClipboardActions): TNyxCollectionClipboardOptions;
begin
  Validate;
  Result := Self;
  Result.FActions := AValue;
end;

function TNyxCollectionClipboardOptions.Fields(
  AValue: TNyxCollectionCopyFields): TNyxCollectionClipboardOptions;
begin
  Validate;
  Result := Self;
  Result.FFields := AValue;
  Result.Validate;
end;

function TNyxCollectionClipboardOptions.Headers(
  AValue: Boolean): TNyxCollectionClipboardOptions;
begin
  Validate;
  Result := Self;
  Result.FHeaders := AValue;
end;

function TNyxCollectionClipboardOptions.LineEnding(
  AValue: TNyxCollectionCopyLineEnding): TNyxCollectionClipboardOptions;
begin
  Validate;
  Result := Self;
  Result.FLines := AValue;
  Result.Validate;
end;

function TNyxCollectionClipboardOptions.Limits(
  const AValue: TNyxCollectionClipboardLimits): TNyxCollectionClipboardOptions;
begin
  Validate;
  Result := Self;
  Result.FLimits := AValue;
  Result.Validate;
end;

end.
