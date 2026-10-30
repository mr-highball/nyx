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
  nyx.collections.clipboard, nyx.collections.representation, nyx.collections.identities,
  nyx.clipboard, nyx.scheduler, nyx.events, nyx.collections.range.actions;

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

  { Presentation-only observation of the existing single write slot. Serial is
    the original admitted request, not a new operation. Result is meaningful
    only when Phase=Completed; cancellation without a host receipt never invents
    an outcome. No copied cell, payload, source, target or callback is retained. }
  TNyxCollectionClipboardFeedback = record
    Serial: Integer;
    Phase: TNyxCollectionClipboardPhase;
    Result: TNyxClipboardWriteResult;
  end;

  { Separate borrowed UI receiver preserves application-owned raw, represented
    and identity observers. The owner detaches it before retirement. Delivery is
    UI-only; it must not throw, and must fence any reentrant presentation work
    against the supplied serial. This callback grants no transfer authority. }
  TNyxCollectionClipboardFeedbackObserver = procedure(
    const AFeedback: TNyxCollectionClipboardFeedback) of object;

  { Optional copied state and default-presentation choice, without changing any
    established mount GUID/vtable. Disabling default presentation preserves the
    receipt and all caller observers. Target adapters provide the actual surface;
    this state contract alone makes no widget or accessibility claim. }
  INyxCollectionClipboardFeedbackMount = interface(IInterface)
    ['{3D574B92-F501-4363-8322-E956EF382AB4}']
    function GetClipboardFeedback: TNyxCollectionClipboardFeedback;
    function GetClipboardFeedbackEnabled: Boolean;
    procedure SetClipboardFeedbackEnabled(AEnabled: Boolean);
    property ClipboardFeedback: TNyxCollectionClipboardFeedback read GetClipboardFeedback;
    property ClipboardFeedbackEnabled: Boolean read GetClipboardFeedbackEnabled
      write SetClipboardFeedbackEnabled;
  end;

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

  { Optional readonly context entry reserves the SAME original command/input
    proof as physical copy, before any source or cell getter. It may capture
    one accepted cell even when cell copying is not permitted. Complete cannot
    turn that capture into a disallowed write. Old physical BeginCellInput and
    disallowed Ctrl+C retain their no-capture contract. UI-owner, synchronous
    reservation only; no callback or host effect occurs merely on entry. }
  INyxCollectionClipboardInspectionInput = interface(IInterface)
    ['{4ED59038-BF81-49D9-9702-A4CBF014DD96}']
    function BeginCellInspection: INyxCollectionClipboardCellCommand;
  end;

  { Immutable permission copied from the original controller options. Managed
    menus/inspectors use it for accepted, displayed and explicit-draft cell
    transfers; independently permitted cell ranges retain their own proof.
    The Boolean is not current input authority and retains no additional owner. }
  INyxCollectionClipboardCellPermission = interface(IInterface)
    ['{BBF4F68F-E0AF-4519-9C78-D8355F291CD3}']
    function GetCellCopyAllowed: Boolean;
    property CellCopyAllowed: Boolean read GetCellCopyAllowed;
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

  { Optional enrichment of the SAME accepted-cell command before Prepare enters
    source getters. It reserves no second clipboard request.
    Prepare normalizes the supplied immutable range against its exact original
    source/page/column serial and typed launch cell. Unselected, stale, hidden
    or disallowed ranges produce no extra action; ordinary RAW capture remains
    available. Returned TSV uses the original controller's configured headers,
    lines and bounds. Payload/readiness own only copied values/cancel flags,
    never the command, live view, mount or editor. UI-owner methods only. }
  INyxCollectionClipboardCellRangeContext = interface(IInterface)
    ['{57DCE031-FAF0-4B90-A2EA-E310AF545BA0}']
    procedure AttachRange(const AIntent: INyxCollectionCellRangeIntent);
    function GetRangePayload: INyxCollectionClipboardText;
    function GetRangeReadiness: INyxCancellationReadiness;
    property RangePayload: INyxCollectionClipboardText read GetRangePayload;
    property RangeReadiness: INyxCancellationReadiness read GetRangeReadiness;
  end;

  { Optional selected-row enrichment of the SAME original context command.
    Both membership cohorts are captured before outward input factories;
    current-page and complete-source choices remain explicit. Readiness is
    independent of RAW inspection/ranges and owns only synchronized flags.
    Missing/disallowed/empty/over-budget choices return nil, never a fallback. }
  INyxCollectionClipboardCellRowsContext = interface(IInterface)
    ['{E14D3D97-E450-4A9F-AE2E-9DF1CE460683}']
    function GetSourceRowsPayload: INyxCollectionClipboardText;
    function GetPageRowsPayload: INyxCollectionClipboardText;
    function GetRowsReadiness: INyxCancellationReadiness;
    property SourceRowsPayload: INyxCollectionClipboardText read GetSourceRowsPayload;
    property PageRowsPayload: INyxCollectionClipboardText read GetPageRowsPayload;
    property RowsReadiness: INyxCancellationReadiness read GetRowsReadiness;
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

  { Immutable prepared action is produced only by its original controller.
    Values/readiness retain no controller/view/mount. Its private identity proof
    prevents relabeling a range as rows/cell or adopting it on another service.
    Public value getters alone never grant permission to submit foreign data. }
  INyxCollectionClipboardActionCapture = interface(IInterface)
    ['{E5C92DAA-C628-46D6-8E0B-8D46906EDBFC}']
    function GetAction: TNyxCollectionClipboardAction;
    function GetPayload: INyxCollectionClipboardText;
    function GetReadiness: INyxCancellationReadiness;
    property Action: TNyxCollectionClipboardAction read GetAction;
    property Payload: INyxCollectionClipboardText read GetPayload;
    property Readiness: INyxCancellationReadiness read GetReadiness;
  end;

  { Optional readonly original transfers enrich the same prepared context.
    Nil means disallowed, stale, empty or over budget; it never selects a newer
    row/range cohort. Returned actions share the existing host request slot. }
  INyxCollectionClipboardContextTransfers = interface(IInterface)
    ['{0DB4F1F8-34F1-43EF-A8A9-C97FD7737A89}']
    function GetRangeAction: INyxCollectionClipboardActionCapture;
    function GetSourceRowsAction: INyxCollectionClipboardActionCapture;
    function GetPageRowsAction: INyxCollectionClipboardActionCapture;
    property RangeAction: INyxCollectionClipboardActionCapture read GetRangeAction;
    property SourceRowsAction: INyxCollectionClipboardActionCapture read GetSourceRowsAction;
    property PageRowsAction: INyxCollectionClipboardActionCapture read GetPageRowsAction;
  end;

  { Optional explicit accepted-snapshot action uses the SAME host/serial slot.
    Only an original sealed capture from this controller is admitted, with its
    captured action permission and exact payload/readiness. Callers cannot
    substitute an action, payload or proof; cancellation/reply ownership remains
    the same as WriteCaptured. Old cell-only port semantics stay unchanged. }
  INyxCollectionClipboardCapturedActionWrite = interface(IInterface)
    ['{5A50650D-7210-4F43-B138-EC886BDF312D}']
    function WriteCapturedAction(const AAction: INyxCollectionClipboardActionCapture;
      const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
  end;

  { Sealed original identity representation. Matching selected-source/page row
    permission governs IDs as a separate output, without granting cell/range or
    broader membership. Values/readiness own no controller, mount or live view. }
  INyxCollectionClipboardIdentityAction = interface(IInterface)
    ['{194872F5-4D8C-4A6B-8770-E0E7F393C5FC}']
    function GetValue: INyxCollectionIdentityText;
    function GetReadiness: INyxCancellationReadiness;
    property Value: INyxCollectionIdentityText read GetValue;
    property Readiness: INyxCancellationReadiness read GetReadiness;
  end;

  { Optional readonly original identity choices on the same context command.
    Nil means disallowed, stale, empty or over budget. Identity output is admitted
    independently of column-field TSV and never invents an authored ID field. }
  INyxCollectionClipboardIdentityTransfers = interface(IInterface)
    ['{CC05D61D-889A-40B9-8758-ED06B393F1B6}']
    function GetSourceIdentityAction: INyxCollectionClipboardIdentityAction;
    function GetPageIdentityAction: INyxCollectionClipboardIdentityAction;
    property SourceIdentityAction: INyxCollectionClipboardIdentityAction read GetSourceIdentityAction;
    property PageIdentityAction: INyxCollectionClipboardIdentityAction read GetPageIdentityAction;
  end;

  { Borrowed UI receiver sees identity text/provenance and its real host result.
    Identity output never invokes RAW-column or single-cell display observers. }
  TNyxCollectionIdentityObserver = procedure(const AValue: INyxCollectionIdentityText;
    const AResult: TNyxClipboardWriteResult) of object;

  { UI-only identity transfer shares the original controller's ONE serial/host
    slot. Only its private original seal is accepted. Additional readiness can
    restrict, never replace the captured cohort. Receiver ownership and platform
    cancellation limits match WriteCaptured. Distinct phase/value/result avoid
    presenting IDs as RAW schema cells or single-cell formatted output. }
  INyxCollectionClipboardIdentityWrite = interface(IInterface)
    ['{138592DA-01AA-463D-B3EF-BBBAFC46FD7D}']
    function WriteIdentities(const AAction: INyxCollectionClipboardIdentityAction;
      const ACancellation: INyxCancellationScope; const AReadiness: INyxCancellationReadiness;
      AReply: TNyxClipboardWriteReply; const AReplyOwner: IInterface): INyxClipboardWriteRequest;
    procedure ObserveIdentities(AObserver: TNyxCollectionIdentityObserver);
    function GetIdentityPhase: TNyxCollectionClipboardPhase;
    function GetLastIdentityResult: TNyxClipboardWriteResult;
    function GetLastIdentities: INyxCollectionIdentityText;
    property IdentityPhase: TNyxCollectionClipboardPhase read GetIdentityPhase;
    property LastIdentityResult: TNyxClipboardWriteResult read GetLastIdentityResult;
    property LastIdentities: INyxCollectionIdentityText read GetLastIdentities;
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

{ Shared English status formatting keeps browser/native meanings equal. Host
  diagnostics remain plain portable text, never markup or a claimed write. }
function NyxCollectionClipboardFeedbackText(
  const AFeedback: TNyxCollectionClipboardFeedback): TNyxText;

implementation

function NyxCollectionClipboardFeedbackText(
  const AFeedback: TNyxCollectionClipboardFeedback): TNyxText;
begin
  Result := 'Copy results appear here.';
  case AFeedback.Phase of
    nccpIdle:
      begin
        Exit;
      end;
    nccpPending:
      begin
        Result := 'Copying to clipboard...';
      end;
    nccpCancelled:
      begin
        Result := 'Copy completion was interrupted.';
      end;
    nccpCompleted:
      begin
        case AFeedback.Result.Outcome of
          ncwoWritten:
            begin
              Result := 'Copied to clipboard.';
            end;
          ncwoUnavailable:
            begin
              Result := 'Clipboard unavailable.';
            end;
          ncwoActivationRequired:
            begin
              Result := 'Choose Copy again from a current user action.';
            end;
          ncwoDenied:
            begin
              Result := 'Clipboard permission was denied.';
            end;
          ncwoCancelled:
            begin
              Result := 'Copy was cancelled.';
            end;
          ncwoFailed:
            begin
              Result := 'Copy failed.';
            end;
        end;

        if AFeedback.Result.Error <> '' then
        begin
          Result := Result + ' ' + AFeedback.Result.Error;
        end;
      end;
  end;
end;

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
