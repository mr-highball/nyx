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
unit nyx.collections.export.actions;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections.view, nyx.collections.export, nyx.collections.clipboard.actions,
  nyx.files.save, nyx.scheduler, nyx.events;

type
  { A pre-canonical export envelope reserves one original table/input intention.
    Prepare captures actual query/page/columns before callbacks; Complete never
    recaptures newer data. Retire withdraws only an untransferred default. These
    operational methods and full Current checks are UI-thread confined, like
    the original collection view; they are not worker admission primitives. }
  INyxCollectionExportCommand = interface(IInterface)
    ['{F0E63D93-0497-4EC9-85CB-A49B069A7032}']
    procedure Prepare;
    function GetCurrent: Boolean;
    function GetDispatchCurrent: Boolean;
    function GetCapture: INyxCollectionExportCapture;
    function Complete(AAllowDefault: Boolean): Boolean;
    procedure Retire;
    property Current: Boolean read GetCurrent;
    property DispatchCurrent: Boolean read GetDispatchCurrent;
    property Capture: INyxCollectionExportCapture read GetCapture;
  end;

  { Optional original copy envelope attachment requests the table output capture
    only for an opted-in context menu. It does not change clipboard payloads or
    let later menu callbacks capture a new query. No old interface ABI changes. }
  INyxCollectionClipboardOutputCapture = interface(IInterface)
    ['{EE93977F-5068-40AE-B36D-50A65733B381}']
    procedure PrepareOutput;
    function GetOutput: INyxCollectionExportCapture;
    property Output: INyxCollectionExportCapture read GetOutput;
  end;

  { One terminal save observer borrows its receiver until cancellation/Revoke.
    Results retain the existing Written/DownloadRequested distinction. Callback
    failures propagate once after terminal detachment, including inline hosts. }
  TNyxCollectionExportObserver = procedure(const APayload: INyxCollectionExportText;
    const AResult: TNyxFileSaveResult) of object;

  { A detached already-captured action shares this export controller's operation
    slot. Original readiness must be callback-free and owner-free; it accompanies
    the original data capture, never a fresh epoch. ReplyOwner leases a borrowed
    method receiver through inline/reentrant completion. Cancelling its returned
    token affects only this exact save receiver, not a newer command; external
    file/download effects already entered are never promised to roll back. }
  INyxCollectionExportCapturedSave = interface(IInterface)
    ['{C1ED922B-DCC4-40D4-8CF1-50E05A283B86}']
    function SaveCaptured(const ACapture: INyxCollectionExportCapture;
      AMembership: TNyxCollectionExportRows; const ACancellation: INyxCancellationScope;
      const AReadiness: INyxCancellationReadiness; AReply: TNyxFileSaveReply;
      const AReplyOwner: IInterface): INyxFileSaveRequest;
  end;

  { Lifecycle is independent of whether an external host reported success.
    Only Completed has an actual terminal host receipt. Cancelled is silent
    receiver withdrawal, not proof that no platform effect occurred. }
  TNyxCollectionExportPhase = (ncepIdle, ncepPending, ncepCompleted, ncepCancelled);

  { Runtime-only file authority with one exact pending export. Merely preparing
    or consuming an input never cancels an earlier save; newer accepted saves do.
    Controller retains a data view but no mount/renderer and Revoke clears its
    borrowed mount-fence callback before target retirement. Construction requires
    the supplied scheduler's UI entry; callers keep all command/save/observe/
    cancellation operations on that UI thread, as required by view and host. }
  INyxCollectionExportCommands = interface(IInterface)
    ['{31A2511D-9025-4680-94E5-345AFFA8AAB9}']
    function BeginExport(AMembership: TNyxCollectionExportRows): INyxCollectionExportCommand;
    procedure Observe(AObserver: TNyxCollectionExportObserver);
    procedure CancelPending;
    procedure Revoke;
    function GetPhase: TNyxCollectionExportPhase;
    function GetLastResult: TNyxFileSaveResult;
    function GetLastPayload: INyxCollectionExportText;
    property Phase: TNyxCollectionExportPhase read GetPhase;
    property LastResult: TNyxFileSaveResult read GetLastResult;
    property LastPayload: INyxCollectionExportText read GetLastPayload;
  end;

  { Optional mount registration keeps all prior GUIDs/vtables intact. Host
    authority is injected explicitly and never reconstructed from saved data.
    Configure before cell actions to expose managed context export choices. }
  INyxCollectionExportMount = interface(IInterface)
    ['{6B02F757-71AD-4B6B-A16A-32364E0969FD}']
    procedure ConfigureExport(const AHost: INyxFileSaveHost; const AEvents: INyxEvents;
      const AScheduler: INyxScheduler; const AOptions: TNyxCollectionExportOptions);
    function GetExportCommands: INyxCollectionExportCommands;
    property ExportCommands: INyxCollectionExportCommands read GetExportCommands;
  end;

implementation

end.
