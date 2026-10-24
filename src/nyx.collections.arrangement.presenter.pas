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

{ Ordinary column-management presentation contracts, independent of mounts and
  platform controls. The existing view remains the sole preference owner. }
unit nyx.collections.arrangement.presenter;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model, nyx.root.types, nyx.events, nyx.popover.types,
  nyx.publication, nyx.collections.arrangement, nyx.collections.view,
  nyx.collections.arrangement.controls;

const
  NyxColumnArrangementEditorID = 'nyx-column-arrangement';
  NyxColumnArrangementStatusState = 'nyx-column-arrangement-status';
  NyxColumnArrangementReloadID = 'nyx-column-arrangement-reload';
  { A safe initial focus and explicit dismissal for the mounted presentation.
    Closing retains the current form/drafts; it does not reset preferences. }
  NyxColumnArrangementCloseID = 'nyx-column-arrangement-close';

type
  { One ordinary mounted Nyx form. Events decorates the REAL form router with
    its actual per-open admission and the original target input lifetime.
    CaptureRoot borrows the exact realized tree while this presenter is retained
    and connected; use it synchronously without intervening outward calls.
    Authored popover Content is never a replacement for synchronized live values.
    Target-specific optional facets may expose the unchanged actual popover. }
  INyxColumnArrangementPresenter = interface(IInterface)
    ['{7C38DD03-D8FB-48A2-8E71-3A04BC4F64B8}']
    function GetEvents: INyxEvents;
    function GetConnected: Boolean;
    function GetOpen: Boolean;
    function CaptureRoot: TNyxNode;
    procedure SetStatus(const AText: TNyxText);
    procedure Show(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Disconnect;
    property Events: INyxEvents read GetEvents;
    property Connected: Boolean read GetConnected;
    property IsOpen: Boolean read GetOpen;
  end;

  { Revocable exact-table target factory. CreatePresenter copies the supplied
    document/root through the ordinary popover owner. It retains no manager or
    mount. Revoke clears borrowed theme/anchor access before physical teardown;
    existing presenters retain only the factory's inert retirement facts. }
  INyxColumnArrangementPresenterFactory = interface(IInterface)
    ['{6485695A-701F-4F52-84AC-05D0ED646392}']
    function CreatePresenter(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxColumnArrangementPresenter;
    procedure Revoke;
  end;

  { Synchronous borrowed mount port, used only on the UI thread after original
    command readiness. The mount revokes its manager before destroying itself.
    The returned preparation owns the exact mount through validation/notification
    and uses the ORIGINAL capture and full typed field. Measurement is assigned
    under an entered owner; var deliberately avoids releasing an aliased managed
    output before entry. No callback may recapture a newer launch for this work. }
  TNyxColumnArrangementFitPrepare = function(
    const ALaunch: INyxCollectionColumnsCapture;
    const AField: TNyxColumnIdentity;
    var AMeasurement: INyxColumnFitMeasurement): INyxPreparedPublication of object;

  { Per-mounted-table controller. It owns a captured view/form and detached
    drafts, never another preference/query store. Plain Open preserves a closed
    form's draft; ReloadDraft deliberately discards it and captures current
    columns. Disabling closes the form and supersedes queued work without
    withdrawing accepted preferences. Revoke is flags-only; Disconnect attempts
    every independent subscription/presenter cleanup after logical retirement. }
  INyxColumnArrangementManager = interface(IInterface)
    ['{887413D1-03B2-45CE-AD8C-16A8E60A1079}']
    procedure Open;
    procedure ReloadDraft;
    procedure SetEnabled(AEnabled: Boolean);
    function GetConnected: Boolean;
    function GetObservation: TNyxColumnLayoutObservation;
    function GetPresenter: INyxColumnArrangementPresenter;
    procedure Revoke;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
    property Observation: TNyxColumnLayoutObservation read GetObservation;
    property Presenter: INyxColumnArrangementPresenter read GetPresenter;
  end;

implementation

end.
