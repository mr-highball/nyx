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

unit nyx.confirmation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.modal,
  nyx.behavior, nyx.events, nyx.data;

type
  { Presentation result only: neither dismissal nor confirmation edits a
    document. The consumer applies its revision-aware command after confirming. }
  TNyxConfirmationResult = (ncrNotOpened, ncrPending, ncrConfirmed, ncrCancelled);
  { Fit the current content at Open, or reserve the supplied modal viewport.
    Both choices remain capped. Oversized custom native content must provide an
    ordinary Nyx scrolling layout; fitting alone is not a scrolling contract. }
  TNyxConfirmationSizing = (ncsContent, ncsViewport);

  { Immutable typed presentation options. Initial focus is a named part in the
    independent confirmation content; the default is the safer Cancel action. }
  TNyxConfirmationOptions = record
  private
    FWindow: TNyxModalOptions;
    FFocus: TNyxPartRef;
    FSizing: TNyxConfirmationSizing;
  public
    { A copied modal value controls geometry and accessible window title. }
    function Window(const AOptions: TNyxModalOptions): TNyxConfirmationOptions;
    { Part must resolve to an enabled, visible focus face at Open; otherwise
      opening refuses and leaves the previous result/background unchanged. }
    function Focus(const APart: TNyxPartRef): TNyxConfirmationOptions;
    function Sizing(AValue: TNyxConfirmationSizing): TNyxConfirmationOptions;
    property Modal: TNyxModalOptions read FWindow;
    property InitialFocus: TNyxPartRef read FFocus;
    property SizeMode: TNyxConfirmationSizing read FSizing;
  end;

  { Managed reusable presentation. Content is a specialized, independent clone
    of the supplied recipe, including arbitrary added controls/actions. Configure
    it before Open; changes while open apply on the next Open. Retained content
    handles stay safe after the presenter ends; no cycle retains its renderer.

    Events exposes the ordinary mounted Nyx event router for all control events.
    OnConfirm/OnCancel are separate completion streams with ordinary multiple
    registrations and scheduler policy. Completion closes first, then sends an
    owned snapshot. A callback may reopen or release the presenter safely.
    Escape/window close is Cancel; Close is silent and resolves a pending result
    as Cancelled. Open/Close during a focus transition and repeated Open while
    pending refuse. Callbacks must not strongly retain their owning presenter,
    which would form an application-created registration cycle. Methods are UI-thread
    operations; threaded callbacks must submit UI work through the scheduler. }
  INyxConfirmation = interface(IInterface)
    ['{271BE5DA-8A62-4F45-A8BC-001006000001}']
    function GetContent: INyxConfirmationDialog;
    function GetEvents: INyxEvents;
    function GetOpen: Boolean;
    function GetResult: TNyxConfirmationResult;
    function OnConfirm: INyxEventStream;
    function OnCancel: INyxEventStream;
    procedure Open(const AOptions: TNyxConfirmationOptions);
    procedure Close;
    property Content: INyxConfirmationDialog read GetContent;
    property Events: INyxEvents read GetEvents;
    property IsOpen: Boolean read GetOpen;
    property Result: TNyxConfirmationResult read GetResult;
  end;

  { Common implementation for target adapters. Owns document/content and
    completion router; platform hosts/renderers remain in subclasses. Adapter
    receiver methods borrow Self; no event registration retains this presenter.
    Subclasses disconnect and free renderers before inherited destruction. }
  TNyxConfirmationPresenter = class(TInterfacedObject, INyxConfirmation)
  private
    FDocument: TNyxDocument;
    FContent: INyxConfirmationDialog;
    FCompletion: INyxEvents;
    FResult: TNyxConfirmationResult;
    FOpen: Boolean;
    FOpening: Boolean;
    FClosing: Boolean;
  protected
    procedure Present(const AOptions: TNyxConfirmationOptions;
      const AFocusID: TNyxText); virtual; abstract;
    procedure Conceal; virtual; abstract;
    procedure Accept(const AEvent: TNyxEventInfo);
    procedure Dismiss;
    property Document: TNyxDocument read FDocument;
  public
    { Nil templates refuse. Named focus parts are admitted by Open before the
      target opens; arbitrary added content remains independently owned. }
    constructor Create(const ATemplate: INyxConfirmationDialog);
    destructor Destroy; override;
    function GetContent: INyxConfirmationDialog;
    function GetEvents: INyxEvents; virtual; abstract;
    function GetOpen: Boolean;
    function GetResult: TNyxConfirmationResult;
    function OnConfirm: INyxEventStream;
    function OnCancel: INyxEventStream;
    procedure Open(const AOptions: TNyxConfirmationOptions);
    procedure Close;
  end;

{ Default compact geometry is 560x360 logical pixels, capped by 94% of the
  available viewport. Title is user text; all behavior choices are typed. }
function NyxConfirmation(const ATitle: TNyxText): TNyxConfirmationOptions;

implementation

function NyxConfirmation(const ATitle: TNyxText): TNyxConfirmationOptions;
begin
  Result := Default(TNyxConfirmationOptions);
  Result.FWindow := NyxModal(ATitle).MaximumWidth(560).MaximumHeight(360);
  Result.FFocus := NyxPart('actions/cancel');
end;

function TNyxConfirmationOptions.Window(
  const AOptions: TNyxModalOptions): TNyxConfirmationOptions;
begin
  AOptions.Viewport(AOptions.ViewportPercent).MaximumWidth(AOptions.WidthLimit)
    .MaximumHeight(AOptions.HeightLimit);
  Result := Self;
  Result.FWindow := AOptions;
end;

function TNyxConfirmationOptions.Focus(
  const APart: TNyxPartRef): TNyxConfirmationOptions;
begin

  if APart.Name = '' then
  begin
    raise ENyxModel.Create('Confirmation initial focus requires a named part');
  end;
  Result := Self;
  Result.FFocus := APart;
end;

function TNyxConfirmationOptions.Sizing(
  AValue: TNyxConfirmationSizing): TNyxConfirmationOptions;
begin

  if (AValue < Low(TNyxConfirmationSizing)) or
    (AValue > High(TNyxConfirmationSizing)) then
  begin
    raise ENyxModel.Create('Unknown confirmation sizing policy');
  end;
  Result := Self;
  Result.FSizing := AValue;
end;

constructor TNyxConfirmationPresenter.Create(const ATemplate: INyxConfirmationDialog);
begin
  inherited Create;

  if ATemplate = nil then
  begin
    raise ENyxModel.Create('Confirmation requires a specialized content template');
  end;
  FDocument := TNyxDocument.Create;
  FContent := ATemplate.Clone as INyxConfirmationDialog;
  { The confirmation itself is the independently owned view root; no unnamed
    wrapper page or borrowed original application document is required. }
  FDocument.AddPage(FContent);
  FCompletion := NewNyxEvents;
  FResult := ncrNotOpened;
end;

destructor TNyxConfirmationPresenter.Destroy;
begin

  if FCompletion <> nil then
  begin
    FCompletion.Close;
  end;
  FCompletion := nil;
  FContent := nil;
  FDocument.Free;
  inherited Destroy;
end;

function TNyxConfirmationPresenter.GetContent: INyxConfirmationDialog;
begin
  Result := FContent;
end;

function TNyxConfirmationPresenter.GetOpen: Boolean;
begin
  Result := FOpen;
end;

function TNyxConfirmationPresenter.GetResult: TNyxConfirmationResult;
begin
  Result := FResult;
end;

function TNyxConfirmationPresenter.OnConfirm: INyxEventStream;
begin
  Result := FCompletion.OnNamed(NyxCompoundEvents(FContent.ID), NyxSemantic(nseConfirm));
end;

function TNyxConfirmationPresenter.OnCancel: INyxEventStream;
begin
  Result := FCompletion.OnNamed(NyxCompoundEvents(FContent.ID), NyxSemantic(nseCancel));
end;

procedure TNyxConfirmationPresenter.Open(const AOptions: TNyxConfirmationOptions);
var
  LFocus: INyxControl;
  LKeepAlive: INyxConfirmation;
begin
  LKeepAlive := Self;
  FCompletion.Scheduler.RequireUI;

  if FOpen or FOpening or FClosing then
  begin
    raise ENyxModel.Create('Confirmation is already awaiting a result');
  end;
  AOptions.Window(AOptions.Modal).Focus(AOptions.InitialFocus).Sizing(AOptions.SizeMode);
  LFocus := FContent.Part(AOptions.InitialFocus);
  { Admission is checked by the actual target too: ancestor visibility,
    renderer capabilities and physical focus are not inferred from model text. }
  FOpening := True;
  try
    Present(AOptions, LFocus.ID);
  finally
    FOpening := False;
  end;
  FResult := ncrPending;
  FOpen := True;
  LKeepAlive.GetOpen;
end;

procedure TNyxConfirmationPresenter.Close;
var
  LKeepAlive: INyxConfirmation;
begin
  LKeepAlive := Self;
  FCompletion.Scheduler.RequireUI;

  if FOpening or FClosing then
  begin
    raise ENyxModel.Create('Confirmation is establishing initial focus');
  end;

  if not FOpen then
  begin
    Exit;
  end;
  FOpen := False;
  FResult := ncrCancelled;
  { Returning focus can run the invoker's callbacks. Hold the controller and
    refuse another presentation until that transition has completely returned. }
  FClosing := True;
  try
    Conceal;
  finally
    FClosing := False;
  end;
  LKeepAlive.GetResult;
end;

procedure TNyxConfirmationPresenter.Accept(const AEvent: TNyxEventInfo);
var
  LKeepAlive: INyxConfirmation;
  LEvents: INyxEvents;
  LID: TNyxText;
  LSnapshot: TNyxEventInfo;
begin

  if not FOpen or (AEvent.SourceID <> FContent.ID) or
    not (AEvent.IsNamed(NyxSemantic(nseConfirm)) or
      AEvent.IsNamed(NyxSemantic(nseCancel))) then
  begin
    Exit;
  end;
  { Hold Self until synchronous completion returns. Queued work owns snapshots,
    never this borrowed receiver. Close before callbacks allows safe reopening. }
  LKeepAlive := Self;
  LEvents := FCompletion;
  LID := FContent.ID;
  LSnapshot := AEvent.Copy;
  FOpen := False;

  if AEvent.IsNamed(NyxSemantic(nseConfirm)) then
  begin
    FResult := ncrConfirmed;
  end
  else
  begin
    FResult := ncrCancelled;
  end;
  FClosing := True;
  try
    Conceal;
  finally
    FClosing := False;
  end;
  LEvents.Dispatch(LSnapshot, LSnapshot.OriginID, LID);
  { A real read documents the lifetime lease and avoids unused-local notes. }
  LKeepAlive.GetResult;
end;

procedure TNyxConfirmationPresenter.Dismiss;
var
  LEvent: TNyxEventInfo;
begin

  if not FOpen then
  begin
    Exit;
  end;
  LEvent := Default(TNyxEventInfo);
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  LEvent.Trigger := ntNamed;
  LEvent.Name := NyxSemantic(nseCancel);
  LEvent.SourceID := FContent.ID;
  LEvent.OriginID := FContent.ID;
  LEvent.TargetID := FContent.ID;
  Accept(LEvent);
end;

end.
