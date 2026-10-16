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
{ Optional floating composition reuses the ordinary modal and diagram adapters.
  Closing hides the same mounted controls; only Disconnect retires their data. }

type
  { Forward UI owner for the modal's revocable dismissal callback. }
  TDiagramWindow = class;

  { Event-router-owned callback borrows its floating owner. An independent
    scheduler refuses worker entry before reading that pointer; Disconnect
    revokes it before retiring the mounted view and modal. }
  TDiagramWindowClose = class(TNyxEventCallback)
  public
    Owner: TDiagramWindow;
    Scheduler: INyxScheduler;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

  { Owns its target modal and mounted Nyx view. The native parent is borrowed and
    must outlive this window; the browser modal owns a standard HTML dialog.
    Save authority is an explicit revocable host supplied by caller;
    no document/session is retained after construction. }
  TDiagramWindow = class(TInterfacedObject, INyxDiagramEditorWindow)
  private
    FModal: IDiagramModal;
    FView: INyxDiagramEditorView;
    FTitle: TNyxText;
    FScheduler: INyxScheduler;
    FCloseCallback: TDiagramWindowClose;
    FCloseLease: INyxEventCallback;
    FCloseSubscription: INyxEventSubscription;
    procedure Dismissed;
  public
    constructor Create(const AID: TNyxText; ADocument: TNyxDocument;
      const AReference: TNyxDiagramRef; AOwner: TDiagramHost;
      const ASaveHost: INyxDiagramSaveHost; ATheme: TNyxTheme);
    destructor Destroy; override;
    function GetView: INyxDiagramEditorView;
    function GetOpen: Boolean;
    procedure Show;
    procedure Hide;
    procedure Disconnect;
  end;

procedure TDiagramWindowClose.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LOwner: TDiagramWindow;
  LLease: INyxDiagramEditorWindow;
begin
  Scheduler.RequireUI;
  LOwner := Owner;

  if LOwner <> nil then
  begin
    LLease := LOwner;
    LOwner.Hide;
  end;
end;

constructor TDiagramWindow.Create(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef; AOwner: TDiagramHost;
  const ASaveHost: INyxDiagramSaveHost; ATheme: TNyxTheme);
var
  LEditor: INyxDiagramEditor;
  LTarget: TNyxDiagramSaveTarget;
  LEvents: INyxEvents;
begin
  inherited Create;
  FScheduler := NewNyxScheduler;
  FScheduler.RequireUI;
  LTarget := NyxDiagramSaveTarget(ADocument, AReference);
  FTitle := ADocument.Title + ' / ' + AReference.Name;

  if ADocument.Diagrams.Contains(AReference) then
  begin
    LEditor := NewNyxDiagramEditor(AID, ADocument, AReference);
  end
  else
  begin
    LEditor := NewNyxDiagramEditor(AID, NewNyxStarterDiagram);
  end;
  LEditor.Node.Find(NyxDiagramEditorPart(AID, ndepClose).ID).Configure.Visible(True);
  {$ifdef PAS2JS}
  FModal := NewNyxBrowserModalHost;
  {$else}
  FModal := NewNyxLCLModalHost(AOwner);
  {$endif}
  try
    FModal.OnDismiss := Dismissed;
    Show;
    {$ifdef PAS2JS}
    FView := MountNyxBrowserDiagramEditor(LEditor, ADocument, FModal.Element, nil, ATheme);
    {$else}
    FView := MountNyxLCLDiagramEditor(LEditor, ADocument, FModal.Control, nil, ATheme);
    {$endif}
    FView.SavingTo(LTarget, ASaveHost);
    LEvents := (FView as IDiagramTargetView).Renderer.Events;
    FCloseCallback := TDiagramWindowClose.Create;
    FCloseLease := FCloseCallback;
    FCloseCallback.Owner := Self;
    FCloseCallback.Scheduler := FScheduler;
    FCloseSubscription := LEvents.On(NyxControlEvents(
      NyxDiagramEditorPart(AID, ndepClose).ID), ntClick).Subscribe(FCloseLease);
  except
    Disconnect;
    raise;
  end;
end;

destructor TDiagramWindow.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TDiagramWindow.GetView: INyxDiagramEditorView;
begin
  FScheduler.RequireUI;
  Result := FView;
end;

function TDiagramWindow.GetOpen: Boolean;
begin
  FScheduler.RequireUI;
  Result := (FModal <> nil) and FModal.IsOpen;
end;

procedure TDiagramWindow.Show;
begin
  FScheduler.RequireUI;

  if FModal = nil then
  begin
    raise ENyxDiagram.Create('This diagram window has retired');
  end;
  FModal.Show(NyxModal(FTitle).Sizing(nhfAvailableHeight));
end;

procedure TDiagramWindow.Hide;
begin
  FScheduler.RequireUI;

  if (FView <> nil) and FView.Connected then
  begin
    FView.CancelManipulation;
  end;

  if FModal <> nil then
  begin
    FModal.Hide;
  end;
end;

procedure TDiagramWindow.Dismissed;
begin
  { OnDismiss delegates dismissal to its controller; it does not hide itself
    afterward. Share the explicit Close path so capture and visibility retire. }
  Hide;
end;

procedure TDiagramWindow.Disconnect;
begin

  if FScheduler <> nil then
  begin
    FScheduler.RequireUI;
  end;

  if FCloseCallback <> nil then
  begin
    FCloseCallback.Owner := nil;
    FCloseCallback := nil;
  end;

  if FCloseSubscription <> nil then
  begin
    FCloseSubscription.Cancel;
    FCloseSubscription := nil;
  end;
  FCloseLease := nil;

  if FView <> nil then
  begin
    FView.Disconnect;
    FView := nil;
  end;

  if FModal <> nil then
  begin
    FModal.OnDismiss := nil;
    FModal.Hide;
    FModal := nil;
  end;
end;
