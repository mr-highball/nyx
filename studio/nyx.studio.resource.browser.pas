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
unit nyx.studio.resource.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.behavior, nyx.events, nyx.publication,
  nyx.resources.catalog, nyx.resources.browser, nyx.resources.editor,
  nyx.collections.mount, nyx.studio.session, nyx.studio.section.views;

type
  TNyxStudioResourceBrowseIntent = (rbiNone, rbiWorkspace, rbiOpen);

  { One runtime metadata catalog serves both ordinary public Nyx list faces.
    It owns no document, asset payload, session or renderer. Its managed
    selection receiver borrows the section facade; release this owner before
    the controller's views. Mounts and event tokens retire with their views.
    A copied context isolates independent project loads.
    Metadata preparation precedes shell admission; publication/mounting follows
    it. This does not pretend shell and model form one atomic publication. }
  TNyxStudioResourceBrowser = class
  private
    FCatalog: INyxResourceCatalog;
    FContext: TNyxStudioCommandContext;
    FMounts: array of INyxCollectionMount;
    FSelectionCallback: TNyxEventCallback;
    FSelectionLease: INyxEventCallback;
    FSelectionTokens: array of INyxEventSubscription;
    procedure SyncActions(AViews: TNyxStudioSectionViews);
  public
    constructor Create;
    destructor Destroy; override;
    { False also detects an old live mount after a failed replacement. Such a
      frame must stage anew before attaching this project's independent view. }
    function Compatible(ASession: TNyxStudioSession): Boolean;
    function Prepare(ASession: TNyxStudioSession): INyxPreparedPublication;
    { Copy the full face first, falling back to the compact picker. Absence
      preserves the parked state; no mounted node or provider is retained. }
    procedure Capture(AViews: TNyxStudioSectionViews;
      var AState: TNyxResourceBrowserState);
    procedure Mount(AViews: TNyxStudioSectionViews;
      const APrepared: INyxPreparedPublication; const AState: TNyxResourceBrowserState;
      const ASelection: TNyxResourceEditorSelection);
    { Filter input/actions synchronize fixed compounds only. Row selection
      never opens/replaces a proposal. Open validates current scoped metadata
      and project load; actual mutations still require the form's exact accepted
      resource/binding baselines and ordinary paired source admission. }
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      AViews: TNyxStudioSectionViews; ASession: TNyxStudioSession;
      var AState: TNyxResourceBrowserState;
      out AIntent: TNyxStudioResourceBrowseIntent;
      out ASelection: TNyxResourceEditorSelection): Boolean;
  end;

const
  NyxStudioResourcePickerID = 'studio-resource-picker';
  NyxStudioResourceBrowserID = 'studio-resource-browser';
  NyxStudioResourceWorkspaceID = 'studio-resource-workspace';

implementation

uses nyx.resources, nyx.collections, nyx.binding.types, nyx.types, nyx.scheduler;

type
  { Typed collection events are authoritative. The legacy renderer callback
    intentionally forwards only click/change/named events on native targets.
    One managed receiver borrows the helper/facade without a reference cycle;
    the helper clears those pointers before cancelling tokens and retiring. }
  TResourceSelectionCallback = class(TNyxEventCallback)
  public
    Owner: TNyxStudioResourceBrowser;
    Views: TNyxStudioSectionViews;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

const
  CEditors: array[0..1] of TNyxText =
    (NyxStudioResourcePickerID, NyxStudioResourceBrowserID);

procedure TResourceSelectionCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if (Owner <> nil) and (Views <> nil) and (AEvent.Trigger = ntSelectionChange) then
  begin
    Owner.SyncActions(Views);
    Views.Sync;
  end;
end;

constructor TNyxStudioResourceBrowser.Create;
begin
  inherited Create;
  SetLength(FMounts, Length(CEditors));
  SetLength(FSelectionTokens, Length(CEditors));
  FSelectionCallback := TResourceSelectionCallback.Create;
  FSelectionLease := FSelectionCallback;
  TResourceSelectionCallback(FSelectionCallback).Owner := Self;
end;

destructor TNyxStudioResourceBrowser.Destroy;
var
  LIndex: Integer;
begin

  if FSelectionCallback <> nil then
  begin
    TResourceSelectionCallback(FSelectionCallback).Owner := nil;
    TResourceSelectionCallback(FSelectionCallback).Views := nil;
  end;
  for LIndex := 0 to High(FSelectionTokens) do
  begin

    if FSelectionTokens[LIndex] <> nil then
    begin
      FSelectionTokens[LIndex].Cancel;
      FSelectionTokens[LIndex] := nil;
    end;
  end;
  for LIndex := 0 to High(FMounts) do
  begin

    if FMounts[LIndex] <> nil then
    begin
      FMounts[LIndex].Disconnect;
      FMounts[LIndex] := nil;
    end;
  end;
  FSelectionLease := nil;
  FSelectionCallback := nil;
  FCatalog := nil;
  inherited Destroy;
end;

function TNyxStudioResourceBrowser.Compatible(ASession: TNyxStudioSession): Boolean;
var
  LIndex: Integer;
begin
  Result := (FCatalog <> nil) and ASession.MatchesCommandContext(FContext);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FMounts) do
  begin

    if (FMounts[LIndex] <> nil) and FMounts[LIndex].Connected and
      (FMounts[LIndex].View <> FCatalog.View) then
    begin
      Exit(False);
    end;
  end;
end;

function TNyxStudioResourceBrowser.Prepare(
  ASession: TNyxStudioSession): INyxPreparedPublication;
begin
  Result := nil;

  if (FCatalog = nil) or not ASession.MatchesCommandContext(FContext) then
  begin
    FCatalog := NewNyxResourceCatalog(NyxCollection('studio-resource-catalog'),
      ASession.Document.Resources);
    FContext := ASession.CommandContext;
  end
  else
  begin
    Result := FCatalog.Prepare(ASession.Document.Resources);
  end;
end;

procedure TNyxStudioResourceBrowser.Mount(AViews: TNyxStudioSectionViews;
  const APrepared: INyxPreparedPublication; const AState: TNyxResourceBrowserState;
  const ASelection: TNyxResourceEditorSelection);
var
  LIndex: Integer;
  LItem: TNyxItemRef;
begin
  TResourceSelectionCallback(FSelectionCallback).Views := AViews;

  if APrepared <> nil then
  begin
    PublishNyxGroup([APrepared]);
  end;
  FCatalog.Filter(AState.Query);

  if not FCatalog.View.HasSelection and ASelection.Reference.Defined then
  begin
    LItem := FCatalog.Find(ASelection.Reference, ASelection.Locale);

    if LItem.Defined then
    begin
      FCatalog.View.Select(LItem);
    end;
  end;
  for LIndex := 0 to High(CEditors) do
  begin

    if AViews.Root.Find(CEditors[LIndex]) = nil then
    begin
      Continue;
    end;

    if (FMounts[LIndex] <> nil) and FMounts[LIndex].Connected and
      (FMounts[LIndex].View <> FCatalog.View) then
    begin
      FMounts[LIndex].Disconnect;
    end;

    if (FMounts[LIndex] = nil) or not FMounts[LIndex].Connected then
    begin
      FMounts[LIndex] := AViews.BindCollection(
        NyxResourceBrowserListID(CEditors[LIndex]), FCatalog.View);
    end;

    if (FSelectionTokens[LIndex] = nil) or not FSelectionTokens[LIndex].Active then
    begin
      FSelectionTokens[LIndex] := AViews.ViewFor(NyxResourceBrowserListID(CEditors[LIndex]))
        .Events.On(NyxControlEvents(NyxResourceBrowserListID(CEditors[LIndex]), niRuntime),
          ntSelectionChange).Subscribe(FSelectionLease);
    end;
  end;
  SyncActions(AViews);
  AViews.Sync;
end;

procedure TNyxStudioResourceBrowser.SyncActions(AViews: TNyxStudioSectionViews);
var
  LButton: TNyxNode;
  LEnabled: Boolean;
begin
  LEnabled := FCatalog.View.HasSelection and
    FCatalog.View.Snapshot.Has(FCatalog.View.Selected);
  LButton := AViews.Root.Find(NyxResourceBrowserActionID(NyxStudioResourceBrowserID, rbaOpen));

  if LButton <> nil then
  begin
    { Hidden membership survives filtering, but never silently opens a file
      outside the displayed result. Resetting the query can reveal it again. }
    LButton.Configure.Enabled(LEnabled).Done;
  end;
end;

procedure TNyxStudioResourceBrowser.Capture(AViews: TNyxStudioSectionViews;
  var AState: TNyxResourceBrowserState);
var
  LIndex: Integer;
  LEditor: TNyxNode;
begin

  if AViews.Root = nil then
  begin
    Exit;
  end;
  for LIndex := High(CEditors) downto 0 do
  begin
    LEditor := AViews.Root.Find(CEditors[LIndex]);

    if LEditor <> nil then
    begin
      AState := ReadNyxResourceBrowser(LEditor);
      Exit;
    end;
  end;
end;

function TNyxStudioResourceBrowser.Handle(ANode: TNyxNode;
  const AEvent: TNyxEventInfo; AViews: TNyxStudioSectionViews;
  ASession: TNyxStudioSession; var AState: TNyxResourceBrowserState;
  out AIntent: TNyxStudioResourceBrowseIntent;
  out ASelection: TNyxResourceEditorSelection): Boolean;
var
  LIndex: Integer;
  LFace: Integer;
  LEditor: TNyxNode;
  LOther: TNyxNode;
  LState: TNyxResourceBrowserState;
  LChoice: TNyxResourceCatalogChoice;
begin
  Result := False;
  AIntent := rbiNone;
  ASelection := NyxNewResourceSelection;

  if (ANode = nil) or not Compatible(ASession) then
  begin
    Exit;
  end;
  for LFace := 0 to High(CEditors) do
  begin
    LEditor := AViews.Root.Find(CEditors[LFace]);

    if (LEditor = nil) or (LEditor.Find(ANode.ID) <> ANode) then
    begin
      Continue;
    end;

    if (ANode.ID = NyxResourceBrowserListID(LEditor.ID)) and
      (AEvent.Trigger = ntSelectionChange) then
    begin
      SyncActions(AViews);
      AViews.Sync;
      Exit(True);
    end;

    if (AEvent.Trigger = ntClick) and
      (ANode.ID = NyxResourceBrowserActionID(LEditor.ID, rbaOpen)) then
    begin

      if LFace = 0 then
      begin
        { Opening the workspace preserves its parked unfinished proposal. }
        AIntent := rbiWorkspace;
      end
      else
      begin

        if not FCatalog.View.HasSelection or
          not FCatalog.View.Snapshot.Has(FCatalog.View.Selected) then
        begin
          raise ENyxResource.Create('Choose a resource in the displayed list before opening it');
        end;
        LChoice := FCatalog.Choice(FCatalog.View.Selected,
          FCatalog.View.Selection.DataRevision);
        { A metadata revision does not identify content. Navigation opens the
          latest accepted exact pair; Apply guards that content separately. }
        ASession.Document.Resources.Definition(LChoice.Reference, LChoice.Locale);
        ASelection := NyxResourceSelection(LChoice.Reference, LChoice.Locale);
        AIntent := rbiOpen;
      end;
      Exit(True);
    end;

    if (AEvent.Trigger = ntChange) and NyxResourceBrowserInput(ANode, LEditor) then
    begin
      LState := ReadNyxResourceBrowser(LEditor);
    end
    else if (AEvent.Trigger <> ntClick) or
      not PrepareNyxResourceBrowserAction(ANode, LEditor, LState) then
    begin
      Exit(False);
    end;
    { Pure disclosure retains the mounted collection revision and selection.
      Filtering it again would turn a presentation toggle into a data change. }

    if ANode.ID <> NyxResourceBrowserActionID(LEditor.ID, rbaFilters) then
    begin
      FCatalog.Filter(LState.Query);
    end;
    AState := LState;
    for LIndex := 0 to High(CEditors) do
    begin
      LOther := AViews.Root.Find(CEditors[LIndex]);

      if LOther <> nil then
      begin
        RestoreNyxResourceBrowser(LOther, LState);
      end;
    end;
    SyncActions(AViews);
    AViews.Sync;
    Exit(True);
  end;
end;

end.
