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
program nyx_content_live_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Forms, Controls, StdCtrls, Classes,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.content,
  nyx.state, nyx.behavior, nyx.presentations, nyx.editing, nyx.generated.view
  , nyx.collections, nyx.collections.view.types, nyx.collections.view,
  nyx.collections.selection, nyx.codec, nyx.containers, nyx.responsive
  {$ifdef PAS2JS}, JS, Web, nyx.editing.browser, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  THost = TForm;
  TControlAccess = class(TControl);
  {$endif}

  { A receiver retains only a managed weak view capability. It never owns a
    renderer, document or native handle. Factory failure is deliberate evidence. }
  TObservation = class
  public
    Clicks: Integer;
    FactoryCalls: Integer;
    SelectOnClick: Boolean;
    Lease: INyxPresentationView;
    { Borrowed only during the review's registered synchronous callback. }
    Renderer: TRenderer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

  { The review owns resources across real UI queue turns. All source additions
    are explicitly typed fixture instrumentation over the unchanged compiled
    generated companion; they are not represented as MCP-authored definitions. }
  TReview = class
  private
    FRenderer: TRenderer;
    FOther: TRenderer;
    FFault: TRenderer;
    FHost: THost;
    FOtherHost: THost;
    FFaultHost: THost;
    FStore: TNyxState;
    FOtherStore: TNyxState;
    FFaultStore: TNyxState;
    FObservation: TObservation;
    FLease: INyxPresentationView;
    FFaultLease: INyxPresentationView;
    FWideID: TNyxText;
    FCompactID: TNyxText;
    FWideNumberID: TNyxText;
    FCompactNumberID: TNyxText;
    FWideSummaryID: TNyxText;
    FCompactSummaryID: TNyxText;
    FWideListID: TNyxText;
    FCompactListID: TNyxText;
    FCollection: INyxCollection;
    FCollectionRevision: Integer;
    FSourceDesign: TNyxText;
    FContainerDesign: TNyxText;
    FContainerLease: INyxPresentationView;
    FOtherRevision: Integer;
    FText: TNyxText;
    FTextSelection: TNyxTextSelection;
    FRevision: Integer;
    FRemembered: TFace;
    FFaultRoot: TNyxNode;
    FStage: Integer;
    FPolls: Integer;
    procedure Setup;
    function Ready(ACondition: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Step: Boolean;
    {$ifdef PAS2JS}
    { Optional bounded capture after the teardown assertions. These are fresh
      proof views from the same copied fixture source, not editor automation. }
    procedure ShowPreview;
    {$endif}
  end;

var
  GReview: TReview;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TObservation.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LAccepted: TNyxNode;
begin

  if AEvent.Trigger = ntClick then
  begin
    Inc(Clicks);

    if SelectOnClick then
    begin
      LAccepted := Renderer.Root;
      Lease.Select(NyxPresentation('focused'));
      {$ifndef PAS2JS}CheckSynchronize;{$endif}
      Check(Renderer.Root = LAccepted,
        'Nested UI queue service cannot retire a synchronous callback control set');
    end;
  end;
end;

function RefuseMemo(ANode: TNyxNode
  {$ifndef PAS2JS}; AOwner: TComponent{$endif}): TFace;
begin
  Result := nil;
  Inc(GReview.FObservation.FactoryCalls);
  raise Exception.Create('Controlled recipe factory failure');
end;

{ Bound extension factories require an updater. This deliberate failure occurs
  during construction, so its declared updater can never enter publication. }
procedure UpdateRefusedMemo(ANode: TNyxNode; AFace: TFace);
begin
  raise Exception.Create('A refused candidate must not enter its updater');
end;

function NewHost(AWidth: Integer): THost;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('div'));
  Result.style.setProperty('width', IntToStr(AWidth) + 'px');
  Result.style.setProperty('height', '700px');
  document.body.appendChild(Result);
  {$else}
  Result := TForm.Create(nil);
  Result.ClientWidth := AWidth;
  Result.ClientHeight := 700;
  {$endif}
end;

procedure SetWidth(AHost: THost; AWidth: Integer);
begin
  {$ifdef PAS2JS}AHost.style.setProperty('width', IntToStr(AWidth) + 'px');
  {$else}AHost.ClientWidth := AWidth;{$endif}
end;

function Face(ARenderer: TRenderer; const AID: TNyxText): TFace;
begin
  Result := nil;

  if ARenderer.Root.Find(AID) <> nil then
  begin
    Result := ARenderer.InputFor(AID, niRuntime);
  end;
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := NyxBrowserInputText(AFace);
  {$else}Result := TCustomEdit(AFace).Text;{$endif}
end;

procedure PutText(AFace: TFace; const AText: TNyxText; ACommit: Boolean);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AText;

  if ACommit then
  begin
    AFace.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}
  TCustomEdit(AFace).Text := AText;
  {$endif}
end;

function Focused(AFace: TFace): Boolean;
begin
  {$ifdef PAS2JS}Result := document.activeElement = AFace;
  {$else}Result := Screen.ActiveControl = AFace;{$endif}
end;

procedure ClickAction(ARenderer: TRenderer; const AID: TNyxText);
begin
  {$ifdef PAS2JS}ARenderer.ElementFor(AID).click;
  {$else}TControlAccess(ARenderer.ControlFor(AID)).Click;{$endif}
end;

constructor TReview.Create;
begin
  inherited Create;
  FRenderer := TRenderer.Create;
  FOther := TRenderer.Create;
  FFault := TRenderer.Create;
  FObservation := TObservation.Create;
end;

destructor TReview.Destroy;
begin
  FRenderer.Free;
  FOther.Free;
  FFault.Free;
  FObservation.Free;
  FStore.Free;
  FOtherStore.Free;
  FFaultStore.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;

  if FOtherHost <> nil then
  begin
    FOtherHost.remove;
  end;

  if FFaultHost <> nil then
  begin
    FFaultHost.remove;
  end;
  {$else}
  FHost.Free;
  FOtherHost.Free;
  FFaultHost.Free;
  {$endif}
  inherited Destroy;
end;

procedure TReview.Setup;
var
  LDocument: TNyxDocument;
  LCompact: TNyxText;
  LNumber: INyxInput;
  LCollectionSpec: TNyxCollectionViewSpec;
  LSummaryInput: INyxInput;
  LSummaryMemo: INyxMemo;
begin
  LDocument := BuildNyxDocument;
  try
    LCompact := LDocument.Find('workspace').Content.Rule(1).Component.Name;
    LDocument.Find('wide-name').Configure.PartName(NyxPart('notes')).Done;
    LDocument.Find('compact-notes').Configure.PartName(NyxPart('notes')).Done;
    LDocument.State.SetValue(NyxNumberState('estimate'), Double(1.25));
    LDocument.Collections.Define(NyxCollection('tasks'),
      NyxCollectionSchema.Text(NyxTextField('caption'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'first'))
        .WithValue(NyxTextField('caption'), 'Task from blueprint')]);
    LCollectionSpec := NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('caption'), 'Task', cmEditable).Scoped(csInstance);
    LDocument.Find('wide-form').Add(NewNyxList('wide-tasks')
      .Configure.PartName(NyxPart('tasks')).Done.Binds.Collection(LCollectionSpec).Done);
    LDocument.Find(LCompact).Add(NewNyxList('compact-tasks')
      .Configure.PartName(NyxPart('tasks')).Done.Binds.Collection(LCollectionSpec).Done);
    LSummaryInput := NewNyxInput('wide-summary');
    LSummaryInput.Configure.PartName(NyxPart('summary')).Done;
    LSummaryInput.Value := 'Summary default';
    LDocument.Find('wide-form').Add(LSummaryInput);
    LSummaryMemo := NewNyxMemo('compact-summary');
    LSummaryMemo.Configure.PartName(NyxPart('summary')).Done;
    LSummaryMemo.Value := 'Different summary default';
    LDocument.Find(LCompact).Add(LSummaryMemo);
    LNumber := NewNyxInput('wide-estimate');
    LNumber.Configure.InputType(niNumber).PartName(NyxPart('estimate')).Done;
    LNumber.Binds.Value(NyxNumberState('estimate')).Done;
    LDocument.Find('wide-form').Add(LNumber);
    LNumber := NewNyxInput('compact-estimate');
    LNumber.Configure.InputType(niNumber).PartName(NyxPart('estimate')).Done;
    LNumber.Binds.Value(NyxNumberState('estimate')).Done;
    LDocument.Find(LCompact).Add(LNumber);
    LDocument.Find('wide-form').Add(NewNyxButton('wide-action').WithText('Focus workspace'));
    LDocument.Find(LCompact).Add(NewNyxButton('compact-action').WithText('Save idea'));
    FStore := LDocument.State.Clone;
    FOtherStore := LDocument.State.Clone;
    FFaultStore := LDocument.State.Clone;
    FOtherStore.SetValue(NyxTextState('notes'), 'Independent writer');
    FHost := NewHost(900);
    FOtherHost := NewHost(390);
    FFaultHost := NewHost(900);
    FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}FObservation.Event;
    FFault.RegisterFactory(NyxKindName(nkMemo), {$ifdef PAS2JS}@{$endif}RefuseMemo,
      {$ifdef PAS2JS}@{$endif}UpdateRefusedMemo);
    FRenderer.Render(LDocument, LDocument.Pages[0], FHost, False, FStore);
    FOther.Render(LDocument, LDocument.Pages[0], FOtherHost, False, FOtherStore);
    FFault.Render(LDocument, LDocument.Pages[0], FFaultHost, False, FFaultStore);
    FSourceDesign := TNyxCodec.Encode(LDocument);
    {$ifndef PAS2JS}
    FOtherHost.Show;
    FHost.Show;
    FHost.BringToFront;
    {$endif}
  finally
    LDocument.Free;
  end;
  FWideID := NyxQualifiedID('workspace', 'wide-name');
  FCompactID := NyxQualifiedID('workspace', 'compact-notes');
  FWideNumberID := NyxQualifiedID('workspace', 'wide-estimate');
  FCompactNumberID := NyxQualifiedID('workspace', 'compact-estimate');
  FWideSummaryID := NyxQualifiedID('workspace', 'wide-summary');
  FCompactSummaryID := NyxQualifiedID('workspace', 'compact-summary');
  FWideListID := NyxQualifiedID('workspace', 'wide-tasks');
  FCompactListID := NyxQualifiedID('workspace', 'compact-tasks');
  FCollection := FRenderer.CollectionView(FWideListID).Store;
  FCollection.Update(NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'first'))
    .WithValue(NyxTextField('caption'), 'Accepted task'));
  FRenderer.CollectionView(FWideListID).Select(NyxItem(NyxCollection('tasks'), 'first'));
  FCollectionRevision := FCollection.Snapshot.Revision;
  FLease := FRenderer.Presentations;
  FObservation.Lease := FLease;
  FObservation.Renderer := FRenderer;
  FFaultLease := FFault.Presentations;
  FText := TNyxText('A') + NyxScalarText($1F680) + TNyxText(' shared idea');
  Check(Face(FRenderer, FWideID) <> nil, 'Initial wide recipe is mounted');
  Check(Face(FOther, FCompactID) <> nil, 'Concurrent compact recipe is mounted');
  PutText(Face(FRenderer, FWideID), FText, True);
  PutText(Face(FRenderer, FWideSummaryID), FText, True);
  PutText(Face(FRenderer, FWideNumberID), '03.500', False);
  {$ifdef PAS2JS}Face(FRenderer, FWideID).focus;
  {$else}TWinControl(Face(FRenderer, FWideID)).SetFocus;{$endif}
  FRenderer.SetTextSelection(FWideID, NyxTextSelection(FText, 1, 2, ntdUnknown));
  FTextSelection := FRenderer.TextSelectionFor(FWideID);
  Check(FTextSelection.Defined and (FTextSelection.Start = 1) and
    (FTextSelection.Finish = 2), 'Initial target selection covers the exact supplementary scalar');
  FRevision := FStore.Revision;
  Check(FStore.Value(NyxNumberState('estimate').Name).NumberValue = Double(1.25),
    'Physical numeric draft remains separate from accepted state');
  Check(Focused(Face(FRenderer, FWideID)), 'Actual wide input receives focus');
  SetWidth(FHost, 390);
end;

function TReview.Ready(ACondition: Boolean): Boolean;
begin
  Result := ACondition;

  if not Result then
  begin
    Inc(FPolls);
    if FPolls >= 200 then
    begin
      raise Exception.Create('Live recipe publication timed out at ' + IntToStr(FStage) +
        TNyxText(': ') + FRenderer.LastContentError);
    end;
  end
  else
  begin
    FPolls := 0;
  end;
end;

function TReview.Step: Boolean;
var
  LDocument: TNyxDocument;
  LCompact: TNyxComponentRef;
begin
  Result := False;
  case FStage of
    0:
      Setup;
    1:
      begin

        if not Ready(Face(FRenderer, FCompactID) <> nil) then
        begin
          Exit;
        end;
        Check(Face(FRenderer, FWideID) = nil, 'Resize retires the old control set without Render');
        Check(TextOf(Face(FRenderer, FCompactID)) = FText, 'Named notes part keeps exact supplementary text');
        Check(TextOf(Face(FRenderer, FCompactSummaryID)) = FText,
          'Compatible unbound field retains its accepted value across recipe types');
        Check(TextOf(Face(FRenderer, FCompactNumberID)) = '03.500', 'Named numeric part keeps its unfinished physical draft');
        Check(FStore.Revision = FRevision, 'Structural resize writes no state');
        Check(FRenderer.CollectionView(FCompactListID).Store = FCollection,
          'The explicit recipe instance reuses its exact local collection store');
        Check(FCollection.Snapshot.Revision = FCollectionRevision,
          'Structural publication imports no collection defaults');
        Check(FRenderer.CollectionView(FCompactListID).HasSelection and
          (FRenderer.CollectionView(FCompactListID).Selected.ID = 'first'),
          'Compatible named list part keeps selection');
        Check(FRenderer.CollectionView(FCompactListID).CellText(
          NyxItem(NyxCollection('tasks'), 'first'), 0) = 'Accepted task',
          'The new collection view reads accepted runtime data');
        Check(FOther.CollectionView(FCompactListID).CellText(
          NyxItem(NyxCollection('tasks'), 'first'), 0) = 'Task from blueprint',
          'Concurrent recipe instances retain independent local collections');
        FRenderer.CollectionView(FCompactListID).Edit(
          NyxItem(NyxCollection('tasks'), 'first'), 0, TNyxStateValue.FromText('Renamed task'));
        {$ifdef PAS2JS}
        Check(Pos('Renamed task', FRenderer.ElementFor(FCompactListID).textContent) > 0,
          'Published browser collection receives live runtime edits');
        {$else}
        Check(TListBox(FRenderer.ControlFor(FCompactListID)).Items[0] = 'Renamed task',
          'Published native collection receives live runtime edits');
        {$endif}
        Check(FLease.Connected and (FRenderer.Presentations = FLease), 'Structural resize preserves the original managed capability');
        Check(Focused(Face(FRenderer, FCompactID)), 'Focus follows the shared named notes part');
        Check(FRenderer.TextSelectionFor(FCompactID).SameRange(FTextSelection),
          'Selection follows the supplementary Unicode scalar range');
        Check(TextOf(Face(FOther, FCompactID)) = 'Independent writer', 'Concurrent view keeps its independent store');
        ClickAction(FRenderer, NyxQualifiedID('workspace', 'compact-action'));
        Check(FObservation.Clicks = 1, 'New compact callback dispatches once');
        SetWidth(FHost, 900);
      end;
    2:
      begin

        if not Ready(Face(FRenderer, FWideID) <> nil) then
        begin
          Exit;
        end;
        Check(TextOf(Face(FRenderer, FWideNumberID)) = '03.500', 'Reverse resize keeps the same physical numeric draft');
        Check(TextOf(Face(FRenderer, FWideSummaryID)) = FText,
          'Reverse resize retains the unbound value instead of either source default');
        Check(Focused(Face(FRenderer, FWideID)), 'Reverse resize restores the logical focused field');
        Check(FRenderer.CollectionView(FWideListID).Store = FCollection,
          'Reverse resize retains the same local collection store');
        Check(FRenderer.CollectionView(FWideListID).HasSelection and
          (FRenderer.CollectionView(FWideListID).Selected.ID = 'first'),
          'Reverse resize retains logical list selection');
        FRemembered := Face(FRenderer, FWideID);
        SetWidth(FHost, 840);
      end;
    3:
      begin
        Check(Face(FRenderer, FWideID) = FRemembered, 'Same-recipe geometry retains the actual input object');
        LDocument := TNyxCodec.Decode(FSourceDesign);
        try
          LDocument.Find('wide-action').Configure.Text('Choose focused view').Done;
          LDocument.Find('compact-action').Configure.Text('Save revised idea').Done;
          Check(FRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
            'Retained source refresh selects the actual current host recipe');
          Check(Face(FRenderer, FWideID) = FRemembered,
            'Retained source refresh keeps the mounted input object');
          Check(TextOf(Face(FRenderer, FWideNumberID)) = '03.500',
            'Retained source refresh preserves the unfinished number draft');
          Check(TextOf(Face(FRenderer, FWideSummaryID)) = FText,
            'Retained source refresh preserves accepted unbound input');
        finally
          LDocument.Free;
        end;
        FObservation.SelectOnClick := True;
        ClickAction(FRenderer, NyxQualifiedID('workspace', 'wide-action'));
        FObservation.SelectOnClick := False;
        Check(FObservation.Clicks = 2, 'A sequential callback may request structural selection');
        Check(Face(FRenderer, FWideID) = FRemembered, 'Selection waits until borrowed callback controls unwind');
        Check(not FLease.Selection.Reference.Defined, 'Capability still reports its accepted selection before queued publication');
      end;
    4:
      begin

        if not Ready(Face(FRenderer, FCompactID) <> nil) then
        begin
          Exit;
        end;
        Check(FLease.Selection.Reference.Name = 'focused', 'The same capability publishes its manual selection');
        Check(FRenderer.Root.Find(NyxQualifiedID('workspace', 'compact-action')).Prop('text') =
          'Save revised idea', 'Future selection consumes the admitted replacement blueprint');
        Check(TextOf(Face(FRenderer, FCompactNumberID)) = '03.500', 'Manual selection preserves compatible named drafts');
        FRemembered := Face(FRenderer, FCompactID);
        SetWidth(FHost, 1000);
      end;
    5:
      begin
        Check(Face(FRenderer, FCompactID) = FRemembered, 'Manual selection remains active across host resizing');
        FLease.Automatic;
      end;
    6:
      begin

        if not Ready(Face(FRenderer, FWideID) <> nil) then
        begin
          Exit;
        end;
        Check(not FLease.Selection.Reference.Defined, 'Automatic clears only the view-local manual choice');
        Check(FStore.Revision = FRevision, 'Resize/manual transitions import no document defaults');
        FStore.SetValue(NyxNumberState('estimate'), Double(2.5));
        Check(TextOf(Face(FRenderer, FWideNumberID)) = '2.5', 'A new accepted value replaces its old numeric draft');
        FFaultRoot := FFault.Root;
        FFaultLease.Select(NyxPresentation('focused'));
      end;
    7:
      begin

        if not Ready(FFault.LastContentError <> '') then
        begin
          Exit;
        end;
        Check(FFault.Root = FFaultRoot, 'Failed recipe factory preserves the exact mounted tree');
        Check(FFaultLease.Connected and not FFaultLease.Selection.Reference.Defined,
          'Failed recipe factory preserves its accepted managed selection');
        Check(FObservation.FactoryCalls = 1, 'Selected factory failure is observed once: ' +
          IntToStr(FObservation.FactoryCalls) + TNyxText(' / ') + FFault.LastContentError);
        FFault.Sync;
      end;
    8:
      begin
        Check(FObservation.FactoryCalls = 1, 'The same failed observation cannot cause a retry storm');
        FFaultLease.Automatic;
      end;
    9:
      begin

        if not Ready(FFault.LastContentError = '') then
        begin
          Exit;
        end;
        Check(FFault.Root = FFaultRoot, 'Explicit automatic recovery retains the valid control set');
        FLease.Select(NyxPresentation('focused')).Automatic.Select(NyxPresentation('focused'));
      end;
    10:
      begin

        if not Ready(Face(FRenderer, FCompactID) <> nil) then
        begin
          Exit;
        end;
        Check(FLease.Selection.Reference.Name = 'focused', 'Related queued choices coalesce to the last request');
        Check(TextOf(Face(FRenderer, FCompactNumberID)) = '2.5', 'Coalesced publication consumes current accepted state');
        FLease.Automatic;
        FRenderer.Unmount;
        Check(not FLease.Connected, 'Unmount retires the stable capability');
      end;
    11:
      begin
        Check(FRenderer.Root = nil, 'Pending work cannot call into an unmounted view');
        Check(FObservation.Clicks = 2, 'Structural work never invents activation callbacks');
        { This independently owned mount now exercises allocated container
          space at an unchanged wide viewport. Fixture source editing uses the
          same admitted retained refresh; no private resize hook is invoked. }
        SetWidth(FOtherHost, 900);
        LDocument := TNyxCodec.Decode(FSourceDesign);
        try
          LCompact := LDocument.Find('workspace').Content.Rule(1).Component;
          LDocument.Find('home').Configure.QueryContainer(NyxContainer('workspace space'))
            .Containment(nccWidth).Width(390).Done;
          LDocument.Presentations.Define(NyxPresentation('compact space'),
            TNyxPresentationCondition.Within(NyxContainer('workspace space'),
              TNyxViewportCondition.Any.WidthBelow(640)));
          LDocument.Find('workspace').Content.WhenViewport(TNyxViewportWidth.Below(640)).Clear
            .WhenPresentation(NyxPresentation('compact space')).Use(LCompact).Done;
          FOther.Render(LDocument, LDocument.Pages[0], FOtherHost, False, FOtherStore);
          FContainerDesign := TNyxCodec.Encode(LDocument);
          FContainerLease := FOther.Presentations;
          FOtherRevision := FOtherStore.Revision;
        finally
          LDocument.Free;
        end;
      end;
    12:
      begin

        if not Ready(Face(FOther, FCompactID) <> nil) then
        begin
          Exit;
        end;
        Check(Face(FOther, FWideID) = nil,
          'An allocated narrow container chooses compact controls in a wide viewport');
        Check(TextOf(Face(FOther, FCompactID)) = 'Independent writer',
          'Allocated container publication retains accepted independent state');
        LDocument := TNyxCodec.Decode(FContainerDesign);
        try
          LDocument.Find('home').Configure.Width(900).Done;
          Check(FOther.TryRefresh(LDocument, LDocument.Pages[0], False),
            'A retained publisher width edit admits before structural recomposition');
        finally
          LDocument.Free;
        end;
      end;
    13:
      begin

        if not Ready(Face(FOther, FWideID) <> nil) then
        begin
          Exit;
        end;
        Check(FContainerLease.Connected and (FOther.Presentations = FContainerLease),
          'Allocated container transition preserves its managed capability');
        Check(Face(FOther, FCompactID) = nil,
          'Widening only the allocated publisher changes the actual control set');
        LDocument := TNyxCodec.Decode(FContainerDesign);
        try
          LDocument.Find('home').Configure.Width(250).Done;
          Check(FOther.TryRefresh(LDocument, LDocument.Pages[0], False),
            'A reverse allocated publisher edit uses the current wide recipe');
        finally
          LDocument.Free;
        end;
      end;
    14:
      begin

        if not Ready(Face(FOther, FCompactID) <> nil) then
        begin
          Exit;
        end;
        {$ifdef PAS2JS}
        Check(FOtherHost.clientWidth = 900, 'Container choices leave the outer browser viewport unchanged');
        {$else}
        Check(FOtherHost.ClientWidth = 900, 'Container choices leave the outer native viewport unchanged');
        {$endif}
        Check(TextOf(Face(FOther, FCompactID)) = 'Independent writer',
          'Reverse allocated container transition preserves accepted values');
        Check(FOtherStore.Revision = FOtherRevision,
          'Allocated container transitions import no defaults or state writes');
        FOther.Unmount;
        Check(not FContainerLease.Connected, 'Container unmount retires its managed capability');
      end;
    15:
      begin
        Check(FOther.Root = nil, 'Container observation work stays inert after unmount');
        Result := True;
      end;
  end;
  Inc(FStage);
end;

{$ifdef PAS2JS}
procedure TReview.ShowPreview;
var
  LDocument: TNyxDocument;
begin
  FHost.style.setProperty('width', '700px');
  FOtherHost.style.setProperty('width', '390px');
  FFaultHost.style.setProperty('display', 'none');
  TJSHTMLElement(document.body).style.setProperty('display', 'flex');
  TJSHTMLElement(document.body).style.setProperty('gap', '16px');
  LDocument := TNyxCodec.Decode(FSourceDesign);
  try
    FRenderer.Render(LDocument, LDocument.Pages[0], FHost, False, FStore);
    FOther.Render(LDocument, LDocument.Pages[0], FOtherHost, False, FOtherStore);
  finally
    LDocument.Free;
  end;
end;

procedure ReleasePreview;
begin
  FreeAndNil(GReview);
end;
{$endif}

procedure Drive;
begin

  if GReview = nil then
  begin
    Exit;
  end;
  try

    if GReview.Step then
    begin
      {$ifdef PAS2JS}

      if Pos('preview', window.location.search) > 0 then
      begin
        GReview.ShowPreview;
        window.setTimeout(@ReleasePreview, 2000);
      end
      else
      {$endif}
      begin
        FreeAndNil(GReview);
      end;
      WriteLn('PASS ', GChecks, ' actual live content checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'passed');
      document.body.setAttribute('data-projection-refresh-checks', IntToStr(GChecks));
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LException.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GReview := TReview.Create;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  while GReview <> nil do
  begin
    Drive;
    Application.ProcessMessages;
    CheckSynchronize(10);
  end;
  CheckSynchronize;
  {$endif}
end.
