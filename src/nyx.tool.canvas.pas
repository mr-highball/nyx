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


unit nyx.tool.canvas;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.behavior, nyx.canvas.types, nyx.canvas.scene, nyx.canvas.workspace,
  nyx.tool.ports, nyx.scheduler, nyx.tool.connections, nyx.events;

type
  { Public portable inspector/controller consumed by both target attachments.
    It owns an independent source context and Nyx form, never a widget/renderer.
    Runtime scene/behavior access is revoked by Disconnect before borrowed hosts
    retire. The optional connection capability below adds runtime graph editing
    through public Nyx forms; script/source editing retains its separate owner. }
  INyxToolCanvasInspector = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000019}']
    procedure Disconnect;
    function GetDocument: TNyxDocument;
    function GetRoot: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
    procedure SelectControl(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef);
    property Document: TNyxDocument read GetDocument;
    property Root: INyxCard read GetRoot;
  end;

  { Optional connection editing preserves the original inspector ABI. Physical
    adapters provide a scoped gesture response; programmatic creators can use
    the same typed controller without acquiring widgets or renderer authority.
    ConnectionEditor is nil for creator views lacking optional flow editing. }
  INyxToolCanvasConnections = interface(IInterface)
    ['{8902BF7F-5E04-40A0-A098-91DB02CC1501}']
    function ConnectionEditor: INyxToolConnectionEditor;
    procedure ShowConnections(AVisible: Boolean = True);
    function HandleGesture(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse): Boolean;
  end;

  { Adapter-owned face. Close precedes disposal of its borrowed host and the
    canvas it observes. Inspector values remain ordinary public Nyx controls. }
  INyxToolCanvasView = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000020}']
    procedure Disconnect;
    procedure Refresh;
    function GetInspector: INyxToolCanvasInspector;
    function GetRefreshExecution: INyxExecution;
    property Inspector: INyxToolCanvasInspector read GetInspector;
    { Deferred physical publication reports ordinary scheduler failure. Its
      diagnostic can safely outlive the retired view. }
    property RefreshExecution: INyxExecution read GetRefreshExecution;
  end;

{ An existing ordinary canvas view supplies independently owned item content and
  optional behavior. The form uses normal Nyx cards/inputs/selects/switches/badges.
  Query and execution feedback are bounded; creator callback payloads are never
  displayed as executable source or persisted runtime authority. }
function NewNyxToolCanvasInspector(const AID: TNyxText; ADocument: TNyxDocument;
  const AView: INyxCanvasWorkspaceView): INyxToolCanvasInspector;

implementation

uses
  nyx.composition, nyx.logic.catalog, nyx.flow.types, nyx.view.behavior,
  nyx.state, nyx.root.types, nyx.codec;

type
  { Closed inspector presentation; connections use the available panel space
    instead of adding another long form below control port inspection. }
  TInspectorTab = (titPorts, titConnections);

  { Independent UI form/source owner. Its connection controller retains only
    the separate canvas view; no returned node retains this inspector. }
  TInspector = class(TInterfacedObject, INyxToolCanvasInspector, INyxToolCanvasConnections)
  private
    FID: TNyxText;
    FSource: TNyxDocument;
    FDocument: TNyxDocument;
    FRoot: INyxCard;
    FView: INyxCanvasWorkspaceView;
    FRuntime: INyxCanvasRuntimeView;
    FItems: array of TNyxCanvasItemRef;
    FItemLabels: array of TNyxText;
    FControls: array of TNyxControlRef;
    FControlLabels: array of TNyxText;
    { One effective item snapshot avoids revalidating unrelated scenes on every
      search/page/toggle. Exact content bytes and scene revision invalidate it;
      external mutation of a scene control cannot leave stale port discovery. }
    FEffectiveItem: TNyxCanvasItemRef;
    FEffectiveContext: TNyxDocument;
    FEffectiveRoot: TNyxNode;
    FEffectiveRevision: Integer;
    FEffectiveContent: TNyxText;
    FSelectedItem: TNyxCanvasItemRef;
    FSelectedControl: TNyxControlRef;
    FQuery: TNyxText;
    FPortOffset: Integer;
    FShowPorts: Boolean;
    FConnected: Boolean;
    FUpdating: Boolean;
    FConnections: INyxToolConnectionEditor;
    FTab: TInspectorTab;
    procedure AddTabs(const AParent: INyxCard);
    procedure PublishForm(const ARoot: INyxCard);
    procedure RequireConnected;
    procedure Rebuild;
    function PartID(const APart: TNyxText): TNyxText;
    function CurrentBehavior: INyxViewBehavior;
  public
    constructor Create(const AID: TNyxText; ADocument: TNyxDocument;
      const AView: INyxCanvasWorkspaceView);
    destructor Destroy; override;
    procedure Disconnect;
    function GetDocument: TNyxDocument;
    function GetRoot: INyxCard;
    function Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
    procedure SelectControl(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef);
    function ConnectionEditor: INyxToolConnectionEditor;
    procedure ShowConnections(AVisible: Boolean = True);
    function HandleGesture(ANode: TNyxNode; const AEvent: TNyxEventInfo;
      const AResponse: INyxGestureResponse): Boolean;
  end;

function Caption(const AText: TNyxText; AMaximum: Integer): TNyxText;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  Result := '';
  LIndex := 1;
  LCount := 0;
  while (LIndex <= Length(AText)) and (LCount < AMaximum) do
  begin
    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Tool port caption has invalid Unicode');
    end;

    if LScalar < 32 then
    begin
      Result := Result + ' ';
    end
    else
    begin
      Result := Result + NyxScalarText(LScalar);
    end;
    Inc(LCount);
  end;
end;

constructor TInspector.Create(const AID: TNyxText; ADocument: TNyxDocument;
  const AView: INyxCanvasWorkspaceView);
var
  LRuntime: INyxCanvasRuntimeView;
  LEditing: INyxCanvasFlowEditing;
begin
  inherited Create;

  if (ADocument = nil) or (AView = nil) or
    not Supports(AView, INyxCanvasRuntimeView, LRuntime) then
  begin
    raise ENyxModel.Create('Tool Canvas inspector requires its source and mounted runtime view');
  end;
  { pas2js finalizes a direct interface out-field argument on routine exit.
    Admit into a local first, then assign an ordinary retained field lease. }
  FRuntime := LRuntime;
  FID := AID;
  FSource := ADocument.Clone;
  { The ordinary inspector form owns only its private controls. Retaining the
    complete canvas/application in that render document repeatedly validates
    unrelated scenes and graphs on each UI refresh. Discovery uses the separate
    immutable source context; no state projection is authored by this form. }
  FDocument := TNyxDocument.Create;
  FView := AView;
  FConnected := True;
  FShowPorts := True;

  if Supports(FView, INyxCanvasFlowEditing, LEditing) then
  begin
    FConnections := NewNyxToolConnectionEditor(PartID('connections'), FView);
  end;
  Rebuild;
end;

destructor TInspector.Destroy;
begin
  Disconnect;
  ReleaseNyxNode(FEffectiveRoot);
  FEffectiveContext.Free;
  FSource.Free;
  FDocument.Free;
  FRoot := nil;
  inherited Destroy;
end;

procedure TInspector.RequireConnected;
begin

  if not FConnected or (FView = nil) or not FView.Connected then
  begin
    raise ENyxModel.Create('Tool Canvas inspector is retired');
  end;
end;

procedure TInspector.Disconnect;
begin
  FConnected := False;

  if FConnections <> nil then
  begin
    FConnections.Disconnect;
  end;
  FView := nil;
  FRuntime := nil;
end;

function TInspector.GetDocument: TNyxDocument;
begin
  RequireConnected;
  Result := FDocument;
end;

function TInspector.GetRoot: INyxCard;
begin
  RequireConnected;
  Result := FRoot;
end;

function TInspector.PartID(const APart: TNyxText): TNyxText;
begin
  Result := FID + '-' + APart;
end;

function TInspector.CurrentBehavior: INyxViewBehavior;
begin
  RequireConnected;
  Result := nil;

  if FSelectedItem.Defined then
  begin
    try
      Result := FRuntime.BehaviorFor(FSelectedItem);
    except
      on E: ENyxCanvasScene do
      begin
        { Off-screen items may not have a physical mount yet. Inspector discovery
          still works from their owned content, without creating hidden controls. }
      end;
    end;
  end;
end;

procedure TInspector.Rebuild;
var
  LNew: INyxCard;
  LItems: INyxSelect;
  LControls: INyxSelect;
  LSwitch: INyxSwitch;
  LActions: INyxRow;
  LPorts: INyxToolControlPorts;
  LRows: INyxColumn;
  LRow: INyxCard;
  LPort: TNyxToolControlPort;
  LSnapshot: INyxCanvasSnapshot;
  LContent: INyxNode;
  LRuntimeRoot: TNyxNode;
  LContext: TNyxDocument;
  LAuthoredRoot: TNyxNode;
  LNode: TNyxNode;
  LBehavior: INyxViewBehavior;
  LIndex: Integer;
  LCount: Integer;
  LTotal: Integer;
  LList: TNyxText;
  LLabel: TNyxText;
  LItemChoice: TNyxText;
  LControlChoice: TNyxText;
  LStatus: TNyxText;
  LReport: TNyxDataValue;
  LConnected: Boolean;
  LContentKey: TNyxText;

  procedure Collect(ANode: TNyxNode);
  var
    LIndex: Integer;
    LCount: Integer;
  begin
    { The UI offers a bounded initial control window. Public SelectControl can
      address any exact effective identity; broader searchable paging stays open. }

    if Length(FControls) < 128 then
    begin
      LCount := Length(FControls);
      SetLength(FControls, LCount + 1);
      SetLength(FControlLabels, LCount + 1);
      FControls[LCount] := NyxControl(ANode.ID);
      FControlLabels[LCount] := IntToStr(LCount + 1) + ' / ' + Caption(ANode.ID, 80) +
        ' / ' + Caption(ANode.Kind, 32);
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LIndex]);
    end;
  end;

begin
  RequireConnected;

  if FUpdating then
  begin
    raise ENyxModel.Create('Tool Canvas inspector is already updating');
  end;
  FUpdating := True;
  LRuntimeRoot := nil;
  LContext := nil;
  try

    if (FTab = titConnections) and (FConnections <> nil) then
    begin
      LNew := NewNyxCard(FID);
      AddTabs(LNew);
      LNew.Add(FConnections.Build);
      PublishForm(LNew);
      Exit;
    end;
    LSnapshot := FRuntime.Scene.Snapshot;
    SetLength(FItems, LSnapshot.Count);
    SetLength(FItemLabels, LSnapshot.Count);
    LItemChoice := '';
    LList := '';
    for LIndex := 0 to LSnapshot.Count - 1 do
    begin
      FItems[LIndex] := LSnapshot.Items[LIndex].Item;
      FItemLabels[LIndex] := IntToStr(LIndex + 1) + ' / ' + Caption(FItems[LIndex].Name, 80);

      if LIndex > 0 then
      begin
        LList := LList + #10;
      end;
      LList := LList + FItemLabels[LIndex];

      if FSelectedItem.Defined and (FItems[LIndex].Name = FSelectedItem.Name) then
      begin
        LItemChoice := FItemLabels[LIndex];
      end;
    end;

    if (LItemChoice = '') and (Length(FItems) > 0) then
    begin
      FSelectedItem := FItems[0];
      LItemChoice := FItemLabels[0];
    end;
    LNew := NewNyxCard(FID);
    AddTabs(LNew);
    LNew.Add(NewNyxHeading(PartID('title')).WithText('Tool ports'));
    LNew.Add(NewNyxLabel(PartID('help')).WithText(
      'Inspect real event outputs and state inputs. Graph execution is independent from static callbacks.'));
    LItems := NewNyxSelect(PartID('item')).WithText('Canvas item');
    LItems.Configure.Items(LList).Value(LItemChoice).Done;
    LNew.Add(LItems);
    FControls := nil;
    FControlLabels := nil;
    LControlChoice := '';

    if LItemChoice <> '' then
    begin
      LContent := FRuntime.Scene.Control(FSelectedItem);
      LContentKey := TNyxCodec.NodeSnapshot(LContent.Node).ToJSON;

      if (FEffectiveRoot = nil) or (FEffectiveItem.Name <> FSelectedItem.Name) or
        (FEffectiveRevision <> FRuntime.Scene.Revision) or
        (FEffectiveContent <> LContentKey) then
      begin
        LContext := NewNyxCanvasRenderContext(FSource, LContent.Node, LAuthoredRoot);
        LRuntimeRoot := RealizeNyxView(LContext, LAuthoredRoot);
        ReleaseNyxNode(FEffectiveRoot);
        FreeAndNil(FEffectiveContext);
        FEffectiveRoot := LRuntimeRoot;
        FEffectiveContext := LContext;
        FEffectiveItem := FSelectedItem;
        FEffectiveRevision := FRuntime.Scene.Revision;
        FEffectiveContent := LContentKey;
        LRuntimeRoot := nil;
        LContext := nil;
      end;
      LRuntimeRoot := FEffectiveRoot;
      LContext := FEffectiveContext;
      Collect(LRuntimeRoot);
      LNode := LRuntimeRoot.Find(FSelectedControl.ID);

      if LNode = nil then
      begin
        LNode := LRuntimeRoot;
        FSelectedControl := NyxControl(LNode.ID);
      end;
      LList := '';
      for LIndex := 0 to High(FControls) do
      begin

        if LIndex > 0 then
        begin
          LList := LList + #10;
        end;
        LList := LList + FControlLabels[LIndex];

        if FControls[LIndex].ID = FSelectedControl.ID then
        begin
          LControlChoice := FControlLabels[LIndex];
        end;
      end;
      LControls := NewNyxSelect(PartID('control')).WithText('Control');
      LControls.Configure.Items(LList).Value(LControlChoice).Done;
      LNew.Add(LControls);
      LBehavior := CurrentBehavior;
      LConnected := LBehavior <> nil;
      LSwitch := NewNyxSwitch(PartID('graphs')).WithText('Run event graphs');
      LSwitch.Configure.Enabled(LConnected).Value(LConnected and LBehavior.GraphsEnabled).Done;
      LNew.Add(LSwitch);
      LSwitch := NewNyxSwitch(PartID('ports')).WithText('Show ports');
      LSwitch.Configure.Value(FShowPorts).Done;
      LNew.Add(LSwitch);
      LActions := NewNyxRow(PartID('actions'));
      LActions.Add(NewNyxButton(PartID('refresh')).WithText('Refresh'));
      LActions.Add(NewNyxButton(PartID('cancel')).WithText('Cancel active work'));
      LNew.Add(LActions);
      LStatus := 'No runtime attachment: this item is off screen or is a design preview.';

      if LConnected then
      begin
        LCount := 0;
        LStatus := '';
        for LIndex := 0 to LBehavior.Routes.Count - 1 do
        begin
          LReport := LBehavior.Routes.Report(LIndex);
          Inc(LCount, LReport.Field('active').AsInteger);

          if LReport.Field('failure').AsText <> '' then
          begin
            LStatus := Caption(LReport.Field('failure').AsText, 256);
          end;
        end;
        LStatus := IntToStr(LBehavior.Routes.Count) + ' routes / ' + IntToStr(LCount) +
          ' active. ' + LStatus;
      end;
      LNew.Add(NewNyxLabel(PartID('status')).WithText(LStatus));
      LRows := NewNyxColumn(PartID('port-list'));
      LRows.Configure.Visible(FShowPorts).Done;
      LPorts := NyxToolControlPorts(FSelectedItem, LNode, LContext);
      LTotal := 0;
      LCount := 0;
      for LIndex := 0 to LPorts.Count - 1 do
      begin

        if not LPorts.Matches(LIndex, FQuery) then
        begin
          Continue;
        end;

        if (LTotal >= FPortOffset) and (LCount < 12) then
        begin
          LPort := LPorts.Port(LIndex);
          LRow := NewNyxCard(PartID('port-' + IntToStr(LCount)));
          LLabel := 'Output';

          if LPort.Direction = nfdInput then
          begin
            LLabel := 'Input';
          end;
          LRow.Add(NewNyxBadge(PartID('type-' + IntToStr(LCount))).WithText(
            LLabel + ' / ' + NyxFlowTypeName(LPort.ValueType)));
          LRow.Add(NewNyxLabel(PartID('name-' + IntToStr(LCount))).WithText(LPort.Title));
          LRow.Add(NewNyxLabel(PartID('meaning-' + IntToStr(LCount))).WithText(LPort.Description));
          LRows.Add(LRow);
          Inc(LCount);
        end;
        Inc(LTotal);
      end;
      LNew.Add(NewNyxInput(PartID('query')).WithText('Find a port'));
      LNew.Node.Find(PartID('query')).Configure.Value(FQuery).Done;
      LNew.Add(NewNyxButton(PartID('search')).WithText('Search ports'));
      LNew.Add(LRows);
      LActions := NewNyxRow(PartID('pages'));
      LActions.Add(NewNyxButton(PartID('previous')).WithText('Previous'));
      LActions.Node.Find(PartID('previous')).Configure.Enabled(FPortOffset > 0).Done;
      LActions.Add(NewNyxButton(PartID('next')).WithText('Next'));
      LActions.Node.Find(PartID('next')).Configure.Enabled(FPortOffset + 12 < LTotal).Done;
      LNew.Add(LActions);
      LNew.Add(NewNyxLabel(PartID('count')).WithText(
        IntToStr(LCount) + ' shown / ' + IntToStr(LTotal) + ' matching ports'));
    end;

    { Replace only the inspector's private root; all source templates/defaults
      and mounted controls remain independent. Retained old forms remain valid. }

    PublishForm(LNew);
  finally
    if LRuntimeRoot <> FEffectiveRoot then
    begin
      ReleaseNyxNode(LRuntimeRoot);
    end;
    if LContext <> FEffectiveContext then
    begin
      LContext.Free;
    end;
    FUpdating := False;
  end;
end;

function TInspector.Handle(ANode: TNyxNode; const AEvent: TNyxEventInfo): Boolean;
var
  LLease: INyxToolCanvasInspector;
  LIndex: Integer;
  LValue: TNyxText;
  LBehavior: INyxViewBehavior;
begin
  RequireConnected;
  LLease := Self;
  Result := False;

  if ANode = nil then
  begin
    Exit;
  end;

  if (AEvent.Trigger = ntClick) and
    ((ANode.ID = PartID('tab-ports')) or (ANode.ID = PartID('tab-connections'))) then
  begin
    ShowConnections(ANode.ID = PartID('tab-connections'));
    Exit(True);
  end;

  if (FConnections <> nil) and FConnections.Handle(ANode, AEvent) then
  begin
    Rebuild;
    Exit(True);
  end;
  LValue := ANode.Prop(NyxAttributeName(atValue));

  if (ANode.ID = PartID('item')) and (AEvent.Trigger = ntChange) then
  begin
    for LIndex := 0 to High(FItems) do
    begin

      if FItemLabels[LIndex] = LValue then
      begin
        FSelectedItem := FItems[LIndex];
        FSelectedControl := NyxControl('');
        FPortOffset := 0;
        Result := True;
        Break;
      end;
    end;
  end
  else if (ANode.ID = PartID('control')) and (AEvent.Trigger = ntChange) then
  begin
    for LIndex := 0 to High(FControls) do
    begin

      if FControlLabels[LIndex] = LValue then
      begin
        FSelectedControl := FControls[LIndex];
        FPortOffset := 0;
        Result := True;
        Break;
      end;
    end;
  end
  else if AEvent.Trigger = ntChange then
  begin

    if (ANode.ID = PartID('graphs')) and AEvent.HasValue and
      (AEvent.ValueKind = nskBoolean) then
    begin
      LBehavior := CurrentBehavior;

      if LBehavior <> nil then
      begin
        LBehavior.EnableGraphs(AEvent.Value.AsBoolean);
      end;
      Result := True;
    end
    else if (ANode.ID = PartID('ports')) and AEvent.HasValue and
      (AEvent.ValueKind = nskBoolean) then
    begin
      FShowPorts := AEvent.Value.AsBoolean;
      Result := True;
    end;
  end
  else if AEvent.Trigger = ntClick then
  begin
    Result := True;

    if ANode.ID = PartID('previous') then
    begin
      Dec(FPortOffset, 12);

      if FPortOffset < 0 then
      begin
        FPortOffset := 0;
      end;
    end
    else if ANode.ID = PartID('next') then
    begin
      Inc(FPortOffset, 12);
    end
    else if ANode.ID = PartID('search') then
    begin
      FQuery := ANode.Parent.Find(PartID('query')).Prop(NyxAttributeName(atValue));
      FPortOffset := 0;
    end
    else if ANode.ID = PartID('cancel') then
    begin
      LBehavior := CurrentBehavior;

      if LBehavior <> nil then
      begin
        for LIndex := 0 to LBehavior.Routes.Count - 1 do
        begin
          LBehavior.Routes.Cancel(LIndex);
        end;
      end;
    end
    else if ANode.ID <> PartID('refresh') then
    begin
      Result := False;
    end;
  end;

  if Result then
  begin
    Rebuild;
  end;
end;

procedure TInspector.SelectControl(const AItem: TNyxCanvasItemRef;
  const AControl: TNyxControlRef);
var
  LContent: INyxNode;
  LContext: TNyxDocument;
  LAuthored: TNyxNode;
  LRuntime: TNyxNode;
begin
  RequireConnected;
  LContent := FRuntime.Scene.Control(AItem);
  LContext := nil;
  LRuntime := nil;
  try
    if (FEffectiveRoot <> nil) and (FEffectiveItem.Name = AItem.Name) and
      (FEffectiveRevision = FRuntime.Scene.Revision) and
      (FEffectiveContent = TNyxCodec.NodeSnapshot(LContent.Node).ToJSON) then
    begin
      LRuntime := FEffectiveRoot;
    end
    else
    begin
      LContext := NewNyxCanvasRenderContext(FSource, LContent.Node, LAuthored);
      LRuntime := RealizeNyxView(LContext, LAuthored);
    end;

    if LRuntime.Find(AControl.ID) = nil then
    begin
      raise ENyxModel.Create('Tool Canvas selection requires an exact effective control');
    end;
  finally
    if LRuntime <> FEffectiveRoot then
    begin
      ReleaseNyxNode(LRuntime);
    end;
    LContext.Free;
  end;
  FSelectedItem := AItem;
  FSelectedControl := AControl;
  FPortOffset := 0;
  Rebuild;
end;

function NewNyxToolCanvasInspector(const AID: TNyxText; ADocument: TNyxDocument;
  const AView: INyxCanvasWorkspaceView): INyxToolCanvasInspector;
begin
  Result := TInspector.Create(AID, ADocument, AView);
end;

function TInspector.ConnectionEditor: INyxToolConnectionEditor;
begin
  RequireConnected;
  Result := FConnections;
end;

procedure TInspector.AddTabs(const AParent: INyxCard);
var
  LTabs: INyxRow;
  LButton: INyxButton;
begin
  LTabs := NewNyxRow(PartID('tabs'));
  LButton := NewNyxButton(PartID('tab-ports')).WithText('Control ports');

  if FTab = titPorts then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  LButton := NewNyxButton(PartID('tab-connections')).WithText('Connections');
  LButton.Configure.Enabled(FConnections <> nil).Done;

  if FTab = titConnections then
  begin
    LButton.Configure.Variant(nvPrimary).Done;
  end;
  LTabs.Add(LButton);
  AParent.Add(LTabs);
end;

procedure TInspector.PublishForm(const ARoot: INyxCard);
begin

  if FRoot <> nil then
  begin
    FDocument.RemoveRoot(NyxReusableRoot(FID));
  end;
  FDocument.AddComponent(ARoot);
  FRoot := ARoot;
end;

procedure TInspector.ShowConnections(AVisible: Boolean);
begin
  RequireConnected;

  if AVisible and (FConnections = nil) then
  begin
    raise ENyxModel.Create('This canvas supports port inspection without connection editing');
  end;
  FTab := titPorts;

  if AVisible then
  begin
    FTab := titConnections;
  end;
  Rebuild;
end;

function TInspector.HandleGesture(ANode: TNyxNode; const AEvent: TNyxEventInfo;
  const AResponse: INyxGestureResponse): Boolean;
var
  LLease: INyxToolCanvasInspector;
begin
  RequireConnected;
  LLease := Self;
  Result := (FConnections <> nil) and FConnections.Handle(ANode, AEvent, AResponse);

  if Result then
  begin
    Rebuild;
  end;
end;

end.
