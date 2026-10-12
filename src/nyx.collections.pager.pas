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
unit nyx.collections.pager;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.behavior,
  nyx.scheduler, nyx.collections, nyx.collections.view, nyx.collections.paging,
  nyx.collections.bindings, nyx.collections.view.types, nyx.interaction.policy;

type
  { Borrowed adapter receiver, valid only while its managed attachment remains
    connected. The adapter disconnects before releasing controls or its root. }
  TNyxPagerRefresh = procedure of object;

  { Portable attachment to the actual specialized pagination contract. It owns
    the retained control/view and one weak view-subscription token, with no back
    reference to a renderer or document. Default commands run before application
    callbacks; event-stream scheduling never changes the navigation semantics. }
  INyxCollectionPager = interface(IInterface)
    ['{38D93F89-B9B4-452C-900E-EF359DB96F23}']
    procedure Disconnect;
    procedure Refresh;
    { Accept only Page/Previous/Next's ordinary admitted change/click. Extra
      customized parts remain application-owned. Disconnected calls refuse. }
    function ApplyEvent(const AEvent: TNyxEventInfo): Boolean;
    function GetConnected: Boolean;
    property Connected: Boolean read GetConnected;
  end;

{ The supplied control must be a realized runtime copy. Named parts can be
  independently styled/extended; missing or retyped required parts refuse.
  The source document and its authored defaults are never borrowed or mutated.
  Runtime paging can be reset to all results while retaining this attachment;
  authored PageView admission still requires an explicitly paged target. }
function NewNyxCollectionPager(const AControl: INyxPagination;
  const AView: INyxCollectionView; const AScheduler: INyxScheduler;
  ARefresh: TNyxPagerRefresh): INyxCollectionPager;

{ Resolve a portable design identity inside this realized page and exact reusable
  instance. Another instance's data view cannot satisfy a missing local target.
  Missing, ambiguous, unpaged and tree targets refuse before target publication. }
function NyxCollectionPagerView(ARoot, APager: TNyxNode;
  const AViews: INyxCollectionBindings): INyxCollectionView;
{ Borrowed exact target, for shared candidate admission before adapter creation. }
function NyxCollectionPagerTarget(ARoot, APager: TNyxNode): TNyxNode;

implementation

type
  TPagerPartRole = (pprPrevious, pprNext, pprPage);
  { A private marker identifies only this component's constraints on a freshly
    reconfigured runtime copy. Other library/application policies stay intact. }
  INyxPagerEnabledPolicy = interface(INyxRuntimeEnabledPolicy)
    ['{071B03DE-74A2-47D6-9237-A91FE025C974}']
    procedure Disconnect;
  end;
  TPagerEnabledPolicy = class(TInterfacedObject, INyxRuntimeEnabledPolicy,
    INyxPagerEnabledPolicy)
  private
    FPages: INyxCollectionPages;
    FScheduler: INyxScheduler;
    FRole: TPagerPartRole;
  public
    constructor Create(const APages: INyxCollectionPages;
      const AScheduler: INyxScheduler; ARole: TPagerPartRole);
    function GetAllowed: Boolean;
    procedure Disconnect;
  end;

  TCollectionPager = class(TInterfacedObject, INyxCollectionPager)
  private
    FControl: INyxPagination;
    FView: INyxCollectionView;
    FPages: INyxCollectionPages;
    FScheduler: INyxScheduler;
    FToken: INyxCollectionViewSubscription;
    FRefresh: TNyxPagerRefresh;
    FConnected: Boolean;
    { pas2js does not support static arrays of COM interfaces. The owned
      three-entry vector is initialized before activation and never shared. }
    FPolicies: array of INyxPagerEnabledPolicy;
    function PartNode(ARole: TPagerPartRole): TNyxNode;
    procedure AttachPolicies;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure Present(ANotify: Boolean);
  public
    constructor Create(const AControl: INyxPagination;
      const AView: INyxCollectionView; const AScheduler: INyxScheduler;
      ARefresh: TNyxPagerRefresh);
    destructor Destroy; override;
    procedure Activate;
    procedure Disconnect;
    procedure Refresh;
    function ApplyEvent(const AEvent: TNyxEventInfo): Boolean;
    function GetConnected: Boolean;
  end;

constructor TPagerEnabledPolicy.Create(const APages: INyxCollectionPages;
  const AScheduler: INyxScheduler; ARole: TPagerPartRole);
begin
  inherited Create;
  FPages := APages;
  FScheduler := AScheduler;
  FRole := ARole;
end;

function TPagerEnabledPolicy.GetAllowed: Boolean;
var
  LWindow: TNyxCollectionPageWindow;
begin
  Result := True;

  if FPages = nil then
  begin
    { A retained command/projection can outlive the attachment. Retirement
      revokes its restriction, and never calls a disposed UI owner. }
    Exit;
  end;
  FScheduler.RequireUI;
  LWindow := FPages.Window;
  case FRole of
    pprPrevious:
      begin
        Result := LWindow.CanPrevious;
      end;
    pprNext:
      begin
        Result := LWindow.CanNext;
      end;
    pprPage:
      begin
        Result := LWindow.Pages > 0;
      end;
  end;
end;

procedure TPagerEnabledPolicy.Disconnect;
begin
  FPages := nil;
  FScheduler := nil;
end;

function NyxCollectionPagerTarget(ARoot, APager: TNyxNode): TNyxNode;
var
  LTarget: TNyxText;
  LPrefix: TNyxText;
  LIndex: Integer;

  procedure Visit(ANode: TNyxNode);
  var
    LChild: Integer;
  begin

    if ANode.ID = LTarget then
    begin

      if Result <> nil then
      begin
        raise ENyxCollection.Create('Pagination collection-view reference is ambiguous');
      end;
      Result := ANode;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild]);
    end;
  end;
begin
  Result := nil;

  if (ARoot = nil) or (APager = nil) or not APager.IsRealized or
    (APager.Kind <> NyxKindName(nkPagination)) then
  begin
    raise ENyxCollection.Create('A pager requires its realized page and collection views');
  end;
  { Compound scopes belong to their own state/parts, so a pager's collection
    sibling need not share its compound scope. Resolve the exact qualified
    design ID under the same reusable expansion prefix instead. Escaped source
    IDs contain no slash; separators belong exclusively to instance prefixes. }
  LPrefix := '';
  for LIndex := Length(APager.ID) downto 1 do
  begin

    if APager.ID[LIndex] = '/' then
    begin
      LPrefix := Copy(APager.ID, 1, LIndex - 1);
      Break;
    end;
  end;
  LTarget := NyxQualifiedID(LPrefix, APager.Prop(NyxAttributeName(atPageView)));
  Visit(ARoot);

  if (Result = nil) or not Result.HasCollectionView or
    not Result.CollectionView.Defined or not Result.CollectionView.PagingPolicy.Defined or
    not (NyxCollectionProjectionForNode(Result) in [cpList, cpTable]) then
  begin
    raise ENyxCollection.Create('Pagination requires an explicitly paged local collection view');
  end;
end;

function NyxCollectionPagerView(ARoot, APager: TNyxNode;
  const AViews: INyxCollectionBindings): INyxCollectionView;
var
  LTarget: TNyxNode;
begin
  LTarget := NyxCollectionPagerTarget(ARoot, APager);

  if AViews = nil then
  begin
    raise ENyxCollection.Create('Pagination requires its retained collection-view set');
  end;
  Result := AViews.ViewFor(LTarget.ID);

  if Result = nil then
  begin
    raise ENyxCollection.Create('Pagination requires an explicitly paged local collection view');
  end;
end;

constructor TCollectionPager.Create(const AControl: INyxPagination;
  const AView: INyxCollectionView; const AScheduler: INyxScheduler;
  ARefresh: TNyxPagerRefresh);
begin
  inherited Create;

  if (AControl = nil) or not AControl.Node.IsRealized or (AView = nil) or
    (AScheduler = nil) or not Assigned(ARefresh) then
  begin
    raise ENyxCollection.Create('A pager attachment requires realized controls, a view and UI refresh');
  end;
  AScheduler.RequireUI;
  FPages := NyxCollectionPages(AView);

  { Validate every specialized part before retaining an observer or changing
    any value. A derived recipe with incompatible required parts fails intact. }

  if (AControl.PreviousButton = nil) or (AControl.NextButton = nil) or
    (AControl.PageSpin = nil) then
  begin
    raise ENyxCollection.Create('Pagination requires specialized navigation and page parts');
  end;

  if AControl.TotalLabel = nil then
  begin
    raise ENyxCollection.Create('Pagination requires its specialized total label');
  end;
  FControl := AControl;
  FView := AView;
  FScheduler := AScheduler;
  FRefresh := ARefresh;
end;

function TCollectionPager.PartNode(ARole: TPagerPartRole): TNyxNode;
begin
  case ARole of
    pprPrevious:
      begin
        Result := FControl.PreviousButton.Node;
      end;
    pprNext:
      begin
        Result := FControl.NextButton.Node;
      end;
    pprPage:
      begin
        Result := FControl.PageSpin.Node;
      end;
  end;
end;

procedure TCollectionPager.AttachPolicies;
var
  LRole: TPagerPartRole;
  LNode: TNyxNode;
  LIndex: Integer;
  LPolicy: INyxRuntimeEnabledPolicy;
begin
  SetLength(FPolicies, Ord(High(TPagerPartRole)) + 1);
  for LRole := Low(TPagerPartRole) to High(TPagerPartRole) do
  begin
    LNode := PartNode(LRole);
    for LIndex := LNode.EnabledPolicyCount - 1 downto 0 do
    begin
      LPolicy := LNode.EnabledPolicyAt(LIndex);

      if Supports(LPolicy, INyxPagerEnabledPolicy) then
      begin
        { Independent runtime copies may retain the former attachment's policy.
          Remove that reference here without cancelling the accepted owner. }
        LNode.DetachEnabledPolicy(LPolicy);
      end;
    end;
    FPolicies[Ord(LRole)] := TPagerEnabledPolicy.Create(FPages, FScheduler, LRole);
    LNode.AttachEnabledPolicy(FPolicies[Ord(LRole)]);
  end;
end;

procedure TCollectionPager.Activate;
begin
  FConnected := True;
  AttachPolicies;
  FToken := FView.Subscribe(Changed);
  Present(False);
end;

destructor TCollectionPager.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCollectionPager.Disconnect;
var
  LIndex: Integer;
begin
  FConnected := False;
  FRefresh := nil;

  if FToken <> nil then
  begin
    FToken.Disconnect;
    FToken := nil;
  end;
  for LIndex := 0 to High(FPolicies) do
  begin

    if FPolicies[LIndex] <> nil then
    begin
      PartNode(TPagerPartRole(LIndex)).DetachEnabledPolicy(FPolicies[LIndex]);
      FPolicies[LIndex].Disconnect;
      FPolicies[LIndex] := nil;
    end;
  end;
  FPages := nil;
  FView := nil;
  FControl := nil;
  FScheduler := nil;
end;

function TCollectionPager.GetConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TCollectionPager.Present(ANotify: Boolean);
var
  LWindow: TNyxCollectionPageWindow;
  LRefresh: TNyxPagerRefresh;
  LKeepAlive: INyxCollectionPager;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Pagination attachment is disconnected');
  end;
  FScheduler.RequireUI;
  LKeepAlive := Self as INyxCollectionPager;
  LWindow := FPages.Window;
  FControl.PageSpin.Configure.Minimum(1).Maximum(Max(1, LWindow.Pages))
    .Value(LWindow.Page).Done;
  { Availability is read through independent runtime constraints. Do not write
    Enabled over caller configuration, viewport overrides or live bindings. }

  if LWindow.Pages = 0 then
  begin
    FControl.TotalLabel.WithText('No results');
  end
  else
  begin
    FControl.TotalLabel.WithText('of ' + IntToStr(LWindow.Pages));
  end;
  LRefresh := FRefresh;

  if ANotify and Assigned(LRefresh) then
  begin
    { Last borrowed call: host refresh can navigate and disconnect this owner. }
    LRefresh;
  end;
end;

procedure TCollectionPager.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin

  if FConnected then
  begin
    Present(True);
  end;
end;

procedure TCollectionPager.Refresh;
begin
  Present(True);
end;

function TCollectionPager.ApplyEvent(const AEvent: TNyxEventInfo): Boolean;
var
  LKeepAlive: INyxCollectionPager;
  LPage: Integer;
begin

  if not FConnected then
  begin
    raise ENyxCollection.Create('Pagination attachment is disconnected');
  end;
  FScheduler.RequireUI;
  Result := (AEvent.SourceID = FControl.ID) and
    (((AEvent.Trigger = ntChange) and (AEvent.OriginID = FControl.PageSpin.ID)) or
    ((AEvent.Trigger = ntClick) and
      ((AEvent.OriginID = FControl.PreviousButton.ID) or
      (AEvent.OriginID = FControl.NextButton.ID))));

  if not Result then
  begin
    Exit;
  end;
  LKeepAlive := Self as INyxCollectionPager;
  LPage := FControl.PageSpin.Value;
  FPages.GoToPage(LPage);

  if FConnected then
  begin
    { Equal/clamped page intents may not notify, but still restore the exact
      accepted controls. Observer retirement revokes the borrowed refresh. }
    Present(True);
  end;
end;

function NewNyxCollectionPager(const AControl: INyxPagination;
  const AView: INyxCollectionView; const AScheduler: INyxScheduler;
  ARefresh: TNyxPagerRefresh): INyxCollectionPager;
var
  LPager: TCollectionPager;
begin
  LPager := TCollectionPager.Create(AControl, AView, AScheduler, ARefresh);
  Result := LPager;
  LPager.Activate;
end;

end.
