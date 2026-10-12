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
  nyx.collections.bindings, nyx.collections.view.types;

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
  The source document and its authored defaults are never borrowed or mutated. }
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
  TCollectionPager = class(TInterfacedObject, INyxCollectionPager)
  private
    FControl: INyxPagination;
    FView: INyxCollectionView;
    FPages: INyxCollectionPages;
    FScheduler: INyxScheduler;
    FToken: INyxCollectionViewSubscription;
    FRefresh: TNyxPagerRefresh;
    FConnected: Boolean;
    FPreviousEnabled: Boolean;
    FNextEnabled: Boolean;
    FPageEnabled: Boolean;
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

  if (Result = nil) or not NyxCollectionPages(Result).Paging.Defined then
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

  if not FPages.Paging.Defined then
  begin
    raise ENyxCollection.Create('Configure collection paging before attaching a pager');
  end;
  { Validate every specialized part before retaining an observer or changing
    any value. A derived recipe with incompatible required parts fails intact. }
  FPreviousEnabled := AControl.PreviousButton.Node.Prop(NyxAttributeName(atEnabled), 'true') <> 'false';
  FNextEnabled := AControl.NextButton.Node.Prop(NyxAttributeName(atEnabled), 'true') <> 'false';
  FPageEnabled := AControl.PageSpin.Node.Prop(NyxAttributeName(atEnabled), 'true') <> 'false';

  if AControl.TotalLabel = nil then
  begin
    raise ENyxCollection.Create('Pagination requires its specialized total label');
  end;
  FControl := AControl;
  FView := AView;
  FScheduler := AScheduler;
  FRefresh := ARefresh;
end;

procedure TCollectionPager.Activate;
begin
  FConnected := True;
  FToken := FView.Subscribe(Changed);
  Present(False);
end;

destructor TCollectionPager.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCollectionPager.Disconnect;
begin
  FConnected := False;
  FRefresh := nil;

  if FToken <> nil then
  begin
    FToken.Disconnect;
    FToken := nil;
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
    .Value(LWindow.Page).Enabled(FPageEnabled and (LWindow.Pages > 0)).Done;
  FControl.PreviousButton.Configure.Enabled(FPreviousEnabled and LWindow.CanPrevious).Done;
  FControl.NextButton.Configure.Enabled(FNextEnabled and LWindow.CanNext).Done;

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
