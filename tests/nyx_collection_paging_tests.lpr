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
program nyx_collection_paging_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.data, nyx.state, nyx.collections,
  nyx.collections.paging, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query, nyx.collections.selection, nyx.collections.columns,
  nyx.model, nyx.types, nyx.codec, nyx.codegen, nyx.source, nyx.controls,
  nyx.composition, nyx.collections.bindings, nyx.collections.pager, nyx.interaction.policy,
  nyx.responsive
  {$ifdef PAS2JS}, Web{$endif};

type
  { An independent extension policy proves the portable model composes arbitrary
    managed constraints without acquiring callbacks or a tree backreference. }
  TExtensionPolicy = class(TInterfacedObject, INyxRuntimeEnabledPolicy)
    Allowed: Boolean;
    function GetAllowed: Boolean;
  end;
  TPageObserver = class
    Calls: Integer;
    Reenter: Boolean;
    ReentryRefused: Boolean;
    procedure Changed(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function TExtensionPolicy.GetAllowed: Boolean;
begin
  Result := Allowed;
end;

procedure CheckRuntimePolicies;
var
  LSource: TNyxNode;
  LCopy: TNyxNode;
  LDescriptor: TNyxNode;
  LObject: TExtensionPolicy;
  LPolicy: INyxRuntimeEnabledPolicy;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LSource := TNyxNode.CreateRealized('button', 'command', 'command', 'command');
  LCopy := nil;
  LDescriptor := nil;
  LObject := TExtensionPolicy.Create;
  LPolicy := LObject;
  try
    LSource.Configure.Enabled(True).Done;
    LBefore := LSource.Props.Text;
    LSource.AttachEnabledPolicy(LPolicy);
    LSource.AttachEnabledPolicy(LPolicy);
    Check((LSource.EnabledPolicyCount = 1) and (LSource.Prop('enabled') = 'false') and
      (LSource.StoredProp('enabled') = 'true') and (LSource.Props.Text = LBefore),
      'runtime constraint restricts interaction without authoring its Boolean default');
    LObject.Allowed := True;
    Check(LSource.Prop('enabled') = 'true', 'an allowing policy exposes the actual caller value');
    LSource.Configure.WhenViewport(TNyxViewportWidth.Below(420)).Enabled(False).Done;
    LSource.ApplyViewport(390, npfBrowser);
    Check(LSource.Prop('enabled') = 'false', 'an allowing policy preserves the current compact viewport restriction');
    LSource.ApplyViewport(800, npfBrowser);
    Check(LSource.Prop('enabled') = 'true', 'a larger viewport exposes the current caller setting');
    LSource.Configure.Enabled(False).Done;
    Check(LSource.Prop('enabled') = 'false', 'an allowing policy never overrides caller disablement');
    LSource.Configure.Enabled(True).Done;
    LObject.Allowed := False;
    LSource.ApplyViewport(390, npfNativeLCL);
    LSource.ApplyViewport(800, npfNativeLCL);
    Check(LSource.Prop('enabled') = 'false', 'native viewport reevaluation cannot lift a runtime restriction');
    LDescriptor := LSource.Clone;
    Check((LDescriptor.EnabledPolicyCount = 0) and (LDescriptor.Prop('enabled') = 'true'),
      'ordinary descriptor clones exclude runtime capabilities');
    LCopy := LSource.CloneRuntimeProjection;
    Check((LCopy.EnabledPolicyCount = 1) and (LCopy.Prop('enabled') = 'false'),
      'runtime command projections retain the live interaction restriction');
    LSource.DetachEnabledPolicy(LPolicy);
    Check((LSource.EnabledPolicyCount = 0) and (LCopy.EnabledPolicyCount = 1),
      'detaching a policy does not resize a shared projection array');
    LPolicy := nil;
    FreeAndNil(LSource);
    Check(LCopy.Prop('enabled') = 'false', 'retained projection owns its policy after the source retires');
    LSource := TNyxNode.Create(nkButton, 'authored-command');
    LRejected := False;
    try
      LSource.AttachEnabledPolicy(LCopy.EnabledPolicyAt(0));
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'authored descriptors refuse runtime policy attachment');
  finally
    LPolicy := nil;
    LSource.Free;
    LCopy.Free;
    LDescriptor.Free;
  end;
end;

procedure TPageObserver.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);

  if Reenter then
  begin
    try
      NyxCollectionPages(AView).GoToPage(1);
    except
      on ENyxCollection do
      begin
        ReentryRefused := True;
      end;
    end;
  end;
end;

procedure Run;
var
  LPolicy: TNyxCollectionPaging;
  LWindow: TNyxCollectionPageWindow;
  LTotal: Integer;
  LSize: Integer;
  LRequest: Integer;
  LExpectedPages: Double;
  LExpectedPage: Double;
  LKey: TNyxCollectionRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LOther: INyxCollectionView;
  LPages: INyxCollectionPages;
  LSnapshot: INyxCollectionSnapshot;
  LToken: INyxCollectionViewSubscription;
  LObserver: TPageObserver;
  LRejected: Boolean;
  LBefore: TNyxText;
  LCalls: Integer;
  LSpec: TNyxCollectionViewSpec;
  LNextSpec: TNyxCollectionViewSpec;
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LPage: TNyxNode;
  LTable: TNyxNode;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LPager: INyxPagination;
  LDefinition: TNyxNode;
  LRuntime: TNyxNode;
  LContext: INyxCollectionContext;
  LBindings: INyxCollectionBindings;
  LRecomposed: INyxCollectionBindings;
  LRuntimePager: TNyxNode;
begin
  LPolicy := NyxCollectionPaging.PageSize(2).InitialPage(2);
  Check(TNyxCollectionPaging.FromData(LPolicy.ToData).ItemsPerPage = 2,
    'typed authored defaults round-trip independently');
  LBefore := LPolicy.ToData.ToJSON;
  LRejected := False;
  try
    LPolicy.PageSize(0);
  except
    on ENyxCollection do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPolicy.ToData.ToJSON = LBefore), 'invalid fluent size preserves the original');
  LSpec := NyxCollectionView(NyxCollection('tasks')).Column(NyxIntegerField('priority'), 'Priority')
    .ColumnSizing(NyxColumnSizing.Fixed(96));
  LBefore := LSpec.ToData.ToJSON;
  LNextSpec := LSpec.Paged(LPolicy);
  Check((LNextSpec.ToData.Field('version').AsInteger = 6) and
    (TNyxCollectionViewSpec.FromData(LNextSpec.ToData).PagingPolicy.FirstPage = 2),
    'authored pages select strict version-six meaning');
  Check((LSpec.ToData.ToJSON = LBefore) and
    (LNextSpec.UseDefaultPaging.ToData.ToJSON = LBefore),
    'copied paging/reset retain exact version-five columns');
  for LTotal := 0 to 12 do
  begin
    for LSize := 1 to 8 do
    begin
      for LRequest := 1 to 18 do
      begin
        LWindow := NyxCollectionPageWindow(LTotal, LSize, LRequest);
        LExpectedPages := Ceil(LTotal / LSize);
        LExpectedPage := Min(LRequest, Max(1, LExpectedPages));
        Check((LWindow.Pages = LExpectedPages) and (LWindow.Page = LExpectedPage) and
          (LWindow.Count >= 0) and (LWindow.Count <= LSize) and
          (LWindow.First + LWindow.Count <= LTotal), 'independent small-window oracle');
      end;
    end;
  end;
  LWindow := NyxCollectionPageWindow(High(Integer), 2, High(Integer));
  Check((LWindow.Pages = 1073741824) and (LWindow.First = 2147483646) and
    (LWindow.Count = 1), 'ceiling/page multiplication stays safe at High(Integer)');
  LWindow := NyxCollectionPageWindow(0, High(Integer), High(Integer));
  Check((LWindow.Page = 1) and (LWindow.Pages = 0) and not LWindow.CanNext and
    not LWindow.CanPrevious, 'empty membership has no navigation authority');
  LKey := NyxCollection('tasks');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Integer(NyxIntegerField('priority'), 1), [
    NyxCollectionItem(NyxItem(LKey, 'one')).WithValue(NyxIntegerField('priority'), 1),
    NyxCollectionItem(NyxItem(LKey, 'two')).WithValue(NyxIntegerField('priority'), 2),
    NyxCollectionItem(NyxItem(LKey, 'three')).WithValue(NyxIntegerField('priority'), 3),
    NyxCollectionItem(NyxItem(LKey, 'four')).WithValue(NyxIntegerField('priority'), 4),
    NyxCollectionItem(NyxItem(LKey, 'five')).WithValue(NyxIntegerField('priority'), 5)]);
  LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable).Selection(nsmMultiple), cpTable);
  LOther := NewNyxCollectionView(LStore, LView.Spec, cpList);
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LDocument.Collections.Define(LStore.Snapshot);
    LPage := TNyxNode.Create(nkPage, 'home');
    LDocument.AddPage(LPage);
    LTable := TNyxNode.Create(nkTable, 'tasks-table');
    LPage.Add(LTable);
    LTable.SetCollectionView(LNextSpec);
    LPager := NewNyxPagination('tasks-pager');
    LPage.Add(LPager.Node);
    LPager.Configure.PageView(NyxControl('tasks-table')).Done;
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('.Paged(NyxCollectionPaging.PageSize(2).InitialPage(2))', LSource) > 0) and
      (Pos('nyx.collections.paging,', LSource) > 0), 'crafted source carries typed page defaults');
    Check((Pos('PageView(NyxControl(''tasks-table''))', LSource) > 0) and
      (Pos('INyxPagination', LSource) > 0), 'specialized source carries the exact typed pager target');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore, 'source admission reconstructs exact paged binding');
    LCandidate.Free;
    LCandidate := nil;
    LOther := NewNyxCollectionView(LStore, LNextSpec, cpList);
    Check((LOther.Snapshot.Count = 2) and (NyxCollectionPages(LOther).Window.Page = 2),
      'the authored specification initializes ordinary runtime paging');
    LOther := NewNyxCollectionView(LStore, LView.Spec, cpList);
    LDefinition := TNyxNode.Create(nkColumn, 'task-component');
    LDocument.AddComponent(LDefinition);
    LDefinition.Add(TNyxNode.Create(nkTable, 'component-table'));
    LDefinition.Children[0].SetCollectionView(LNextSpec.Scoped(csInstance));
    LPager := NewNyxPagination('component-pager');
    LDefinition.Add(LPager.Node);
    LPager.Configure.PageView(NyxControl('component-table')).Done;
    LPage.Add(TNyxNode.Create(nkComponent, 'instance-a').Configure
      .Component(NyxComponent('task-component')).Done);
    LPage.Add(TNyxNode.Create(nkComponent, 'instance-b').Configure
      .Component(NyxComponent('task-component')).Done);
    LRuntime := RealizeNyxView(LDocument, LPage);
    try
      LContext := NewNyxCollectionContext(LDocument.Collections);
      LBindings := NewNyxCollectionBindings(LRuntime, LContext);
      LRuntimePager := LRuntime.Find(NyxQualifiedID('instance-a', 'component-pager'));
      LOther := NyxCollectionPagerView(LRuntime, LRuntimePager, LBindings);
      Check((LOther.Store <> LBindings.ViewFor(NyxQualifiedID('instance-b', 'component-table')).Store) and
        (NyxCollectionPagerTarget(LRuntime, LRuntimePager).ID =
        NyxQualifiedID('instance-a', 'component-table')), 'reusable pager resolves its exact independent instance');
      NyxCollectionPages(LOther).NextPage;
      Check((NyxCollectionPages(LOther).Window.Page = 3) and
        (NyxCollectionPages(LBindings.ViewFor(NyxQualifiedID('instance-b', 'component-table'))).Window.Page = 2),
        'independent reusable views keep their live page');
      LRecomposed := LBindings.Recompose(LRuntime);
      Check(NyxCollectionPages(LRecomposed.ViewFor(NyxQualifiedID('instance-a', 'component-table'))).Window.Page = 3,
        'independent recomposition retains the live page');
      LRuntimePager.Configure.PageView(NyxControl('tasks-table')).Done;
      LRejected := False;
      try
        NyxCollectionPagerTarget(LRuntime, LRuntimePager);
      except
        on ENyxCollection do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'a missing local target cannot borrow the outer page data view');
    finally
      LRecomposed := nil;
      LBindings := nil;
      LContext := nil;
      LRuntime.Free;
    end;
    LOther := NewNyxCollectionView(LStore, LView.Spec, cpList);
    LRejected := False;
    try
      ValidateNyxCollectionViewSnapshot(LStore.Snapshot, LNextSpec, cpTree);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'flat paging refuses trees before target publication');
    LRejected := False;
    try
      NyxPageSnapshot(LStore.Snapshot, Default(TNyxCollectionPageWindow));
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unconstructed geometry cannot masquerade as an empty admitted page');
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
    LPager := nil;
  end;
  LObserver := TPageObserver.Create;
  LToken := LView.Subscribe(LObserver.Changed);
  try
    LPages := NyxCollectionPages(LView);
    LPages.ConfigurePaging(LPolicy);
    Check((LPages.Window.Pages = 3) and (LPages.Window.Page = 2) and
      (LView.Snapshot.Count = 2) and (LView.Snapshot.ItemAt(0).Ref.ID = 'three'),
      'real runtime view projects its initial page');
    Check((LOther.Snapshot.Count = 5) and (LStore.Snapshot.Count = 5),
      'other views and the complete store remain independently owned');
    Check(LView.Snapshot.DataBytes = LStore.Snapshot.DataBytes, 'a page retains full logical source cost');
    LView.SelectAll;
    Check((LView.Selection.Count = 5) and (LView.Selection.Focus.ID = 'three'),
      'select all retains the complete query result and visible page focus');
    LView.ClearSelection;
    LSnapshot := LView.Snapshot;
    LCalls := LObserver.Calls;
    LPages.GoToPage(2);
    Check((LObserver.Calls = LCalls) and (LView.Snapshot = LSnapshot),
      'same-page intent is a silent identity-preserving no-op');
    LView.Select(NyxItem(LKey, 'three'));
    LPages.NextPage;
    Check((LView.Snapshot.Count = 1) and (LView.Snapshot.ItemAt(0).Ref.ID = 'five') and
      LView.Selection.Contains(NyxItem(LKey, 'three')) and
      (LView.Selection.Focus.ID = 'five'), 'page changes retain hidden membership and move visible focus');
    LObserver.Reenter := True;
    LPages.FirstPage;
    Check(LObserver.ReentryRefused and (LPages.Window.Page = 1),
      'a page observer cannot mutate this view reentrantly');
    LObserver.Reenter := False;
    LView.ConfigureQuery(NyxCollectionQuery.OrderBy(NyxIntegerField('priority'), nsdDescending));
    Check(LView.Snapshot.ItemAt(0).Ref.ID = 'five', 'paging follows stable query ordering');
    LPages.LastPage;
    Check(LView.Snapshot.ItemAt(0).Ref.ID = 'one', 'last page reflects the ordered result');
    LView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(4)));
    Check((LPages.Window.Total = 2) and (LPages.Window.Pages = 1) and
      (LPages.Window.Page = 1), 'query shrink clamps against filtered membership');
    LStore.Remove(NyxItem(LKey, 'five'));
    Check((LPages.Window.Total = 1) and (LView.Snapshot.ItemAt(0).Ref.ID = 'four') and
      (LOther.Snapshot.Count = 4), 'coordinated store admission installs all projected page views');
    LStore.Remove(NyxItem(LKey, 'four'));
    Check((LPages.Window.Total = 0) and (LPages.Window.Pages = 0) and
      (LView.Snapshot.Count = 0), 'removing the final filtered member yields an empty page');
    LView.ConfigureQuery(NyxCollectionQuery);
    LPages.ConfigurePaging(Default(TNyxCollectionPaging));
    Check((LView.Snapshot.Count = 3) and not LPages.Paging.Defined,
      'explicit disable restores complete filtered membership');
  finally
    LToken.Disconnect;
    LToken := nil;
    LObserver.Free;
    LSnapshot := nil;
    LPages := nil;
    LOther := nil;
    LView := nil;
    LStore := nil;
  end;
end;

begin
  try
    CheckRuntimePolicies;
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-collection-paging', 'passed');
    document.body.setAttribute('data-paging-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection paging contract/runtime checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-collection-paging', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
