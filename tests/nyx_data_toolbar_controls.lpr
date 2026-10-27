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
program nyx_data_toolbar_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Forms, Controls, StdCtrls, Grids, nyx.render.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.controls, nyx.model, nyx.codec, nyx.state,
  nyx.data.toolbar, nyx.coordinators, nyx.collections, nyx.collections.query,
  nyx.collections.view, nyx.collections.view.types, nyx.collections.query.session,
  nyx.collections.selection, nyx.publication, nyx.binding.types, nyx.binding.inputs,
  nyx.data.toolbar.query;

type
  { Authored binding cases exercise ordinary input/query publication, never
    private renderer flags or a substitute query engine. Default keeps the
    existing application unchanged; each additional fixture owns its stores. }
  TBindingFixture = (tbfDefault, tbfTwoWay, tbfSearchProjection,
    tbfSortProjection, tbfReadOnly, tbfAliasedState, tbfProjectionAtBaseline);
  {$ifndef PAS2JS}
  { Keep native callback failures observable without hiding behind a modal.
    The fixture receiver outlives all forms and renderers. }
  TFailures = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Independent application consumer; each renderer owns separate live stores
    from the same authored defaults. Destruction retires the renderer first. }
  TJourney = class
  public
    Document: TNyxDocument;
    {$ifdef PAS2JS}
    Renderer: TNyxBrowserRenderer;
    Host: TJSHTMLElement;
    {$else}
    Renderer: TNyxLCLRenderer;
    Host: TForm;
    {$endif}
    constructor Create(AInitialBindings: Boolean = False;
      AFixture: TBindingFixture = tbfDefault);
    destructor Destroy; override;
    function Role(const AToolbar, APart: TNyxText): TNyxText;
    function Toolbar(const AID: TNyxText): INyxDataToolbarBinding;
    procedure TypeSearch(const AID, AText: TNyxText);
    procedure ChooseSort(const AID, AText: TNyxText);
    procedure ClickClear(const AID: TNyxText);
    function InputText(const AID, APart: TNyxText): TNyxText;
    function Rows(const AID: TNyxText): INyxCollectionView;
    function VisibleFirst(const AID: TNyxText): TNyxText;
    function StatusText: TNyxText;
  end;

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GFailures: TFailures;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Message <> '' then
  begin
    raise Exception.Create('Native callback: ' + GFailures.Message);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
  {$ifndef PAS2JS}
  WriteLn('CHECK ', GChecks, ' / ', AReason);
  Flush(Output);
  {$endif}
end;

{$ifndef PAS2JS}
procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin

  if Message = '' then
  begin
    Message := AException.ClassName + ': ' + AException.Message;
  end;
end;
{$endif}

constructor TJourney.Create(AInitialBindings: Boolean; AFixture: TBindingFixture);
var
  LPage: INyxPage;
  LToolbar: INyxDataToolbar;
  LConfigured: INyxDataToolbar;
  LOptions: TNyxDataToolbarOptions;
  LPeople: TNyxCollectionRef;
  LName: TNyxTextFieldRef;
  LSpec: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  LPeople := NyxCollection('people');
  LName := NyxTextField('name');
  Document.Collections.Define(LPeople, NyxCollectionSchema.Text(LName, ''), [
    NyxCollectionItem(NyxItem(LPeople, 'ada')).WithValue(LName, 'Ada'),
    NyxCollectionItem(NyxItem(LPeople, 'bea')).WithValue(LName, 'Bea'),
    NyxCollectionItem(NyxItem(LPeople, 'grace')).WithValue(LName, 'Grace')]);
  LPage := NewNyxPage('home');
  LPage.Configure.Padding(12).Gap(8).Done;
  LOptions := NyxDataToolbarOptions(NyxControl('people-list'), LName)
    .Sort(NyxToolbarSort('Name descending'), NyxSort(LName, nsdDescending));
  LToolbar := NewNyxDataToolbar('list-tools');
  LToolbar.CreateButton.WithText('Invite a person');
  LBefore := TNyxCodec.Encode(Document);
  LConfigured := ConfigureNyxDataToolbar(LToolbar, LOptions);
  Check((LToolbar.Count = 4) and (LConfigured.Count = 5),
    'Clear is opt-in and the original static recipe remains unchanged');
  Check(LConfigured.CreateButton.Text = 'Invite a person',
    'detached configuration preserves customized companion actions');
  Check(TNyxCodec.Encode(Document) = LBefore,
    'detached configuration has no document side effects');
  Check(TNyxDataToolbarOptions.FromData(LOptions.ToData).ToData.ToJSON =
    LOptions.ToData.ToJSON, 'typed options round-trip exactly');
  LPage.Add(LConfigured);
  LSpec := NyxCollectionView(LPeople).Column(LName, 'People').Selection(nsmMultiple)
    .Query(NyxCollectionQuery.OrderBy(LName));
  LPage.Add(NewNyxList('people-list'));
  LPage.Children[1].Binds.Collection(LSpec);
  LOptions := NyxDataToolbarOptions(NyxControl('people-table'), LName)
    .Sort(NyxToolbarSort('Name descending'), NyxSort(LName, nsdDescending));

  if AFixture <> tbfDefault then
  begin
    LOptions := LOptions.QueryContext(NyxDataToolbarQueryOptions.Display(ntqdSummary));
  end;
  LConfigured := ConfigureNyxDataToolbar(NewNyxDataToolbar('table-tools'), LOptions);

  if AInitialBindings or (AFixture <> tbfDefault) then
  begin
    Document.State.SetValue(NyxTextState('initial-search'), 'Ada');
    Document.State.SetValue(NyxTextState('initial-sort'), 'Name descending');

    if AFixture = tbfProjectionAtBaseline then
    begin
      Document.State.SetValue(NyxTextState('initial-search'), '');
      Document.State.SetValue(NyxTextState('initial-sort'), 'Original order');
    end;

    if AFixture = tbfAliasedState then
    begin
      Document.State.SetValue(NyxTextState('initial-search'), 'Name descending');
    end;

    if AFixture in [tbfSearchProjection, tbfProjectionAtBaseline] then
    begin
      LConfigured.SearchInput.Binds.Value(NyxTextState('initial-search'), bdFromState);
    end
    else
    begin
      LConfigured.SearchInput.Binds.Value(NyxTextState('initial-search'));
    end;

    if AFixture in [tbfSortProjection, tbfProjectionAtBaseline] then
    begin
      LConfigured.SortSelect.Binds.Value(NyxTextState('initial-sort'), bdFromState);
    end
    else if AFixture = tbfAliasedState then
    begin
      LConfigured.SortSelect.Binds.Value(NyxTextState('initial-search'));
    end
    else
    begin
      LConfigured.SortSelect.Binds.Value(NyxTextState('initial-sort'));
    end;
    LConfigured.SearchInput.Configure.ReadOnly(AFixture = tbfReadOnly).Done;
  end;
  LPage.Add(LConfigured);
  LPage.Add(NewNyxTable('people-table'));
  LPage.Children[3].Binds.Collection(LSpec);
  LOptions := NyxDataToolbarOptions(NyxControl('people-list'), LName)
    .Sort(NyxToolbarSort('Name descending'), NyxSort(LName, nsdDescending));
  LPage.Add(ConfigureNyxDataToolbar(NewNyxDataToolbar('peer-tools'), LOptions));
  LToolbar := NewNyxDataToolbar('disabled-tools');
  SetNyxDataToolbarIntent(LToolbar, TNyxDataToolbarIntent.Disabled);
  LPage.Add(LToolbar);
  LToolbar := NewNyxDataToolbar('invalid-clear');
  LToolbar.Add(NewNyxInput('occupied-clear').Configure.PartName(NyxPart('clear')).Done);
  LBefore := LToolbar.SortSelect.Node.Prop(NyxAttributeName(atItems));
  LRejected := False;
  try
    LConfigured := ConfigureNyxDataToolbar(LToolbar, LOptions);
  except
    on ENyxDataToolbar do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LToolbar.Count = 5) and
    (LToolbar.SortSelect.Node.Prop(NyxAttributeName(atItems)) = LBefore) and
    not LToolbar.Extensions.Has(NyxExtension(NyxDataToolbarIntentKey)),
    'incompatible Clear refuses without editing the supplied toolbar');
  Document.AddPage(LPage);
  {$ifdef PAS2JS}
  Host := TJSHTMLElement(Web.document.createElement('div'));
  Web.document.body.appendChild(Host);
  Renderer := TNyxBrowserRenderer.Create;
  Renderer.Render(Document, Document.Pages[0], Host);
  {$else}
  Host := TForm.CreateNew(nil);
  Host.ClientWidth := 1280;

  if ParamCount = 1 then
  begin
    Host.ClientWidth := StrToInt(ParamStr(1));
  end;
  Host.ClientHeight := 850;
  Host.Show;
  Renderer := TNyxLCLRenderer.Create;
  Renderer.Render(Document, Document.Pages[0], Host);
  Application.ProcessMessages;
  TWinControl(Renderer.InputFor(Role('list-tools', 'search'))).HandleNeeded;
  TWinControl(Renderer.InputFor(Role('list-tools', 'sort'))).HandleNeeded;
  TWinControl(Renderer.InputFor(Role('table-tools', 'search'))).HandleNeeded;
  TWinControl(Renderer.InputFor(Role('table-tools', 'sort'))).HandleNeeded;
  {$endif}
end;

destructor TJourney.Destroy;
begin
  Renderer.Free;
  {$ifdef PAS2JS}

  if Host <> nil then
  begin
    Host.remove;
  end;
  {$else}
  Host.Free;
  {$endif}
  Document.Free;
  inherited Destroy;
end;

function TJourney.Role(const AToolbar, APart: TNyxText): TNyxText;
begin
  Result := Renderer.Root.Find(AToolbar).Part(NyxPart(APart)).ID;
end;

function TJourney.Toolbar(const AID: TNyxText): INyxDataToolbarBinding;
begin
  Check(Supports(Renderer.CoordinatorBinding(AID), INyxDataToolbarBinding, Result),
    'mounted toolbar retains its specialized public binding');
end;

function TJourney.Rows(const AID: TNyxText): INyxCollectionView;
begin
  Result := Renderer.CollectionView(AID);
end;

function TJourney.VisibleFirst(const AID: TNyxText): TNyxText;
{$ifdef PAS2JS}
var
  LRow: TJSElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LRow := Renderer.ElementFor(AID).querySelector('[data-nyx-item]');
  Check(LRow <> nil, 'the physical collection exposes a rendered first row');
  Result := LRow.textContent;
  {$else}

  if Renderer.ControlFor(AID) is TListBox then
  begin
    Check(TListBox(Renderer.ControlFor(AID)).Items.Count > 0,
      'the actual native List has a displayed row');
    Exit(TNyxText(TListBox(Renderer.ControlFor(AID)).Items[0]));
  end;
  Result := TNyxText(TStringGrid(Renderer.ControlFor(AID)).Cells[0, 1]);
  {$endif}
end;

procedure TJourney.TypeSearch(const AID, AText: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(Renderer.InputFor(Role(AID, 'search'))).value := AText;
  Renderer.InputFor(Role(AID, 'search')).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(Renderer.InputFor(Role(AID, 'search'))).Text := AText;
  Application.ProcessMessages;
  {$endif}
end;

procedure TJourney.ChooseSort(const AID, AText: TNyxText);
{$ifndef PAS2JS}
var
  LSelect: TComboBox;
{$endif}
begin
  {$ifdef PAS2JS}
  TJSHTMLSelectElement(Renderer.InputFor(Role(AID, 'sort'))).value := AText;
  Renderer.InputFor(Role(AID, 'sort')).dispatchEvent(TJSEvent.new('change'));
  {$else}
  LSelect := TComboBox(Renderer.InputFor(Role(AID, 'sort')));
  LSelect.ItemIndex := LSelect.Items.IndexOf(String(AText));
  Check(LSelect.ItemIndex >= 0, 'ordinary native Sort offers the configured choice');
  LSelect.OnChange(LSelect);
  Application.ProcessMessages;
  {$endif}
end;

procedure TJourney.ClickClear(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  Renderer.ElementFor(Role(AID, 'clear')).click;
  {$else}
  TButton(Renderer.ControlFor(Role(AID, 'clear'))).Click;
  Application.ProcessMessages;
  {$endif}
end;

function TJourney.InputText(const AID, APart: TNyxText): TNyxText;
begin
  {$ifdef PAS2JS}

  if APart = 'sort' then
  begin
    Exit(TJSHTMLSelectElement(Renderer.InputFor(Role(AID, APart))).value);
  end;
  Result := TJSHTMLInputElement(Renderer.InputFor(Role(AID, APart))).value;
  {$else}

  if APart = 'sort' then
  begin
    Exit(TNyxText(TComboBox(Renderer.InputFor(Role(AID, APart))).Text));
  end;
  Result := TNyxText(TCustomEdit(Renderer.InputFor(Role(AID, APart))).Text);
  {$endif}
end;

function TJourney.StatusText: TNyxText;
var
  LID: TNyxText;
begin
  LID := Renderer.Root.Find('table-tools').Part(NyxPart(NyxDataToolbarQueryStatusPart)).ID;
  {$ifdef PAS2JS}
  Result := Renderer.ElementFor(LID).textContent;
  {$else}
  Application.ProcessMessages;
  Result := TNyxText(TLabel(Renderer.ControlFor(LID)).Caption);
  {$endif}
end;

{ The actual Clear button should explain expected input refusals, while direct
  Pascal calls retain their typed exception and exact unchanged transaction.
  Read visible values/status as well as state/query models. }
procedure RunBindingCases;
var
  LFixture: TBindingFixture;
  LJourney: TJourney;
  LBinding: INyxDataToolbarBinding;
  LFeedback: INyxDataToolbarCommandFeedback;
  LBefore: TNyxText;
  LQuery: TNyxText;
  LSearch: TNyxText;
  LSort: TNyxText;
  LRevision: Integer;
  LKind: TNyxInputRefusalKind;
  LRefused: Boolean;
begin
  for LFixture := tbfTwoWay to tbfProjectionAtBaseline do
  begin
    LJourney := TJourney.Create(False, LFixture);
    try
      LBinding := LJourney.Toolbar('table-tools');
      Check(Supports(LBinding, INyxDataToolbarCommandFeedback, LFeedback),
        'the original toolbar binding exposes optional typed command feedback');
      Check(LFeedback.LastCommand.Outcome = ntcoIdle, 'initial adoption is not an invented user command');
      LBefore := TNyxCodec.Encode(LJourney.Document);
      LQuery := LJourney.Rows('people-table').QueryPolicy.ToData.ToJSON;
      LSearch := LJourney.InputText('table-tools', 'search');
      LSort := LJourney.InputText('table-tools', 'sort');
      LRevision := LJourney.Renderer.State.Revision;
      LJourney.ClickClear('table-tools');

      if LFixture in [tbfTwoWay, tbfProjectionAtBaseline] then
      begin
        Check((LFeedback.LastCommand.Command = ntcClear) and
          (LFeedback.LastCommand.Outcome = ntcoSucceeded),
          'writable bindings and equal one-way projections admit a real Clear');
        Check((LJourney.Rows('people-table').Snapshot.Count = 3) and
          (LJourney.InputText('table-tools', 'search') = '') and
          (LJourney.InputText('table-tools', 'sort') = 'Original order'),
          'accepted Clear synchronizes visible Search, Sort and real Table membership');
        Check(Pos('Clear was not applied', LJourney.StatusText) = 0,
          'successful status contains no stale refusal');
      end
      else
      begin
        Check((LFeedback.LastCommand.Command = ntcClear) and
          (LFeedback.LastCommand.Outcome = ntcoRefused) and
          (LFeedback.LastCommand.Message <> ''),
          'actual Clear exposes a structured expected refusal');
        Check((LJourney.Renderer.State.Revision = LRevision) and
          (LJourney.Rows('people-table').QueryPolicy.ToData.ToJSON = LQuery) and
          (LJourney.InputText('table-tools', 'search') = LSearch) and
          (LJourney.InputText('table-tools', 'sort') = LSort),
          'a refused Clear preserves state, query and both physical input values');
        Check(Pos('Clear was not applied.', LJourney.StatusText) > 0,
          'the ordinary rendered query status explains the unchanged Clear');
        LRefused := False;
        try
          LBinding.Clear;
        except
          on LError: ENyxInputAssignmentRefused do
          begin
            LRefused := True;
            LKind := nirkProjection;

            if LFixture = tbfReadOnly then
            begin
              LKind := nirkInteraction;
            end
            else if LFixture = tbfAliasedState then
            begin
              LKind := nirkConflictingState;
            end;
            Check((LError.Kind = LKind) and (LError.Control.ID <> '') and
              (LError.Diagnostic = LFeedback.LastCommand.Message),
              'direct callers retain exact typed input-refusal diagnostics and identity');
          end;
        end;
        Check(LRefused and (LJourney.Renderer.State.Revision = LRevision) and
          (LJourney.Rows('people-table').QueryPolicy.ToData.ToJSON = LQuery),
          'direct refused commands keep their exception and unchanged models');

        if LFixture in [tbfSearchProjection, tbfSortProjection] then
        begin

          if LFixture = tbfSearchProjection then
          begin
            LJourney.Renderer.State.SetValue(NyxTextState('initial-search'), '');
          end
          else
          begin
            LJourney.Renderer.State.SetValue(NyxTextState('initial-sort'), 'Original order');
          end;
          LJourney.ClickClear('table-tools');
          Check((LFeedback.LastCommand.Outcome = ntcoSucceeded) and
            (LJourney.Rows('people-table').Snapshot.Count = 3) and
            (Pos('Clear was not applied', LJourney.StatusText) = 0),
            'updating the authoritative bound value permits recovery and clears the old explanation');
        end
        else if LFixture = tbfReadOnly then
        begin
          LBinding.Sort(NyxToolbarSort('Original order'));
          Check((LFeedback.LastCommand.Command = ntcSort) and
            (LFeedback.LastCommand.Outcome = ntcoSucceeded) and
            (LJourney.InputText('table-tools', 'search') = LSearch) and
            (Pos('Clear was not applied', LJourney.StatusText) = 0),
            'an independent writable Sort remains usable beside a read-only Search');
        end;
      end;
      Check((TNyxCodec.Encode(LJourney.Document) = LBefore) and
        (LJourney.Rows('people-table').Store.Snapshot.Count = 3),
        'binding command outcomes preserve authored defaults and source rows');
      LBinding := nil;
      FreeAndNil(LJourney);
      Check(LFeedback.LastCommand.Outcome in [ntcoSucceeded, ntcoRefused],
        'copied feedback remains inspectable after the mounted owner retires');
      LFeedback := nil;
    finally
      LFeedback := nil;
      LBinding := nil;
      LJourney.Free;
    end;
  end;
end;

procedure Run;
var
  LJourney: TJourney;
  LPeer: TJourney;
  LBinding: INyxDataToolbarBinding;
  LPeerBinding: INyxDataToolbarBinding;
  LBaseline: TNyxText;
  LAuthored: TNyxText;
  LSession: INyxCollectionQuerySession;
  LActor: INyxCollectionQueryOwner;
  LRefused: Boolean;
begin
  LJourney := TJourney.Create;
  try
    LPeer := TJourney.Create(True);
    try
      LAuthored := TNyxCodec.Encode(LJourney.Document);
      LBaseline := LJourney.Rows('people-list').QueryPolicy.ToData.ToJSON;
      LBinding := LJourney.Toolbar('list-tools');
      Check(LBinding.Connected, 'ordinary mounted toolbar is active');
      Check(not LJourney.Toolbar('disabled-tools').Connected,
        'Disabled keeps a dormant lifetime owner without requiring a Clear role or target');
      Check(LJourney.Rows('people-list').Store <> LPeer.Rows('people-list').Store,
        'independent applications own separate live data stores');
      Check((LPeer.Rows('people-table').Snapshot.Count = 1) and
        (LPeer.InputText('table-tools', 'search') = 'Ada') and
        (LPeer.InputText('table-tools', 'sort') = 'Name descending'),
        'initial bound values drive the exact newly adopted toolbar view');
      LJourney.Rows('people-list').Select(NyxItem(NyxCollection('people'), 'bea'));
      LJourney.TypeSearch('list-tools', 'AD');
      Check((LJourney.Rows('people-list').Snapshot.Count = 1) and
        (LJourney.Rows('people-list').Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Ada'),
        'real Search changes displayed List membership with explicit ASCII folding');
      Check(Pos('Ada', LJourney.VisibleFirst('people-list')) > 0,
        'actual List presentation follows the accepted Search');
      Check((LJourney.Rows('people-table').Snapshot.Count = 3) and
        (LPeer.Rows('people-list').Snapshot.Count = 3),
        'another target and another application remain independent');
      Check(LJourney.Rows('people-list').Selection.Count = 1,
        'filtering preserves hidden selected membership');
      LJourney.TypeSearch('list-tools', 'No matching person');
      Check(LJourney.Rows('people-list').Snapshot.Count = 0, 'empty results are accepted');
      LJourney.ClickClear('list-tools');
      Check((LJourney.Rows('people-list').Snapshot.Count = 3) and
        (LJourney.InputText('list-tools', 'search') = '') and
        (LJourney.InputText('list-tools', 'sort') = 'Original order') and
        (LJourney.Rows('people-list').QueryPolicy.ToData.ToJSON = LBaseline),
        'visible Clear restores Search, Sort and the exact authored query together');
      LJourney.ChooseSort('list-tools', 'Name descending');
      Check(LJourney.Rows('people-list').Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Grace',
        'real Sort changes displayed List ordering');
      Check(Pos('Grace', LJourney.VisibleFirst('people-list')) > 0,
        'actual List presentation follows the accepted Sort');
      LJourney.ClickClear('list-tools');
      LJourney.ClickClear('list-tools');
      Check(LJourney.Rows('people-list').QueryPolicy.ToData.ToJSON = LBaseline,
        'repeated Clear remains at the admitted baseline');
      LRefused := False;
      try
        LBinding.Sort(NyxToolbarSort('Not a configured choice'));
      except
        on ENyxDataToolbar do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and
        (LJourney.InputText('list-tools', 'sort') = 'Original order') and
        (LJourney.Rows('people-list').QueryPolicy.ToData.ToJSON = LBaseline),
        'an unknown typed Sort refuses without changing input or query');
      LJourney.TypeSearch('table-tools', 'Grace');
      Check(LJourney.Rows('people-table').Snapshot.Count = 1,
        'the same public toolbar filters an actual Table');
      Check(Pos('Grace', LJourney.VisibleFirst('people-table')) > 0,
        'actual Table cell presentation follows the accepted Search');
      LJourney.ClickClear('table-tools');
      LJourney.ChooseSort('table-tools', 'Name descending');
      Check(LJourney.Rows('people-table').Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Grace',
        'Table ordering uses the same typed query contract');
      Check(Pos('Grace', LJourney.VisibleFirst('people-table')) > 0,
        'actual Table cell presentation follows the accepted Sort');
      LJourney.TypeSearch('peer-tools', 'G');
      LBinding.Search('ra');
      Check(LJourney.Rows('people-list').Snapshot.Count = 1,
        'two ordinary toolbars on one target compose their searches');
      LJourney.ChooseSort('peer-tools', 'Name descending');
      LBinding.Clear;
      Check((LJourney.Rows('people-list').Snapshot.Count = 1) and
        (LJourney.InputText('peer-tools', 'search') = 'G') and
        (LJourney.InputText('peer-tools', 'sort') = 'Name descending'),
        'Clear preserves a peer toolbar search and its shared sort ownership');
      LJourney.ClickClear('peer-tools');
      Check(LJourney.Rows('people-list').QueryPolicy.ToData.ToJSON = LBaseline,
        'clearing the remaining toolbar restores the shared authored baseline');
      LSession := NyxCollectionQuerySession(LJourney.Rows('people-list'));
      LActor := LSession.AcquireOwner(NyxQueryOrigin('peer-filter'), 'Another filter');
      PublishNyxGroup([LSession.PrepareSession(LSession.Capture, LActor,
        NyxQuerySessionEdits.Filter(NyxQueryChannel('prefix'),
          NyxWhere(NyxTextField('name')).StartsWith('G')))]);
      LBinding.Search('ra');
      LBinding.Clear;
      Check((LJourney.Rows('people-list').Snapshot.Count = 1) and LActor.Connected,
        'Clear preserves the current independent peer filter');
      Check((TNyxCodec.Encode(LJourney.Document) = LAuthored) and
        (LJourney.Rows('people-list').Store.Snapshot.Count = 3),
        'runtime query and input commands preserve authored defaults and source rows');
      LJourney.Rows('people-list').ConfigureQuery(LJourney.Rows('people-list').QueryPolicy);
      Check(LBinding.Connected, 'equal raw query assignment preserves accepted toolbar authority');
      LJourney.Rows('people-list').ConfigureQuery(NyxCollectionQuery);
      Check(not LBinding.Connected, 'actual raw query replacement revokes former toolbar authority');
      LRefused := False;
      try
        LBinding.Clear;
      except
        on ENyxDataToolbarCancelled do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'stale Clear refuses instead of overwriting the new query');
      LJourney.ClickClear('list-tools');
      Check(not LJourney.Rows('people-list').QueryPolicy.Defined,
        'a stale physical Clear slot is inert after external query replacement');
      FreeAndNil(LJourney);
      Check(not LBinding.Connected, 'retained specialized binding is inert after renderer disposal');
      LPeerBinding := LPeer.Toolbar('list-tools');
      LPeerBinding.Search('Bea');
      LPeerBinding.Disconnect;
      Check(not LPeerBinding.Connected and (LPeer.Rows('people-list').Snapshot.Count = 1),
        'explicit disconnect retires authority without rolling back an accepted filter');
      LPeer.ClickClear('list-tools');
      Check(LPeer.Rows('people-list').Snapshot.Count = 1,
        'retired physical observation cannot mutate its retained orphan query');
      LPeer.TypeSearch('table-tools', 'Ada');
      Check(LPeer.Rows('people-table').Snapshot.Count = 1,
        'peer real controls remain usable after original application disposal');
    finally
      LPeer.Free;
    end;
  finally
    LJourney.Free;
  end;
end;

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GFailures := TFailures.Create;
    try
      Application.OnException := GFailures.Failed;
      Run;
      RunBindingCases;
      Check(GFailures.Message = '', 'native dispatch has no unhandled callback exception');
    finally
      Application.OnException := nil;
      GFailures.Free;
    end;
    WriteLn('PASS ', GChecks, ' DataToolbar application checks');
    {$else}
    Run;
    RunBindingCases;
    Web.document.body.setAttribute('data-toolbar-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      Web.document.body.setAttribute('data-toolbar-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
