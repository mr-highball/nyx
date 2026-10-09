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
program nyx_view_section_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls, ExtCtrls,
    nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.behavior, nyx.view.sections, nyx.collections, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.view, nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.view.sections.browser
  {$else}, nyx.render.lcl, nyx.view.sections.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  ISection = INyxBrowserViewSection;
  TFace = TJSHTMLElement;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  ISection = INyxLCLViewSection;
  TFace = TControl;
  THost = TPanel;
  TControlAccess = class(TControl);
  {$endif}

  { Managed observer contains no section/view reference. The candidate's same
    IDs must not impersonate an accepted node at this receiver. }
  TScenario = class(TInterfacedObject, INyxViewSectionObserver)
  public
    Clicks: Integer;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Configure(ARenderer: TRenderer);
  end;

  { Fixture-only temporary registration. Cancel its token and release Change
    before disposal so its strong handle never forms a persistent router cycle. }
  TReentrantProbe = class(TNyxEventCallback)
  public
    Change: INyxViewSectionChange;
    Refused: Boolean;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
    procedure ViewChanged(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;
  GFailBuild: Boolean;
  GFailUpdate: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('View sections: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
function CustomButton(ANode: TNyxNode): TJSHTMLElement;
begin

  if GFailBuild then
  begin
    raise ENyxModel.Create('Intentional section factory refusal');
  end;
  Result := TJSHTMLElement(document.createElement('button'));
  Result.className := 'nyx-button';
  Result.textContent := ANode.Prop('text');
end;

procedure UpdateButton(ANode: TNyxNode; AFace: TJSHTMLElement);
begin

  if GFailUpdate then
  begin
    GFailUpdate := False;
    raise ENyxModel.Create('Intentional section preview refusal');
  end;
  AFace.textContent := ANode.Prop('text');
end;
{$else}
function CustomButton(ANode: TNyxNode; AOwner: TComponent): TControl;
begin

  if GFailBuild then
  begin
    raise ENyxModel.Create('Intentional section factory refusal');
  end;
  Result := TButton.Create(AOwner);
  TButton(Result).Caption := ANode.Prop('text');
end;

procedure UpdateButton(ANode: TNyxNode; AFace: TControl);
begin

  if GFailUpdate then
  begin
    GFailUpdate := False;
    raise ENyxModel.Create('Intentional section preview refusal');
  end;
  TButton(AFace).Caption := ANode.Prop('text');
end;
{$endif}

procedure TScenario.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (AEvent.Trigger = ntClick) and (ANode.Kind = NyxKindName(nkButton)) then
  begin
    Inc(Clicks);
  end;
end;

procedure TScenario.Configure(ARenderer: TRenderer);
begin
  ARenderer.RegisterFactory(NyxKindName(nkButton), CustomButton, UpdateButton);
end;

procedure TReentrantProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin

  if (AEvent.Trigger = ntClick) and (AExecution <> nil) then
  begin
    Refused := not PublishNyxViewSections([Change]);
  end;
end;

procedure TReentrantProbe.ViewChanged(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin

  if (AView <> nil) and (AChanges = nil) then
  begin
    Refused := not PublishNyxViewSections([Change]);
  end;
end;

function Example(const APrefix, ATitle: TNyxText; AExtra: Boolean): TNyxDocument;
var
  LPage: INyxColumn;
  LTable: INyxTable;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxColumn(APrefix + '-section');
    LPage.Configure.Padding(18).Gap(12).Done;
    LPage.Add(NewNyxHeading(APrefix + '-heading').WithText(ATitle));
    LPage.Add(NewNyxInput(APrefix + '-input').WithText('Design notes')
      .Configure.Value('Ready to create').Done);
    LPage.Add(NewNyxButton(APrefix + '-action').WithText('Save notes'));
    Result.Collections.Define(NyxCollection(APrefix + '-notes'),
      NyxCollectionSchema.Text(NyxTextField('title'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection(APrefix + '-notes'), 'first'))
        .WithValue(NyxTextField('title'), 'A thoughtful interface')]);
    LTable := NewNyxTable(APrefix + '-table');
    LTable.Configure.Height(96).Done;
    LTable.Binds.Collection(NyxCollectionView(NyxCollection(APrefix + '-notes'))
      .Column(NyxTextField('title'), 'Note', cmEditable)).Done;
    LPage.Add(LTable);

    if AExtra then
    begin
      LPage.Add(NewNyxLabel(APrefix + '-extra').WithText('A new independently published control.'));
    end;
    Result.AddPage(LPage);
  except
    Result.Free;
    raise;
  end;
end;

function Face(ASection: ISection; const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}
  Result := ASection.Renderer.ElementFor(AID);
  {$else}
  Result := ASection.Renderer.ControlFor(AID);
  {$endif}
end;

function Input(ASection: ISection; const AID: TNyxText): TFace;
begin
  Result := ASection.Renderer.InputFor(AID);
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(AFace).value;
  {$else}
  Result := TCustomEdit(AFace).Text;
  {$endif}
end;

procedure Draft(AFace: TFace);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := 'Independent draft / 🌙';
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := TNyxText('Independent draft / 🌙');
  {$endif}
end;

procedure Click(AFace: TFace);
begin
  {$ifdef PAS2JS}
  AFace.click;
  {$else}
  TControlAccess(AFace).Click;
  {$endif}
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  { Surface timer setup failure through the same awaited promise. }
  try
    window.setTimeout(procedure
      begin
        AResolve('');
      end, 15);
  except
    on LException: Exception do
    begin
      AReject(LException.Message);
    end;
  end;
end;

procedure Capture; async;
var
  LStarted: Double;
begin
  document.body.setAttribute('data-capture-checkpoint', 'view-sections-live');
  LStarted := window.performance.now;
  repeat
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));

    if window.performance.now - LStarted > 30000 then
    begin
      raise ENyxModel.Create('View section capture acknowledgment timed out');
    end;
  until document.body.getAttribute('data-capture-observed') = 'view-sections-live';
end;
{$endif}

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LHeader: ISection;
  LDetails: ISection;
  LHeaderDoc: TNyxDocument;
  LDetailsDoc: TNyxDocument;
  LNextHeader: TNyxDocument;
  LNextDetails: TNyxDocument;
  LHeaderChange: INyxViewSectionChange;
  LDetailsChange: INyxViewSectionChange;
  LStale: INyxViewSectionChange;
  LObserver: TScenario;
  LObserverLease: INyxViewSectionObserver;
  LHeaderHost: THost;
  LDetailsHost: THost;
  LInput: TFace;
  LAction: TFace;
  LDetailsInput: TFace;
  LHeaderRoot: TNyxNode;
  LDetailsRoot: TNyxNode;
  LBefore: TNyxText;
  LRefused: Boolean;
  LRevision: Integer;
  LHeaderMount: INyxCollectionMount;
  LFormerDetailsMount: INyxCollectionMount;
  LDetailsMount: INyxCollectionMount;
  LHostWidth: Integer;
  LProbe: TReentrantProbe;
  LProbeLease: INyxEventCallback;
  LProbeToken: INyxEventSubscription;
  LViewProbeToken: INyxCollectionViewSubscription;
  {$ifdef PAS2JS}LHostStyle: TNyxText;{$endif}
  {$ifndef PAS2JS}LWindow: TForm;{$endif}
begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  LHeaderDoc := Example('header', 'Keep your ideas close', False);
  LDetailsDoc := Example('details', 'Shape the details', False);
  LNextHeader := Example('header', 'Your next idea', True);
  LNextDetails := Example('details', 'Explore another possibility', True);
  LObserver := TScenario.Create;
  LObserverLease := LObserver;
  {$ifdef PAS2JS}
  LHeaderHost := TJSHTMLElement(document.createElement('section'));
  LDetailsHost := TJSHTMLElement(document.createElement('section'));
  LHeaderHost.style.cssText := 'height:340px;flex:1;min-width:0;max-width:440px;';
  LDetailsHost.style.cssText := LHeaderHost.style.cssText;
  document.getElementById('sections').appendChild(LHeaderHost);
  document.getElementById('sections').appendChild(LDetailsHost);
  {$else}
  LWindow := TForm.CreateNew(nil);
  LWindow.Caption := 'Nyx view sections';
  LWindow.ClientWidth := 900;
  LWindow.ClientHeight := 380;
  LHeaderHost := TPanel.Create(LWindow);
  LHeaderHost.Parent := LWindow;
  LHeaderHost.SetBounds(12, 12, 430, 350);
  LHeaderHost.BevelOuter := bvNone;
  LDetailsHost := TPanel.Create(LWindow);
  LDetailsHost.Parent := LWindow;
  LDetailsHost.SetBounds(454, 12, 430, 350);
  LDetailsHost.BevelOuter := bvNone;
  LWindow.Show;
  Application.ProcessMessages;
  {$endif}
  try
    {$ifdef PAS2JS}
    LHeader := NewNyxBrowserViewSection(TNyxViewSectionRef.Named('header'), LHeaderHost,
      nil, LObserverLease);
    LDetails := NewNyxBrowserViewSection(TNyxViewSectionRef.Named('details'), LDetailsHost,
      @LObserver.Configure, LObserverLease);
    {$else}
    LHeader := NewNyxLCLViewSection(TNyxViewSectionRef.Named('header'), LHeaderHost,
      nil, LObserverLease);
    LDetails := NewNyxLCLViewSection(TNyxViewSectionRef.Named('details'), LDetailsHost,
      LObserver.Configure, LObserverLease);
    {$endif}
    Check((LHeader.Root = nil) and (LHeader.Revision = 0), 'new section has no accepted view');
    LHeaderChange := LHeader.Prepare(LHeaderDoc, LHeaderDoc.Pages[0]);
    LDetailsChange := LDetails.Prepare(LDetailsDoc, LDetailsDoc.Pages[0]);
    Check((LHeader.Root = nil) and (LDetails.Root = nil) and (LObserver.Clicks = 0),
      'preparation leaves both live hosts and observers unchanged');
    FreeAndNil(LHeaderDoc);
    FreeAndNil(LDetailsDoc);
    Check(PublishNyxViewSections([LHeaderChange, LDetailsChange]),
      'independent source snapshots publish as one actual two-view group');
    Check((LHeaderChange.State = vcsPublished) and (LDetailsChange.State = vcsPublished) and
      (LHeader.Revision = 1) and (LDetails.Revision = 1), 'publication has explicit revisions/states');
    LInput := Input(LHeader, 'header-input');
    LAction := Face(LHeader, 'header-action');
    LHeaderRoot := LHeader.Root;
    LHeaderMount := LHeader.Renderer.CollectionMount('header-table');
    LFormerDetailsMount := LDetails.Renderer.CollectionMount('details-table');
    Check(LHeaderMount.EditCell(NyxItem(NyxCollection('header-notes'), 'first'),
      0, 'Retained row edit'), 'actual table mount admits a typed-scoped row edit');
    Draft(LInput);
    LBefore := TNyxCodec.Encode(LNextDetails);
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    Check((LHeader.Root = LHeaderRoot) and (Input(LHeader, 'header-input') = LInput) and
      (TextOf(LInput) = TNyxText('Independent draft / 🌙')), 'staging another section keeps live input');
    Check(PublishNyxViewSections([LDetailsChange]) and
      (Input(LHeader, 'header-input') = LInput) and (Face(LHeader, 'header-action') = LAction) and
      (LHeader.Root = LHeaderRoot) and (TextOf(LInput) = TNyxText('Independent draft / 🌙')),
      'structural sibling publication preserves actual unrelated controls and draft');
    Check(LHeaderMount.Connected and not LFormerDetailsMount.Connected and
      (LHeaderMount.View.CellText(NyxItem(NyxCollection('header-notes'), 'first'), 0) =
        'Retained row edit'), 'unmentioned collection retains its edit; former sibling mount disconnects');
    Check(TNyxCodec.Encode(LNextDetails) = LBefore, 'view publication does not mutate its source document');
    Click(LAction);
    Click(Face(LDetails, 'details-action'));
    Check(LObserver.Clicks = 2, 'retained and newly published actual buttons reach the managed observer');
    Check(LDetails.Root.Find('details-extra') <> nil, 'new structure belongs to the published section');

    LDetailsRoot := LDetails.Root;
    LDetailsMount := LDetails.Renderer.CollectionMount('details-table');
    LDetailsInput := Input(LDetails, 'details-input');
    GFailBuild := True;
    LRefused := False;
    try
      LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    GFailBuild := False;
    Check(LRefused and (LDetails.Root = LDetailsRoot) and
      (Input(LDetails, 'details-input') = LDetailsInput), 'failed real target factory retains accepted view');

    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    GFailUpdate := True;
    LRefused := False;
    try
      PublishNyxViewSections([LHeaderChange, LDetailsChange]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and not GFailUpdate and (LHeader.Root = LHeaderRoot) and
      (LDetails.Root = LDetailsRoot) and (Input(LHeader, 'header-input') = LInput) and
      (Input(LDetails, 'details-input') = LDetailsInput),
      'failed second physical preview restores BOTH exact old control trees');
    Check(LHeaderMount.Connected and LDetailsMount.Connected,
      'failed publication retains both accepted collection scopes');
    Check((LHeaderChange.State = vcsCanceled) and (LDetailsChange.State = vcsCanceled) and
      (LHeader.Revision = 1) and (LDetails.Revision = 2) and
      (TextOf(LInput) = TNyxText('Independent draft / 🌙')),
      'rollback preserves revisions/draft and cancels the whole group');
    Click(LAction);
    Check(LObserver.Clicks = 3, 'rollback retains original button event route');

    LStale := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    Check(PublishNyxViewSections([LDetailsChange]), 'a newer view supersedes an outstanding proposal');
    LDetailsRoot := LDetails.Root;
    Check(not PublishNyxViewSections([LStale]) and (LDetails.Root = LDetailsRoot) and
      (LStale.State = vcsPrepared), 'stale revision refuses before placement changes');
    LStale.Cancel;
    Check(LStale.State = vcsCanceled, 'explicit cancellation releases an unpublished candidate');
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    LRefused := False;
    try
      PublishNyxViewSections([LDetailsChange, LDetailsChange]);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDetails.Root = LDetailsRoot) and
      (LDetailsChange.State = vcsPrepared), 'duplicate group refuses before preview');
    LDetailsChange.Cancel;
    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    LProbe := TReentrantProbe.Create;
    LProbeLease := LProbe;
    LProbe.Change := LHeaderChange;
    { EditCell can already focus/select this row. Force a real subsequent
      selection transition so the guard is exercised inside Notify. }
    LHeaderMount.View.ClearSelection;
    LViewProbeToken := LHeaderMount.View.Subscribe({$ifdef PAS2JS}@{$endif}LProbe.ViewChanged);
    try
      LHeaderMount.View.Select(NyxItem(NyxCollection('header-notes'), 'first'));
      Check(LProbe.Refused and (LHeader.Root = LHeaderRoot),
        'collection selection notification cannot retire its borrowed view');
    finally
      LViewProbeToken.Disconnect;
      LViewProbeToken := nil;
      LProbe.Change := nil;
      LProbeLease := nil;
      LHeaderChange.Cancel;
    end;
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    LProbe := TReentrantProbe.Create;
    LProbeLease := LProbe;
    LProbe.Change := LDetailsChange;
    LProbeToken := LDetails.Renderer.Events.On(
      NyxControlEvents('details-action', niRuntime), ntClick).Subscribe(LProbeLease);
    try
      Click(Face(LDetails, 'details-action'));
      Check(LProbe.Refused and (LDetailsChange.State = vcsPrepared) and
        (LDetails.Root = LDetailsRoot), 'actual callback cannot retire its borrowed input view');
      LProbe.Change := nil;
      Check(PublishNyxViewSections([LDetailsChange]) and not LProbeToken.Active,
        'later idle publication succeeds and retires the former event scope');
      LDetailsRoot := LDetails.Root;
    finally
      LProbeToken.Cancel;
      LProbe.Change := nil;
      LProbeToken := nil;
      LProbeLease := nil;
    end;
    LDetailsChange := LDetails.Prepare(LNextDetails, LNextDetails.Pages[0]);
    {$ifdef PAS2JS}
    LHostStyle := LDetailsHost.style.cssText;
    LHostWidth := LDetailsHost.clientWidth;
    LDetailsHost.style.setProperty('flex', 'none');
    LDetailsHost.style.setProperty('max-width', 'none');
    LDetailsHost.style.setProperty('width', IntToStr(LHostWidth + 7) + 'px');
    {$else}
    LHostWidth := LDetailsHost.Width;
    LDetailsHost.Width := LHostWidth + 7;
    {$endif}
    Check(not PublishNyxViewSections([LDetailsChange]) and (LDetails.Root = LDetailsRoot),
      'changed host allocation refuses stale prepared target geometry');
    {$ifdef PAS2JS}LDetailsHost.style.cssText := LHostStyle;
    {$else}LDetailsHost.Width := LHostWidth;{$endif}
    LDetailsChange.Cancel;
    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    Check(PublishNyxViewSections([LHeaderChange]), 'later fresh replacement succeeds after rollback');
    Check((LHeader.Root.Find('header-extra') <> nil) and (LHeader.Revision = 2),
      'successful structural publication transfers the new owned tree');
    LRevision := LHeader.Revision;
    LHeaderChange := LHeader.Prepare(LNextHeader, LNextHeader.Pages[0]);
    LHeaderChange.Cancel;
    Check((LHeader.Revision = LRevision) and (LHeader.Root.Find('header-extra') <> nil),
      'canceling a staged candidate retains the accepted view');
    Check(PublishNyxViewSections([]), 'empty publication group is an explicit no-op');
    {$ifdef PAS2JS}await(Capture);{$else}
    Application.ProcessMessages;

    if ParamCount > 0 then
    begin
      SaveNyxNativeCapture(LWindow, ParamStr(1), ncmPrint);
    end;
    {$endif}
    LDetailsMount := LDetails.Renderer.CollectionMount('details-table');
    LDetails.Close;
    Check((LDetails.Root = nil) and (LHeader.Root <> nil),
      'explicit section retirement keeps its independent sibling alive');
    Check(not LDetailsMount.Connected,
      'closed section no longer has a live collection mount');
    LHeader.Close;
    Check((LHeader.Root = nil) and (LHeader.Revision = LRevision + 1),
      'explicit retirement revokes the mounted owner');
    WriteLn('PASS ', GChecks, ' actual staged section checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-section-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
  finally
    LProbeToken := nil;
    LViewProbeToken := nil;
    LProbeLease := nil;
    LHeaderChange := nil;
    LDetailsChange := nil;
    LStale := nil;
    LHeader := nil;
    LDetails := nil;
    LObserverLease := nil;
    LHeaderDoc.Free;
    LDetailsDoc.Free;
    LNextHeader.Free;
    LNextDetails.Free;
    {$ifdef PAS2JS}
    LHeaderHost.remove;
    LDetailsHost.remove;
    {$else}
    LWindow.Free;
    {$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-section-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
