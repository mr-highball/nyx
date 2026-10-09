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
program nyx_resource_catalog_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Forms, Controls, StdCtrls, nyx.test.capture.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.catalog,
  nyx.behavior, nyx.events, nyx.scheduler, nyx.resources, nyx.resources.catalog, nyx.collections,
  nyx.collections.view, nyx.collections.mount, nyx.collections.selection,
  nyx.collections.query, nyx.studio.sections, nyx.studio.section.views,
  nyx.studio.resourceedits
  {$ifdef PAS2JS}, JS, Web{$endif};

type
  { Borrows the provider only for this scenario. Copied selection metadata is
    resolved with its exact data revision before the receiver returns. }
  TSelectionProbe = class(TNyxEventCallback)
  public
    Catalog: INyxResourceCatalog;
    Calls: Integer;
    Last: TNyxResourceCatalogChoice;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Resource catalog controls: ' + AReason);
  end;
  Inc(GChecks);
  WriteLn('PASS ', GChecks, ' / ', AReason);
end;

procedure TSelectionProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.HasCollectionSelection and
    (AEvent.Selection.Count = 1) then
  begin
    Last := Catalog.Choice(AEvent.Selection.ItemAt(0), AEvent.Selection.DataRevision);
    Inc(Calls);
  end;
end;

{ This small public Nyx shell exercises the actual Studio section facade without
  substituting a model-only store fixture for mounted adapters. It deliberately
  has no authored collection defaults; the runtime provider owns its dataset. }
function Shell: TNyxDocument;
var
  LRoot: INyxColumn;
  LProject: INyxColumn;
  LInspector: INyxColumn;
begin
  Result := TNyxDocument.Create;
  try
    LRoot := NewNyxColumn('studio-shell');
    LRoot.Configure.Padding(16).Gap(12).Done;
    LRoot.Add(NewNyxHeading('resource-title').WithText('Project resources'));
    LProject := NewNyxColumn('studio-left');
    LProject.Configure.Gap(12).Done;
    LProject.Add(NewNyxList('resource-picker').Configure.Height(160)
      .AccessibleName('Available resources').Done);
    LProject.Add(NewNyxMemo('resource-draft').Configure.Text('Resource notes')
      .Value('Accepted notes').Height(80).Done);
    LInspector := NewNyxColumn('studio-right');
    LInspector.Add(NewNyxLabel('resource-help').WithText(
      'Browse resources while keeping unfinished notes.'));
    LRoot.Add(LProject).Add(LInspector);
    Result.AddPage(LRoot);
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 15);
end;
{$endif}

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LDocument: TNyxDocument;
  LResourceDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LComponentCatalog: TNyxCatalog;
  LViews: TNyxStudioSectionViews;
  LResources: INyxResources;
  LCatalog: INyxResourceCatalog;
  LMount: INyxCollectionMount;
  LProbe: TSelectionProbe;
  LLease: INyxEventCallback;
  LToken: INyxEventSubscription;
  LSelection: TNyxItemRef;
  LControl: TNyxStudioSectionControl;
  LDraft: TNyxStudioSectionControl;
  LRoot: TNyxNode;
  LWire: TNyxText;
  LRefused: Boolean;
  LIndex: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LRow: TJSHTMLElement;
  LStarted: Double;
  {$else}
  LHost: TForm;
  {$endif}
begin
  LResourceDocument := nil;
  LCandidate := nil;
  LComponentCatalog := nil;
  LResources := NewNyxResources.Define(NyxResourceRef('project-notes'),
    NyxTextResource('Notes').Tagged(NyxResourceLabel('Help')).Describe('Project notes', 'Help for our project'))
    .Define(NyxResourceRef('project-notes'), NyxLocale('en-US'),
      NyxTextResource('English notes').Tagged(NyxResourceLabel('Help')))
    .Define(NyxResourceRef('sample-data'), NyxJSONResource('{"ready":true}'));
  LCatalog := NewNyxResourceCatalog(NyxCollection('control-review'), LResources);
  LDocument := Shell;
  LWire := TNyxCodec.Encode(LDocument);
  LViews := TNyxStudioSectionViews.Create;
  LProbe := TSelectionProbe.Create;
  LLease := LProbe;
  LProbe.Catalog := LCatalog;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.Caption := 'Nyx resource catalog review';
  LHost.SetBounds(20, 20, 680, 640);
  LHost.Show;
  {$endif}
  try
    LViews.Render(LDocument, LDocument.Pages[0], LHost);
    LMount := LViews.BindCollection('resource-picker', LCatalog.View);
    LToken := LViews.ViewFor('resource-picker').Events.On(
      NyxControlEvents('resource-picker', niRuntime), ntSelectionChange).Subscribe(LLease);
    LControl := LViews.ControlFor('resource-picker');
    LDraft := LViews.InputFor('resource-draft');
    LRoot := LViews.SectionRoot(nssProject);
    Check(LMount.Connected and (LViews.CollectionMount('resource-picker') = LMount) and
      (LViews.CollectionView('resource-picker') = LCatalog.View),
      'facade queries the actual independent mounted provider');
    LSelection := LCatalog.Find(NyxResourceRef('project-notes'), NyxLocale('en-US'));
    {$ifdef PAS2JS}
    Check(LControl.querySelectorAll('[data-nyx-item]').length = 3,
      'actual browser rows display all resource/locale pairs');
    LRow := TJSHTMLElement(LControl.querySelector('[data-nyx-item="' + LSelection.ID + '"]'));
    LRow.click;
    TJSHTMLTextAreaElement(LDraft).value := 'Unfinished notes';
    {$else}
    Check(TListBox(LControl).Items.Count = 3,
      'actual native list displays all resource/locale pairs');
    TListBox(LControl).ItemIndex := 1;
    TListBox(LControl).OnClick(LControl);
    TCustomMemo(LDraft).Text := 'Unfinished notes';
    {$endif}
    Check((LProbe.Calls > 0) and (LProbe.Last.Reference.Name = 'project-notes') and
      (LProbe.Last.Locale.Name = 'en-US'), 'actual row selection reports exact typed resource and locale');
    LResources.Define(NyxResourceRef('new-file'), NyxTextResource('New content'));
    LCatalog.Refresh(LResources);
    Check((LViews.ControlFor('resource-picker') = LControl) and
      (LViews.InputFor('resource-draft') = LDraft) and (LViews.SectionRoot(nssProject) = LRoot) and
      (LCatalog.View.Selected.ID = LSelection.ID),
      'insertion retains actual list, editor, section root and selected identity');
    {$ifdef PAS2JS}
    Check((LControl.querySelectorAll('[data-nyx-item]').length = 4) and
      (TJSHTMLTextAreaElement(LDraft).value = 'Unfinished notes'),
      'live browser insertion preserves the unfinished editor value');
    {$else}
    Check((TListBox(LControl).Items.Count = 4) and (TCustomMemo(LDraft).Text = 'Unfinished notes'),
      'live native insertion preserves the unfinished editor value');
    {$endif}
    LViews.Sync;
    Check(LViews.CollectionView('resource-picker') = LCatalog.View,
      'ordinary renderer synchronization retains the manual runtime source');
    LCatalog.Filter(NyxResourceCatalogQuery.Kinds([nrkJSON]));
    {$ifdef PAS2JS}
    Check(LControl.querySelectorAll('[data-nyx-item]').length = 1,
      'typed category query filters actual browser rows');
    {$else}
    Check(TListBox(LControl).Items.Count = 1, 'typed category query filters the actual native list');
    {$endif}
    Check(LCatalog.View.HasSelection and (LCatalog.View.Selected.ID = LSelection.ID),
      'filtering retains hidden exact selection membership');
    LCatalog.Filter(NyxResourceCatalogQuery.Tagged(NyxResourceLabel('Help')));
    {$ifdef PAS2JS}
    Check(LControl.querySelectorAll('[data-nyx-item]').length = 2,
      'exact creator tags filter actual browser rows');
    {$else}
    Check(TListBox(LControl).Items.Count = 2, 'exact creator tags filter the actual native list');
    {$endif}
    Check((LViews.ControlFor('resource-picker') = LControl) and
      (LViews.InputFor('resource-draft') = LDraft) and (LCatalog.View.Selected.ID = LSelection.ID),
      'tag filtering retains mounted control identity and exact selection');
    {$ifdef PAS2JS}
    Check(TJSHTMLTextAreaElement(LDraft).value = 'Unfinished notes',
      'tag filtering preserves the actual browser draft');
    {$else}
    Check(TCustomMemo(LDraft).Text = 'Unfinished notes', 'tag filtering preserves the actual native draft');
    {$endif}
    { Consume the same strict annotation patch that semantic dispatch admits.
      This separately owns its authored candidate; the actual mounted catalog
      receives copied metadata and never retains a document or its payloads. }
    LResourceDocument := TNyxDocument.Create;
    LComponentCatalog := TNyxCatalog.Create;
    for LIndex := 0 to LResources.Count - 1 do
    begin
      LResourceDocument.Resources.Define(LResources.Reference(LIndex),
        LResources.Locale(LIndex), LResources.Definition(LResources.Reference(LIndex),
        LResources.Locale(LIndex)));
    end;
    LCandidate := ReadNyxResourcePatch(NyxResourcePatch([
      NyxSetResourceLabels(NyxResourceRef('project-notes'), NyxLocale('en-US'),
        NyxResourceLabels.Add(NyxResourceLabel('Featured')))]).ToData)
      .Candidate(LResourceDocument, LComponentCatalog);
    LResources := LCandidate.Resources;
    LCatalog.Refresh(LResources);
    {$ifdef PAS2JS}
    Check(LControl.querySelectorAll('[data-nyx-item]').length = 1,
      'semantic candidate metadata refreshes the active browser tag-filtered list');
    {$else}
    Check(TListBox(LControl).Items.Count = 1,
      'semantic candidate metadata refreshes the active native tag-filtered list');
    {$endif}
    LCatalog.Filter(NyxResourceCatalogQuery.Tagged(NyxResourceLabel('Featured')));
    {$ifdef PAS2JS}
    Check(LControl.querySelectorAll('[data-nyx-item]').length = 1,
      'new semantic annotations become queryable in the actual browser rows');
    Check(TJSHTMLTextAreaElement(LDraft).value = 'Unfinished notes',
      'semantic metadata refresh preserves the mounted browser draft');
    {$else}
    Check(TListBox(LControl).Items.Count = 1,
      'new semantic annotations become queryable in the actual native rows');
    Check(TCustomMemo(LDraft).Text = 'Unfinished notes',
      'semantic metadata refresh preserves the mounted native draft');
    {$endif}
    Check((LViews.ControlFor('resource-picker') = LControl) and
      (LViews.InputFor('resource-draft') = LDraft) and
      (LCatalog.View.Selected.ID = LSelection.ID),
      'annotation publication retains adapter/control/selection identity');
    Check(LResources.Definition(NyxResourceRef('project-notes'), NyxLocale('en-US')).Text =
      'English notes', 'annotation publication retains the actual resource contents');
    LCatalog.Filter(NyxResourceCatalogQuery);
    LResources.Remove(NyxResourceRef('project-notes'), NyxLocale('en-US'));
    LCatalog.Refresh(LResources);
    Check(not LCatalog.View.HasSelection, 'removal prunes actual selected resource membership');
    Check(TNyxCodec.Encode(LDocument) = LWire,
      'runtime rows, queries and selections never change authored defaults or source');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-capture-checkpoint', 'resource-catalog-live');
    LStarted := window.performance.now;
    repeat
      await(TJSPromise.resolve(TJSPromise.new(@Pause)));

      if window.performance.now - LStarted > 30000 then
      begin
        raise ENyxResource.Create('Resource catalog capture acknowledgment timed out');
      end;
    until document.body.getAttribute('data-capture-observed') = 'resource-catalog-live';
    {$else}
    Application.ProcessMessages;

    if ParamCount > 0 then
    begin
      SaveNyxNativeCapture(LHost, ParamStr(1), ncmPrint);
    end;
    {$endif}
    LViews.Free;
    LViews := nil;
    Check(not LMount.Connected, 'facade retirement disconnects retained mount without retaining widgets');
    Check(not LToken.Active, 'facade retirement cancels the managed selection callback');
    LRefused := False;
    try
      LMount.Select(LSelection);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired mount refuses selection commands');
    LCatalog.Refresh(LResources.Define(NyxResourceRef('after-retirement'), NyxTextResource('Safe')));
    Check(LCatalog.Find(NyxResourceRef('after-retirement'), NyxDefaultLocale).Defined,
      'provider remains independently usable after adapter retirement');
    WriteLn('PASS ', GChecks, ' actual resource catalog control checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-catalog-checks', IntToStr(GChecks));
    document.body.setAttribute('data-test-result', 'passed');
    {$endif}
  finally
    LViews.Free;
    LToken := nil;
    LProbe.Catalog := nil;
    LLease := nil;
    LMount := nil;
    LResources := nil;
    LCandidate.Free;
    LResourceDocument.Free;
    LComponentCatalog.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
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
      document.body.setAttribute('data-catalog-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  Application.Initialize;
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
