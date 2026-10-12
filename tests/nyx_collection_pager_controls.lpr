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
program nyx_collection_pager_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.controls,
  nyx.collections, nyx.collections.view, nyx.collections.pager,
  nyx.collections.query, nyx.events, nyx.behavior, nyx.scheduler, nyx.state,
  nyx.collections.bindings, nyx.collections.paging, nyx.composition,
  nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, Spin,
    Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl;{$endif}

type
  { The fixture callback borrows the renderer only for this synchronous journey.
    Actual navigation retires its attachment while its owned event remains valid. }
  TPagerProbe = class(TNyxEventCallback)
    Calls: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  { Tiny read-only host observation for the maintained trusted-input consumer.
    This reports the public view after normal adapter dispatch, never authors it. }
  TPagerObservation = class
    procedure Signal(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;
  {$endif}
  {$ifndef PAS2JS}
  TCaptionAccess = class(TControl);
  {$endif}

var
  GDocument: TNyxDocument;
  GView: INyxCollectionView;
  GPager: INyxPagination;
  GAttachment: INyxCollectionPager;
  GCollections: INyxCollectionBindings;
  GAuthored: TNyxText;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GObservation: TPagerObservation;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TForm;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Collection pager: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifdef PAS2JS}
procedure TPagerObservation.Signal(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  document.body.setAttribute('data-pager-page', IntToStr(NyxCollectionPages(GView).Window.Page));
  document.body.setAttribute('data-pager-pages', IntToStr(NyxCollectionPages(GView).Window.Pages));
  document.body.setAttribute('data-pager-visible', IntToStr(GView.Snapshot.Count));
end;
{$endif}

procedure InitializeCollections;
var
  LRoot: TNyxNode;
begin
  LRoot := RealizeNyxView(GDocument, GDocument.Find('paging-review'));
  try
    GCollections := NewNyxCollectionBindings(LRoot,
      NewNyxCollectionContext(GDocument.Collections));
  finally
    LRoot.Free;
  end;
end;

procedure Mount;
var
  LControl: INyxControl;
begin
  GRenderer.Render(GDocument, GDocument.Find('paging-review'), GHost, False, nil, GCollections);
  GView := GRenderer.CollectionView('paging-table');
  LControl := RetainNyxControl(GRenderer.Root.Find('ideas-pager'));
  Check(Supports(LControl, INyxPagination), 'actual mounted specialized pagination contract');
  GPager := LControl as INyxPagination;
  GAttachment := GRenderer.CollectionPager('ideas-pager');
  Check((GAttachment <> nil) and GAttachment.Connected, 'automatic portable pager attachment');
end;

procedure Click(const AButton: INyxButton);
begin
  {$ifdef PAS2JS}
  GRenderer.ElementFor(AButton.ID).click;
  {$else}
  TButton(GRenderer.ControlFor(AButton.ID)).Click;
  Application.ProcessMessages;
  {$endif}
end;

function ButtonEnabled(const AButton: INyxButton): Boolean;
begin
  {$ifdef PAS2JS}
  Result := not TJSHTMLButtonElement(GRenderer.ElementFor(AButton.ID)).disabled;
  {$else}
  Result := GRenderer.ControlFor(AButton.ID).Enabled;
  {$endif}
end;

function TotalText: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := GRenderer.ElementFor(GPager.TotalLabel.ID).textContent;
  {$else}
  Result := TCaptionAccess(GRenderer.ControlFor(GPager.TotalLabel.ID)).Caption;
  {$endif}
end;

{ Read the actual input widget, not only its portable Enabled property. Both
  adapters must present the same live restriction on the editable page part. }
function PageInputEnabled: Boolean;
begin
  {$ifdef PAS2JS}
  Result := not TJSHTMLInputElement(GRenderer.ElementFor(GPager.PageSpin.ID)
    .querySelector('input')).disabled;
  {$else}
  Result := GRenderer.InputFor(GPager.PageSpin.ID).Enabled;
  {$endif}
end;

procedure EnterPage(APage: Integer);
var
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$else}
  LInput: TSpinEdit;
  {$endif}
begin
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(GRenderer.ElementFor(GPager.PageSpin.ID).querySelector('input'));
  LInput.focus;
  LInput.value := IntToStr(APage);
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput := TSpinEdit(GRenderer.InputFor(GPager.PageSpin.ID));
  LInput.SetFocus;
  LInput.Value := APage;

  if Assigned(LInput.OnEditingDone) then
  begin
    LInput.OnEditingDone(LInput);
  end;
  Application.ProcessMessages;
  {$endif}
end;

procedure TPagerProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  Check((AEvent.OriginID = GPager.NextButton.ID) and
    (NyxCollectionPages(GView).Window.Page = 2),
    'application callback observes the already-admitted page');
  GRenderer.Unmount;
end;

procedure Run;
var
  LPages: INyxCollectionPages;
  LOther: INyxCollectionView;
  LOld: INyxCollectionPager;
  LEvent: TNyxEventInfo;
  LProbe: TPagerProbe;
  LCallback: INyxEventCallback;
  LToken: INyxEventSubscription;
  LRejected: Boolean;
  LInitial: TNyxCollectionPaging;
begin
  LPages := NyxCollectionPages(GView);
  LOther := GRenderer.CollectionView('disabled-paging-table');
  Check((LPages.Window.Pages = 6) and (LPages.Window.Total = 40) and
    (GView.Snapshot.Count = 7), 'exact MCP-authored page defaults and complete membership');
  Check((TotalText = 'of 6') and not ButtonEnabled(GPager.PreviousButton) and
    ButtonEnabled(GPager.NextButton), 'physical first-page count and navigation boundaries');

  if GRenderer.State.Has('pager-actions-enabled') then
  begin
    GRenderer.State.SetValue(NyxBooleanState('pager-actions-enabled'), False);
    LPages.NextPage;
    Check(not ButtonEnabled(GPager.NextButton) and not ButtonEnabled(GPager.PreviousButton),
      'live state restriction survives page changes on both navigation parts');
    Check(not PageInputEnabled and
      (GPager.PageSpin.Node.Prop(NyxAttributeName(atEnabled)) = 'false'),
      'live state restricts the actual page input and its portable property');
    GRenderer.State.SetValue(NyxBooleanState('pager-actions-enabled'), True);
    Check(ButtonEnabled(GPager.NextButton) and ButtonEnabled(GPager.PreviousButton) and PageInputEnabled,
      'live state restores controls only when the current page permits them');
    LPages.LastPage;
    GRenderer.State.SetValue(NyxBooleanState('pager-actions-enabled'), False);
    GRenderer.State.SetValue(NyxBooleanState('pager-actions-enabled'), True);
    Check(not ButtonEnabled(GPager.NextButton) and ButtonEnabled(GPager.PreviousButton),
      'state publication cannot enable Next beyond the last page');
    LPages.FirstPage;
    LInitial := LPages.Paging;
    LPages.ConfigurePaging(Default(TNyxCollectionPaging));
    Check((GView.Snapshot.Count = 40) and not ButtonEnabled(GPager.NextButton),
      'runtime unpaged policy exposes all results and closes navigation');
    LOld := GAttachment;
    Mount;
    Check(not LOld.Connected and (GView.Snapshot.Count = 40) and
      not ButtonEnabled(GPager.PreviousButton) and not ButtonEnabled(GPager.NextButton),
      'remount retains the runtime unpaged policy and replaces retired constraints');
    LPages := NyxCollectionPages(GView);
    LPages.ConfigurePaging(LInitial);
    LPages.GoToPage(2);
    Mount;
    Check((NyxCollectionPages(GView).Window.Page = 2) and ButtonEnabled(GPager.PreviousButton),
      'remount preserves a live page without inheriting the prior first-page restriction');
    LPages := NyxCollectionPages(GView);
    LPages.FirstPage;
  end;
  GPager.NextButton.Configure.Enabled(False).Done;
  GRenderer.Sync;
  Check(not ButtonEnabled(GPager.NextButton), 'caller can disable a live pager part');
  LPages.NextPage;
  Check(not ButtonEnabled(GPager.NextButton), 'page refresh preserves caller-disabled part');
  LPages.FirstPage;
  GPager.NextButton.Configure.Enabled(True).Done;
  GRenderer.Sync;
  Click(GPager.NextButton);
  Check((LPages.Window.Page = 2) and (GView.Snapshot.ItemAt(0).Ref.ID = 'idea-8'),
    'ordinary Next action projects the next seven stable items');
  GView.SelectAll;
  Check((GView.Selection.Count = 40) and (GView.Selection.Focus.ID = 'idea-8'),
    'select all covers the filtered result while keeping focus on this page');
  GView.ClearSelection;
  Check((NyxCollectionPages(LOther).Window.Page = 1) and (LOther.Snapshot.Count = 7),
    'another collection view retains its independent page');
  Check(ButtonEnabled(GPager.PreviousButton), 'previous button becomes physically enabled');
  Click(GPager.PreviousButton);
  Check(LPages.Window.Page = 1, 'ordinary Previous returns to the first page');
  EnterPage(5);
  Check((LPages.Window.Page = 5) and (GView.Snapshot.ItemAt(0).Ref.ID = 'idea-29'),
    'the actual page input admits its ordinary commit');
  Click(GPager.NextButton);
  Check((LPages.Window.Page = 6) and (GView.Snapshot.Count = 5) and
    not ButtonEnabled(GPager.NextButton), 'last partial page disables physical Next');
  Click(GPager.NextButton);
  Check(LPages.Window.Page = 6, 'a forced disabled callback cannot advance past the final page');
  GView.Select(NyxItem(NyxCollection('project-ideas'), 'idea-38'));
  LPages.FirstPage;
  Check(GView.Selection.Contains(NyxItem(NyxCollection('project-ideas'), 'idea-38')),
    'paging retains selection membership outside the current page');
  GView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(38)));
  Check((TotalText = 'of 1') and (GPager.PageSpin.Value = 1) and
    (GView.Snapshot.Count = 3) and not ButtonEnabled(GPager.NextButton),
    'filter changes synchronize the real pager count and current page');
  GView.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(100)));
  Check((TotalText = 'No results') and not ButtonEnabled(GPager.PreviousButton) and
    not ButtonEnabled(GPager.NextButton) and (GView.Snapshot.Count = 0),
    'empty filtered results have no navigation authority');
  GView.ConfigureQuery(NyxCollectionQuery);
  Check((TotalText = 'of 6') and ButtonEnabled(GPager.NextButton),
    'query recovery restores the reusable pager');
  Check(TNyxCodec.Encode(GDocument) = GAuthored, 'runtime paging never rewrites authored defaults');

  LProbe := TPagerProbe.Create;
  LCallback := LProbe;
  LToken := GRenderer.Events.On(NyxCompoundEvents(GPager.ID, niRuntime), ntClick).Subscribe(LCallback);
  LOld := GAttachment;
  try
    Click(GPager.NextButton);
    Check((LProbe.Calls = 1) and not LOld.Connected,
      'callback retirement disconnects the retained pager before releasing its controls');
    LRejected := False;
    try
      LEvent := Default(TNyxEventInfo);
      LOld.ApplyEvent(LEvent);
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'retained retired attachments refuse borrowed control access');
    LPages.FirstPage;
    Check(not LOld.Connected, 'later data/view notifications cannot revive retired controls');
  finally
    LToken.Cancel;
    LToken := nil;
    LCallback := nil;
  end;
  Mount;
end;

{$ifndef PAS2JS}
procedure CaptureNative;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    GHost.Repaint;
    Application.ProcessMessages;
    LBitmap.SetSize(GHost.Width, GHost.Height);
    GHost.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    GDocument := BuildNyxDocument;
    GAuthored := TNyxCodec.Encode(GDocument);
    { The public runtime view set retains stores/page state across remounts,
      exactly as an application does. Layout/defaults remain the MCP export. }
    InitializeCollections;
    {$ifdef PAS2JS}
    GHost := TJSHTMLElement(document.getElementById('app'));
    GRenderer := TNyxBrowserRenderer.Create;
    {$else}
    GHost := TForm.Create(nil);
    GHost.SetBounds(0, 0, 900, 650);
    GHost.Show;
    GRenderer := TNyxLCLRenderer.Create;
    {$endif}
    Mount;
    Run;
    {$ifdef PAS2JS}
    GObservation := TPagerObservation.Create;
    GRenderer.OnEvent := @GObservation.Signal;
    GObservation.Signal(nil, Default(TNyxEventInfo));
    document.body.setAttribute('data-pager-previous-id', GPager.PreviousButton.ID);
    document.body.setAttribute('data-pager-next-id', GPager.NextButton.ID);
    document.body.setAttribute('data-pager-input-id', GPager.PageSpin.ID);
    document.body.setAttribute('data-pager-controls', 'passed');
    document.body.setAttribute('data-pager-checks', IntToStr(GChecks));
    {$else}
    CaptureNative;
    WriteLn('PASS ', GChecks, ' actual compiled native collection-pager checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-pager-controls', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  GView := nil;
  GPager := nil;
  GAttachment := nil;
  GCollections := nil;
  GRenderer.Free;
  GHost.Free;
  GDocument.Free;
  {$endif}
end.
