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
program nyx_canvas_collection_lifecycle_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.root.types,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.canvas.types, nyx.canvas.geometry, nyx.canvas.declarations,
  nyx.canvas.workspace, nyx.application.context, nyx.application.resources,
  nyx.scheduler, nyx.resources, nyx.resources.rows, nyx.resource.sources,
  nyx.collections.registry
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.canvas.browser
  {$else}, Interfaces, Forms, Grids, nyx.application.lcl, nyx.canvas.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Each browser application owns its normal runtime and revocable canvas mounts. }
  TTestApplication = TNyxBrowserApplication;
  { Retained inspection facet borrows item renderers only until mount retirement. }
  TTestCanvas = INyxBrowserCanvasWorkspaceView;
  {$else}
  { Each native application owns its normal runtime and revocable canvas mounts. }
  TTestApplication = TNyxLCLApplication;
  { Retained inspection facet borrows item renderers only until mount retirement. }
  TTestCanvas = INyxLCLCanvasWorkspaceView;
  {$endif}

const
  CPrivateTable: TNyxText = 'notes-instance/notes-table';
  CEditedNote: TNyxText = 'First card only 🌙';
  CLocalizedNote: TNyxText = 'A localized note 🌙';

var
  GDocument: TNyxDocument;
  GApplications: array[0..1] of TTestApplication;
  GCanvases: array of TTestCanvas;
  GPrivate: array of INyxCollection;
  GShared: INyxCollection;
  GContext: INyxApplicationContext;
  GFragmentContext: INyxApplicationContext;
  GCapturedScopes: INyxCollectionContext;
  GResourceRevision0: Integer;
  GResourceRevision1: Integer;
  GBefore: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GDone: Boolean;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Item(AIndex: Integer): TNyxCanvasItemRef;
begin

  if AIndex = 0 then
  begin
    Result := NyxCanvasItem('first-card');
  end
  else
  begin
    Result := NyxCanvasItem('second-card');
  end;
end;

function BuildDocument: TNyxDocument;
var
  LNotes: INyxPanel;
  LCard: INyxPanel;
  LInstance: INyxComponent;
  LPage: INyxPage;
  LCanvas: INyxCanvas;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Shared team, private notes';
    Result.Collections.Define(NyxCollection('team'),
      NyxCollectionSchema.Text(NyxTextField('name'), ''),
      [NyxCollectionItem(NyxItem(NyxCollection('team'), 'ada'))
        .WithValue(NyxTextField('name'), 'Ada')]);
    Result.Resources.Define(NyxResourceRef('notes'),
      NyxJSONResource('[{"id":"note","name":"Your private note"}]'));
    Result.Resources.Define(NyxResourceRef('notes'), NyxLocale('en-US'),
      NyxJSONResource('[{"id":"note","name":"A localized note 🌙"}]'));
    Result.Resources.Define(NyxResourceRef('notes'), NyxLocale('invalid-note'),
      NyxJSONResource('[{"id":"note","name":true}]'));
    NyxResourceCollections(Result.Collections).Define(NyxCollection('notes'),
      NyxResourceRows(NyxResourceRef('notes')).Identity(NyxResourcePath.Field('id'))
        .Text(NyxTextField('name')));
    LNotes := NewNyxPanel('private-notes');
    LNotes.Add(NewNyxTable('notes-table').Configure.Height(100).Done
      .Binds.Collection(NyxCollectionView(NyxCollection('notes'))
        .Column(NyxTextField('name'), 'Private note', cmEditable).Scoped(csInstance)).Done);
    Result.AddComponent(LNotes);
    LCard := NewNyxPanel('team-card');
    LCard.Configure.Padding(12).Gap(8).Surface(True).Done;
    LCard.Add(NewNyxHeading('card-title').WithText('Shared team, private notes'));
    LCard.Add(NewNyxTable('team-table').Configure.Height(100).Done
      .Binds.Collection(NyxCollectionView(NyxCollection('team'))
        .Column(NyxTextField('name'), 'Shared name', cmEditable)).Done);
    LInstance := NewNyxComponent('notes-instance');
    LInstance.Reference := NyxComponent('private-notes');
    LCard.Add(LInstance);
    Result.AddComponent(LCard);
    Result.Canvases.Define(NyxCanvasRef('Team cards'), NewNyxCanvasDefinition
      .WithCamera(NyxCanvasCamera(600, 760, NyxCanvasPoint(160, 365), 1))
      .WithLayer(NyxCanvasLayer('Work'))
      .WithItem(Item(0), NyxReusableRoot('team-card'),
        NyxCanvasPlacement(NyxCanvasLayer('Work'), NyxCanvasBounds(0, 0, 320, 350)))
      .WithItem(Item(1), NyxReusableRoot('team-card'),
        NyxCanvasPlacement(NyxCanvasLayer('Work'), NyxCanvasBounds(0, 380, 320, 350))));
    LCanvas := NewNyxCanvas('workspace').WithScene(NyxCanvasRef('Team cards'));
    LCanvas.Configure.Height(760).HeightSizing(nsAutomatic).Flex(0).MinimumHeight(360).Done;
    LPage := NewNyxPage('home');
    LPage.Add(LCanvas);
    Result.AddPage(LPage);
    LPage := NewNyxPage('away');
    LPage.Add(NewNyxHeading('away-title').WithText('Another page'));
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function View(AApplication, AItem: Integer; const AID: TNyxText): INyxCollectionView;
begin
  Result := GCanvases[AApplication].RendererFor(Item(AItem)).CollectionView(AID);
end;

function Painted(AApplication, AItem: Integer; const AID: TNyxText): TNyxText;
{$ifdef PAS2JS}
var
  LInput: TJSHTMLInputElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(GCanvases[AApplication].RendererFor(Item(AItem))
    .ElementFor(AID).querySelector('input[data-nyx-column="0"]'));
  Result := LInput.value;
  {$else}
  Result := TNyxText(RawByteString(TStringGrid(GCanvases[AApplication]
    .RendererFor(Item(AItem)).ControlFor(AID)).Cells[0, 1]));
  {$endif}
end;

{ Enter the actual mounted Table's edit boundary. Browser dispatch uses its DOM
  change handler; native dispatch uses the real grid's entry-validation callback.
  Neither path claims trusted OS keyboard/touch input or writes stores directly. }
procedure Edit(AItem: Integer; const AID, AText: TNyxText);
var
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$else}
  LGrid: TStringGrid;
  LDraft: String;
  {$endif}
begin
  {$ifdef PAS2JS}
  LInput := TJSHTMLInputElement(GCanvases[0].RendererFor(Item(AItem)).ElementFor(AID)
    .querySelector('input[data-nyx-column="0"]'));
  LInput.value := AText;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LGrid := TStringGrid(GCanvases[0].RendererFor(Item(AItem)).ControlFor(AID));
  LDraft := UTF8Encode(AText);
  LGrid.OnValidateEntry(LGrid, 0, 1, LGrid.Cells[0, 1], LDraft);
  LGrid.Cells[0, 1] := LDraft;
  {$endif}
end;

procedure Attach(AApplication: Integer);
begin
  GCanvases[AApplication] := GApplications[AApplication].View.CanvasView('workspace') as TTestCanvas;
end;

procedure Step;
var
  LRuntime: INyxCanvasRuntimeView;
  LRefused: Boolean;
begin

  if Clock - GStarted > 30000 then
  begin
    raise Exception.Create('Canvas lifecycle deadline at stage ' + IntToStr(GStage));
  end;
  case GStage of
    0:
      begin

        if (GCanvases[0].MountedCount <> 2) or (GCanvases[1].MountedCount <> 2) then
        begin
          Exit;
        end;
        GContext := GApplications[0].Context;
        GCapturedScopes := NyxApplicationCollectionContext(GContext);
        Check(GCapturedScopes <> nil, 'built-in application exposes its independent collection scopes');
        Check(Supports(GCanvases[0], INyxCanvasRuntimeView, LRuntime),
          'mounted canvas exposes its revocable fragment behavior');
        GFragmentContext := LRuntime.BehaviorFor(Item(0)).Context;
        GShared := GApplications[0].Collections.Collection(NyxCollection('team'));
        GPrivate[0] := View(0, 0, CPrivateTable).Store;
        GPrivate[1] := View(0, 1, CPrivateTable).Store;
        Check(GPrivate[0] <> GPrivate[1], 'identical nested reusable IDs have distinct private stores');
        Check((View(0, 0, 'team-table').Store = GShared) and
          (View(0, 1, 'team-table').Store = GShared),
          'both canvas fragments use the live application store');
        Check(View(1, 0, CPrivateTable).Store <> GPrivate[0],
          'independent applications never share a private store');
        Edit(0, CPrivateTable, CEditedNote);
        Edit(0, 'team-table', 'Ada from the first card');
        Inc(GStage);
      end;
    1:
      begin
        Check(Painted(0, 0, CPrivateTable) = CEditedNote, 'first private edit is painted exactly');
        Check(Painted(0, 1, CPrivateTable) = 'Your private note', 'second private card is unchanged');
        Check(Painted(0, 1, 'team-table') = 'Ada from the first card',
          'shared edit paints through the sibling fragment');
        Check((Painted(1, 0, CPrivateTable) = 'Your private note') and
          (Painted(1, 0, 'team-table') = 'Ada'), 'peer application retains both independent datasets');
        Check(GApplications[0].Collections.Collection(NyxCollection('notes')).Snapshot
          .ItemAt(0).GetValue(NyxTextField('name')) = TNyxText('Your private note'),
          'private editing does not mutate the application default-scope store');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime edits leave authored defaults exact');
        GCanvases[0] := nil;
        GApplications[0].ShowPage('away');
        GApplications[0].ShowPage('home');
        Attach(0);
        Inc(GStage);
      end;
    2:
      begin

        if GCanvases[0].MountedCount <> 2 then
        begin
          Exit;
        end;
        Check(View(0, 0, CPrivateTable).Store = GPrivate[0],
          'navigation remount preserves the first logical private store');
        Check(View(0, 1, CPrivateTable).Store = GPrivate[1],
          'navigation remount preserves the sibling logical private store');
        Check(Painted(0, 0, CPrivateTable) = CEditedNote,
          'remounted private control paints its accepted Unicode edit');
        Check(Painted(0, 1, 'team-table') = 'Ada from the first card',
          'application data remains connected after navigation');
        Check(TNyxCodec.Encode(GDocument) = GBefore, 'navigation does not write runtime values into the document');
        Check(not GFragmentContext.Active, 'old fragment authority retires despite persistent data');
        LRefused := False;
        try
          NyxApplicationCollectionContext(GFragmentContext);
        except
          on ENyxApplicationContext do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'retired fragment cannot reacquire its persistent collection scopes');
        GResourceRevision0 := GPrivate[0].Snapshot.Revision;
        GResourceRevision1 := GPrivate[1].Snapshot.Revision;
        GApplications[0].Resources.Localize(NyxLocale('en-US'), NyxDefaultLocale);
        Inc(GStage);
      end;
    3:
      begin
        Check((Painted(0, 0, CPrivateTable) = CLocalizedNote) and
          (Painted(0, 1, CPrivateTable) = CLocalizedNote),
          'one resource frame updates both real namespaced controls');
        Check((GPrivate[0].Snapshot.Revision = GResourceRevision0 + 1) and
          (GPrivate[1].Snapshot.Revision = GResourceRevision1 + 1),
          'resource coordination publishes each private store exactly once');
        Check(Painted(1, 0, CPrivateTable) = 'Your private note',
          'localized resource publication retains the independent application');
        GResourceRevision0 := GPrivate[0].Snapshot.Revision;
        GResourceRevision1 := GPrivate[1].Snapshot.Revision;
        LRefused := False;
        try
          GApplications[0].Resources.Localize(NyxLocale('invalid-note'), NyxDefaultLocale);
        except
          on Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'invalid resource row type refuses the entire application frame');
        Check((GPrivate[0].Snapshot.Revision = GResourceRevision0) and
          (GPrivate[1].Snapshot.Revision = GResourceRevision1) and
          (GApplications[0].Resources.Context.Locale.Name = 'en-US'),
          'failed resource admission preserves both private stores and accepted locale');
        GCanvases[0] := nil;
        GApplications[0].ShowPage('away');
        GApplications[0].Resources.Localize(NyxDefaultLocale, NyxDefaultLocale);
        Check((GPrivate[0].Snapshot.Revision = GResourceRevision0 + 1) and
          (GPrivate[1].Snapshot.Revision = GResourceRevision1 + 1),
          'offscreen persistent scopes join the next resource publication once');
        GApplications[0].ShowPage('home');
        Attach(0);
        Inc(GStage);
      end;
    4:
      begin

        if GCanvases[0].MountedCount <> 2 then
        begin
          Exit;
        end;
        Check((View(0, 0, CPrivateTable).Store = GPrivate[0]) and
          (View(0, 1, CPrivateTable).Store = GPrivate[1]),
          'resource changes while unmounted keep both logical store identities');
        Check((Painted(0, 0, CPrivateTable) = 'Your private note') and
          (Painted(0, 1, CPrivateTable) = 'Your private note'),
          'remount paints the resource values admitted while offscreen');
        Edit(0, CPrivateTable, CEditedNote);
        Inc(GStage);
      end;
    5:
      begin
        Check(Painted(0, 0, CPrivateTable) = CEditedNote,
          'the remounted resource-backed Table remains editable');
        GResourceRevision0 := GPrivate[0].Snapshot.Revision;
        GApplications[0].Resources.Reload;
        Check(GPrivate[0].Snapshot.Revision = GResourceRevision0,
          'unchanged embedded resource reload preserves the later private edit');
        Check(Painted(0, 1, 'team-table') = 'Ada from the first card',
          'resource publication never resets unrelated shared application data');
        Check(TNyxCodec.Encode(GDocument) = GBefore,
          'editing, resources and repeated navigation keep the saved document exact');
        GDone := True;
      end;
  end;
end;

procedure Retire;
var
  LIndex: Integer;
  LRefused: Boolean;
  {$ifndef PAS2JS}
  LScheduler: INyxScheduler;
  LMonitor: INyxSchedulerMonitor;
  LUntil: Double;
  {$endif}
begin
  for LIndex := 0 to 1 do
  begin
    GCanvases[LIndex] := nil;
    {$ifndef PAS2JS}
    LScheduler := nil;

    if GApplications[LIndex] <> nil then
    begin
      LScheduler := GApplications[LIndex].View.Events.Scheduler;
    end;
    {$endif}
    FreeAndNil(GApplications[LIndex]);
    {$ifndef PAS2JS}

    if LScheduler <> nil then
    begin
      LMonitor := LScheduler as INyxSchedulerMonitor;
      LScheduler.Shutdown;
      LUntil := Clock + 5000;
      while LMonitor.WorkerLoad.ActiveWorkers > 0 do
      begin
        Application.ProcessMessages;
        CheckSynchronize;
        Sleep(1);

        if Clock > LUntil then
        begin
          raise Exception.Create('Canvas lifecycle workers did not join');
        end;
      end;
    end;
    {$endif}
  end;
  GDocument.Free;
  GDocument := nil;

  if GDone and (GContext <> nil) then
  begin
    Check(not GContext.Active, 'application disposal revokes the retained context');
    LRefused := False;
    try
      NyxApplicationCollectionContext(GContext);
    except
      on ENyxApplicationContext do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired context cannot reacquire collection scope authority');
    Check(GPrivate[0].Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = CEditedNote,
      'independently retained private data outlives application and document');
    Check(GCapturedScopes.Collections.Collection(NyxCollection('team')) = GShared,
      'captured scope resolver remains independent of its former raw owner');
  end;
  GContext := nil;
  GFragmentContext := nil;
  GCapturedScopes := nil;
  GPrivate[0] := nil;
  GPrivate[1] := nil;
  GShared := nil;
end;

procedure InitializeApplications;
var
  LIndex: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$endif}
begin
  SetLength(GCanvases, 2);
  SetLength(GPrivate, 2);
  GDocument := BuildDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  for LIndex := 0 to 1 do
  begin
    GApplications[LIndex] := TTestApplication.Create;
    GApplications[LIndex].ConfigureResources(NyxApplicationResourceOptions.Loading(nrlOnDemand));
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('main'));
    document.body.appendChild(LHost);
    GApplications[LIndex].Run(GDocument, LHost);
    {$else}
    GApplications[LIndex].Mount(GDocument);
    GApplications[LIndex].Window.ClientWidth := StrToIntDef(ParamStr(1), 1280);
    GApplications[LIndex].Window.ClientHeight := 900;
    GApplications[LIndex].Window.Show;
    {$endif}
    Attach(LIndex);
  end;
  GStarted := Clock;
end;

{$ifdef PAS2JS}
procedure Drive;
begin
  try
    Step;

    if not GDone then
    begin
      window.setTimeout(@Drive, 5);
      Exit;
    end;
    Retire;
    document.body.setAttribute('data-canvas-lifecycle', 'passed');
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' canvas collection lifecycle checks';
  except
    on LError: Exception do
    begin
      document.body.setAttribute('data-canvas-lifecycle', 'failed');
      document.body.textContent := LError.Message + ' / stage ' + IntToStr(GStage);
      Retire;
    end;
    else
    begin
      document.body.setAttribute('data-canvas-lifecycle', 'failed');
      document.body.textContent := String(JSExceptValue) + ' / stage ' + IntToStr(GStage);
      Retire;
    end;
  end;
end;
{$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    InitializeApplications;
    {$ifdef PAS2JS}Drive;{$else}
    while not GDone do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
      Step;
      Sleep(5);
    end;
    Retire;
    WriteLn('PASS ', GChecks, ' canvas collection lifecycle checks');
    {$endif}
  except
    on LError: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-lifecycle', 'failed');
      document.body.textContent := LError.Message + ' / initialization';
      {$else}
      WriteLn('FAIL ', LError.Message, ' / stage ', GStage);
      Retire;
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
