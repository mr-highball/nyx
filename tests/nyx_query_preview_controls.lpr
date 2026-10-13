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
program nyx_query_preview_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.types, nyx.behavior, nyx.state, nyx.collections, nyx.collections.query,
  nyx.collections.query.preview, nyx.collections.query.editor,
  nyx.collections.view.types, nyx.studio.projects, nyx.studio.session,
  nyx.studio.view, nyx.studio.collections, nyx.studio.inspector,
  nyx.studio.authoring,
  nyx.menu.editor,
  nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Graphics, IntfGraphics,
    FPWritePNG, nyx.render.lcl, nyx.studio.lcl{$endif};

const
  CEditor = 'inspector-collection-query';

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer; THost = TJSHTMLElement;
  {$else}TRenderer = TNyxLCLRenderer; THost = TForm;{$endif}
  TReview = class
  private
    FSession: TNyxStudioSession;
    FState: TNyxStudioViewState;
    FRenderer: TRenderer;
    FHost: THost;
    FShell: TNyxDocument;
    FBefore: TNyxText;
    FUndo: Boolean;
    FRedo: Boolean;
    FFirst: TNyxText;
    FSecond: TNyxText;
    FRefreshPending: Boolean;
    FEvents: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Refresh;
    procedure Change(const AValue: TNyxText);
    procedure ChangeField(AField: TNyxQueryEditorField; const AValue: TNyxText);
    procedure Preview;
    function Pair: TNyxText;
  public
    constructor Create(AInteger: Boolean = False);
    destructor Destroy; override;
    procedure Run;
    procedure RunMalformed;
  end;
  {$ifndef PAS2JS}TControlAccess = class(TControl);{$endif}

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure LibraryCases;
var
  LKey: TNyxCollectionRef;
  LSchema: TNyxCollectionSchema;
  LStore: INyxCollection;
  LSource: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LQuery: TNyxCollectionQuery;
  LBefore: TNyxText;
  LReport: TNyxQueryPreview;
  LRefused: Boolean;
  LIndex: Integer;
  LUnicode: TNyxText;
begin
  LKey := NyxCollection('preview-cases');
  LSchema := NyxCollectionSchema.Text(NyxTextField('title'), '')
    .Text(NyxTextField('parent'), '').Integer(NyxIntegerField('points'), 0)
    .Boolean(NyxBooleanField('done'), False).Number(NyxNumberField('estimate'), 0);
  LUnicode := TNyxText('Notes / 🌙') + #0 + 'exact';
  LStore := NewNyxCollection(LKey, LSchema, [
    NyxCollectionItem(NyxItem(LKey, 'root')).WithValue(NyxTextField('title'), 'Root')
      .WithValue(NyxIntegerField('points'), 2),
    NyxCollectionItem(NyxItem(LKey, 'child')).WithValue(NyxTextField('title'), LUnicode)
      .WithValue(NyxTextField('parent'), 'root').WithValue(NyxIntegerField('points'), 3)
      .WithValue(NyxBooleanField('done'), True).WithValue(NyxNumberField('estimate'), 1.25),
    NyxCollectionItem(NyxItem(LKey, 'last')).WithValue(NyxTextField('title'), 'Last')
      .WithValue(NyxIntegerField('points'), 3)]);
  LSource := LStore.Snapshot;
  LSpec := NyxCollectionView(LKey).Column(NyxTextField('title'), 'Title')
    .Column(NyxIntegerField('points'), 'Points').Column(NyxBooleanField('done'), 'Done')
    .Column(NyxNumberField('estimate'), 'Estimate');
  LQuery := NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('points')).AtLeast(3))
    .OrderBy(NyxIntegerField('points'), nsdDescending);
  LBefore := LQuery.ToData.ToJSON;
  LReport := PreviewNyxCollectionQuery(LSource, LSpec, cpTable, LQuery,
    TNyxQueryPreviewOptions.Defaults.SampleRows(1));
  Check((LReport.SourceCount = 3) and (LReport.VisibleCount = 2) and (LReport.SampleCount = 1),
    'read-only query produces complete counts and bounded sample');
  Check(Pos('1. child', LReport.Text) > 0, 'stable equal-key ordering preserves first matching source identity');
  Check(Pos('1.25', LReport.Text) > 0, 'typed numeric display preserves portable notation');
  Check((Pos('\u0000', LReport.Text) > 0) and (Pos(#0, LReport.Text) = 0),
    'NUL is visible in the sample without truncating native output');
  Check(Pos('🌙', LReport.Text) > 0, 'supplementary Unicode survives sample formatting');
  Check((LStore.Snapshot = LSource) and (LQuery.ToData.ToJSON = LBefore), 'preview leaves source and query exact');
  Check(Pos('NyxIntegerField(''points'')', LReport.Pascal) > 0, 'Pascal uses typed field contracts');
  LStore.Update(LStore.Snapshot.ItemAt(1).WithValue(NyxTextField('title'), 'Later'));
  Check(Pos('🌙', LReport.Text) > 0, 'value-owned report survives later source edits');

  LSpec := LSpec.Parent(NyxTextField('parent'));
  LReport := PreviewNyxCollectionQuery(LSource, LSpec, cpTree,
    NyxCollectionQuery.Where(NyxWhere(NyxTextField('title')).EqualTo(LUnicode)),
    TNyxQueryPreviewOptions.Defaults);
  Check((LReport.VisibleCount = 2) and LReport.Tree, 'tree query includes the required ancestor');
  Check((Pos('1. root', LReport.Text) > 0) and (Pos('2. child', LReport.Text) > 0),
    'tree preview preserves parent-before-child source semantics');
  LReport := PreviewNyxCollectionQuery(LSource, LSpec, cpTable,
    NyxCollectionQuery.Where(NyxWhere(NyxTextField('title')).EqualTo('absent')),
    TNyxQueryPreviewOptions.Defaults);
  Check((LReport.VisibleCount = 0) and (LReport.SampleCount = 0), 'empty query result is explicit');

  for LIndex := 0 to 3 do
  begin
    LRefused := False;
    try
      case LIndex of
        0: LReport := PreviewNyxCollectionQuery(LSource, LSpec, cpTable, LQuery,
          TNyxQueryPreviewOptions.Defaults.MaximumSourceRows(2));
        1: LReport := PreviewNyxCollectionQuery(LSource,
          NyxCollectionView(LKey).Column(NyxIntegerField('missing'), 'Missing'), cpTable,
          NyxCollectionQuery.Where(NyxWhere(NyxTextField('title')).EqualTo('absent')),
          TNyxQueryPreviewOptions.Defaults);
        2: LReport := PreviewNyxCollectionQuery(LSource, LSpec, cpTable, LQuery,
          TNyxQueryPreviewOptions.Defaults.MaximumTextScalars(256));
        3: LReport := PreviewNyxCollectionQuery(nil, LSpec, cpTable, LQuery,
          TNyxQueryPreviewOptions.Defaults);
      end;
    except
      on LException: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'bounded invalid preview refuses instead of returning partial output');
  end;
  LStore.Update(LStore.Snapshot.ItemAt(0).WithValue(NyxTextField('parent'), 'child'));
  LRefused := False;
  try
    LReport := PreviewNyxCollectionQuery(LStore.Snapshot, LSpec, cpTree, LQuery,
      TNyxQueryPreviewOptions.Defaults);
  except
    on LException: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'cyclic hierarchy is rejected by the same tree query engine');
  LStore := nil;
  LSource := nil;
  Check(Pos('NyxTextField(''title'')', LReport.Pascal) > 0,
    'last admitted value report survives later failures and source disposal');
end;

constructor TReview.Create(AInteger: Boolean);
var
  LDocument: TNyxDocument;
  LSource: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
begin
  inherited Create;
  LDocument := BuildNyxDocument;
  try
    { Exact semantic seed supplies controls/data. This explicit local candidate
      adds a typed saved filter before creating the independent review session. }
    LSource := LDocument.Collections.Snapshot(NyxCollection('measured'));
    FFirst := LSource.ItemAt(0).GetValue(NyxTextField('caption'));
    FSecond := LSource.ItemAt(1).GetValue(NyxTextField('caption'));
    LSpec := LDocument.Find('measured-table').CollectionView.Query(
      NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption')).EqualTo(FFirst)));

    if AInteger then
    begin
      LSpec := LDocument.Find('measured-table').CollectionView.Query(
        NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('priority')).AtLeast(0)));
    end;
    LDocument.Find('measured-table').SetCollectionView(LSpec);
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
  finally
    LDocument.Free;
  end;
  FSession.Activate('measured-view');
  FSession.Select('measured-table');
  FSession.SetSourceDraft(FSession.Source + #10 + '{ unfinished user source }');
  FBefore := Pair;
  FUndo := FSession.CanUndo;
  FRedo := FSession.CanRedo;
  FState := DefaultNyxStudioViewState;
  FState.BindingsVisible := True;
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:100vh;overflow:auto;max-width:720px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(10, 10, 740, 900);
  FHost.Show;
  {$endif}
  Refresh;
end;

destructor TReview.Destroy;
begin
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}FHost.remove;{$else}FHost.Free;{$endif}
  inherited Destroy;
end;

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
  FState.QueryEditorDraft.Capture(CEditor, FRenderer.Root);
  LShell := BuildNyxStudioView(FSession, FState);
  try
    FState.QueryEditorDraft.Restore(LShell.Pages[0]);
    RestoreNyxStudioQueryPreview(FState.QueryPreview, FSession, LShell.Pages[0]);
    FRenderer.Render(LShell, LShell.Find('studio-right'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
var
  LPreview: TNyxStudioQueryPreview;
begin
  Inc(FEvents);

  if RouteNyxStudioQueryPreview(FSession, AEvent,
    FRenderer.Root, Default(TNyxStudioPendingDesign), LPreview) then
  begin
    FState.QueryPreview := LPreview;
    FRefreshPending := True;
  end;
end;

procedure TReview.Change(const AValue: TNyxText);
begin
  ChangeField(nqfValue, AValue);
end;

procedure TReview.ChangeField(AField: TNyxQueryEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'query value is an actual target input');
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
    TEdit(LInput).OnChange(LInput);
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue, 'actual input updates the draft value');
end;

procedure TReview.Preview;
begin
  {$ifdef PAS2JS}FRenderer.ElementFor(NyxQueryEditorPreviewID(CEditor)).click;
  {$else}TControlAccess(FRenderer.ControlFor(NyxQueryEditorPreviewID(CEditor))).Click;{$endif}
  Check(FRefreshPending and FState.QueryPreview.Defined,
    'actual preview click captures a presentation receipt / events ' + TNyxText(IntToStr(FEvents)) +
    ' / error ' + FRenderer.LastBindingError);

  if FRefreshPending then
  begin
    FRefreshPending := False;
    Refresh;
    Check(FState.QueryPreview.Defined, 'receipt retains its exact context during shell replacement');
  end;
  Check((Pair = FBefore) and (FSession.CanUndo = FUndo) and (FSession.CanRedo = FRedo),
    'preview preserves accepted pair, unfinished draft/base and history availability');
end;

procedure TReview.Run;
var
  LText: TNyxText;
  LButton: TNyxNode;
  LOutput: TNyxNode;
  LOutputID: TNyxText;
  LPreview: TNyxStudioQueryPreview;
  LRefused: Boolean;
begin
  Change(FSecond);
  Preview;
  LOutput := FRenderer.Root.Find(NyxQueryEditorPreviewOutputID(CEditor));
  LOutputID := LOutput.ID;
  Check((Pos('1 of 3 rows', LOutput.Prop('value')) > 0) and
    (Pos(TNyxCodegen.TextLiteral(FSecond), LOutput.Prop('value')) > 0),
    'unapplied query draft produces its own visible preview and typed Pascal / ' + LOutput.Prop('value'));
  {$ifdef PAS2JS}LText := TJSHTMLTextAreaElement(FRenderer.InputFor(LOutput.ID)).value;
  {$else}LText := TNyxText(TMemo(FRenderer.InputFor(LOutput.ID)).Text);{$endif}
  Check(Pos('1 of 3 rows', LText) > 0, 'actual target memo renders the report');
  Check(FSession.Selected.CollectionView.QueryPolicy.Filter.Expected.TextValue = FFirst,
    'unapplied preview does not replace the accepted filter');
  Refresh;
  Check(Pos(FSecond, FRenderer.Root.Find(LOutputID).Prop('value')) > 0,
    'copied report survives unrelated shell replacement');
  LButton := TNyxNode.Create(nkButton, NyxQueryEditorPreviewID(CEditor));
  try
    LRefused := False;
    try
      CaptureNyxStudioQueryPreview(FSession, LButton, FRenderer.Root,
        Default(TNyxStudioPendingDesign), LPreview);
    except
      on LException: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'same-ID detached preview button cannot impersonate mounted chrome');
  finally
    LButton.Free;
  end;
  FState.QueryPreview.Context := FSession.CommandContext;
  FSession.Select('measured-list');
  Refresh;
  Check(not FState.QueryPreview.Defined, 'selection change retires the old preview receipt');
  FSession.Select('measured-table');
  Refresh;
  Change(FSecond);
  Preview;
  {$ifdef PAS2JS}
  FRenderer.ElementFor(NyxQueryEditorPreviewID(CEditor)).scrollIntoView;
  {$endif}
end;

procedure TReview.RunMalformed;
begin
  Change('-');
  Preview;
  Check(Pos('Preview refused', FRenderer.Root.Find(NyxQueryEditorPreviewOutputID(CEditor)).Prop('value')) > 0,
    'malformed integer draft displays a refusal without partial results');
  Check(FRenderer.Root.Find(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue))
    .Prop('value') = '-', 'refused preview retains the unfinished form value');
  Change('0');
  Preview;
  Check(Pos('3 of 3 rows', FRenderer.Root.Find(NyxQueryEditorPreviewOutputID(CEditor)).Prop('value')) > 0,
    'corrected numeric draft previews successfully without applying');
end;

{$ifndef PAS2JS}
procedure OrdinaryNative;
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LSpec: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LSource: INyxCollectionSnapshot;
  LValue: TNyxText;
  LStarted: QWord;
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LOutput: TControl;
  LParent: TWinControl;

  procedure Ready;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 60000 then
      begin
        raise Exception.Create('Ordinary native preview did not settle');
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceBusy;
  end;

  procedure Button(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
  end;

begin
  LForm := TForm.CreateNew(nil);
  LForm.SetBounds(10, 10, 1100, 850);
  LStudio := TNyxNativeStudio.Create(LForm, '');
  try
    LDocument := BuildNyxDocument;
    try
      LSource := LDocument.Collections.Snapshot(NyxCollection('measured'));
      LValue := LSource.ItemAt(1).GetValue(NyxTextField('caption'));
      LSpec := LDocument.Find('measured-table').CollectionView.Query(
        NyxCollectionQuery.Where(NyxWhere(NyxTextField('caption')).EqualTo('absent')));
      LDocument.Find('measured-table').SetCollectionView(LSpec);
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument)));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Activate('measured-view');
    LStudio.Session.Select('measured-table');
    LStudio.Run;
    LForm.Show;
    Ready;
    Button(NyxStudioBindingsToggleID);
    TEdit(LStudio.ShellView.InputFor(NyxQueryEditorFieldID(
      NyxQueryEditorPredicateID(CEditor), nqfValue))).Text := LValue;
    TEdit(LStudio.ShellView.InputFor(NyxQueryEditorFieldID(
      NyxQueryEditorPredicateID(CEditor), nqfValue))).OnChange(
      LStudio.ShellView.InputFor(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)));
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button(NyxQueryEditorPreviewID(CEditor));
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'ordinary native controller keeps the exact pair');
    Check(Pos('1 of 3 rows', TNyxText(TMemo(LStudio.ShellView.InputFor(
      NyxQueryEditorPreviewOutputID(CEditor))).Text)) > 0, 'ordinary native controller renders the draft preview');

    if ParamCount = 1 then
    begin
      { Print capture is diagnostic. Reveal the inspected report as a person
        scrolling this Inspector would, without changing document/presentation. }
      LOutput := LStudio.ShellView.InputFor(NyxQueryEditorPreviewOutputID(CEditor));
      LParent := LOutput.Parent;
      while LParent <> nil do
      begin

        if LParent is TScrollBox then
        begin
          TScrollBox(LParent).ScrollInView(LOutput);
        end;
        LParent := LParent.Parent;
      end;
      Application.ProcessMessages;
      ForceDirectories(ParamStr(1));
      LBitmap := LForm.GetFormImage;
      LImage := nil;
      LWriter := nil;
      try
        LImage := LBitmap.CreateIntfImage;
        LWriter := TFPWriterPNG.Create;
        LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + 'native-query-preview.png', LWriter);
      finally
        LWriter.Free;
        LImage.Free;
        LBitmap.Free;
      end;
    end;
    Button(NyxInspectorEventsID);
    Button(NyxInspectorPropertiesID);
    Check(Pos('1 of 3 rows', TNyxText(TMemo(LStudio.ShellView.InputFor(
      NyxQueryEditorPreviewOutputID(CEditor))).Text)) > 0, 'ordinary native tab parking retains the copied report');
  finally
    LStudio.Free;
    LForm.Free;
  end;
end;
{$endif}

var
  LReview: TReview;
  LMalformed: TReview;
{$ifdef PAS2JS}
  GWaits: Integer;

procedure AwaitCapture;
begin
  Inc(GWaits);

  if document.body.getAttribute('data-capture-observed') = 'query-preview' then
  begin
    FreeAndNil(LReview);
    document.body.setAttribute('data-query-preview', 'passed');
    document.body.setAttribute('data-query-preview-checks', IntToStr(GChecks));
    document.body.setAttribute('data-query-preview-disposed', 'true');
  end
  else if GWaits > 1200 then
  begin
    FreeAndNil(LReview);
    document.body.textContent := 'FAIL Preview capture was not observed';
    document.body.setAttribute('data-query-preview', 'failed');
  end
  else
  begin
    window.setTimeout(@AwaitCapture, 25);
  end;
end;
{$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    LibraryCases;
    LMalformed := TReview.Create(True);
    try
      LMalformed.RunMalformed;
    finally
      FreeAndNil(LMalformed);
    end;
    LReview := TReview.Create;
    {$ifdef PAS2JS}
    LReview.Run;
    document.body.setAttribute('data-capture-checkpoint', 'query-preview');
    AwaitCapture;
    {$else}
    try
      LReview.Run;
    finally
      LReview.Free;
    end;
    OrdinaryNative;
    WriteLn('PASS ', GChecks, ' query preview / actual controls checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      FreeAndNil(LReview);
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-query-preview', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
