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
program nyx_content_settling_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Forms, Controls, StdCtrls, Classes,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.content,
  nyx.state, nyx.presentations, nyx.containers, nyx.responsive, nyx.editing
  {$ifdef PAS2JS}, JS, Web, nyx.editing.browser, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  TFace = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TFace = TControl;
  {$endif}

  { Typed consumer instrumentation, not MCP-authored application content. Two
    independent mounts retain their runtime stores across actual UI queue turns.
    Public Render and the managed presentation capability are the only routes
    into structural publication; no private stage or layout method is called. }
  TReview = class
  private
    FRenderer: TRenderer;
    FFault: TRenderer;
    FHost: THost;
    FFaultHost: THost;
    FStore: TNyxState;
    FFaultStore: TNyxState;
    FLease: INyxPresentationView;
    FFaultLease: INyxPresentationView;
    FAccepted: TNyxNode;
    FFaultAccepted: TNyxNode;
    FFaultFace: TFace;
    FStage: Integer;
    FPolls: Integer;
    FRevision: Integer;
    FFaultRevision: Integer;
    procedure Setup;
  public
    constructor Create;
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GFactoryCalls: Integer;
  GProbeCalls: Integer;
  GFeedback: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function LayerID(ALayer: Integer): TNyxText;
var
  LIndex: Integer;
begin
  Result := 'layer-1';
  for LIndex := 2 to ALayer do
  begin
    Result := NyxQualifiedID(Result, 'layer-' + IntToStr(LIndex));
  end;
end;

function BuildNested(ADepth: Integer): TNyxDocument;
var
  LPage: INyxPage;
  LWaiting: INyxColumn;
  LDetails: INyxColumn;
  LInstance: INyxComponent;
  LIndex: Integer;
  LNext: Integer;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Room for nested ideas';
    Result.State.SetValue(NyxTextState('notes'), 'An accepted idea');
    Result.Presentations.Define(NyxPresentation('expanded'), TNyxPresentationCondition.Manual);
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Width(900).Height(650).Padding(0).Done;
    Result.AddPage(LPage);
    for LIndex := 1 to ADepth do
    begin
      LWaiting := NewNyxColumn('waiting-body-' + IntToStr(LIndex));
      LWaiting.Add(NewNyxInput('waiting-notes-' + IntToStr(LIndex))
        .WithText('Idea').Binds.Value(NyxTextState('notes')).Done);
      Result.AddComponent(LWaiting);
      LDetails := NewNyxColumn('details-body-' + IntToStr(LIndex));
      LDetails.Configure.QueryContainer(NyxContainer('space ' + IntToStr(LIndex)))
        .Containment(nccWidth).Width(250).Height(400).Padding(0).Done;
      LDetails.Add(NewNyxLabel('probe-' + IntToStr(LIndex)).WithText('A nested workspace'));
      Result.AddComponent(LDetails);

      if LIndex = ADepth then
      begin
        LDetails.Add(NewNyxMemo('deep-notes').WithText('Notes')
          .Binds.Value(NyxTextState('notes')).Done);
      end
      else
      begin
        LNext := LIndex + 1;
        Result.Presentations.Define(NyxPresentation('detail ' + IntToStr(LNext)),
          TNyxPresentationCondition.Within(NyxContainer('space ' + IntToStr(LIndex)),
            TNyxViewportCondition.Any.WidthBelow(640)));
        LInstance := NewNyxComponent('layer-' + IntToStr(LNext));
        LInstance.Content.Use(NyxComponent('waiting-body-' + IntToStr(LNext)))
          .WhenPresentation(NyxPresentation('detail ' + IntToStr(LNext)))
          .Use(NyxComponent('details-body-' + IntToStr(LNext))).Done;
        LDetails.Add(LInstance);
      end;
    end;
    LInstance := NewNyxComponent('layer-1');
    LInstance.Content.Use(NyxComponent('waiting-body-1'))
      .WhenPresentation(NyxPresentation('expanded')).Use(NyxComponent('details-body-1')).Done;
    LPage.Add(LInstance);
  except
    Result.Free;
    raise;
  end;
end;

{ Controlled extension allocation feedback: the updater deliberately alternates
  an owned candidate publisher's width according to its selected descendants.
  This stresses real target allocation/cycle refusal, not ordinary CSS or a claim
  about arbitrary external stylesheet behavior. No accepted tree/store is edited. }
function BuildFeedback: TNyxDocument;
var
  LPage: INyxPage;
  LInstance: INyxComponent;
begin
  Result := BuildNested(1);
  LPage := RetainNyxControl(Result.Pages[0]) as INyxPage;
  LPage.Configure.QueryContainer(NyxContainer('feedback space')).Containment(nccWidth).Done;
  Result.Presentations.Define(NyxPresentation('feedback compact'),
    TNyxPresentationCondition.Within(NyxContainer('feedback space'),
      TNyxViewportCondition.Any.WidthBelow(640)));
  LInstance := RetainNyxControl(Result.Find('layer-1')) as INyxComponent;
  LInstance.Content.WhenPresentation(NyxPresentation('expanded')).Clear
    .WhenPresentation(NyxPresentation('feedback compact')).Use(NyxComponent('details-body-1')).Done;
  LPage.Add(NewNyxLabel('feedback-probe').WithText('Allocation feedback'));
end;

function NewHost: THost;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.createElement('div'));
  Result.style.setProperty('width', '900px');
  Result.style.setProperty('height', '700px');
  document.body.appendChild(Result);
  {$else}
  Result := TForm.Create(nil);
  Result.ClientWidth := 900;
  Result.ClientHeight := 700;
  Result.Show;
  {$endif}
end;

function RefuseMemo(ANode: TNyxNode
  {$ifndef PAS2JS}; AOwner: TComponent{$endif}): TFace;
begin
  Result := nil;

  if GFeedback then
  begin
    {$ifdef PAS2JS}Result := TJSHTMLElement(document.createElement('textarea'));
    {$else}Result := TMemo.Create(AOwner);{$endif}
    Exit;
  end;
  Inc(GFactoryCalls);
  raise Exception.Create('Controlled later-pass factory failure');
end;

procedure UpdateRefusedMemo(ANode: TNyxNode; AFace: TFace);
begin

  if GFeedback then
  begin
    {$ifdef PAS2JS}TJSHTMLTextAreaElement(AFace).value := ANode.Prop('value');
    {$else}TCustomEdit(AFace).Text := ANode.Prop('value');{$endif}
    Exit;
  end;
  raise Exception.Create('A refused candidate cannot update');
end;

function ProbeLabel(ANode: TNyxNode
  {$ifndef PAS2JS}; AOwner: TComponent{$endif}): TFace;
begin
  Inc(GProbeCalls);
  {$ifndef PAS2JS}
  { Exercise constructors that service queued native work. The accepted fault
    view must survive every hidden pass, including a pending failed request. }
  CheckSynchronize;

  if (GReview.FFaultAccepted <> nil) and (GReview.FFault.Root <> GReview.FFaultAccepted) then
  begin
    raise Exception.Create('A hidden constructor retired the accepted native mount');
  end;
  {$endif}
  {$ifdef PAS2JS}Result := TJSHTMLElement(document.createElement('div'));
  {$else}Result := TLabel.Create(AOwner);{$endif}
end;

procedure UpdateProbe(ANode: TNyxNode; AFace: TFace);
var
  LWidth: Integer;
begin
  {$ifdef PAS2JS}AFace.textContent := ANode.Prop('text');
  {$else}TLabel(AFace).Caption := ANode.Prop('text');{$endif}

  if not GFeedback or (ANode.SourceID <> 'feedback-probe') then
  begin
    Exit;
  end;
  LWidth := 250;

  if ANode.Parent.Find(NyxQualifiedID('layer-1', 'deep-notes')) <> nil then
  begin
    LWidth := 900;
  end;
  ANode.Parent.Configure.Width(LWidth).Done;
  {$ifdef PAS2JS}
  TJSHTMLElement(AFace.parentElement).style.setProperty('width', IntToStr(LWidth) + 'px');
  {$endif}
end;

constructor TReview.Create;
begin
  inherited Create;
  FRenderer := TRenderer.Create;
  FFault := TRenderer.Create;
end;

destructor TReview.Destroy;
begin
  FRenderer.Free;
  FFault.Free;
  FStore.Free;
  FFaultStore.Free;
  {$ifdef PAS2JS}

  if FHost <> nil then
  begin
    FHost.remove;
  end;

  if FFaultHost <> nil then
  begin
    FFaultHost.remove;
  end;
  {$else}
  FHost.Free;
  FFaultHost.Free;
  {$endif}
  inherited Destroy;
end;

procedure TReview.Setup;
var
  LDocument: TNyxDocument;
  LCold: TRenderer;
  LColdHost: THost;
begin
  FHost := NewHost;
  FFaultHost := NewHost;
  LDocument := BuildNested(3);
  try
    FStore := LDocument.State.Clone;
    FFaultStore := LDocument.State.Clone;
    FRenderer.Render(LDocument, LDocument.Pages[0], FHost, False, FStore);
    FFault.Render(LDocument, LDocument.Pages[0], FFaultHost, False, FFaultStore);
  finally
    LDocument.Free;
  end;
  LDocument := BuildNested(3);
  LCold := TRenderer.Create;
  LColdHost := NewHost;
  try
    LDocument.Find('layer-1').Content.WhenPresentation(NyxPresentation('expanded'))
      .Clear.Done.Use(NyxComponent('details-body-1'));
    {$ifdef PAS2JS}
    LColdHost.style.setProperty('display', 'none');
    LCold.Render(LDocument, LDocument.Pages[0], LColdHost, False);
    Check(LCold.Root.Find(NyxQualifiedID(LayerID(2), 'waiting-notes-2')) <> nil,
      'A hidden browser host retains fallback content for missing publisher boxes');
    LColdHost.style.removeProperty('display');
    {$endif}
    LCold.Render(LDocument, LDocument.Pages[0], LColdHost, False);
    Check(LCold.Root.Find(NyxQualifiedID(LayerID(3), 'deep-notes')) <> nil,
      'Cold Render returns the settled deepest recipe before any UI queue turn');
    Check(LCold.InputFor(NyxQualifiedID(LayerID(3), 'deep-notes')) <> nil,
      'The cold accepted recipe has its actual deepest physical input');
  finally
    LCold.Free;
    {$ifdef PAS2JS}LColdHost.remove;{$else}LColdHost.Free;{$endif}
    LDocument.Free;
  end;
  FLease := FRenderer.Presentations;
  FFaultLease := FFault.Presentations;
  FAccepted := FRenderer.Root;
  FFaultAccepted := FFault.Root;
  FFaultFace := FFault.InputFor(NyxQualifiedID('layer-1', 'waiting-notes-1'));
  {$ifdef PAS2JS}TJSHTMLInputElement(FFaultFace).value := 'An unfinished draft';
  {$else}TCustomEdit(FFaultFace).Text := 'An unfinished draft';{$endif}
  FRevision := FStore.Revision;
  FFaultRevision := FFaultStore.Revision;
  FFault.RegisterFactory('label', @ProbeLabel, @UpdateProbe);
  FFault.RegisterFactory('memo', @RefuseMemo, @UpdateRefusedMemo);
  FLease.Select(NyxPresentation('expanded'));
  FFaultLease.Select(NyxPresentation('expanded'));
  Check(FRenderer.Root = FAccepted, 'Queued nested work leaves the accepted root until idle');
  Check(FFault.Root = FFaultAccepted, 'Queued failure work leaves the accepted root until idle');
end;

function TReview.Step: Boolean;
var
  LDocument: TNyxDocument;
  LRejected: Boolean;
  LReason: TNyxText;
  LCalls: Integer;
begin
  Result := False;
  case FStage of
    0: Setup;
    1:
      begin

        if (FRenderer.Root = FAccepted) or (FFault.LastContentError = '') then
        begin
          Inc(FPolls);

          if FPolls > 100 then
          begin
            raise Exception.Create('Nested publication timed out / ' + FRenderer.LastContentError);
          end;
          Exit;
        end;
        Check(FRenderer.Root.Find(NyxQualifiedID(LayerID(3), 'deep-notes')) <> nil,
          'One accepted nested publication includes the deepest allocated memo');
        Check(FRenderer.Root.Find(NyxQualifiedID(LayerID(2), 'waiting-notes-2')) = nil,
          'An intermediate fallback never becomes the accepted control set');
        Check(FLease.Connected and (FRenderer.Presentations = FLease),
          'Nested publication preserves the original managed capability');
        Check(FLease.Selection.Reference.Name = 'expanded', 'Nested choice publishes after settling');
        Check(FStore.Revision = FRevision, 'Hidden nested passes do not write runtime defaults');
        Check(FFault.Root = FFaultAccepted, 'A later-pass factory failure preserves the exact root');
        Check(FFault.InputFor(NyxQualifiedID('layer-1', 'waiting-notes-1')) = FFaultFace,
          'A later-pass factory failure preserves the exact physical input');
        Check(GProbeCalls >= 3, 'Failure follows multiple real hidden construction passes');
        Check(GFactoryCalls = 1, 'A failed deep factory runs only once');
        Check(FFaultLease.Connected and not FFaultLease.Selection.Reference.Defined,
          'Later-pass refusal preserves accepted choice and capability');
        Check(FFaultStore.Revision = FFaultRevision, 'Later-pass failure retains the independent store');
        {$ifdef PAS2JS}
        Check(NyxBrowserInputText(FFaultFace) = 'An unfinished draft', 'Failure retains browser draft');
        Check(document.querySelector('[inert][data-nyx-theme]') = nil,
          'No connected hidden candidate remains after acceptance or failure');
        {$else}
        Check(TCustomEdit(FFaultFace).Text = 'An unfinished draft', 'Failure retains native draft');
        {$endif}
        FFault.Sync;
      end;
    2:
      begin
        Check(GFactoryCalls = 1, 'The failed nested observation cannot automatically retry');
        { The normal target allocation fixture introduces one publisher per
          pass. Nine levels require more than eight hidden candidates. }
        LDocument := BuildNested(9);
        LRejected := False;
        LReason := '';
        try
          LDocument.Find('layer-1').Content.WhenPresentation(NyxPresentation('expanded')).Clear
            .Done.Use(NyxComponent('details-body-1'));
          try
            FRenderer.Render(LDocument, LDocument.Pages[0], FHost, False, FStore);
          except
            on LError: Exception do
            begin
              LRejected := True;
              LReason := LError.Message;
            end;
          end;
          Check(LRejected and (Pos('eight hidden candidates', LReason) > 0),
            'Excess nested allocation refuses at the documented admission bound / ' + LReason);
          Check(FLease.Connected and (FRenderer.Presentations = FLease),
            'Bound refusal leaves the accepted nested mount/capability alive');
          Check(FRenderer.Root.Find(NyxQualifiedID(LayerID(3), 'deep-notes')) <> nil,
            'Bound refusal preserves the accepted deepest memo');
          Check(FStore.Revision = FRevision, 'Bound refusal writes no runtime defaults');
        finally
          LDocument.Free;
        end;
        LDocument := BuildFeedback;
        GFeedback := True;
        LRejected := False;
        LReason := '';
        LCalls := GProbeCalls;
        try
          try
            FFault.Render(LDocument, LDocument.Pages[0], FFaultHost, False, FFaultStore);
          except
            on LError: Exception do
            begin
              LRejected := True;
              LReason := LError.Message;
            end;
          end;
          Check(LRejected and (Pos('cycles before publication', LReason) > 0),
            'Real candidate allocation feedback refuses its cycle / ' + LReason);
          Check((GProbeCalls > LCalls) and (GProbeCalls - LCalls < 8),
            'Cycle refusal is bounded by actual candidate construction');
          Check((FFault.Root = FFaultAccepted) and FFaultLease.Connected,
            'Feedback refusal leaves the accepted flat mount alive');
          Check(FFaultStore.Revision = FFaultRevision, 'Feedback never writes application state');
        finally
          GFeedback := False;
          LDocument.Free;
        end;
        FFaultLease.Automatic;
      end;
    3:
      begin

        if FFault.LastContentError <> '' then
        begin
          Exit;
        end;
        Check(FFault.Root = FFaultAccepted, 'An explicit recovery retains the valid flat controls');
        FRenderer.Unmount;
        FFault.Unmount;
        Check(not FLease.Connected and not FFaultLease.Connected, 'Teardown retires both capabilities');
        Result := True;
      end;
  end;
  Inc(FStage);
end;

procedure Drive;
begin
  try

    if GReview.Step then
    begin
      FreeAndNil(GReview);
      WriteLn('PASS ', GChecks, ' actual nested content checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'passed');
      document.body.setAttribute('data-projection-refresh-checks', IntToStr(GChecks));
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LError.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GReview := TReview.Create;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  while GReview <> nil do
  begin
    Drive;
    Application.ProcessMessages;
    CheckSynchronize(10);
  end;
  CheckSynchronize;
  {$endif}
end.
