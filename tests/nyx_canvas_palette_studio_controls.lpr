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
program nyx_canvas_palette_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.state, nyx.canvas.palette, nyx.canvas.types,
  nyx.canvas.declarations, nyx.canvas.workspace, nyx.disclosure,
  nyx.studio.projects, nyx.studio.sourcejobs, nyx.studio.menu
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser, nyx.render.browser, nyx.canvas.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics,
    nyx.studio.lcl, nyx.render.lcl, nyx.canvas.lcl{$endif};

type
  {$ifndef PAS2JS}
  { Expose the inherited real widget Click without extending its borrowed life. }
  TControlInput = class(TControl);
  { Owned fixture receiver outlives the editor and its revoked queued turns.
    It prevents a blocking Lazarus exception dialog and retains exact failures. }
  TFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}
  { Borrowed browser DOM element or LCL control for identity checks only. Studio
    owns its target view; no input pointer is dereferenced after paired remount. }
  TInputFace = {$ifdef PAS2JS}TJSHTMLElement{$else}TControl{$endif};

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GBase: TNyxProjectPair;
  GFirst: TNyxProjectPair;
  GSecond: TNyxProjectPair;
  GPending: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GDone: Boolean;
  GStarted: Double;
  GSearchFace: TInputFace;
  {$ifndef PAS2JS}
  GForm: TForm;
  GFailure: TFailure;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Actual Studio canvas palette: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

function Node(const AID: TNyxText): TNyxNode;
var
  LRoot: TNyxNode;
begin
  LRoot := GStudio.ShellView.RootFor(AID);
  Result := nil;

  if LRoot <> nil then
  begin
    Result := LRoot.Find(AID);
  end;
end;

procedure Click(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  GStudio.ShellView.ElementFor(AID).click;
  {$else}
  TControlInput(GStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

function InputFace(const AID: TNyxText): TInputFace;
begin
  Result := GStudio.ShellView.InputFor(AID);
end;

procedure Text(const AField, AValue: TNyxText);
var
  LFace: TInputFace;
begin
  LFace := InputFace('inspector-canvas-content-' + AField);
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LFace).value := AValue;
  LFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  Check(LFace is TEdit, 'placement/discovery field uses an actual native edit control');
  TEdit(LFace).Text := AValue;
  {$endif}
end;

function Value(const AField: TNyxText): TNyxText;
begin
  Result := Node('inspector-canvas-content-' + AField).Prop(NyxAttributeName(atValue));
end;

procedure OpenPlacement;
var
  LDisclosure: TNyxNode;
begin
  LDisclosure := Node('inspector-canvas-content-placement');

  if not NyxDisclosureExpanded(LDisclosure) then
  begin
    Click(LDisclosure.Part(NyxPart('header')).ID);
  end;
end;

function Seed: TNyxDocument;
var
  LPage: INyxPage;
  LCard: INyxCard;
  LLabel: INyxLabel;
begin
  Result := TNyxDocument.Create;
  Result.State.SetValue(NyxTextState('greeting'), 'Welcome');
  LPage := NewNyxPage('home');
  LPage.Add(NewNyxToolCanvas('tools'));
  Result.AddPage(LPage);
  LCard := NewNyxCard('welcome-card');
  LLabel := NewNyxLabel('welcome-label');
  LLabel.Binds.Text(NyxTextState('greeting')).Done;
  LCard.Add(LLabel);
  Result.AddComponent(LCard);
end;

{$ifndef PAS2JS}
{ Export the exact UI-authored pair into this fixture's explicitly fresh owner,
  for independent compiler consumers. No machine paths enter the portable pair. }
procedure ExportPair(const AStage: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure WriteBytes(const AName, AValue: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(AValue) > 0 then
      begin
        LStream.WriteBuffer(AValue[1], Length(AValue));
      end;
    finally
      LStream.Free;
    end;
  end;

begin
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + 'exports' + PathDelim + AStage + PathDelim;
  ForceDirectories(LDirectory);
  WriteBytes('nyx.generated.view.pas', APair.Source);
  WriteBytes('design.nyx', APair.Design);
end;

procedure Capture;
var
  LBitmap: TBitmap;
  LPNG: TPortableNetworkGraphic;
begin

  if ParamCount < 2 then
  begin
    Exit;
  end;
  LBitmap := GForm.GetFormImage;
  LPNG := TPortableNetworkGraphic.Create;
  try
    LPNG.Assign(LBitmap);
    LPNG.SaveToFile(ParamStr(2));
  finally
    LPNG.Free;
    LBitmap.Free;
  end;
end;
{$endif}

procedure History(ARedo: Boolean);
begin

  if ARedo then
  begin
    Click('action-redo');
  end
  else
  begin
    Click('action-undo');
  end;
end;

procedure PendingPascal;
var
  {$ifdef PAS2JS}LCode: TJSHTMLTextAreaElement;{$else}LCode: TControl;{$endif}
begin
  {$ifdef PAS2JS}
  LCode := TJSHTMLTextAreaElement(document.querySelector('[data-nyx-id="studio-code"] textarea'));
  Check(LCode <> nil, 'source textarea is mounted before draft input');
  LCode.value := GSecond.Source + #10 + '// Unfinished application Pascal';
  LCode.dispatchEvent(TJSEvent.new('input'));
  {$else}
  LCode := GStudio.CodeView.InputFor('studio-code');
  Check(LCode is TMemo, 'actual Pascal source memo is mounted');
  TMemo(LCode).Text := GSecond.Source + #10 + '// Unfinished application Pascal';
  {$endif}
  GPending := EncodeNyxProject(GStudio.ProjectSnapshot);
  Check(GStudio.ProjectSnapshot.Source <> GStudio.ProjectSnapshot.Draft,
    'source input retains an unfinished buffer outside the accepted pair');
end;

procedure Step;
var
  LDocument: TNyxDocument;
  LOwner: TNyxControlRef;
  LScene: TNyxCanvasRef;
  LView: INyxCanvasWorkspaceView;
  LCanvas: {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif};
  LRenderer: {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif};
begin
  {$ifndef PAS2JS}
  if GFailure.Error <> '' then
  begin
    raise Exception.Create('Native callback failure: ' + GFailure.Error);
  end;
  {$endif}

  if Clock - GStarted > 120000 then
  begin
    raise Exception.Create('Deadline at stage ' + TNyxText(IntToStr(GStage)));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        LDocument := Seed;
        try
          GStudio.SourceCommands.OpenProject(NyxProjectPair(TNyxCodec.Encode(LDocument),
            TNyxCodegen.Generate(LDocument)), nprRequireMatch);
        finally
          LDocument.Free;
        end;
      end;
    1:
      begin
        GBase := GStudio.ProjectSnapshot;
        {$ifdef PAS2JS}
        GStudio.CanvasView.ElementFor('tools').click;
        {$else}
        TControlInput(GStudio.CanvasView.ControlFor('tools')).Click;
        {$endif}
      end;
    2:
      begin
        Check(Node('inspector-canvas-content-search') <> nil,
          'real designer selection opens the public canvas content palette');
        GSearchFace := InputFace('inspector-canvas-content-search');
        Text('search', 'button');
        Check(GSearchFace = InputFace('inspector-canvas-content-search'),
          'live discovery filtering retains the exact input widget');
        OpenPlacement;
        Text('item', 'Send action');
        Text('width', '320');
        Text('height', '72');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GBase),
          'palette/placement input changes no accepted source or design');
        Click('action-code');
      end;
    3:
      begin
        Check((Value('search') = 'button') and (Value('item') = 'Send action') and
          (Value('width') = '320') and NyxDisclosureExpanded(Node('inspector-canvas-content-placement')),
          'shell/source-pane refresh retains unfinished fields and their disclosure');
        {$ifndef PAS2JS}Capture;{$endif}
        Click('inspector-canvas-content-add');
      end;
    4:
      begin
        GFirst := GStudio.ProjectSnapshot;
        Check(GStudio.SourceCommands.State = nssApplied, 'ordinary source queue admits actual palette insertion');
        LDocument := TNyxCodec.Decode(GFirst.Design);
        try
          Check(TryNyxCanvasPaletteOwner(LDocument, NyxControl('tools'), LOwner),
            'saved source retains the exact Tool Canvas owner');
          LScene := LDocument.Find(LOwner.ID).CanvasScene;
          Check((LDocument.ComponentCount = 2) and
            (LDocument.Canvases.Definition(LScene).ItemCount = 1),
            'first control creates its independent reusable, scene and attachment together');
        finally
          LDocument.Free;
        end;
        Check((Pos('INyxButton', GFirst.Source) > 0) and (Value('search') = ''),
          'crafted specialized Pascal is paired with a fresh insertion form');
        Check(Pos('LSendActionButton: INyxButton;', GFirst.Source) > 0,
          'actual generated source names the user purpose with its control type');
        {$ifndef PAS2JS}ExportPair('ui-new', GFirst);{$endif}
        Text('search', 'Saved / welcome-card');
        OpenPlacement;
        Text('item', 'Welcome panel');
        Text('left', '360');
        Click('inspector-canvas-content-add');
      end;
    5:
      begin
        GSecond := GStudio.ProjectSnapshot;
        Check(GStudio.SourceCommands.State = nssApplied, 'existing reusable insertion uses the same source queue');
        LDocument := TNyxCodec.Decode(GSecond.Design);
        try
          Check(TryNyxCanvasPaletteOwner(LDocument, NyxControl('tools'), LOwner),
            'second accepted pair retains its source owner');
          LScene := LDocument.Find(LOwner.ID).CanvasScene;
          Check((LDocument.ComponentCount = 2) and
            (LDocument.Canvases.Definition(LScene).ItemCount = 2),
            'actual reuse adds an item without duplicating the authored definition');
        finally
          LDocument.Free;
        end;
        {$ifndef PAS2JS}ExportPair('ui-reuse', GSecond);{$endif}
        Click('action-preview');
      end;
    6:
      begin
        LDocument := TNyxCodec.Decode(GSecond.Design);
        try
          Check(TryNyxCanvasPaletteOwner(LDocument, NyxControl('tools'), LOwner),
            'saved Interact owner resolves through its public typed contract');
        finally
          LDocument.Free;
        end;
        LView := GStudio.CanvasView.CanvasView(LOwner.ID);
        Check(Supports(LView, {$ifdef PAS2JS}INyxBrowserCanvasWorkspaceView{$else}INyxLCLCanvasWorkspaceView{$endif}, LCanvas),
          'actual canvas exposes its target attachment without retaining Studio');
        LCanvas.Execute(nccFit);
        LRenderer := LCanvas.RendererFor(NyxCanvasItem('Welcome panel'));
        Check({$ifdef PAS2JS}LRenderer.ElementFor('welcome-label').textContent{$else}
          TLabel(LRenderer.ControlFor('welcome-label')).Caption{$endif} = 'Welcome',
          'UI-authored scene actually mounts its independent bound reusable in Interact');
        {$ifndef PAS2JS}
        Check(LRenderer.ControlFor('welcome-label').IsVisible,
          'fit exposes the actual bound reusable control within the native viewport');
        {$endif}
        LCanvas := nil;
        LView := nil;
        History(False);
      end;
    7:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GFirst),
          'one actual Undo restores the entire first source/design pair');
        History(True);
      end;
    8:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSecond),
          'one actual Redo restores the exact second pair');
        PendingPascal;
        Text('search', 'button');
        Click('inspector-canvas-content-add');
      end;
    9:
      begin
        Check((GStudio.SourceCommands.State = nssRejected) and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = GPending),
          'pending Pascal refuses actual Add with pair, buffer and base exact');
        Check(Value('search') = 'button', 'refusal preserves the unfinished palette proposal');
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

procedure Cleanup;
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  FreeAndNil(GStudio);
  {$ifndef PAS2JS}
  FreeAndNil(GForm);
  { Drain actual revoked queue turns after every borrowed editor/widget is gone. }
  for LTurn := 1 to 3 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
  end;
  Application.OnException := nil;
  FreeAndNil(GFailure);
  {$endif}
end;

procedure Drive;
begin
  try
    Step;

    if GDone then
    begin
      Cleanup;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-palette-studio', 'passed');
      document.body.setAttribute('data-canvas-palette-checks', IntToStr(GChecks));
      {$else}
      WriteLn('PASS ', GChecks, ' actual Studio canvas palette checks');
      {$endif}
      Exit;
    end;
  except
    on E: Exception do
    begin
      GDone := True;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-canvas-palette-studio', 'failed');
      document.body.setAttribute('data-canvas-palette-error', E.Message);
      document.body.setAttribute('data-canvas-palette-stage', IntToStr(GStage));
      {$else}
      WriteLn('FAIL stage ', GStage, ' / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      Cleanup;
      Exit;
    end;
  end;
  {$ifdef PAS2JS}window.setTimeout(@Drive, 25);{$endif}
end;

begin
  {$ifndef PAS2JS}

  if (ParamCount < 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned Studio project directory');
  end;
  Application.Initialize;
  GFailure := TFailure.Create;
  Application.OnException := GFailure.Failed;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(30, 30, 1280, 900);
  GForm.Show;
  GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
  GStudio.Run;
  {$else}
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  {$endif}
  GStarted := Clock;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  while not GDone do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
    Drive;
    Sleep(25);
  end;
  {$endif}
end.
