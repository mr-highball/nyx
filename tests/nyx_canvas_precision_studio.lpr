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
program nyx_canvas_precision_studio;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.data,
  nyx.codec, nyx.codegen, nyx.state, nyx.canvas.types, nyx.canvas.geometry,
  nyx.canvas.workspace, nyx.canvas.declarations, nyx.canvas.editor, nyx.canvas.precision,
  nyx.canvas.scene, nyx.programs.types, nyx.programs.contract, nyx.programs,
  nyx.programs.canvas, nyx.studio.projects, nyx.controls
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser, nyx.studio.browser, nyx.canvas.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, LCLType,
    nyx.application.lcl, nyx.studio.lcl, nyx.canvas.lcl
  {$endif};

type
  { The real Studio owns its source queue and Nyx shell; native storage is an
    explicitly fresh fixture directory, browser recovery/connection opt out. }
  TTestStudio = {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  {$ifndef PAS2JS}
  { Borrow installed Win32/LCL input handlers without creating controls or
    extending their ownership. Direct handler input claims no hardware event. }
  TNativeInput = class(TWinControl);
  { Fixture-owned receiver catches native host exceptions before any blocking
    dialog. It outlives the application, Studio and all revoked queued work. }
  TFailure = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  {$else}
  { Standard browser Object used as copied KeyboardEvent constructor options;
    it contains no host authority. This matched RTL needs the explicit bridge. }
  TKeyOptions = class external name 'Object'(TJSObject)
    key: String;
    bubbles: Boolean;
    cancelable: Boolean;
  end;
  { Bridge to the standard browser-owned KeyboardEvent. Synthetic input goes
    through the attached DOM listener; it is not a trusted hardware event. }
  TKeyEvent = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TKeyOptions);
  end;
  {$endif}

const
  CEditor: TNyxText = 'inspector-canvas-precision';

var
  GChecks: Integer;
  GStudio: TTestStudio;
  GDocument: TNyxDocument;
  GCompact: Boolean;
  {$ifndef PAS2JS}
  GHost: TForm;
  GFailure: TFailure;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GFailure <> nil) and (GFailure.Error <> '') then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('Canvas precision input: ' + AReason);
  end;
  Inc(GChecks);
  {$ifndef PAS2JS}
  WriteLn('PASS ', GChecks, ' ', AReason);
  Flush(Output);
  {$endif}
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Error := AError.ClassName + ': ' + AError.Message;
end;
{$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}
  Result := TJSDate.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

{$ifdef PAS2JS}
{ Allow ordinary native browser turns, without draining private schedulers. }
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 25);
end;
{$endif}

procedure Idle; {$ifdef PAS2JS}async;{$endif}
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  {$ifdef PAS2JS}
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  await(TJSPromise.resolve(TJSPromise.new(@Pause)));
  {$else}
  for LTurn := 1 to 3 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
    Sleep(10);
  end;
  {$endif}
end;

procedure WaitStudio; {$ifdef PAS2JS}async;{$endif}
var
  LStarted: Double;
begin
  LStarted := Clock;
  repeat
    {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}

    if Clock - LStarted > 120000 then
    begin
      raise Exception.Create('Studio precision source/display did not settle');
    end;
  until not GStudio.SourceBusy and not GStudio.PresentationPending;
end;

function Part(APart: TNyxCanvasPrecisionPart): TNyxText;
begin
  Result := NyxCanvasPrecisionPart(CEditor, APart).ID;
end;

procedure Click(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  GStudio.ShellView.ElementFor(AID).click;
  {$else}
  if GStudio.ShellView.Root.Find(AID) <> nil then
  begin
    TNativeInput(GStudio.ShellView.ControlFor(AID)).Click;
  end
  else
  begin
    TNativeInput(GStudio.SourceView.ControlFor(AID)).Click;
  end;
  {$endif}
end;

{$ifndef PAS2JS}
{ Borrow only a button in an ordinary visible menu belonging to this isolated
  Studio. Captions are fixed English fixture UI, never application identifiers.
  Neither the window nor any native child is adopted or freed by this search. }
function MenuButton(const ATitle, ACaption: String): TWinControl;
var
  LIndex: Integer;

  function Find(AParent: TWinControl): TWinControl;
  var
    LChildIndex: Integer;
    LChild: TControl;
  begin
    Result := nil;
    for LChildIndex := 0 to AParent.ControlCount - 1 do
    begin
      LChild := AParent.Controls[LChildIndex];

      if LChild is TWinControl then
      begin

        if LChild.Visible and LChild.Enabled and
          ((TNativeInput(LChild).Caption = ACaption) or
            (Pos(ACaption + ' ', TNativeInput(LChild).Caption) = 1)) then
        begin
          Exit(TWinControl(LChild));
        end;
        Result := Find(TWinControl(LChild));

        if Result <> nil then
        begin
          Exit;
        end;
      end;
    end;
  end;
begin
  Result := nil;
  for LIndex := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LIndex].Visible and (Screen.Forms[LIndex].Caption = ATitle) then
    begin
      Result := Find(Screen.Forms[LIndex]);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
  raise Exception.Create('Visible Studio menu button is unavailable: ' + ACaption);
end;
{$endif}

{ Compact chrome deliberately hides the desktop Pascal button. Exercise its
  real public Actions/View menu; invoking a hidden native button proves nothing. }
procedure ToggleCode; {$ifdef PAS2JS}async;{$endif}
begin

  if not GCompact then
  begin
    Click('action-code');
    {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
    Exit;
  end;
  Click('action-actions');
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  {$ifdef PAS2JS}
  TJSHTMLElement(document.querySelector('[data-node="studio-menu-view"]')).click;
  await(Idle);
  TJSHTMLElement(document.querySelector('[data-node="studio-menu-code"]')).click;
  {$else}
  TNativeInput(MenuButton('Component actions', 'View')).Click;
  Idle;
  TNativeInput(MenuButton('View', 'Pascal source')).Click;
  {$endif}
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
end;

procedure Choose(APart: TNyxCanvasPrecisionPart; AIndex: Integer);
var
  LSelect: {$ifdef PAS2JS}TJSHTMLSelectElement{$else}TComboBox{$endif};
begin
  LSelect := {$ifdef PAS2JS}TJSHTMLSelectElement{$else}TComboBox{$endif}(
    GStudio.ShellView.InputFor(Part(APart)));
  {$ifdef PAS2JS}
  LSelect.selectedIndex := AIndex;
  LSelect.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LSelect.ItemIndex := AIndex;
  LSelect.OnChange(LSelect);
  {$endif}
end;

procedure Text(APart: TNyxCanvasPrecisionPart; const AValue: TNyxText);
var
  {$ifdef PAS2JS}
  LInput: TJSHTMLInputElement;
  {$else}
  LInput: TEdit;
  {$endif}
begin
  LInput := {$ifdef PAS2JS}TJSHTMLInputElement{$else}TEdit{$endif}(
    GStudio.ShellView.InputFor(Part(APart)));
  {$ifdef PAS2JS}
  LInput.value := AValue;
  LInput.dispatchEvent(TJSEvent.new('input'));
  {$else}
  LInput.Text := AValue;
  {$endif}
end;

function Value(APart: TNyxCanvasPrecisionPart): TNyxText;
begin
  Result := {$ifdef PAS2JS}TJSHTMLInputElement{$else}TEdit{$endif}(
    GStudio.ShellView.InputFor(Part(APart))).{$ifdef PAS2JS}value{$else}Text{$endif};
end;

{$ifndef PAS2JS}
procedure ExportPair(const AStage: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure Save(const AName, AText: TNyxText);
  var
    LBytes: TNyxBytes;
    LFile: TFileStream;
  begin
    LBytes := NyxEncodeUTF8(AText);
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LFile.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LFile.Free;
    end;
  end;

begin
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + 'exports' + PathDelim +
    AStage + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('expected.nyx', APair.Design);
end;
{$endif}

{ Inspect a copied accepted snapshot on either host; the returned value owns no
  Studio document or widget. Construction is distinct from runtime execution. }
function SavedOptions: TNyxCanvasPrecisionOptions;
var
  LDocument: TNyxDocument;
begin
  LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
  try
    Result := NyxCanvasPrecisionOptionsOf(LDocument.Canvases.Definition(NyxCanvasRef('Notes')));
  finally
    LDocument.Free;
  end;
end;

procedure StudioInput; {$ifdef PAS2JS}async;{$endif}
var
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LExpected: TNyxDocument;
  LDefinition: INyxCanvasDefinition;
  {$ifdef PAS2JS}
  LSource: TJSHTMLTextAreaElement;
  {$endif}
begin
  {$ifdef PAS2JS}
  GStudio := TTestStudio.Create;
  GStudio.Run(False);
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1280, 900);

  if GCompact then
  begin
    GHost.Width := 390;
  end;
  GHost.Show;
  GStudio := TTestStudio.Create(GHost, ParamStr(1));
  GStudio.Run;
  {$endif}
  GStudio.SourceCommands.OpenProject(NyxProjectPair(TNyxCodec.Encode(GDocument),
    TNyxCodegen.Generate(GDocument)), nprRequireMatch);
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  LBefore := GStudio.ProjectSnapshot;
  {$ifdef PAS2JS}
  GStudio.CanvasView.ElementFor('board').click;
  {$else}
  TNativeInput(GStudio.CanvasView.ControlFor('board')).Click;
  {$endif}
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}

  if GCompact then
  begin
    { Compact Studio shows one panel at a time; exercise the ordinary tab before
      asking its inspector for controls that are deliberately not mounted yet. }
    Click('action-panel-inspector');
    {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  end;
  Check(GStudio.ShellView.InputFor(Part(ncppAxes)) <> nil,
    'ordinary selected Canvas exposes the public precision form');
  Choose(ncppAxes, 1);
  Choose(ncppSnapping, 1);
  Text(ncppOriginX, '10.25');
  Text(ncppUnitCaption, 'cm');
  Text(ncppUnitSize, '2.5');
  Choose(ncppGuideAction, 1);
  Text(ncppGuideName, 'Left margin');
  Text(ncppGuidePosition, '-120.5');
  Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(LBefore),
    'unfinished precision fields do not mutate accepted source/design');
  {$ifdef PAS2JS}await(ToggleCode);{$else}ToggleCode;{$endif}

  if GCompact then
  begin
    Click('action-panel-inspector');
    {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  end;
  Check((Value(ncppOriginX) = '10.25') and (Value(ncppGuidePosition) = '-120.5'),
    'source-pane refresh retains the exact unfinished precision proposal');
  Click(Part(ncppApply));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  LAfter := GStudio.ProjectSnapshot;
  LExpected := GDocument.Clone;
  try
    LDefinition := LExpected.Canvases.Definition(NyxCanvasRef('Notes'));
    LDefinition := NyxCanvasPrecision(LDefinition)
      .Rulers(NyxCanvasRulers.FromOrigin(NyxCanvasPoint(10.25, 0)).Units(2.5, 'cm'))
      .Guide(NyxCanvasGuide(NyxCanvasGuideRef('Left margin'), ncaxX, -120.5))
      .Snapping(ncgspVisible, 8).Done;
    LExpected.Canvases.Define(NyxCanvasRef('Notes'), LDefinition);
    Check((LAfter.Design = TNyxCodec.Encode(LExpected)) and
      (Pos('.Rulers(NyxCanvasRulers', LAfter.Source) > 0),
      'ordinary Apply publishes the entire expected precision design/Pascal pair');
  finally
    LExpected.Free;
  end;
  {$ifndef PAS2JS}ExportPair('applied', LAfter);{$endif}
  Click('action-undo');
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(LBefore),
    'one ordinary Undo restores the complete previous pair');
  Click('action-redo');
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(LAfter),
    'one ordinary Redo restores the complete precision pair');
  Choose(ncppGuideChoice, 1);
  Text(ncppOriginX, '12.5');
  Click(Part(ncppLoad));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check((Value(ncppGuideName) = 'Left margin') and
    (Value(ncppGuidePosition) = '-120.5') and (Value(ncppOriginX) = '12.5'),
    'ordinary Load reads saved guide fields and retains unrelated ruler proposal');
  Text(ncppOriginX, '10.25');
  Text(ncppGuidePosition, '-80.25');
  Choose(ncppGuideLocked, 0);
  Click(Part(ncppApply));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  LAfter := GStudio.ProjectSnapshot;
  Check(SavedOptions.Named(NyxCanvasGuideRef('Left margin')).IsLocked,
    'authoring can deliberately lock an existing guide');
  Check(SavedOptions.Named(NyxCanvasGuideRef('Left margin')).Position = -80.25,
    'loaded existing guide keeps its exact identity during an authored move');
  {$ifndef PAS2JS}ExportPair('moved', LAfter);{$endif}
  Choose(ncppGuideChoice, 1);
  Click(Part(ncppLoad));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Choose(ncppGuideAction, 2);
  Click(Part(ncppApply));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check(SavedOptions.Count = 0,
    'deliberate authoring removal is distinct from runtime lock guards');
  {$ifndef PAS2JS}ExportPair('removed', GStudio.ProjectSnapshot);{$endif}
  Click('action-undo');
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(LAfter),
    'one Undo restores exact removed guide, source and settings');
  Text(ncppIncrement, '-unfinished');
  Click(Part(ncppApply));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check((EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(LAfter)) and
    (Value(ncppIncrement) = '-unfinished'),
    'invalid target input preserves the whole accepted pair and unfinished fields');
  Text(ncppIncrement, '1');

  if GCompact then
  begin
    Click('action-panel-design');
    {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  end;

  if GStudio.ShellView.Root.Find('studio-split') = nil then
  begin
    {$ifdef PAS2JS}await(ToggleCode);{$else}ToggleCode;{$endif}
  end;
  { Source chrome has its own native renderer. Open the ordinary Source tab
    before asking for its textarea, regardless of prior diagnostic presentation. }
  Click('action-source-tab');
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  {$ifdef PAS2JS}
  LSource := TJSHTMLTextAreaElement(document.querySelector('textarea[data-node="studio-code"]'));
  Check(LSource <> nil, 'ordinary browser source textarea is mounted');
  LSource.value := LAfter.Source + #10 + '// Unfinished source';
  LSource.dispatchEvent(TJSEvent.new('input'));
  {$else}
  Check(GStudio.CodeView.Root <> nil, 'ordinary native source textarea is mounted');
  TMemo(GStudio.CodeView.InputFor('studio-code')).Text := LAfter.Source + #10 + '// Unfinished source';
  {$endif}
  {$ifdef PAS2JS}await(Idle);{$else}Idle;{$endif}
  LPending := GStudio.ProjectSnapshot;
  Check(LPending.Source <> LPending.Draft, 'actual source editor holds an unfinished buffer');

  if GCompact then
  begin
    Click('action-panel-inspector');
    {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  end;
  Choose(ncppAxes, 0);
  Click(Part(ncppApply));
  {$ifdef PAS2JS}await(WaitStudio);{$else}WaitStudio;{$endif}
  Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(LPending),
    'pending Pascal refuses precision publication with the entire pair/buffer/base retained');
  {$ifndef PAS2JS}ExportPair('pending', LPending);{$endif}
end;

procedure Cleanup;
{$ifndef PAS2JS}
var
  LTurn: Integer;
{$endif}
begin
  FreeAndNil(GStudio);
  FreeAndNil(GDocument);
  {$ifndef PAS2JS}
  FreeAndNil(GHost);
  for LTurn := 1 to 3 do
  begin
    CheckSynchronize;
    Application.ProcessMessages;
  end;
  Application.OnException := nil;
  FreeAndNil(GFailure);
  {$endif}
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
begin
  GDocument := TNyxDocument.Create;
  GDocument.AddPage(NewNyxPage('home').Add(NewNyxCanvas('board').WithScene(NyxCanvasRef('Notes'))));
  GDocument.Canvases.Define(NyxCanvasRef('Notes'),
    NewNyxCanvasDefinition.WithCamera(NyxCanvasCamera(800, 600, NyxCanvasPoint(0, 0))));
  {$ifdef PAS2JS}
  await(StudioInput);
  {$else}
  StudioInput;
  {$endif}
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    try
      await(Run);
      document.body.setAttribute('data-canvas-precision-studio', 'passed');
      document.body.setAttribute('data-canvas-precision-studio-checks', IntToStr(GChecks));
    finally
      Cleanup;
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-canvas-precision-studio', 'failed');
      document.body.setAttribute('data-canvas-precision-studio-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GCompact := window.innerWidth < 600;
  window.setTimeout(@Start, 100);
  {$else}

  if (ParamStr(1) = '') or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned Studio fixture directory');
  end;
  Application.Initialize;
  GCompact := ParamStr(2) = '--compact';
  GFailure := TFailure.Create;
  Application.OnException := GFailure.Failed;
  try
    try
      Run;
    finally
      Cleanup;
    end;
    WriteLn('PASS ', GChecks, ' actual camera/Studio precision checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
