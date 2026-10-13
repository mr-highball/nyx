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

program nyx_button_palette_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.colors,
  nyx.theme, nyx.design.tokens, nyx.theme.editor, nyx.selection, nyx.state,
  nyx.data, nyx.codec, nyx.codegen, nyx.source, nyx.generated.view
  {$ifdef NYX_COMPILED_PALETTE}, nyx.generated.palette{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, Interfaces, Classes, Forms, Controls, Graphics, IntfGraphics,
    FPWritePNG, LCLIntf, LCLType, nyx.render.lcl, nyx.widgets.lcl{$endif};

const
  CNames: array[TNyxVariant] of TNyxText =
    ('Default', 'Primary', 'Secondary', 'Danger', 'Success', 'Warning', 'Ghost');

var
  GDocument: TNyxDocument;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GHosts: array[Boolean] of TJSHTMLElement;
  GRenderers: array[Boolean] of TNyxBrowserRenderer;
  GThemes: array[Boolean] of TNyxTheme;
  GTimer: Integer;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Button palette: ' + AReason);
  end;
  Inc(GChecks);
end;

function ButtonID(AVariant: TNyxVariant): TNyxText;
begin
  Result := 'palette-' + LowerCase(CNames[AVariant]);
end;

{ The exact seed is composed/exported/built through semantic MCP. This explicit
  current-source enrichment exercises new roles absent from the installed
  release; it never replaces an observing user's project. }
procedure Enrich(ADocument: TNyxDocument);
var
  LPage: INyxPage;
  LRow: INyxRow;
  LButton: INyxButton;
  LVariant: TNyxVariant;
  LSteps: INyxStepper;
begin
  LPage := NewNyxPage('palette-review');
  ADocument.AddPage(LPage.Node);
  LPage.Configure.Padding(24).Gap(16).Done;
  LPage.Add(NewNyxHeading('palette-title').WithText('Actions with character'));
  LPage.Add(NewNyxLabel('palette-help').WithText(
    'A shared Pascal palette, from everyday actions to important decisions.'));
  LRow := NewNyxRow('palette-actions');
  LRow.Configure.Wrap(nfwWrap).Gap(12).Done;
  LPage.Add(LRow);
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin
    LButton := NewNyxButton(ButtonID(LVariant)).WithText(CNames[LVariant]);
    LButton.Configure.Variant(LVariant).Done;
    LRow.Add(LButton);
  end;
  LSteps := NewNyxStepper('palette-steps');
  SetNyxSelectionOptions(LSteps,
    TNyxSelectionOptions.Defaults.Selected(nvSuccess).Unselected(nvGhost));
  LPage.Add(NewNyxHeading('palette-selection-title').WithText('A little progress'));
  LPage.Add(LSteps);
  SetNyxThemeTokens(ADocument, NyxThemeTokens
    .Danger(TNyxRGBColor.FromText('#B42318'))
    .DangerText(NyxRGB(255, 255, 255))
    .Success(NyxRGB(6, 95, 70)).SuccessText(NyxRGB(255, 255, 255))
    .Warning(NyxRGB(253, 176, 34)).WarningText(NyxRGB(32, 39, 55)));
end;

{ WCAG relative luminance for defined eight-bit sRGB, without ratio rounding.
  This verifies the default idle text pairs, not every state or conformance of
  a whole application. Custom caller palettes remain admitted without clamping. }
function Luminance(const AColor: TNyxText): Double;
var
  LRGB: TNyxRGBColor;
  function Linear(AChannel: Byte): Double;
  begin
    Result := AChannel / 255;

    if Result <= 0.04045 then
    begin
      Result := Result / 12.92;
    end
    else
    begin
      Result := Power((Result + 0.055) / 1.055, 2.4);
    end;
  end;
begin
  LRGB := TNyxRGBColor.FromText(AColor);
  Result := 0.2126 * Linear(LRGB.Red) + 0.7152 * Linear(LRGB.Green) +
    0.0722 * Linear(LRGB.Blue);
end;

procedure Shared;
var
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LTheme: TNyxTheme;
  LCopy: TNyxTheme;
  LFace: TNyxButtonAppearance;
  LVariant: TNyxVariant;
  LDark: Boolean;
  LTokens: TNyxThemeTokens;
  LBefore: TNyxText;
  LRatio: Double;
  LForm: INyxCard;
  LDraft: TNyxThemeEditorDraft;
  LData: TNyxDataValue;
  LValues: array of TNyxDataValue;
  LOverrides: array of TNyxDataValue;
  LIndex: Integer;
  LRefused: Boolean;
  LChange: TNyxThemeEditorChange;
begin
  LRead := nil;
  LWorkspace := nil;
  try
    LBefore := TNyxCodec.Encode(GDocument);
    LRead := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LRead) = LBefore, 'new partial roles preserve exact persistence');
    LRead.Free;
    LRead := TNyxSourceWorkspace.PrepareDraft(
      TNyxCodegen.Generate(GDocument), LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore, 'crafted fluent source reconstructs exact roles');
  finally
    LRead.Free;
    LWorkspace.Free;
  end;
  for LDark := False to True do
  begin
    LTheme := TNyxTheme.Create(LDark);
    try
      for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
      begin
        LFace := LTheme.ButtonAppearance(LVariant);
        LRatio := (Max(Luminance(LFace.Background), Luminance(LFace.Foreground)) + 0.05) /
          (Min(Luminance(LFace.Background), Luminance(LFace.Foreground)) + 0.05);
        Check(LRatio >= 4.5, 'default idle text contrast for ' + CNames[LVariant]);

        if LFace.Transparent then
        begin
          LRatio := (Max(Luminance(LTheme.Background), Luminance(LFace.Foreground)) + 0.05) /
            (Min(Luminance(LTheme.Background), Luminance(LFace.Foreground)) + 0.05);
          Check(LRatio >= 4.5, 'ghost text on the standard page background');
        end;
      end;
      LTheme.Danger := '#123456';
      LCopy := NewNyxDocumentTheme(nil, LTheme);
      try
        LTheme.Danger := '#abcdef';
        Check(LCopy.Danger = '#123456', 'copied base keeps independent status roles');
      finally
        LCopy.Free;
      end;
    finally
      LTheme.Free;
    end;
  end;
  LTokens := NyxThemePreset(ntpDark);
  Check((LTokens.ToData.Count = 16) and LTokens.Has(ntcWarningText),
    'complete preset includes every new fill and foreground role');
  LTokens := LTokens.Danger(NyxRGB(0, 0, 0)).DangerText(NyxRGB(0, 0, 0));
  Check(LTokens.ColorValue(ntcDanger).ToText = '#000000',
    'deliberate low contrast caller override remains admitted');
  LForm := NewNyxThemeEditor('palette-theme', GDocument);
  Check(LForm.Node.Find(NyxThemeEditorFieldID('palette-theme', ntrDanger)).Kind = 'color',
    'ordinary theme editor specializes the new status field');
  LForm.Node.Find(NyxThemeEditorFieldID('palette-theme', ntrDanger)).SetProp('value', '#aBcDeF');
  Check(CaptureNyxThemeEditor(LForm.Node.Find(
    NyxThemeEditorActionID('palette-theme', nteApply)), LForm.Node, LChange) and
    (LChange.Tokens.ColorValue(ntcDanger).ToText = '#aBcDeF'),
    'new fields submit an exact typed authoring proposal');
  LDraft.Capture('palette-theme', LForm.Node);
  LData := LDraft.ToData;
  Check((LData.Field('version').AsInteger = 2) and (LData.Field('values').Count = 16),
    'expanded copied draft has a versioned complete shape');
  SetLength(LValues, 10);
  SetLength(LOverrides, 10);
  for LIndex := 0 to 9 do
  begin
    LValues[LIndex] := LData.Field('values').Item(LIndex);
    LOverrides[LIndex] := LData.Field('overrides').Item(LIndex);
  end;
  LDraft := TNyxThemeEditorDraft.FromData(NyxObject([
    NyxField('version', NyxData(1)), NyxField('editor', LData.Field('editor')),
    NyxField('context', LData.Field('context')), NyxField('values', NyxArray(LValues)),
    NyxField('overrides', NyxArray(LOverrides))]));
  LData := LDraft.ToData;
  Check((LData.Field('values').Item(Ord(ntrRadius)).ToJSON = LValues[Ord(ntrRadius)].ToJSON) and
    (LData.Field('overrides').Item(Ord(ntrDanger)).AsText = 'false'),
    'legacy metrics retain ordinals while new roles inherit');
  LRefused := False;
  try
    LDraft := TNyxThemeEditorDraft.FromData(NyxObject([
      NyxField('version', NyxData(99)), NyxField('editor', LData.Field('editor')),
      NyxField('context', LData.Field('context')), NyxField('values', LData.Field('values')),
      NyxField('overrides', LData.Field('overrides'))]));
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unknown preference version refuses');
end;

{$ifdef PAS2JS}
function CSSColor(const AValue: TNyxText): String;
var
  LColor: TNyxRGBColor;
begin
  LColor := TNyxRGBColor.FromText(AValue);
  Result := 'rgb(' + IntToStr(LColor.Red) + ', ' + IntToStr(LColor.Green) +
    ', ' + IntToStr(LColor.Blue) + ')';
end;

{ The fixture owns each renderer and its borrowed theme together. Retire views
  before their themes/documents on either successful capture or refusal. }
procedure CleanupBrowser;
var
  LDark: Boolean;
begin

  if GTimer >= 0 then
  begin
    window.clearInterval(GTimer);
    GTimer := -1;
  end;
  for LDark := False to True do
  begin
    GRenderers[LDark].Free;
    GRenderers[LDark] := nil;
    GThemes[LDark].Free;
    GThemes[LDark] := nil;

    if GHosts[LDark] <> nil then
    begin
      GHosts[LDark].remove;
    end;
    GHosts[LDark] := nil;
  end;
end;

procedure Finish;
begin
  CleanupBrowser;
  GDocument.Free;
  GDocument := nil;
  document.body.setAttribute('data-palette-checks', IntToStr(GChecks));
  document.body.setAttribute('data-palette', 'passed');
end;

procedure AwaitCapture;
begin

  if document.body.getAttribute('data-capture-observed') = 'button-palettes' then
  begin
    Finish;
  end;
end;
{$else}
type
  TButtonAccess = class(TNyxLCLButton);

procedure Capture(AHost: TForm; const APath: String; AGhost: TNyxLCLButton;
  const AText: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LDC: HDC;
  LX: Integer;
  LY: Integer;
  LFound: Boolean;
  LPoint: TPoint;
begin
  AHost.BringToFront;
  AHost.Update;
  Application.ProcessMessages;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(AHost.ClientWidth, AHost.ClientHeight);
    LDC := GetDC(AHost.Handle);
    try
      Check(BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
        LDC, 0, 0, SRCCOPY), 'owned displayed client pixels captured');
    finally
      ReleaseDC(AHost.Handle, LDC);
    end;
    LPoint := AHost.ScreenToClient(AGhost.ClientToScreen(Point(0, 0)));
    LFound := False;
    for LY := 3 to 7 do
    begin
      for LX := 10 to AGhost.Width - 10 do
      begin

        if ColorToRGB(LBitmap.Canvas.Pixels[LPoint.X + LX, LPoint.Y + LY]) =
          NyxLCLColor(AText) then
        begin
          LFound := True;
        end;
      end;
    end;
    Check(LFound, 'focused ghost has an actual visible inner focus line');
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(APath, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

procedure Actual(ADark: Boolean);
var
  LTheme: TNyxTheme;
  LVariant: TNyxVariant;
  LFace: TNyxButtonAppearance;
  LBefore: TNyxText;
  LSteps: TNyxNode;
  LStep: TNyxNode;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LRenderer: TNyxBrowserRenderer;
  LElement: TJSHTMLElement;
  LStyle: TJSCSSStyleDeclaration;
  {$else}
  LHost: TForm;
  LRenderer: TNyxLCLRenderer;
  LButton: TNyxLCLButton;
  {$endif}
begin
  LTheme := TNyxTheme.Create(ADark);
  { Document overrides are tested separately. Here host defaults qualify both
    shipped palettes. Restoring the exact declaration after mounting retains
    portable authored identity; the view owns its independent realized tree. }
  LBefore := NyxThemeDeclaration(GDocument).ToJSON;
  ResetNyxThemeTokens(GDocument);
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('section'));
  LHost.style.cssText := 'width:100%;min-height:280px;margin-bottom:16px;';
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create(LTheme);
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(50, 50, 960, 390);
  LRenderer := TNyxLCLRenderer.Create(LTheme);
  {$endif}
  try
    LRenderer.Render(GDocument, GDocument.Find('palette-review'), LHost);
    {$ifndef PAS2JS}
    LHost.Show;
    Application.ProcessMessages;
    {$endif}
    for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
    begin
      LFace := LTheme.ButtonAppearance(LVariant);
      {$ifdef PAS2JS}
      LElement := LRenderer.ElementFor(ButtonID(LVariant));
      LStyle := window.getComputedStyle(LElement);
      Check(LStyle.getPropertyValue('color') = CSSColor(LFace.Foreground),
        'actual browser foreground for ' + CNames[LVariant]);

      if LFace.Transparent then
      begin
        Check((LStyle.getPropertyValue('background-color') = 'rgba(0, 0, 0, 0)') and
          (LStyle.getPropertyValue('border-top-color') = 'rgba(0, 0, 0, 0)'),
          'ghost retains transparent browser fill and outline');
      end
      else
      begin
        Check((LStyle.getPropertyValue('background-color') = CSSColor(LFace.Background)) and
          (LStyle.getPropertyValue('border-top-color') = CSSColor(LFace.Border)),
          'actual browser fill/outline for ' + CNames[LVariant]);
      end;
      {$else}
      LButton := TNyxLCLButton(LRenderer.ControlFor(ButtonID(LVariant)));
      Check(LButton.Font.Color = NyxLCLColor(LFace.Foreground),
        'actual native foreground for ' + CNames[LVariant]);
      Check(LButton.Color = NyxLCLColor(LFace.Background),
        'actual native copied face for ' + CNames[LVariant]);
      {$endif}
    end;
    LSteps := LRenderer.Root.Find('palette-steps');
    LStep := LSteps.Part(NyxPart('step-1'));
    Check(LStep.Prop('variant') = 'success', 'selection policy starts with the status variant');
    {$ifdef PAS2JS}
    LRenderer.ElementFor(LSteps.Part(NyxPart('step-2')).ID).click;
    {$else}
    TButtonAccess(LRenderer.ControlFor(LSteps.Part(NyxPart('step-2')).ID)).Click;
    Application.ProcessMessages;
    {$endif}
    Check((LStep.Prop('variant') = 'ghost') and
      (LSteps.Part(NyxPart('step-2')).Prop('variant') = 'success'),
      'actual activation projects selected/unselected status variants');
    LSteps.SetProp('readonly', 'true');
    LRenderer.Sync;
    {$ifdef PAS2JS}
    LRenderer.ElementFor(LSteps.Part(NyxPart('step-4')).ID).click;
    {$else}
    TButtonAccess(LRenderer.ControlFor(LSteps.Part(NyxPart('step-4')).ID)).Click;
    Application.ProcessMessages;
    {$endif}
    Check(LSteps.Prop('value') = '2', 'new appearance cannot bypass readonly command admission');
    {$ifdef PAS2JS}
    Check(window.getComputedStyle(LRenderer.ElementFor(
      LSteps.Part(NyxPart('step-2')).ID)).getPropertyValue('background-color') =
      CSSColor(LTheme.Success), 'selection refresh recolors the actual browser button');
    GHosts[ADark] := LHost;
    GRenderers[ADark] := LRenderer;
    GThemes[ADark] := LTheme;
    LTheme := nil;
    LRenderer := nil;
    LHost := nil;
    {$else}
    Check(TNyxLCLButton(LRenderer.ControlFor(
      LSteps.Part(NyxPart('step-2')).ID)).Color = NyxLCLColor(LTheme.Success),
      'selection refresh recolors the actual native button');
    LButton := TNyxLCLButton(LRenderer.ControlFor(ButtonID(nvGhost)));
    LButton.SetFocus;
    Check(LButton.Focused, 'native ghost retains focus admission');

    if ParamCount >= 2 then
    begin
      Capture(LHost, ParamStr(2) + '-' + IntToStr(Ord(ADark)) + '.png', LButton, LTheme.Text);
    end;
    {$endif}
  finally
    SetNyxThemeTokens(GDocument, TNyxThemeTokens.FromData(TNyxDataValue.ParseJSON(LBefore)));
    LRenderer.Free;
    {$ifdef PAS2JS}

    if LHost <> nil then
    begin
      LHost.remove;
    end;
    {$else}
    LHost.Free;
    {$endif}
    LTheme.Free;
  end;
end;

var
  LExpected: TNyxDocument;
  {$ifndef PAS2JS}LFile: TFileStream;{$endif}
  LSource: TNyxText;
begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  {$ifdef PAS2JS}GTimer := -1;{$endif}
  LExpected := nil;
  try
    GDocument := nyx.generated.view.BuildNyxDocument;
    Enrich(GDocument);
    {$ifdef NYX_COMPILED_PALETTE}
    LExpected := GDocument;
    GDocument := nyx.generated.palette.BuildNyxDocument;
    Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(LExpected),
      'compiled fluent source reconstructs the exact enriched semantic companion');
    FreeAndNil(LExpected);
    {$endif}
    Shared;
    LSource := TNyxCodegen.Generate(GDocument, 'nyx.generated.palette');
    {$ifndef PAS2JS}

    if ParamCount >= 1 then
    begin
      LFile := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LFile.WriteBuffer(LSource[1], Length(LSource));
      finally
        LFile.Free;
      end;
    end;
    {$endif}
    Actual(False);
    Actual(True);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-capture-checkpoint', 'button-palettes');
    GTimer := window.setInterval(@AwaitCapture, 25);
    {$else}
    FreeAndNil(GDocument);
    WriteLn('PASS ', GChecks, ' button palette contract/source/actual-control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}CleanupBrowser;{$endif}
      LExpected.Free;
      GDocument.Free;
      GDocument := nil;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-palette', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
