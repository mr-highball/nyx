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
program nyx_projection_refresh_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, StdCtrls, ExtCtrls,
    LCLIntf, LCLType, Math,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.state, nyx.behavior, nyx.projection.refresh, nyx.codec, nyx.json,
  nyx.presentations, nyx.resources, nyx.collections
  {$ifdef NYX_ARRANGEMENT_MCP}, nyx.generated.view{$endif}
  {$ifdef NYX_IDENTITY_COMPILED}, nyx.identity.view{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  TControlAccess = class(TControl);
  {$endif}

  { The same real button routes through its retained native/DOM binding.
    Counts qualify hook continuity; document-only fixtures cannot prove it. }
  TObservation = class
  public
    Clicks: Integer;
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create('Projection refresh: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObservation.Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if (ANode.ID = 'action') and (AEvent.Trigger = ntClick) then
  begin
    Inc(Clicks);
  end;
end;

procedure Click(AFace: TFace);
begin
  {$ifdef PAS2JS}AFace.click;
  {$else}TControlAccess(AFace).Click;{$endif}
end;

function InputText(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := TJSHTMLInputElement(AFace).value;
  {$else}Result := TCustomEdit(AFace).Text;{$endif}
end;

function Caption(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := AFace.textContent;
  {$else}Result := TLabel(AFace).Caption;{$endif}
end;

procedure SetDraft(AFace: TFace);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := 'Independent draft / 🌙';
  AFace.dispatchEvent(TJSEvent.new('input'));
  TJSHTMLInputElement(AFace).focus;
  TJSHTMLInputElement(AFace).selectionStart := 4;
  TJSHTMLInputElement(AFace).selectionEnd := 4;
  {$else}
  TCustomEdit(AFace).Text := 'Independent draft / 🌙';
  TCustomEdit(AFace).SetFocus;
  TCustomEdit(AFace).SelStart := 4;
  {$endif}
end;

function Caret(AFace: TFace): Integer;
begin
  {$ifdef PAS2JS}Result := TJSHTMLInputElement(AFace).selectionStart;
  {$else}Result := TCustomEdit(AFace).SelStart;{$endif}
end;

{$ifndef PAS2JS}
type
  { Independent LCL reference uses a fresh drawing context for every sample. }
  TReferenceMeasureLabel = class(TLabel)
  public
    function HeightFor(ASource: TLabel): Integer;
  end;
  { A real preferred-size refusal happens after preceding labels have measured,
    exercising layout cleanup rather than an earlier constructor rejection. }
  TMeasureRefusalButton = class(TButton)
  protected
    procedure CalculatePreferredSize(var AWidth, AHeight: Integer;
      AWithThemeSpace: Boolean); override;
  public
    Refuse: Boolean;
    procedure SetRefusal(AValue: Boolean);
  end;

function TReferenceMeasureLabel.HeightFor(ASource: TLabel): Integer;
var
  LContext: HDC;
  LWidth: Integer;
  LHeight: Integer;
begin
  AutoSize := False;
  Font.Assign(ASource.Font);
  Caption := ASource.Caption;
  WordWrap := ASource.WordWrap;
  BiDiMode := ASource.BiDiMode;
  ShowAccelChar := ASource.ShowAccelChar;
  LContext := LCLIntf.GetDC(0);

  if LContext = 0 then
  begin
    raise Exception.Create('Reference measurement context unavailable');
  end;
  try
    Canvas.Handle := LContext;
    CalculateSize(Max(1, ASource.Width), LWidth, LHeight);
    Result := Max(28, LHeight + 4);
  finally
    Canvas.Handle := 0;
    LCLIntf.ReleaseDC(0, LContext);
  end;
end;

procedure TMeasureRefusalButton.CalculatePreferredSize(var AWidth, AHeight: Integer;
  AWithThemeSpace: Boolean);
begin

  if Refuse then
  begin
    raise Exception.Create('Intentional native measurement refusal');
  end;
  inherited CalculatePreferredSize(AWidth, AHeight, AWithThemeSpace);
end;

procedure TMeasureRefusalButton.SetRefusal(AValue: Boolean);
begin
  Refuse := AValue;
  InvalidatePreferredSize;
end;

function MeasurementFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TMeasureRefusalButton.Create(AOwner);
end;

procedure MeasureIsolation;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LReference: TReferenceMeasureLabel;
  LFirst: TLabel;
  LSecond: TLabel;
  LButton: TMeasureRefusalButton;
  LRound: Integer;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.CreateNew(nil);
  LReference := TReferenceMeasureLabel.Create(nil);
  try
    LHost.ClientWidth := 280;
    LHost.ClientHeight := 500;
    LDocument.AddPage(NewNyxPage('measurement-room'));
    LDocument.Pages[0].Configure.Padding(0).Gap(0).Done;
    LDocument.Pages[0].Add(NewNyxLabel('first-caption').WithText('A thoughtful line / 🌙'));
    LDocument.Pages[0].Add(NewNyxLabel('second-caption').WithText('Two lines' + #10 + 'of ideas'));
    LDocument.Pages[0].Add(NewNyxButton('measurement-action').WithText('Continue'));
    LDocument.Find('measurement-action').Configure.WidthSizing(nsContent).Done;
    LRenderer.RegisterFactory('button', MeasurementFactory);
    LHost.Show;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LFirst := TLabel(LRenderer.ControlFor('first-caption'));
    LSecond := TLabel(LRenderer.ControlFor('second-caption'));
    LButton := TMeasureRefusalButton(LRenderer.ControlFor('measurement-action'));
    for LRound := 0 to 11 do
    begin
      LFirst.Font.Height := -(14 + (LRound mod 3) * 8);
      LSecond.Font.Height := -(30 - (LRound mod 3) * 8);
      LFirst.BiDiMode := bdLeftToRight;
      LSecond.BiDiMode := bdRightToLeft;
      LHost.ClientWidth := 180 + (LRound mod 2) * 100;
      LRenderer.Sync;
      Check(LFirst.Height = LReference.HeightFor(LFirst),
        'first shared measurement retains exact independent LCL typography');
      Check(LSecond.Height = LReference.HeightFor(LSecond),
        'alternating font/bidi measurement restores independent drawing state');
      LButton.SetRefusal(True);
      LRefused := False;
      try
        LRenderer.Sync;
      except
        on LException: Exception do
        begin
          LRefused := Pos('Intentional native measurement refusal', LException.Message) > 0;
        end;
      end;
      Check(LRefused, 'actual later preferred-size refusal is observed');
      LButton.SetRefusal(False);
      LRenderer.Sync;
      Check((LRenderer.ControlFor('first-caption') = LFirst) and
        (LSecond.Height = LReference.HeightFor(LSecond)),
        'measurement failure retires its context and preserves subsequent live layout');
    end;
  finally
    LRenderer.Free;
    LReference.Free;
    LDocument.Free;
    LHost.Free;
  end;
end;

function RefuseFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := nil;
  raise ENyxModel.Create('Intentional staged factory refusal');
end;
{$endif}

{$ifdef NYX_IDENTITY_COMPILED}
procedure CompiledIdentity;
var
  LDocument: TNyxDocument;
  LRenderer: TRenderer;
  LFace: TFace;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$else}
  LHost: TForm;
  {$endif}
begin
  { Execute the actual native-exported companion before mounting its real face.
    This separates compiler/runtime/control evidence from parser admission. }
  LDocument := nyx.identity.view.BuildNyxDocument;
  LRenderer := TRenderer.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.ClientWidth := 500;
  LHost.ClientHeight := 240;
  LHost.Show;
  {$endif}
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef PAS2JS}
    LFace := LRenderer.ElementFor('intro-🌙''😀');
    {$else}
    LFace := LRenderer.ControlFor('intro-🌙''😀');
    {$endif}
    Check(LFace <> nil, 'compiled Unicode/quoted identity resolves its physical control');
    Check(Caption(LFace) = TNyxText('A writer''s revised idea / 🌙''😀'),
      'compiled literal escaping preserves exact Unicode/quoted target caption');
    LRenderer.Sync;
    Check(Caption(LFace) = TNyxText('A writer''s revised idea / 🌙''😀'),
      'ordinary target synchronization retains the exact compiled caption');
  finally
    LRenderer.Free;
    LDocument.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
  end;
end;
{$endif}

{ Frozen pre-optimization context algorithm. Keep this independent full-value
  path as the compatibility oracle: production now projects the codec's first
  admitted DOM, while this still decodes/canonicalizes/indexes the full export. }
function LegacyProjectionContext(ADocument: TNyxDocument): TNyxText;
var
  LData: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LKey: TNyxText;
begin
  LData := TNyxDataValue.ParseJSON(TNyxCodec.Encode(ADocument));
  SetLength(LFields, LData.Count);
  LCount := 0;
  for LIndex := 0 to LData.Count - 1 do
  begin
    LKey := LData.Key(LIndex);

    if (LKey = 'title') or (LKey = 'pages') or (LKey = 'components') or
      (LKey = 'version') or
      ((LKey = NyxPresentationsWireField) and (ADocument.Presentations.Count > 0)) or
      ((LKey = 'collections') and (ADocument.Collections.Count = 0) and
      not ADocument.Extensions.Has(NyxExtension('collections'))) then
    begin
      Continue;
    end;
    { Wire version is framing, not runtime meaning. Named definitions are
      checked per referenced property by retained projection admission; unused
      definitions cannot disturb live controls. Older opaque data stays exact. }
    LFields[LCount] := NyxField(LKey, LData.Field(LKey));
    Inc(LCount);
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields).ToJSON;
end;

procedure CheckContext(ADocument: TNyxDocument; const AReason: TNyxText);
var
  LBefore: TNyxText;
  LExpected: TNyxText;
begin
  LBefore := TNyxCodec.Encode(ADocument);
  LExpected := LegacyProjectionContext(ADocument);
  Check((NyxProjectionContext(ADocument) = LExpected) and
    (TNyxCodec.ProjectionContext(ADocument) = LExpected), AReason);
  Check(TNyxCodec.Encode(ADocument) = LBefore,
    'context construction leaves the complete authored export byte-exact');
end;

{ A reduced context must refuse invalid data even when the offending source is
  one of the fields it discards. Match both exception family and diagnostic to
  the original full-export algorithm; a small successful result is a failure. }
procedure RejectContext(ADocument: TNyxDocument; const AReason: TNyxText);
var
  LExpected: TNyxText;
  LActual: TNyxText;
begin
  LExpected := '';
  LActual := '';
  try
    LegacyProjectionContext(ADocument);
  except
    on LException: Exception do
    begin
      LExpected := TNyxText(LException.ClassName) + ': ' + TNyxText(LException.Message);
    end;
  end;
  try
    NyxProjectionContext(ADocument);
  except
    on LException: Exception do
    begin
      LActual := TNyxText(LException.ClassName) + ': ' + TNyxText(LException.Message);
    end;
  end;
  Check((LExpected <> '') and (LActual = LExpected), AReason);
end;

procedure ProjectionContextCompatibility;
var
  LDocument: TNyxDocument;
  LContext: TNyxText;
  LValue: TNyxDataValue;
  LIndex: Integer;
  LHasPresentation: Boolean;
  LMalformed: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('context-page'));
    CheckContext(LDocument, 'empty context matches the frozen full-value oracle');
    LContext := NyxProjectionContext(LDocument);
    LDocument.Title := TNyxText('English title / 🌙') + #0 + ' / slash';
    LDocument.Pages[0].Add(NewNyxLabel('context-label').WithText('Independent caption'));
    LDocument.AddComponent(NewNyxColumn('unused-context-component'));
    CheckContext(LDocument, 'title, pages and unused components keep the exact old context');
    Check(NyxProjectionContext(LDocument) = LContext,
      'excluded authored trees and Unicode/NUL title do not change context meaning');

    LDocument.Extensions.SetValue(NyxExtension(TNyxText('future / 🌙') + #0),
      TNyxDataValue.ParseJSON('{"precise":9007199254740993,"decimal":1.2300e+4,' +
        '"negativeZero":-0,"escaped":"\u0000\/\uD83C\uDF19","Empty":[],"empty":{}}'));
    CheckContext(LDocument, 'opaque key/order/exact decimal/Unicode/NUL values remain byte-exact');
    LDocument.State.SetValue(NyxTextState('context-caption'), TNyxText('A retained default'));
    CheckContext(LDocument, 'typed state defaults retain the old context representation');
    LDocument.Collections.Define(NyxCollection('context-records'),
      NyxCollectionSchema.Text(NyxTextField('name'), ''), []);
    CheckContext(LDocument, 'typed collection defaults retain their context and ordering');

    LDocument.Resources.Define(NyxResourceRef('context-copy'),
      NyxTextResource('An embedded English resource'));
    CheckContext(LDocument, 'resources and emitted empty typed presentations preserve context');
    LValue := TNyxDataValue.ParseJSON(NyxProjectionContext(LDocument));
    Check(LValue.Field(NyxPresentationsWireField).Kind = ndObject,
      'an emitted empty presentation field remains significant when no typed definition exists');
    LDocument.Presentations.Define(NyxPresentation('unused-context-presentation'),
      TNyxPresentationCondition.Manual);
    CheckContext(LDocument, 'unused typed named presentations preserve the exact exclusion rule');
    LValue := TNyxDataValue.ParseJSON(NyxProjectionContext(LDocument));
    LHasPresentation := False;
    for LIndex := 0 to LValue.Count - 1 do
    begin
      LHasPresentation := LHasPresentation or (LValue.Key(LIndex) = NyxPresentationsWireField);
    end;
    Check(not LHasPresentation, 'actual typed presentation definitions remain excluded from context');
  finally
    LDocument.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('empty-registry-context-page'));
    LDocument.Presentations.Define(NyxPresentation('empty-registry-presentation'),
      TNyxPresentationCondition.Manual);
    CheckContext(LDocument, 'a higher wire version retains the exact empty-collection exclusion');
    Check(TNyxDataValue.ParseJSON(NyxProjectionContext(LDocument)).Count = 0,
      'unused named definitions and emitted empty typed collections leave no context fields');
  finally
    LDocument.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('opaque-context-page'));
    LDocument.Extensions.SetValue(NyxExtension('collections'),
      TNyxDataValue.ParseJSON('{"legacy":"keep this exact opaque collection value"}'));
    LDocument.Extensions.SetValue(NyxExtension(NyxPresentationsWireField),
      TNyxDataValue.ParseJSON('["keep this opaque presentation value"]'));
    CheckContext(LDocument, 'older opaque collections and presentations remain significant');
    LValue := TNyxDataValue.ParseJSON(NyxProjectionContext(LDocument));
    Check((LValue.Field('collections').Kind = ndObject) and
      (LValue.Field(NyxPresentationsWireField).Kind = ndArray),
      'opaque legacy fields cannot be mistaken for empty typed registries');
    LDocument.Presentations.Define(NyxPresentation('conflicting-context-presentation'),
      TNyxPresentationCondition.Manual);
    RejectContext(LDocument, 'typed promotion still refuses retained opaque metadata conflicts');
  finally
    LDocument.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('budget-context-page'));
    LDocument.Title := StringOfChar('x', NyxMaximumJSONBytes);
    RejectContext(LDocument, 'discarded title still obeys the complete export byte budget');
    LDocument.Title := 'Context budget review';
    LDocument.Pages[0].Add(NewNyxLabel('large-context-label')
      .WithText(StringOfChar('x', NyxMaximumJSONBytes)));
    RejectContext(LDocument, 'discarded page text still obeys the complete export byte budget');
    LDocument.Find('large-context-label').Configure.Text('A small caption again').Done;
    for LIndex := 0 to NyxMaximumJSONMembers do
    begin
      LDocument.Find('large-context-label').Props.Add('future-' + IntToStr(LIndex) + '=value');
    end;
    RejectContext(LDocument, 'discarded page properties still obey the full object member budget');
  finally
    LDocument.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('depth-context-page'));
    LDocument.Pages[0].Extensions.SetValue(NyxExtension('future.depth'),
      TNyxDataValue.ParseJSON(StringOfChar('[', NyxMaximumJSONDepth - 2) + '0' +
        StringOfChar(']', NyxMaximumJSONDepth - 2)));
    RejectContext(LDocument, 'discarded node envelope still obeys aggregate JSON depth');
  finally
    LDocument.Free;
  end;

  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxPage('unicode-context-page'));
    {$ifdef PAS2JS}
    LMalformed := Chr($D800);
    {$else}
    SetLength(LMalformed, 1);
    LMalformed[1] := #$FF;
    {$endif}
    LDocument.Title := LMalformed;
    RejectContext(LDocument, 'discarded title cannot bypass malformed Unicode admission');
  finally
    LDocument.Free;
  end;
  RejectContext(nil, 'context construction preserves the original nil-document refusal');
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LRenderer: TRenderer;
  LObserver: TObservation;
  LRoot: TNyxNode;
  LEntryNode: TNyxNode;
  LCaption: TFace;
  LInput: TFace;
  LOtherInput: TFace;
  LRestores: TNyxProjectionValueRestores;
  LButton: TFace;
  LClicks: Integer;
  LRefused: Boolean;
  LContext: TNyxText;
  LRepeat: Integer;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$else}
  LHost: TForm;
  {$endif}
begin
  {$ifdef NYX_ARRANGEMENT_MCP}LDocument := BuildNyxDocument;
  {$else}LDocument := TNyxDocument.Create;{$endif}
  LCandidate := nil;
  LRenderer := TRenderer.Create;
  LObserver := TObservation.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('width', '720px');

  if window.location.search = '?compact' then
  begin
    LHost.style.setProperty('width', '390px');
  end;
  LHost.style.setProperty('height', '540px');
  document.body.appendChild(LHost);
  {$else}
  Application.Initialize;
  LHost := TForm.Create(nil);
  LHost.ClientWidth := 720;
  LHost.ClientHeight := 540;
  LHost.Show;
  {$endif}
  try
    {$ifndef NYX_ARRANGEMENT_MCP}
    LDocument.Title := 'English control review';
    LDocument.AddPage(NewNyxPage('review'));
    LDocument.Pages[0].Add(NewNyxLabel('caption').WithText('Original caption'));
    LDocument.Pages[0].Add(NewNyxRow('workspace'));
    LDocument.Find('workspace').Add(NewNyxColumn('left-room'));
    LDocument.Find('workspace').Add(NewNyxColumn('right-room'));
    LDocument.Find('left-room').Configure.Width(300).Done;
    LDocument.Find('right-room').Configure.Width(300).Done;
    LDocument.Find('left-room').Add(NewNyxInput('entry').WithText('Your notes'));
    LDocument.Find('entry').Configure.Value('Original default').Done;
    LDocument.Find('right-room').Add(NewNyxInput('other-entry').WithText('Independent notes'));
    LDocument.Find('other-entry').Configure.Value('Other default').Done;
    LDocument.Find('left-room').Add(NewNyxButton('action').WithText('Continue'));
    {$endif}
    LRenderer.OnEvent := LObserver.Changed;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LRoot := LRenderer.Root;
    {$ifdef PAS2JS}
    LCaption := LRenderer.ElementFor('caption');
    LButton := LRenderer.ElementFor('action');
    {$else}
    LCaption := LRenderer.ControlFor('caption');
    LButton := LRenderer.ControlFor('action');
    {$endif}
    LInput := LRenderer.InputFor('entry');
    LOtherInput := LRenderer.InputFor('other-entry');
    Click(LButton);
    LClicks := LObserver.Clicks;
    Check(LClicks > 0, 'Original actual button reaches its portable event route');
    SetDraft(LInput);
    LDocument.Find('caption').Configure.Text('Refreshed caption').Done;
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'Fresh supported scalar candidate reuses its mounted realization');
    Check(LRenderer.Root = LRoot, 'Scalar refresh retains the independently owned realized root');
    {$ifdef PAS2JS}
    Check((LRenderer.ElementFor('caption') = LCaption) and
      (LRenderer.ElementFor('action') = LButton), 'Actual DOM faces retain identity');
    {$else}
    Check((LRenderer.ControlFor('caption') = LCaption) and
      (LRenderer.ControlFor('action') = LButton), 'Actual native faces retain identity');
    {$endif}
    Check(Caption(LCaption) = 'Refreshed caption', 'Actual retained caption paints new authored text');
    Check((LRenderer.InputFor('entry') = LInput) and
      (InputText(LInput) = TNyxText('Independent draft / 🌙')) and (Caret(LInput) = 4),
      'Unchanged authored default cannot reset independent input text or caret');
    Click(LButton);
    Check(LObserver.Clicks = 2 * LClicks, 'Same retained binding fires once per original registration');

    LContext := NyxProjectionContext(LDocument);
    LDocument.Title := 'Another English project title';
    Check(NyxProjectionContext(LDocument) = LContext,
      'Project identity alone does not change runtime context');
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False) and
      (InputText(LInput) = TNyxText('Independent draft / 🌙')), 'No-op refresh retains current input');
    LDocument.Find('entry').Configure.Value('New authored value / 🌙').Done;
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False) and
      (InputText(LInput) = TNyxText('New authored value / 🌙')),
      'An actual authored value delta updates the retained input');

    SetDraft(LInput);
    SetDraft(LOtherInput);
    LCandidate := LDocument.Clone;
    LCandidate.Find('caption').Configure.Text('Must not publish this caption').Done;
    SetLength(LRestores, 2);
    LRestores[0] := TNyxProjectionValueRestore.ForField('entry', 'entry');
    LRestores[1] := TNyxProjectionValueRestore.ForField('missing-field', 'entry');
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False, LRestores) and
      (Caption(LCaption) = 'Refreshed caption') and
      (InputText(LInput) = TNyxText('Independent draft / 🌙')),
      'A partially invalid restore group refuses before copying any authored delta');
    FreeAndNil(LCandidate);
    SetLength(LRestores, 1);
    LRestores[0] := TNyxProjectionValueRestore.ForField('entry', 'other-entry');
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False, LRestores),
      'Matching runtime ID cannot restore another editable owner');
    LRestores[0] := TNyxProjectionValueRestore.ForField('entry', 'entry');
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False, LRestores) and
      (LRenderer.InputFor('entry') = LInput) and
      (InputText(LInput) = TNyxText('New authored value / 🌙')),
      'Explicit typed restoration resets an unchanged default without replacing its input');
    Check((LRenderer.InputFor('other-entry') = LOtherInput) and (Caret(LOtherInput) = 4) and
      (InputText(LOtherInput) = TNyxText('Independent draft / 🌙')),
      'Restoring one field preserves another field draft, control and caret');
    LDocument.Find('entry').Props.Delete(
      LDocument.Find('entry').Props.IndexOfName(NyxAttributeName(atValue)));
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False, LRestores) and
      (InputText(LInput) = ''), 'Absent authored Value is restored as an absent property');
    LDocument.Find('entry').Configure.Value('New authored value / 🌙').Done;
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'Ordinary authored value deltas continue after explicit field restoration');
    {$ifndef PAS2JS}
    Check((LRenderer.InputIdentity(LInput) = 'entry') and
      (LRenderer.InputIdentity(LCaption) = ''), 'Native input capture returns only exact live input identity');
    {$endif}

    { This admission is deliberately separate from scalar refresh. The same
      logical controls move between ordinary hosts; alternate node sets, live
      changed binding contracts and special pane structures still need a full mount. }
    LEntryNode := LRoot.Find('entry');
    SetDraft(LInput);
    LCandidate := LDocument.Clone;
    LCandidate.Find('right-room').Insert(0, LCandidate.Find('left-room').Extract(0));
    LCandidate.Find('right-room').Insert(1, LCandidate.Find('left-room').Extract(0));
    LCandidate.Find('caption').Configure.Text('Rearranged caption').Done;
    for LRepeat := 1 to 6 do
    begin
      { The callback exercise below clicks another control and may focus it.
        Establish this move's intended focused field without rewriting its
        already retained text/range; focus preservation concerns actual state
        immediately before publication, not a former application action. }
      {$ifdef PAS2JS}
      LInput.focus;
      TJSHTMLInputElement(LInput).selectionStart := 4;
      TJSHTMLInputElement(LInput).selectionEnd := 4;
      {$else}
      TWinControl(LInput).SetFocus;
      TCustomEdit(LInput).SelStart := 4;
      TCustomEdit(LInput).SelLength := 0;
      {$endif}
      Check(Caret(LInput) = 4, 'Each arrangement begins with the requested physical caret');
      Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
        'Exact control set admits forward rearrangement');
      Check((LRenderer.Root = LRoot) and (LRoot.Find('entry') = LEntryNode) and
        (LRoot.Find('entry').Parent.ID = 'right-room'),
        'Rearrangement retains exact node identity with its new borrowed parent');
      Check((LRenderer.InputFor('entry') = LInput) and
        (InputText(LInput) = TNyxText('Independent draft / 🌙')) and (Caret(LInput) = 4),
        'Reparent retains actual input object, independent supplementary text and caret');
      {$ifdef PAS2JS}
      Check((document.activeElement = LInput) and
        (LRenderer.ElementFor('entry').parentNode = LRenderer.ElementFor('right-room')) and
        (LRenderer.ElementFor('right-room').children[0] = LRenderer.ElementFor('entry')),
        'DOM parent/order and focused editing follow admitted ownership');
      {$else}
      Check(LHost.ActiveControl = LInput, 'Native reparent retains focused editing');
      Check(LRenderer.ControlFor('entry').Parent = LRenderer.ControlFor('right-room'),
        'Native physical parent follows admitted ownership');
      Check(TWinControl(LRenderer.ControlFor('entry')).TabOrder = 0,
        'Native tab order follows the new authored order');
      {$endif}
      Check(Caption(LCaption) = 'Rearranged caption',
        'Authored deltas follow identity independently of former child positions');
      LClicks := LObserver.Clicks;
      Click(LButton);
      Check(LObserver.Clicks = LClicks + 1,
        'Retained callback fires once after physical reparenting');
      Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False) and
        (LRoot.Find('entry').Parent.ID = 'left-room') and
        (InputText(LInput) = TNyxText('Independent draft / 🌙')) and (Caret(LInput) = 4),
        'Reverse arrangement restores ownership while retaining live editing');
    end;
    FreeAndNil(LCandidate);

    LCandidate := LDocument.Clone;
    LCandidate.Find('left-room').Insert(0, LCandidate.Find('left-room').Extract(1));
    Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False) and
      (LRoot.Find('left-room').Children[0].ID = 'action') and
      (LRenderer.InputFor('entry') = LInput) and (Caret(LInput) = 4),
      'Sibling-only reorder retains the same physical input and range');
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'Original sibling order remains independently recoverable');
    FreeAndNil(LCandidate);

    LCandidate := LDocument.Clone;
    LCandidate.Find('right-room').Add(LCandidate.Find('left-room').Extract(0));
    LCandidate.Find('caption').Configure.Text('Must not publish arrangement').Done;
    LRestores[0] := TNyxProjectionValueRestore.ForField('entry', 'other-entry');
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False, LRestores) and
      (LRoot.Find('entry').Parent.ID = 'left-room') and
      (Caption(LCaption) = 'Refreshed caption') and (Caret(LInput) = 4),
      'A mismatched restore refuses the complete structure/property group');
    FreeAndNil(LCandidate);
    LRestores[0] := TNyxProjectionValueRestore.ForField('entry', 'entry');
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False, LRestores),
      'Explicit restoration retains its scalar behavior after structural changes');

    LCandidate := LDocument.Clone;
    LCandidate.Pages[0].Add(NewNyxBadge('additional').WithText('New control'));
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False) and
      (LRenderer.Root = LRoot) and (Caption(LCaption) = 'Refreshed caption'),
      'Structural mismatch refuses reuse without changing mounted controls');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    if LCandidate.Find('action').StoredProp(NyxAttributeName(atVariant)) =
      NyxVariantName(nvPrimary) then
    begin
      LCandidate.Find('action').Configure.Variant(nvSecondary).Done;
    end
    else
    begin
      LCandidate.Find('action').Configure.Variant(nvPrimary).Done;
    end;
    Check(not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'Unqualified style changes request complete candidate rendering');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Extensions.SetValue(NyxExtension('review.context'), NyxData('Changed'));
    Check((NyxProjectionContext(LCandidate) <> LContext) and
      not LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False),
      'Document extension/context changes cannot silently reuse old meaning');
    FreeAndNil(LCandidate);
    LCandidate := LDocument.Clone;
    LCandidate.Find('entry').SetProp(NyxAttributeName(atHeight), 'invalid');
    LRefused := False;
    try
      LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], False);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRenderer.Root = LRoot) and
      (InputText(LInput) = TNyxText('New authored value / 🌙')),
      'Invalid current document fails fresh validation before changing the view');
    FreeAndNil(LCandidate);
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], True),
      'Switching designer/runtime mode requires a complete mount');

    {$ifndef PAS2JS}
    { A failed full candidate must release LCL sizing locks and retain the
      accepted tree. Real resize afterward qualifies the balanced host lifetime. }
    LRenderer.RegisterFactory('label', RefuseFactory);
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'New custom factory forbids old default projection reuse');
    LRefused := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRefused := Pos('Intentional staged factory refusal', LException.Message) > 0;
      end;
    end;
    Check(LRefused and (LRenderer.ControlFor('caption') = LCaption) and
      (InputText(LInput) = TNyxText('New authored value / 🌙')),
      'Failed staged factory retains the original actual controls');
    LHost.ClientWidth := 760;
    Application.ProcessMessages;
    Check(LRenderer.ControlFor('review').Width > 720,
      'Host sizing is released after failed candidate destruction');
    {$endif}
    LRenderer.Unmount;
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'Unmounted renderer owns no eligible projection');
    { An unbound design canvas intentionally has no active runtime subscription.
      Its idle scalar edit must still preserve real target controls. Use a fresh
      renderer after the intentional native factory refusal above. }
    FreeAndNil(LRenderer);
    LRenderer := TRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, True);
    {$ifdef PAS2JS}
    LCaption := LRenderer.ElementFor('caption');
    {$else}
    LCaption := LRenderer.ControlFor('caption');
    {$endif}
    LCandidate := LDocument.Clone;
    LCandidate.Find('caption').Configure.Text('A retained design caption').Done;
    Check(LRenderer.TryRefresh(LCandidate, LCandidate.Pages[0], True),
      'Idle unbound design mode refreshes without an active runtime subscription');
    {$ifdef PAS2JS}
    Check(LRenderer.ElementFor('caption') = LCaption,
      'Browser design caption retains its exact DOM face');
    {$else}
    Check(LRenderer.ControlFor('caption') = LCaption,
      'Native design caption retains its exact control face');
    {$endif}
    Check(Caption(LCaption) = 'A retained design caption',
      'Retained design caption consumes the exact accepted scalar edit');
    FreeAndNil(LCandidate);
    LDocument.State.SetValue(NyxTextState('caption-state'), TNyxText('Bound caption'));
    LDocument.Find('caption').Binds.Text(NyxTextState('caption-state')).Done;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, True);
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], True),
      'An inactive bound design coordinator retains its complete readiness guard');
    { Use a fresh default renderer after the intentional native factory change. }
    FreeAndNil(LRenderer);
    LRenderer := TRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Check(not CanRefreshNyxProjection(LRenderer.Root, LRenderer.Root),
      'The public unbound guard still refuses live binding descriptors');
    Check(LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'An exact live binding contract uses the separate coordinator-owned path');
    {$ifndef PAS2JS}
    MeasureIsolation;
    {$endif}
    {$ifdef NYX_IDENTITY_COMPILED}
    CompiledIdentity;
    {$endif}

    ProjectionContextCompatibility;
    WriteLn('PASS ', GChecks, ' actual retained projection checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-projection-refresh', 'passed');
    document.body.setAttribute('data-projection-refresh-checks', IntToStr(GChecks));
    {$endif}
  finally
    LRenderer.Free;
    LObserver.Free;
    LCandidate.Free;
    LDocument.Free;
    {$ifdef PAS2JS}LHost.remove;
    {$else}LHost.Free;{$endif}
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LException.Message);
      {$else}DumpExceptionBackTrace(Output); ExitCode := 1;{$endif}
    end;
  end;
end.
