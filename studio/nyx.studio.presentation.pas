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
unit nyx.studio.presentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.binding.types,
  nyx.studio.view, nyx.studio.inspector, nyx.studio.authoring, nyx.studio.palette;

type
  { Owned editor presentation, independent of a document pair and its history.
    A controller captures this before project navigation and restores it once
    that same project's authoritative pair has mounted. No widgets, transport
    capabilities, compiler paths or document/session pointers are retained.
    Caret indices follow the target's ordinary editor contract; they are clamped
    to the current text on restoration. Scroll coordinates are logical pixels.
    New-state fields retain an unsubmitted authoring draft; Palette retains the
    user's current search/filter, separately from global discovery preferences. }
  TNyxStudioPresentation = record
    CodeVisible: Boolean;
    SourceTab: TNyxStudioSourceTab;
    SourceExpanded: Boolean;
    CanvasPercent: Integer;
    Phone: Boolean;
    Preview: Boolean;
    AgentsVisible: Boolean;
    Panel: TNyxStudioPanel;
    AdvancedProperties: Boolean;
    InspectorTab: TNyxInspectorTab;
    StateVisible: Boolean;
    BindingsVisible: Boolean;
    BindingTarget: TNyxBindingProperty;
    BindingDirection: TNyxBindingDirection;
    NewStateName: TNyxText;
    NewStateInput: TNyxStudioStateInput;
    NewStateValue: TNyxText;
    OutputVisible: Boolean;
    OutputTarget: TNyxText;
    FilesVisible: Boolean;
    LeftScroll: Integer;
    RightScroll: Integer;
    AgentsScroll: Integer;
    CanvasScrollTop: Integer;
    CanvasScrollLeft: Integer;
    CanvasView: TNyxText;
    CodeCaretStart: Integer;
    CodeCaretEnd: Integer;
    CodeScrollTop: Integer;
    CodeScrollLeft: Integer;
    CodeFocused: Boolean;
    Palette: TNyxStudioPaletteState;
  end;

function DefaultNyxStudioPresentation: TNyxStudioPresentation;
{ Platform scroll APIs may report fractional pixels or negative overscroll.
  Store the nearest nonnegative logical pixel (halves round up), saturating at
  the portable signed range. Non-finite input refuses instead of poisoning
  saved preferences. }
function NyxStudioScrollPosition(APosition: Double): Integer;
{ Closed versioned persistence boundary. Enum ordinals and exact scalar types
  are admitted into a detached value before a controller publishes any field.
  Invalid versions, unknown keys, out-of-range choices or reversed carets refuse. }
function EncodeNyxStudioPresentation(const AValue: TNyxStudioPresentation): TNyxText;
function DecodeNyxStudioPresentation(const AText: TNyxText): TNyxStudioPresentation;

implementation

uses
  Math, nyx.studio.outputs;

function NyxStudioScrollPosition(APosition: Double): Integer;
begin

  if IsNan(APosition) or IsInfinite(APosition) then
  begin
    raise ENyxModel.Create('Editor scroll position must be finite');
  end;

  if APosition <= 0 then
  begin
    Exit(0);
  end;

  if APosition >= 2147483647 then
  begin
    Exit(2147483647);
  end;
  { Native Round and JavaScript rounding can disagree on exact half pixels.
    The admitted nonnegative domain makes truncation after adding half explicit
    and identical on both targets, including values near the signed limit. }
  Result := Trunc(APosition + 0.5);
end;

function DefaultNyxStudioPresentation: TNyxStudioPresentation;
begin
  Result.CodeVisible := False;
  Result.SourceTab := nstSource;
  Result.SourceExpanded := False;
  Result.CanvasPercent := 65;
  Result.Phone := False;
  Result.Preview := False;
  Result.AgentsVisible := False;
  Result.Panel := nspDesign;
  Result.AdvancedProperties := False;
  Result.InspectorTab := nitProperties;
  Result.StateVisible := False;
  Result.BindingsVisible := False;
  Result.BindingTarget := bpValue;
  Result.BindingDirection := bdTwoWay;
  Result.NewStateName := '';
  Result.NewStateInput := ssiText;
  Result.NewStateValue := '';
  Result.OutputVisible := False;
  Result.OutputTarget := '';
  Result.FilesVisible := False;
  Result.LeftScroll := 0;
  Result.RightScroll := 0;
  Result.AgentsScroll := 0;
  Result.CanvasScrollTop := 0;
  Result.CanvasScrollLeft := 0;
  Result.CanvasView := '';
  Result.CodeCaretStart := 0;
  Result.CodeCaretEnd := 0;
  Result.CodeScrollTop := 0;
  Result.CodeScrollLeft := 0;
  Result.CodeFocused := False;
  Result.Palette := DefaultNyxStudioPaletteState;
end;

function EncodeNyxStudioPresentation(const AValue: TNyxStudioPresentation): TNyxText;
begin
  Result := NyxObject([
    NyxField('version', NyxData(3)),
    NyxField('codeVisible', NyxData(AValue.CodeVisible)),
    NyxField('sourceTab', NyxData(Ord(AValue.SourceTab))),
    NyxField('sourceExpanded', NyxData(AValue.SourceExpanded)),
    NyxField('canvasPercent', NyxData(AValue.CanvasPercent)),
    NyxField('phone', NyxData(AValue.Phone)),
    NyxField('preview', NyxData(AValue.Preview)),
    NyxField('agentsVisible', NyxData(AValue.AgentsVisible)),
    NyxField('panel', NyxData(Ord(AValue.Panel))),
    NyxField('advancedProperties', NyxData(AValue.AdvancedProperties)),
    NyxField('inspectorTab', NyxData(Ord(AValue.InspectorTab))),
    NyxField('stateVisible', NyxData(AValue.StateVisible)),
    NyxField('bindingsVisible', NyxData(AValue.BindingsVisible)),
    NyxField('bindingTarget', NyxData(Ord(AValue.BindingTarget))),
    NyxField('bindingDirection', NyxData(Ord(AValue.BindingDirection))),
    NyxField('newStateName', NyxData(AValue.NewStateName)),
    NyxField('newStateInput', NyxData(Ord(AValue.NewStateInput))),
    NyxField('newStateValue', NyxData(AValue.NewStateValue)),
    NyxField('outputVisible', NyxData(AValue.OutputVisible)),
    NyxField('outputTarget', NyxData(AValue.OutputTarget)),
    NyxField('filesVisible', NyxData(AValue.FilesVisible)),
    NyxField('leftScroll', NyxData(AValue.LeftScroll)),
    NyxField('rightScroll', NyxData(AValue.RightScroll)),
    NyxField('agentsScroll', NyxData(AValue.AgentsScroll)),
    NyxField('canvasScrollTop', NyxData(AValue.CanvasScrollTop)),
    NyxField('canvasScrollLeft', NyxData(AValue.CanvasScrollLeft)),
    NyxField('canvasView', NyxData(AValue.CanvasView)),
    NyxField('codeCaretStart', NyxData(AValue.CodeCaretStart)),
    NyxField('codeCaretEnd', NyxData(AValue.CodeCaretEnd)),
    NyxField('codeScrollTop', NyxData(AValue.CodeScrollTop)),
    NyxField('codeScrollLeft', NyxData(AValue.CodeScrollLeft)),
    NyxField('codeFocused', NyxData(AValue.CodeFocused)),
    NyxField('palette', NyxData(EncodeNyxStudioPalettePreferences(AValue.Palette))),
    NyxField('search', NyxData(AValue.Palette.Search))]).ToJSON;
end;

function IntegerValue(const AValue: TNyxDataValue; const AKey: TNyxText;
  AMinimum, AMaximum: Integer): Integer;
begin
  Result := AValue.Field(AKey).AsInteger;

  if (Result < AMinimum) or (Result > AMaximum) then
  begin
    raise ENyxModel.Create('Presentation choice exceeds its supported range: ' + AKey);
  end;
end;

function DecodeNyxStudioPresentation(const AText: TNyxText): TNyxStudioPresentation;
var
  LValue: TNyxDataValue;
  LIndex: Integer;
const
  CKeys: TNyxText = '|version|codeVisible|sourceTab|sourceExpanded|canvasPercent|phone|preview|agentsVisible|panel|advancedProperties|inspectorTab|stateVisible|bindingsVisible|bindingTarget|bindingDirection|newStateName|newStateInput|newStateValue|outputVisible|outputTarget|filesVisible|leftScroll|rightScroll|agentsScroll|canvasScrollTop|canvasScrollLeft|canvasView|codeCaretStart|codeCaretEnd|codeScrollTop|codeScrollLeft|codeFocused|palette|search|';
begin
  Result := DefaultNyxStudioPresentation;
  LValue := TNyxDataValue.ParseJSON(AText);

  if (LValue.Kind <> ndObject) or
    not (LValue.Field('version').AsInteger in [2, 3]) or
    ((LValue.Field('version').AsInteger = 2) and (LValue.Count <> 32)) or
    ((LValue.Field('version').AsInteger = 3) and (LValue.Count <> 34)) then
  begin
    raise ENyxModel.Create('Unsupported editor presentation packet');
  end;
  for LIndex := 0 to LValue.Count - 1 do
  begin

    if (Pos('|', LValue.Key(LIndex)) > 0) or
      (Pos('|' + LValue.Key(LIndex) + '|', CKeys) = 0) or
      ((LValue.Field('version').AsInteger = 2) and
        ((LValue.Key(LIndex) = 'sourceTab') or
          (LValue.Key(LIndex) = 'sourceExpanded'))) then
    begin
      raise ENyxModel.Create('Unknown editor presentation field');
    end;
  end;
  Result.CodeVisible := LValue.Field('codeVisible').AsBoolean;
  { Version 2 preferences retain their original strict shape and default to the
    source view. Version 3 adds only typed per-project presentation choices. }
  if LValue.Field('version').AsInteger = 3 then
  begin
    Result.SourceTab := TNyxStudioSourceTab(IntegerValue(LValue, 'sourceTab',
      Ord(Low(TNyxStudioSourceTab)), Ord(High(TNyxStudioSourceTab))));
    Result.SourceExpanded := LValue.Field('sourceExpanded').AsBoolean;
  end;
  Result.CanvasPercent := IntegerValue(LValue, 'canvasPercent', 10, 90);
  Result.Phone := LValue.Field('phone').AsBoolean;
  Result.Preview := LValue.Field('preview').AsBoolean;
  Result.AgentsVisible := LValue.Field('agentsVisible').AsBoolean;
  Result.Panel := TNyxStudioPanel(IntegerValue(LValue, 'panel',
    Ord(Low(TNyxStudioPanel)), Ord(High(TNyxStudioPanel))));
  Result.AdvancedProperties := LValue.Field('advancedProperties').AsBoolean;
  Result.InspectorTab := TNyxInspectorTab(IntegerValue(LValue, 'inspectorTab',
    Ord(Low(TNyxInspectorTab)), Ord(High(TNyxInspectorTab))));
  Result.StateVisible := LValue.Field('stateVisible').AsBoolean;
  Result.BindingsVisible := LValue.Field('bindingsVisible').AsBoolean;
  Result.BindingTarget := TNyxBindingProperty(IntegerValue(LValue, 'bindingTarget',
    Ord(Low(TNyxBindingProperty)), Ord(High(TNyxBindingProperty))));
  Result.BindingDirection := TNyxBindingDirection(IntegerValue(LValue, 'bindingDirection',
    Ord(Low(TNyxBindingDirection)), Ord(High(TNyxBindingDirection))));
  Result.NewStateName := LValue.Field('newStateName').AsText;
  Result.NewStateInput := TNyxStudioStateInput(IntegerValue(LValue, 'newStateInput',
    Ord(Low(TNyxStudioStateInput)), Ord(High(TNyxStudioStateInput))));
  Result.NewStateValue := LValue.Field('newStateValue').AsText;
  Result.OutputVisible := LValue.Field('outputVisible').AsBoolean;
  Result.OutputTarget := LValue.Field('outputTarget').AsText;
  Result.FilesVisible := LValue.Field('filesVisible').AsBoolean;
  Result.LeftScroll := IntegerValue(LValue, 'leftScroll', 0, 2147483647);
  Result.RightScroll := IntegerValue(LValue, 'rightScroll', 0, 2147483647);
  Result.AgentsScroll := IntegerValue(LValue, 'agentsScroll', 0, 2147483647);
  Result.CanvasScrollTop := IntegerValue(LValue, 'canvasScrollTop', 0, 2147483647);
  Result.CanvasScrollLeft := IntegerValue(LValue, 'canvasScrollLeft', 0, 2147483647);
  Result.CanvasView := LValue.Field('canvasView').AsText;
  Result.CodeCaretStart := IntegerValue(LValue, 'codeCaretStart', 0, 2147483647);
  Result.CodeCaretEnd := IntegerValue(LValue, 'codeCaretEnd', 0, 2147483647);
  Result.CodeScrollTop := IntegerValue(LValue, 'codeScrollTop', 0, 2147483647);
  Result.CodeScrollLeft := IntegerValue(LValue, 'codeScrollLeft', 0, 2147483647);
  Result.CodeFocused := LValue.Field('codeFocused').AsBoolean;
  ValidateNyxOutputTarget(Result.OutputTarget);
  DecodeNyxStudioPalettePreferences(LValue.Field('palette').AsText, Result.Palette);
  Result.Palette.Search := LValue.Field('search').AsText;

  if Result.CodeCaretEnd < Result.CodeCaretStart then
  begin
    raise ENyxModel.Create('Editor selection end precedes its start');
  end;
end;

end.
