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

unit nyx.codegen;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  nyx.text,
  nyx.dates,
  nyx.times,
  nyx.colors,
  nyx.images,
  nyx.resources,
  nyx.resources.rows,
  nyx.resource.sources,
  nyx.design.tokens,
  nyx.data,
  nyx.contract,
  nyx.types,
  nyx.responsive,
  nyx.presentations,
  nyx.content,
  nyx.menu.declarations,
  nyx.menu.bar.declarations,
  nyx.menu.types,
  nyx.popover.types,
  nyx.typeahead,
  nyx.root.types,
  nyx.containers,
  nyx.state,
  nyx.collections,
  nyx.collections.registry,
  nyx.collections.view.types,
  nyx.collections.query,
  nyx.collections.selection,
  nyx.binding.types,
  SysUtils,
  nyx.model;

type
  { Generates readable, deterministic Delphi-dialect source from the same model
    used by renderers. BuildNyxDocument returns an owned design tree. This is a
    generation boundary, not a parser for arbitrary hand-edited Pascal.
    Unit names are checked before any source is emitted. }
  TNyxCodegen = class
  public
    { Shared namespace admission for source emission and confined companion
      filenames. Names contain Pascal identifier segments, never filesystem paths. }
    class procedure AdmitUnitName(const AName: TNyxText); static;
    class function Generate(ADocument: TNyxDocument;
      const AUnitName: TNyxText = 'nyx.generated.view'): TNyxText; static;
  end;

implementation

uses
  fpjson,
  nyx.json,
  nyx.callbacks,
  nyx.scheduler,
  nyx.composition,
  nyx.schema;

function ControlStem(const AKind: TNyxText): TNyxText;
var
  LIndex: Integer;
  LChar: Char;
  LUpper: Boolean;
begin
  { Kind names become bounded ASCII Pascal identifiers: comment-thread becomes
    CommentThread. Extension punctuation/Unicode acts as a word boundary; the
    original kind and authored ID remain untouched in the generated constructor.
    The local-name caller adds L and admits collisions; enum symbol callers
    supply their own fixed prefix. Digits and Pascal keywords stay harmless. }
  Result := '';
  LUpper := True;
  for LIndex := 1 to Length(AKind) do
  begin
    LChar := AKind[LIndex];

    if LChar in ['a'..'z', 'A'..'Z', '0'..'9'] then
    begin

      if LUpper then
      begin
        LChar := UpCase(LChar);
      end;
      Result := Result + LChar;
      LUpper := False;

      if Length(Result) >= 48 then
      begin
        Break;
      end;
    end
    else
    begin
      LUpper := True;
    end;
  end;

  if Result = '' then
  begin
    Result := 'Control';
  end;
end;

function PascalInteger(AValue: Integer): TNyxText;
begin
  { Boundary literals can be ambiguous between Integer and Double overloads in
    pas2js. The familiar Pascal limit constructs carry their exact signed type. }

  if AValue = High(Integer) then
  begin
    Exit('High(Integer)');
  end;

  if AValue = Low(Integer) then
  begin
    Exit('Low(Integer)');
  end;
  Result := IntToStr(AValue);
end;

function PascalString(const AValue: TNyxText): TNyxText;
var
  LIndex: Integer;
  LStart: Integer;
  LRun: TNyxText;
begin
  { Ordinary captions remain ordinary Pascal literals. Text containing controls
    uses explicitly typed runs: older FPC can route adjacent Unicode/#N literals
    through a WideString conversion that drops embedded NUL. Typed UTF8String
    concatenation preserves it. Never assemble a Unicode caption byte by byte. }
  Result := '';
  LStart := 1;
  for LIndex := 1 to Length(AValue) do
  begin

    if Ord(AValue[LIndex]) < 32 then
    begin
      LRun := StringReplace(Copy(AValue, LStart, LIndex - LStart),
        '''', '''''', [rfReplaceAll]);

      if Result <> '' then
      begin
        Result := Result + ' + ';
      end;

      if LRun <> '' then
      begin
        Result := Result + 'TNyxText(''' + LRun + ''') + ';
      end;

      if AValue[LIndex] = #0 then
      begin
        { A runtime scalar avoids FPC's constant-folded Unicode conversion too;
          casting an adjacent #0 literal is insufficient on FPC 3.2. }
        Result := Result + 'NyxScalarText(0)';
      end
      else
      begin
        Result := Result + 'TNyxText(#' + IntToStr(Ord(AValue[LIndex])) + ')';
      end;
      LStart := LIndex + 1;
    end;
  end;
  LRun := StringReplace(Copy(AValue, LStart, MaxInt), '''', '''''', [rfReplaceAll]);

  if Result = '' then
  begin
    Result := '''' + LRun + '''';
  end
  else if LRun <> '' then
  begin
    Result := Result + ' + TNyxText(''' + LRun + ''')';
  end;
end;

function PascalDate(const AText: TNyxText): TNyxText;
var
  LDate: TNyxCalendarDate;
begin
  LDate := TNyxCalendarDate.FromText(AText);

  if not LDate.Defined then
  begin
    Exit('NyxNoDate');
  end;
  Result := 'NyxDate(' + TNyxText(IntToStr(LDate.Year)) + ', ' +
    TNyxText(IntToStr(LDate.Month)) + ', ' + TNyxText(IntToStr(LDate.Day)) + ')';
end;

function PascalRGB(const AText: TNyxText): TNyxText;
var
  LColor: TNyxRGBColor;
begin
  LColor := TNyxRGBColor.FromText(AText);

  if not LColor.Defined then
  begin
    Exit('NyxNoColor');
  end;
  { Ordinary numeric authoring is readable and canonical. Imported mixed/upper
    case remains an explicit typed wire constructor, never a lossy rewrite. }

  if NyxRGB(LColor.Red, LColor.Green, LColor.Blue).ToText <> AText then
  begin
    Exit('TNyxRGBColor.FromText(' + PascalString(AText) + ')');
  end;
  Result := 'NyxRGB(' + TNyxText(IntToStr(LColor.Red)) + ', ' +
    TNyxText(IntToStr(LColor.Green)) + ', ' + TNyxText(IntToStr(LColor.Blue)) + ')';
end;

function PascalImage(const AText: TNyxText): TNyxText;
var
  LImage: TNyxImageSource;
begin
  LImage := TNyxImageSource.FromWire(AText);
  case LImage.Kind of
    nisEmpty: Result := 'NyxNoImage';
    nisLocation: Result := 'NyxImage(NyxImageLocation(' + PascalString(AText) + '))';
    nisEmbedded:
      begin
        Result := 'NyxEmbeddedImage(' + NyxImageFormatSymbol(LImage.Format) + ', ' +
          PascalString(LImage.Encoded) + ')';

        if NyxEmbeddedImage(LImage.Format, LImage.Encoded).ToWire <> AText then
        begin
          Result := 'TNyxImageSource.FromWire(' + PascalString(AText) + ')';
        end;
      end;
  end;
end;

function PascalResourceValue(const AValue: TNyxResourceValueRef): TNyxText;
const
  CMethods: array[TNyxStateKind] of TNyxText =
    ('AsText', 'AsBoolean', 'AsInteger', 'AsNumber');
var
  LSteps: TNyxDataValue;
  LStep: TNyxDataValue;
  LIndex: Integer;

  function Locale(const ALocale: TNyxLocaleRef): TNyxText;
  begin

    if not ALocale.Defined then
    begin
      Exit('NyxDefaultLocale');
    end;
    Result := 'NyxLocale(' + PascalString(ALocale.Name) + ')';
  end;

begin
  Result := 'NyxResourceValue(NyxResourceRef(' + PascalString(AValue.Reference.Name) + '))';
  LSteps := AValue.Path.ToData;
  for LIndex := 0 to LSteps.Count - 1 do
  begin
    LStep := LSteps.Item(LIndex);

    if LStep.Kind = ndText then
    begin
      Result := Result + '.Field(' + PascalString(LStep.AsText) + ')';
    end
    else
    begin
      Result := Result + '.Item(' + TNyxText(IntToStr(LStep.AsInteger)) + ')';
    end;
  end;

  if AValue.Locale.Defined then
  begin
    Result := Result + '.Localize(' + Locale(AValue.Locale) + ', ' + Locale(AValue.Fallback) + ')';
  end;
  Result := Result + '.' + CMethods[AValue.Kind];
end;

function PascalResourceCache(const APolicy: TNyxResourceCachePolicy;
  const AIndent: TNyxText): TNyxText;
const
  CModes: array[TNyxResourceCacheMode] of TNyxText = ('Bypass', 'Memory', 'Persistent');
  CServers: array[TNyxResourceServerPolicy] of TNyxText = ('rcspRespect', 'rcspOverride');
begin
  APolicy.Validate;
  { Each choice has its own readable line. These are ordinary fluent calls;
    neither replay nor application behavior depends on whitespace. }
  Result := 'NyxResourceCache' + #10 + AIndent + '  .' + CModes[APolicy.Mode] +
    #10 + AIndent + '  .FreshFor(' + TNyxText(IntToStr(APolicy.FreshSeconds)) + ')' +
    #10 + AIndent + '  .StaleFor(' + TNyxText(IntToStr(APolicy.StaleSeconds)) + ')' +
    #10 + AIndent + '  .MaximumBytes(' + TNyxText(IntToStr(APolicy.ByteLimit)) + ')' +
    #10 + AIndent + '  .ServerPolicy(' + CServers[APolicy.Server] + ')';
end;

function PascalResource(const ADefinition: INyxResourceDefinition;
  const AIndent: TNyxText): TNyxText;
const
  CKinds: array[TNyxResourceKind] of TNyxText = ('nrkImage', 'nrkJSON', 'nrkText', 'nrkBinary');
var
  LContent: TNyxText;
begin

  if ADefinition.Source.Kind = rskHosted then
  begin
    Result := 'NyxHostedResource(' + CKinds[ADefinition.Kind] + ', NyxResourceURL(' +
      PascalString(ADefinition.Source.URL.Address) + '))' +
      #10 + AIndent + '  .Cache(' +
        PascalResourceCache(ADefinition.Source.CachePolicy, AIndent + '  ') + ')';

    if ADefinition.FallbackDefinition <> nil then
    begin
      Result := Result + #10 + AIndent + '  .Fallback(' +
        PascalResource(ADefinition.FallbackDefinition, AIndent + '  ') + ')';
    end;

    if (ADefinition.Title <> '') or (ADefinition.Description <> '') then
    begin
      Result := Result + #10 + AIndent + '  .Describe(' + PascalString(ADefinition.Title) + ',' +
        #10 + AIndent + '    ' + PascalString(ADefinition.Description) + ')';
    end;
    Exit;
  end;
  LContent := ADefinition.ToData.Field('content').AsText;
  case ADefinition.Kind of
    nrkImage:
      begin
        Result := 'NyxImageResource(' + PascalImage(LContent) + ')';
      end;
    nrkJSON:
      begin
        Result := 'NyxJSONResource(' + PascalString(LContent) + ')';
      end;
    nrkText:
      begin
        Result := 'NyxTextResource(' + PascalString(LContent) + ')';
      end;
    nrkBinary:
      begin
        Result := 'NyxBinaryResource(NyxDecodeBase64(' + PascalString(LContent) + '))';
      end;
  end;

  if (ADefinition.Title <> '') or (ADefinition.Description <> '') then
  begin
    Result := Result + #10 + AIndent + '  .Describe(' + PascalString(ADefinition.Title) + ',' +
      #10 + AIndent + '    ' + PascalString(ADefinition.Description) + ')';
  end;
end;

function PascalTime(const AText: TNyxText): TNyxText;
var
  LTime: TNyxClockTime;
  LNatural: TNyxClockTime;
begin
  LTime := TNyxClockTime.FromText(AText);

  if not LTime.Defined then
  begin
    Exit('NyxNoTime');
  end;
  Result := 'NyxTime(' + TNyxText(IntToStr(LTime.Hour)) + ', ' +
    TNyxText(IntToStr(LTime.Minute));

  if (LTime.Second <> 0) or (LTime.Millisecond <> 0) then
  begin
    Result := Result + ', ' + TNyxText(IntToStr(LTime.Second));
  end;

  if LTime.Millisecond <> 0 then
  begin
    Result := Result + ', ' + TNyxText(IntToStr(LTime.Millisecond));
  end;
  Result := Result + ')';
  LNatural := NyxTime(LTime.Hour, LTime.Minute, LTime.Second, LTime.Millisecond);
  { Explicit zero seconds/fractions and shorter fractional spellings are part
    of the authored wire. Preserve them with a closed enum, without text parsing
    in ordinary generated authoring or silently normalizing the accepted pair. }

  if LTime.Precision <> LNatural.Precision then
  begin
    Result := Result + '.WithPrecision(' + NyxTimePrecisionPascal(LTime.Precision) + ')';
  end;
end;

function HasDataField(const AData: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function PascalEventReference(const AName: TNyxText): TNyxText;
var
  LSemantic: TNyxSemanticEvent;
begin
  { Default actions are enum-valued Pascal. Exact open names and deliberately
    suppressed empty physical names retain their explicit reference boundary. }

  if TryNyxSemantic(AName, LSemantic) then
  begin
    Exit('NyxSemantic(' + NyxSemanticSymbol(LSemantic) + ')');
  end;
  Result := 'NyxEvent(' + PascalString(AName) + ')';
end;

function ConfigurationCall(ANode: TNyxNode; const AKey, AValue: TNyxText;
  ACalendarValue: Boolean = False; AClockValue: Boolean = False;
  ARGBValue: Boolean = False): TNyxText;
const
  CAttributeSymbols: array[TNyxAttribute] of TNyxText = (
    'atText', 'atValue', 'atPlaceholder', 'atItems', 'atHint', 'atAccessibleName',
    'atHref', 'atSource', 'atAlt', 'atLayout', 'atPadding', 'atGap', 'atColumns',
    'atWidth', 'atHeight', 'atLeft', 'atTop', 'atFlex', 'atMinimum', 'atMaximum',
    'atEnabled', 'atVisible', 'atReadOnly', 'atSurface', 'atCompound', 'atPressed',
    'atVariant', 'atAction', 'atProjection', 'atOverrideMode', 'atInputType',
    'atPart', 'atTarget', 'atComponent', 'atEmit', 'atEmitChange', 'atOption',
    'atPath', 'atDesignID', 'atSplitOrientation', 'atSplitPosition',
    'atSplitMinimum', 'atSplitMaximum', 'atSplitResizable',
    'atDragSource', 'atDropTarget', 'atTouchBehavior', 'atFlowWrap',
    'atCrossAlignment', 'atJustification', 'atWidthSizing', 'atHeightSizing',
    'atMinimumWidth', 'atMaximumWidth', 'atMinimumHeight', 'atMaximumHeight',
    'atQueryContainer', 'atContainerContainment', 'atSliderIntervals',
    'atImageFit', 'atImageHorizontal', 'atImageVertical');
  CTouchSymbols: array[TNyxTouchBehavior] of TNyxText =
    ('ntbAutomatic', 'ntbNone', 'ntbPanX', 'ntbPanY', 'ntbManipulation');
  CWrapSymbols: array[TNyxFlowWrap] of TNyxText = ('nfwAutomatic', 'nfwNoWrap', 'nfwWrap');
  CCrossSymbols: array[TNyxCrossAlignment] of TNyxText =
    ('ncaAutomatic', 'ncaStart', 'ncaCenter', 'ncaEnd', 'ncaStretch');
  CJustificationSymbols: array[TNyxJustification] of TNyxText =
    ('njStart', 'njCenter', 'njEnd', 'njSpaceBetween', 'njSpaceAround', 'njSpaceEvenly');
  CSizingSymbols: array[TNyxSizing] of TNyxText = ('nsAutomatic', 'nsContent', 'nsFill');
var
  LAttribute: TNyxAttribute;
  LNumber: Integer;
  LKind: TNyxKind;
  LLayout: TNyxLayoutMode;
  LWrap: TNyxFlowWrap;
  LCross: TNyxCrossAlignment;
  LJustification: TNyxJustification;
  LSizing: TNyxSizing;
  LVariant: TNyxVariant;
  LAction: TNyxAction;
  LMode: TNyxOverrideMode;
  LInputType: TNyxInputType;
  LTouch: TNyxTouchBehavior;
  LContainment: TNyxContainerContainment;
  LMethod: TNyxText;
  LDomain: TNyxValueDomain;
  LData: TNyxDataValue;
  LOwner: TNyxNode;
  LNumberText: TNyxText;
begin

  if not TryNyxAttribute(AKey, LAttribute) then
  begin
    Exit('Extension(' + PascalString(AKey) + ', ' + PascalString(AValue) + ')');
  end;

  LMethod := '';

  if (LAttribute in [atSplitOrientation, atSplitPosition, atSplitMinimum,
    atSplitMaximum, atSplitResizable, atQueryContainer, atContainerContainment]) and (AValue = '') then
  begin
    Exit('Clear(' + CAttributeSymbols[LAttribute] + ')');
  end;
  case LAttribute of
    atSplitOrientation:
      begin

        if AValue = 'stacked' then
        begin
          Exit('SplitOrientation(nsoStacked)');
        end;

        if AValue = 'side-by-side' then
        begin
          Exit('SplitOrientation(nsoSideBySide)');
        end;
      end;
    atSplitPosition, atSplitMinimum, atSplitMaximum:
      begin
        LMethod := 'SplitPosition';

        if LAttribute = atSplitMinimum then
        begin
          LMethod := 'SplitMinimum';
        end;

        if LAttribute = atSplitMaximum then
        begin
          LMethod := 'SplitMaximum';
        end;
        Exit(LMethod + '(' + AValue + ')');
      end;
    atDragSource, atDropTarget:
      begin
        LMethod := 'DragSource';

        if LAttribute = atDropTarget then
        begin
          LMethod := 'DropTarget';
        end;

        if AValue = '' then
        begin
          Exit('Clear(' + CAttributeSymbols[LAttribute] + ')');
        end;

        if AValue = 'true' then
        begin
          Exit(LMethod + '(True)');
        end;
        Exit(LMethod + '(False)');
      end;
    atTouchBehavior:
      begin
        for LTouch := Low(TNyxTouchBehavior) to High(TNyxTouchBehavior) do
        begin

          if AValue = NyxTouchBehaviorName(LTouch) then
          begin
            Exit('TouchBehavior(' + CTouchSymbols[LTouch] + ')');
          end;
        end;
        Exit('Clear(atTouchBehavior)');
      end;
    atSplitResizable:
      begin
        if AValue = 'true' then
        begin
          Exit('SplitResizable(True)');
        end;
        Exit('SplitResizable(False)');
      end;
    atText: LMethod := 'Text';
    atPlaceholder: LMethod := 'Placeholder';
    atItems: LMethod := 'Items';
    atHint: LMethod := 'Hint';
    atAccessibleName: LMethod := 'AccessibleName';
    atHref: LMethod := 'LinkTo';
    atSource:
      begin
        Exit('Source(' + PascalImage(AValue) + ')');
      end;
    atImageFit:
      begin

        if AValue <> '' then
        begin
          Exit('ImageFit(' + NyxImageFitSymbol(ReadNyxImageFit(AValue)) + ')');
        end;
      end;
    atImageHorizontal, atImageVertical:
      begin

        if AValue <> '' then
        begin
          LMethod := 'ImageHorizontal';

          if LAttribute = atImageVertical then
          begin
            LMethod := 'ImageVertical';
          end;
          Exit(LMethod + '(' + NyxImageAnchorSymbol(ReadNyxImageAnchor(AValue)) + ')');
        end;
      end;
    atAlt: LMethod := 'AlternativeText';
    else
      begin
        { Other attributes continue through their typed families below. }
      end;
  end;

  if LMethod <> '' then
  begin
    Exit(LMethod + '(' + PascalString(AValue) + ')');
  end;

  if (LAttribute = atValue) and
    ((ANode.ProjectionKind = 'date') or ACalendarValue) then
  begin
    Exit('Value(' + PascalDate(AValue) + ')');
  end;

  if LAttribute = atValue then
  begin
    LDomain := NyxNodeValueDomain(ANode);

    if (ANode.ProjectionKind = 'color') or ARGBValue or
      (LDomain.Defined and LDomain.RGBColor) then
    begin
      Exit('Value(' + PascalRGB(AValue) + ')');
    end;

    if (ANode.ProjectionKind = 'time') or AClockValue or
      (LDomain.Defined and LDomain.ClockTime) then
    begin
      Exit('Value(' + PascalTime(AValue) + ')');
    end;
  end;

  if (AValue = '') and (LAttribute = atValue) then
  begin
    LDomain := NyxNodeValueDomain(ANode);

    if LDomain.Defined and (LDomain.Kind <> nskText) then
    begin
      Exit('Clear(atValue)');
    end;
    Exit('Value('''')');
  end;

  if (AValue = '') and (LAttribute = atVariant) then
  begin
    Exit('Variant(nvDefault)');
  end;

  if AValue = '' then
  begin
    Exit('Clear(' + CAttributeSymbols[LAttribute] + ')');
  end;
  case LAttribute of
    atOption, atValue:
      begin
        LOwner := ANode;

        if LAttribute = atOption then
        begin
          LOwner := ANode.Parent;
          while (LOwner <> nil) and (LOwner.Prop('compound') <> 'true') do
          begin
            LOwner := LOwner.Parent;
          end;
        end;
        LDomain := NyxNoDomain;

        if LOwner <> nil then
        begin
          LDomain := NyxNodeValueDomain(LOwner);
        end;
        LMethod := 'Value';

        if LAttribute = atOption then
        begin
          LMethod := 'Option';
        end;

        if LDomain.Defined then
        begin
          LData := LDomain.ReadWire(AValue);
          case LDomain.Kind of
            nskBoolean:
              begin

                if LData.AsBoolean then
                begin
                  Exit(LMethod + '(True)');
                end;
                Exit(LMethod + '(False)');
              end;
            nskInteger:
              begin

                if IntToStr(LData.AsInteger) <> AValue then
                begin
                  Exit('Metadata(' + CAttributeSymbols[LAttribute] + ', ' + PascalString(AValue) + ')');
                end;
                Exit(LMethod + '(' + PascalInteger(LData.AsInteger) + ')');
              end;
            nskNumber:
              begin
                LNumberText := NyxStateNumberText(LData.AsNumber);

                if LNumberText <> AValue then
                begin
                  Exit('Metadata(' + CAttributeSymbols[LAttribute] + ', ' + PascalString(AValue) + ')');
                end;

                if (Pos('.', LNumberText) = 0) and (Pos('e', LNumberText) = 0) then
                begin
                  LNumberText := LNumberText + '.0';
                end;
                Exit(LMethod + '(' + LNumberText + ')');
              end;
            nskText:
              begin
                { The shared PascalString path below retains exact text. }
              end;
          end;
        end;
        Exit(LMethod + '(' + PascalString(AValue) + ')');
      end;
    atPadding: LMethod := 'Padding';
    atGap: LMethod := 'Gap';
    atColumns: LMethod := 'Columns';
    atWidth: LMethod := 'Width';
    atHeight: LMethod := 'Height';
    atMinimumWidth: LMethod := 'MinimumWidth';
    atMaximumWidth: LMethod := 'MaximumWidth';
    atMinimumHeight: LMethod := 'MinimumHeight';
    atMaximumHeight: LMethod := 'MaximumHeight';
    atLeft: LMethod := 'Left';
    atTop: LMethod := 'Top';
    atFlex: LMethod := 'Flex';
    atMinimum: LMethod := 'Minimum';
    atMaximum: LMethod := 'Maximum';
    atSliderIntervals: LMethod := 'SliderIntervals';
    else
      begin
        { This attribute does not use a numeric configuration method. }
      end;
  end;

  if LMethod <> '' then
  begin

    if TryStrToInt(AValue, LNumber) and (IntToStr(LNumber) = AValue) then
    begin
      Exit(LMethod + '(' + AValue + ')');
    end;
  end;
  LMethod := '';
  case LAttribute of
    atEnabled: LMethod := 'Enabled';
    atVisible: LMethod := 'Visible';
    atReadOnly: LMethod := 'ReadOnly';
    atSurface: LMethod := 'Surface';
    atCompound: LMethod := 'Compound';
    atPressed: LMethod := 'Pressed';
    else
      begin
        { This attribute does not use a Boolean configuration method. }
      end;
  end;

  if LMethod <> '' then
  begin

    if AValue = 'true' then
    begin
      Exit(LMethod + '(True)');
    end;

    if AValue = 'false' then
    begin
      Exit(LMethod + '(False)');
    end;
  end;
  case LAttribute of
    atQueryContainer:
      begin
        Exit('QueryContainer(' + NyxContainer(AValue).Pascal + ')');
      end;
    atContainerContainment:
      begin

        if TryNyxContainerContainment(AValue, LContainment) then
        begin
          Exit('Containment(' + NyxContainerContainmentSymbol(LContainment) + ')');
        end;
      end;
    atLayout:
      begin
        for LLayout := Low(TNyxLayoutMode) to High(TNyxLayoutMode) do
        begin

          if NyxLayoutName(LLayout) = AValue then
          begin
            Exit('Layout(nl' + ControlStem(AValue) + ')');
          end;
        end;
      end;
    atFlowWrap:
      begin
        for LWrap := Low(TNyxFlowWrap) to High(TNyxFlowWrap) do
        begin

          if NyxFlowWrapName(LWrap) = AValue then
          begin
            Exit('Wrap(' + CWrapSymbols[LWrap] + ')');
          end;
        end;
      end;
    atCrossAlignment:
      begin
        for LCross := Low(TNyxCrossAlignment) to High(TNyxCrossAlignment) do
        begin

          if NyxCrossAlignmentName(LCross) = AValue then
          begin
            Exit('Align(' + CCrossSymbols[LCross] + ')');
          end;
        end;
      end;
    atJustification:
      begin
        for LJustification := Low(TNyxJustification) to High(TNyxJustification) do
        begin

          if NyxJustificationName(LJustification) = AValue then
          begin
            Exit('Justify(' + CJustificationSymbols[LJustification] + ')');
          end;
        end;
      end;
    atWidthSizing, atHeightSizing:
      begin
        LMethod := 'WidthSizing';

        if LAttribute = atHeightSizing then
        begin
          LMethod := 'HeightSizing';
        end;
        for LSizing := Low(TNyxSizing) to High(TNyxSizing) do
        begin

          if NyxSizingName(LSizing) = AValue then
          begin
            Exit(LMethod + '(' + CSizingSymbols[LSizing] + ')');
          end;
        end;
      end;
    atVariant:
      begin
        for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
        begin

          if NyxVariantName(LVariant) = AValue then
          begin
            Exit('Variant(nv' + ControlStem(AValue) + ')');
          end;
        end;
        Exit('CustomVariant(NyxStyle(' + PascalString(AValue) + '))');
      end;
    atProjection:
      begin

        if TryNyxKind(AValue, LKind) then
        begin
          Exit('ProjectAs(nk' + ControlStem(AValue) + ')');
        end;
        Exit('CustomProjection(NyxCustomKind(' + PascalString(AValue) + '))');
      end;
    atAction:
      begin
        for LAction := Low(TNyxAction) to High(TNyxAction) do
        begin

          if NyxActionName(LAction) = AValue then
          begin
            Exit('Action(na' + ControlStem(AValue) + ')');
          end;
        end;
      end;
    atOverrideMode:
      begin
        for LMode := Low(TNyxOverrideMode) to High(TNyxOverrideMode) do
        begin

          if NyxOverrideName(LMode) = AValue then
          begin
            Exit('OverrideMode(no' + ControlStem(AValue) + ')');
          end;
        end;
      end;
    atInputType:
      begin
        for LInputType := Low(TNyxInputType) to High(TNyxInputType) do
        begin

          if NyxInputTypeName(LInputType) = AValue then
          begin
            Exit('InputType(ni' + ControlStem(AValue) + ')');
          end;
        end;
      end;
    atPart: Exit('PartName(NyxPart(' + PascalString(AValue) + '))');
    atTarget: Exit('Target(NyxPart(' + PascalString(AValue) + '))');
    atPath: Exit('OverridePath(NyxPart(' + PascalString(AValue) + '))');
    atComponent: Exit('Component(NyxComponent(' + PascalString(AValue) + '))');
    atEmit: Exit('OnClick(' + PascalEventReference(AValue) + ')');
    atEmitChange: Exit('OnChange(' + PascalEventReference(AValue) + ')');
    else
      begin
        { Preserve legacy metadata through the explicit enum boundary below. }
      end;
  end;
  { Legacy noncanonical values stay byte-for-byte portable at an explicit key
    enum boundary. The normal built-in configuration path above emits typed
    Boolean/integer/enum/reference arguments; extension text is never guessed. }
  Result := 'Metadata(' + CAttributeSymbols[LAttribute] + ', ' + PascalString(AValue) + ')';
end;

class procedure TNyxCodegen.AdmitUnitName(const AName: TNyxText);
var
  LParts: TStringList;
  LIndex: Integer;
  LWord: TNyxText;
begin

  if (AName = '') or (Length(AName) > 120) or (AName[1] = '.') or
    (AName[Length(AName)] = '.') or (Pos('..', AName) > 0) then
  begin
    raise ENyxModel.Create('Invalid generated unit name');
  end;
  LParts := TStringList.Create;
  try
    LParts.Delimiter := '.';
    LParts.StrictDelimiter := True;
    LParts.DelimitedText := AName;
    for LIndex := 0 to LParts.Count - 1 do
    begin
      LWord := LowerCase(LParts[LIndex]);

      if not IsValidIdent(LWord) or
        (Pos('|' + LWord + '|',
          '|unit|program|begin|end|interface|implementation|uses|type|var|const|' +
          'class|record|function|procedure|if|then|else|while|do|for|in|case|' +
          'try|finally|except|raise|nil|true|false|and|or|not|div|mod|array|' +
          'of|object|property|constructor|destructor|inherited|repeat|until|' +
          'initialization|finalization|packed|set|with|goto|label|file|') > 0) then
      begin
        raise ENyxModel.Create('Invalid generated unit name');
      end;
    end;
  finally
    LParts.Free;
  end;
end;

class function TNyxCodegen.Generate(ADocument: TNyxDocument;
  const AUnitName: TNyxText): TNyxText;
const
  CStateFactories: array[TNyxStateKind] of TNyxText = (
    'NyxTextState', 'NyxBooleanState', 'NyxIntegerState', 'NyxNumberState');
var
  LLines: TNyxStrings;
  LNodes: array of TNyxNode;
  LVariables: array of TNyxText;
  LUsedVariables: TStringList;
  LIndex: Integer;
  LNextNode: Integer;
  LStateVariables: array of TNyxText;
  LStateValue: TNyxStateValue;
  LKind: TNyxKind;
  LHasSavedTypeAhead: Boolean;

  procedure EmitData(AData: TJSONData; const APrefix, ATail: TNyxText;
    AIndent: Integer);
  var
    LItemIndex: Integer;
    LPrefix: TNyxText;
    LTail: TNyxText;
    LExpression: TNyxText;
    LNumber: Integer;
    LNumberText: TNyxText;
  begin
    { Structured data stays ordinary typed Pascal, with nested construction
      blocks instead of a serialized JSON blob. Exact decimal references are
      used where a numeric literal would erase the admitted representation. }
    case AData.JSONType of
      jtObject, jtArray:
        begin
          LExpression := 'NyxArray';

          if AData.JSONType = jtObject then
          begin
            LExpression := 'NyxObject';
          end;

          if AData.Count = 0 then
          begin
            LLines.Add(APrefix + LExpression + '([])' + ATail);
            Exit;
          end;
          LLines.Add(APrefix + LExpression + '([');
          for LItemIndex := 0 to AData.Count - 1 do
          begin
            LPrefix := StringOfChar(' ', AIndent + 2);
            LTail := '';

            if AData.JSONType = jtObject then
            begin
              LPrefix := LPrefix + 'NyxField(' +
                PascalString(TJSONObject(AData).Names[LItemIndex]) + ', ';
              LTail := ')';
            end;

            if LItemIndex < AData.Count - 1 then
            begin
              LTail := LTail + ',';
            end;
            EmitData(AData.Items[LItemIndex], LPrefix, LTail, AIndent + 2);
          end;
          LLines.Add(StringOfChar(' ', AIndent) + '])' + ATail);
          Exit;
        end;
      jtString:
        begin
          LExpression := 'NyxData(' + PascalString(AData.AsString) + ')';
        end;
      jtBoolean:
        begin
          LExpression := 'NyxData(False)';

          if AData.AsBoolean then
          begin
            LExpression := 'NyxData(True)';
          end;
        end;
      jtNull:
        begin
          LExpression := 'NyxNull';
        end;
      jtNumber:
        begin
          LNumberText := AData.AsJSON;
          LExpression := 'NyxData(NyxDecimal(' + PascalString(LNumberText) + '))';

          if TryNyxInteger(LNumberText, LNumber) and
            (IntToStr(LNumber) = LNumberText) then
          begin
            LExpression := 'NyxData(' + PascalInteger(LNumber) + ')';
          end;
        end;
      else
        begin
          raise ENyxModel.Create('Unsupported extension data kind');
        end;
    end;
    LLines.Add(APrefix + LExpression + ATail);
  end;

  function DomainExpression(const ADomain: TNyxValueDomain): TNyxText;
  const
    CFactories: array[TNyxStateKind] of TNyxText =
      ('NyxTextDomain', 'NyxBooleanDomain', 'NyxIntegerDomain', 'NyxNumberDomain');
  var
    LData: TNyxDataValue;
    LChoices: TNyxDataValue;
    LIndex: Integer;
    LPart: TNyxText;
    LChoiceIndex: Integer;

    function Literal(const AValue: TNyxDataValue): TNyxText;
    begin
      case ADomain.Kind of
        nskText:
          begin
            Result := PascalString(AValue.AsText);

            if ADomain.CalendarDate then
            begin
              Result := PascalDate(AValue.AsText);
            end;

            if ADomain.ClockTime then
            begin
              Result := PascalTime(AValue.AsText);
            end;

            if ADomain.RGBColor then
            begin
              Result := PascalRGB(AValue.AsText);
            end;
          end;
        nskBoolean:
          begin
            Result := 'False';

            if AValue.AsBoolean then
            begin
              Result := 'True';
            end;
          end;
        nskInteger: Result := PascalInteger(AValue.AsInteger);
        nskNumber: Result := NyxStateNumberText(AValue.AsNumber);
      end;
    end;

  begin
    LData := ADomain.ToData;
    Result := CFactories[ADomain.Kind];

    if ADomain.CalendarDate then
    begin
      Result := 'NyxDateDomain';
    end;

    if ADomain.ClockTime then
    begin
      Result := 'NyxTimeDomain';
    end;

    if ADomain.RGBColor then
    begin
      Result := 'NyxRGBDomain';
    end;
    for LIndex := 0 to LData.Count - 1 do
    begin

      if LData.Key(LIndex) = 'min' then
      begin

        if ADomain.ClockTime and not HasDataField(LData, 'max') then
        begin
          Result := Result + '.Minimum(' + Literal(LData.Field('min')) + ')';
        end
        else
        begin
          Result := Result + '.Range(' + Literal(LData.Field('min')) + ', ' +
            Literal(LData.Field('max')) + ')';
        end;
      end;

      if ADomain.ClockTime and (LData.Key(LIndex) = 'max') and
        not HasDataField(LData, 'min') then
      begin
        Result := Result + '.Maximum(' + Literal(LData.Field('max')) + ')';
      end;

      if ADomain.ClockTime and (LData.Key(LIndex) = 'step') then
      begin

        if ADomain.TimeStepMilliseconds = 0 then
        begin
          Result := Result + '.AnyStep';
        end
        else
        begin
          Result := Result + '.StepMilliseconds(' +
            PascalInteger(ADomain.TimeStepMilliseconds) + ')';
        end;
      end;

      if LData.Key(LIndex) = 'choices' then
      begin
        LChoices := LData.Field('choices');
        LPart := '';
        for LChoiceIndex := 0 to LChoices.Count - 1 do
        begin

          if LChoiceIndex > 0 then
          begin
            LPart := LPart + ', ';
          end;
          LPart := LPart + Literal(LChoices.Item(LChoiceIndex));
        end;
        Result := Result + '.Choices([' + LPart + '])';
      end;
    end;
  end;

  {$I nyx.codegen.collections.inc}
  {$I nyx.codegen.menus.inc}

  function AuthoredTimeDomainData(const ADomain: TNyxValueDomain): TNyxDataValue;
  var
    LData: TNyxDataValue;
    LDomain: TNyxTimeDomain;
    LChoices: array of TNyxClockTime;
    LIndex: Integer;
    LChoiceIndex: Integer;
  begin
    Result := NyxNull;
    LData := ADomain.ToData;
    LDomain := NyxTimeDomain;
    { Fingerprint the actual typed chain, including every intermediate admission.
      A valid imported final domain may order choices/step before the minimum
      defining its step base. If that chain refuses, EmitContract preserves the
      complete admitted descriptor through its existing atomic Metadata boundary.
      Reordering it would lose exact representation; ignoring the refusal would
      emit Pascal that compiles but fails while building the application. }
    try
      for LIndex := 0 to LData.Count - 1 do
      begin

        if LData.Key(LIndex) = 'min' then
        begin

          if HasDataField(LData, 'max') then
          begin
            LDomain := LDomain.Range(TNyxClockTime.FromText(LData.Field('min').AsText),
              TNyxClockTime.FromText(LData.Field('max').AsText));
          end
          else
          begin
            LDomain := LDomain.Minimum(TNyxClockTime.FromText(LData.Field('min').AsText));
          end;
        end;

        if (LData.Key(LIndex) = 'max') and not HasDataField(LData, 'min') then
        begin
          LDomain := LDomain.Maximum(TNyxClockTime.FromText(LData.Field('max').AsText));
        end;

        if LData.Key(LIndex) = 'step' then
        begin

          if ADomain.TimeStepMilliseconds = 0 then
          begin
            LDomain := LDomain.AnyStep;
          end
          else
          begin
            LDomain := LDomain.StepMilliseconds(ADomain.TimeStepMilliseconds);
          end;
        end;

        if LData.Key(LIndex) = 'choices' then
        begin
          SetLength(LChoices, LData.Field('choices').Count);
          for LChoiceIndex := 0 to High(LChoices) do
          begin
            LChoices[LChoiceIndex] := TNyxClockTime.FromText(
              LData.Field('choices').Item(LChoiceIndex).AsText);
          end;
          LDomain := LDomain.Choices(LChoices);
        end;
      end;
      Result := LDomain.Definition.ToData;
    except
      on LException: ENyxContract do
      begin
        { Null is a fingerprint mismatch, never an emitted domain replacement. }
        Result := NyxNull;
      end;
    end;
  end;

  function AuthoredDomainData(const ADomain: TNyxValueDomain): TNyxDataValue;
  var
    LData: TNyxDataValue;
    LFields: array of TNyxDataField;
    LItems: array of TNyxDataValue;
    LIndex: Integer;
    LChoiceIndex: Integer;

    procedure Add(const AName: TNyxText; const AValue: TNyxDataValue);
    var
      LNext: Integer;
    begin
      LNext := Length(LFields);
      SetLength(LFields, LNext + 1);
      LFields[LNext] := NyxField(AName, AValue);
    end;

    function Scalar(const AValue: TNyxDataValue): TNyxDataValue;
    begin
      case ADomain.Kind of
        nskText: Result := NyxData(AValue.AsText);
        nskBoolean: Result := NyxData(AValue.AsBoolean);
        nskInteger: Result := NyxData(AValue.AsInteger);
        nskNumber: Result := NyxData(AValue.AsNumber);
      end;
    end;

  begin
    Result := NyxNull;

    if not ADomain.Defined then
    begin
      Exit;
    end;

    if ADomain.ClockTime then
    begin
      Exit(AuthoredTimeDomainData(ADomain));
    end;
    LData := ADomain.ToData;
    LFields := nil;
    Add('type', LData.Field('type'));
    { Canonical domain data must retain format during authored fingerprinting;
      otherwise a date would collapse to unconstrained text after generation. }

    if ADomain.CalendarDate or ADomain.RGBColor then
    begin
      Add('format', LData.Field('format'));
    end;
    for LIndex := 0 to LData.Count - 1 do
    begin

      if LData.Key(LIndex) = 'min' then
      begin
        Add('min', Scalar(LData.Field('min')));
        Add('max', Scalar(LData.Field('max')));
      end;

      if LData.Key(LIndex) = 'choices' then
      begin
        SetLength(LItems, LData.Field('choices').Count);
        for LChoiceIndex := 0 to Length(LItems) - 1 do
        begin
          LItems[LChoiceIndex] := Scalar(LData.Field('choices').Item(LChoiceIndex));
        end;
        Add('choices', NyxArray(LItems));
      end;
    end;
    Result := NyxObject(LFields);
  end;

  function AuthoredContractData(AContract: TNyxContract): TNyxDataValue;
  const
    CSources: array[TNyxEventValueSource] of TNyxText =
      ('none', 'target', 'origin', 'source', 'part');
  var
    LData: TNyxDataValue;
    LFields: array of TNyxDataField;
    LEventFields: array of TNyxDataField;
    LItems: array of TNyxDataValue;
    LIndex: Integer;
    LItemIndex: Integer;
    LField: TNyxFieldContract;
    LEvent: TNyxEventContract;

    procedure Add(const AName: TNyxText; const AValue: TNyxDataValue);
    var
      LNext: Integer;
    begin
      LNext := Length(LFields);
      SetLength(LFields, LNext + 1);
      LFields[LNext] := NyxField(AName, AValue);
    end;

  begin
    LData := AContract.Snapshot;
    LFields := nil;
    Add('version', NyxData(1));
    for LIndex := 0 to LData.Count - 1 do
    begin

      if LData.Key(LIndex) = 'value' then
      begin
        Add('value', AuthoredDomainData(TNyxValueDomain.FromData(LData.Field('value'))));
      end
      else if LData.Key(LIndex) = 'fields' then
      begin
        SetLength(LItems, AContract.FieldCount);
        for LItemIndex := 0 to Length(LItems) - 1 do
        begin
          LField := AContract.FieldAt(LItemIndex);
          LItems[LItemIndex] := NyxObject([
            NyxField('part', NyxData(LField.Part.Name)),
            NyxField('domain', AuthoredDomainData(LField.Domain))
          ]);
        end;

        if Length(LItems) > 0 then
        begin
          Add('fields', NyxArray(LItems));
        end;
      end
      else if LData.Key(LIndex) = 'events' then
      begin
        SetLength(LItems, AContract.EventCount);
        for LItemIndex := 0 to Length(LItems) - 1 do
        begin
          LEvent := AContract.EventAt(LItemIndex);
          SetLength(LEventFields, 3);
          LEventFields[0] := NyxField('trigger', NyxData(NyxTriggerName(LEvent.Trigger)));
          LEventFields[1] := NyxField('source', NyxData(CSources[LEvent.ValueSource.Source]));
          LEventFields[2] := NyxField('domain', AuthoredDomainData(LEvent.Domain));

          if LEvent.ValueSource.Source = nvsPart then
          begin
            SetLength(LEventFields, 4);
            LEventFields[3] := NyxField('part', NyxData(LEvent.ValueSource.Part));
          end;
          LItems[LItemIndex] := NyxObject(LEventFields);
        end;

        if Length(LItems) > 0 then
        begin
          Add('events', NyxArray(LItems));
        end;
      end;
    end;
    Result := NyxObject(LFields);
  end;

  procedure EmitContract(const AData: TNyxDataValue; const AOwner: TNyxText);
  const
    CSources: array[TNyxEventValueSource] of TNyxText =
      ('NyxNoEventValue', 'NyxTargetValue', 'NyxOriginValue', 'NyxSourceValue', '');
  var
    LStore: TNyxExtensions;
    LContract: TNyxContract;
    LIndex: Integer;
    LFieldIndex: Integer;
    LDomain: TNyxValueDomain;
    LField: TNyxFieldContract;
    LEvent: TNyxEventContract;
    LSource: TNyxText;
    LWireData: TJSONData;
  begin
    LStore := TNyxExtensions.Create(nesNode);
    LContract := nil;
    try
      LContract := TNyxContract.Create(LStore);
      LContract.Metadata(AData);
      LLines.Add('    ' + AOwner + '.Contract');
      { A typed factory reconstructs its canonical descriptor. An imported
        descriptor may also retain empty arrays, member order or decimal
        spelling. Keep those explicitly at the metadata boundary; never silently
        rewrite accepted design data merely to make the source prettier. }

      if (AuthoredContractData(LContract).ToJSON <> AData.ToJSON) or
        (AData.Count = 1) then
      begin
        LWireData := DecodeNyxJSON(AData.ToJSON);
        try
          EmitData(LWireData, '      .Metadata(', ');', 6);
        finally
          LWireData.Free;
        end;
        Exit;
      end;
      { Preserve declaration order inside the owned namespace. Readers expose
        immutable domain/field/event snapshots; source names remain ordinary
        typed constructs that can be written by hand. }
      for LIndex := 0 to AData.Count - 1 do
      begin

        if AData.Key(LIndex) = 'value' then
        begin
          LDomain := TNyxValueDomain.FromData(AData.Field('value'));

          if LDomain.Defined then
          begin
            LLines.Add('      .Value(' + DomainExpression(LDomain) + ')');
          end
          else
          begin
            LLines.Add('      .NoValue');
          end;
        end;

        if AData.Key(LIndex) = 'fields' then
        begin
          for LFieldIndex := 0 to LContract.FieldCount - 1 do
          begin
            LField := LContract.FieldAt(LFieldIndex);
            LLines.Add('      .Field(NyxPart(' + PascalString(LField.Part.Name) + '), ' +
              DomainExpression(LField.Domain) + ')');
          end;
        end;

        if AData.Key(LIndex) = 'events' then
        begin
          for LFieldIndex := 0 to LContract.EventCount - 1 do
          begin
            LEvent := LContract.EventAt(LFieldIndex);

            if not LEvent.Domain.Defined then
            begin
              LLines.Add('      .Signal(' + NyxTriggerSymbol(LEvent.Trigger) + ')');
            end
            else
            begin
              LSource := CSources[LEvent.ValueSource.Source];

              if LEvent.ValueSource.Source = nvsPart then
              begin
                LSource := 'NyxPartValue(NyxPart(' + PascalString(LEvent.ValueSource.Part) + '))';
              end;
              LLines.Add('      .On(' + NyxTriggerSymbol(LEvent.Trigger) + ', ' + LSource + ', ' +
                DomainExpression(LEvent.Domain) + ')');
            end;
          end;
        end;
      end;

      LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ';';
    finally
      LContract.Free;
      LStore.Free;
    end;
  end;

  procedure EmitCallbacks(const AData: TNyxDataValue; const AOwner: TNyxText);
  const
    CPolicies: array[TNyxExecutionPolicy] of TNyxText =
      ('neSequential', 'neAsynchronous', 'neUIQueue', 'neThreaded');
  var
    LNode: TNyxNode;
    LInfos: TNyxAuthoredEventInfos;
    LIndex: Integer;
    LCallbackIndex: Integer;
    LWire: TJSONData;
  begin
    LNode := TNyxNode.Create(nkLabel, 'callback-metadata');
    try
      NyxCallbacks(LNode).Metadata(AData);
      LInfos := NyxAuthoredEvents(LNode);

      if EncodeNyxAuthoredEvents(LInfos).ToJSON <> AData.ToJSON then
      begin
        LWire := DecodeNyxJSON(AData.ToJSON);
        try
          EmitData(LWire, '    NyxCallbacks(' + AOwner + ').Metadata(', ');', 4);
        finally
          LWire.Free;
        end;
        Exit;
      end;

      if Length(LInfos) = 0 then
      begin
        LLines.Add('    NyxCallbacks(' + AOwner + ').Clear;');
      end;
      for LIndex := 0 to High(LInfos) do
      begin

        if LInfos[LIndex].Trigger = ntNamed then
        begin
          LLines.Add('    NyxCallbacks(' + AOwner + ').OnNamed(' +
            PascalEventReference(LInfos[LIndex].Name.Name) + ')');
        end
        else
        begin
          LLines.Add('    NyxCallbacks(' + AOwner + ').' + NyxTriggerTitle(LInfos[LIndex].Trigger));
        end;
        LLines.Add('      .Policy(' + CPolicies[LInfos[LIndex].Policy] + ')');
        for LCallbackIndex := 0 to High(LInfos[LIndex].Callbacks) do
        begin
          LLines.Add('      .Add(NyxHandler(' +
            PascalString(LInfos[LIndex].Callbacks[LCallbackIndex].Handler.Name) +
            '), NyxCallbackID(' + PascalString(LInfos[LIndex].Callbacks[LCallbackIndex].ID.Name) + '))');
        end;
        LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ';';
      end;
    finally
      ReleaseNyxNode(LNode);
    end;
  end;

  procedure EmitExtensions(AExtensions: TNyxExtensions; const AOwner: TNyxText);
  var
    LData: TJSONData;
    LObject: TJSONObject;
    LFieldIndex: Integer;
    LTokens: TNyxThemeTokens;
    LValues: TNyxDataValue;
    LTokenIndex: Integer;
    LMethod: TNyxText;
    LArgument: TNyxText;
  begin

    if AExtensions.Count = 0 then
    begin
      Exit;
    end;
    LData := DecodeNyxJSON(AExtensions.ToJSON);
    try
      LObject := TJSONObject(LData);
      LLines.Add('');
      for LFieldIndex := 0 to LObject.Count - 1 do
      begin

        if (AExtensions.Scope = nesDocument) and
          (LObject.Names[LFieldIndex] = NyxDesignTokensKey) then
        begin
          LTokens := TNyxThemeTokens.FromData(
            TNyxDataValue.ParseJSON(LObject.Items[LFieldIndex].AsJSON));
          LValues := LTokens.ToData;
          LLines.Add('    { Semantic palette and logical metrics for every application view. }');
          LArgument := '    SetNyxThemeTokens(' + AOwner + ', NyxThemeTokens';

          if LValues.Count = 0 then
          begin
            LLines.Add(LArgument + ');');
          end
          else
          begin
            LLines.Add(LArgument);
            for LTokenIndex := 0 to LValues.Count - 1 do
            begin
              LMethod := LValues.Key(LTokenIndex);
              LMethod := UpperCase(Copy(LMethod, 1, 1)) + Copy(LMethod, 2, MaxInt);

              if LValues.Field(LValues.Key(LTokenIndex)).Kind = ndText then
              begin
                LArgument := PascalRGB(LValues.Field(LValues.Key(LTokenIndex)).AsText);
              end
              else
              begin
                LArgument := IntToStr(LValues.Field(LValues.Key(LTokenIndex)).AsInteger);
              end;
              LArgument := '      .' + LMethod + '(' + LArgument + ')';

              if LTokenIndex = LValues.Count - 1 then
              begin
                LArgument := LArgument + ');';
              end;
              LLines.Add(LArgument);
            end;
          end;
        end
        else if (AExtensions.Scope = nesNode) and
          (LObject.Names[LFieldIndex] = NyxContractKey) then
        begin
          EmitContract(TNyxDataValue.ParseJSON(LObject.Items[LFieldIndex].AsJSON), AOwner);
        end
        else if (AExtensions.Scope = nesNode) and
          (LObject.Names[LFieldIndex] = NyxCallbacksKey) then
        begin
          EmitCallbacks(TNyxDataValue.ParseJSON(LObject.Items[LFieldIndex].AsJSON), AOwner);
        end
        else
        begin
          EmitData(LObject.Items[LFieldIndex], '    ' + AOwner +
            '.Extensions.SetValue(NyxExtension(' + PascalString(LObject.Names[LFieldIndex]) +
            '), ', ');', 4);
        end;
      end;
    finally
      LData.Free;
    end;
  end;

  function StateVariableBase(const AKey: TNyxText; AKind: TNyxStateKind): TNyxText;
  var
    LStem: TNyxText;
    LReferenceStem: TNyxText;
    LKindStem: TNyxText;
  begin
    LStem := ControlStem(AKey);
    LReferenceStem := Copy(CStateFactories[AKind], 4, MaxInt);
    LKindStem := Copy(LReferenceStem, 1, Length(LReferenceStem) - Length('State'));
    { Handwritten names such as replyText and replyTextState already convey the
      scalar type. Preserve their purpose without emitting TextTextState or
      StateTextState; admission still uses the common collision-safe namespace. }

    if UpperCase(Copy(LStem, Length(LStem) - Length(LReferenceStem) + 1, MaxInt)) =
      UpperCase(LReferenceStem) then
    begin
      Exit('L' + LStem);
    end;

    if UpperCase(Copy(LStem, Length(LStem) - Length(LKindStem) + 1, MaxInt)) =
      UpperCase(LKindStem) then
    begin
      Exit('L' + LStem + 'State');
    end;
    Result := 'L' + LStem + LReferenceStem;
  end;

  function UniqueVariable(const ABase: TNyxText): TNyxText;
  var
    LSuffix: Integer;
  begin
    Result := ABase;
    LSuffix := 1;
    while LUsedVariables.IndexOf(Result) >= 0 do
    begin
      Inc(LSuffix);
      Result := ABase + IntToStr(LSuffix);
    end;
    LUsedVariables.Add(Result);
  end;

  function StateVariable(const AName: TNyxText): TNyxText;
  var
    LStateIndex: Integer;
  begin
    for LStateIndex := 0 to ADocument.State.Count - 1 do
    begin

      if ADocument.State.Key(LStateIndex) = AName then
      begin
        Exit(LStateVariables[LStateIndex]);
      end;
    end;
    raise ENyxModel.Create('Generated binding requires an admitted state reference');
  end;

  procedure EmitBindings(ANode: TNyxNode; const AVariable: TNyxText);
  const
    CMethods: array[TNyxBindingProperty] of TNyxText = (
      'Text', 'Value', 'Enabled', 'Visible', 'ReadOnly', 'Pressed', 'Placeholder',
      'Hint', 'AccessibleName', 'Width', 'Height', 'Left', 'Top', 'Padding', 'Gap',
      'Columns', 'Flex', 'Minimum', 'Maximum');
  var
    LBindingIndex: Integer;
    LSpec: TNyxBindingSpec;
    LCall: TNyxText;
  begin

    if (ANode.BindingCount = 0) and not ANode.HasCollectionView then
    begin
      Exit;
    end;
    LLines.Add('    ' + AVariable + '.Binds');
    for LBindingIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LBindingIndex];

      if LSpec.Cleared then
      begin
        LCall := 'Clear(bp' + CMethods[LSpec.Target] + ')';
      end
      else if LSpec.Source = bsResource then
      begin
        LCall := CMethods[LSpec.Target] + '(' + PascalResourceValue(LSpec.ResourceValue) + ')';
      end
      else
      begin
        LCall := CMethods[LSpec.Target] + '(' + StateVariable(LSpec.StateName);

        if (LSpec.Target = bpValue) and (LSpec.Direction = bdFromState) then
        begin
          LCall := LCall + ', bdFromState';
        end;
        LCall := LCall + ')';
      end;
      LLines.Add('      .' + LCall);
    end;
    EmitCollectionBinding(ANode, AVariable);
    LLines.Add('      .Done;');
  end;

  procedure EmitState;
  var
    LStateIndex: Integer;
    LValue: TNyxStateValue;
    LArgument: TNyxText;
  begin

    if ADocument.State.Count = 0 then
    begin
      Exit;
    end;
    LLines.Add('');
    LLines.Add('    // Named references give defaults and their controls one typed contract.');
    for LStateIndex := 0 to ADocument.State.Count - 1 do
    begin
      LValue := ADocument.State.Value(ADocument.State.Key(LStateIndex));
      LLines.Add('    ' + LStateVariables[LStateIndex] + ' := ' + CStateFactories[LValue.Kind] +
        '(' + PascalString(ADocument.State.Key(LStateIndex)) + ');');
    end;
    LLines.Add('');
    LLines.Add('    // Authored defaults; each application owns its runtime copy.');
    LLines.Add('    Result.State');
    for LStateIndex := 0 to ADocument.State.Count - 1 do
    begin
      LValue := ADocument.State.Value(ADocument.State.Key(LStateIndex));
      case LValue.Kind of
        nskText:
          begin
            LArgument := PascalString(LValue.TextValue);
          end;
        nskBoolean:
          begin
            LArgument := 'False';

            if LValue.BooleanValue then
            begin
              LArgument := 'True';
            end;
          end;
        nskInteger:
          begin
            LArgument := IntToStr(LValue.IntegerValue);
          end;
        nskNumber:
          begin
            LArgument := LValue.NumberText;
            { A real literal communicates the number contract. FPC can treat
              Double(Int64Literal) as a bit cast, so integral-looking defaults
              need a decimal point rather than an explicit cast. The typed
              reference already selects the Double setter. }

            if (Pos('.', LArgument) = 0) and (Pos('e', LArgument) = 0) then
            begin
              LArgument := LArgument + '.0';
            end;
          end;
      end;
      LLines.Add('      .SetValue(' + LStateVariables[LStateIndex] + ', ' + LArgument + ')');
    end;
    LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ';';
  end;

  procedure Collect(ANode: TNyxNode);
  var
    LChildIndex: Integer;
    LKindStem: TNyxText;
    LIDStem: TNyxText;
    LBase: TNyxText;
    LVariable: TNyxText;
    LOwner: TNyxNode;
    LPart: TNyxNode;
    LPartPath: TNyxText;
    LPrefix: TNyxText;
    LSerial: Integer;
    LSerialText: TNyxText;
  begin
    { Import the saved search contract even in applications without menus.
      Reuse this existing preorder: definitions and overrides participate, and
      unconfigured applications retain their original generated import list. }

    if ANode.HasCollectionView and ANode.CollectionView.HasTypeAhead then
    begin
      LHasSavedTypeAhead := True;
    end;
    { A local conveys both authored purpose and control type: project-description
      becomes LProjectDescriptionMemo. Type-bearing IDs such as welcome-card or
      memo-17 avoid repeating the type. Non-ASCII-only IDs use the control kind.
      Full identifiers are admitted case-insensitively, including normalization,
      truncation and digit suffix collisions. Unrelated IDs never renumber a memo. }
    LKindStem := ControlStem(ANode.Kind);
    LIDStem := ControlStem(ANode.ID);
    { Catalog instance IDs use an internal serial to ensure identity. Named
      composition paths carry the author's purpose instead: search/query becomes
      LSearchQueryInput, independently of serial changes in unrelated parts.
      Explicitly authored IDs retain the existing naming rule. }
    LOwner := ANode.Parent;
    while LOwner <> nil do
    begin
      LPrefix := LOwner.ID + '-part-';
      LSerialText := Copy(ANode.ID, Length(LPrefix) + 1, MaxInt);

      if (Copy(ANode.ID, 1, Length(LPrefix)) = LPrefix) and
        TryStrToInt(LSerialText, LSerial) and (LSerial > 0) and
        (IntToStr(LSerial) = LSerialText) and (ANode.Prop('part') <> '') then
      begin
        LPartPath := '';
        LPart := ANode;
        while LPart <> LOwner do
        begin
          LPartPath := '-' + LPart.Prop('part', LPart.Kind) + LPartPath;
          LPart := LPart.Parent;
        end;
        LIDStem := ControlStem(LOwner.ID + LPartPath);
        Break;
      end;
      LOwner := LOwner.Parent;
    end;

    if LIDStem = 'Control' then
    begin
      LIDStem := LKindStem;
    end;

    if (UpperCase(Copy(LIDStem, 1, Length(LKindStem))) <> UpperCase(LKindStem)) and
      (UpperCase(Copy(LIDStem, Length(LIDStem) - Length(LKindStem) + 1, MaxInt)) <>
        UpperCase(LKindStem)) then
    begin
      LIDStem := LIDStem + LKindStem;
    end;
    LBase := 'L' + LIDStem;
    LVariable := UniqueVariable(LBase);
    SetLength(LNodes, Length(LNodes) + 1);
    LNodes[Length(LNodes) - 1] := ANode;
    SetLength(LVariables, Length(LVariables) + 1);
    LVariables[Length(LVariables) - 1] := LVariable;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LChildIndex]);
    end;
  end;

  procedure Emit(ANode: TNyxNode; const AOwner: TNyxText);
  var
    LVariable: TNyxText;
    LPropIndex: Integer;
    LChildIndex: Integer;
    LKey: TNyxText;
    LKind: TNyxKind;
    LKindArgument: TNyxText;
    LWireKey: TNyxText;
    LPlatform: TNyxPlatform;
    LScope: TNyxPlatform;
    LAttribute: TNyxAttribute;
    LViewport: TNyxViewportCondition;
    LViewportScope: TNyxViewportCondition;
    LPresentation: TNyxPresentationRef;
    LPresentationScope: TNyxText;
    LPresentationPlatform: TNyxPlatform;
    LPresentationAttribute: TNyxAttribute;
    LContentRule: TNyxContentRule;
    LContentScope: TNyxContentRule;
    LContentPlatform: TNyxPlatform;
    LContentIndex: Integer;
    LCalendarValue: Boolean;
    LClockValue: Boolean;
    LRGBValue: Boolean;
    LEarlyExtensions: Boolean;
    LContext: TNyxNode;
    LProjection: TNyxNode;
  begin
    LScope := npfAny;
    LViewportScope := TNyxViewportCondition.Any;
    LPresentationScope := '';
    { Admit each newly created node to its owner before applying properties.
      Generated try/except can then release the document if later work fails,
      without leaving unowned local builder variables behind. }
    { Collect and Emit walk the same preorder. A monotonic cursor gives each
      node its declared variable in O(1), avoiding a full scan per node while
      the designer regenerates source for a large application. }
    LVariable := LVariables[LNextNode];
    Inc(LNextNode);
    LKindArgument := 'NyxCustomKind(' + PascalString(ANode.Kind) + ')';

    if TryNyxKind(ANode.Kind, LKind) then
    begin
      LKindArgument := 'nk' + ControlStem(ANode.Kind);
    end;
    LLines.Add('');

    if TryNyxKind(ANode.Kind, LKind) then
    begin
      LKindArgument := 'NewNyx' + ControlStem(ANode.Kind) + '(' + PascalString(ANode.ID);

      if (LKind >= nkLabeledButton) and (LKind <> nkSlotOverride) then
      begin
        { The authored tree already contains its compound parts. Reconstruction
          is exact; a default recipe here would silently add a second set. }
        LKindArgument := LKindArgument + ', ncoDescriptor';
      end;
      LKindArgument := LKindArgument + ')';
    end
    else
    begin
      LKindArgument := 'NewNyxControl(' + LKindArgument + ', ' + PascalString(ANode.ID) + ')';
    end;
    LLines.Add('    ' + LVariable + ' := ' + LKindArgument + ';');
    LLines.Add('    ' + AOwner + '(' + LVariable + ');');
    { A specialized slider defaults to Integer. Declare its Number contract
      before calling a typed Number setter; deferring it would make otherwise
      valid generated Pascal refuse at runtime. Emit the whole ordered group
      once so opaque extension order and callback definitions remain exact. }
    LEarlyExtensions := (ANode.Kind = 'slider') and
      (NyxNodeValueDomain(ANode).Kind = nskNumber);

    if LEarlyExtensions then
    begin
      EmitExtensions(ANode.Extensions, LVariable);
    end;

    if (ANode.Props.Count > 0) or ANode.HasMenu or ANode.HasMenuBar then
    begin
      LLines.Add('    ' + LVariable + '.Configure');
    end;

    if ANode.HasMenu then
    begin

      if ANode.MenuReference.Name = '' then
      begin
        LLines.Add('      .NoMenu');
      end
      else
      begin
        LLines.Add('      .Menu(NyxMenuRef(' + PascalString(ANode.MenuReference.Name) + '))');
      end;
    end;
    EmitMenuBar(ANode);
    LCalendarValue := False;
    LClockValue := False;
    LRGBValue := False;

    if (ANode.Kind = 'slot-override') and
      (ANode.Props.IndexOfName('value') >= 0) and (ANode.Prop('mode') <> 'remove') then
    begin
      { A properties override has no primitive kind of its own. Resolve once
        against its independently owned reusable context so its default date
        authoring stays typed, including nested/replaced named parts. }
      LContext := RealizeNyxContext(ADocument, ANode, LProjection);
      try
        LCalendarValue := (LProjection <> nil) and (LProjection.ProjectionKind = 'date');
        LClockValue := (LProjection <> nil) and (LProjection.ProjectionKind = 'time');
        LRGBValue := (LProjection <> nil) and (LProjection.ProjectionKind = 'color');
      finally
        LContext.Free;
      end;
    end;
    for LPropIndex := 0 to ANode.Props.Count - 1 do
    begin
      LWireKey := ANode.Props.Names[LPropIndex];
      LKey := LWireKey;
      LPlatform := npfAny;
      LViewport := TNyxViewportCondition.Any;
      LPresentation := Default(TNyxPresentationRef);

      if TryNyxPlatformKey(LWireKey, LPlatform, LAttribute) then
      begin
        LKey := NyxAttributeName(LAttribute);
      end
      else if TryNyxViewportKey(LWireKey, LViewport, LPlatform, LAttribute) then
      begin
        { The wire namespaces are disjoint. A failed viewport probe initializes
          its out parameters; probing after a successful platform decode would
          discard that platform and generate its value into the base scope. }
        LKey := NyxAttributeName(LAttribute);
      end;

      if TryNyxPresentationKey(LWireKey, LPresentation,
        LPresentationPlatform, LPresentationAttribute) then
      begin
        LPlatform := LPresentationPlatform;
        LAttribute := LPresentationAttribute;
        LKey := NyxAttributeName(LAttribute);

        if LPresentation.Name <> LPresentationScope then
        begin
          LLines.Add('      .WhenPresentation(NyxPresentation(' + PascalString(LPresentation.Name) + '))');
          LPresentationScope := LPresentation.Name;
          LViewportScope := TNyxViewportCondition.Any;
        end;
      end
      else if (LPresentationScope <> '') or not LViewport.Same(LViewportScope) then
      begin
        LLines.Add('      .WhenViewport(' + LViewport.Pascal + ')');
        LViewportScope := LViewport;
        LPresentationScope := '';
      end;

      if LPlatform <> LScope then
      begin
        LLines.Add('      .ForPlatform(' + NyxPlatformSymbol(LPlatform) + ')');
        LScope := LPlatform;
      end;
      LLines.Add('      .' + ConfigurationCall(ANode, LKey, ANode.Prop(LWireKey),
        LCalendarValue, LClockValue, LRGBValue));
    end;

    if (ANode.Props.Count > 0) or ANode.HasMenu or ANode.HasMenuBar then
    begin
      LLines.Add('      .Done;');
    end;

    if ANode.HasContent then
    begin
      LLines.Add('    ' + LVariable + '.Content');
      LContentScope := Default(TNyxContentRule);
      LContentPlatform := npfAny;
      for LContentIndex := 0 to ANode.Content.Count - 1 do
      begin
        LContentRule := ANode.Content.Rule(LContentIndex);

        if LContentRule.Platform <> LContentPlatform then
        begin
          LLines.Add('      .ForPlatform(' + NyxPlatformSymbol(LContentRule.Platform) + ')');
          LContentPlatform := LContentRule.Platform;
        end;

        if not LContentRule.SameScope(LContentScope) then
        begin

          if LContentRule.Scope = ncsPresentation then
          begin
            LLines.Add('      .WhenPresentation(NyxPresentation(' +
              PascalString(LContentRule.Presentation.Name) + '))');
          end
          else
          begin
            LLines.Add('      .WhenViewport(' + LContentRule.Viewport.Pascal + ')');
          end;
        end;
        LLines.Add('      .Use(NyxComponent(' + PascalString(LContentRule.Component.Name) + '))');
        LContentScope := LContentRule;
      end;
      LLines.Add('      .Done;');
    end;
    EmitBindings(ANode, LVariable);

    if not LEarlyExtensions then
    begin
      EmitExtensions(ANode.Extensions, LVariable);
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      Emit(ANode.Children[LChildIndex], LVariable + '.Add');
    end;
  end;

begin
  AdmitUnitName(AUnitName);

  if ADocument = nil then
    raise ENyxModel.Create('Document is required');
  ValidateNyxDocumentProperties(ADocument);
  LLines := TNyxStrings.Create;
  LUsedVariables := TStringList.Create;
  try
    LHasSavedTypeAhead := False;
    LUsedVariables.CaseSensitive := False;
    LUsedVariables.Sorted := True;
    SetLength(LStateVariables, ADocument.State.Count);
    for LIndex := 0 to ADocument.State.Count - 1 do
    begin
      LStateValue := ADocument.State.Value(ADocument.State.Key(LIndex));
      LStateVariables[LIndex] := UniqueVariable(StateVariableBase(ADocument.State.Key(LIndex),
        LStateValue.Kind));
    end;
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      Collect(ADocument.Pages[LIndex]);
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      Collect(ADocument.Components[LIndex]);
    end;
    LLines.Add('{ Nyx application views. BuildNyxDocument returns an owned document. }');
    LLines.Add('unit ' + AUnitName + ';');
    LLines.Add('');
    LLines.Add('{$mode delphi}{$H+}');
    LLines.Add('{$codepage utf8}');
    LLines.Add('');
    LLines.Add('interface');
    LLines.Add('');
    LLines.Add('uses');
    LLines.Add('  nyx.text,');
    LLines.Add('  nyx.dates,');
    LLines.Add('  nyx.times,');
    LLines.Add('  nyx.colors,');
    LLines.Add('  nyx.images,');

    if ADocument.Resources.Count > 0 then
    begin
      LLines.Add('  nyx.bytes,');
      LLines.Add('  nyx.resources,');
      LLines.Add('  nyx.resource.sources,');
    end;
    LLines.Add('  nyx.design.tokens,');
    LLines.Add('  nyx.types,');
    LLines.Add('  nyx.responsive,');
    LLines.Add('  nyx.presentations,');
    LLines.Add('  nyx.content,');

    if ADocument.HasMenuDeclarations then
    begin
      LLines.Add('  nyx.menu.declarations,');
      LLines.Add('  nyx.menu.bar.declarations,');
      LLines.Add('  nyx.menu.types,');
      LLines.Add('  nyx.popover.types,');
      LLines.Add('  nyx.typeahead,');
      LLines.Add('  nyx.root.types,');
    end
    else if LHasSavedTypeAhead then
    begin
      LLines.Add('  nyx.typeahead,');
    end;
    LLines.Add('  nyx.containers,');
    LLines.Add('  nyx.editing,');
    LLines.Add('  nyx.gestures,');

    { A stable public-contract import list also supports handwritten helpers
      around the builder. Adding the first binding/domain/extension through the
      designer must not invalidate a user's preserved application imports. }
    LLines.Add('  nyx.contract,');
    LLines.Add('  nyx.data,');
    LLines.Add('  nyx.state,');

    if ADocument.HasCollectionViews then
    begin
      LLines.Add('  nyx.collections.view.types,');
      LLines.Add('  nyx.collections.selection,');
      LLines.Add('  nyx.collections.query,');
    end;

    if ADocument.Collections.Count > 0 then
    begin
      LLines.Add('  nyx.collections,');
      LLines.Add('  nyx.collections.registry,');
    end;

    if NyxHasResourceCollections(ADocument.Collections) then
    begin
      LLines.Add('  nyx.resources.rows,');
    end;
    LLines.Add('  nyx.binding.types,');
    LLines.Add('  nyx.behavior,');
    LLines.Add('  nyx.scheduler,');
    LLines.Add('  nyx.events,');
    LLines.Add('  nyx.callbacks,');
    LLines.Add('  nyx.model,');
    LLines.Add('  nyx.controls;');
    LLines.Add('');
    LLines.Add('function BuildNyxDocument: TNyxDocument;');
    LLines.Add('');
    LLines.Add('implementation');
    LLines.Add('');
    LLines.Add('// <nyx:views>');
    LLines.Add('// Studio synchronizes this builder; keep application helpers outside it.');
    LLines.Add('function BuildNyxDocument: TNyxDocument;');

    if (Length(LNodes) > 0) or (ADocument.State.Count > 0) then
    begin
      LLines.Add('var');
      for LIndex := 0 to ADocument.State.Count - 1 do
      begin
        LStateValue := ADocument.State.Value(ADocument.State.Key(LIndex));
        LLines.Add('  ' + LStateVariables[LIndex] + ': T' + CStateFactories[LStateValue.Kind] + 'Ref;');
      end;
      for LIndex := 0 to Length(LNodes) - 1 do
      begin

        if TryNyxKind(LNodes[LIndex].Kind, LKind) then
        begin
          LLines.Add('  ' + LVariables[LIndex] + ': INyx' + ControlStem(LNodes[LIndex].Kind) + ';');
        end
        else
        begin
          LLines.Add('  ' + LVariables[LIndex] + ': INyxControl;');
        end;
      end;
    end;
    LLines.Add('begin');
    LLines.Add('  Result := TNyxDocument.Create;');
    LLines.Add('  try');
    LLines.Add('    // Admit each control to its owner before configuring it.');
    LLines.Add('    // A failed configuration releases the complete owned document.');
    LLines.Add('    Result.Title := ' + PascalString(ADocument.Title) + ';');
    EmitExtensions(ADocument.Extensions, 'Result');
    EmitState;
    EmitCollections;
    for LIndex := 0 to ADocument.Resources.Count - 1 do
    begin
      LLines.Add('');
      LLines.Add('    Result.Resources.Define(NyxResourceRef(' +
        PascalString(ADocument.Resources.Reference(LIndex).Name) + '),');

      if ADocument.Resources.Locale(LIndex).Defined then
      begin
        LLines.Add('      NyxLocale(' + PascalString(ADocument.Resources.Locale(LIndex).Name) + '),');
      end;
      LLines.Add('      ' + PascalResource(ADocument.Resources.Definition(
        ADocument.Resources.Reference(LIndex), ADocument.Resources.Locale(LIndex)), '      ') + ');');
    end;
    for LIndex := 0 to ADocument.Presentations.Count - 1 do
    begin
      LLines.Add('');
      LLines.Add('    Result.Presentations.Define(NyxPresentation(' +
        PascalString(ADocument.Presentations.Reference(LIndex).Name) + '),');
      LLines.Add('      ' + ADocument.Presentations.Definition(
        ADocument.Presentations.Reference(LIndex)).Pascal + ');');
    end;
    LNextNode := 0;
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      Emit(ADocument.Pages[LIndex], 'Result.AddPage');
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      Emit(ADocument.Components[LIndex], 'Result.AddComponent');
    end;
    EmitMenus;
    LLines.Add('  except');
    LLines.Add('    Result.Free;');
    LLines.Add('    raise;');
    LLines.Add('  end;');
    LLines.Add('end;');
    LLines.Add('// </nyx:views>');
    LLines.Add('');
    LLines.Add('end.');
    Result := LLines.Text;
  finally
    LUsedVariables.Free;
    LLines.Free;
  end;
end;

end.
