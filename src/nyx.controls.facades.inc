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


{ Maintained by tools/nyx_control_contracts.lpr; edit the contract source, then regenerate. }
  { Managed Configure authoring retains its control. Returned values are
    independent snapshots; release this interface normally, never Free it. }
  INyxConfiguration = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-030000000001}']
    function ForPlatform(APlatform: TNyxPlatform): INyxConfiguration;
    function WhenViewport(const AWidth: TNyxViewportWidth): INyxConfiguration; overload;
    function WhenViewport(const ACondition: TNyxViewportCondition): INyxConfiguration; overload;
    function WhenPresentation(const AReference: TNyxPresentationRef): INyxConfiguration;
    function QueryContainer(const AReference: TNyxContainerRef): INyxConfiguration;
    function Containment(AValue: TNyxContainerContainment): INyxConfiguration;
    function SplitOrientation(AValue: TNyxSplitOrientation): INyxConfiguration;
    function SplitPosition(APercent: Integer): INyxConfiguration;
    function SplitMinimum(APercent: Integer): INyxConfiguration;
    function SplitMaximum(APercent: Integer): INyxConfiguration;
    function SplitResizable(AValue: Boolean): INyxConfiguration;
    function DragSource(AValue: Boolean): INyxConfiguration;
    function DropTarget(AValue: Boolean): INyxConfiguration;
    function TouchBehavior(AValue: TNyxTouchBehavior): INyxConfiguration;
    function Text(const AValue: TNyxText): INyxConfiguration;
    function Placeholder(const AValue: TNyxText): INyxConfiguration;
    function Items(const AValue: TNyxText): INyxConfiguration;
    function Hint(const AValue: TNyxText): INyxConfiguration;
    function AccessibleName(const AValue: TNyxText): INyxConfiguration;
    function LinkTo(const AValue: TNyxText): INyxConfiguration;
    function Source(const AValue: TNyxText): INyxConfiguration;
    function AlternativeText(const AValue: TNyxText): INyxConfiguration;
    function Option(const AValue: TNyxText): INyxConfiguration; overload;
    function Option(AValue: Integer): INyxConfiguration; overload;
    function Option(AValue: Boolean): INyxConfiguration; overload;
    function Option(AValue: Double): INyxConfiguration; overload;
    function Value(const AValue: TNyxText): INyxConfiguration; overload;
    function Value(const AValue: TNyxCalendarDate): INyxConfiguration; overload;
    { Retains exact clock precision; no time is distinct from midnight. }
    function Value(const AValue: TNyxClockTime): INyxConfiguration; overload;
    { Retains exact optional RGB; absence is distinct from black. }
    function Value(const AValue: TNyxRGBColor): INyxConfiguration; overload;
    function Value(AValue: Integer): INyxConfiguration; overload;
    function Value(AValue: Boolean): INyxConfiguration; overload;
    function Value(AValue: Double): INyxConfiguration; overload;
    function Padding(AValue: Integer): INyxConfiguration;
    function Gap(AValue: Integer): INyxConfiguration;
    function Columns(AValue: Integer): INyxConfiguration;
    function Width(AValue: Integer): INyxConfiguration;
    function Height(AValue: Integer): INyxConfiguration;
    function MinimumWidth(AValue: Integer): INyxConfiguration;
    function MaximumWidth(AValue: Integer): INyxConfiguration;
    function MinimumHeight(AValue: Integer): INyxConfiguration;
    function MaximumHeight(AValue: Integer): INyxConfiguration;
    function Constraints(const AValue: TNyxSizeConstraints): INyxConfiguration;
    function Left(AValue: Integer): INyxConfiguration;
    function Top(AValue: Integer): INyxConfiguration;
    function Flex(AValue: Integer): INyxConfiguration;
    function Minimum(AValue: Integer): INyxConfiguration;
    function Maximum(AValue: Integer): INyxConfiguration;
    function SliderIntervals(AValue: Integer): INyxConfiguration;
    function Enabled(AValue: Boolean): INyxConfiguration;
    function Visible(AValue: Boolean): INyxConfiguration;
    function ReadOnly(AValue: Boolean): INyxConfiguration;
    function Surface(AValue: Boolean): INyxConfiguration;
    function Compound(AValue: Boolean): INyxConfiguration;
    function Pressed(AValue: Boolean): INyxConfiguration;
    function Layout(AValue: TNyxLayoutMode): INyxConfiguration; overload;
    function Layout(const AValue: TNyxLayoutPolicy): INyxConfiguration; overload;
    function Wrap(AValue: TNyxFlowWrap): INyxConfiguration;
    function Align(AValue: TNyxCrossAlignment): INyxConfiguration;
    function Justify(AValue: TNyxJustification): INyxConfiguration;
    function WidthSizing(AValue: TNyxSizing): INyxConfiguration;
    function HeightSizing(AValue: TNyxSizing): INyxConfiguration;
    function Variant(AValue: TNyxVariant): INyxConfiguration;
    function Action(AValue: TNyxAction): INyxConfiguration;
    function ProjectAs(AValue: TNyxKind): INyxConfiguration;
    function OverrideMode(AValue: TNyxOverrideMode): INyxConfiguration;
    function InputType(AValue: TNyxInputType): INyxConfiguration;
    function PartName(const AValue: TNyxPartRef): INyxConfiguration;
    function Target(const AValue: TNyxPartRef): INyxConfiguration;
    function OverridePath(const AValue: TNyxPartRef): INyxConfiguration;
    function Component(const AValue: TNyxComponentRef): INyxConfiguration;
    function Menu(const AReference: TNyxMenuRef): INyxConfiguration;
    function NoMenu: INyxConfiguration;
    function InheritMenu: INyxConfiguration;
    { Saved immutable grouping; an explicit mask differs from inheritance. }
    function MenuBar(const ADefinition: INyxMenuBarDefinition): INyxConfiguration;
    function NoMenuBar: INyxConfiguration;
    function InheritMenuBar: INyxConfiguration;
    function OnClick(const AValue: TNyxEventRef): INyxConfiguration;
    function OnChange(const AValue: TNyxEventRef): INyxConfiguration;
    function Clear(AKey: TNyxAttribute): INyxConfiguration;
    function Metadata(AKey: TNyxAttribute; const AValue: TNyxText): INyxConfiguration;
    function CustomVariant(const AStyle: TNyxStyleRef): INyxConfiguration;
    function CustomProjection(const AKind: TNyxKindRef): INyxConfiguration;
    function Extension(const AKey, AValue: TNyxText): INyxConfiguration;
    function Done: INyxControl;
  end;

  { Fresh, acyclic facade: it owns the control interface; the control does
    not cache this facade. Fluent chains return the same managed object. }
  TNyxConfiguration = class(TInterfacedObject, INyxConfiguration)
  private
    FOwner: INyxControl;
    FPlatform: TNyxPlatform;
    FViewport: TNyxViewportCondition;
    FPresentation: TNyxPresentationRef;
    function ConfigurationScope: TNyxNodeConfig;
  public
    constructor Create(const AOwner: INyxControl);
    function ForPlatform(APlatform: TNyxPlatform): INyxConfiguration;
    function WhenViewport(const AWidth: TNyxViewportWidth): INyxConfiguration; overload;
    function WhenViewport(const ACondition: TNyxViewportCondition): INyxConfiguration; overload;
    function WhenPresentation(const AReference: TNyxPresentationRef): INyxConfiguration;
    function QueryContainer(const AReference: TNyxContainerRef): INyxConfiguration;
    function Containment(AValue: TNyxContainerContainment): INyxConfiguration;
    function SplitOrientation(AValue: TNyxSplitOrientation): INyxConfiguration;
    function SplitPosition(APercent: Integer): INyxConfiguration;
    function SplitMinimum(APercent: Integer): INyxConfiguration;
    function SplitMaximum(APercent: Integer): INyxConfiguration;
    function SplitResizable(AValue: Boolean): INyxConfiguration;
    function DragSource(AValue: Boolean): INyxConfiguration;
    function DropTarget(AValue: Boolean): INyxConfiguration;
    function TouchBehavior(AValue: TNyxTouchBehavior): INyxConfiguration;
    function Text(const AValue: TNyxText): INyxConfiguration;
    function Placeholder(const AValue: TNyxText): INyxConfiguration;
    function Items(const AValue: TNyxText): INyxConfiguration;
    function Hint(const AValue: TNyxText): INyxConfiguration;
    function AccessibleName(const AValue: TNyxText): INyxConfiguration;
    function LinkTo(const AValue: TNyxText): INyxConfiguration;
    function Source(const AValue: TNyxText): INyxConfiguration;
    function AlternativeText(const AValue: TNyxText): INyxConfiguration;
    function Option(const AValue: TNyxText): INyxConfiguration; overload;
    function Option(AValue: Integer): INyxConfiguration; overload;
    function Option(AValue: Boolean): INyxConfiguration; overload;
    function Option(AValue: Double): INyxConfiguration; overload;
    function Value(const AValue: TNyxText): INyxConfiguration; overload;
    function Value(const AValue: TNyxCalendarDate): INyxConfiguration; overload;
    function Value(const AValue: TNyxClockTime): INyxConfiguration; overload;
    function Value(const AValue: TNyxRGBColor): INyxConfiguration; overload;
    function Value(AValue: Integer): INyxConfiguration; overload;
    function Value(AValue: Boolean): INyxConfiguration; overload;
    function Value(AValue: Double): INyxConfiguration; overload;
    function Padding(AValue: Integer): INyxConfiguration;
    function Gap(AValue: Integer): INyxConfiguration;
    function Columns(AValue: Integer): INyxConfiguration;
    function Width(AValue: Integer): INyxConfiguration;
    function Height(AValue: Integer): INyxConfiguration;
    function MinimumWidth(AValue: Integer): INyxConfiguration;
    function MaximumWidth(AValue: Integer): INyxConfiguration;
    function MinimumHeight(AValue: Integer): INyxConfiguration;
    function MaximumHeight(AValue: Integer): INyxConfiguration;
    function Constraints(const AValue: TNyxSizeConstraints): INyxConfiguration;
    function Left(AValue: Integer): INyxConfiguration;
    function Top(AValue: Integer): INyxConfiguration;
    function Flex(AValue: Integer): INyxConfiguration;
    function Minimum(AValue: Integer): INyxConfiguration;
    function Maximum(AValue: Integer): INyxConfiguration;
    function SliderIntervals(AValue: Integer): INyxConfiguration;
    function Enabled(AValue: Boolean): INyxConfiguration;
    function Visible(AValue: Boolean): INyxConfiguration;
    function ReadOnly(AValue: Boolean): INyxConfiguration;
    function Surface(AValue: Boolean): INyxConfiguration;
    function Compound(AValue: Boolean): INyxConfiguration;
    function Pressed(AValue: Boolean): INyxConfiguration;
    function Layout(AValue: TNyxLayoutMode): INyxConfiguration; overload;
    function Layout(const AValue: TNyxLayoutPolicy): INyxConfiguration; overload;
    function Wrap(AValue: TNyxFlowWrap): INyxConfiguration;
    function Align(AValue: TNyxCrossAlignment): INyxConfiguration;
    function Justify(AValue: TNyxJustification): INyxConfiguration;
    function WidthSizing(AValue: TNyxSizing): INyxConfiguration;
    function HeightSizing(AValue: TNyxSizing): INyxConfiguration;
    function Variant(AValue: TNyxVariant): INyxConfiguration;
    function Action(AValue: TNyxAction): INyxConfiguration;
    function ProjectAs(AValue: TNyxKind): INyxConfiguration;
    function OverrideMode(AValue: TNyxOverrideMode): INyxConfiguration;
    function InputType(AValue: TNyxInputType): INyxConfiguration;
    function PartName(const AValue: TNyxPartRef): INyxConfiguration;
    function Target(const AValue: TNyxPartRef): INyxConfiguration;
    function OverridePath(const AValue: TNyxPartRef): INyxConfiguration;
    function Component(const AValue: TNyxComponentRef): INyxConfiguration;
    function Menu(const AReference: TNyxMenuRef): INyxConfiguration;
    function NoMenu: INyxConfiguration;
    function InheritMenu: INyxConfiguration;
    function MenuBar(const ADefinition: INyxMenuBarDefinition): INyxConfiguration;
    function NoMenuBar: INyxConfiguration;
    function InheritMenuBar: INyxConfiguration;
    function OnClick(const AValue: TNyxEventRef): INyxConfiguration;
    function OnChange(const AValue: TNyxEventRef): INyxConfiguration;
    function Clear(AKey: TNyxAttribute): INyxConfiguration;
    function Metadata(AKey: TNyxAttribute; const AValue: TNyxText): INyxConfiguration;
    function CustomVariant(const AStyle: TNyxStyleRef): INyxConfiguration;
    function CustomProjection(const AKind: TNyxKindRef): INyxConfiguration;
    function Extension(const AKey, AValue: TNyxText): INyxConfiguration;
    function Done: INyxControl;
  end;

  { Managed Binds authoring retains its control. Returned values are
    independent snapshots; release this interface normally, never Free it. }
  INyxBindings = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-030000000002}']
    function Text(const AState: TNyxTextStateRef): INyxBindings; overload;
    function Text(const AState: TNyxBooleanStateRef): INyxBindings; overload;
    function Text(const AState: TNyxIntegerStateRef): INyxBindings; overload;
    function Text(const AState: TNyxNumberStateRef): INyxBindings; overload;
    function Value(const AState: TNyxTextStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Value(const AState: TNyxBooleanStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Value(const AState: TNyxIntegerStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Value(const AState: TNyxNumberStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Enabled(const AState: TNyxBooleanStateRef): INyxBindings;
    function Visible(const AState: TNyxBooleanStateRef): INyxBindings;
    function ReadOnly(const AState: TNyxBooleanStateRef): INyxBindings;
    function Pressed(const AState: TNyxBooleanStateRef): INyxBindings;
    function Placeholder(const AState: TNyxTextStateRef): INyxBindings;
    function Hint(const AState: TNyxTextStateRef): INyxBindings;
    function AccessibleName(const AState: TNyxTextStateRef): INyxBindings;
    function Width(const AState: TNyxIntegerStateRef): INyxBindings;
    function Height(const AState: TNyxIntegerStateRef): INyxBindings;
    function Left(const AState: TNyxIntegerStateRef): INyxBindings;
    function Top(const AState: TNyxIntegerStateRef): INyxBindings;
    function Padding(const AState: TNyxIntegerStateRef): INyxBindings;
    function Gap(const AState: TNyxIntegerStateRef): INyxBindings;
    function Columns(const AState: TNyxIntegerStateRef): INyxBindings;
    function Flex(const AState: TNyxIntegerStateRef): INyxBindings;
    function Minimum(const AState: TNyxIntegerStateRef): INyxBindings;
    function Maximum(const AState: TNyxIntegerStateRef): INyxBindings;
    function Clear(ATarget: TNyxBindingProperty): INyxBindings;
    function Inherit(ATarget: TNyxBindingProperty): INyxBindings;
    function Collection(const ASpec: TNyxCollectionViewSpec): INyxBindings;
    function ClearCollection: INyxBindings;
    function InheritCollection: INyxBindings;
    function Done: INyxControl;
  end;

  { Fresh, acyclic facade: it owns the control interface; the control does
    not cache this facade. Fluent chains return the same managed object. }
  TNyxBindings = class(TInterfacedObject, INyxBindings)
  private
    FOwner: INyxControl;
  public
    constructor Create(const AOwner: INyxControl);
    function Text(const AState: TNyxTextStateRef): INyxBindings; overload;
    function Text(const AState: TNyxBooleanStateRef): INyxBindings; overload;
    function Text(const AState: TNyxIntegerStateRef): INyxBindings; overload;
    function Text(const AState: TNyxNumberStateRef): INyxBindings; overload;
    function Value(const AState: TNyxTextStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Value(const AState: TNyxBooleanStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Value(const AState: TNyxIntegerStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Value(const AState: TNyxNumberStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings; overload;
    function Enabled(const AState: TNyxBooleanStateRef): INyxBindings;
    function Visible(const AState: TNyxBooleanStateRef): INyxBindings;
    function ReadOnly(const AState: TNyxBooleanStateRef): INyxBindings;
    function Pressed(const AState: TNyxBooleanStateRef): INyxBindings;
    function Placeholder(const AState: TNyxTextStateRef): INyxBindings;
    function Hint(const AState: TNyxTextStateRef): INyxBindings;
    function AccessibleName(const AState: TNyxTextStateRef): INyxBindings;
    function Width(const AState: TNyxIntegerStateRef): INyxBindings;
    function Height(const AState: TNyxIntegerStateRef): INyxBindings;
    function Left(const AState: TNyxIntegerStateRef): INyxBindings;
    function Top(const AState: TNyxIntegerStateRef): INyxBindings;
    function Padding(const AState: TNyxIntegerStateRef): INyxBindings;
    function Gap(const AState: TNyxIntegerStateRef): INyxBindings;
    function Columns(const AState: TNyxIntegerStateRef): INyxBindings;
    function Flex(const AState: TNyxIntegerStateRef): INyxBindings;
    function Minimum(const AState: TNyxIntegerStateRef): INyxBindings;
    function Maximum(const AState: TNyxIntegerStateRef): INyxBindings;
    function Clear(ATarget: TNyxBindingProperty): INyxBindings;
    function Inherit(ATarget: TNyxBindingProperty): INyxBindings;
    function Collection(const ASpec: TNyxCollectionViewSpec): INyxBindings;
    function ClearCollection: INyxBindings;
    function InheritCollection: INyxBindings;
    function Done: INyxControl;
  end;

  { Managed Contract authoring retains its control. Returned values are
    independent snapshots; release this interface normally, never Free it. }
  INyxControlContract = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-030000000003}']
    function Snapshot: TNyxDataValue;
    procedure Validate;
    function FindValue(out ADomain: TNyxValueDomain): Boolean;
    function FieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxFieldContract;
    function EventCount: Integer;
    function EventAt(AIndex: Integer): TNyxEventContract;
    function FindEvent(ATrigger: TNyxTrigger; out AEvent: TNyxEventContract): Boolean;
    function NoValue: INyxControlContract;
    function InheritValue: INyxControlContract;
    function Value(const ADomain: TNyxValueDomain): INyxControlContract; overload;
    function Signal(ATrigger: TNyxTrigger): INyxControlContract;
    function Metadata(const AData: TNyxDataValue): INyxControlContract;
    function Value(const ADomain: TNyxTextDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxTextDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxTextDomain): INyxControlContract; overload;
    { Distinct clock bounds/choices; every admitted declaration is copied. }
    function Value(const ADomain: TNyxTimeDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxTimeDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxTimeDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxBooleanDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxBooleanDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxBooleanDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxIntegerDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxIntegerDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxIntegerDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxNumberDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxNumberDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxNumberDomain): INyxControlContract; overload;
  end;

  { Fresh, acyclic facade: it owns the control interface; the control does
    not cache this facade. Fluent chains return the same managed object. }
  TNyxControlContract = class(TInterfacedObject, INyxControlContract)
  private
    FOwner: INyxControl;
  public
    constructor Create(const AOwner: INyxControl);
    function Snapshot: TNyxDataValue;
    procedure Validate;
    function FindValue(out ADomain: TNyxValueDomain): Boolean;
    function FieldCount: Integer;
    function FieldAt(AIndex: Integer): TNyxFieldContract;
    function EventCount: Integer;
    function EventAt(AIndex: Integer): TNyxEventContract;
    function FindEvent(ATrigger: TNyxTrigger; out AEvent: TNyxEventContract): Boolean;
    function NoValue: INyxControlContract;
    function InheritValue: INyxControlContract;
    function Value(const ADomain: TNyxValueDomain): INyxControlContract; overload;
    function Signal(ATrigger: TNyxTrigger): INyxControlContract;
    function Metadata(const AData: TNyxDataValue): INyxControlContract;
    function Value(const ADomain: TNyxTextDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxTextDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxTextDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxTimeDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxTimeDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxTimeDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxBooleanDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxBooleanDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxBooleanDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxIntegerDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxIntegerDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxIntegerDomain): INyxControlContract; overload;
    function Value(const ADomain: TNyxNumberDomain): INyxControlContract; overload;
    function Field(const APart: TNyxPartRef; const ADomain: TNyxNumberDomain): INyxControlContract; overload;
    function On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxNumberDomain): INyxControlContract; overload;
  end;

  { Managed Extensions authoring retains its control. Returned values are
    independent snapshots; release this interface normally, never Free it. }
  INyxControlExtensions = interface(IInterface)
    ['{737A7921-4621-4C6F-8C01-030000000004}']
    function Key(AIndex: Integer): TNyxExtensionRef;
    function Has(const AKey: TNyxExtensionRef): Boolean;
    function Value(const AKey: TNyxExtensionRef): TNyxDataValue;
    function SetValue(const AKey: TNyxExtensionRef; const AValue: TNyxDataValue): INyxControlExtensions;
    function Remove(const AKey: TNyxExtensionRef): INyxControlExtensions;
    procedure Validate;
    function ToJSON: TNyxText;
    procedure LoadJSON(const ASource: TNyxText);
  end;

  { Fresh, acyclic facade: it owns the control interface; the control does
    not cache this facade. Fluent chains return the same managed object. }
  TNyxControlExtensions = class(TInterfacedObject, INyxControlExtensions)
  private
    FOwner: INyxControl;
  public
    constructor Create(const AOwner: INyxControl);
    function Key(AIndex: Integer): TNyxExtensionRef;
    function Has(const AKey: TNyxExtensionRef): Boolean;
    function Value(const AKey: TNyxExtensionRef): TNyxDataValue;
    function SetValue(const AKey: TNyxExtensionRef; const AValue: TNyxDataValue): INyxControlExtensions;
    function Remove(const AKey: TNyxExtensionRef): INyxControlExtensions;
    procedure Validate;
    function ToJSON: TNyxText;
    procedure LoadJSON(const ASource: TNyxText);
  end;
