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
constructor TNyxConfiguration.Create(const AOwner: INyxControl);
begin
  inherited Create;

  if AOwner = nil then
  begin
    raise ENyxModel.Create('A managed facade requires its control');
  end;
  FOwner := AOwner;
end;

function TNyxConfiguration.ForPlatform(APlatform: TNyxPlatform): INyxConfiguration;
var
  LFacade: TNyxConfiguration;
begin
  LFacade := TNyxConfiguration.Create(FOwner);
  LFacade.FPlatform := APlatform;
  Result := LFacade;
end;

function TNyxConfiguration.SplitOrientation(AValue: TNyxSplitOrientation): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).SplitOrientation(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.SplitPosition(APercent: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).SplitPosition(APercent);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.SplitMinimum(APercent: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).SplitMinimum(APercent);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.SplitMaximum(APercent: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).SplitMaximum(APercent);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.SplitResizable(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).SplitResizable(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.DragSource(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).DragSource(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.DropTarget(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).DropTarget(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.TouchBehavior(AValue: TNyxTouchBehavior): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).TouchBehavior(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Text(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Text(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Placeholder(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Placeholder(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Items(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Items(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Hint(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Hint(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.AccessibleName(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).AccessibleName(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.LinkTo(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).LinkTo(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Source(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Source(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.AlternativeText(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).AlternativeText(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Option(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Option(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Option(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Option(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Option(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Option(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Option(AValue: Double): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Option(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Value(const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Value(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Value(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Value(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Value(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Value(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Value(AValue: Double): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Value(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Padding(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Padding(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Gap(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Gap(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Columns(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Columns(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Width(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Width(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Height(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Height(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.MinimumWidth(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).MinimumWidth(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.MaximumWidth(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).MaximumWidth(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.MinimumHeight(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).MinimumHeight(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.MaximumHeight(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).MaximumHeight(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Constraints(const AValue: TNyxSizeConstraints): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Constraints(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Left(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Left(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Top(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Top(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Flex(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Flex(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Minimum(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Minimum(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Maximum(AValue: Integer): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Maximum(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Enabled(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Enabled(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Visible(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Visible(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.ReadOnly(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).ReadOnly(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Surface(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Surface(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Compound(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Compound(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Pressed(AValue: Boolean): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Pressed(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Layout(AValue: TNyxLayoutMode): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Layout(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Layout(const AValue: TNyxLayoutPolicy): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Layout(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Wrap(AValue: TNyxFlowWrap): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Wrap(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Align(AValue: TNyxCrossAlignment): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Align(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Justify(AValue: TNyxJustification): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Justify(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.WidthSizing(AValue: TNyxSizing): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).WidthSizing(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.HeightSizing(AValue: TNyxSizing): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).HeightSizing(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Variant(AValue: TNyxVariant): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Variant(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Action(AValue: TNyxAction): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Action(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.ProjectAs(AValue: TNyxKind): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).ProjectAs(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.OverrideMode(AValue: TNyxOverrideMode): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).OverrideMode(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.InputType(AValue: TNyxInputType): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).InputType(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.PartName(const AValue: TNyxPartRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).PartName(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Target(const AValue: TNyxPartRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Target(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.OverridePath(const AValue: TNyxPartRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).OverridePath(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Component(const AValue: TNyxComponentRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Component(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.OnClick(const AValue: TNyxEventRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).OnClick(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.OnChange(const AValue: TNyxEventRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).OnChange(AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Clear(AKey: TNyxAttribute): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Clear(AKey);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Metadata(AKey: TNyxAttribute; const AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Metadata(AKey, AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.CustomVariant(const AStyle: TNyxStyleRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).CustomVariant(AStyle);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.CustomProjection(const AKind: TNyxKindRef): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).CustomProjection(AKind);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Extension(const AKey, AValue: TNyxText): INyxConfiguration;
begin
  FOwner.Node.Configure.ForPlatform(FPlatform).Extension(AKey, AValue);
  Result := Self as INyxConfiguration;
end;

function TNyxConfiguration.Done: INyxControl;
begin
  Result := FOwner;
end;

constructor TNyxBindings.Create(const AOwner: INyxControl);
begin
  inherited Create;

  if AOwner = nil then
  begin
    raise ENyxModel.Create('A managed facade requires its control');
  end;
  FOwner := AOwner;
end;

function TNyxBindings.Text(const AState: TNyxTextStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Text(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Text(const AState: TNyxBooleanStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Text(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Text(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Text(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Text(const AState: TNyxNumberStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Text(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Value(const AState: TNyxTextStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings;
begin
  FOwner.Node.Binds.Value(AState, ADirection);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Value(const AState: TNyxBooleanStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings;
begin
  FOwner.Node.Binds.Value(AState, ADirection);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Value(const AState: TNyxIntegerStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings;
begin
  FOwner.Node.Binds.Value(AState, ADirection);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Value(const AState: TNyxNumberStateRef; ADirection: TNyxBindingDirection = bdTwoWay): INyxBindings;
begin
  FOwner.Node.Binds.Value(AState, ADirection);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Enabled(const AState: TNyxBooleanStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Enabled(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Visible(const AState: TNyxBooleanStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Visible(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.ReadOnly(const AState: TNyxBooleanStateRef): INyxBindings;
begin
  FOwner.Node.Binds.ReadOnly(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Pressed(const AState: TNyxBooleanStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Pressed(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Placeholder(const AState: TNyxTextStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Placeholder(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Hint(const AState: TNyxTextStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Hint(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.AccessibleName(const AState: TNyxTextStateRef): INyxBindings;
begin
  FOwner.Node.Binds.AccessibleName(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Width(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Width(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Height(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Height(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Left(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Left(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Top(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Top(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Padding(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Padding(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Gap(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Gap(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Columns(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Columns(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Flex(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Flex(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Minimum(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Minimum(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Maximum(const AState: TNyxIntegerStateRef): INyxBindings;
begin
  FOwner.Node.Binds.Maximum(AState);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Clear(ATarget: TNyxBindingProperty): INyxBindings;
begin
  FOwner.Node.Binds.Clear(ATarget);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Inherit(ATarget: TNyxBindingProperty): INyxBindings;
begin
  FOwner.Node.Binds.Inherit(ATarget);
  Result := Self as INyxBindings;
end;

function TNyxBindings.Collection(const ASpec: TNyxCollectionViewSpec): INyxBindings;
begin
  FOwner.Node.Binds.Collection(ASpec);
  Result := Self as INyxBindings;
end;

function TNyxBindings.ClearCollection: INyxBindings;
begin
  FOwner.Node.Binds.ClearCollection;
  Result := Self as INyxBindings;
end;

function TNyxBindings.InheritCollection: INyxBindings;
begin
  FOwner.Node.Binds.InheritCollection;
  Result := Self as INyxBindings;
end;

function TNyxBindings.Done: INyxControl;
begin
  Result := FOwner;
end;

constructor TNyxControlContract.Create(const AOwner: INyxControl);
begin
  inherited Create;

  if AOwner = nil then
  begin
    raise ENyxModel.Create('A managed facade requires its control');
  end;
  FOwner := AOwner;
end;

function TNyxControlContract.Snapshot: TNyxDataValue;
begin
  Result := FOwner.Node.Contract.Snapshot;
end;

procedure TNyxControlContract.Validate;
begin
  FOwner.Node.Contract.Validate;
end;

function TNyxControlContract.FindValue(out ADomain: TNyxValueDomain): Boolean;
begin
  Result := FOwner.Node.Contract.FindValue(ADomain);
end;

function TNyxControlContract.FieldCount: Integer;
begin
  Result := FOwner.Node.Contract.FieldCount;
end;

function TNyxControlContract.FieldAt(AIndex: Integer): TNyxFieldContract;
begin
  Result := FOwner.Node.Contract.FieldAt(AIndex);
end;

function TNyxControlContract.EventCount: Integer;
begin
  Result := FOwner.Node.Contract.EventCount;
end;

function TNyxControlContract.EventAt(AIndex: Integer): TNyxEventContract;
begin
  Result := FOwner.Node.Contract.EventAt(AIndex);
end;

function TNyxControlContract.FindEvent(ATrigger: TNyxTrigger; out AEvent: TNyxEventContract): Boolean;
begin
  Result := FOwner.Node.Contract.FindEvent(ATrigger, AEvent);
end;

function TNyxControlContract.NoValue: INyxControlContract;
begin
  FOwner.Node.Contract.NoValue;
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Signal(ATrigger: TNyxTrigger): INyxControlContract;
begin
  FOwner.Node.Contract.Signal(ATrigger);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Metadata(const AData: TNyxDataValue): INyxControlContract;
begin
  FOwner.Node.Contract.Metadata(AData);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Value(const ADomain: TNyxTextDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Value(ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Field(const APart: TNyxPartRef; const ADomain: TNyxTextDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Field(APart, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxTextDomain): INyxControlContract;
begin
  FOwner.Node.Contract.On(ATrigger, ASource, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Value(const ADomain: TNyxBooleanDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Value(ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Field(const APart: TNyxPartRef; const ADomain: TNyxBooleanDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Field(APart, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxBooleanDomain): INyxControlContract;
begin
  FOwner.Node.Contract.On(ATrigger, ASource, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Value(const ADomain: TNyxIntegerDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Value(ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Field(const APart: TNyxPartRef; const ADomain: TNyxIntegerDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Field(APart, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxIntegerDomain): INyxControlContract;
begin
  FOwner.Node.Contract.On(ATrigger, ASource, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Value(const ADomain: TNyxNumberDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Value(ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.Field(const APart: TNyxPartRef; const ADomain: TNyxNumberDomain): INyxControlContract;
begin
  FOwner.Node.Contract.Field(APart, ADomain);
  Result := Self as INyxControlContract;
end;

function TNyxControlContract.On(ATrigger: TNyxTrigger; const ASource: TNyxEventValueRef; const ADomain: TNyxNumberDomain): INyxControlContract;
begin
  FOwner.Node.Contract.On(ATrigger, ASource, ADomain);
  Result := Self as INyxControlContract;
end;

constructor TNyxControlExtensions.Create(const AOwner: INyxControl);
begin
  inherited Create;

  if AOwner = nil then
  begin
    raise ENyxModel.Create('A managed facade requires its control');
  end;
  FOwner := AOwner;
end;

function TNyxControlExtensions.Key(AIndex: Integer): TNyxExtensionRef;
begin
  Result := FOwner.Node.Extensions.Key(AIndex);
end;

function TNyxControlExtensions.Has(const AKey: TNyxExtensionRef): Boolean;
begin
  Result := FOwner.Node.Extensions.Has(AKey);
end;

function TNyxControlExtensions.Value(const AKey: TNyxExtensionRef): TNyxDataValue;
begin
  Result := FOwner.Node.Extensions.Value(AKey);
end;

function TNyxControlExtensions.SetValue(const AKey: TNyxExtensionRef; const AValue: TNyxDataValue): INyxControlExtensions;
begin
  FOwner.Node.Extensions.SetValue(AKey, AValue);
  Result := Self as INyxControlExtensions;
end;

function TNyxControlExtensions.Remove(const AKey: TNyxExtensionRef): INyxControlExtensions;
begin
  FOwner.Node.Extensions.Remove(AKey);
  Result := Self as INyxControlExtensions;
end;

procedure TNyxControlExtensions.Validate;
begin
  FOwner.Node.Extensions.Validate;
end;

function TNyxControlExtensions.ToJSON: TNyxText;
begin
  Result := FOwner.Node.Extensions.ToJSON;
end;

procedure TNyxControlExtensions.LoadJSON(const ASource: TNyxText);
begin
  FOwner.Node.Extensions.LoadJSON(ASource);
end;

