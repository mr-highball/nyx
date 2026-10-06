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

unit nyx.controls;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.layout.policy,
  nyx.layout.constraints,
  nyx.data,
  nyx.contract,
  nyx.state,
  nyx.binding.types,
  nyx.collections.view.types,
  nyx.model;

type
  { Default compounds instantiate their registered recipe. Descriptor mode is
    explicit empty construction used when reconstructing an already expanded
    authored tree; it never adds defaults or duplicates inherited parts. }
  TNyxConstruction = (ncoDefault, ncoDescriptor);

  INyxControl = interface;
  {$I nyx.controls.facades.inc}

  { Public managed component contract. Node is the explicit borrowed portable
    descriptor bridge. Every implementation retains it for its own lifetime.
    Parent/document ownership extends descriptor life independently of interface
    locals; no parent or document interface is retained back from a child.
    Configuration/binding/contract/extension interfaces may safely be retained.
    Each is fresh and retains this control, so these references form no cycles. }
  INyxControl = interface(INyxNode)
    ['{737A7921-4621-4C6F-8C01-010000000002}']
    function GetID: TNyxText;
    function GetKind: TNyxText;
    function GetCount: Integer;
    function GetChild(AIndex: Integer): INyxControl;
    function GetConfigure: INyxConfiguration;
    function GetBinds: INyxBindings;
    function GetContract: INyxControlContract;
    function GetExtensions: INyxControlExtensions;
    function Named(const AID: TNyxText): INyxControl;
    function Add(const AChild: INyxNode): INyxControl;
    procedure Insert(AIndex: Integer; const AChild: INyxNode);
    function Extract(AIndex: Integer): INyxControl;
    procedure Remove(const AChild: INyxNode);
    function Part(const APath: TNyxPartRef): INyxControl;
    function OverridePart(const APath: TNyxPartRef;
      AMode: TNyxOverrideMode): INyxControl;
    function Clone: INyxControl;
    property ID: TNyxText read GetID;
    property Kind: TNyxText read GetKind;
    property Count: Integer read GetCount;
    property Children[AIndex: Integer]: INyxControl read GetChild;
    property Configure: INyxConfiguration read GetConfigure;
    property Binds: INyxBindings read GetBinds;
    property Contract: INyxControlContract read GetContract;
    property Extensions: INyxControlExtensions read GetExtensions;
  end;

  { Caption is user text, independent of identity and scalar editable values.
    In particular setting Memo.Text changes its label, while Memo.Value changes
    the editable content. Both adapters consume the same admitted properties. }
  INyxCaptionControl = interface(INyxControl)
    ['{737A7921-4621-4C6F-8C01-010000000003}']
    function GetText: TNyxText;
    procedure SetText(const AText: TNyxText);
    property Text: TNyxText read GetText write SetText;
  end;

  { Layout dimensions use integer pixels. Negative padding/gap and unsupported
    counts are rejected by the existing portable configuration admission. }
  INyxLayoutControl = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-010000000004}']
    function GetGap: Integer;
    procedure SetGap(AValue: Integer);
    function GetPadding: Integer;
    procedure SetPadding(AValue: Integer);
    function GetColumns: Integer;
    procedure SetColumns(AValue: Integer);
    property Gap: Integer read GetGap write SetGap;
    property Padding: Integer read GetPadding write SetPadding;
    property Columns: Integer read GetColumns write SetColumns;
  end;

  { Ordinary fields expose a text value. Numeric input-type customization uses
    explicit Configure/domain declarations, rather than coercing text into a
    number through this property. Placeholder and read-only remain typed. }
  INyxTextInput = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-010000000005}']
    function GetValue: TNyxText;
    procedure SetValue(const AValue: TNyxText);
    function GetPlaceholder: TNyxText;
    procedure SetPlaceholder(const AValue: TNyxText);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    property Value: TNyxText read GetValue write SetValue;
    property Placeholder: TNyxText read GetPlaceholder write SetPlaceholder;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  INyxBooleanInput = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-010000000006}']
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  { Split-specific references retain layout and managed ownership. Scalar
    setters check typed limits; admission checks position/bounds together. }
  INyxSplitControl = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-01000000000B}']
    function GetOrientation: TNyxSplitOrientation;
    procedure SetOrientation(AValue: TNyxSplitOrientation);
    function GetPosition: Integer;
    procedure SetPosition(AValue: Integer);
    function GetResizable: Boolean;
    procedure SetResizable(AValue: Boolean);
    property Orientation: TNyxSplitOrientation read GetOrientation write SetOrientation;
    property Position: Integer read GetPosition write SetPosition;
    property Resizable: Boolean read GetResizable write SetResizable;
  end;

  INyxIntegerInput = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-010000000007}']
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    function GetMinimum: Integer;
    procedure SetMinimum(AValue: Integer);
    function GetMaximum: Integer;
    procedure SetMaximum(AValue: Integer);
    property Value: Integer read GetValue write SetValue;
    property Minimum: Integer read GetMinimum write SetMinimum;
    property Maximum: Integer read GetMaximum write SetMaximum;
  end;

  INyxImageControl = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-010000000008}']
    function GetSource: TNyxText;
    procedure SetSource(const AValue: TNyxText);
    function GetAlternativeText: TNyxText;
    procedure SetAlternativeText(const AValue: TNyxText);
    property Source: TNyxText read GetSource write SetSource;
    property AlternativeText: TNyxText read GetAlternativeText write SetAlternativeText;
  end;

  { Items retain the current line-list wire format. Typed observable collections
    have a separate acceptance task; this property does not claim that behavior. }
  INyxChoiceControl = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-010000000009}']
    function GetItems: TNyxText;
    procedure SetItems(const AValue: TNyxText);
    property Items: TNyxText read GetItems write SetItems;
  end;

  INyxReferenceControl = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-01000000000A}']
    function GetReference: TNyxComponentRef;
    procedure SetReference(const AValue: TNyxComponentRef);
    property Reference: TNyxComponentRef read GetReference write SetReference;
  end;

  { Default implementation owns exactly one descriptor reference. A raw caller
    may retain an existing node without transferring its ownership token.
    New controls transfer that token after acquiring their reference. Destroy
    releases it; final unowned release frees the descriptor and its descendants.
    Descendant references survive ancestor disposal with their Parent cleared. }
  TNyxControl = class(TInterfacedObject, INyxControl, INyxNode)
  private
    FNode: TNyxNode;
  protected
    { Compounds with a named scalar field expose that field's typed value.
      A descriptor without its required part fails explicitly. Other controls
      retain their own value; no generic layout invents a scalar contract. }
    function ValueNode: TNyxNode;
  public
    constructor Create(AKind: TNyxKind; const AID: TNyxText;
      AConstruction: TNyxConstruction = ncoDefault); overload;
    constructor CreateFromNode(ANode: TNyxNode);
    destructor Destroy; override;
    function GetNode: TNyxNode;
    function GetID: TNyxText;
    function GetKind: TNyxText;
    function GetCount: Integer;
    function GetChild(AIndex: Integer): INyxControl;
    function GetConfigure: INyxConfiguration;
    function GetBinds: INyxBindings;
    function GetContract: INyxControlContract;
    function GetExtensions: INyxControlExtensions;
    function Named(const AID: TNyxText): INyxControl;
    function Add(const AChild: INyxNode): INyxControl;
    procedure Insert(AIndex: Integer; const AChild: INyxNode);
    function Extract(AIndex: Integer): INyxControl;
    procedure Remove(const AChild: INyxNode);
    function Part(const APath: TNyxPartRef): INyxControl;
    function OverridePart(const APath: TNyxPartRef;
      AMode: TNyxOverrideMode): INyxControl;
    function Clone: INyxControl;
    property Node: TNyxNode read GetNode;
  end;

  TNyxCaptionControl = class(TNyxControl, INyxCaptionControl)
  public
    function GetText: TNyxText;
    procedure SetText(const AText: TNyxText);
    property Text: TNyxText read GetText write SetText;
  end;

  TNyxLayoutControl = class(TNyxCaptionControl, INyxLayoutControl)
  public
    function GetGap: Integer;
    procedure SetGap(AValue: Integer);
    function GetPadding: Integer;
    procedure SetPadding(AValue: Integer);
    function GetColumns: Integer;
    procedure SetColumns(AValue: Integer);
    property Gap: Integer read GetGap write SetGap;
    property Padding: Integer read GetPadding write SetPadding;
    property Columns: Integer read GetColumns write SetColumns;
  end;

  TNyxTextInput = class(TNyxCaptionControl, INyxTextInput)
  public
    function GetValue: TNyxText;
    procedure SetValue(const AValue: TNyxText);
    function GetPlaceholder: TNyxText;
    procedure SetPlaceholder(const AValue: TNyxText);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    property Value: TNyxText read GetValue write SetValue;
    property Placeholder: TNyxText read GetPlaceholder write SetPlaceholder;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  TNyxBooleanInput = class(TNyxCaptionControl, INyxBooleanInput)
  public
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  TNyxSplitControl = class(TNyxLayoutControl, INyxSplitControl)
  public
    function GetOrientation: TNyxSplitOrientation;
    procedure SetOrientation(AValue: TNyxSplitOrientation);
    function GetPosition: Integer;
    procedure SetPosition(AValue: Integer);
    function GetResizable: Boolean;
    procedure SetResizable(AValue: Boolean);
  end;

  TNyxIntegerInput = class(TNyxCaptionControl, INyxIntegerInput)
  public
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    function GetMinimum: Integer;
    procedure SetMinimum(AValue: Integer);
    function GetMaximum: Integer;
    procedure SetMaximum(AValue: Integer);
    property Value: Integer read GetValue write SetValue;
    property Minimum: Integer read GetMinimum write SetMinimum;
    property Maximum: Integer read GetMaximum write SetMaximum;
  end;

  TNyxImageControl = class(TNyxCaptionControl, INyxImageControl)
  public
    function GetSource: TNyxText;
    procedure SetSource(const AValue: TNyxText);
    function GetAlternativeText: TNyxText;
    procedure SetAlternativeText(const AValue: TNyxText);
    property Source: TNyxText read GetSource write SetSource;
    property AlternativeText: TNyxText read GetAlternativeText write SetAlternativeText;
  end;

  TNyxChoiceControl = class(TNyxTextInput, INyxChoiceControl)
  public
    function GetItems: TNyxText;
    procedure SetItems(const AValue: TNyxText);
    property Items: TNyxText read GetItems write SetItems;
  end;

  TNyxReferenceControl = class(TNyxCaptionControl, INyxReferenceControl)
  public
    function GetReference: TNyxComponentRef;
    procedure SetReference(const AValue: TNyxComponentRef);
    property Reference: TNyxComponentRef read GetReference write SetReference;
  end;

  {$I nyx.controls.types.inc}

{ Retain an existing descriptor without taking the raw caller's ownership token.
  Built-in descriptors return their specialized interface through QueryInterface.
  Unknown registered kinds retain the open INyxControl contract. }
function RetainNyxControl(ANode: TNyxNode): INyxControl;
{ Dynamic built-in construction retains the same specialized default object
  used by the named factories. Use a named factory when the kind is known to
  retain its specialized type at the call site. }
function NewNyxBuiltinControl(AKind: TNyxKind; const AID: TNyxText = '';
  AConstruction: TNyxConstruction = ncoDefault): INyxControl;
{ Extension construction retains a distinct kind reference, never a raw string
  selecting built-in behavior. The descriptor starts independently managed. }
function NewNyxControl(const AKind: TNyxKindRef;
  const AID: TNyxText = ''): INyxControl;

{$I nyx.controls.factories.inc}

implementation

uses
  nyx.catalog;

var
  { Private immutable recipe registry, shared only to clone default blueprints.
    Clients cannot mutate it. Construction/authoring is a UI-thread operation;
    future scheduler workers must marshal model mutations to that thread. }
  GDefaultCatalog: TNyxCatalog;

constructor TNyxControl.Create(AKind: TNyxKind; const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create;

  if (AConstruction = ncoDefault) and (AKind >= nkLabeledButton) and
    (AKind <> nkSlotOverride) then
  begin

    if GDefaultCatalog = nil then
    begin
      GDefaultCatalog := TNyxCatalog.Create;
    end;
    FNode := GDefaultCatalog.NewNode(AKind, AID);
  end
  else
  begin
    FNode := TNyxNode.Create(AKind, AID);
  end;
  FNode.AcquireReference;
  FNode.ReleaseOwnership;
end;

constructor TNyxControl.CreateFromNode(ANode: TNyxNode);
begin
  inherited Create;

  if ANode = nil then
  begin
    raise ENyxModel.Create('A retained descriptor is required');
  end;
  FNode := ANode;
  FNode.AcquireReference;
end;

destructor TNyxControl.Destroy;
begin

  if FNode <> nil then
  begin
    FNode.ReleaseReference;
  end;
  inherited Destroy;
end;

function TNyxControl.GetNode: TNyxNode;
begin
  Result := FNode;
end;

function TNyxControl.ValueNode: TNyxNode;
begin
  Result := Node;

  if Node.Kind = NyxKindName(nkSearchField) then
  begin
    Result := Node.Part(NyxPart('query'));
  end;

  if Node.Kind = NyxKindName(nkNumberStepper) then
  begin
    Result := Node.Part(NyxPart('value'));
  end;

  if Node.Kind = NyxKindName(nkPagination) then
  begin
    Result := Node.Part(NyxPart('page'));
  end;
end;

function TNyxControl.GetID: TNyxText;
begin
  Result := FNode.ID;
end;

function TNyxControl.GetKind: TNyxText;
begin
  Result := FNode.Kind;
end;

function TNyxControl.GetCount: Integer;
begin
  Result := FNode.Count;
end;

function TNyxControl.GetChild(AIndex: Integer): INyxControl;
begin
  Result := RetainNyxControl(FNode.Children[AIndex]);
end;

function TNyxControl.GetConfigure: INyxConfiguration;
begin
  Result := TNyxConfiguration.Create(Self as INyxControl);
end;

function TNyxControl.GetBinds: INyxBindings;
begin
  Result := TNyxBindings.Create(Self as INyxControl);
end;

function TNyxControl.GetContract: INyxControlContract;
begin
  Result := TNyxControlContract.Create(Self as INyxControl);
end;

function TNyxControl.GetExtensions: INyxControlExtensions;
begin
  Result := TNyxControlExtensions.Create(Self as INyxControl);
end;

function TNyxControl.Named(const AID: TNyxText): INyxControl;
begin
  FNode.Named(AID);
  Result := Self as INyxControl;
end;

function TNyxControl.Add(const AChild: INyxNode): INyxControl;
begin
  FNode.Add(AChild);
  Result := Self as INyxControl;
end;

procedure TNyxControl.Insert(AIndex: Integer; const AChild: INyxNode);
begin

  if AChild = nil then
  begin
    raise ENyxModel.Create('Child interface is required');
  end;
  FNode.Insert(AIndex, AChild);
end;

function TNyxControl.Extract(AIndex: Integer): INyxControl;
var
  LChild: TNyxNode;
begin
  { Acquire before detaching so allocation/interface failure preserves ownership.
    Raw Extract then returns its caller token, which this managed path releases. }
  Result := RetainNyxControl(FNode.Children[AIndex]);
  LChild := FNode.Extract(AIndex);
  LChild.ReleaseOwnership;
end;

procedure TNyxControl.Remove(const AChild: INyxNode);
begin

  if AChild = nil then
  begin
    raise ENyxModel.Create('Child interface is required');
  end;
  FNode.Remove(AChild.Node);
end;

function TNyxControl.Part(const APath: TNyxPartRef): INyxControl;
begin
  Result := RetainNyxControl(FNode.Part(APath));
end;

function TNyxControl.OverridePart(const APath: TNyxPartRef;
  AMode: TNyxOverrideMode): INyxControl;
begin
  Result := RetainNyxControl(FNode.OverridePart(APath, AMode));
end;

function TNyxControl.Clone: INyxControl;
var
  LClone: TNyxNode;
begin
  LClone := FNode.Clone;
  try
    Result := RetainNyxControl(LClone);
  except
    LClone.Free;
    raise;
  end;
  LClone.ReleaseOwnership;
end;

function NewNyxControl(const AKind: TNyxKindRef;
  const AID: TNyxText): INyxControl;
var
  LNode: TNyxNode;
begin
  LNode := TNyxNode.Create(AKind, AID);
  try
    Result := TNyxControl.CreateFromNode(LNode);
  except
    LNode.Free;
    raise;
  end;
  LNode.ReleaseOwnership;
end;

function TNyxCaptionControl.GetText: TNyxText;
begin
  Result := Node.Prop(NyxAttributeName(atText));
end;

procedure TNyxCaptionControl.SetText(const AText: TNyxText);
begin
  Node.Configure.Text(AText);
end;

function TNyxLayoutControl.GetGap: Integer;
begin
  Result := StrToInt(Node.Prop(NyxAttributeName(atGap), '0'));
end;

function TNyxSplitControl.GetOrientation: TNyxSplitOrientation;
begin
  Result := nsoStacked;

  if Node.Prop('split-orientation') = NyxSplitOrientationName(nsoSideBySide) then
  begin
    Result := nsoSideBySide;
  end;
end;

procedure TNyxSplitControl.SetOrientation(AValue: TNyxSplitOrientation);
begin
  Node.Configure.SplitOrientation(AValue);
end;

function TNyxSplitControl.GetPosition: Integer;
begin
  Result := StrToIntDef(Node.Prop('split-position'), 65);
end;

procedure TNyxSplitControl.SetPosition(AValue: Integer);
begin
  Node.Configure.SplitPosition(AValue);
end;

function TNyxSplitControl.GetResizable: Boolean;
begin
  Result := Node.Prop('split-resizable', 'true') <> 'false';
end;

procedure TNyxSplitControl.SetResizable(AValue: Boolean);
begin
  Node.Configure.SplitResizable(AValue);
end;

procedure TNyxLayoutControl.SetGap(AValue: Integer);
begin
  Node.Configure.Gap(AValue);
end;

function TNyxLayoutControl.GetPadding: Integer;
begin
  Result := StrToInt(Node.Prop(NyxAttributeName(atPadding), '0'));
end;

procedure TNyxLayoutControl.SetPadding(AValue: Integer);
begin
  Node.Configure.Padding(AValue);
end;

function TNyxLayoutControl.GetColumns: Integer;
begin
  Result := StrToInt(Node.Prop(NyxAttributeName(atColumns), '1'));
end;

procedure TNyxLayoutControl.SetColumns(AValue: Integer);
begin
  Node.Configure.Columns(AValue);
end;

function TNyxTextInput.GetValue: TNyxText;
begin
  Result := ValueNode.Prop(NyxAttributeName(atValue));
end;

procedure TNyxTextInput.SetValue(const AValue: TNyxText);
begin
  ValueNode.Configure.Value(AValue);
end;

function TNyxTextInput.GetPlaceholder: TNyxText;
begin
  Result := ValueNode.Prop(NyxAttributeName(atPlaceholder));
end;

procedure TNyxTextInput.SetPlaceholder(const AValue: TNyxText);
begin
  ValueNode.Configure.Placeholder(AValue);
end;

function TNyxTextInput.GetReadOnly: Boolean;
begin
  Result := ValueNode.Prop(NyxAttributeName(atReadOnly), 'false') = 'true';
end;

procedure TNyxTextInput.SetReadOnly(AValue: Boolean);
begin
  ValueNode.Configure.ReadOnly(AValue);
end;

function TNyxBooleanInput.GetChecked: Boolean;
begin
  Result := Node.Prop(NyxAttributeName(atValue), 'false') = 'true';
end;

procedure TNyxBooleanInput.SetChecked(AValue: Boolean);
begin
  Node.Configure.Value(AValue);
end;

function TNyxIntegerInput.GetValue: Integer;
begin
  Result := StrToInt(ValueNode.Prop(NyxAttributeName(atValue), '0'));
end;

procedure TNyxIntegerInput.SetValue(AValue: Integer);
begin
  ValueNode.Configure.Value(AValue);
end;

function TNyxIntegerInput.GetMinimum: Integer;
begin
  Result := StrToInt(ValueNode.Prop(NyxAttributeName(atMinimum), '0'));
end;

procedure TNyxIntegerInput.SetMinimum(AValue: Integer);
begin
  ValueNode.Configure.Minimum(AValue);
end;

function TNyxIntegerInput.GetMaximum: Integer;
begin
  Result := StrToInt(ValueNode.Prop(NyxAttributeName(atMaximum), '100'));
end;

procedure TNyxIntegerInput.SetMaximum(AValue: Integer);
begin
  ValueNode.Configure.Maximum(AValue);
end;

function TNyxImageControl.GetSource: TNyxText;
begin
  Result := Node.Prop(NyxAttributeName(atSource));
end;

procedure TNyxImageControl.SetSource(const AValue: TNyxText);
begin
  Node.Configure.Source(AValue);
end;

function TNyxImageControl.GetAlternativeText: TNyxText;
begin
  Result := Node.Prop(NyxAttributeName(atAlt));
end;

procedure TNyxImageControl.SetAlternativeText(const AValue: TNyxText);
begin
  Node.Configure.AlternativeText(AValue);
end;

function TNyxChoiceControl.GetItems: TNyxText;
begin
  Result := Node.Prop(NyxAttributeName(atItems));
end;

procedure TNyxChoiceControl.SetItems(const AValue: TNyxText);
begin
  Node.Configure.Items(AValue);
end;

function TNyxReferenceControl.GetReference: TNyxComponentRef;
begin
  Result := NyxComponent(Node.Prop(NyxAttributeName(atComponent)));
end;

procedure TNyxReferenceControl.SetReference(const AValue: TNyxComponentRef);
begin
  Node.Configure.Component(AValue);
end;

{$I nyx.controls.implementation.inc}
{$I nyx.controls.facades.implementation.inc}

{$IFNDEF PAS2JS}
finalization
  GDefaultCatalog.Free;
{$ENDIF}

end.
