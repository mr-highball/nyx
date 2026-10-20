{ Whole native witness for flat typed-data pointers. Bodies record which
  exact overload the compiler selected and never dereference their inputs. }
{$ifdef NYX_POINTER_UNICODE}
  {$mode delphiunicode}
{$else}
  {$mode delphi}
{$endif}
{$H+}{$codepage utf8}{$modeswitch implicitfunctionspecialization+}
unit pointers.identity.fixture;

interface

function NativePointerIdentity: AnsiString;
function NativePointerCalls: Integer;

implementation

uses SysUtils, TypInfo;

{$if not defined(CPUI386)}
  {$fatal This pointer witness requires the pinned flat i386 compiler}
{$endif}

type
  { Distinct record declarations remain different pointee identities. }
  TWidget = record Value: LongInt; end;
  { An unrelated record intentionally has the same storage shape. }
  TOtherWidget = record Value: LongInt; end;
  { Recursive field syntax must not turn equal declared pointees into an
    unbounded structural walk or confuse two unrelated pointer chains. }
  TRecursive = record Next: ^TRecursive; end;
  { Generic arguments remain part of the complete pointee identity. }
  TBox<T> = class Value: T; end;
  { A distinct scalar must not become equal merely because its width matches. }
  TCount = type LongInt;
  { Unlike a distinct ordinal, this copied record has independent identity. }
  TDistinctWidget = type TWidget;
  { A copied record's pointer must not acquire the original record overload. }
  PDistinctWidget = ^TDistinctWidget;
  { Alias specialization still retains the compiler's generic pointer owner. }
  TWidgetBoxAlias = TBox<TWidget>;
  { A different substitution stays distinct in the semantic type itself. }
  TOtherBoxAlias = TBox<TOtherWidget>;
  { Aliases must not hide a pointer's compiler specialization provenance. }
  PBoxAlias = ^TWidgetBoxAlias;
  { A second aliased pointer exercises the same compiler equivalence rule. }
  POtherBoxAlias = ^TOtherBoxAlias;
  { Ordinary source inheritance supplies a directional pointer conversion. }
  TParent = class end;
  { No class layout or runtime instance is needed to establish this ancestry. }
  TChild = class(TParent) end;
  { An ancestor pointer remains different from its child's exact type. }
  PParent = ^TParent;
  { The actual argument exercises child-to-parent compatibility. }
  PChild = ^TChild;
  { First ordinary pointer declaration. }
  PWidget = ^TWidget;
  { Transparent alias retains its source-facing declaration. }
  PAliasWidget = PWidget;
  { Separate pointer syntax still targets the exact same record declaration. }
  PSeparateWidget = ^TWidget;
  { This pointer's matching storage layout supplies no pointee equality. }
  POtherWidget = ^TOtherWidget;
  { Nominal distinctness belongs to the pointer itself. }
  TDistinctPointer = type PWidget;
  { Ordinary primitive pointer, separate from the distinct scalar below. }
  PLong = ^LongInt;
  { The pointee's distinct declaration must survive pointer normalization. }
  PCount = ^TCount;
  { Nested pointer comparison must retain the complete target chain. }
  PPWidget = ^PWidget;
  { A nested alias changes spelling, not the pointed-to declaration. }
  PPAliasWidget = ^PAliasWidget;
  { Closed concrete generic target. }
  PBoxWidget = ^TBox<TWidget>;
  { Different generic substitution must not share canonical identity. }
  PBoxOther = ^TBox<TOtherWidget>;
  { Pointer to a declared recursive record; equality stops at that declaration. }
  PRecursive = ^TRecursive;
  { Native result types expose overload selection independently of the binder. }
  TExact = record Code: Integer; end;
  { Void-pointer conversion is intentionally a different observable result. }
  TFallback = record Code: Integer; end;
  { Unrelated pointee overload result. }
  TOther = record Code: Integer; end;
  { Distinct pointer overload result. }
  TDistinct = record Code: Integer; end;
  { Primitive pointer overload result. }
  TPrimitive = record Code: Integer; end;
  { Distinct primitive pointee overload result. }
  TDistinctTarget = record Code: Integer; end;
  { Nested pointer overload result. }
  TNested = record Code: Integer; end;
  { Concrete generic pointee overload result. }
  TGeneric = record Code: Integer; end;
  { A different generic pointee must keep its separate overload. }
  TOtherGeneric = record Code: Integer; end;
  { Recursive declared-pointee overload result. }
  TRecursiveResult = record Code: Integer; end;
  { Native output separately observes a qualified ancestry conversion. }
  TAncestorResult = record Code: Integer; end;
  { One operand owns all visible overloads, exercising ordinary exact-first
    resolution rather than an invented standalone pointer comparison API. }
  TSelector = record
    class operator +(const A: PWidget; const B: TSelector): TExact;
    class operator +(const A: Pointer; const B: TSelector): TFallback;
    class operator +(const A: POtherWidget; const B: TSelector): TOther;
    class operator +(const A: PLong; const B: TSelector): TPrimitive;
    class operator +(const A: PCount; const B: TSelector): TDistinctTarget;
    class operator +(const A: PPWidget; const B: TSelector): TNested;
    class operator +(const A: PBoxWidget; const B: TSelector): TGeneric;
    class operator +(const A: PBoxOther; const B: TSelector): TOtherGeneric;
    class operator +(const A: PRecursive; const B: TSelector): TRecursiveResult;
  end;
  { This separate family preserves nominal exact matching without making every
    equivalent record-pointer expression ambiguous in the valid witness. }
  TDistinctSelector = record
    class operator +(const A: TDistinctPointer; const B: TDistinctSelector): TDistinct;
  end;
  { A single ancestor candidate avoids conflating compatibility with ambiguity
    between equally ranked ancestor and void-pointer conversion candidates. }
  TClassSelector = record
    class operator +(const A: PParent; const B: TClassSelector): TAncestorResult;
  end;
  { Both candidates intentionally have equivalent pointees. Conditional native
    programs below must refuse, independently of the successful witness. }
  TAmbiguousSelector = record
    class operator +(const A: PWidget; const B: TAmbiguousSelector): TExact;
    class operator +(const A: TDistinctPointer; const B: TAmbiguousSelector): TDistinct;
  end;

var GText, GProfile: AnsiString; GCalls, GMarker: Integer;
  P: PWidget; AliasP: PAliasWidget; SeparateP: PSeparateWidget;
  OtherP: POtherWidget; DistinctP: TDistinctPointer; LongP: PLong; CountP: PCount;
  PP: PPWidget; AliasPP: PPAliasWidget; BoxP: PBoxWidget; OtherBoxP: PBoxOther;
  RecursiveP: PRecursive; Selector: TSelector;
  SpecificSelector: TDistinctSelector; AmbiguousSelector: TAmbiguousSelector;
  Widget: TWidget; Other: TOtherWidget; Number: LongInt; Count: TCount;
  Box: TBox<TWidget>; OtherBox: TBox<TOtherWidget>; Recursive: TRecursive;
  CopiedWidget: TDistinctWidget; CopiedWidgetP: PDistinctWidget;
  AliasBoxP: PBoxAlias; AliasOtherBoxP: POtherBoxAlias;
  Parent: TParent; Child: TChild; ParentP: PParent; ChildP: PChild;
  ClassSelector: TClassSelector;

class operator TSelector.+(const A: PWidget; const B: TSelector): TExact;
begin Inc(GCalls); GMarker := 1; Result.Code := GMarker; end;
class operator TSelector.+(const A: Pointer; const B: TSelector): TFallback;
begin Inc(GCalls); GMarker := 2; Result.Code := GMarker; end;
class operator TSelector.+(const A: POtherWidget; const B: TSelector): TOther;
begin Inc(GCalls); GMarker := 3; Result.Code := GMarker; end;
class operator TDistinctSelector.+(const A: TDistinctPointer; const B: TDistinctSelector): TDistinct;
begin Inc(GCalls); GMarker := 4; Result.Code := GMarker; end;
class operator TAmbiguousSelector.+(const A: PWidget; const B: TAmbiguousSelector): TExact;
begin Inc(GCalls); GMarker := 1; Result.Code := GMarker; end;
class operator TAmbiguousSelector.+(const A: TDistinctPointer; const B: TAmbiguousSelector): TDistinct;
begin Inc(GCalls); GMarker := 4; Result.Code := GMarker; end;
class operator TSelector.+(const A: PLong; const B: TSelector): TPrimitive;
begin Inc(GCalls); GMarker := 5; Result.Code := GMarker; end;
class operator TSelector.+(const A: PCount; const B: TSelector): TDistinctTarget;
begin Inc(GCalls); GMarker := 6; Result.Code := GMarker; end;
class operator TSelector.+(const A: PPWidget; const B: TSelector): TNested;
begin Inc(GCalls); GMarker := 7; Result.Code := GMarker; end;
class operator TSelector.+(const A: PBoxWidget; const B: TSelector): TGeneric;
begin Inc(GCalls); GMarker := 8; Result.Code := GMarker; end;
class operator TSelector.+(const A: PBoxOther; const B: TSelector): TOtherGeneric;
begin Inc(GCalls); GMarker := 9; Result.Code := GMarker; end;
class operator TSelector.+(const A: PRecursive; const B: TSelector): TRecursiveResult;
begin Inc(GCalls); GMarker := 10; Result.Code := GMarker; end;
class operator TClassSelector.+(const A: PParent; const B: TClassSelector): TAncestorResult;
begin Inc(GCalls); GMarker := 11; Result.Code := GMarker; end;

function Keep<T>(const A: T): T;
begin Result := A; end;

procedure Emit<T>(const ALabel: AnsiString; const A: T);
begin
  GText := GText + GProfile + '|' + ALabel + '|' +
    AnsiString(PTypeInfo(TypeInfo(T))^.Name) + '|' + AnsiString(IntToStr(GMarker)) + #10;
end;

{$T+}
procedure TypedAddresses;
begin
  GProfile := 'typed';
  {$I pointer.identity.cases.inc}
end;

{$T-}
procedure UntypedAddresses;
begin
  GProfile := 'untyped';
  {$I pointer.identity.cases.inc}
end;

{ A source macro cannot retarget the compiler. The complete original cases
  execute again with typed addresses after masking that namespace spelling. }
{$undef CPUI386}
{$T+}
procedure MaskedTargetAddresses;
begin
  GProfile := 'masked';
  {$I pointer.identity.cases.inc}
end;
{$define CPUI386}

{$T+}
procedure AmbiguousExamples;
begin
  {$ifdef NYX_POINTER_AMBIGUITY}
  Emit('ambiguous-separate', SeparateP + AmbiguousSelector);
  {$endif}
  {$ifdef NYX_POINTER_AMBIGUITY_ADDRESS}
  Emit('ambiguous-address', @Widget + AmbiguousSelector);
  {$endif}
end;

function NativePointerIdentity: AnsiString;
begin
  GText := ''; GCalls := 0;
  Widget.Value := 42; Other.Value := 17; Number := 42; Count := 17;
  Box := nil; OtherBox := nil; Recursive.Next := nil;
  P := @Widget; AliasP := P; SeparateP := P; OtherP := @Other;
  DistinctP := TDistinctPointer(P); LongP := @Number; CountP := @Count;
  PP := @P; AliasPP := @AliasP; BoxP := @Box; OtherBoxP := @OtherBox;
  RecursiveP := @Recursive;
  CopiedWidget.Value := 13; CopiedWidgetP := @CopiedWidget;
  AliasBoxP := @Box; AliasOtherBoxP := @OtherBox;
  Parent := nil; Child := nil; ParentP := @Parent; ChildP := @Child;
  ClassSelector := Default(TClassSelector);
  Selector := Default(TSelector); SpecificSelector := Default(TDistinctSelector);
  AmbiguousSelector := Default(TAmbiguousSelector);
  TypedAddresses; UntypedAddresses; MaskedTargetAddresses;
  Result := GText;
end;

function NativePointerCalls: Integer;
begin Result := GCalls; end;

end.
