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

unit nyx.collections.query;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.collections;

const
  NyxMaximumQueryPredicates = 64;
  NyxMaximumQueryDepth = 16;
  NyxMaximumQuerySorts = 8;
  NyxMaximumQueryBytes = 65536;

type
  { Portable ordering has no dependency on a machine locale or JS collation.
    Text uses Unicode scalar order. The explicitly named ASCII alternative folds
    A..Z only; other scalars, combining sequences and displayed text stay exact. }
  TNyxQueryTextComparison = (nqtExact, nqtAsciiInsensitive);
  TNyxSortDirection = (nsdAscending, nsdDescending);
  TNyxQueryComparison = (nqcEqual, nqcNotEqual, nqcLess, nqcAtMost,
    nqcGreater, nqcAtLeast, nqcContains, nqcStartsWith, nqcEndsWith);
  TNyxQueryPredicateKind = (nqpField, nqpAll, nqpAny, nqpNot);

  { Immutable reference-counted predicate tree. Factories own only scalar values
    and immutable children, never stores, documents, widgets or callbacks.
    Branch construction cannot introduce cycles; depth/node budgets reject
    before returning a new tree. Leaf-only accessors refuse branch reads.
    Validate checks exact schema field families. Matches refuses missing/wrong
    scalar fields instead of coercing formatted text into numeric behavior.
    ToData/FromData are the closed persistence/extension boundary. }
  INyxCollectionPredicate = interface
    ['{CC0F2905-7F1D-4F03-A17D-F2B5233462E9}']
    function GetKind: TNyxQueryPredicateKind;
    function GetFieldName: TNyxText;
    function GetFieldKind: TNyxStateKind;
    function GetComparison: TNyxQueryComparison;
    function GetTextComparison: TNyxQueryTextComparison;
    function GetExpected: TNyxStateValue;
    function GetChildCount: Integer;
    function Child(AIndex: Integer): INyxCollectionPredicate;
    function GetNodeCount: Integer;
    function GetDepth: Integer;
    function AndAlso(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
    function OrElse(const AOther: INyxCollectionPredicate): INyxCollectionPredicate;
    function Negated: INyxCollectionPredicate;
    procedure Validate(const ASchema: TNyxCollectionSchema);
    function Matches(const AItem: TNyxCollectionItem): Boolean;
    function ToData: TNyxDataValue;
    property Kind: TNyxQueryPredicateKind read GetKind;
    property FieldName: TNyxText read GetFieldName;
    property FieldKind: TNyxStateKind read GetFieldKind;
    property Comparison: TNyxQueryComparison read GetComparison;
    property TextComparison: TNyxQueryTextComparison read GetTextComparison;
    property Expected: TNyxStateValue read GetExpected;
    property ChildCount: Integer read GetChildCount;
    property NodeCount: Integer read GetNodeCount;
    property Depth: Integer read GetDepth;
  end;

  { Family-specific fluent field facades. A Boolean field cannot accept text or
    numeric ordering; text search cannot be applied to an integer. Each facade
    owns its admitted open name and returns independently owned predicates. }
  TNyxTextWhere = record
  private
    FField: TNyxTextFieldRef;
    function Compare(const AValue: TNyxText; AComparison: TNyxQueryComparison;
      ATextComparison: TNyxQueryTextComparison): INyxCollectionPredicate;
  public
    function EqualTo(const AValue: TNyxText;
      AComparison: TNyxQueryTextComparison = nqtExact): INyxCollectionPredicate;
    function NotEqualTo(const AValue: TNyxText;
      AComparison: TNyxQueryTextComparison = nqtExact): INyxCollectionPredicate;
    function Contains(const AValue: TNyxText;
      AComparison: TNyxQueryTextComparison = nqtExact): INyxCollectionPredicate;
    function StartsWith(const AValue: TNyxText;
      AComparison: TNyxQueryTextComparison = nqtExact): INyxCollectionPredicate;
    function EndsWith(const AValue: TNyxText;
      AComparison: TNyxQueryTextComparison = nqtExact): INyxCollectionPredicate;
  end;
  TNyxBooleanWhere = record
  private
    FField: TNyxBooleanFieldRef;
  public
    function EqualTo(AValue: Boolean): INyxCollectionPredicate;
    function NotEqualTo(AValue: Boolean): INyxCollectionPredicate;
  end;
  TNyxIntegerWhere = record
  private
    FField: TNyxIntegerFieldRef;
    function Compare(AValue: Integer;
      AComparison: TNyxQueryComparison): INyxCollectionPredicate;
  public
    function EqualTo(AValue: Integer): INyxCollectionPredicate;
    function NotEqualTo(AValue: Integer): INyxCollectionPredicate;
    function LessThan(AValue: Integer): INyxCollectionPredicate;
    function AtMost(AValue: Integer): INyxCollectionPredicate;
    function GreaterThan(AValue: Integer): INyxCollectionPredicate;
    function AtLeast(AValue: Integer): INyxCollectionPredicate;
  end;
  TNyxNumberWhere = record
  private
    FField: TNyxNumberFieldRef;
    function Compare(AValue: Double;
      AComparison: TNyxQueryComparison): INyxCollectionPredicate;
  public
    function EqualTo(AValue: Double): INyxCollectionPredicate;
    function NotEqualTo(AValue: Double): INyxCollectionPredicate;
    function LessThan(AValue: Double): INyxCollectionPredicate;
    function AtMost(AValue: Double): INyxCollectionPredicate;
    function GreaterThan(AValue: Double): INyxCollectionPredicate;
    function AtLeast(AValue: Double): INyxCollectionPredicate;
  end;

  { Detached immutable sort key. Compare returns -1/0/1, never subtracts integers
    or multiplies unchecked values. Equal keys remain equal; the projector must
    use a stable ordering to retain the accepted source order of ties. }
  TNyxCollectionSort = record
  private
    FInitialized: Boolean;
    FField: TNyxText;
    FKind: TNyxStateKind;
    FDirection: TNyxSortDirection;
    FTextComparison: TNyxQueryTextComparison;
    procedure Check;
  public
    function Copy: TNyxCollectionSort;
    procedure Validate(const ASchema: TNyxCollectionSchema);
    function Read(const AItem: TNyxCollectionItem): TNyxStateValue;
    function Compare(const ALeft, ARight: TNyxStateValue): Integer;
    function ToData: TNyxDataValue;
    property FieldName: TNyxText read FField;
    property Kind: TNyxStateKind read FKind;
    property Direction: TNyxSortDirection read FDirection;
    property TextComparison: TNyxQueryTextComparison read FTextComparison;
  end;

  { Reusable value policy. Empty/default means the unmodified dataset. Where
    replaces the predicate; WithoutFilter retains ordering. OrderBy starts a new
    ordering; ThenBy adds a distinct secondary field and refuses without a primary.
    Every fluent call copies its sort array explicitly for pas2js independence.
    Filter returns a separately owned interface tree; retain it for a complete
    evaluation rather than repeatedly reading it for individual rows.
    Predicates attached from another implementation are normalized through their
    closed descriptor into an independently owned immutable Nyx tree. }
  TNyxCollectionQuery = record
  private
    { pas2js cannot retain a COM interface in a record. This immutable owned
      descriptor preserves value semantics; Filter returns a new owned tree.
      Evaluators retain that interface once for the complete evaluation. }
    FFilterData: TNyxDataValue;
    FSorts: array of TNyxCollectionSort;
    function GetDefined: Boolean;
    function GetSortCount: Integer;
    function GetFilter: INyxCollectionPredicate;
    function AddSort(const ASort: TNyxCollectionSort;
      APrimary: Boolean): TNyxCollectionQuery;
  public
    function Copy: TNyxCollectionQuery;
    function Where(const APredicate: INyxCollectionPredicate): TNyxCollectionQuery;
    function WithoutFilter: TNyxCollectionQuery;
    function Unsorted: TNyxCollectionQuery;
    function OrderBy(const ASort: TNyxCollectionSort): TNyxCollectionQuery; overload;
    function OrderBy(const AField: TNyxTextFieldRef;
      ADirection: TNyxSortDirection = nsdAscending;
      AComparison: TNyxQueryTextComparison = nqtExact): TNyxCollectionQuery; overload;
    function OrderBy(const AField: TNyxBooleanFieldRef;
      ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionQuery; overload;
    function OrderBy(const AField: TNyxIntegerFieldRef;
      ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionQuery; overload;
    function OrderBy(const AField: TNyxNumberFieldRef;
      ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionQuery; overload;
    function ThenBy(const ASort: TNyxCollectionSort): TNyxCollectionQuery; overload;
    function ThenBy(const AField: TNyxTextFieldRef;
      ADirection: TNyxSortDirection = nsdAscending;
      AComparison: TNyxQueryTextComparison = nqtExact): TNyxCollectionQuery; overload;
    function ThenBy(const AField: TNyxBooleanFieldRef;
      ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionQuery; overload;
    function ThenBy(const AField: TNyxIntegerFieldRef;
      ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionQuery; overload;
    function ThenBy(const AField: TNyxNumberFieldRef;
      ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionQuery; overload;
    function SortAt(AIndex: Integer): TNyxCollectionSort;
    procedure Validate(const ASchema: TNyxCollectionSchema);
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCollectionQuery; static;
    property Defined: Boolean read GetDefined;
    property Filter: INyxCollectionPredicate read GetFilter;
    property SortCount: Integer read GetSortCount;
  end;

function NyxWhere(const AField: TNyxTextFieldRef): TNyxTextWhere; overload;
function NyxWhere(const AField: TNyxBooleanFieldRef): TNyxBooleanWhere; overload;
function NyxWhere(const AField: TNyxIntegerFieldRef): TNyxIntegerWhere; overload;
function NyxWhere(const AField: TNyxNumberFieldRef): TNyxNumberWhere; overload;
function NyxSort(const AField: TNyxTextFieldRef;
  ADirection: TNyxSortDirection = nsdAscending;
  AComparison: TNyxQueryTextComparison = nqtExact): TNyxCollectionSort; overload;
function NyxSort(const AField: TNyxBooleanFieldRef;
  ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionSort; overload;
function NyxSort(const AField: TNyxIntegerFieldRef;
  ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionSort; overload;
function NyxSort(const AField: TNyxNumberFieldRef;
  ADirection: TNyxSortDirection = nsdAscending): TNyxCollectionSort; overload;
function NyxCollectionQuery: TNyxCollectionQuery;
{ Read a bounded closed predicate descriptor. Null is not a predicate; an empty
  query represents absence. Unknown members/operators/families and inappropriate
  operations refuse rather than becoming executable callbacks. }
function NyxPredicateFromData(const AData: TNyxDataValue): INyxCollectionPredicate;

implementation

{$include nyx.collections.query.implementation.inc}

end.
