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
program nyx_pascal_types_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$I nyx.callback.features.inc}

uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.types, nyx.pascal.syntax.translated, nyx.bytes
  {$ifndef PAS2JS}, TypInfo, calling.winapi.fixture{$endif};

type
  {$ifndef PAS2JS}
  { Native compiler oracle for the same multidimensional/pointer forms inspected
    below. This type is never confused with a resolved analyzer symbol. }
  TCompilerGrid = array[0..2, 1..4] of Integer;
  { Compiler-owned pointer oracle; no object/application lifetime is involved. }
  PCompilerGrid = ^TCompilerGrid;
  { The real compiler accepts the same source-level code-page declarations.
    This ordinary test does not publish compiler-root presence/size facts. }
  TCompilerUTF8Text = type AnsiString(CP_UTF8);
  { Explicit keyword syntax selects its authored code page independently of
    the analyzer's separate default-String policy and intrinsic capture. }
  TCompilerWesternText = type String(1252);
  { Compiler-qualified procedural type with explicit parameter modes. }
  TCompilerCallback = function(const AText: TNyxText; var ACount: Integer): Boolean of object;
  { The compiler oracle keeps the same adjacent generic closer/alias delimiter
    exercised by the declared-syntax parser, without rewriting the input text. }
  TCompilerBox<T>=class
    Value: T;
  end;
  {$endif}
  { Controlled cancellation borrows no parser or snapshot; construction polls it
    without publishing an unfinished candidate. }
  TTypeCancellation = class(TInterfacedObject, INyxCancellationScope)
  private
    FCancelled: Boolean;
  public
    Polls: Integer;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure TTypeCancellation.Cancel;
begin
  FCancelled := True;
end;

function TTypeCancellation.GetCancelled: Boolean;
begin
  Inc(Polls);
  Result := FCancelled or (Polls >= 8);
end;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Parse(const AText: TNyxText): INyxPascalTypeSyntax;
var
  LLexical: INyxPascalLexicalSnapshot;
begin
  LLexical := NewNyxPascalLexicalSnapshot(AText);
  Result := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, LLexical.Scalars),
    TNyxPascalTypeOptions.Defaults);
end;

procedure Validate(const AType: INyxPascalTypeSyntax);
var
  LAt: Integer;
  LNode: TNyxPascalTypeNode;
  LChild: Integer;
  LSteps: Integer;
  LParent: Integer;
begin
  Check((AType.Root >= 0) and (AType.Root < AType.Count), 'The type arena has a valid root');
  for LAt := 0 to AType.Count - 1 do
  begin
    LNode := AType.Node(LAt);
    Check((LNode.Span.Start >= AType.Window.Start) and
      (LNode.Span.Finish <= AType.Window.Finish), 'Every type span remains in its exact source window');
    LChild := LNode.FirstChild;
    LSteps := 0;
    while LChild >= 0 do
    begin
      Inc(LSteps);
      Check((LSteps <= AType.Count) and (AType.Node(LChild).Parent = LAt),
        'Type children are acyclic with exact parent ownership');
      LChild := AType.Node(LChild).NextSibling;
    end;
    LParent := LAt;
    LSteps := 0;
    while LParent >= 0 do
    begin
      Inc(LSteps);
      Check(LSteps <= AType.Count, 'Type parent links have no cycle');
      LParent := AType.Node(LParent).Parent;
    end;
  end;
end;

procedure EncodedStringTypes;
const
  CInvalid: array[0..6] of TNyxText = ('AnsiString()', 'AnsiString(CP_UTF8',
    'AnsiString(1252, 1251)', 'AnsiString((CP_UTF8])', 'TBox(1252)',
    'UnicodeString(1252)', 'String[10](1252)');
var
  LType: INyxPascalTypeSyntax;
  LBase, LCodePage, LAt: Integer;
  LRejected: Boolean;
  LLexical: INyxPascalLexicalSnapshot;
  {$ifndef PAS2JS}
  LUTF8: TCompilerUTF8Text;
  LWestern: TCompilerWesternText;
  {$endif}
begin
  LType := Parse('type AnsiString(CP_UTF8)');
  Validate(LType);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxDistinct),
    'The actual RTL distinct-string declaration retains its outer nominal constructor');
  LBase := LType.Child(LType.Root, ptrTarget);
  Check((LType.Node(LBase).Kind = ptxEncodedString) and
    (LType.Node(LType.Child(LBase, ptrBase)).Name = 'AnsiString'),
    'The authored ANSI base is a separate source node inside the encoded-string shape');
  LCodePage := LType.Child(LBase, ptrCodePage);
  Check((LType.Node(LCodePage).Kind = ptxBound) and
    (LType.Lexical.Slice(LType.Node(LCodePage).Span) = 'CP_UTF8'),
    'A code-page operand is exact unevaluated value syntax, never a type argument');
  LType := Parse('array of AnsiString((CP_UTF8 + 0))');
  Validate(LType);
  LBase := LType.Child(LType.Root, ptrElement);
  Check(LType.Complete and
    (LType.Lexical.Slice(LType.Node(LType.Child(LBase, ptrCodePage)).Span) = '(CP_UTF8 + 0)'),
    'Nested operand grouping survives inside a real enclosing array type');
  LType := Parse('String(1252)');
  Validate(LType);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxEncodedString) and
    (LType.Node(LType.Child(LType.Root, ptrBase)).Kind = ptxString),
    'Keyword and named string bases retain their different authored nodes');
  for LAt := 0 to High(CInvalid) do
  begin
    LType := Parse(CInvalid[LAt]);
    Check(not LType.Complete, 'Missing/multiple operands and unrelated type calls retain refusal');
  end;
  LLexical := NewNyxPascalLexicalSnapshot('AnsiString(CP_UTF8)');
  LRejected := False;
  try
    LType := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, LLexical.Scalars),
      TNyxPascalTypeOptions.Defaults.UpToNodes(2));
  except
    on ENyxPascalTypeLimit do LRejected := True;
  end;
  Check(LRejected, 'Base, request and operand share the existing node quota without truncation');
  {$ifndef PAS2JS}
  LUTF8 := 'English example';
  LWestern := 'English example';
  Check((StringCodePage(LUTF8) = CP_UTF8) and (StringCodePage(LWestern) = 1252),
    'The separately compiled ordinary declarations retain their requested code pages');
  {$endif}
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
end;

{ Adjacent generic delimiters retain the original immutable token. Only the
  closed first-angle boundary is admitted; names and other token cuts refuse. }
procedure GenericCloseWindows;
var
  LLexical: INyxPascalLexicalSnapshot;
  LType: INyxPascalTypeSyntax;
  LAt: Integer;
  LToken: Integer;
  LText: TNyxText;
  LBefore: TNyxPascalToken;
  LRejected: Boolean;
begin
  for LAt := 0 to 1 do
  begin

    if LAt = 0 then LText := 'TBox<Integer>>' else LText := 'TBox<Integer>=';
    LLexical := NewNyxPascalLexicalSnapshot(LText);
    LToken := LLexical.TokenAt(Length(LText) - 2);
    LBefore := LLexical.Token(LToken);
    LType := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, Length(LText) - 1),
      TNyxPascalTypeOptions.Defaults);
    Check(LType.Complete and (LType.Lexical = LLexical),
      'A borrowed first closing angle retains the original lexical owner');
    Validate(LType);
    Check((LType.Node(LType.Root).Span.Finish = Length(LText) - 1) and
      (LLexical.Token(LToken).Symbol = LBefore.Symbol) and
      (LLexical.Token(LToken).Span.Length = 2),
      'The exact window ends before its neighbour without modifying the source token');
  end;
  LLexical := NewNyxPascalLexicalSnapshot('TBox<Integer>');
  LRejected := False;
  try
    LType := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, 8),
      TNyxPascalTypeOptions.Defaults);
  except
    on ENyxPascalLexical do LRejected := True;
  end;
  Check(LRejected, 'A significant identifier cut still refuses instead of inventing a prefix type');
  LLexical := NewNyxPascalLexicalSnapshot('Integer>>');
  LType := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, 8),
    TNyxPascalTypeOptions.Defaults);
  Check(not LType.Complete, 'An unmatched borrowed closing angle cannot certify an ordinary name');
end;

procedure Types;
var
  LType: INyxPascalTypeSyntax;
  LLexical: INyxPascalLexicalSnapshot;
  LNode: Integer;
  LGeneric: Integer;
  LGroup: Integer;
  LInner: Integer;
  LRejected: Boolean;
  LCancel: INyxCancellationScope;
  LScope: TTypeCancellation;
begin
  LType := Parse('^array[0..2, 1..4] of System.TBox<TList<Integer>>');
  Validate(LType);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxPointer), 'Pointer syntax retains its structured target');
  LNode := LType.Child(LType.Root, ptrPointee);
  Check((LType.Node(LNode).Kind = ptxArray) and (LType.Child(LNode, ptrDimension, 1) >= 0),
    'Multidimensional arrays retain separate exact ordinal dimensions');
  LGeneric := LType.Child(LNode, ptrElement);
  Check((LType.Node(LGeneric).Kind = ptxGeneric) and
    (LType.Node(LType.Child(LGeneric, ptrBase)).Kind = ptxQualified),
    'Qualified generic bases are structured independently of their arguments');
  LInner := LType.Child(LGeneric, ptrArgument);
  Check((LType.Node(LInner).Kind = ptxGeneric) and
    (LType.Node(LType.Child(LInner, ptrArgument)).Name = TNyxText('Integer')),
    'Nested generic >> closers do not swallow type arguments');
  {$ifndef PAS2JS}
  Check((SizeOf(TCompilerGrid) = 12 * SizeOf(Integer)) and
    (SizeOf(PCompilerGrid) = SizeOf(Pointer)) and (SizeOf(TCompilerCallback) > 0),
    'Actual compiler declarations accept the array/pointer/callback oracle forms');
  {$endif}

  LType := Parse('reference to function(constref A,B: TList<INyxControl>; var Raw; out C: Integer): INyxBadge; cdecl');
  Validate(LType);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxReference),
    'Reference-to procedural syntax remains distinct from a method pointer');
  LNode := LType.Child(LType.Root, ptrTarget);
  LGroup := LType.Child(LNode, ptrParameter);
  Check((LType.Node(LGroup).ParameterMode = pspmConstRef) and
    (LType.Child(LGroup, ptrParameterName, 1) >= 0) and
    (LType.Node(LType.Child(LGroup, ptrParameterType)).Kind = ptxGeneric),
    'Grouped named parameters retain constref passing and one declared type');
  LGroup := LType.Child(LNode, ptrParameter, 1);
  Check((LType.Node(LGroup).ParameterMode = pspmVar) and
    (LType.Node(LType.Child(LGroup, ptrParameterType)).Kind = ptxUntyped),
    'Untyped var parameters are explicit rather than missing-type guesses');
  Check((LType.Node(LType.Child(LNode, ptrResult)).Name = TNyxText('INyxBadge')) and
    (LType.Node(LNode).CallingConvention = pccCdecl),
    'Specialized interface results and calling conventions remain typed syntax');

  LType := Parse('function(const AText: TNyxText; var ACount: Integer): Boolean of object');
  Check(LType.Complete and (ptfOfObject in LType.Node(LType.Root).Flags),
    'Object-method callback qualifiers are retained');
  LType := Parse('procedure is nested');
  Check(LType.Complete and (ptfNested in LType.Node(LType.Root).Flags),
    'Nested procedure variables have their own authored qualifier');
  LType := Parse('packed array of string[80]');
  LNode := LType.Child(LType.Root, ptrElement);
  Check(LType.Complete and (ptfPacked in LType.Node(LType.Root).Flags) and
    (LType.Node(LNode).Kind = ptxString) and (LType.Child(LNode, ptrLength) >= 0),
    'Packed dynamic arrays and short-string lengths retain distinct constructs');
  LType := Parse('set of (choiceFirst, choiceLast = 9)');
  Validate(LType);
  Check(LType.Complete and
    (LType.Node(LType.Child(LType.Root, ptrElement)).Kind = ptxEnumeration),
    'Set elements retain enumeration structure without evaluating assigned ordinals');
  Check(Parse('type System.Integer').Complete, 'Distinct aliases retain their authored type prefix');
  LType := Parse('specialize TBox<Integer>');
  Check(LType.Complete and (ptfSpecialize in LType.Node(LType.Root).Flags) and
    (LType.Node(LType.Root).Span.Start = 0),
    'Explicit specialization retains its keyword and exact outer source range');
  Check(not Parse('set[0..3] of Integer').Complete and not Parse('TBox<Integer><String>').Complete,
    'Invalid dimensions and duplicate generic argument lists retain explicit uncertainty');
  Check(Parse('class of TWidget').Complete, 'Class references retain their target type');
  Check(Parse('file of TRecord').Complete and Parse('file').Complete,
    'Typed and untyped file forms are explicit');
  Check(Parse('-10..$FF').Complete, 'Subrange bounds preserve exact signed/base spellings');
  Check(Parse('procedure(A: Integer = High(Integer))').Complete,
    'Nested default expressions remain exact opaque syntax');
  Check(not Parse('TList<').Complete and not Parse('array[0..1 of Integer').Complete,
    'Incomplete generic/array syntax cannot become a complete type');
  Check(not Parse('class end').Complete, 'Structured class bodies require the owning declaration scope');
  Check(Parse('&bitpacked').Complete and not Parse('procedure; &cdecl').Complete,
    'Escaped identifiers cannot become packing or ABI directives');
  Check(not Parse('Integer {unfinished').Complete,
    'An unfinished trailing comment remains lexical uncertainty within the selected type');
  Check(not Parse('TList<{$ifdef X}Integer{$endif}>').Complete,
    'Physical directives require captured translation before type inference');

  LLexical := NewNyxPascalLexicalSnapshot('prefix TBox<Integer> suffix');
  LType := NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(7, 13), TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and
    (LType.Lexical.Slice(LType.Node(LType.Root).Span) = TNyxText('TBox<Integer>')),
    'A bounded type window retains its original source coordinates');
  LRejected := False;
  try
    NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(8, 12), TNyxPascalTypeOptions.Defaults);
  except
    on ENyxPascalLexical do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'A caller cannot cut a significant type-name token');
  LRejected := False;
  try
    NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(7, 13),
      TNyxPascalTypeOptions.Defaults.UpToNodes(1));
  except
    on ENyxPascalTypeLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LType.Complete, 'Node quotas refuse without altering an older tree');
  LLexical := NewNyxPascalLexicalSnapshot('^^^Integer');
  LRejected := False;
  try
    NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, LLexical.Scalars),
      TNyxPascalTypeOptions.Defaults.UpToDepth(2));
  except
    on ENyxPascalTypeLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Recursive type depth is bounded independently of node count');
  LLexical := NewNyxPascalLexicalSnapshot('TList<,>');
  LRejected := False;
  try
    NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, LLexical.Scalars),
      TNyxPascalTypeOptions.Defaults.UpToDiagnostics(1));
  except
    on ENyxPascalTypeLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Diagnostic quotas preserve failure information without partial publication');
  LScope := TTypeCancellation.Create;
  LCancel := LScope;
  LLexical := NewNyxPascalLexicalSnapshot('TBox<TList<TList<TList<Integer>>>>');
  LRejected := False;
  try
    NewNyxPascalTypeSyntax(LLexical, NyxPascalSpan(0, LLexical.Scalars),
      TNyxPascalTypeOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LScope.Polls >= 8), 'Cancellation publishes no partial type candidate');
end;

procedure ProceduralDeclarations;
const
  CSource: TNyxText = 'unit CallbackShapes; interface type ' +
    'TCall=function(A:LongInt):LongInt; cdecl; ' +
    'TNext=LongInt; TNested=procedure is nested; ' +
    'TMethod=procedure of object; stdcall; ' +
    'TVarargs=procedure(A:LongInt); cdecl; varargs; ' +
    'TSlots=record Callback:function(A:LongInt):LongInt; cdecl; After:LongInt; end; ' +
    'TLast=procedure; &cdecl=LongInt; ' +
    'var Direct:procedure(A:LongInt); stdcall; After:LongInt; implementation end.';
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LType: INyxPascalTypeSyntax;
  LAt: Integer;
  LMembers: Integer;
  LText: TNyxText;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot(CSource, TNyxPascalSyntaxOptions.Defaults);
  Check((LSyntax.Lexical.Source = CSource) and (LSyntax.DiagnosticCount = 0),
    'Procedural suffixes retain the whole source and separate declarations without spurious type names');
  LAt := LSyntax.Named(0, 'TCall');
  LText := LSyntax.Lexical.Slice(LSyntax.Declaration(LAt).TypeSpan);
  Check(LText = TNyxText('function(A:LongInt):LongInt; cdecl'),
    'The callback type range owns its signature separator and calling directive exactly');
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccCdecl),
    'The ordinary declared-type consumer receives the authored callback convention');
  Check(LSyntax.Lexical.Slice(LSyntax.Declaration(LSyntax.Named(0, 'TNext')).TypeSpan) = TNyxText('LongInt'),
    'A following alias is not swallowed by the callback range');
  LAt := LSyntax.Named(0, 'TVarargs');
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccCdecl) and
    (ptfVarargs in LType.Node(LType.Root).Flags),
    'Multiple authored suffix separators remain one typed callback expression');
  LMembers := LSyntax.Declaration(LSyntax.Named(0, 'TSlots')).NestedScope;
  LAt := LSyntax.Named(LMembers, 'Callback');
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccCdecl) and
    (LSyntax.Named(LMembers, 'After') >= 0),
    'Inline record callback fields share suffix ownership and preserve the next field');
  LAt := LSyntax.Named(0, 'Direct');
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccStdcall) and
    (LSyntax.Named(0, 'After') >= 0),
    'Inline procedural variables preserve the directive and following variable');
  Check((LSyntax.Named(0, 'cdecl') >= 0) and
    (LSyntax.Lexical.Slice(LSyntax.Declaration(LSyntax.Named(0, 'TLast')).TypeSpan) = TNyxText('procedure')),
    'An escaped contextual identifier remains its own declaration');
  LType := Parse('procedure; &cdecl');
  Check(not LType.Complete,
    'The standalone type parser never treats an escaped identifier as a calling directive');
  LType := Parse('procedure; cdecl; stdcall');
  Check(not LType.Complete, 'Conflicting procedural conventions remain explicit type diagnostics');
end;

{ A procedural variable's convention separator belongs to declaration syntax,
  while its initializer stays outside the type grammar. These windows exercise
  the independent type reader, including an inner default and reference target. }
procedure InitializedProceduralVariables;
const
  CSource: TNyxText = 'unit InitializedCallbacks; interface var ' +
    'Release:procedure(_para1:Pointer); stdcall; = nil; ' +
    'Evaluate:function(AValue:LongInt=7):Boolean; cdecl; = nil; ' +
    'Deferred:reference to function:LongInt; cdecl; = nil; ' +
    'Tail:LongInt; implementation end.';
  CNames: array[0..2] of TNyxText = ('Release', 'Evaluate', 'Deferred');
  CTypes: array[0..2] of TNyxText = ('procedure(_para1:Pointer); stdcall',
    'function(AValue:LongInt=7):Boolean; cdecl', 'reference to function:LongInt; cdecl');
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LType: INyxPascalTypeSyntax;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LAt, LOrdinal, LTarget, LGroup, LDefault: Integer;
begin
  LSyntax := NewNyxPascalSyntaxSnapshot(CSource, TNyxPascalSyntaxOptions.Defaults);
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.Lexical.Source = CSource),
    'Complete initialized procedural declarations preserve their original source without recovery');
  for LAt := 0 to High(CNames) do
  begin
    LOrdinal := LSyntax.Named(0, CNames[LAt]);
    Check(LOrdinal >= 0, 'Each initialized callback retains its actual declared name');
    LDeclaration := LSyntax.Declaration(LOrdinal);
    Check((LSyntax.Lexical.Slice(LDeclaration.TypeSpan) = CTypes[LAt]) and
      (LSyntax.Lexical.Slice(LDeclaration.ExpressionSpan) = 'nil'),
      'The exact procedural type window excludes its separator and outer initializer');
    LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LDeclaration.TypeSpan,
      TNyxPascalTypeOptions.Defaults);
    LTarget := LType.Root;

    if LAt = 2 then
    begin
      Check(LType.Node(LTarget).Kind = ptxReference,
        'A reference callback keeps its distinct outer type family');
      LTarget := LType.Child(LTarget, ptrTarget);
    end;
    Check(LType.Complete and (LTarget >= 0) and
      (((LAt = 0) and (LType.Node(LTarget).CallingConvention = pccStdcall)) or
       ((LAt <> 0) and (LType.Node(LTarget).CallingConvention = pccCdecl))),
      'The independent type reader receives the complete authored callback convention');

    if LAt = 1 then
    begin
      LGroup := LType.Child(LTarget, ptrParameter);
      LDefault := LType.Child(LGroup, ptrDefault);
      Check((LDefault >= 0) and (LType.Lexical.Slice(LType.Node(LDefault).Span) = '7') and
        (LType.Node(LType.Child(LTarget, ptrResult)).Name = 'Boolean'),
        'An inner parameter default and function result remain independent of the outer nil initializer');
    end;
  end;
  Check((LSyntax.Named(0, 'nil') < 0) and (LSyntax.Named(0, 'Tail') >= 0),
    'No initializer becomes a fabricated declaration or swallows the following real variable');
end;

{ Declaration and procedural-type requests share a closed source contract.
  This checks authored intent and ownership, independently of compiler admission
  and the separate global CALLING policy domain. }
procedure WinapiRequests;
const
  CSource: TNyxText = 'unit ABIRequests; interface type ' +
    'TCall=function(A:LongInt):LongInt; WiNaPi; ' +
    'TSlots=record Callback:procedure; winapi; After:LongInt; end; ' +
    'TLast=procedure; &winapi=LongInt; ' +
    'var Direct:function(A:LongInt):LongInt; winapi = nil; After:LongInt; ' +
    'procedure Output(const Value:LongInt); winapi; ' +
    'function Answer:LongInt winapi; implementation ' +
    'procedure Output(const Value:LongInt); winapi; begin end; ' +
    'function Answer:LongInt; winapi; begin Result:=42; end; end.';
var
  LSyntax: INyxPascalSyntaxSnapshot;
  LType: INyxPascalTypeSyntax;
  LDecl: TNyxPascalSyntaxDeclaration;
  LAt: Integer;
  LMembers: Integer;
  LConvention: TNyxPascalCallingConvention;
  LSource: INyxPascalTranslation;
begin
  Check((Ord(pccDefault) = 0) and (Ord(pccCdecl) = 2) and
    (Ord(pccVectorcall) = 7) and (Ord(pccWinapi) = 8),
    'The shared convention contract preserves procedural-type ordinals');
  {$ifndef PAS2JS}
  Check(GetEnumName(TypeInfo(TNyxPascalCallingConvention), Ord(pccCdecl)) = 'pccCdecl',
    'The shared owner preserves the original native RTTI enum spellings');
  Check(PTypeInfo(TypeInfo(TNyxPascalCallingConvention))^.Name = 'TNyxPascalCallingConvention',
    'The compatibility type preserves its original native RTTI type name');
  {$endif}
  LConvention := nyx.pascal.syntax.prcWinapi;
  Check((LConvention = nyx.pascal.types.pccWinapi) and
    NyxPascalCallingConventionNamed('WiNaPi', LConvention),
    'The old qualified type API and declaration API share the same typed request');
  Check(not NyxPascalCallingConventionNamed('future_call', LConvention) and (LConvention = pccDefault),
    'Unknown spellings return no guessed convention');
  LSyntax := NewNyxPascalSyntaxSnapshot(CSource);
  Check(LSyntax.DiagnosticCount = 0, 'Winapi suffixes preserve complete declaration structure');
  LAt := LSyntax.Named(0, 'TCall');
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccWinapi),
    'Mixed-case authored Winapi stays distinct from stdcall and cdecl');
  Validate(LType);
  LMembers := LSyntax.Declaration(LSyntax.Named(0, 'TSlots')).NestedScope;
  LAt := LSyntax.Named(LMembers, 'Callback');
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LSyntax.Declaration(LAt).TypeSpan,
    TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccWinapi) and
    (LSyntax.Named(LMembers, 'After') >= 0), 'Record callback modifiers preserve the next field');
  LDecl := LSyntax.Declaration(LSyntax.Named(0, 'Direct'));
  LType := NewNyxPascalTypeSyntax(LSyntax.Lexical, LDecl.TypeSpan, TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccWinapi) and
    (LSyntax.Lexical.Slice(LDecl.ExpressionSpan) = TNyxText('nil')) and (LSyntax.Named(0, 'After') >= 0),
    'The procedural variable owns Winapi while its initializer and sibling remain separate');
  Check((LSyntax.Named(0, 'winapi') >= 0) and
    (LSyntax.Lexical.Slice(LSyntax.Declaration(LSyntax.Named(0, 'TLast')).TypeSpan) = TNyxText('procedure')),
    'An escaped contextual type name cannot become a preceding modifier');
  LDecl := LSyntax.Declaration(LSyntax.Named(0, 'Output'));
  Check((LDecl.CallingConvention = prcWinapi) and
    (LSyntax.Lexical.Slice(LDecl.CallingConventionSpan) = TNyxText('winapi')),
    'An ordinary routine exposes its typed request and exact keyword span');
  Check(LSyntax.Declaration(LSyntax.Named(LDecl.NestedScope, 'Value')).ParameterMode = pspmConst,
    'The convention preserves the original routine parameter mode');
  LDecl := LSyntax.Declaration(LDecl.NextNamed);
  Check((LDecl.CallingConvention = prcWinapi) and LSyntax.Scope(LDecl.NestedScope).Complete and
    not (psdfForward in LDecl.Flags), 'The implementation keeps its request and ordinary body');
  LDecl := LSyntax.Declaration(LSyntax.Named(0, 'Answer'));
  Check((LDecl.CallingConvention = prcWinapi) and
    (LSyntax.Lexical.Slice(LDecl.TypeSpan) = TNyxText('LongInt')),
    'An adjacent first modifier stays outside the routine result type');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Names; interface type TPlain=procedure; ' +
    'winapi=LongInt; function Value:Names.winapi; implementation end.');
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.Named(0, 'winapi') >= 0) and
    (LSyntax.Lexical.Slice(LSyntax.Declaration(LSyntax.Named(0, 'TPlain')).TypeSpan) = TNyxText('procedure')),
    'A contextual alias name retains its own equals-sign boundary');
  LDecl := LSyntax.Declaration(LSyntax.Named(0, 'Value'));
  Check((LDecl.CallingConvention = prcDefault) and
    (LSyntax.Lexical.Slice(LDecl.TypeSpan) = TNyxText('Names.winapi')),
    'A qualified result name cannot impersonate a following calling modifier');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Variables; interface var Plain:procedure; ' +
    'winapi:LongInt; After:LongInt; implementation end.');
  Check((LSyntax.DiagnosticCount = 0) and (LSyntax.Named(0, 'winapi') >= 0) and
    (LSyntax.Lexical.Slice(LSyntax.Declaration(LSyntax.Named(0, 'Plain')).TypeSpan) = TNyxText('procedure')),
    'A following contextual variable name retains its own colon and type');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Bracket; interface procedure P; [WiNaPi]; implementation end.');
  LDecl := LSyntax.Declaration(LSyntax.Named(0, 'P'));
  Check((LSyntax.DiagnosticCount = 0) and (LDecl.CallingConvention = prcWinapi) and
    (LSyntax.Lexical.Slice(LDecl.CallingConventionSpan) = TNyxText('WiNaPi')),
    'Bracket classification preserves intent without asserting native mode eligibility');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Conflict; interface procedure P; cdecl; winapi; implementation end.');
  LDecl := LSyntax.Declaration(LSyntax.Named(0, 'P'));
  Check((LSyntax.DiagnosticCount > 0) and (psdfUnsupported in LDecl.Flags) and
    (LDecl.CallingConvention = prcCdecl), 'Conflicting routine requests retain the first spelling and explicit refusal');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Operand; interface procedure P; [winapi:42]; implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and
    (psdfUnsupported in LSyntax.Declaration(LSyntax.Named(0, 'P')).Flags),
    'A convention marker cannot acquire an operand');
  LSyntax := NewNyxPascalSyntaxSnapshot('unit Unknown; interface procedure P; [winapi,future_call]; implementation end.');
  Check((LSyntax.DiagnosticCount > 0) and
    (psdfUnsupported in LSyntax.Declaration(LSyntax.Named(0, 'P')).Flags),
    'An unknown neighboring attribute keeps its uncertainty');
  LType := Parse('procedure; winapi; cdecl');
  Check(not LType.Complete and (LType.Node(LType.Root).CallingConvention = pccWinapi),
    'Conflicting procedural-type requests retain the first request without admission');
  LType := Parse('procedure; &winapi');
  Check(not LType.Complete, 'An escaped name is not a standalone calling modifier');
  LType := Parse('procedure; future_call');
  Check(not LType.Complete, 'Unknown procedural modifiers remain explicit type failures');
  LSource := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact).WithFile(
    NyxPascalPhysicalFile('global.pas'), '{$calling winapi}'), NyxPascalPhysicalFile('global.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  Check(not LSource.Resolved, 'The separate global CALLING table cannot silently admit the routine-only alias');
end;

{$ifndef PAS2JS}
{ Read the entire independently compiled native unit/include. Source inspection
  and actual callback execution qualify different contracts; neither computes a
  portable mapping from Winapi to another convention. }
procedure WholeWinapiInput;
var
  LFiles: INyxPascalFileCatalog;
  LName: TNyxText;
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LView: INyxPascalTranslatedSyntax;
  LDecl: TNyxPascalSyntaxDeclaration;
  LType: INyxPascalTypeSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LAt: Integer;
begin
  Check(CompilerWinapiValue = 42, 'The native compiler executes named, variable and record Winapi callbacks');

  if ParamCount = 0 then Exit;
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  for LAt := 0 to 1 do
  begin
    LName := 'calling.winapi.fixture.pas';

    if LAt = 1 then LName := 'calling.winapi.fixture.inc';
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'tests/' + String(LName),
      fmOpenRead or fmShareDenyWrite);
    try
      Check((LStream.Size > 0) and (LStream.Size < 65536), 'The whole Winapi source packet is bounded');
      SetLength(LBytes, LStream.Size);
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(LName), NyxDecodeUTF8(LBytes));
  end;
  LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
    NyxPascalPhysicalFile('calling.winapi.fixture.pas'), Facts, TNyxPascalTranslationOptions.Defaults),
    TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation and (LView.Syntax.DiagnosticCount = 0),
    'The complete native unit/include retain resolved translation and declaration syntax');
  LDecl := LView.Syntax.Declaration(LView.Syntax.Named(0, 'IncrementApi'));
  Check((LDecl.CallingConvention = prcWinapi) and LView.Syntax.Scope(LDecl.NestedScope).Complete,
    'The native ordinary implementation retains its request and complete body');
  LOrigin := LView.Origin(LView.OriginAt(LDecl.CallingConventionSpan.Start));
  Check(LView.Source.Files.FileAt(LView.Source.Occurrence(LOrigin.Occurrence).FileIndex).Name =
    TNyxText('calling.winapi.fixture.inc'), 'Convention navigation retains the original include');
  LDecl := LView.Syntax.Declaration(LView.Syntax.Named(0, 'Direct'));
  LType := NewNyxPascalTypeSyntax(LView.Syntax.Lexical, LDecl.TypeSpan, TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).CallingConvention = pccWinapi) and
    (LView.Syntax.Named(0, 'AfterValue') >= 0), 'The compiled initialized callback retains its typed request and sibling');
end;
{$endif}

procedure Projection;
const
  CRoot = 'unit Demo; interface const Caption = ''Ready 🐾''#0; type TBox<T>=class end; ' +
    '{$I one.inc} {$define SECOND} {$I one.inc} ' +
    'var Grid: ^array[0..2, 1..4] of TBox<Integer>; implementation end.';
  CInclude = '{$ifdef SECOND} TSecond {$else} TFirst {$endif} = TBox<INyxBadge>;';
var
  LFiles: INyxPascalFileCatalog;
  LTranslation: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
  LUnknown: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LType: INyxPascalTypeSyntax;
  LOrigin: TNyxPascalSyntaxOrigin;
  LPart: TNyxPascalSyntaxOrigin;
  LAt: Integer;
  LFirst: Integer;
  LSecond: Integer;
  LGeneric: Integer;
  LVirtual: Integer;
  LFile: INyxPascalLexicalSnapshot;
  LOptions: TNyxPascalSyntaxOptions;
  LRejected: Boolean;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('demo.pas'), CRoot)
    .WithFile(NyxPascalPhysicalFile('one.inc'), CInclude);
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('demo.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LView := NewNyxPascalTranslatedSyntax(LTranslation, LOptions);
  LSyntax := LView.Syntax;
  Check(LView.CompleteTranslation and (LSyntax.DiagnosticCount = 0),
    'Known-active translated declarations parse without guessing a conditional branch');
  LGeneric := LSyntax.Named(0, 'TBox');
  Check((LGeneric >= 0) and
    (LSyntax.Lexical.Slice(LSyntax.Declaration(LGeneric).GenericSpan) = TNyxText('< T >')) and
    (LSyntax.Declaration(LGeneric).NestedScope >= 0),
    'A physical >= token splits exactly between a generic header and alias equals');
  LFirst := LSyntax.Named(0, 'TFirst');
  LSecond := LSyntax.Named(0, 'TSecond');
  Check((LFirst >= 0) and (LSecond >= 0), 'Separate include occurrences retain both selected declarations');
  LFirst := LView.OriginAt(LSyntax.Declaration(LFirst).NameSpan.Start);
  LSecond := LView.OriginAt(LSyntax.Declaration(LSecond).NameSpan.Start);
  Check((LView.Origin(LFirst).FileIndex = LView.Origin(LSecond).FileIndex) and
    (LView.Origin(LFirst).Occurrence <> LView.Origin(LSecond).Occurrence) and
    (LView.Origin(LFirst).State <> LView.Origin(LSecond).State),
    'File identity never collapses repeated inclusion/state identity');
  for LAt := 0 to LView.OriginCount - 1 do
  begin
    LOrigin := LView.Origin(LAt);
    LFile := LTranslation.Physical(LOrigin.FileIndex);
    Check((LSyntax.Lexical.Slice(LOrigin.VirtualSpan) = LFile.Slice(LOrigin.PhysicalSpan)) and
      (LView.VirtualAt(LOrigin.Occurrence, LOrigin.PhysicalSpan.Start) = LOrigin.VirtualSpan.Start),
      'Every mapped token round-trips exact physical spelling and scalar coordinates');

    if LSyntax.Lexical.Slice(LOrigin.VirtualSpan) = TNyxText('''Ready 🐾''') then
    begin
      LPart := LView.Part(LAt, NyxPascalSpan(LOrigin.VirtualSpan.Start + 7, 1));
      Check(LFile.Slice(LPart.PhysicalSpan) = TNyxText('🐾'),
        'Partial physical mapping counts a supplementary scalar rather than UTF-8/UTF-16 units');
    end;

    if LAt > 0 then
    begin
      Check(LView.OriginAt(LOrigin.VirtualSpan.Start - 1) = -1,
        'Synthetic analysis separators cannot become authored positions');
    end;
  end;
  LVirtual := LSyntax.Declaration(LGeneric).GenericSpan.Finish - 1;
  LAt := LView.OriginAt(LVirtual);
  LPart := LView.Part(LAt, NyxPascalSpan(LVirtual, 1));
  Check(LTranslation.Physical(LPart.FileIndex).Slice(LPart.PhysicalSpan) = TNyxText('>'),
    'A partial >= range maps just the authored generic closer');
  LAt := LSyntax.Named(0, 'Grid');
  LType := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
  Validate(LType);
  Check(LType.Complete and (LType.Node(LType.Root).Kind = ptxPointer),
    'Declared translated field/variable types feed the same public structured type parser');

  LFiles := LFiles.WithFile(NyxPascalPhysicalFile('demo.pas'),
    'unit Maybe; interface var Known: Integer; {$ifdef UNCERTAIN}A{$else}B{$endif} implementation end.');
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('demo.pas'),
    Facts.CompleteDefines(False), TNyxPascalTranslationOptions.Defaults);
  LUnknown := NewNyxPascalTranslatedSyntax(LTranslation, LOptions);
  Check(not LUnknown.CompleteTranslation and (LUnknown.BlockedAt >= 0) and
    (LUnknown.Syntax.Named(0, 'Known') >= 0) and
    not LUnknown.Syntax.Scope(0).Complete,
    'Unknown significant participation ends a visible incomplete prefix instead of inventing a branch');
  LRejected := False;
  try
    NewNyxPascalTranslatedSyntax(LView.Source,
      LOptions.Lexical(TNyxPascalLexicalOptions.Defaults.UpToSourceBytes(8)));
  except
    on ENyxPascalLexicalLimit do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and LView.CompleteTranslation, 'Projection byte quota refuses while an earlier view survives');
end;

{$ifndef PAS2JS}
function ReadText(const APath: String): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

procedure ActualControls;
const
  CFiles: array[0..5] of String = ('nyx.controls.pas', 'nyx.controls.facades.inc',
    'nyx.controls.types.inc', 'nyx.controls.factories.inc', 'nyx.controls.implementation.inc',
    'nyx.controls.facades.implementation.inc');
var
  LFiles: INyxPascalFileCatalog;
  LView: INyxPascalTranslatedSyntax;
  LTranslation: INyxPascalTranslation;
  LType: INyxPascalTypeSyntax;
  LAt: Integer;
  LBadge: Integer;
  LMembers: Integer;
  LMethod: Integer;
  LOrigin: TNyxPascalSyntaxOrigin;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  for LAt := Low(CFiles) to High(CFiles) do
  begin
    LFiles := LFiles.WithFile(NyxPascalPhysicalFile(TNyxText(CFiles[LAt])),
      ReadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'src/' + CFiles[LAt]));
  end;
  LTranslation := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('nyx.controls.pas'),
    Facts, TNyxPascalTranslationOptions.Defaults);
  LView := NewNyxPascalTranslatedSyntax(LTranslation, TNyxPascalSyntaxOptions.Defaults);
  Check(LView.CompleteTranslation, 'Actual controls/includes have a resolved captured translation context');
  LBadge := LView.Syntax.Named(0, 'INyxBadge');
  while (LBadge >= 0) and (LView.Syntax.Declaration(LBadge).NestedScope < 0) do
  begin
    LBadge := LView.Syntax.Declaration(LBadge).NextNamed;
  end;
  Check(LBadge >= 0, 'The real specialized badge interface is visible through its include');
  LMembers := LView.Syntax.Declaration(LBadge).NestedScope;
  LMethod := LView.Syntax.Named(LMembers, 'WithText');
  Check(LMethod >= 0, 'The real fluent badge member remains in its declared interface scope');
  LType := LView.TypeOfDeclaration(LMethod, TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).Name = TNyxText('INyxBadge')),
    'The included method result is structured as its specialized interface name');
  LOrigin := LView.Origin(LView.OriginAt(LView.Syntax.Declaration(LMethod).NameSpan.Start));
  Check(LFiles.FileAt(LOrigin.FileIndex).Name = TNyxText('nyx.controls.types.inc'),
    'The ordinary included member maps to its actual physical declaration file');
  LMembers := LView.Syntax.Declaration(LMethod).NestedScope;
  LAt := LView.Syntax.Named(LMembers, 'AText');
  LType := LView.TypeOfDeclaration(LAt, TNyxPascalTypeOptions.Defaults);
  Check(LType.Complete and (LType.Node(LType.Root).Name = TNyxText('TNyxText')),
    'The real method parameter retains the portable text type');
  WriteLn('actual controls / ', LView.Syntax.DeclarationCount, ' declarations / ',
    LView.Syntax.DiagnosticCount, ' explicit grammar diagnostics / ',
    LView.OriginCount, ' mapped tokens');
end;
{$endif}

begin
  try
    Types;
    EncodedStringTypes;
    GenericCloseWindows;
    ProceduralDeclarations;
    InitializedProceduralVariables;
    WinapiRequests;
    Projection;
    {$ifndef PAS2JS}
    ActualControls;
    WholeWinapiInput;
    {$endif}
    WriteLn('PASS ', GChecks, ' structured-type/physical-origin checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
