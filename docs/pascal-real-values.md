# Exact real values and compiler constant expressions

Nyx keeps source text, mathematical values, represented binary values and Pascal
types separate. This prevents an editor query from silently using its own host
floating-point format to predict a different compiler.

`nyx.pascal.reals` has no native floating-point dependency. An exact source value
retains its complete decimal coefficient, exponent, original unsigned spelling
and separate sign. A represented value retains an explicit binary format, sign,
normalized binary significand and exponent. Both are detached value records.

```pascal
uses nyx.pascal.reals;

LLeft := NyxPascalRoundRealToEven(
  NyxPascalRealToken('1.25'), pbpBinary64);
LRight := NyxPascalRoundRealToEven(
  NyxPascalRealToken('2.5'), pbpBinary64);
LSum := NyxPascalCalculateRealToEven(
  proAdd, LLeft, LRight, pbpBinary64);

if LSum.TryCompare(LLeft, LOrder) then
begin
  { LOrder is positive: the represented sum is greater than the left operand. }
end;
```

The mathematical operations are addition, subtraction, multiplication and
division. Inputs can have different formats; the operation uses their actual
represented values and rounds the exact result once to the requested format.
`RoundToEven` explicitly converts a represented value again. It cannot recover
bits lost during an earlier parse or operation. Binary32, binary64, x87 extended80
and binary128 are explicit mathematical choices; their availability does not
establish support in a particular compiler or target.

`Inexact` describes the current operation or conversion, independently of earlier
history. Exact cancellation produces positive zero under nearest-even rounding.
Equal negative zeros add to negative zero; multiplication and division combine
operand signs. Exact unary `Negated` preserves signed zero and clears `Inexact`.
Finite comparison treats the two signed zeros as equal and compares actual
values across formats. Unknown/nonfinite operands supply no comparison proof;
`TryCompare` returns False and leaves its out relation zero.

The closed outcomes distinguish finite values, finite-format overflow, nonzero
division by zero and zero divided by zero. Failure outcomes do not construct
infinities or NaNs, and they do not predict a compiler diagnostic. Unknown input
or precision remains Unknown. Invalid operation/precision enums and addressable
work-capacity failures raise the existing Pascal-value exception family.

All arithmetic accepts the existing optional cancellation scope. An analysis
budget charges the actual work, including integer storage and digit processing.
Cancellation or budget refusal publishes no partial value or comparison. Results
retain no work integers, budget, thread, source world or callback.

`nyx.pascal.ieee` adds a separate abstract IEEE value owner. Explicit constructors
represent finite values, signed infinities and unordered NaN meaning. It performs
nearest-even arithmetic/rounding without host floats, delegates finite work to
the exact binary owner and distinguishes unknown from known unordered results.
NaN sign/payload/signaling state are outside this contract; its sign and finite
value getters refuse rather than inventing those properties. Wrapping a failed
finite conversion does not create an infinity: permissive IEEE projection is
an explicit operation. This mathematical owner does not grant compiler admission
or infer exception masks.

`NyxPascalRoundIntegerToRealEven` converts the existing exact signed/unsigned-64
integer value domain without a host integer or floating conversion. It refuses
Boolean and nominal ordinal values; an ordinal needs its explicit integer
projection. This mathematical conversion remains independent of a compiler's
implicit conversion stages and intermediate precision.

## Resolver integration

The compiler context has two independent optional capabilities:

- `INyxPascalContextRealParser` describes qualified decimal parsing.
- `INyxPascalContextRealEvaluation` describes qualified compiler-host operation
  rounding, through `TNyxPascalRealEvaluationPolicy`.

Their constructors record evidence supplied by the profile owner. A target
define, primitive byte count, source FPU selector or parsing policy cannot
establish the evaluation policy. Missing capabilities, legacy contexts and
normally enrolled profiles remain Unknown. This implementation does not enroll
either policy automatically or inspect a host to invent one.

Under the qualified FPC 3.3.1 Delphi/i386 contract, literal and expression queries
also require the active source settings and existing compiler-root primitive
facts. Real constant arithmetic selects the nominal result type, evaluates at
the independently qualified compiler-host precision, then narrows to the result
format. Each operation in a nested expression retains its intermediate value.
Represented-value comparisons require no arithmetic-rounding policy.

`INyxPascalRealLiteral` exposes finite original-source and parsed-value evidence.
`INyxPascalRealConstant` exposes a represented finite value on an admitted literal
or expression. A folded expression does not implement the source-literal
capability. Generic calls inherit the specialization type without inheriting a
proof about their return value. Ordinary integer/Boolean constant channels never
receive a real value disguised as an integer.

`INyxPascalIEEERealLiteral` additionally exposes exact source mathematics and
`ParsedIEEEValue`, including a qualified parser infinity. It is available on
finite literals too; the original finite interfaces keep their GUIDs and method
layouts and are absent on infinity. The parser precision remains separate from
the compiler's selected nominal literal type. A generic result inherits neither
the argument's source proof nor its value.

For the qualified FPC 3.3.1 Delphi/i386 parser, sufficiently small nonzero tokens
can round to signed zero under either IEEE checking setting. The exact source
still records a nonzero value, and the parsed finite value retains its original
`Inexact` evidence. An overflowing token requires known disabled IEEE checking at
its unsigned real-node construction. Both `1.0e9999` and `-1.0e9999` therefore
refuse under active checking; a unary minus cannot rescue an invalid child.
Only after that check does the sign contribute to the admitted literal value.
The minimum-precision equality test also applies to infinity and signed zero.
Immediate parser/FPU/minimum facts and queued IEEE requests remain distinct.
Caller tokens beyond the qualified RTL parser's 255-unit short-string boundary
refuse without truncation; the standalone mathematical owner retains its wider
exact-input contract.

`INyxPascalConstantExpression` now makes the existing scalar-expression contract
public, preserving its original GUID and method layout. Its `ConstantValue`
contains an independently proven integer or Boolean, otherwise Unknown;
`ResultType` carries the ordinary semantic type. A real expression instead uses
`INyxPascalRealConstant`. Neither capability is inherited by a generic return.

Constant unary signs retain the operand's represented type; they do not apply
the widening used for a runtime negative value. Mixed integer constants convert
first through the compiler-host representation, then through the common real
operand type, before arithmetic or comparison. This distinction matters:
under the measured FPC contract, `16777217 = 16777216.0` is True with a Single
minimum and False with a Double minimum. A runtime operand supplies only type
proof even when a particular optimizer might simplify the expression.

Integer-only `/` can retain a real constant when explicit evaluation evidence
is present. Without that evidence its existing type-only answer remains
value-unknown; this is not a claim of complete constant range/admission analysis.

Untyped numeric declarations retain the initializer's own lexical scope and
source settings. Later use cannot change a constant's declaration-time minimum
or reinterpret its value through a different local shadow. Each dependency
evaluates at its original declaration, with a bounded, query-owned recursion
path. Forward/self references, incomplete declarations and unknown source effects
refuse. A reference exposes a constant-expression proof without pretending that
the reference name is an original numeric literal. Typed constants retain their
ordinary storage type and supply no constant value, including under `{$J-}`.

Explicit binary real casts accept one complete numeric expression and retain
each conversion stage. `Double(Single(0.1))` therefore preserves the earlier
Single rounding. Integer arguments require independent host-conversion evidence;
already represented real arguments do not invent another host integer stage.
Runtime operands retain the target type without a fabricated value. A distinct
cast target such as `TAmount = type Double` retains its ordinary declaration
identity and its generic specialization identity. Other cast families keep their
existing contracts; general conversion/admission is still incomplete.

Qualified finite real casts now retain positive/negative zero on underflow.
The independent compiler accepts those results with IEEE errors both enabled
and disabled. Cast admission remains distinct from unsigned source-literal
construction and from the standalone mathematical conversion.

`TNyxPascalCompilerPolicies.RequestIEEEErrors` records authored IEEE checking
intent independently of R/Q checks, host masks and startup defaults. The shared
translator understands the qualified `IEEEERRORS` directive; source states,
local PUSH/POP, mode/capture and exact storage identity retain that request.
It is an authored setting, not a claim that a queued local switch has already
activated on a compiler node. The qualified resolver follows that node setting
through the same bounded scanner chronology as excess precision. Pending local
requests do not activate early; IFOPT flushes even through a false branch. A
copied named constant is a new node at its use site, while its initializer keeps
its original source scope and settings. Missing required facts remain explicit.

`INyxPascalIEEEConstant` optionally exposes an admitted finite/infinity/NaN value
on a literal or expression. `INyxPascalRealConstant` retains its original GUID,
methods and finite-only meaning; a special value never advertises that interface.
Scalar Boolean proofs express comparisons: NaN is unordered, so equality and
ordered relations are False and inequality is True. Runtime and generic results
retain ordinary types without inheriting an argument's value evidence.

This is qualified FPC 3.3.1 Delphi/i386 behavior with the existing measured roots
and explicit parser/evaluator facts. FPC checks a zero divisor separately before
folding. Its inspected real-node range check rejects positive infinity when IEEE
checking is active; negative infinity and NaN are distinct cases. Unary negation
reuses an admitted child without constructing a second checked real node. These
compiler admission rules remain separate from abstract IEEE mathematics and
host exception masks. Casts and alias/distinct targets keep their authored type.

These optional facts participate in exact context, capture and storage identity.
Source MODE, local PUSH/POP and unknown source effects do not replace initial
compiler-host metadata. Resolver arithmetic consumes the existing remaining
semantic-step allowance, including when no aggregate budget is supplied.

The native witness independently checks 85 arithmetic values and 30 comparisons
at five authored precision/FPU sites. The resolver queries the original complete
unit and repeated includes; it invokes no witness bodies. Browser consumers
compile only. A second independent whole native unit adds 140 unary/mixed
conversion cases, including actual comparison values and runtime proof barriers.
A third independent whole unit adds 100 declaration and cast cases, including
the original repeated declaration/case includes and nominal type evidence.
Thirty independent native IEEE values/relations and fourteen separate compiler
inputs qualify the abstract layer and checking distinction. Four intentional
compiler errors remain separate from product warning debt. A further whole native
unit supplies 191 original expression windows across checking/precision/FPU sites;
semantic queries inspect those inputs without invoking their compiled bodies.
Thirteen separate compiler inputs distinguish node admission and delayed settings.
Eighteen further independent compiler inputs distinguish source-token overflow,
signed underflow and unsigned-node checking from queued switch activation.
General cast families, arbitrary intrinsic admission, broader parser/compiler
families and default profile enrollment remain open, alongside full editor
integration and target parity. The separately qualified conditional evaluator
is described below.

## Compiler conditional expressions

`TNyxPascalConstantOptions.Defaults.InDialect(pcdFPCConditional)` selects FPC's
separate conditional numeric rules. Real tokens and `/` use the explicit
compiler-host parser/evaluation facts. Target storage formats, `MINFPCONSTPREC`,
source FPU selection and IEEE/R/Q requests do not establish those facts or alter
this evaluator's arithmetic. Qualified FPC 3.3.1 compiler entry code masks host
exceptions before evaluating these conditions. Infinity and unordered NaN are
therefore distinct from ordinary Pascal real-node admission.

A numeric result exposes the existing optional `INyxPascalIEEEConstant` interface.
Its scalar `INyxPascalConstantResult.Value` stays Unknown; Boolean comparisons
use the original scalar channel. A real by itself does not become a valid Boolean
directive. Well-formed short-circuited branches require no unused numeric facts
or declaration lookups. Unsupported grammar still refuses the complete plan.

Declared constants retain the value of their original Pascal initializer, with
its own source scope, rounding and checks. A later directive cannot reinterpret
`Single(0.1)` as an unrounded host literal. Source binding also captures qualified
names, nominal cast targets and transitive constant dependencies. Query-local
memo values retain no mutable arithmetic work or application state; independent
readers share immutable plans and worlds only.

Numeric children consume the existing remaining constant-work allowance.
Proved distant range bounds use `10 > 2^3`, including the complete coefficient's
decimal order, to avoid constructing enormous powers for certain zero/infinity
outcomes. Borderline values still receive full rational rounding. No host float,
logarithm, input truncation or increased budget supplies the answer.

The original whole native unit and repeated includes qualify selected/opposite
branches, local shadowing and target-setting independence. Compiler-conditional
macro/string/set/general intrinsic behavior, ordinary source-parser range,
default profile enrollment and full language/editor/target adoption remain open.
