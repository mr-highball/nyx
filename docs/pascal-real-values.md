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

`INyxPascalRealLiteral` exposes the original exact source and parsed value.
`INyxPascalRealConstant` exposes a represented finite value on an admitted literal
or expression. A folded expression does not implement the source-literal
capability. Generic calls inherit the specialization type without inheriting a
proof about their return value. Ordinary integer/Boolean constant channels never
receive a real value disguised as an integer.

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

These optional facts participate in exact context, capture and storage identity.
Source MODE, local PUSH/POP and unknown source effects do not replace initial
compiler-host metadata. Resolver arithmetic consumes the existing remaining
semantic-step allowance, including when no aggregate budget is supplied.

The native witness independently checks 85 arithmetic values and 30 comparisons
at five authored precision/FPU sites. The resolver queries the original complete
unit and repeated includes; it invokes no witness bodies. Browser consumers
compile only. A second independent whole native unit adds 140 unary/mixed
conversion cases, including actual comparison values and runtime proof barriers.
Named real constants, explicit constant casts, broader conversion/range admission
and default profile enrollment remain open, alongside full editor integration
and target parity.
