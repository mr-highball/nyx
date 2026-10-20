# Specialized managed controls

`nyx.controls` provides a specialized interface, default class and `NewNyx…`
factory for every built-in kind: the 75 public catalog controls plus the internal
slot override descriptor. `NewNyxBadge` returns `INyxBadge`; `TNyxBadge` is its
default implementation. Low-level portable descriptors remain in `nyx.model`.

```pascal
function BuildApplication: TNyxDocument;
var
  LPage: INyxPage;
  LStatus: INyxBadge;
  LDiscussion: INyxCommentThread;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Padding(24).Gap(16).Done;
    LStatus := NewNyxBadge('status').WithText('Ready');
    LDiscussion := NewNyxCommentThread('discussion');
    LDiscussion.ReplyMemo.Placeholder := 'Write something thoughtful';
    LDiscussion.ReplyMemo.Value := 'A first reply';
    LPage.Add(LStatus).Add(LDiscussion);
    Result.AddPage(LPage);
  except
    Result.Free;
    raise;
  end;
end;
```

Import `nyx.controls`, `nyx.model` and `nyx.types` for this example. Captions,
editable text values, Boolean checked values, integer values/ranges, image
sources and distinct component references have their appropriate property types.
Memo.Text is its caption; Memo.Value is its editable content. A badge has no
editable Value property. Generic Configure is an explicitly shared typed surface;
WithText retains the exact specialized interface.

Sliders also expose typed `NumberValue`/`WithNumber` for declared Number domains.
Their ordinary target controls consume a portable numeric scale; see
[typed sliders](sliders.md) for ranges, choices and exact value retention.

Compound interfaces expose their default named parts with specialized types:
CommentThread.ReplyMemo, StatCard.TrendBadge and NumberStepper.ValueSpin, for
example. These properties retain their parts; missing or incompatibly replaced
parts fail explicitly. General Part/OverridePart APIs serve custom compositions.
NumberStepper, SearchField and Pagination scalar properties address their named
input parts. Independent factory calls clone their own recipe payloads.

## Ownership and implementations

Interface adoption retains the actual implementation and its portable descriptor.
Parent/document links stay weak. Disposing an owner releases its ownership; a
retained descendant remains valid. Its parent is cleared when that parent is
finally disposed. Managed Extract preserves the implementation and transfers a
retained interface. Remove releases the owner's reference while external
interfaces still work. Clone copies portable meaning into independent default
implementations.

Configure, Binds, Contract and Extensions are managed interfaces. Each fresh
facade retains its control; controls do not cache these facades. A configuration
therefore remains usable after the original variable is released, without a
reference cycle. Compilers may retain fluent-result temporaries until routine
exit; routine scopes give an operation a precise final-release boundary.

Renderers borrow documents and own target controls separately. Dispose a renderer
before its borrowed document. A retained component preserves authored model
meaning; it does not retain a DOM element or an LCL widget.

Raw TNyxNode construction keeps its caller ownership token. Raw Add / Insert
transfer ownership; raw Extract returns it. RetainNyxControl adds a reference
without taking a raw caller's token. ReleaseOwnership explicitly transfers that
token to managed references. Never Free a borrowed Node descriptor; retained
descriptors reject direct Free. Owning low-level adapters use ReleaseNyxNode,
which clears the raw variable before relinquishing its ownership token. Retained
realized nodes survive renderer disposal; they preserve portable data rather than
target controls. A factory failure likewise releases only the adapter's ownership,
retaining any separately leased rejected descriptor and the previous accepted view.

Alternative objects may implement the same interfaces on a different Pascal
base class. They retain Node for their entire interface lifetime. Adapters,
generation and composition consume this portable bridge without casting to a
default control class. Owners retain the adopted implementation; managed
child/part lookup returns it. Public interfaces contain no DOM or LCL types.

## Reconstruction and maintenance

Ordinary compound factories include their registered default recipe. Studio emits
`ncoDescriptor` for an already expanded compound, then reconstructs its authored
parts/configuration exactly. This prevents duplicate children or new defaults
from changing saved meaning. Primitive factories create minimal descriptors.
Custom kinds use NewNyxControl with TNyxKindRef and the open INyxControl contract.

The private default registry is reused for independent blueprint cloning.
Authoring runs on the UI thread. Native finalization releases the registry;
pas2js owns it for the browser context lifetime. Scheduler worker behavior has
its own acceptance task and is not implied by this API.

The checked-in Pascal includes are maintained by
[nyx_control_contracts.lpr](../tools/nyx_control_contracts.lpr). It reads typed
descriptor declarations and catalog recipes, emits all classes/interfaces,
factories, typed part accessors and managed facades, and checks stable GUID
uniqueness. Compile it with FPC from the current checkout. With no arguments,
the executable now compares the five generated includes with the maintained
source, byte for byte, without writing. `--check` makes that choice explicit.
It reports each `MATCH` or `DIFF`; status 0 means a match, 2 means missing or
different source, and 1 means invalid arguments or a generation/I/O failure.

Generate a review candidate with an explicit new destination:

```text
nyx_control_contracts --source-root D:\Docs\GitHub\nyx --output-dir E:\Blekohlab\nyx\control-contract-candidate
```

The destination's parent must exist and the destination itself must be absent.
The tool prepares every output before creating the directory, then writes the
five includes beneath its `src` directory. Existing files/directories refuse;
a failed write leaves its incomplete candidate available for inspection.
It never merges with an older candidate or replaces maintained includes.
Review the candidate diff, then apply reviewed changes only after
all source readers have joined. Run the affected compiler/consumer checks after
application. Retain the candidate and logs when generation or checking fails.

`--source-root` selects descriptor/license inputs and the maintained comparison
files; it defaults to the working directory. Output paths are relative to the
invocation's working directory. This option does not rebuild the executable's
compiled kind/family/catalog recipes: those must come from the same reviewed
checkout. `--help` requires no source inputs. This remains a development
operation, not a runtime dependency or automatic documentation inventory.

Before checking or writing, the tool captures typed declaration IR from the exact
serialized types, facade and factory includes using Nyx's existing Pascal syntax
owner. Explicit include grammars retain complete scopes, parameter passing/types,
defaults, result spelling and property declarations. Lexical/syntax diagnostics,
incomplete/unsupported scopes and mismatched kind/facade coverage refuse the
whole generation. It verifies the 81 specialized interfaces/classes/factories
and four facades, including their actual specialized results and fluent return
types. The same facade map drives emission and admission; a second signature
parser or manually maintained help inventory is not added.

Forwarding argument extraction now uses that existing syntax owner too. Ordered
parameter declarations supply the call arguments; punctuation inside defaults
or nested type syntax cannot create extra arguments. Exact name spans retain
escaped parameter spelling. Recovered, conditional or incomplete signatures
refuse before candidate publication. A cached checked method now also supplies
the logical return shape, escaped method name and actual implementation-header
terminator. Only an entire unqualified direct builder result maps to the fluent
interface. Defaults, comments, longer identifiers, procedural parameter names and
qualified designators retain exact text. A builder inside a parameter or compound
result requires an explicit conversion bridge and currently refuses before disk
publication. No container conversion, qualified binding or ABI is guessed.
The typed rewriting passes the current generator qualification recorded in
WORK.md. Facade discovery still follows
the maintained descriptor declaration convention; typed forwarding does not
establish complete extraction of arbitrary hand-authored Pascal source.

This is declared source structure. Imported type spelling is still unbound, and
compiling the resulting complete application remains necessary. The IR does not
yet emit documentation, capture the complete hand-authored core closure or
register built-in help. Its existing source/syntax limits remain unchanged.

The maintained native
[CLI consumer](../tests/nyx_control_contract_generator_tests.lpr) passes 103 checks
through 21 joined generator invocations, with clean parent and child heaps.
Both fresh candidates match all five maintained includes for 81 built-in kinds
and four authoring facades. The consumer also checks copied-source drift,
missing outputs, argument/destination refusal and exact original input bytes.
An actual copied descriptor with an explicit empty parameter type refuses through
the shared syntax owner in both check and candidate modes, before output exists.
Another copied actual descriptor verifies string/constant-expression punctuation,
grouped const/var/out parameters and escaped names in real candidate call arguments.
The CLI fixture extends that same copied descriptor with source-class text
inside defaults, method/argument/type identifiers, namespace qualifiers, callback
parameter names and comments, and separate unsupported self-conversion refusals.
Its arguments are the compiled generator, repository root and a new runtime
directory. Current source/compiler receipts and broader qualification are in
[WORK.md](../WORK.md); no generated repository include changes in this batch.

Maintained FPC/pas2js fixtures exercise lifetime, complete catalog construction,
alternative implementations, typing and source recovery. Generated-target checks
compile regenerated legacy companions and execute their handwritten helpers.
Actual browser/LCL controls consume specialized memo/badge/compound properties.
Multiple events, schedulers and Studio's Properties / Events workflow retain
their separate required acceptance owners.
