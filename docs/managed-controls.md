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
uniqueness. Compile it with FPC and run it from the repository to regenerate the
includes. It is a development operation, not a runtime dependency.

Maintained FPC/pas2js fixtures exercise lifetime, complete catalog construction,
alternative implementations, typing and source recovery. Generated-target checks
compile regenerated legacy companions and execute their handwritten helpers.
Actual browser/LCL controls consume specialized memo/badge/compound properties.
Multiple events, schedulers and Studio's Properties / Events workflow retain
their separate required acceptance owners.
