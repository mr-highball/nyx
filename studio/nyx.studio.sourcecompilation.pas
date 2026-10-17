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


unit nyx.studio.sourcecompilation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.project, nyx.studio.sourceprojection;

type
  { Producer lifetime, distinct from editor admission. Completed means a bound
    executed projection was delivered; Failed retains no usable candidate.
    Cancelled is terminal only after the producer's resources have retired. }
  TNyxSourceCompilationState = (scsPending, scsRunning, scsCompleted,
    scsCancelled, scsFailed);
  { A compiler owns copied source, process/transport and execution identities.
    It never borrows a session or accepted tree. Complete must report an actually
    bound projection, or an infrastructure failure; a compiled browser receipt
    alone cannot publish. The controller's port stages independent owners under
    its captured creators and queues delivery on the UI scheduler. Hosts may
    complete from a native worker or a browser callback, exactly once per job. }
  INyxSourceCompilationPort = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100001}']
    procedure Complete(const AProjection: INyxSourceProjection;
      const AFailure: TNyxText = '');
  end;

  { Independent operation lifetime. Cancel requests producer retirement; it must
    not publish, free or dereference an editor. Hosts bound their own work and
    retain enough state to retire it even after this caller releases its token. }
  INyxSourceCompilation = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100002}']
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
    { Terminal means the producer has retired its process/transport and will no
      longer borrow a compiler host. A queued independent UI courier may remain. }
    property State: TNyxSourceCompilationState read GetState;
  end;

  { Optional trusted host strategy for ordinary Pascal Apply. Installing one is
    explicit execution authority, not a document/output flag. Each Start receives
    exact immutable source and returns a nonnil operation token. It either starts
    one bounded job or raises before retaining its port. Browser hosts delegate
    compilation and bind the resulting worker to the same source/receipt/target;
    native hosts use the existing executor. Literal authoring without this host
    retains its existing compiler-independent path. }
  INyxSourceCompiler = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100003}']
    function Start(const ASource: TNyxText;
      const APort: INyxSourceCompilationPort): INyxSourceCompilation;
  end;

  { Optional whole-project capability. The host freezes the complete provider
    before retaining a port or starting work. Source, helper membership/order and
    bytes remain one immutable invocation through cancellation and completion.
    Existing single-unit implementations need not implement this extension. }
  INyxProjectSourceCompiler = interface(INyxSourceCompiler)
    ['{68FE918B-457C-45BE-B2E0-CA2300000010}']
    function StartProject(const AProject: INyxPascalProject;
      const APort: INyxSourceCompilationPort): INyxSourceCompilation;
  end;

{ Shared dispatch for sealed editor requests. Absent files preserve the original
  Start contract. Present files require the optional project capability; refusal
  occurs before invoking an older host, so helpers can never be silently omitted.
  Reconstructed inputs are independent of the editor and original provider. }
function StartNyxSourceCompilation(const ACompiler: INyxSourceCompiler;
  const ASource: TNyxText; const AFiles: TNyxPascalProjectFiles;
  const APort: INyxSourceCompilationPort): INyxSourceCompilation;

implementation

uses SysUtils, nyx.model;

function StartNyxSourceCompilation(const ACompiler: INyxSourceCompiler;
  const ASource: TNyxText; const AFiles: TNyxPascalProjectFiles;
  const APort: INyxSourceCompilationPort): INyxSourceCompilation;
var
  LCompiler: INyxProjectSourceCompiler;
begin

  if ACompiler = nil then
  begin
    raise ENyxModel.Create('Configure a source compiler before dispatching Pascal');
  end;

  if AFiles.IsEmpty then
  begin
    Exit(ACompiler.Start(ASource, APort));
  end;

  if not Supports(ACompiler, INyxProjectSourceCompiler, LCompiler) then
  begin
    raise ENyxModel.Create('This compiler host does not support captured Pascal project files');
  end;
  Result := LCompiler.StartProject(AFiles.Open(ASource), APort);
end;

end.
