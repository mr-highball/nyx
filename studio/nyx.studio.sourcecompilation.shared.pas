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

unit nyx.studio.sourcecompilation.shared;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.pascal.project, nyx.pascal.drafts, nyx.studio.sourceprojection, nyx.studio.sourcecompilation,
  nyx.studio.sourcepublications, nyx.studio.workspaces,
  nyx.studio.session, nyx.source.preparation;

type
  { Specialized shared success contract. A committed result includes the exact
    owning-server revision; a local Apply-only port cannot satisfy this type.
    Caller coordinates paired admission and observing synchronization, retaining
    unfinished work on refusal/unconfirmed delivery. The compiler owns this port
    only until terminal completion/cancellation; no accepted tree is borrowed. }
  INyxSharedSourceCompilationPort = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100008}']
    procedure Complete(AOutcome: TNyxSourcePublicationOutcome;
      const AProjection: INyxSourceProjection;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText = '');
  end;
  { Compilation/execution/publication form one owned operation. Cancel detaches
    local delivery and retires workers; a remote admission already in progress
    can still complete, requiring ordinary observing reconciliation. }
  INyxSharedSourceCompiler = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100009}']
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  end;
  { Optional complete-input shared compiler. The producer captures immutable
    inputs beside its existing authority/revision before work begins. This
    capability does not let a main-source Apply replace accepted helper files;
    the server independently compares the complete captured baseline. }
  INyxSharedProjectSourceCompiler = interface(INyxSharedSourceCompiler)
    ['{68FE918B-457C-45BE-B2E0-CA2300000012}']
    function StartProject(const AProject: INyxPascalProject;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  end;

  { Host-owned provider creation at dispatch, after draft acknowledgement. Every
    compiler copies this exact authority/project/revision; neither the provider
    nor its workers borrow the bridge or accepted editor. Machine configuration
    remains outside portable documents and ordinary compiler-free startup. }
  INyxSharedSourceCompilerFactory = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100010}']
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
  end;

  { Optional visual continuation. Intent contains only the closed semantic edit;
    it cannot carry a live checkpoint, source authority or an accepted tree. The
    owning server independently prepares the same proposal before compilation.
    Older Apply-only factories remain usable through the base interface. }
  INyxSharedVisualSourceCompilerFactory = interface(INyxSharedSourceCompilerFactory)
    ['{6C080403-81B5-4E91-B222-101026100013}']
    function CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
  end;

  { Optional selected-file strategy. The owning backend reconstructs the one
    acknowledged file edit; proposed inventories cannot grant publication. }
  INyxSharedFileSourceCompilerFactory = interface(INyxSharedSourceCompilerFactory)
    ['{68FE918B-457C-45BE-B2E0-CA2300000015}']
    function CreateFileCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AFile: TNyxPascalSourceRef): INyxSharedSourceCompiler;
  end;

  { UI-only revocable dispatch courier. Resume requests another queue dispatch;
    a detached controller ignores it. Hosts must not borrow controller pointers. }
  INyxSharedSourceDispatch = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100011}']
    procedure Resume;
  end;

  { One editor/project coordinator, implemented behind a revocable bridge port.
    Start reserves the acknowledged frame. Admit uses ordinary sealed completion
    and then waits for exact observing acknowledgement before Ready releases the
    next queued edit. New drafts remain local throughout that wait. Abandon with
    uncertain delivery freezes synchronization for explicit reconciliation.
    All calls are UI-only; no worker may call these methods or borrow the session. }
  INyxSharedSourceHost = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100012}']
    procedure Attach(const ADispatch: INyxSharedSourceDispatch);
    { Bridge retirement revokes the borrowed owner before any session is freed. }
    procedure Detach;
    function Ready: Boolean;
    function Waiting: Boolean;
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function Admit(const ARequest: TNyxStudioSourceRequest;
      const APrepared: INyxPreparedSource;
      const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
    procedure Abandon(AOutcome: TNyxSourcePublicationOutcome; const AMessage: TNyxText);
  end;

  { A visual command keeps an independent unfinished draft. Its sealed design
    request must therefore complete through visual history, never through Apply.
    StartDesign reserves the acknowledged pair; AdmitDesign verifies the local
    request before waiting for exact observing acknowledgement as usual. }
  INyxSharedDesignSourceHost = interface(INyxSharedSourceHost)
    ['{6C080403-81B5-4E91-B222-101026100014}']
    function StartDesign(const ARequest: TNyxStudioDesignRequest;
      const AProposal: INyxPreparedDesign;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function AdmitDesign(const ARequest: TNyxStudioDesignRequest;
      const APrepared: INyxPreparedDesign;
      const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
  end;

  { Revocable UI-only selected-file dispatch. It reserves the same acknowledged
    pair as Apply; existing Admit verifies and consumes the sealed file request. }
  INyxSharedFileSourceHost = interface(INyxSharedSourceHost)
    ['{68FE918B-457C-45BE-B2E0-CA2300000016}']
    function StartFile(const ARequest: TNyxStudioSourceRequest;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  end;

  { Optional two-phase visual lifetime. Reserve the acknowledged frame before
    off-thread proposal preparation, deferring newer independent buffer sync.
    Release affects only an unstarted preparation and grants no compilation or
    publication authority. All calls remain on the owning UI thread. }
  INyxSharedDesignPreparationHost = interface(INyxSharedDesignSourceHost)
    ['{68FE918B-457C-45BE-B2E0-CA2300000017}']
    procedure ReserveDesign(const ARequest: TNyxStudioDesignRequest);
    procedure ReleaseDesignPreparation;
  end;

{ Older hosts refuse before any compiler effect; never turn a helper Apply into
  main Apply or fall back to local publication behind a shared editor. }
function StartNyxSharedFileCompilation(const AHost: INyxSharedSourceHost;
  const ARequest: TNyxStudioSourceRequest;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;

implementation

uses SysUtils;

function StartNyxSharedFileCompilation(const AHost: INyxSharedSourceHost;
  const ARequest: TNyxStudioSourceRequest;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
var
  LHost: INyxSharedFileSourceHost;
begin

  if not Supports(AHost, INyxSharedFileSourceHost, LHost) then
  begin
    raise Exception.Create('Shared source host does not support selected-file compilation');
  end;
  Result := LHost.StartFile(ARequest, APort);
end;

end.
