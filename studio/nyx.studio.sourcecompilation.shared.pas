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
  nyx.text, nyx.studio.sourceprojection, nyx.studio.sourcecompilation,
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

  { Host-owned provider creation at dispatch, after draft acknowledgement. Every
    compiler copies this exact authority/project/revision; neither the provider
    nor its workers borrow the bridge or accepted editor. Machine configuration
    remains outside portable documents and ordinary compiler-free startup. }
  INyxSharedSourceCompilerFactory = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100010}']
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
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

implementation

end.
