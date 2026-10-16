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
unit nyx.studio.previewruntime;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.root.types, nyx.state,
  nyx.scheduler, nyx.application.state, nyx.application.context,
  nyx.collections.registry, nyx.collections.bindings;

type
  { One Studio Interact mount owns independent application stores and a scoped
    capability for embedded Nyx sandboxes. Saved defaults/source never change.
    It consumes the public application-state/context contracts, not another UI
    runtime. Root Pascal callback implementations still require compiled hosts.

    The renderer borrows State/Bindings until unmount. Close revokes context and
    pending generations before widget teardown; Free follows renderer teardown.
    Private source/prototypes outlive all raw state validators. All operations
    and final disposal belong to the renderer's application UI thread. }
  TNyxStudioPreviewRuntime = class
  private
    FDocument: TNyxDocument;
    FRuntime: TNyxApplicationState;
    FRoot: TNyxRootRef;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FLease: INyxApplicationContextLease;
    FClosed: Boolean;
    function ResolveState: TNyxState;
    function ResolveCollections: INyxCollections;
    function ResolveGeneration: INyxCancellationScope;
    function GetContext: INyxApplicationContext;
    function GetBindings: INyxCollectionBindings;
  public
    { Capture an independent source/default frame. A focused reusable gets the
      same typed stores and hidden-page validation as an ordinary page. No file,
      HTTP or arbitrary native compiler authority is inferred by this host. }
    constructor Create(ADocument: TNyxDocument; const ARoot: TNyxRootRef;
      APlatform: TNyxPlatform; const AScheduler: INyxScheduler);
    destructor Destroy; override;
    procedure Close;
    property State: TNyxState read ResolveState;
    property Context: INyxApplicationContext read GetContext;
    property Bindings: INyxCollectionBindings read GetBindings;
  end;

implementation

constructor TNyxStudioPreviewRuntime.Create(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; APlatform: TNyxPlatform; const AScheduler: INyxScheduler);
begin
  inherited Create;

  if (ADocument = nil) or (AScheduler = nil) or (APlatform = npfAny) then
  begin
    raise ENyxModel.Create('Interact runtime requires a source, concrete target and UI scheduler');
  end;
  FScheduler := AScheduler;
  FScheduler.RequireUI;
  FRoot := ARoot;
  FDocument := ADocument.Clone;
  FRuntime := TNyxApplicationState.CreateForRoot(FDocument, FRoot, APlatform);
  FGeneration := NewNyxCancellationScope;
  FLease := NewNyxApplicationContext(FScheduler,
    {$ifdef PAS2JS}@{$endif}ResolveState, {$ifdef PAS2JS}@{$endif}ResolveCollections,
    {$ifdef PAS2JS}@{$endif}ResolveGeneration);
end;

destructor TNyxStudioPreviewRuntime.Destroy;
begin
  Close;
  FLease := nil;
  FRuntime.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TNyxStudioPreviewRuntime.Close;
begin

  if FClosed then
  begin
    Exit;
  end;
  FClosed := True;

  if FLease <> nil then
  begin
    FLease.Close;
  end;

  if FGeneration <> nil then
  begin
    FGeneration.Cancel;
  end;
end;

function TNyxStudioPreviewRuntime.ResolveState: TNyxState;
begin
  FScheduler.RequireUI;

  if FClosed then
  begin
    raise ENyxApplicationContext.Create('Studio Interact runtime is retired');
  end;
  Result := FRuntime.State;
end;

function TNyxStudioPreviewRuntime.ResolveCollections: INyxCollections;
begin
  ResolveState;
  Result := FRuntime.Collections;
end;

function TNyxStudioPreviewRuntime.ResolveGeneration: INyxCancellationScope;
begin
  ResolveState;
  Result := FGeneration;
end;

function TNyxStudioPreviewRuntime.GetContext: INyxApplicationContext;
begin
  Result := FLease.Context;
end;

function TNyxStudioPreviewRuntime.GetBindings: INyxCollectionBindings;
begin
  ResolveState;
  Result := FRuntime.RootCollections(FRoot);
end;

end.
