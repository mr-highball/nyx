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

unit nyx.studio.sourcecompilation.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.sourcecompilation, nyx.studio.sourceprojection;

type
  { Compilation provider boundary. A same-origin backend owns its HTTP operation
    and returns an exact compiled browser receipt, never a document or source
    admission flag. Complete may be synchronous; worker execution is separate. }
  INyxBrowserSourceBuildPort = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100004}']
    procedure Compiled(const ABuild: INyxSourceProjectionBuild;
      const AFailure: TNyxText = '');
  end;
  INyxBrowserSourceBuilder = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100005}']
    function Compile(const ASource: TNyxText;
      const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
  end;

{ Reusable browser execution strategy. The provider delegates compilation; this
  adapter owns the resulting worker, deadline and bound receive channel. Only an
  executed, validated projection reaches the ordinary source-command port.
  Cancellation/terminal delivery removes handlers and retires that exact worker.
  It borrows no Studio, accepted document, output profile or editor selection. }
function NewNyxBrowserSourceCompiler(
  const ABuilder: INyxBrowserSourceBuilder): INyxSourceCompiler;

implementation

uses SysUtils, JS, Web, nyx.model, nyx.studio.builds;

type
  TBrowserCompilation = class(TInterfacedObject, INyxSourceCompilation,
    INyxBrowserSourceBuildPort)
  private
    FState: TNyxSourceCompilationState;
    FSource: TNyxText;
    FPort: INyxSourceCompilationPort;
    FBuild: INyxSourceCompilation;
    FReference: TNyxSourceProjectionRef;
    FWorker: TJSWorker;
    FTimeout: NativeInt;
    FMessage: TJSEventHandler;
    FError: TJSEventHandler;
    FLease: INyxSourceCompilation;
    procedure Retire;
    procedure Fail(const AReason: TNyxText);
    function Receive(AEvent: TJSEvent): Boolean;
    function WorkerError(AEvent: TJSEvent): Boolean;
    procedure Deadline;
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
    procedure Compiled(const ABuild: INyxSourceProjectionBuild;
      const AFailure: TNyxText = '');
  end;
  TBrowserCompiler = class(TInterfacedObject, INyxSourceCompiler)
  public
    Builder: INyxBrowserSourceBuilder;
    function Start(const ASource: TNyxText;
      const APort: INyxSourceCompilationPort): INyxSourceCompilation;
  end;

procedure TBrowserCompilation.Retire;
begin

  if FTimeout <> 0 then
  begin
    window.clearTimeout(FTimeout);
    FTimeout := 0;
  end;

  if FWorker <> nil then
  begin
    FWorker.removeEventListener('message', FMessage);
    FWorker.removeEventListener('error', FError);
    FWorker.terminate;
    FWorker := nil;
  end;
end;

function TBrowserCompilation.GetState: TNyxSourceCompilationState;
begin
  Result := FState;
end;

procedure TBrowserCompilation.Cancel;
var
  { Keep Self alive while releasing its retained transport and self lease. }
  LLease: INyxSourceCompilation;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := scsCancelled;
  Retire;

  if FBuild <> nil then
  begin
    FBuild.Cancel;
    FBuild := nil;
  end;
  FPort := nil;
  FLease := nil;
end;

procedure TBrowserCompilation.Fail(const AReason: TNyxText);
var
  { Completion can release the last caller token; retain Self through delivery. }
  LLease: INyxSourceCompilation;
  LPort: INyxSourceCompilationPort;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := scsFailed;
  Retire;
  LPort := FPort;
  FPort := nil;
  FBuild := nil;
  FLease := nil;
  LPort.Complete(nil, AReason);
end;

procedure TBrowserCompilation.Compiled(const ABuild: INyxSourceProjectionBuild;
  const AFailure: TNyxText);
begin

  if FState <> scsPending then
  begin
    Exit;
  end;
  try

    if AFailure <> '' then
    begin
      Fail(AFailure);
      Exit;
    end;

    if (ABuild = nil) or (ABuild.Projection.Source <> FSource) or
      (ABuild.Projection.Target <> btBrowser) then
    begin
      raise ENyxModel.Create('Browser compiler receipt differs from the dispatched source');
    end;

    if ABuild.Projection.State <> spsCompiled then
    begin

      if ABuild.Projection.State = spsExecuted then
      begin
        raise ENyxModel.Create('Browser compiler provider must return its compiled worker');
      end;
      FState := scsFailed;
      Retire;
      FPort.Complete(ABuild.Projection);
      FPort := nil;
      FLease := nil;
      Exit;
    end;
    FReference := ABuild.Reference;
    FState := scsRunning;
    FMessage := Receive;
    FError := WorkerError;
    FWorker := TJSWorker.new(ABuild.Artifact);
    FWorker.addEventListener('message', FMessage);
    FWorker.addEventListener('error', FError);
    FTimeout := window.setTimeout(@Deadline, 30000);
  except
    on LException: Exception do
    begin
      Fail(LException.Message);
    end;
  end;
end;

function TBrowserCompilation.Receive(AEvent: TJSEvent): Boolean;
var
  { Retiring event handlers/self lease must not destroy the receiving callback. }
  LLease: INyxSourceCompilation;
  LPort: INyxSourceCompilationPort;
  LProjection: INyxSourceProjection;
begin
  Result := False;
  LLease := Self;

  if FState <> scsRunning then
  begin
    Exit;
  end;
  try

    if not isString(TJSMessageEvent(AEvent).data) then
    begin
      raise ENyxModel.Create('Browser constructor requires a bounded text result');
    end;
    LProjection := ReceiveNyxSourceProjection(FSource, FReference, btBrowser,
      TNyxText(TJSMessageEvent(AEvent).data));
    Retire;
    FState := scsCompleted;

    if LProjection.State <> spsExecuted then
    begin
      FState := scsFailed;
    end;
    LPort := FPort;
    FPort := nil;
    FBuild := nil;
    FLease := nil;
    LPort.Complete(LProjection);
  except
    on LException: Exception do
    begin
      Fail(LException.Message);
    end;
  end;
end;

function TBrowserCompilation.WorkerError(AEvent: TJSEvent): Boolean;
begin
  Result := False;
  Fail('The compiled Pascal constructor worker failed to load or execute');
end;

procedure TBrowserCompilation.Deadline;
begin
  Fail('The compiled Pascal constructor exceeded its browser execution deadline');
end;

function TBrowserCompiler.Start(const ASource: TNyxText;
  const APort: INyxSourceCompilationPort): INyxSourceCompilation;
var
  LOwner: TBrowserCompilation;
  LPort: INyxBrowserSourceBuildPort;
begin

  if APort = nil then
  begin
    raise ENyxModel.Create('Browser compilation needs an independent completion port');
  end;
  ValidateNyxProjectionSource(ASource);
  LOwner := TBrowserCompilation.Create;
  Result := LOwner;
  LPort := LOwner;
  LOwner.FSource := ASource;
  LOwner.FPort := APort;
  LOwner.FLease := Result;
  try
    LOwner.FBuild := Builder.Compile(ASource, LPort);

    if LOwner.FBuild = nil then
    begin
      raise ENyxModel.Create('Browser compilation returned no operation lifetime');
    end;

    if not (LOwner.FState in [scsPending, scsRunning]) then
    begin
      LOwner.FBuild.Cancel;
      LOwner.FBuild := nil;
    end;
  except
    LOwner.Cancel;
    raise;
  end;
end;

function NewNyxBrowserSourceCompiler(
  const ABuilder: INyxBrowserSourceBuilder): INyxSourceCompiler;
var
  LOwner: TBrowserCompiler;
begin

  if ABuilder = nil then
  begin
    raise ENyxModel.Create('Browser source execution needs an explicit compiler provider');
  end;
  LOwner := TBrowserCompiler.Create;
  Result := LOwner;
  LOwner.Builder := ABuilder;
end;

end.
