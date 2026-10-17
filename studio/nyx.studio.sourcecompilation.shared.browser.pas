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

unit nyx.studio.sourcecompilation.shared.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.studio.sourcecompilation.browser, nyx.studio.sourcecompilation.shared;

{ Reuses the ordinary owned browser worker adapter, then delegates only its live
  executed result to the same shared provider. The specialized completion port
  carries server revision/context; it cannot be injected as an ordinary local
  source compiler. Construction is idle and borrows no editor/bridge/session. }
function NewNyxSharedBrowserSourceCompiler(
  const ABuilder: INyxBrowserSharedSourceBuilder): INyxSharedSourceCompiler;

{ Idle factory for the private HTTP provider. Each queue dispatch receives fresh
  copied bridge authority/context; enabling it never starts compilation itself. }
function NewNyxSharedBrowserSourceCompilerFactory: INyxSharedSourceCompilerFactory;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.pascal.project, nyx.studio.sourcecompilation,
  nyx.studio.sourceprojection, nyx.studio.sourcepublications,
  nyx.studio.workspaces, nyx.studio.sourcecompilation.service.browser;

type
  { One shared browser operation coordinates private compilation, a separately
    bound worker and revision-bearing publication. Frozen project inputs follow
    all three phases; cancellation revokes ports and retires each owned phase. }
  TSharedCompilation = class(TInterfacedObject, INyxSourceCompilation,
    INyxBrowserSourceBuilder, INyxBrowserProjectSourceBuilder, INyxBrowserSourceBuildPort,
    INyxSourceCompilationPort, INyxBrowserSourcePublicationPort)
  private
    FState: TNyxSourceCompilationState;
    FSource: TNyxText;
    FProject: INyxPascalProject;
    FBuilder: INyxBrowserSharedSourceBuilder;
    FPort: INyxSharedSourceCompilationPort;
    FBuildPort: INyxBrowserSourceBuildPort;
    FBuild: INyxSourceProjectionBuild;
    FProjection: INyxSourceProjection;
    FExecution: INyxSourceCompilation;
    FPublication: INyxSourceCompilation;
    FLease: INyxSourceCompilation;
    procedure Retire;
    procedure Finish(AOutcome: TNyxSourcePublicationOutcome;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
    function Compile(const ASource: TNyxText;
      const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
    function CompileProject(const AProject: INyxPascalProject;
      const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
    procedure Compiled(const ABuild: INyxSourceProjectionBuild;
      const AFailure: TNyxText = '');
    procedure Complete(const AProjection: INyxSourceProjection;
      const AFailure: TNyxText = ''); overload;
    procedure Complete(AOutcome: TNyxSourcePublicationOutcome;
      const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText = ''); overload;
  end;
  { Idle shared strategy owns a provider, never an editor. Whole-project dispatch
    requires the provider's explicit capability before retaining operation ports. }
  TSharedCompiler = class(TInterfacedObject, INyxSharedSourceCompiler,
    INyxSharedProjectSourceCompiler)
  private
    function StartInputs(const ASource: TNyxText; const AProject: INyxPascalProject;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  public
    Builder: INyxBrowserSharedSourceBuilder;
    function Start(const ASource: TNyxText;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function StartProject(const AProject: INyxPascalProject;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
  end;
  TSharedFactory = class(TInterfacedObject, INyxSharedSourceCompilerFactory,
    INyxSharedVisualSourceCompilerFactory)
    function CreateCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
    function CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
  end;

function TSharedFactory.CreateCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer): INyxSharedSourceCompiler;
begin
  Result := NewNyxSharedBrowserSourceCompiler(NewNyxSharedBrowserSourceService(
    ACapability, AIssuer, AWorkspace, ARevision));
end;

function TSharedFactory.CreateVisualCompiler(const ACapability, AIssuer: TNyxText;
  const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
  const AIntent: TNyxDataValue): INyxSharedSourceCompiler;
begin
  Result := NewNyxSharedBrowserSourceCompiler(NewNyxVisualSharedBrowserSourceService(
    ACapability, AIssuer, AWorkspace, ARevision, AIntent));
end;

function NewNyxSharedBrowserSourceCompilerFactory: INyxSharedSourceCompilerFactory;
begin
  Result := TSharedFactory.Create;
end;

procedure TSharedCompilation.Retire;
var
  LExecution: INyxSourceCompilation;
  LPublication: INyxSourceCompilation;
begin
  LExecution := FExecution;
  LPublication := FPublication;
  FExecution := nil;
  FPublication := nil;
  FBuildPort := nil;
  FPort := nil;
  FBuild := nil;
  FProjection := nil;
  FProject := nil;
  FBuilder := nil;

  if LExecution <> nil then
  begin
    LExecution.Cancel;
  end;

  if LPublication <> nil then
  begin
    LPublication.Cancel;
  end;
  FLease := nil;
end;

procedure TSharedCompilation.Cancel;
var
  LLease: INyxSourceCompilation;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := scsCancelled;
  Retire;
end;

function TSharedCompilation.GetState: TNyxSourceCompilationState;
begin
  Result := FState;
end;

procedure TSharedCompilation.Finish(AOutcome: TNyxSourcePublicationOutcome;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
var
  LLease: INyxSourceCompilation;
  LPort: INyxSharedSourceCompilationPort;
  LProjection: INyxSourceProjection;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FState := scsCompleted;

  if AOutcome <> npoCommitted then
  begin
    FState := scsFailed;
  end;
  LPort := FPort;
  LProjection := FProjection;
  Retire;
  LPort.Complete(AOutcome, LProjection, AReceipt, AMessage);
end;

function TSharedCompilation.Compile(const ASource: TNyxText;
  const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
var
  LReceiver: INyxBrowserSourceBuildPort;
begin

  if (FState <> scsPending) or (ASource <> FSource) or (APort = nil) then
  begin
    raise ENyxModel.Create('Shared browser execution requires its exact captured source and build port');
  end;
  FBuildPort := APort;
  LReceiver := Self;
  Result := FBuilder.Compile(ASource, LReceiver);
end;

function TSharedCompilation.CompileProject(const AProject: INyxPascalProject;
  const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
var
  LProject: INyxPascalProject;
  LBuilder: INyxBrowserProjectSourceBuilder;
  LReceiver: INyxBrowserSourceBuildPort;
begin
  LProject := CaptureNyxPascalProject(AProject);

  if (FState <> scsPending) or (LProject.Source <> FSource) or
    (FProject = nil) or (APort = nil) then
  begin
    raise ENyxModel.Create('Shared project execution requires its exact pending capture');
  end;

  if not Supports(FBuilder, INyxBrowserProjectSourceBuilder, LBuilder) then
  begin
    raise ENyxModel.Create('Shared browser provider cannot compile supporting files');
  end;
  FBuildPort := APort;
  LReceiver := Self;
  Result := LBuilder.CompileProject(LProject, LReceiver);
end;

procedure TSharedCompilation.Compiled(const ABuild: INyxSourceProjectionBuild;
  const AFailure: TNyxText);
var
  LLease: INyxSourceCompilation;
  LPort: INyxBrowserSourceBuildPort;
begin
  LLease := Self;

  if FState <> scsPending then
  begin
    Exit;
  end;
  FBuild := ABuild;
  FState := scsRunning;
  LPort := FBuildPort;
  FBuildPort := nil;
  LPort.Compiled(ABuild, AFailure);
end;

procedure TSharedCompilation.Complete(const AProjection: INyxSourceProjection;
  const AFailure: TNyxText);
var
  LLease: INyxSourceCompilation;
  LReceiver: INyxBrowserSourcePublicationPort;
  LOperation: INyxSourceCompilation;
begin
  LLease := Self;

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;
  FProjection := AProjection;
  try

    if (AFailure <> '') or (AProjection = nil) then
    begin
      Finish(npoRefused, Default(TNyxSourcePublicationReceipt), AFailure);
      Exit;
    end;

    if AProjection.State <> spsExecuted then
    begin
      Finish(npoRefused, Default(TNyxSourcePublicationReceipt), AProjection.Message);
      Exit;
    end;
    LReceiver := Self;
    LOperation := FBuilder.Publish(FBuild, AProjection, LReceiver);

    if LOperation = nil then
    begin
      raise ENyxModel.Create('Shared publication returned no operation lifetime');
    end;

    if FState in [scsPending, scsRunning] then
    begin
      FPublication := LOperation;
    end
    else
    begin
      LOperation.Cancel;
    end;
  except
    on LException: Exception do
    begin
      Finish(npoRefused, Default(TNyxSourcePublicationReceipt), LException.Message);
    end;
  end;
end;

procedure TSharedCompilation.Complete(AOutcome: TNyxSourcePublicationOutcome;
  const AReceipt: TNyxSourcePublicationReceipt; const AMessage: TNyxText);
begin

  if not (FState in [scsPending, scsRunning]) then
  begin
    Exit;
  end;

  if (AOutcome = npoCommitted) and
    ((FBuild = nil) or (FProjection = nil) or
    (AReceipt.Reference.Name <> FBuild.Reference.Name)) then
  begin
    Finish(npoUnconfirmed, Default(TNyxSourcePublicationReceipt),
      'Shared publication acknowledged a different compiled producer');
    Exit;
  end;
  Finish(AOutcome, AReceipt, AMessage);
end;

function TSharedCompiler.Start(const ASource: TNyxText;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
begin
  Result := StartInputs(ASource, nil, APort);
end;

function TSharedCompiler.StartProject(const AProject: INyxPascalProject;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
var
  LProject: INyxPascalProject;
begin
  LProject := CaptureNyxPascalProject(AProject);
  Result := StartInputs(LProject.Source, LProject, APort);
end;

function TSharedCompiler.StartInputs(const ASource: TNyxText;
  const AProject: INyxPascalProject;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
var
  LOwner: TSharedCompilation;
  LCompiler: INyxSourceCompiler;
  LReceiver: INyxSourceCompilationPort;
  LBuilder: INyxBrowserSourceBuilder;
  LExecution: INyxSourceCompilation;
  LProjectBuilder: INyxBrowserProjectSourceBuilder;
  LProjectCompiler: INyxProjectSourceCompiler;
begin
  ValidateNyxProjectionSource(ASource);

  if APort = nil then
  begin
    raise ENyxModel.Create('Shared compilation needs its specialized owning-editor completion port');
  end;

  if (AProject <> nil) and not Supports(Builder, INyxBrowserProjectSourceBuilder, LProjectBuilder) then
  begin
    raise ENyxModel.Create('Shared browser provider does not support captured project files');
  end;
  LOwner := TSharedCompilation.Create;
  Result := LOwner;
  LOwner.FSource := ASource;
  LOwner.FProject := AProject;
  LOwner.FBuilder := Builder;
  LOwner.FPort := APort;
  LOwner.FLease := Result;
  LBuilder := LOwner;
  LReceiver := LOwner;
  LCompiler := NewNyxBrowserSourceCompiler(LBuilder);
  try

    if AProject = nil then
    begin
      LExecution := LCompiler.Start(ASource, LReceiver);
    end
    else
    begin

      if not Supports(LCompiler, INyxProjectSourceCompiler, LProjectCompiler) then
      begin
        raise ENyxModel.Create('Owned browser executor lacks project capability');
      end;
      LExecution := LProjectCompiler.StartProject(AProject, LReceiver);
    end;

    if LOwner.FState in [scsPending, scsRunning] then
    begin
      LOwner.FExecution := LExecution;
    end
    else if LExecution <> nil then
    begin
      LExecution.Cancel;
    end;
  except
    LOwner.Cancel;
    raise;
  end;
end;

function NewNyxSharedBrowserSourceCompiler(
  const ABuilder: INyxBrowserSharedSourceBuilder): INyxSharedSourceCompiler;
var
  LCompiler: TSharedCompiler;
begin

  if ABuilder = nil then
  begin
    raise ENyxModel.Create('Shared browser compilation needs an explicit publication provider');
  end;
  LCompiler := TSharedCompiler.Create;
  LCompiler.Builder := ABuilder;
  Result := LCompiler;
end;

end.
