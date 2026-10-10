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
unit nyx.studio.recovery.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.directories, nyx.studio.recovery,
  nyx.studio.sourceprojection, nyx.studio.buildexecutor;

{ Explicit native startup verification of complete Pascal constructors. Copies
  host directories/profile/limits; output selection and portable files carry no
  compiler configuration. Each invocation compiles/executes with the existing
  bounded process owner and joins it before returning independent evidence.
  No scheduler, UI, HTTP listener or enrollment is owned.

  This executes native construction, including unit initializers. Browser-only
  units and target-dependent design differences refuse; a browser worker's
  execution cannot be inferred from a native pass. Missing FPC refuses executed
  recovery without rewriting the checkpoint. A nil verifier still supports
  ordinary compiler-independent literal recovery. Caller cancellation remains
  borrowed through a managed token and is checked even for cached source pairs. }
function NewNyxNativeRuntimeSourceVerifier(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits;
  const ACancellation: INyxBuildCancellation = nil;
  AChecks: TNyxSourceProjectionChecks = spcDefault): INyxRuntimeSourceVerifier;

implementation

uses nyx.model, nyx.source, nyx.studio.outputs, nyx.studio.builds;

type
  TNativeRecoveryVerifier = class(TInterfacedObject, INyxRuntimeSourceVerifier)
  public
    Directories: TNyxStudioDirectories;
    Profile: TNyxText;
    Limits: TNyxCompilerLimits;
    Cancellation: INyxBuildCancellation;
    Checks: TNyxSourceProjectionChecks;
    procedure RequireActive;
    function Verify(const ASource: TNyxText): INyxSourceProjection;
  end;

procedure TNativeRecoveryVerifier.RequireActive;
begin

  if (Cancellation <> nil) and Cancellation.Cancelled then
  begin
    raise ENyxModel.Create('Runtime source recovery was cancelled; checkpoint retained');
  end;
end;

function TNativeRecoveryVerifier.Verify(const ASource: TNyxText): INyxSourceProjection;
var
  LExecutor: TNyxBuildExecutor;
  LBuild: INyxSourceProjectionBuild;
begin
  RequireActive;
  LExecutor := TNyxBuildExecutor.Create(Directories, Profile);
  try
    LExecutor.ConfigureLimits(Limits);
    LBuild := LExecutor.ProjectSource(ASource,
      NyxPascalUnit(NyxCompanionUnitName(ASource)), btNativeLCL, Checks, Cancellation);
    Result := LBuild.Projection;
  finally
    LExecutor.Free;
  end;
  RequireActive;
end;

function NewNyxNativeRuntimeSourceVerifier(const ADirectories: TNyxStudioDirectories;
  const AProfile: TNyxText; const ALimits: TNyxCompilerLimits;
  const ACancellation: INyxBuildCancellation;
  AChecks: TNyxSourceProjectionChecks): INyxRuntimeSourceVerifier;
var
  LProfile: TNyxOutputConfiguration;
  LVerifier: TNativeRecoveryVerifier;
begin
  ADirectories.Validate;
  ALimits.Validate;
  LProfile := TNyxOutputConfiguration.Decode(AProfile);
  LProfile.Free;
  LVerifier := TNativeRecoveryVerifier.Create;
  Result := LVerifier;
  LVerifier.Directories := ADirectories;
  LVerifier.Profile := AProfile;
  LVerifier.Limits := ALimits;
  LVerifier.Cancellation := ACancellation;
  LVerifier.Checks := AChecks;
end;

end.
