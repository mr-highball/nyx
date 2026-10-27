<# nyx
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
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [Parameter(Mandatory=$true)][string]$Project,
  [Parameter(Mandatory=$true)][string]$HealthURL,
  [string]$Toolchain = '.local/toolchain.json'
)

# Orchestration only: Pascal admits the exact MCP pair, builds both ordinary
# launchers and exercises inherited native controls through an opt-in observer.
# The private entry copy adds only that observer unit; selected host setup stays
# exact. Direct window-message/physical input remains separate qualification.
# The explicit existing health endpoint is read-only. No listener, installed
# service, agent enrollment or observing project is created or replaced here.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxHTTPRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
$nyxHTTPTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
$nyxHTTPToolFile = [IO.Path]::GetFullPath($Toolchain)
$nyxHTTPProject = [IO.Path]::GetFullPath($Project)
if (Test-Path -LiteralPath $nyxHTTPRoot) {
  throw 'HTTP output qualification requires fresh evidence; preserve prior runs.'
}
if (-not $IsWindows) {
  throw 'The emitted native window-message consumer currently qualifies Windows only.'
}
if (-not (Test-Path -LiteralPath $nyxHTTPProject -PathType Leaf)) {
  throw 'Supply the accepted MCP Workshop export.'
}
$nyxHTTPOldTemp = $env:TEMP
$nyxHTTPOldTmp = $env:TMP
try {
  $env:TEMP = $nyxHTTPTools.TEMP_ROOT
  $env:TMP = $nyxHTTPTools.TEMP_ROOT
  New-Item -ItemType Directory -Force $env:TEMP | Out-Null
  foreach ($nyxPart in @('builder','studio','web')) {
    New-Item -ItemType Directory (Join-Path $nyxHTTPRoot $nyxPart) | Out-Null
  }
  function Invoke-HTTPTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
    & $Executable @Arguments *> $Log
    if ($LASTEXITCODE -ne 0) {
      Get-Content -LiteralPath $Log -Tail 14
      throw "HTTP output qualification failed; retained $Log"
    }
    if (Select-String -LiteralPath $Log -Pattern '\b[1-9][0-9]* unfreed memory blocks\b' -Quiet) {
      throw "HTTP output qualification retained native allocations; inspect $Log"
    }
  }
  $nyxHTTPFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
    '-Fusrc','-Fustudio','-Futests','-Fisrc')
  $nyxHTTPLazarus = $nyxHTTPTools.LAZARUS
  $nyxHTTPLCL = @(
    "-Fu$nyxHTTPLazarus/lcl/units/i386-win32",
    "-Fu$nyxHTTPLazarus/lcl/units/i386-win32/win32",
    "-Fu$nyxHTTPLazarus/components/lazutils/lib/i386-win32",
    "-Fu$nyxHTTPLazarus/packager/units/i386-win32")
  foreach ($nyxEntry in @(
    @('nyx_file_runtime_build','builder'),
    @('nyx_file_runtime_studio_controls','studio'))) {
    $nyxTest = $nyxEntry[0]
    $nyxOut = Join-Path $nyxHTTPRoot $nyxEntry[1]
    Invoke-HTTPTool $nyxHTTPTools.LCL_FPC ($nyxHTTPFlags + $nyxHTTPLCL + @(
      "-FU$nyxOut", "-FE$nyxOut", "tests/$nyxTest.lpr")) (Join-Path $nyxHTTPRoot "$nyxTest-compile.log")
  }
  foreach ($nyxWidth in @(1280,390)) {
    Invoke-HTTPTool (Join-Path $nyxHTTPRoot 'studio/nyx_file_runtime_studio_controls.exe') @(
      (Join-Path $nyxHTTPRoot "owned-project-$nyxWidth"), "$nyxWidth") (Join-Path $nyxHTTPRoot "studio-$nyxWidth-run.log")
  }
  $nyxHTTPJobs = Join-Path $nyxHTTPRoot 'compiler-jobs'
  Invoke-HTTPTool (Join-Path $nyxHTTPRoot 'builder/nyx_file_runtime_build.exe') @(
    (Get-Location).Path, $nyxHTTPJobs, $nyxHTTPToolFile, '--http', $nyxHTTPProject) (Join-Path $nyxHTTPRoot 'compiler-jobs.log')
  foreach ($nyxPolicy in @(1,2)) {
    $nyxJob = (Get-Content -LiteralPath (Join-Path $nyxHTTPJobs "lcl-http-$nyxPolicy-job.txt") -Raw).Trim()
    $nyxJobRoot = Join-Path $nyxHTTPJobs "build/studio/jobs/$nyxJob"
    $nyxObservedRoot = Join-Path $nyxHTTPRoot "entry-http-$nyxPolicy"
    New-Item -ItemType Directory -Path $nyxObservedRoot | Out-Null
    $nyxEntry = Join-Path $nyxJobRoot 'nyx_native.lpr'
    $nyxOriginal = [IO.File]::ReadAllText($nyxEntry)
    $nyxNeedle = 'nyx.studio.runtimeclient'
    $nyxReplacement = 'nyx.studio.runtimeclient, nyx.test.http.output.observer'
    if ($nyxOriginal.IndexOf($nyxNeedle,[StringComparison]::Ordinal) -lt 0 -or
      $nyxOriginal.IndexOf($nyxNeedle,[StringComparison]::Ordinal) -ne
      $nyxOriginal.LastIndexOf($nyxNeedle,[StringComparison]::Ordinal)) {
      throw 'Expected one exact reporter unit in the emitted entry.'
    }
    # Fixed compiler-test instrumentation only. Pascal owns every application,
    # control, transport and assertion; this replaces no executable setup code.
    $nyxObserved = $nyxOriginal.Replace($nyxNeedle,$nyxReplacement)
    if ($nyxObserved.Replace($nyxReplacement,$nyxNeedle) -cne $nyxOriginal) {
      throw 'Observer preparation changed other generated entry bytes.'
    }
    $nyxObservedEntry = Join-Path $nyxObservedRoot 'nyx_native.lpr'
    [IO.File]::WriteAllText($nyxObservedEntry,$nyxObserved,[Text.UTF8Encoding]::new($false))
    [ordered]@{originalSHA256=(Get-FileHash -LiteralPath $nyxEntry -Algorithm SHA256).Hash;
      observedSHA256=(Get-FileHash -LiteralPath $nyxObservedEntry -Algorithm SHA256).Hash;
      change='one observer unit in uses; all generated setup preserved'} |
      ConvertTo-Json | Set-Content -LiteralPath (Join-Path $nyxObservedRoot 'entry-receipt.json') -Encoding utf8
    Invoke-HTTPTool $nyxHTTPTools.LCL_FPC (@('-Mdelphi','-gl','-gh',
      "-Fu$nyxJobRoot/units", "-Fu$nyxJobRoot", '-Futests','-Fusrc','-Fustudio') +
      $nyxHTTPLCL + @("-FU$nyxObservedRoot", "-FE$nyxObservedRoot", $nyxObservedEntry)) (
        Join-Path $nyxObservedRoot 'compile.log')
    foreach ($nyxWidth in @(1280,390)) {
      Invoke-HTTPTool (Join-Path $nyxObservedRoot 'nyx_native.exe') @(
        "$nyxWidth", $HealthURL) (Join-Path $nyxHTTPRoot "runtime-$nyxPolicy-$nyxWidth-run.log")
    }
  }
  Invoke-HTTPTool $nyxHTTPTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$(Join-Path $nyxHTTPRoot 'web')",
    'tests/nyx_file_runtime_studio_controls.lpr') (Join-Path $nyxHTTPRoot 'studio-browser-compile.log')
  Write-Host 'Native Outputs and observed generated HTTP entries pass cleanly at both widths; six ordinary builds pass. Browser controls compile only.'
} finally {
  $env:TEMP = $nyxHTTPOldTemp
  $env:TMP = $nyxHTTPOldTmp
}
