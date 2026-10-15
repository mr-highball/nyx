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
  [string]$Toolchain = '.local/toolchain.json',
  [string]$StaticJobsDirectory = '',
  [string]$BuildsBaseURL = ''
)

# Only orchestrates installed compilers and Pascal consumers. The optional HTTP
# path stages fresh static files on a separately preflighted existing service.
# No server/listener/profile enrollment or protected project is started/changed.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxRuntimeRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxRuntimeRoot) {
  throw 'Runtime qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the preflighted static jobs directory and builds URL together.'
}
$nyxRuntimeTools = Get-Content -LiteralPath $Toolchain -Raw | ConvertFrom-Json
$nyxRuntimeToolFile = [IO.Path]::GetFullPath($Toolchain)
$nyxRuntimeSource = (Get-Location).Path
$nyxRuntimeBuilder = Join-Path $nyxRuntimeRoot 'builder'
$nyxRuntimeCore = Join-Path $nyxRuntimeRoot 'core'
$nyxRuntimeNative = Join-Path $nyxRuntimeRoot 'native'
$nyxRuntimeStudio = Join-Path $nyxRuntimeRoot 'studio-native'
$nyxRuntimeWeb = Join-Path $nyxRuntimeRoot 'web'
$nyxRuntimeDriver = Join-Path $nyxRuntimeRoot 'driver'
$nyxRuntimeJobs = Join-Path $nyxRuntimeRoot 'compiler-jobs'
New-Item -ItemType Directory $nyxRuntimeBuilder, $nyxRuntimeCore,
  $nyxRuntimeNative, $nyxRuntimeStudio, $nyxRuntimeWeb, $nyxRuntimeDriver | Out-Null

function Invoke-RuntimeTool([string]$Executable, [string[]]$Arguments, [string]$Log, [bool]$Receipt = $false) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "Qualification tool failed; retained $Log"
  }
  if ($Receipt) {
    Get-Content -LiteralPath $Log -TotalCount 2
  }
}

$nyxRuntimeFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
Invoke-RuntimeTool $nyxRuntimeTools.FPC ($nyxRuntimeFlags + @(
  "-FU$nyxRuntimeCore", "-FE$nyxRuntimeCore", 'tests/nyx_core_tests.lpr')) (Join-Path $nyxRuntimeRoot 'core-compile.log')
Invoke-RuntimeTool (Join-Path $nyxRuntimeCore 'nyx_core_tests.exe') @() (Join-Path $nyxRuntimeRoot 'core-run.log') $true
Invoke-RuntimeTool $nyxRuntimeTools.FPC ($nyxRuntimeFlags + @(
  "-FU$nyxRuntimeBuilder", "-FE$nyxRuntimeBuilder", 'tests/nyx_file_runtime_build.lpr')) (Join-Path $nyxRuntimeRoot 'builder-compile.log')
Invoke-RuntimeTool (Join-Path $nyxRuntimeBuilder 'nyx_file_runtime_build.exe') @(
  $nyxRuntimeSource, $nyxRuntimeJobs, $nyxRuntimeToolFile) (Join-Path $nyxRuntimeRoot 'compiler-jobs.log') $true

$nyxRuntimeLazarus = $nyxRuntimeTools.LAZARUS
$nyxRuntimeLCL = @(
  "-Fu$nyxRuntimeLazarus/lcl/units/i386-win32",
  "-Fu$nyxRuntimeLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxRuntimeLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxRuntimeLazarus/packager/units/i386-win32")
Invoke-RuntimeTool $nyxRuntimeTools.LCL_FPC ($nyxRuntimeFlags + $nyxRuntimeLCL + @(
  "-Fu$nyxRuntimeJobs", "-FU$nyxRuntimeNative", "-FE$nyxRuntimeNative",
  'tests/nyx_file_runtime_controls.lpr')) (Join-Path $nyxRuntimeRoot 'native-controls-compile.log')
Invoke-RuntimeTool (Join-Path $nyxRuntimeNative 'nyx_file_runtime_controls.exe') @(
  (Join-Path $nyxRuntimeRoot 'written')) (Join-Path $nyxRuntimeRoot 'native-controls-run.log') $true
Invoke-RuntimeTool $nyxRuntimeTools.LCL_FPC ($nyxRuntimeFlags + $nyxRuntimeLCL + @(
  "-FU$nyxRuntimeStudio", "-FE$nyxRuntimeStudio", 'tests/nyx_file_runtime_studio_controls.lpr')) (Join-Path $nyxRuntimeRoot 'studio-native-compile.log')
Invoke-RuntimeTool (Join-Path $nyxRuntimeStudio 'nyx_file_runtime_studio_controls.exe') @(
  (Join-Path $nyxRuntimeRoot 'owned-studio-projects')) (Join-Path $nyxRuntimeRoot 'studio-native-run.log') $true

Invoke-RuntimeTool $nyxRuntimeTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxRuntimeWeb",
  'tests/nyx_file_runtime_studio_controls.lpr') (Join-Path $nyxRuntimeRoot 'studio-browser-compile.log')
Invoke-RuntimeTool $nyxRuntimeTools.FPC ($nyxRuntimeFlags + @(
  "-FU$nyxRuntimeDriver", "-FE$nyxRuntimeDriver", 'tests/nyx_file_save_browser_consumer.lpr')) (Join-Path $nyxRuntimeRoot 'save-driver-compile.log')
Invoke-RuntimeTool $nyxRuntimeTools.FPC ($nyxRuntimeFlags + @(
  "-FU$nyxRuntimeDriver", "-FE$nyxRuntimeDriver", 'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxRuntimeRoot 'ready-driver-compile.log')

if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native runtime/input and compiler checks pass; HTTP browser execution remains unqualified.'
  return
}
$nyxRuntimeJob = (Get-Content -LiteralPath (Join-Path $nyxRuntimeJobs 'browser-job.txt') -Raw).Trim()
$nyxRuntimeCompiled = Join-Path (Join-Path $nyxRuntimeJobs 'build/studio/jobs') $nyxRuntimeJob
$nyxRuntimeChild = 'nyx-files-runtime-' + [guid]::NewGuid().ToString('N')
$nyxRuntimeStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxRuntimeChild
New-Item -ItemType Directory $nyxRuntimeStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxRuntimeCompiled 'nyx_preview.js'),
  (Join-Path $nyxRuntimeCompiled 'index.html'), $nyxRuntimeTools.PAS2JS_RUNTIME,
  (Join-Path $nyxRuntimeWeb 'nyx_file_runtime_studio_controls.js'),
  'studio/web/file-runtime-studio.html' -Destination $nyxRuntimeStage
foreach ($nyxRuntimeWidth in @(1280, 390)) {
  $nyxRuntimeURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxRuntimeChild/"
  Invoke-RuntimeTool (Join-Path $nyxRuntimeDriver 'nyx_file_save_browser_consumer.exe') @(
    ($nyxRuntimeURL + 'index.html'), (Join-Path $nyxRuntimeRoot "application-$nyxRuntimeWidth"),
    [string]$nyxRuntimeWidth, 'compiled-picker') (Join-Path $nyxRuntimeRoot "application-$nyxRuntimeWidth.log") $true
  Invoke-RuntimeTool (Join-Path $nyxRuntimeDriver 'nyx_browser_ready_capture.exe') @(
    ($nyxRuntimeURL + 'file-runtime-studio.html'), (Join-Path $nyxRuntimeRoot "studio-$nyxRuntimeWidth"),
    'data-file-runtime-studio', [string]$nyxRuntimeWidth, '900', '--fixture-timeout=60') (Join-Path $nyxRuntimeRoot "studio-$nyxRuntimeWidth.log") $true
}
Write-Host 'Real compiled browser applications/pickers and both-target Studio input pass. Static fixtures are not installed artifacts; native generated launch execution and preserving delivery remain separate.'
