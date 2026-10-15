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

# Installed compiler / owned consumer orchestration only. Pascal owns profile
# admission, semantic application values, actual controls, file fixtures and
# generated launcher execution. No service/listener or existing project changes.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxOpenRuntimeRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxOpenRuntimeRoot) {
  throw 'Opening runtime qualification requires fresh owned evidence.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxOpenRuntimeTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('builder','application','studio','read','web','driver')) {
  New-Item -ItemType Directory (Join-Path $nyxOpenRuntimeRoot $nyxPart) | Out-Null
}
function Invoke-OpenRuntimeTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "Opening runtime qualification failed; retained $Log"
  }
}
$nyxOpenRuntimeFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxOpenRuntimeLazarus = $nyxOpenRuntimeTools.LAZARUS
$nyxOpenRuntimeLCL = @(
  "-Fu$nyxOpenRuntimeLazarus/lcl/units/i386-win32",
  "-Fu$nyxOpenRuntimeLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxOpenRuntimeLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxOpenRuntimeLazarus/packager/units/i386-win32")
$nyxOpenRuntimeJobs = Join-Path $nyxOpenRuntimeRoot 'compiler-jobs'
$nyxOpenRuntimeDriver = Join-Path $nyxOpenRuntimeRoot 'driver'
$nyxOpenRuntimeWeb = Join-Path $nyxOpenRuntimeRoot 'web'
foreach ($nyxPair in @(@('nyx_file_runtime_build','builder'),
  @('nyx_file_open_application_tests','application'),
  @('nyx_file_runtime_studio_controls','studio'),@('nyx_file_open_controls','read'))) {
  $nyxTest = $nyxPair[0]
  $nyxOut = Join-Path $nyxOpenRuntimeRoot $nyxPair[1]
  $nyxArguments = $nyxOpenRuntimeFlags + $nyxOpenRuntimeLCL + @("-FU$nyxOut", "-FE$nyxOut")
  $nyxInput = @()
  switch ($nyxTest) {
    'nyx_file_runtime_build' {
      $nyxInput = @((Get-Location).Path,$nyxOpenRuntimeJobs,
        [IO.Path]::GetFullPath($Toolchain),'--opening')
    }
    'nyx_file_runtime_studio_controls' {
      $nyxInput = @((Join-Path $nyxOpenRuntimeRoot 'owned-studio'))
    }
    'nyx_file_open_controls' {
      $nyxArguments += @('-dNYX_FLOW_CONDITIONS','-dNYX_FILE_OPEN_COMPANION', "-Fu$nyxOpenRuntimeJobs")
      $nyxInput = @((Join-Path $nyxOpenRuntimeRoot 'selected-inputs'))
    }
  }
  Invoke-OpenRuntimeTool $nyxOpenRuntimeTools.LCL_FPC ($nyxArguments + @(
    "tests/$nyxTest.lpr")) (Join-Path $nyxOpenRuntimeRoot "$nyxTest-compile.log")
  Invoke-OpenRuntimeTool (Join-Path $nyxOut "$nyxTest.exe") $nyxInput (Join-Path $nyxOpenRuntimeRoot "$nyxTest-run.log")
}
foreach ($nyxTest in @('nyx_file_open_launch_native','nyx_file_open_launch_browser',
  'nyx_file_open_browser_consumer','nyx_browser_ready_capture')) {
  Invoke-OpenRuntimeTool $nyxOpenRuntimeTools.FPC ($nyxOpenRuntimeFlags + @(
    "-FU$nyxOpenRuntimeDriver", "-FE$nyxOpenRuntimeDriver", "tests/$nyxTest.lpr")) (Join-Path $nyxOpenRuntimeRoot "$nyxTest-compile.log")
}
$nyxOpenRuntimeInputs = @(Get-ChildItem -LiteralPath (Join-Path $nyxOpenRuntimeRoot 'selected-inputs') -File -Filter '*.json')
if ($nyxOpenRuntimeInputs.Count -ne 1) {
  throw 'The owned read fixture must supply exactly one JSON input.'
}
foreach ($nyxPolicy in @('disabled','open','both')) {
  $nyxJob = (Get-Content -LiteralPath (Join-Path $nyxOpenRuntimeJobs "lcl-$nyxPolicy-job.txt") -Raw).Trim()
  $nyxMode = 'enabled'
  if ($nyxPolicy -eq 'disabled') { $nyxMode = 'disabled' }
  Invoke-OpenRuntimeTool (Join-Path $nyxOpenRuntimeDriver 'nyx_file_open_launch_native.exe') @(
    (Join-Path $nyxOpenRuntimeJobs "build/studio/jobs/$nyxJob/nyx_native.exe"),
    $nyxOpenRuntimeInputs[0].FullName,$nyxMode) (Join-Path $nyxOpenRuntimeRoot "native-launch-$nyxPolicy.log")
}
foreach ($nyxTest in @('nyx_file_open_application_tests','nyx_file_runtime_studio_controls','nyx_file_open_controls')) {
  $nyxArguments = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js','-Fusrc','-Fustudio',
    '-Futests','-Fisrc',"-FE$nyxOpenRuntimeWeb")
  if ($nyxTest -eq 'nyx_file_open_controls') {
    $nyxArguments += @('-dNYX_FLOW_CONDITIONS','-dNYX_FILE_OPEN_COMPANION', "-Fu$nyxOpenRuntimeJobs")
  }
  Invoke-OpenRuntimeTool $nyxOpenRuntimeTools.PAS2JS ($nyxArguments + @(
    "tests/$nyxTest.lpr")) (Join-Path $nyxOpenRuntimeRoot "$nyxTest-browser-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native opening permissions/Studio/real files and all generated native launchers pass. Browser execution remains separate.'
  return
}
$nyxOpenRuntimeChild = 'nyx-open-runtime-' + [guid]::NewGuid().ToString('N')
$nyxOpenRuntimeStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxOpenRuntimeChild
New-Item -ItemType Directory $nyxOpenRuntimeStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxOpenRuntimeWeb 'nyx_file_open_application_tests.js'),
  (Join-Path $nyxOpenRuntimeWeb 'nyx_file_runtime_studio_controls.js'),
  (Join-Path $nyxOpenRuntimeWeb 'nyx_file_open_controls.js'),'studio/web/file-open-application.html',
  'studio/web/file-runtime-studio.html','studio/web/flow-condition-controls.html',
  $nyxOpenRuntimeTools.PAS2JS_RUNTIME -Destination $nyxOpenRuntimeStage
foreach ($nyxPolicy in @('disabled','open','both')) {
  $nyxJob = (Get-Content -LiteralPath (Join-Path $nyxOpenRuntimeJobs "browser-$nyxPolicy-job.txt") -Raw).Trim()
  $nyxOut = Join-Path $nyxOpenRuntimeStage $nyxPolicy
  New-Item -ItemType Directory $nyxOut | Out-Null
  Copy-Item -LiteralPath (Join-Path $nyxOpenRuntimeJobs "build/studio/jobs/$nyxJob/index.html"),
    (Join-Path $nyxOpenRuntimeJobs "build/studio/jobs/$nyxJob/nyx_preview.js"),
    $nyxOpenRuntimeTools.PAS2JS_RUNTIME -Destination $nyxOut
}
$nyxOpenRuntimeURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxOpenRuntimeChild/"
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(@('application','file-open-application.html','data-open-application'),
    @('studio','file-runtime-studio.html','data-file-runtime-studio'))) {
    Invoke-OpenRuntimeTool (Join-Path $nyxOpenRuntimeDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxOpenRuntimeURL+$nyxCase[1]),(Join-Path $nyxOpenRuntimeRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2],[string]$nyxWidth,'900','--fixture-timeout=90') (Join-Path $nyxOpenRuntimeRoot "$($nyxCase[0])-$nyxWidth.log")
  }
  foreach ($nyxPolicy in @('disabled','open','both')) {
    $nyxMode = 'enabled'
    if ($nyxPolicy -eq 'disabled') { $nyxMode = 'disabled' }
    Invoke-OpenRuntimeTool (Join-Path $nyxOpenRuntimeDriver 'nyx_file_open_launch_browser.exe') @(
      ($nyxOpenRuntimeURL+$nyxPolicy+'/index.html'),(Join-Path $nyxOpenRuntimeRoot "launch-$nyxPolicy-$nyxWidth"),
      [string]$nyxWidth,(Join-Path $nyxOpenRuntimeRoot 'selected-inputs'),$nyxMode) (Join-Path $nyxOpenRuntimeRoot "launch-$nyxPolicy-$nyxWidth.log")
  }
  Invoke-OpenRuntimeTool (Join-Path $nyxOpenRuntimeDriver 'nyx_file_open_browser_consumer.exe') @(
    ($nyxOpenRuntimeURL+'flow-condition-controls.html'),(Join-Path $nyxOpenRuntimeRoot "read-$nyxWidth"),
    [string]$nyxWidth,(Join-Path $nyxOpenRuntimeRoot 'selected-inputs')) (Join-Path $nyxOpenRuntimeRoot "read-$nyxWidth.log")
}
Write-Host 'Ordinary Studio, independent opening permissions, real generated native/browser launchers and read regressions pass. No active design/service/configuration was replaced.'
