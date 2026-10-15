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

# Compiler orchestration only. Selection, byte comparisons, metadata, lifecycle,
# source/history, semantic operations and real browser input are Pascal consumers.
# Optional static fixtures use an independently preflighted existing HTTP host.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxOpenRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxOpenRoot) {
  throw 'File-open qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the preflighted static directory and builds URL together.'
}
$nyxOpenTools = Get-Content -LiteralPath $Toolchain -Raw | ConvertFrom-Json
foreach ($nyxOpenPart in @('core', 'authoring', 'native', 'web', 'driver', 'export')) {
  New-Item -ItemType Directory (Join-Path $nyxOpenRoot $nyxOpenPart) | Out-Null
}

function Invoke-OpenTool([string]$Executable, [string[]]$Arguments, [string]$Log, [bool]$Receipt = $false) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "File-open qualification failed; retained $Log"
  }
  if ($Receipt) {
    Get-Content -LiteralPath $Log -TotalCount 1
  }
}

$nyxOpenFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxOpenCore = Join-Path $nyxOpenRoot 'core'
$nyxOpenAuthor = Join-Path $nyxOpenRoot 'authoring'
$nyxOpenNative = Join-Path $nyxOpenRoot 'native'
$nyxOpenWeb = Join-Path $nyxOpenRoot 'web'
$nyxOpenDriver = Join-Path $nyxOpenRoot 'driver'
$nyxOpenExport = Join-Path $nyxOpenRoot 'export'
Invoke-OpenTool $nyxOpenTools.FPC ($nyxOpenFlags + @(
  "-FU$nyxOpenCore", "-FE$nyxOpenCore", 'tests/nyx_file_open_tests.lpr')) (Join-Path $nyxOpenRoot 'core-compile.log')
Invoke-OpenTool (Join-Path $nyxOpenCore 'nyx_file_open_tests.exe') @() (Join-Path $nyxOpenRoot 'core-run.log') $true
foreach ($nyxOpenTest in @('nyx_file_open_authoring_tests','nyx_logic_authoring_tests',
  'nyx_http_logic_authoring_tests','nyx_file_save_authoring_tests')) {
  Invoke-OpenTool $nyxOpenTools.FPC ($nyxOpenFlags + @(
    "-FU$nyxOpenAuthor", "-FE$nyxOpenAuthor", "tests/$nyxOpenTest.lpr")) (Join-Path $nyxOpenRoot "$nyxOpenTest-compile.log")
  $nyxOpenArguments = @()
  if ($nyxOpenTest -eq 'nyx_file_open_authoring_tests') {
    $nyxOpenArguments = @($nyxOpenExport)
  }
  Invoke-OpenTool (Join-Path $nyxOpenAuthor "$nyxOpenTest.exe") $nyxOpenArguments (Join-Path $nyxOpenRoot "$nyxOpenTest-run.log") $true
}
$nyxOpenLazarus = $nyxOpenTools.LAZARUS
$nyxOpenLCL = @(
  "-Fu$nyxOpenLazarus/lcl/units/i386-win32",
  "-Fu$nyxOpenLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxOpenLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxOpenLazarus/packager/units/i386-win32")
Invoke-OpenTool $nyxOpenTools.LCL_FPC ($nyxOpenFlags + $nyxOpenLCL + @(
  '-dNYX_FILE_OPEN_COMPANION', "-Fu$nyxOpenExport", "-FU$nyxOpenNative",
  "-FE$nyxOpenNative", 'tests/nyx_file_open_controls.lpr')) (Join-Path $nyxOpenRoot 'native-controls-compile.log')
Invoke-OpenTool (Join-Path $nyxOpenNative 'nyx_file_open_controls.exe') @(
  (Join-Path $nyxOpenRoot 'selected-inputs')) (Join-Path $nyxOpenRoot 'native-controls-run.log') $true

foreach ($nyxOpenTest in @('nyx_file_open_tests','nyx_file_open_authoring_tests','nyx_file_open_controls')) {
  $nyxOpenBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxOpenWeb")
  if ($nyxOpenTest -eq 'nyx_file_open_controls') {
    $nyxOpenBrowserFlags += @('-dNYX_FILE_OPEN_COMPANION', "-Fu$nyxOpenExport")
  }
  Invoke-OpenTool $nyxOpenTools.PAS2JS ($nyxOpenBrowserFlags + @(
    "tests/$nyxOpenTest.lpr")) (Join-Path $nyxOpenRoot "$nyxOpenTest-browser-compile.log")
}
foreach ($nyxOpenTest in @('nyx_file_open_browser_consumer','nyx_browser_ready_capture')) {
  Invoke-OpenTool $nyxOpenTools.FPC ($nyxOpenFlags + @(
    "-FU$nyxOpenDriver", "-FE$nyxOpenDriver", "tests/$nyxOpenTest.lpr")) (Join-Path $nyxOpenRoot "$nyxOpenTest-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native controls and shared/source/semantic checks pass. HTTP browser execution is not qualified.'
  return
}
$nyxOpenChild = 'nyx-file-open-' + [guid]::NewGuid().ToString('N')
$nyxOpenStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxOpenChild
New-Item -ItemType Directory $nyxOpenStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxOpenWeb 'nyx_file_open_tests.js'),
  (Join-Path $nyxOpenWeb 'nyx_file_open_authoring_tests.js'),
  (Join-Path $nyxOpenWeb 'nyx_file_open_controls.js'),
  'studio/web/file-open-contract.html', 'studio/web/file-open-authoring.html',
  'studio/web/file-open-controls.html', $nyxOpenTools.PAS2JS_RUNTIME -Destination $nyxOpenStage
foreach ($nyxOpenWidth in @(1280, 390)) {
  $nyxOpenURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxOpenChild/"
  foreach ($nyxOpenMode in @('contract', 'authoring')) {
    Invoke-OpenTool (Join-Path $nyxOpenDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxOpenURL + "file-open-$nyxOpenMode.html"),
      (Join-Path $nyxOpenRoot "$nyxOpenMode-$nyxOpenWidth"),
      "data-file-open-$nyxOpenMode", [string]$nyxOpenWidth, '900',
      '--fixture-timeout=60') (Join-Path $nyxOpenRoot "$nyxOpenMode-$nyxOpenWidth.log") $true
  }
  Invoke-OpenTool (Join-Path $nyxOpenDriver 'nyx_file_open_browser_consumer.exe') @(
    ($nyxOpenURL + 'file-open-controls.html'),
    (Join-Path $nyxOpenRoot "controls-$nyxOpenWidth"), [string]$nyxOpenWidth,
    (Join-Path $nyxOpenRoot 'selected-inputs')) (Join-Path $nyxOpenRoot "controls-$nyxOpenWidth.log") $true
}
Write-Host 'Shared contracts, crafted companions, paired history/local semantic authoring and real browser/LCL file reads pass. This starts no server, changes no active project and establishes no deployment or physical-device claim.'
