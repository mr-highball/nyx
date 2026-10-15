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

# Compiler/input orchestration only. Pascal owns semantic assertions, source
# export, disk fixtures, actual application routes and browser interactions.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxConditionsRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxConditionsRoot) {
  throw 'Conditional-flow qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and builds URL together.'
}
$nyxConditionsTools = Get-Content -LiteralPath $Toolchain -Raw | ConvertFrom-Json
foreach ($nyxConditionsPart in @('core', 'rows', 'native', 'web', 'driver')) {
  New-Item -ItemType Directory (Join-Path $nyxConditionsRoot $nyxConditionsPart) | Out-Null
}

function Invoke-ConditionTool([string]$Executable, [string[]]$Arguments, [string]$Log, [bool]$Receipt = $false) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "Conditional-flow qualification failed; retained $Log"
  }
  if ($Receipt) {
    Get-Content -LiteralPath $Log -TotalCount 1
  }
}
$nyxConditionsFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxConditionsCore = Join-Path $nyxConditionsRoot 'core'
$nyxConditionsRows = Join-Path $nyxConditionsRoot 'rows'
$nyxConditionsExport = Join-Path $nyxConditionsRoot 'export'
$nyxConditionsNative = Join-Path $nyxConditionsRoot 'native'
$nyxConditionsWeb = Join-Path $nyxConditionsRoot 'web'
$nyxConditionsDriver = Join-Path $nyxConditionsRoot 'driver'
foreach ($nyxConditionsPair in @(
  @('nyx_flow_condition_tests', $nyxConditionsCore),
  @('nyx_logic_table_tests', $nyxConditionsRows))) {
  $nyxConditionsTest = $nyxConditionsPair[0]
  $nyxConditionsOutput = $nyxConditionsPair[1]
  Invoke-ConditionTool $nyxConditionsTools.FPC ($nyxConditionsFlags + @(
    "-FU$nyxConditionsOutput", "-FE$nyxConditionsOutput", "tests/$nyxConditionsTest.lpr")) (Join-Path $nyxConditionsRoot "$nyxConditionsTest-compile.log")
  $nyxConditionsArguments = @()
  if ($nyxConditionsTest -eq 'nyx_flow_condition_tests') {
    $nyxConditionsArguments = @($nyxConditionsExport)
  }
  Invoke-ConditionTool (Join-Path $nyxConditionsOutput "$nyxConditionsTest.exe") $nyxConditionsArguments (Join-Path $nyxConditionsRoot "$nyxConditionsTest-run.log") $true
}
$nyxConditionsLazarus = $nyxConditionsTools.LAZARUS
$nyxConditionsLCL = @(
  "-Fu$nyxConditionsLazarus/lcl/units/i386-win32",
  "-Fu$nyxConditionsLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxConditionsLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxConditionsLazarus/packager/units/i386-win32")
$nyxConditionsCompanion = @('-dNYX_FLOW_CONDITIONS','-dNYX_FILE_OPEN_COMPANION',
  "-Fu$nyxConditionsExport")
Invoke-ConditionTool $nyxConditionsTools.LCL_FPC ($nyxConditionsFlags + $nyxConditionsLCL + $nyxConditionsCompanion + @(
  "-FU$nyxConditionsNative", "-FE$nyxConditionsNative",
  'tests/nyx_file_open_controls.lpr')) (Join-Path $nyxConditionsRoot 'native-controls-compile.log')
Invoke-ConditionTool (Join-Path $nyxConditionsNative 'nyx_file_open_controls.exe') @(
  (Join-Path $nyxConditionsRoot 'selected-inputs')) (Join-Path $nyxConditionsRoot 'native-controls-run.log') $true
foreach ($nyxConditionsTest in @('nyx_flow_condition_tests','nyx_logic_table_tests','nyx_file_open_controls')) {
  $nyxConditionsBrowser = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxConditionsWeb")
  if ($nyxConditionsTest -eq 'nyx_file_open_controls') {
    $nyxConditionsBrowser += $nyxConditionsCompanion
  }
  Invoke-ConditionTool $nyxConditionsTools.PAS2JS ($nyxConditionsBrowser + @(
    "tests/$nyxConditionsTest.lpr")) (Join-Path $nyxConditionsRoot "$nyxConditionsTest-browser-compile.log")
}
foreach ($nyxConditionsTest in @('nyx_file_open_browser_consumer','nyx_browser_ready_capture')) {
  Invoke-ConditionTool $nyxConditionsTools.FPC ($nyxConditionsFlags + @(
    "-FU$nyxConditionsDriver", "-FE$nyxConditionsDriver",
    "tests/$nyxConditionsTest.lpr")) (Join-Path $nyxConditionsRoot "$nyxConditionsTest-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native conditional source/history/semantic, grouped rows and real file routes pass; HTTP execution remains unqualified.'
  return
}
$nyxConditionsChild = 'nyx-conditional-flow-' + [guid]::NewGuid().ToString('N')
$nyxConditionsStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxConditionsChild
New-Item -ItemType Directory $nyxConditionsStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxConditionsWeb 'nyx_flow_condition_tests.js'),
  (Join-Path $nyxConditionsWeb 'nyx_logic_table_tests.js'),
  (Join-Path $nyxConditionsWeb 'nyx_file_open_controls.js'), 'studio/web/flow-conditions.html',
  'studio/web/logic-table.html', 'studio/web/flow-condition-controls.html',
  $nyxConditionsTools.PAS2JS_RUNTIME -Destination $nyxConditionsStage
foreach ($nyxConditionsWidth in @(1280, 390)) {
  $nyxConditionsURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxConditionsChild/"
  Invoke-ConditionTool (Join-Path $nyxConditionsDriver 'nyx_browser_ready_capture.exe') @(
    ($nyxConditionsURL + 'flow-conditions.html'),
    (Join-Path $nyxConditionsRoot "core-$nyxConditionsWidth"), 'data-flow-conditions',
    [string]$nyxConditionsWidth, '900', '--fixture-timeout=60') (Join-Path $nyxConditionsRoot "core-$nyxConditionsWidth.log") $true
  Invoke-ConditionTool (Join-Path $nyxConditionsDriver 'nyx_browser_ready_capture.exe') @(
    ($nyxConditionsURL + 'logic-table.html'), (Join-Path $nyxConditionsRoot "rows-$nyxConditionsWidth"),
    'data-logic-table', [string]$nyxConditionsWidth, '900',
    '--fixture-timeout=60') (Join-Path $nyxConditionsRoot "rows-$nyxConditionsWidth.log") $true
  Invoke-ConditionTool (Join-Path $nyxConditionsDriver 'nyx_file_open_browser_consumer.exe') @(
    ($nyxConditionsURL + 'flow-condition-controls.html'),
    (Join-Path $nyxConditionsRoot "controls-$nyxConditionsWidth"), [string]$nyxConditionsWidth,
    (Join-Path $nyxConditionsRoot 'selected-inputs')) (Join-Path $nyxConditionsRoot "controls-$nyxConditionsWidth.log") $true
}
Write-Host 'Conditional graphs, bounded local semantic history, skipped/selected row publication and exact compiled file routes pass. No listener or active design was changed; Studio condition controls, application output configuration and rollout remain separate.'
