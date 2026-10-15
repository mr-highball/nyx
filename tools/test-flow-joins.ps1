# nyx
# Copyright (c) 2020 mr-highball
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$Toolchain = '.local/toolchain.json',
  [string]$StaticJobsDirectory = '',
  [string]$BuildsBaseURL = ''
)

# Only platform-tool orchestration. Pascal owns join/source/semantic and
# actual-control checks. Existing JSON/editing consumers also exercise previous
# draft versions and the extended ordinary configuration journey.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxJoinRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxJoinRoot) {
  throw 'Branch join qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxJoinTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('core','native','web','export')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxJoinRoot $nyxPart) | Out-Null
}
function Invoke-JoinTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 14
    throw "Branch join qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxJoinFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxJoinCore = Join-Path $nyxJoinRoot 'core'
$nyxJoinNative = Join-Path $nyxJoinRoot 'native'
$nyxJoinWeb = Join-Path $nyxJoinRoot 'web'
$nyxJoinExport = Join-Path $nyxJoinRoot 'export'
Invoke-JoinTool $nyxJoinTools.FPC ($nyxJoinFlags + @(
  "-FU$nyxJoinCore", "-FE$nyxJoinCore", 'tests/nyx_flow_join_tests.lpr')) (Join-Path $nyxJoinRoot 'core-compile.log')
Invoke-JoinTool (Join-Path $nyxJoinCore 'nyx_flow_join_tests.exe') @($nyxJoinExport) (Join-Path $nyxJoinRoot 'core-run.log')
foreach ($nyxTest in @('nyx_flow_condition_tests','nyx_async_flow_tests','nyx_logic_relations_tests')) {
  Invoke-JoinTool $nyxJoinTools.FPC ($nyxJoinFlags + @(
    "-FU$nyxJoinCore", "-FE$nyxJoinCore", "tests/$nyxTest.lpr")) (Join-Path $nyxJoinRoot "$nyxTest-compile.log")
  Invoke-JoinTool (Join-Path $nyxJoinCore "$nyxTest.exe") @() (Join-Path $nyxJoinRoot "$nyxTest-run.log")
}
$nyxJoinLazarus = $nyxJoinTools.LAZARUS
Invoke-JoinTool $nyxJoinTools.LCL_FPC ($nyxJoinFlags + @(
  "-Fu$nyxJoinLazarus/lcl/units/i386-win32",
  "-Fu$nyxJoinLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxJoinLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxJoinLazarus/packager/units/i386-win32", "-Fu$nyxJoinExport",
  "-FU$nyxJoinNative", "-FE$nyxJoinNative",
  'tests/nyx_flow_join_controls.lpr')) (Join-Path $nyxJoinRoot 'native-compile.log')
Invoke-JoinTool (Join-Path $nyxJoinNative 'nyx_flow_join_controls.exe') @(
  (Join-Path $nyxJoinRoot 'native-report.png')) (Join-Path $nyxJoinRoot 'native-run.log')
foreach ($nyxTest in @('nyx_flow_join_tests','nyx_flow_join_controls','nyx_async_flow_tests')) {
  Invoke-JoinTool $nyxJoinTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-Fu$nyxJoinExport", "-FE$nyxJoinWeb",
    "tests/$nyxTest.lpr") (Join-Path $nyxJoinRoot "$nyxTest-browser-compile.log")
}
$nyxJoinRegression = Join-Path $nyxJoinRoot 'regression'
$nyxRegressionOptions = @{ EvidenceDirectory=$nyxJoinRegression; Toolchain=$Toolchain; StaticJobsDirectory=$StaticJobsDirectory; BuildsBaseURL=$BuildsBaseURL }
& (Join-Path $PSScriptRoot 'test-logic-json-editing.ps1') @nyxRegressionOptions
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native joins and legacy controls pass; browser execution remains separate.'
  return
}
# No new listener: a unique immutable static child belongs only to this consumer.
$nyxJoinChild = 'nyx-flow-joins-' + [guid]::NewGuid().ToString('N')
$nyxJoinStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxJoinChild
New-Item -ItemType Directory $nyxJoinStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxJoinWeb 'nyx_flow_join_tests.js'),
  (Join-Path $nyxJoinWeb 'nyx_flow_join_controls.js'),
  (Join-Path $nyxJoinWeb 'nyx_async_flow_tests.js'),
  'studio/web/flow-join.html', 'studio/web/flow-join-controls.html', 'studio/web/logic-async.html',
  $nyxJoinTools.PAS2JS_RUNTIME -Destination $nyxJoinStage
$nyxJoinURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxJoinChild/"
$nyxJoinURL | Set-Content -LiteralPath (Join-Path $nyxJoinRoot 'static-url.txt') -Encoding utf8
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(
    @('core','flow-join.html','data-flow-join'),
    @('application','flow-join-controls.html','data-join-controls'),
    @('async','logic-async.html','data-async-flow'))) {
    Invoke-JoinTool (Join-Path $nyxJoinRegression 'driver/nyx_browser_ready_capture.exe') @(
      ($nyxJoinURL+$nyxCase[1]), (Join-Path $nyxJoinRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2], [string]$nyxWidth, '900', '--fixture-timeout=360') (Join-Path $nyxJoinRoot "$($nyxCase[0])-$nyxWidth.log")
  }
}
Write-Host 'Native and actual HTTP browser branch joins, semantic-authored branch applications and ordinary configuration pass.'
