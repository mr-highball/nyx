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
  [string]$UnicodeDataDirectory = '',
  [string]$StaticJobsDirectory = '',
  [string]$BuildsBaseURL = ''
)

# Only platform-tool orchestration. Pascal owns regex/source/semantic and
# actual-control checks. Existing JSON/editing consumers also exercise previous
# draft versions and the extended ordinary configuration journey.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxRegexRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxRegexRoot) {
  throw 'Regex qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxRegexTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('core','native','web','export')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxRegexRoot $nyxPart) | Out-Null
}
function Invoke-RegexTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 14
    throw "Regex qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxRegexFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxRegexCore = Join-Path $nyxRegexRoot 'core'
$nyxRegexNative = Join-Path $nyxRegexRoot 'native'
$nyxRegexWeb = Join-Path $nyxRegexRoot 'web'
$nyxRegexExport = Join-Path $nyxRegexRoot 'export'
if ($UnicodeDataDirectory -ne '') {
  # Offline inputs are pinned/admitted in Pascal. Reproduce the checked-in
  # include and compare every valid scalar against an independently read oracle.
  $nyxRegexUCD = [IO.Path]::GetFullPath($UnicodeDataDirectory)
  Invoke-RegexTool $nyxRegexTools.FPC ($nyxRegexFlags + @(
    "-FU$nyxRegexCore", "-FE$nyxRegexCore", 'tools/nyx_unicode_regex.lpr')) (Join-Path $nyxRegexRoot 'unicode-generator-compile.log')
  $nyxRegexGenerated = Join-Path $nyxRegexRoot 'unicode-regenerated.inc'
  Invoke-RegexTool (Join-Path $nyxRegexCore 'nyx_unicode_regex.exe') @(
    (Join-Path $nyxRegexUCD 'UnicodeData.txt'), (Join-Path $nyxRegexUCD 'CaseFolding.txt'),
    (Join-Path $nyxRegexUCD 'license.txt'), $nyxRegexGenerated) (Join-Path $nyxRegexRoot 'unicode-generator-run.log')
  if ((Get-FileHash -LiteralPath $nyxRegexGenerated -Algorithm SHA256).Hash -ne
    (Get-FileHash -LiteralPath 'src/nyx.unicode.regex.inc' -Algorithm SHA256).Hash) {
    throw 'Pinned Unicode regeneration differs from the checked-in product table.'
  }
  Invoke-RegexTool $nyxRegexTools.FPC ($nyxRegexFlags + @(
    "-FU$nyxRegexCore", "-FE$nyxRegexCore", 'tests/nyx_unicode_regex_tests.lpr')) (Join-Path $nyxRegexRoot 'unicode-oracle-compile.log')
  Invoke-RegexTool (Join-Path $nyxRegexCore 'nyx_unicode_regex_tests.exe') @(
    (Join-Path $nyxRegexUCD 'UnicodeData.txt'), (Join-Path $nyxRegexUCD 'CaseFolding.txt'),
    (Join-Path $nyxRegexRoot 'unicode-oracle.json')) (Join-Path $nyxRegexRoot 'unicode-oracle-run.log')
}
Invoke-RegexTool $nyxRegexTools.FPC ($nyxRegexFlags + @(
  "-FU$nyxRegexCore", "-FE$nyxRegexCore", 'tests/nyx_regex_tests.lpr')) (Join-Path $nyxRegexRoot 'core-compile.log')
Invoke-RegexTool (Join-Path $nyxRegexCore 'nyx_regex_tests.exe') @($nyxRegexExport) (Join-Path $nyxRegexRoot 'core-run.log')
foreach ($nyxTest in @('nyx_flow_condition_tests','nyx_async_flow_tests','nyx_logic_relations_tests','nyx_flow_join_tests')) {
  Invoke-RegexTool $nyxRegexTools.FPC ($nyxRegexFlags + @(
    "-FU$nyxRegexCore", "-FE$nyxRegexCore", "tests/$nyxTest.lpr")) (Join-Path $nyxRegexRoot "$nyxTest-compile.log")
  Invoke-RegexTool (Join-Path $nyxRegexCore "$nyxTest.exe") @() (Join-Path $nyxRegexRoot "$nyxTest-run.log")
}
$nyxRegexLazarus = $nyxRegexTools.LAZARUS
Invoke-RegexTool $nyxRegexTools.LCL_FPC ($nyxRegexFlags + @(
  "-Fu$nyxRegexLazarus/lcl/units/i386-win32",
  "-Fu$nyxRegexLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxRegexLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxRegexLazarus/packager/units/i386-win32", "-Fu$nyxRegexExport",
  "-FU$nyxRegexNative", "-FE$nyxRegexNative",
  'tests/nyx_regex_controls.lpr')) (Join-Path $nyxRegexRoot 'native-compile.log')
Invoke-RegexTool (Join-Path $nyxRegexNative 'nyx_regex_controls.exe') @(
  (Join-Path $nyxRegexRoot 'native-report.png')) (Join-Path $nyxRegexRoot 'native-run.log')
foreach ($nyxTest in @('nyx_regex_tests','nyx_regex_controls','nyx_async_flow_tests')) {
  Invoke-RegexTool $nyxRegexTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-Fu$nyxRegexExport", "-FE$nyxRegexWeb",
    "tests/$nyxTest.lpr") (Join-Path $nyxRegexRoot "$nyxTest-browser-compile.log")
}
$nyxRegexRegression = Join-Path $nyxRegexRoot 'regression'
$nyxRegressionOptions = @{ EvidenceDirectory=$nyxRegexRegression; Toolchain=$Toolchain; StaticJobsDirectory=$StaticJobsDirectory; BuildsBaseURL=$BuildsBaseURL }
& (Join-Path $PSScriptRoot 'test-logic-json-editing.ps1') @nyxRegressionOptions
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native regex and legacy controls pass; browser execution remains separate.'
  return
}
# No new listener: a unique immutable static child belongs only to this consumer.
$nyxRegexChild = 'nyx-regex-' + [guid]::NewGuid().ToString('N')
$nyxRegexStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxRegexChild
New-Item -ItemType Directory $nyxRegexStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxRegexWeb 'nyx_regex_tests.js'),
  (Join-Path $nyxRegexWeb 'nyx_regex_controls.js'),
  (Join-Path $nyxRegexWeb 'nyx_async_flow_tests.js'),
  'studio/web/regex-tests.html', 'studio/web/regex-controls.html', 'studio/web/logic-async.html',
  $nyxRegexTools.PAS2JS_RUNTIME -Destination $nyxRegexStage
$nyxRegexURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxRegexChild/"
$nyxRegexURL | Set-Content -LiteralPath (Join-Path $nyxRegexRoot 'static-url.txt') -Encoding utf8
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(
    @('core','regex-tests.html','data-regex'),
    @('application','regex-controls.html','data-regex-controls'),
    @('async','logic-async.html','data-async-flow'))) {
    Invoke-RegexTool (Join-Path $nyxRegexRegression 'driver/nyx_browser_ready_capture.exe') @(
      ($nyxRegexURL+$nyxCase[1]), (Join-Path $nyxRegexRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2], [string]$nyxWidth, '900', '--fixture-timeout=360') (Join-Path $nyxRegexRoot "$($nyxCase[0])-$nyxWidth.log")
  }
}
Write-Host 'Native and actual HTTP browser regex transformations, semantic-authored text applications and ordinary configuration pass.'
