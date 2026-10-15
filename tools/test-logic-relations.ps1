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

# Only platform-tool orchestration. Pascal owns relation/source/semantic and
# actual-control checks. Existing JSON/editing consumers also exercise previous
# draft versions and the extended ordinary configuration journey.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxRelationRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxRelationRoot) {
  throw 'Relation qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxRelationTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('core','native','web','export')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxRelationRoot $nyxPart) | Out-Null
}
function Invoke-RelationTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 14
    throw "Relation qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxRelationFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxRelationCore = Join-Path $nyxRelationRoot 'core'
$nyxRelationNative = Join-Path $nyxRelationRoot 'native'
$nyxRelationWeb = Join-Path $nyxRelationRoot 'web'
$nyxRelationExport = Join-Path $nyxRelationRoot 'export'
Invoke-RelationTool $nyxRelationTools.FPC ($nyxRelationFlags + @(
  "-FU$nyxRelationCore", "-FE$nyxRelationCore", 'tests/nyx_logic_relations_tests.lpr')) (Join-Path $nyxRelationRoot 'core-compile.log')
Invoke-RelationTool (Join-Path $nyxRelationCore 'nyx_logic_relations_tests.exe') @($nyxRelationExport) (Join-Path $nyxRelationRoot 'core-run.log')
$nyxRelationLazarus = $nyxRelationTools.LAZARUS
Invoke-RelationTool $nyxRelationTools.LCL_FPC ($nyxRelationFlags + @(
  "-Fu$nyxRelationLazarus/lcl/units/i386-win32",
  "-Fu$nyxRelationLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxRelationLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxRelationLazarus/packager/units/i386-win32", "-Fu$nyxRelationExport",
  "-FU$nyxRelationNative", "-FE$nyxRelationNative",
  'tests/nyx_logic_relations_controls.lpr')) (Join-Path $nyxRelationRoot 'native-compile.log')
Invoke-RelationTool (Join-Path $nyxRelationNative 'nyx_logic_relations_controls.exe') @(
  (Join-Path $nyxRelationRoot 'native-report.png')) (Join-Path $nyxRelationRoot 'native-run.log')
foreach ($nyxTest in @('nyx_logic_relations_tests','nyx_logic_relations_controls')) {
  Invoke-RelationTool $nyxRelationTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-Fu$nyxRelationExport", "-FE$nyxRelationWeb",
    "tests/$nyxTest.lpr") (Join-Path $nyxRelationRoot "$nyxTest-browser-compile.log")
}
$nyxRelationRegression = Join-Path $nyxRelationRoot 'regression'
$nyxRegressionOptions = @{ EvidenceDirectory=$nyxRelationRegression; Toolchain=$Toolchain; StaticJobsDirectory=$StaticJobsDirectory; BuildsBaseURL=$BuildsBaseURL }
& (Join-Path $PSScriptRoot 'test-logic-json-editing.ps1') @nyxRegressionOptions
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native relation and legacy controls pass; browser execution remains separate.'
  return
}
# No new listener: a unique immutable static child belongs only to this consumer.
$nyxRelationChild = 'nyx-relations-' + [guid]::NewGuid().ToString('N')
$nyxRelationStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxRelationChild
New-Item -ItemType Directory $nyxRelationStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxRelationWeb 'nyx_logic_relations_tests.js'),
  (Join-Path $nyxRelationWeb 'nyx_logic_relations_controls.js'),
  'studio/web/logic-relations.html', 'studio/web/logic-relations-controls.html',
  $nyxRelationTools.PAS2JS_RUNTIME -Destination $nyxRelationStage
$nyxRelationURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxRelationChild/"
$nyxRelationURL | Set-Content -LiteralPath (Join-Path $nyxRelationRoot 'static-url.txt') -Encoding utf8
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(
    @('core','logic-relations.html','data-relations'),
    @('report','logic-relations-controls.html','data-relation-controls'))) {
    Invoke-RelationTool (Join-Path $nyxRelationRegression 'driver/nyx_browser_ready_capture.exe') @(
      ($nyxRelationURL+$nyxCase[1]), (Join-Path $nyxRelationRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2], [string]$nyxWidth, '900', '--fixture-timeout=360') (Join-Path $nyxRelationRoot "$($nyxCase[0])-$nyxWidth.log")
  }
}
Write-Host 'Native and actual HTTP browser relations, semantic-authored Tables and ordinary configuration pass.'
