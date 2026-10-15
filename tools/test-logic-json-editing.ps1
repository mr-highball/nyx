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

# Compilers and Pascal consumers own all substantive qualification. Optional
# HTTP execution uses a unique static child of an independently preflighted
# existing host. This never creates/replaces a service or changes live projects.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxJSONRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxJSONRoot) {
  throw 'JSON editing qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the preflighted static jobs directory and URL together.'
}
$nyxJSONTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('core','native','author','web','driver','export','collection-export')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxJSONRoot $nyxPart) | Out-Null
}
function Invoke-JSONTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 14
    throw "JSON editing qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxJSONFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxJSONCore = Join-Path $nyxJSONRoot 'core'
$nyxJSONWeb = Join-Path $nyxJSONRoot 'web'
$nyxJSONExport = Join-Path $nyxJSONRoot 'export'
$nyxJSONCollection = Join-Path $nyxJSONRoot 'collection-export'
foreach ($nyxTest in @('nyx_logic_json_editing_tests','nyx_logic_collections_tests',
  'nyx_logic_authoring_tests','nyx_http_logic_authoring_tests',
  'nyx_file_save_authoring_tests','nyx_file_open_authoring_tests',
  'nyx_flow_condition_editor_tests')) {
  Invoke-JSONTool $nyxJSONTools.FPC ($nyxJSONFlags + @(
    "-FU$nyxJSONCore", "-FE$nyxJSONCore", "tests/$nyxTest.lpr")) (Join-Path $nyxJSONRoot "$nyxTest-compile.log")
  $nyxJSONArgs = @()
  if ($nyxTest -eq 'nyx_logic_json_editing_tests') { $nyxJSONArgs = @($nyxJSONExport) }
  if ($nyxTest -eq 'nyx_logic_collections_tests') { $nyxJSONArgs = @($nyxJSONCollection) }
  Invoke-JSONTool (Join-Path $nyxJSONCore "$nyxTest.exe") $nyxJSONArgs (Join-Path $nyxJSONRoot "$nyxTest-run.log")
}
$nyxJSONLazarus = $nyxJSONTools.LAZARUS
$nyxJSONLCL = @(
  "-Fu$nyxJSONLazarus/lcl/units/i386-win32",
  "-Fu$nyxJSONLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxJSONLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxJSONLazarus/packager/units/i386-win32")
foreach ($nyxCase in @(
  @('nyx_logic_json_editing_controls','native','-dNYX_JSON_EDITING_COMPANION',$nyxJSONExport),
  @('nyx_logic_collections_controls','author','-dNYX_COLLECTION_COMPANION',$nyxJSONCollection))) {
  $nyxOut = Join-Path $nyxJSONRoot $nyxCase[1]
  Invoke-JSONTool $nyxJSONTools.LCL_FPC ($nyxJSONFlags + $nyxJSONLCL + @(
    $nyxCase[2], "-Fu$($nyxCase[3])", "-FU$nyxOut", "-FE$nyxOut",
    "tests/$($nyxCase[0]).lpr")) (Join-Path $nyxJSONRoot "$($nyxCase[1])-compile.log")
  $nyxJSONArgs = @()
  if ($nyxCase[1] -eq 'native') { $nyxJSONArgs = @((Join-Path $nyxJSONRoot 'native-request.png')) }
  Invoke-JSONTool (Join-Path $nyxOut "$($nyxCase[0]).exe") $nyxJSONArgs (Join-Path $nyxJSONRoot "$($nyxCase[1])-run.log")
  Invoke-JSONTool $nyxJSONTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxJSONWeb",
    $nyxCase[2], "-Fu$($nyxCase[3])", "tests/$($nyxCase[0]).lpr") (Join-Path $nyxJSONRoot "$($nyxCase[1])-browser-compile.log")
}
Invoke-JSONTool $nyxJSONTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxJSONWeb",
  'tests/nyx_logic_json_editing_tests.lpr') (Join-Path $nyxJSONRoot 'core-browser-compile.log')
Invoke-JSONTool $nyxJSONTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc', "-FE$nyxJSONWeb",
  'studio/nyx_source_worker.lpr') (Join-Path $nyxJSONRoot 'worker-compile.log')
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native checks and actual controls pass; browser consumers compile, execution remains separate.'
  return
}
$nyxJSONDriver = Join-Path $nyxJSONRoot 'driver'
Invoke-JSONTool $nyxJSONTools.FPC ($nyxJSONFlags + @("-FU$nyxJSONDriver", "-FE$nyxJSONDriver",
  'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxJSONRoot 'driver-compile.log')
$nyxJSONChild = 'nyx-json-editing-' + [guid]::NewGuid().ToString('N')
$nyxJSONStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxJSONChild
New-Item -ItemType Directory $nyxJSONStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxJSONWeb 'nyx_logic_json_editing_tests.js'),
  (Join-Path $nyxJSONWeb 'nyx_logic_json_editing_controls.js'),
  (Join-Path $nyxJSONWeb 'nyx_logic_collections_controls.js'),
  (Join-Path $nyxJSONWeb 'nyx_source_worker.js'),
  'studio/web/logic-json-editing.html','studio/web/logic-json-editing-controls.html',
  'studio/web/logic-collection-controls.html', $nyxJSONTools.PAS2JS_RUNTIME -Destination $nyxJSONStage
$nyxJSONURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxJSONChild/"
$nyxJSONURL | Set-Content -LiteralPath (Join-Path $nyxJSONRoot 'static-url.txt') -Encoding utf8
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(
    @('core','logic-json-editing.html','data-json-editing'),
    @('request','logic-json-editing-controls.html','data-json-editing-controls'),
    @('author','logic-collection-controls.html','data-collection-controls'))) {
    Invoke-JSONTool (Join-Path $nyxJSONDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxJSONURL+$nyxCase[1]), (Join-Path $nyxJSONRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2], [string]$nyxWidth, '900', '--fixture-timeout=360') (Join-Path $nyxJSONRoot "$($nyxCase[0])-$nyxWidth.log")
  }
}
Write-Host 'Native and executed HTTP browser JSON editing, generated request and ordinary authoring pass.'
