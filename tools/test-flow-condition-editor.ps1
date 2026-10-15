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

# Orchestration only. Pascal owns semantic seeding, ordinary Studio input,
# exact paired-history assertions, source export and actual compiled file I/O.
# The caller preflights any shared static host independently; no service starts.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxConditionEditorRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxConditionEditorRoot) {
  throw 'Condition editor qualification needs a fresh owned directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxConditionEditorTools = Get-Content -LiteralPath $Toolchain -Raw | ConvertFrom-Json
foreach ($nyxPart in @('core','studio','runtime','web','driver')) {
  New-Item -ItemType Directory (Join-Path $nyxConditionEditorRoot $nyxPart) | Out-Null
}
function Invoke-ConditionEditorTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "Condition editor qualification failed; retained $Log"
  }
}
$nyxConditionEditorFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxConditionEditorLazarus = $nyxConditionEditorTools.LAZARUS
$nyxConditionEditorLCL = @(
  "-Fu$nyxConditionEditorLazarus/lcl/units/i386-win32",
  "-Fu$nyxConditionEditorLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxConditionEditorLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxConditionEditorLazarus/packager/units/i386-win32")
$nyxConditionEditorCore = Join-Path $nyxConditionEditorRoot 'core'
$nyxConditionEditorStudio = Join-Path $nyxConditionEditorRoot 'studio'
$nyxConditionEditorRuntime = Join-Path $nyxConditionEditorRoot 'runtime'
$nyxConditionEditorWeb = Join-Path $nyxConditionEditorRoot 'web'
$nyxConditionEditorDriver = Join-Path $nyxConditionEditorRoot 'driver'
$nyxConditionEditorExport = Join-Path $nyxConditionEditorRoot 'owned-project'
Invoke-ConditionEditorTool $nyxConditionEditorTools.FPC ($nyxConditionEditorFlags + @(
  "-FU$nyxConditionEditorCore", "-FE$nyxConditionEditorCore", 'tests/nyx_flow_condition_editor_tests.lpr')) (Join-Path $nyxConditionEditorRoot 'core-compile.log')
Invoke-ConditionEditorTool (Join-Path $nyxConditionEditorCore 'nyx_flow_condition_editor_tests.exe') @() (Join-Path $nyxConditionEditorRoot 'core-run.log')
Invoke-ConditionEditorTool $nyxConditionEditorTools.LCL_FPC ($nyxConditionEditorFlags + $nyxConditionEditorLCL + @(
  "-FU$nyxConditionEditorStudio", "-FE$nyxConditionEditorStudio", 'tests/nyx_flow_condition_studio_controls.lpr')) (Join-Path $nyxConditionEditorRoot 'studio-compile.log')
Invoke-ConditionEditorTool (Join-Path $nyxConditionEditorStudio 'nyx_flow_condition_studio_controls.exe') @(
  $nyxConditionEditorExport) (Join-Path $nyxConditionEditorRoot 'studio-run.log')
$nyxConditionEditorCompanion = @('-dNYX_FLOW_CONDITIONS','-dNYX_FILE_OPEN_COMPANION', "-Fu$nyxConditionEditorExport")
Invoke-ConditionEditorTool $nyxConditionEditorTools.LCL_FPC ($nyxConditionEditorFlags + $nyxConditionEditorLCL + $nyxConditionEditorCompanion + @(
  "-FU$nyxConditionEditorRuntime", "-FE$nyxConditionEditorRuntime", 'tests/nyx_file_open_controls.lpr')) (Join-Path $nyxConditionEditorRoot 'runtime-compile.log')
Invoke-ConditionEditorTool (Join-Path $nyxConditionEditorRuntime 'nyx_file_open_controls.exe') @(
  (Join-Path $nyxConditionEditorRoot 'selected-inputs')) (Join-Path $nyxConditionEditorRoot 'runtime-run.log')
foreach ($nyxTest in @('nyx_flow_condition_editor_tests','nyx_flow_condition_studio_controls','nyx_file_open_controls')) {
  $nyxBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js','-Fusrc','-Fustudio',
    '-Futests','-Fisrc', "-FE$nyxConditionEditorWeb")
  if ($nyxTest -eq 'nyx_file_open_controls') {
    $nyxBrowserFlags += $nyxConditionEditorCompanion
  }
  Invoke-ConditionEditorTool $nyxConditionEditorTools.PAS2JS ($nyxBrowserFlags + @(
    "tests/$nyxTest.lpr")) (Join-Path $nyxConditionEditorRoot "$nyxTest-browser-compile.log")
}
Invoke-ConditionEditorTool $nyxConditionEditorTools.PAS2JS @('-B','-Mdelphi','-Tmodule',
  '-Jirtl.js','-Fusrc','-Fustudio','-Fisrc', "-FE$nyxConditionEditorWeb", 'studio/nyx_source_worker.lpr') (Join-Path $nyxConditionEditorRoot 'worker-compile.log')
foreach ($nyxTest in @('nyx_browser_ready_capture','nyx_file_open_browser_consumer')) {
  Invoke-ConditionEditorTool $nyxConditionEditorTools.FPC ($nyxConditionEditorFlags + @(
    "-FU$nyxConditionEditorDriver", "-FE$nyxConditionEditorDriver", "tests/$nyxTest.lpr")) (Join-Path $nyxConditionEditorRoot "$nyxTest-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native condition form, ordinary Studio history and exact compiled file routes pass; browser execution remains separate.'
  return
}
$nyxConditionEditorChild = 'nyx-condition-editor-' + [guid]::NewGuid().ToString('N')
$nyxConditionEditorStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxConditionEditorChild
New-Item -ItemType Directory $nyxConditionEditorStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxConditionEditorWeb 'nyx_flow_condition_editor_tests.js'),
  (Join-Path $nyxConditionEditorWeb 'nyx_flow_condition_studio_controls.js'),
  (Join-Path $nyxConditionEditorWeb 'nyx_file_open_controls.js'),
  (Join-Path $nyxConditionEditorWeb 'nyx_source_worker.js'),
  'studio/web/flow-condition-editor.html','studio/web/flow-condition-studio.html',
  'studio/web/flow-condition-controls.html', $nyxConditionEditorTools.PAS2JS_RUNTIME -Destination $nyxConditionEditorStage
$nyxConditionEditorURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxConditionEditorChild/"
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(@('core','flow-condition-editor.html','data-condition-editor-contract'),
    @('studio','flow-condition-studio.html','data-condition-studio'))) {
    Invoke-ConditionEditorTool (Join-Path $nyxConditionEditorDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxConditionEditorURL + $nyxCase[1]), (Join-Path $nyxConditionEditorRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2], [string]$nyxWidth,'900','--fixture-timeout=600') (Join-Path $nyxConditionEditorRoot "$($nyxCase[0])-$nyxWidth.log")
  }
  Invoke-ConditionEditorTool (Join-Path $nyxConditionEditorDriver 'nyx_file_open_browser_consumer.exe') @(
    ($nyxConditionEditorURL + 'flow-condition-controls.html'), (Join-Path $nyxConditionEditorRoot "runtime-$nyxWidth"),
    [string]$nyxWidth, (Join-Path $nyxConditionEditorRoot 'selected-inputs')) (Join-Path $nyxConditionEditorRoot "runtime-$nyxWidth.log")
}
Write-Host 'Condition forms, ordinary Studio controls/history and exact UI-authored application routes pass on native and executed HTTP browser targets. No active service, design or enrollment is replaced.'
