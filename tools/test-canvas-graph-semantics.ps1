# { nyx
#   Copyright (c) 2020 mr-highball
# 
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
# 
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
# 
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE.
# }

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$SourceEvidence = '',
  [string]$Widgetset = 'win32'
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxSemanticRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxSemanticRoot) {
  throw 'Semantic graph qualification requires a fresh owned evidence directory'
}
$nyxSemanticTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$nyxSemanticNative = Join-Path $nyxSemanticRoot 'native'
$nyxSemanticWeb = Join-Path $nyxSemanticRoot 'web'
$nyxSemanticExports = Join-Path $nyxSemanticRoot 'exports'
New-Item -ItemType Directory -Force $nyxSemanticNative,$nyxSemanticWeb | Out-Null

# Pascal owns semantic calls, exact paired admission/history, schema checks,
# actual Studio observation and constructor execution. This shell starts no
# HTTP host and never addresses an installed server or protected user project.
function Invoke-GraphSemanticTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -TotalCount 20
    Get-Content -LiteralPath $Log -Tail 8
    throw "Semantic graph qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxSemanticFlags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FU$nyxSemanticNative","-FE$nyxSemanticNative")
$nyxSemanticBrowser = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxSemanticWeb")
Invoke-GraphSemanticTool $nyxSemanticTools.FPC ($nyxSemanticFlags + @(
  'tests/nyx_canvas_graph_semantics_tests.lpr')) (Join-Path $nyxSemanticRoot 'semantic-compile.log')
Invoke-GraphSemanticTool (Join-Path $nyxSemanticNative 'nyx_canvas_graph_semantics_tests.exe') @(
  $nyxSemanticExports) (Join-Path $nyxSemanticRoot 'semantic-run.log')
Invoke-GraphSemanticTool $nyxSemanticTools.PAS2JS ($nyxSemanticBrowser + @(
  'tests/nyx_canvas_graph_semantics_tests.lpr')) (Join-Path $nyxSemanticRoot 'semantic-browser-compile.log')
$nyxSemanticTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
[IO.File]::WriteAllText((Join-Path $nyxSemanticWeb 'index.html'),
  $nyxSemanticTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_graph_semantics_tests.js'),
  [Text.UTF8Encoding]::new($false))
Copy-Item -LiteralPath $nyxSemanticTools.PAS2JS_RUNTIME -Destination (Join-Path $nyxSemanticWeb 'rtl.js')
foreach ($nyxSemanticStage in @('before','arranged','cleared')) {
  $nyxSemanticSource = Join-Path $nyxSemanticExports $nyxSemanticStage
  $nyxSemanticCompiled = Join-Path $nyxSemanticRoot "compiled/$nyxSemanticStage"
  $nyxSemanticStageWeb = Join-Path $nyxSemanticWeb $nyxSemanticStage
  New-Item -ItemType Directory -Force $nyxSemanticCompiled,$nyxSemanticStageWeb | Out-Null
  Invoke-GraphSemanticTool $nyxSemanticTools.FPC (@("-Fu$nyxSemanticSource") + $nyxSemanticFlags + @(
    "-FU$nyxSemanticCompiled","-FE$nyxSemanticCompiled",
    'tests/nyx_canvas_saved_routes_compiled.lpr')) (Join-Path $nyxSemanticCompiled 'compile.log')
  Invoke-GraphSemanticTool (Join-Path $nyxSemanticCompiled 'nyx_canvas_saved_routes_compiled.exe') @(
    (Join-Path $nyxSemanticSource 'design.nyx')) (Join-Path $nyxSemanticCompiled 'run.log')
  Invoke-GraphSemanticTool $nyxSemanticTools.PAS2JS (@("-Fu$nyxSemanticSource") + $nyxSemanticBrowser + @(
    "-FE$nyxSemanticStageWeb",'tests/nyx_canvas_saved_routes_compiled.lpr')) (
    Join-Path $nyxSemanticStageWeb 'compile.log')
  Copy-Item -LiteralPath (Join-Path $nyxSemanticSource 'design.nyx') -Destination $nyxSemanticStageWeb
  [IO.File]::WriteAllText((Join-Path $nyxSemanticStageWeb 'index.html'),
    $nyxSemanticTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_saved_routes_compiled.js').Replace(
      'src="rtl.js"','src="../rtl.js"'),[Text.UTF8Encoding]::new($false))
}
$nyxSemanticLCL = $nyxSemanticTools.LAZARUS
$nyxSemanticPlatform = "$((& $nyxSemanticTools.LCL_FPC '-iTP').Trim())-$((& $nyxSemanticTools.LCL_FPC '-iTO').Trim())"
$nyxSemanticLCLFlags = $nyxSemanticFlags + @(
  "-Fu$nyxSemanticLCL/lcl/units/$nyxSemanticPlatform",
  "-Fu$nyxSemanticLCL/lcl/units/$nyxSemanticPlatform/$Widgetset",
  "-Fu$nyxSemanticLCL/components/lazutils/lib/$nyxSemanticPlatform",
  "-Fu$nyxSemanticLCL/packager/units/$nyxSemanticPlatform")
Invoke-GraphSemanticTool $nyxSemanticTools.LCL_FPC ($nyxSemanticLCLFlags + @(
  'tests/nyx_canvas_graph_semantic_studio.lpr')) (Join-Path $nyxSemanticRoot 'observer-compile.log')
Invoke-GraphSemanticTool (Join-Path $nyxSemanticNative 'nyx_canvas_graph_semantic_studio.exe') @(
  (Join-Path $nyxSemanticRoot 'owned-studio'),(Join-Path $nyxSemanticRoot 'studio.png')) (
  Join-Path $nyxSemanticRoot 'observer-run.log')
if ($SourceEvidence -eq '') {
  Invoke-GraphSemanticTool $nyxSemanticTools.LCL_FPC ($nyxSemanticLCLFlags + @(
    'tests/nyx_canvas_graph_source_tests.lpr')) (Join-Path $nyxSemanticRoot 'source-compile.log')
  Invoke-GraphSemanticTool (Join-Path $nyxSemanticNative 'nyx_canvas_graph_source_tests.exe') @(
    (Get-Location).Path,(Join-Path (Get-Location) '.local/toolchain.json'),
    (Join-Path $nyxSemanticRoot 'owned-compiler'),'--semantic-layout') (
    Join-Path $nyxSemanticRoot 'source-run.log')
} else {
  # Explicit evidence reuse is not another compiler/runtime execution. WORK.md
  # records its exact source relationship and independent actual compiler jobs.
  $nyxSemanticRetained = [IO.Path]::GetFullPath($SourceEvidence)
  $nyxSemanticRetainedLog = Join-Path $nyxSemanticRetained 'run.log'
  if (-not (Select-String -LiteralPath $nyxSemanticRetainedLog -Pattern '^PASS 21 actual compiler-backed graph source checks$' -Quiet)) {
    throw 'Explicit source evidence lacks the completed semantic-layout journey'
  }
  if (-not (Test-Path -LiteralPath (Join-Path $nyxSemanticRetained 'owned-runtime/accepted.pas'))) {
    throw 'Explicit source evidence lacks its exact accepted handwritten export'
  }
  [IO.File]::WriteAllText((Join-Path $nyxSemanticRoot 'source-evidence-private.txt'),
    $nyxSemanticRetained,[Text.UTF8Encoding]::new($false))
}
foreach ($nyxSemanticRegression in @('nyx_canvas_document_tests','nyx_canvas_reuse_tests',
  'nyx_canvas_saved_routes_tests','nyx_canvas_graph_layout_tests')) {
  Invoke-GraphSemanticTool $nyxSemanticTools.FPC ($nyxSemanticFlags + @(
    "tests/$nyxSemanticRegression.lpr")) (Join-Path $nyxSemanticRoot "$nyxSemanticRegression-compile.log")
  Invoke-GraphSemanticTool (Join-Path $nyxSemanticNative "$nyxSemanticRegression.exe") @() (
    Join-Path $nyxSemanticRoot "$nyxSemanticRegression-run.log")
  Invoke-GraphSemanticTool $nyxSemanticTools.PAS2JS ($nyxSemanticBrowser + @(
    "tests/$nyxSemanticRegression.lpr")) (Join-Path $nyxSemanticRoot "$nyxSemanticRegression-browser-compile.log")
}
Invoke-GraphSemanticTool $nyxSemanticTools.LCL_FPC ($nyxSemanticLCLFlags + @(
  'studio/nyx_studio_native.lpr')) (Join-Path $nyxSemanticRoot 'studio-native-compile.log')
Invoke-GraphSemanticTool $nyxSemanticTools.LCL_FPC ($nyxSemanticLCLFlags + @(
  'studio/nyx_studio_server.lpr')) (Join-Path $nyxSemanticRoot 'server-compile.log')
Invoke-GraphSemanticTool $nyxSemanticTools.PAS2JS ($nyxSemanticBrowser + @(
  'studio/nyx_studio.lpr')) (Join-Path $nyxSemanticRoot 'studio-browser-compile.log')
Invoke-GraphSemanticTool $nyxSemanticTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$nyxSemanticWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $nyxSemanticRoot 'worker-compile.log')
Write-Host 'Production semantic/layout, compiled companions and native Studio checks passed; browser payloads compiled only.'

