# Nyx saved graph-node layout qualification.
#  Copyright (c) 2020 mr-highball
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$NodeEvidence = '',
  [string]$Widgetset = 'win32'
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxLayoutRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxLayoutRoot) {
  throw 'Graph layout qualification requires a fresh owned evidence directory'
}
New-Item -ItemType Directory -Path $nyxLayoutRoot | Out-Null
$nyxLayoutTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json

# Shell orchestration only. Pascal owns real input, compiler verification,
# candidate/history equality and worker retirement. This starts no HTTP host.
function Invoke-LayoutTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Graph layout qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
if ($NodeEvidence -eq '') {
  & (Join-Path $PSScriptRoot 'test-tool-graph-only.ps1') -Layout -Widgetset $Widgetset -EvidenceDirectory (Join-Path $nyxLayoutRoot 'nodes')
} else {
  # Explicit completed-node evidence is immutable reuse, not another execution.
  $nyxLayoutNodes = [IO.Path]::GetFullPath($NodeEvidence)
  foreach ($nyxLayoutNodeFile in @('native-run.log','companion-run.log','owned-source/accepted.nyx','owned-source/nyx.generated.view.pas')) {
    if (-not (Test-Path -LiteralPath (Join-Path $nyxLayoutNodes $nyxLayoutNodeFile))) {
      throw 'Explicit node evidence is incomplete'
    }
  }
  foreach ($nyxLayoutNodeLog in @('native-run.log','companion-run.log')) {
    if (-not (Select-String -LiteralPath (Join-Path $nyxLayoutNodes $nyxLayoutNodeLog) -Pattern '^PASS [0-9]+ actual graph-only canvas checks$' -Quiet)) {
      throw 'Explicit node evidence has not completed successfully'
    }
  }
  [IO.File]::WriteAllText((Join-Path $nyxLayoutRoot 'node-evidence-private.txt'),$nyxLayoutNodes)
}
$nyxLayoutStudio = Join-Path $nyxLayoutRoot 'studio'
& (Join-Path $PSScriptRoot 'test-canvas-routes.ps1') -AdoptConnections -GraphLayout -Widgetset $Widgetset -EvidenceDirectory $nyxLayoutStudio
$nyxLayoutSaving = Join-Path $nyxLayoutRoot 'saving'
& (Join-Path $PSScriptRoot 'test-canvas-graph-saving.ps1') -Products -Widgetset $Widgetset -AdoptionEvidence $nyxLayoutStudio -EvidenceDirectory $nyxLayoutSaving

# The same real source consumer has an additional computed-position case.
# Its changed helper must still determine untouched placement after combined
# program/graph/mask/layout publication, with complete pair/history comparison.
Invoke-LayoutTool (Join-Path $nyxLayoutSaving 'source-tests/nyx_canvas_graph_source_tests.exe') @(
  (Get-Location).Path,(Join-Path (Get-Location) '.local/toolchain.json'),
  (Join-Path $nyxLayoutRoot 'handwritten-layout'),'--layout') (
  Join-Path $nyxLayoutRoot 'layout-source-run.log')

$nyxLayoutNative = Join-Path $nyxLayoutRoot 'pure'
$nyxLayoutWeb = Join-Path $nyxLayoutRoot 'web'
New-Item -ItemType Directory -Force $nyxLayoutNative,$nyxLayoutWeb | Out-Null
Invoke-LayoutTool $nyxLayoutTools.FPC @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',
  "-Fu$(Join-Path $nyxLayoutSaving 'source-tests')",
  "-FU$nyxLayoutNative","-FE$nyxLayoutNative",'tests/nyx_canvas_graph_layout_tests.lpr') (
  Join-Path $nyxLayoutRoot 'pure-compile.log')
Invoke-LayoutTool (Join-Path $nyxLayoutNative 'nyx_canvas_graph_layout_tests.exe') @() (
  Join-Path $nyxLayoutRoot 'pure-run.log')
Invoke-LayoutTool $nyxLayoutTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxLayoutWeb",
  'tests/nyx_canvas_graph_layout_tests.lpr') (Join-Path $nyxLayoutRoot 'pure-browser-compile.log')
Copy-Item -LiteralPath $nyxLayoutTools.PAS2JS_RUNTIME -Destination (Join-Path $nyxLayoutWeb 'rtl.js')
$nyxLayoutHTML = (Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw).Replace(
  'nyx_program_logic_controls.js','nyx_canvas_graph_layout_tests.js')
[IO.File]::WriteAllText((Join-Path $nyxLayoutWeb 'index.html'),$nyxLayoutHTML,[Text.UTF8Encoding]::new($false))
& (Join-Path $PSScriptRoot 'test-canvas-routes.ps1') -InternalConnections -Widgetset $Widgetset -EvidenceDirectory (Join-Path $nyxLayoutRoot 'routes')
Write-Host 'Native graph layout, ordinary Studio/source/history and regression checks passed; browser payloads compile only.'
