# Nyx applied graph/connection project saving qualification.
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
[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$AdoptionEvidence = '',
  [string]$Widgetset = 'win32',
  [switch]$Products
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxGraphRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxGraphRoot) {
  throw 'Graph saving qualification requires a fresh owned evidence directory'
}
New-Item -ItemType Directory -Force $nyxGraphRoot | Out-Null
$nyxGraphTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
# The maintained Pascal input consumer owns its isolated Studio and exports exact
# accepted bytes. This shell only orchestrates compilers; it starts no HTTP host.
if ($AdoptionEvidence -eq '') {
  $nyxGraphAdoption = Join-Path $nyxGraphRoot 'adoption'
  & (Join-Path $PSScriptRoot 'test-canvas-routes.ps1') -AdoptConnections -Widgetset $Widgetset -EvidenceDirectory $nyxGraphAdoption
} else {
  # Explicit reuse keeps already qualified input evidence immutable when only
  # a compiler consumer changes. It is not a new ordinary-editor execution.
  $nyxGraphAdoption = [IO.Path]::GetFullPath($AdoptionEvidence)
  foreach ($nyxGraphStage in @('first','second','graph-only')) {
    foreach ($nyxGraphArtifact in @('design.nyx','nyx.generated.view.pas')) {
      if (-not (Test-Path -LiteralPath (Join-Path $nyxGraphAdoption "owned-studio/exports/$nyxGraphStage/$nyxGraphArtifact"))) {
        throw 'Explicit adoption evidence is missing an exact accepted pair'
      }
    }
  }
  [IO.File]::WriteAllText((Join-Path $nyxGraphRoot 'adoption-evidence-private.txt'),$nyxGraphAdoption)
}

function Invoke-GraphTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Graph saving qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxGraphLCL = $nyxGraphTools.LAZARUS
$nyxGraphPlatform = "$((& $nyxGraphTools.LCL_FPC '-iTP').Trim())-$((& $nyxGraphTools.LCL_FPC '-iTO').Trim())"
$nyxGraphCommon = Join-Path $nyxGraphAdoption 'controls'
$nyxGraphFlags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-Fu$nyxGraphCommon",
  "-Fu$nyxGraphLCL/lcl/units/$nyxGraphPlatform",
  "-Fu$nyxGraphLCL/lcl/units/$nyxGraphPlatform/$Widgetset",
  "-Fu$nyxGraphLCL/components/lazutils/lib/$nyxGraphPlatform",
  "-Fu$nyxGraphLCL/packager/units/$nyxGraphPlatform")
$nyxGraphWeb = Join-Path $nyxGraphRoot 'web'
New-Item -ItemType Directory -Force $nyxGraphWeb | Out-Null
Copy-Item -LiteralPath $nyxGraphTools.PAS2JS_RUNTIME -Destination (Join-Path $nyxGraphWeb 'rtl.js')
$nyxGraphTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw

foreach ($nyxGraphCase in @(
  @{Name='first';Caption='Hello, Avery! / Hello, Avery!'},
  @{Name='second';Caption='Hello, Avery! + Hello, Avery!'},
  @{Name='graph-only';Caption='Hello, Avery! * Hello, Avery!'})) {
  $nyxGraphSource = Join-Path $nyxGraphAdoption ("owned-studio/exports/" + $nyxGraphCase.Name)
  $nyxGraphNative = Join-Path $nyxGraphRoot ("compiled/" + $nyxGraphCase.Name)
  $nyxGraphBrowser = Join-Path $nyxGraphWeb $nyxGraphCase.Name
  New-Item -ItemType Directory -Force $nyxGraphNative,$nyxGraphBrowser | Out-Null
  # Separate output/search directories prevent a previous stage's generated unit
  # from satisfying this exact-source constructor and actual application check.
  Invoke-GraphTool $nyxGraphTools.LCL_FPC (@("-Fu$nyxGraphSource") + $nyxGraphFlags + @(
    "-FU$nyxGraphNative","-FE$nyxGraphNative",'tests/nyx_canvas_graph_compiled.lpr')) (
    Join-Path $nyxGraphNative 'compile.log')
  Invoke-GraphTool (Join-Path $nyxGraphNative 'nyx_canvas_graph_compiled.exe') @(
    (Join-Path $nyxGraphSource 'design.nyx'),$nyxGraphCase.Caption) (
    Join-Path $nyxGraphNative 'run.log')
  Invoke-GraphTool $nyxGraphTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    "-Fu$nyxGraphSource",'-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxGraphBrowser",
    'tests/nyx_canvas_graph_compiled.lpr') (Join-Path $nyxGraphBrowser 'compile.log')
  Copy-Item -LiteralPath (Join-Path $nyxGraphSource 'design.nyx') -Destination (
    Join-Path $nyxGraphBrowser 'expected.nyx')
  $nyxGraphHTML = $nyxGraphTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_graph_compiled.js')
  $nyxGraphHTML = $nyxGraphHTML.Replace('src="rtl.js"','src="../rtl.js"')
  $nyxGraphHTML = $nyxGraphHTML.Replace('<body>','<body data-expected-caption="' + $nyxGraphCase.Caption + '">')
  [IO.File]::WriteAllText((Join-Path $nyxGraphBrowser 'index.html'),$nyxGraphHTML,
    [Text.UTF8Encoding]::new($false))
}
$nyxGraphSourceNative = Join-Path $nyxGraphRoot 'source-tests'
New-Item -ItemType Directory -Force $nyxGraphSourceNative | Out-Null
Invoke-GraphTool $nyxGraphTools.LCL_FPC ($nyxGraphFlags + @(
  "-FU$nyxGraphSourceNative","-FE$nyxGraphSourceNative",'tests/nyx_canvas_graph_source_tests.lpr')) (
    Join-Path $nyxGraphRoot 'source-compile.log')
Invoke-GraphTool (Join-Path $nyxGraphSourceNative 'nyx_canvas_graph_source_tests.exe') @(
  (Get-Location).Path,(Join-Path (Get-Location) '.local/toolchain.json'),
  (Join-Path $nyxGraphRoot 'handwritten')) (Join-Path $nyxGraphRoot 'source-run.log')

# The runtime workbench still applies/export graphs without a project host.
Invoke-GraphTool $nyxGraphTools.LCL_FPC ($nyxGraphFlags + @(
  "-FU$nyxGraphSourceNative","-FE$nyxGraphSourceNative",'tests/nyx_tool_logic_controls.lpr')) (
    Join-Path $nyxGraphRoot 'runtime-compile.log')
Invoke-GraphTool (Join-Path $nyxGraphSourceNative 'nyx_tool_logic_controls.exe') @() (
  Join-Path $nyxGraphRoot 'runtime-run.log')
Invoke-GraphTool $nyxGraphTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxGraphWeb",
  'tests/nyx_tool_logic_controls.lpr') (Join-Path $nyxGraphRoot 'runtime-browser-compile.log')

if ($Products) {
  Invoke-GraphTool $nyxGraphTools.LCL_FPC ($nyxGraphFlags + @(
    "-FU$nyxGraphSourceNative","-FE$nyxGraphSourceNative",'studio/nyx_studio_native.lpr')) (
      Join-Path $nyxGraphRoot 'studio-native-compile.log')
  Invoke-GraphTool $nyxGraphTools.LCL_FPC ($nyxGraphFlags + @(
    "-FU$nyxGraphSourceNative","-FE$nyxGraphSourceNative",'studio/nyx_studio_server.lpr')) (
      Join-Path $nyxGraphRoot 'server-compile.log')
  Invoke-GraphTool $nyxGraphTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Fisrc',"-FE$nyxGraphWeb",'studio/nyx_studio.lpr') (
      Join-Path $nyxGraphRoot 'studio-browser-compile.log')
}
