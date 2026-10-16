# Nyx runtime Tool Canvas Logic qualification.
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
  [string]$Widgetset = 'win32',
  [switch]$ProgramSource,
  [switch]$Products
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxLogicRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxLogicRoot) {
  throw 'Tool Logic qualification requires a fresh owned evidence directory'
}
$nyxLogicTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$nyxLogicNative = Join-Path $nyxLogicRoot 'native'
$nyxLogicWeb = Join-Path $nyxLogicRoot 'web'
New-Item -ItemType Directory -Force $nyxLogicNative,$nyxLogicWeb | Out-Null

# Shell orchestration only. Pascal owns actual control input, typed script/graph
# publication, stale/draft guards and retirement. This runner starts no server.
function Invoke-LogicTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Tool Logic qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained heap allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxLogicLCL = $nyxLogicTools.LAZARUS
$nyxLogicPlatform = "$((& $nyxLogicTools.LCL_FPC '-iTP').Trim())-$((& $nyxLogicTools.LCL_FPC '-iTO').Trim())"
$nyxLogicFlags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',
  "-Fu$nyxLogicLCL/lcl/units/$nyxLogicPlatform",
  "-Fu$nyxLogicLCL/lcl/units/$nyxLogicPlatform/$Widgetset",
  "-Fu$nyxLogicLCL/components/lazutils/lib/$nyxLogicPlatform",
  "-Fu$nyxLogicLCL/packager/units/$nyxLogicPlatform",
  "-FU$nyxLogicNative","-FE$nyxLogicNative")
Invoke-LogicTool $nyxLogicTools.LCL_FPC (@('-B') + $nyxLogicFlags + @(
  'tests/nyx_tool_logic_controls.lpr')) (Join-Path $nyxLogicRoot 'native-compile.log')
foreach ($nyxLogicContract in @('nyx_logic_authoring_tests','nyx_canvas_adoption_tests')) {
  Invoke-LogicTool $nyxLogicTools.LCL_FPC ($nyxLogicFlags + @(
    "tests/$nyxLogicContract.lpr")) (Join-Path $nyxLogicRoot "$nyxLogicContract-compile.log")
  Invoke-LogicTool (Join-Path $nyxLogicNative "$nyxLogicContract.exe") @() (
    Join-Path $nyxLogicRoot "$nyxLogicContract-run.log")
}
Invoke-LogicTool (Join-Path $nyxLogicNative 'nyx_tool_logic_controls.exe') @(
  (Join-Path $nyxLogicRoot 'native-logic.png')) (Join-Path $nyxLogicRoot 'native-run.log')
Invoke-LogicTool (Join-Path $nyxLogicNative 'nyx_tool_logic_controls.exe') @(
  (Join-Path $nyxLogicRoot 'native-workbench.png'),'--review-workbench') (
    Join-Path $nyxLogicRoot 'native-review-run.log')

$nyxLogicBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxLogicWeb")
Invoke-LogicTool $nyxLogicTools.PAS2JS ($nyxLogicBrowserFlags + @(
  'tests/nyx_tool_logic_controls.lpr')) (Join-Path $nyxLogicRoot 'browser-compile.log')
Copy-Item -LiteralPath $nyxLogicTools.PAS2JS_RUNTIME -Destination (Join-Path $nyxLogicWeb 'rtl.js')
$nyxLogicTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
[IO.File]::WriteAllText((Join-Path $nyxLogicWeb 'index.html'),
  $nyxLogicTemplate.Replace('nyx_program_logic_controls.js','nyx_tool_logic_controls.js'),
  [Text.UTF8Encoding]::new($false))
if ($Products) {
  Invoke-LogicTool $nyxLogicTools.LCL_FPC ($nyxLogicFlags + @(
    'studio/nyx_studio_native.lpr')) (Join-Path $nyxLogicRoot 'studio-native-compile.log')
  Invoke-LogicTool $nyxLogicTools.LCL_FPC ($nyxLogicFlags + @(
    'studio/nyx_studio_server.lpr')) (Join-Path $nyxLogicRoot 'server-compile.log')
  Invoke-LogicTool $nyxLogicTools.PAS2JS ($nyxLogicBrowserFlags + @(
    'studio/nyx_studio.lpr')) (Join-Path $nyxLogicRoot 'studio-browser-compile.log')
  Invoke-LogicTool $nyxLogicTools.PAS2JS (@('-B','-Mdelphi','-Tmodule','-Jirtl.js',
    '-Fusrc','-Fustudio','-Fisrc',"-FE$nyxLogicWeb",'studio/nyx_source_worker.lpr')) (
    Join-Path $nyxLogicRoot 'worker-compile.log')
}

if ($ProgramSource) {
  # Exact handwritten source and real compiler admission remain a distinct
  # consumer from the runtime editor. Pascal owns all construction/history/input.
  Invoke-LogicTool $nyxLogicTools.LCL_FPC ($nyxLogicFlags + @(
    'tests/nyx_canvas_graph_source_tests.lpr')) (Join-Path $nyxLogicRoot 'program-source-compile.log')
  $nyxLogicHandwritten = Join-Path $nyxLogicRoot 'handwritten'
  Invoke-LogicTool (Join-Path $nyxLogicNative 'nyx_canvas_graph_source_tests.exe') @(
    (Get-Location).Path,(Join-Path (Get-Location) '.local/toolchain.json'),$nyxLogicHandwritten) (
    Join-Path $nyxLogicRoot 'program-source-run.log')
  $nyxLogicSaved = Join-Path $nyxLogicRoot 'compiled-program'
  New-Item -ItemType Directory -Force $nyxLogicSaved | Out-Null
  Invoke-LogicTool $nyxLogicTools.LCL_FPC (@("-Fu$nyxLogicHandwritten") + $nyxLogicFlags + @(
    "-FU$nyxLogicSaved","-FE$nyxLogicSaved",'tests/nyx_canvas_graph_compiled.lpr')) (
    Join-Path $nyxLogicRoot 'compiled-program-compile.log')
  Invoke-LogicTool (Join-Path $nyxLogicSaved 'nyx_canvas_graph_compiled.exe') @(
    (Join-Path $nyxLogicHandwritten 'accepted.nyx'),'Welcome, Avery! / Welcome, Avery!','Echo greeting') (
    Join-Path $nyxLogicRoot 'compiled-program-run.log')
  $nyxLogicSavedWeb = Join-Path $nyxLogicWeb 'saved-program'
  New-Item -ItemType Directory -Force $nyxLogicSavedWeb | Out-Null
  Invoke-LogicTool $nyxLogicTools.PAS2JS (@("-Fu$nyxLogicHandwritten") + @(
    '-B','-Mdelphi','-Tbrowser','-Jirtl.js','-Fusrc','-Fustudio','-Futests','-Fisrc',
    "-FE$nyxLogicSavedWeb",'tests/nyx_canvas_graph_compiled.lpr')) (
    Join-Path $nyxLogicRoot 'compiled-program-browser-compile.log')
  Copy-Item -LiteralPath (Join-Path $nyxLogicHandwritten 'accepted.nyx') -Destination (
    Join-Path $nyxLogicSavedWeb 'expected.nyx')
  $nyxLogicSavedHTML = $nyxLogicTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_graph_compiled.js')
  $nyxLogicSavedHTML = $nyxLogicSavedHTML.Replace('src="rtl.js"','src="../rtl.js"')
  $nyxLogicSavedHTML = $nyxLogicSavedHTML.Replace('<body>','<body data-expected-caption="Welcome, Avery! / Welcome, Avery!" data-expected-graph="Echo greeting">')
  [IO.File]::WriteAllText((Join-Path $nyxLogicSavedWeb 'index.html'),$nyxLogicSavedHTML,
    [Text.UTF8Encoding]::new($false))
}
