# Nyx dynamic canvas control-route qualification orchestration.
# MIT License
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
  [string]$Widgetset = 'win32',
  [switch]$ControlConnectors,
  [switch]$PipelineNodes,
  [switch]$SavedConnections,
  [switch]$AdoptConnections
)
$ErrorActionPreference = 'Stop'
if ($PipelineNodes) {
  $ControlConnectors = $true
}
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxCanvasRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxCanvasRoot) {
  throw 'Canvas route qualification requires a fresh owned evidence directory'
}
$nyxCanvasTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$nyxCanvasWeb = Join-Path $nyxCanvasRoot 'web'
New-Item -ItemType Directory -Force $nyxCanvasWeb | Out-Null
function Invoke-CanvasTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Canvas route qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxCanvasFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxCanvasBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxCanvasWeb")
$nyxCanvasTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
$nyxCanvasLCL = $nyxCanvasTools.LAZARUS
$nyxCanvasPlatform = "$((& $nyxCanvasTools.LCL_FPC '-iTP').Trim())-$((& $nyxCanvasTools.LCL_FPC '-iTO').Trim())"
if ($AdoptConnections) {
  if ($ControlConnectors -or $SavedConnections) {
    throw 'Select ordinary connection adoption separately from other route journeys'
  }
  $nyxCanvasCore = Join-Path $nyxCanvasRoot 'adoption-core'
  $nyxCanvasNative = Join-Path $nyxCanvasRoot 'controls'
  New-Item -ItemType Directory -Force $nyxCanvasCore,$nyxCanvasNative | Out-Null
  Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
    "-FU$nyxCanvasCore","-FE$nyxCanvasCore",'tests/nyx_canvas_adoption_tests.lpr')) (
    Join-Path $nyxCanvasRoot 'adoption-native-compile.log')
  Invoke-CanvasTool (Join-Path $nyxCanvasCore 'nyx_canvas_adoption_tests.exe') @() (
    Join-Path $nyxCanvasRoot 'adoption-native-run.log')
  Invoke-CanvasTool $nyxCanvasTools.LCL_FPC ($nyxCanvasFlags + @(
    "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform",
    "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform/$Widgetset",
    "-Fu$nyxCanvasLCL/components/lazutils/lib/$nyxCanvasPlatform",
    "-Fu$nyxCanvasLCL/packager/units/$nyxCanvasPlatform",
    "-FU$nyxCanvasNative","-FE$nyxCanvasNative",'tests/nyx_canvas_adoption_studio_controls.lpr')) (
    Join-Path $nyxCanvasRoot 'adoption-controls-compile.log')
  Invoke-CanvasTool (Join-Path $nyxCanvasNative 'nyx_canvas_adoption_studio_controls.exe') @(
    (Join-Path $nyxCanvasRoot 'owned-studio')) (Join-Path $nyxCanvasRoot 'adoption-controls-run.log')
  foreach ($nyxCanvasConsumer in @('nyx_canvas_adoption_tests','nyx_canvas_adoption_studio_controls')) {
    Invoke-CanvasTool $nyxCanvasTools.PAS2JS ($nyxCanvasBrowserFlags + @(
      "tests/$nyxCanvasConsumer.lpr")) (Join-Path $nyxCanvasRoot "$nyxCanvasConsumer-browser-compile.log")
    [IO.File]::WriteAllText((Join-Path $nyxCanvasWeb "$nyxCanvasConsumer.html"),
      $nyxCanvasTemplate.Replace('nyx_program_logic_controls.js',"$nyxCanvasConsumer.js"),
      [Text.UTF8Encoding]::new($false))
  }
  # The ordinary browser source queue uses the matched Pascal worker. Staging
  # that worker is preparation only; this runner neither starts HTTP nor claims
  # actual browser execution from compilation or native widget input.
  Invoke-CanvasTool $nyxCanvasTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxCanvasWeb",
    'studio/nyx_source_worker.lpr') (Join-Path $nyxCanvasRoot 'source-worker-compile.log')
  Copy-Item -LiteralPath $nyxCanvasTools.PAS2JS_RUNTIME -Destination $nyxCanvasWeb
  Write-Host 'Portable/native Studio connection adoption passed; browser consumers and worker compiled only.'
  Write-Host 'This command starts no server and changes only its fresh owned evidence directory.'
  return
}
if ($SavedConnections) {
  if ($ControlConnectors) {
    throw 'Select saved connection qualification or runtime connector input separately'
  }
  $nyxCanvasCore = Join-Path $nyxCanvasRoot 'saved-core'
  $nyxCanvasExports = Join-Path $nyxCanvasRoot 'exports'
  New-Item -ItemType Directory -Force $nyxCanvasCore | Out-Null
  Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
    "-FU$nyxCanvasCore","-FE$nyxCanvasCore",'tests/nyx_canvas_saved_routes_tests.lpr')) (
    Join-Path $nyxCanvasRoot 'saved-native-compile.log')
  Invoke-CanvasTool (Join-Path $nyxCanvasCore 'nyx_canvas_saved_routes_tests.exe') @($nyxCanvasExports) (
    Join-Path $nyxCanvasRoot 'saved-native-run.log')
  Invoke-CanvasTool $nyxCanvasTools.PAS2JS ($nyxCanvasBrowserFlags + @(
    'tests/nyx_canvas_saved_routes_tests.lpr')) (Join-Path $nyxCanvasRoot 'saved-browser-compile.log')
  [IO.File]::WriteAllText((Join-Path $nyxCanvasWeb 'saved-connections.html'),
    $nyxCanvasTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_saved_routes_tests.js'),
    [Text.UTF8Encoding]::new($false))
  foreach ($nyxCanvasStage in @('literal','stage-0','stage-1','stage-2','stage-3',
    'stage-4','stage-5','stage-6','runtime')) {
    $nyxCanvasStageSource = Join-Path $nyxCanvasExports $nyxCanvasStage
    $nyxCanvasStageNative = Join-Path $nyxCanvasRoot "constructors/$nyxCanvasStage"
    $nyxCanvasStageWeb = Join-Path $nyxCanvasWeb $nyxCanvasStage
    New-Item -ItemType Directory -Force $nyxCanvasStageNative,$nyxCanvasStageWeb | Out-Null
    # Reuse the exact checked core, but compile each different companion in its
    # own output folder. A previous stage's unit can never satisfy this import.
    Invoke-CanvasTool $nyxCanvasTools.FPC (@('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
      "-Fu$nyxCanvasStageSource","-Fu$nyxCanvasCore",'-Fusrc','-Fustudio','-Futests','-Fisrc',
      "-FU$nyxCanvasStageNative","-FE$nyxCanvasStageNative",
      'tests/nyx_canvas_saved_routes_compiled.lpr')) (
      Join-Path $nyxCanvasStageNative 'compile.log')
    Invoke-CanvasTool (Join-Path $nyxCanvasStageNative 'nyx_canvas_saved_routes_compiled.exe') @(
      (Join-Path $nyxCanvasStageSource 'design.nyx')) (Join-Path $nyxCanvasStageNative 'run.log')
    Invoke-CanvasTool $nyxCanvasTools.PAS2JS ($nyxCanvasBrowserFlags + @(
      "-Fu$nyxCanvasStageSource","-FE$nyxCanvasStageWeb",
      'tests/nyx_canvas_saved_routes_compiled.lpr')) (Join-Path $nyxCanvasStageWeb 'compile.log')
    Copy-Item -LiteralPath (Join-Path $nyxCanvasStageSource 'design.nyx'),$nyxCanvasTools.PAS2JS_RUNTIME -Destination $nyxCanvasStageWeb
    [IO.File]::WriteAllText((Join-Path $nyxCanvasStageWeb 'index.html'),
      $nyxCanvasTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_saved_routes_compiled.js'),
      [Text.UTF8Encoding]::new($false))
  }
  $nyxCanvasNative = Join-Path $nyxCanvasRoot 'controls'
  $nyxCanvasRuntime = Join-Path $nyxCanvasExports 'runtime'
  New-Item -ItemType Directory -Force $nyxCanvasNative | Out-Null
  Invoke-CanvasTool $nyxCanvasTools.LCL_FPC ($nyxCanvasFlags + @(
    "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform",
    "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform/$Widgetset",
    "-Fu$nyxCanvasLCL/components/lazutils/lib/$nyxCanvasPlatform",
    "-Fu$nyxCanvasLCL/packager/units/$nyxCanvasPlatform",
    "-Fu$nyxCanvasRuntime",'-dNYX_SAVED_CANVAS_ROUTES',
    "-FU$nyxCanvasNative","-FE$nyxCanvasNative",'tests/nyx_program_logic_controls.lpr')) (
    Join-Path $nyxCanvasRoot 'saved-controls-compile.log')
  Invoke-CanvasTool (Join-Path $nyxCanvasNative 'nyx_program_logic_controls.exe') @() (
    Join-Path $nyxCanvasRoot 'saved-controls-run.log')
  Invoke-CanvasTool $nyxCanvasTools.PAS2JS ($nyxCanvasBrowserFlags + @(
    "-Fu$nyxCanvasRuntime",'-dNYX_SAVED_CANVAS_ROUTES','tests/nyx_program_logic_controls.lpr')) (
    Join-Path $nyxCanvasRoot 'saved-controls-browser-compile.log')
  Copy-Item -LiteralPath $nyxCanvasTools.PAS2JS_RUNTIME,'studio/web/program-logic-controls.html' -Destination $nyxCanvasWeb
  Write-Host 'Saved contracts, exact native constructors and actual native controls passed.'
  Write-Host 'Matching browser consumers are staged, not executed. This command starts no server.'
  return
}
foreach ($nyxCanvasName in @('nyx_observation_publication_tests',
  'nyx_tool_connection_tests','nyx_canvas_route_overlay_tests','nyx_canvas_observation_tests')) {
  $nyxCanvasNative = Join-Path $nyxCanvasRoot $nyxCanvasName
  New-Item -ItemType Directory -Force $nyxCanvasNative | Out-Null
  Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
    "-FU$nyxCanvasNative","-FE$nyxCanvasNative","tests/$nyxCanvasName.lpr")) (
    Join-Path $nyxCanvasRoot "$nyxCanvasName-native-compile.log")
  Invoke-CanvasTool (Join-Path $nyxCanvasNative "$nyxCanvasName.exe") @() (
    Join-Path $nyxCanvasRoot "$nyxCanvasName-native-run.log")
  Invoke-CanvasTool $nyxCanvasTools.PAS2JS ($nyxCanvasBrowserFlags + @(
    "tests/$nyxCanvasName.lpr")) (Join-Path $nyxCanvasRoot "$nyxCanvasName-browser-compile.log")
  [IO.File]::WriteAllText((Join-Path $nyxCanvasWeb "$nyxCanvasName.html"),
    $nyxCanvasTemplate.Replace('nyx_program_logic_controls.js',"$nyxCanvasName.js"),
    [Text.UTF8Encoding]::new($false))
}
$nyxCanvasNative = Join-Path $nyxCanvasRoot 'controls'
New-Item -ItemType Directory -Force $nyxCanvasNative | Out-Null
$nyxCanvasInputDefine = '-dNYX_CANVAS_ROUTE_AUTHORING'
if ($ControlConnectors) {
  $nyxCanvasInputDefine = '-dNYX_CANVAS_CONTROL_CONNECTORS'
}
if ($PipelineNodes) {
  $nyxCanvasInputDefine = '-dNYX_PIPELINE_NODE_FACES'
}
Invoke-CanvasTool $nyxCanvasTools.LCL_FPC ($nyxCanvasFlags + @(
  "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform",
  "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform/$Widgetset",
  "-Fu$nyxCanvasLCL/components/lazutils/lib/$nyxCanvasPlatform",
  "-Fu$nyxCanvasLCL/packager/units/$nyxCanvasPlatform",
  $nyxCanvasInputDefine,"-FU$nyxCanvasNative","-FE$nyxCanvasNative",
  'tests/nyx_program_logic_controls.lpr')) (Join-Path $nyxCanvasRoot 'controls-compile.log')
$nyxCanvasInputArguments = @()
if ($PipelineNodes) {
  $nyxCanvasInputArguments = @((Join-Path $nyxCanvasRoot 'native-pipeline-nodes.png'))
}
Invoke-CanvasTool (Join-Path $nyxCanvasNative 'nyx_program_logic_controls.exe') $nyxCanvasInputArguments (
  Join-Path $nyxCanvasRoot 'controls-run.log')
Invoke-CanvasTool $nyxCanvasTools.PAS2JS ($nyxCanvasBrowserFlags + @(
  $nyxCanvasInputDefine,'tests/nyx_program_logic_controls.lpr')) (
  Join-Path $nyxCanvasRoot 'controls-browser-compile.log')
Copy-Item -LiteralPath $nyxCanvasTools.PAS2JS_RUNTIME,'studio/web/program-logic-controls.html' -Destination $nyxCanvasWeb
$nyxCanvasDriver = Join-Path $nyxCanvasRoot 'driver'
New-Item -ItemType Directory -Force $nyxCanvasDriver | Out-Null
Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
  "-FU$nyxCanvasDriver","-FE$nyxCanvasDriver",'tests/nyx_browser_ready_capture.lpr')) (
  Join-Path $nyxCanvasRoot 'driver-compile.log')
if ($ControlConnectors) {
  Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
    "-FU$nyxCanvasDriver","-FE$nyxCanvasDriver",'tests/nyx_canvas_connector_input.lpr')) (
    Join-Path $nyxCanvasRoot 'connector-driver-compile.log')
  Write-Host 'For connector host keys/touch, use nyx_canvas_connector_input with URL, fresh capture directory, CSS width and height.'
}
Write-Host 'Native contracts and actual controls passed. Browser fixtures staged; no server was started.'
Write-Host 'Serve this owned web directory using the Pascal static host, then run the ready driver at desktop/narrow widths.'
Write-Host 'Markers: data-observation-publication, data-tool-connection-contract, data-route-overlay, data-program-logic-controls.'
if ($PipelineNodes) {
  Write-Host 'Operation-node input currently uses a wide 1800x960 review layout; narrow navigation remains a separate qualification gap.'
}
