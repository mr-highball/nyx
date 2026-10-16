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
  [switch]$ControlConnectors
)
$ErrorActionPreference = 'Stop'
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
$nyxCanvasLCL = $nyxCanvasTools.LAZARUS
$nyxCanvasPlatform = "$((& $nyxCanvasTools.LCL_FPC '-iTP').Trim())-$((& $nyxCanvasTools.LCL_FPC '-iTO').Trim())"
$nyxCanvasInputDefine = '-dNYX_CANVAS_ROUTE_AUTHORING'
if ($ControlConnectors) {
  $nyxCanvasInputDefine = '-dNYX_CANVAS_CONTROL_CONNECTORS'
}
Invoke-CanvasTool $nyxCanvasTools.LCL_FPC ($nyxCanvasFlags + @(
  "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform",
  "-Fu$nyxCanvasLCL/lcl/units/$nyxCanvasPlatform/$Widgetset",
  "-Fu$nyxCanvasLCL/components/lazutils/lib/$nyxCanvasPlatform",
  "-Fu$nyxCanvasLCL/packager/units/$nyxCanvasPlatform",
  $nyxCanvasInputDefine,"-FU$nyxCanvasNative","-FE$nyxCanvasNative",
  'tests/nyx_program_logic_controls.lpr')) (Join-Path $nyxCanvasRoot 'controls-compile.log')
Invoke-CanvasTool (Join-Path $nyxCanvasNative 'nyx_program_logic_controls.exe') @() (
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
