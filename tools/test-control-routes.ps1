# Nyx control/event/script/control qualification orchestration.
# MIT License
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
  [string]$Widgetset = 'win32'
)
$ErrorActionPreference = 'Stop'
$nyxRouteRepository = Split-Path $PSScriptRoot -Parent
Set-Location $nyxRouteRepository
$nyxRouteRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxRouteRoot) {
  throw 'Control-route qualification requires a fresh owned evidence directory'
}
$nyxRouteTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
foreach ($nyxRoutePart in @('core','studio','runtime','web','driver')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxRouteRoot $nyxRoutePart) | Out-Null
}
function Invoke-RouteTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Control-route qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxRouteFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxRouteLCL = $nyxRouteTools.LAZARUS
$nyxRoutePlatform = "$((& $nyxRouteTools.LCL_FPC '-iTP').Trim())-$((& $nyxRouteTools.LCL_FPC '-iTO').Trim())"
$nyxRouteNativeFlags = $nyxRouteFlags + @(
  "-Fu$nyxRouteLCL/lcl/units/$nyxRoutePlatform", "-Fu$nyxRouteLCL/lcl/units/$nyxRoutePlatform/$Widgetset",
  "-Fu$nyxRouteLCL/components/lazutils/lib/$nyxRoutePlatform",
  "-Fu$nyxRouteLCL/packager/units/$nyxRoutePlatform")
$nyxRouteCore = Join-Path $nyxRouteRoot 'core'
$nyxRouteStudio = Join-Path $nyxRouteRoot 'studio'
$nyxRouteRuntime = Join-Path $nyxRouteRoot 'runtime'
$nyxRouteWeb = Join-Path $nyxRouteRoot 'web'
$nyxRouteDriver = Join-Path $nyxRouteRoot 'driver'
$nyxRouteProject = Join-Path $nyxRouteRoot 'native-project'
$nyxRoutePublished = Join-Path $nyxRouteProject 'published'
Invoke-RouteTool $nyxRouteTools.FPC ($nyxRouteFlags + @(
  "-FU$nyxRouteCore","-FE$nyxRouteCore",'tests/nyx_control_routes_tests.lpr')) (
  Join-Path $nyxRouteRoot 'core-compile.log')
Invoke-RouteTool (Join-Path $nyxRouteCore 'nyx_control_routes_tests.exe') @() (
  Join-Path $nyxRouteRoot 'core-run.log')
Invoke-RouteTool $nyxRouteTools.LCL_FPC ($nyxRouteNativeFlags + @(
  '-dNYX_CONTROL_ROUTES_STUDIO',"-FU$nyxRouteStudio","-FE$nyxRouteStudio",
  'tests/nyx_program_studio_controls.lpr')) (Join-Path $nyxRouteRoot 'studio-compile.log')
Invoke-RouteTool (Join-Path $nyxRouteStudio 'nyx_program_studio_controls.exe') @(
  $nyxRouteProject,(Join-Path $nyxRouteRoot 'native-form.png')) (
  Join-Path $nyxRouteRoot 'studio-run.log')
Invoke-RouteTool $nyxRouteTools.LCL_FPC ($nyxRouteNativeFlags + @(
  '-dNYX_CONTROL_ROUTES_RUNTIME',"-Fu$nyxRoutePublished","-FU$nyxRouteRuntime",
  "-FE$nyxRouteRuntime",'tests/nyx_program_logic_controls.lpr')) (
  Join-Path $nyxRouteRoot 'runtime-compile.log')
Invoke-RouteTool (Join-Path $nyxRouteRuntime 'nyx_program_logic_controls.exe') @() (
  Join-Path $nyxRouteRoot 'runtime-run.log')
$nyxRouteBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js','-Fusrc',
  '-Fustudio','-Futests','-Fisrc',"-FE$nyxRouteWeb")
Invoke-RouteTool $nyxRouteTools.PAS2JS ($nyxRouteBrowserFlags + @(
  'tests/nyx_control_routes_tests.lpr')) (Join-Path $nyxRouteRoot 'core-browser-compile.log')
Invoke-RouteTool $nyxRouteTools.PAS2JS ($nyxRouteBrowserFlags + @(
  '-dNYX_CONTROL_ROUTES_STUDIO','tests/nyx_program_studio_controls.lpr')) (
  Join-Path $nyxRouteRoot 'studio-browser-compile.log')
Invoke-RouteTool $nyxRouteTools.PAS2JS ($nyxRouteBrowserFlags + @(
  '-dNYX_CONTROL_ROUTES_RUNTIME',"-Fu$nyxRoutePublished",
  'tests/nyx_program_logic_controls.lpr')) (Join-Path $nyxRouteRoot 'runtime-browser-compile.log')
Invoke-RouteTool $nyxRouteTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$nyxRouteWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $nyxRouteRoot 'worker-compile.log')
Invoke-RouteTool $nyxRouteTools.FPC ($nyxRouteFlags + @(
  "-FU$nyxRouteDriver","-FE$nyxRouteDriver",'tests/nyx_browser_ready_capture.lpr')) (
  Join-Path $nyxRouteRoot 'driver-compile.log')
Copy-Item -LiteralPath $nyxRouteTools.PAS2JS_RUNTIME,'studio/web/control-routes.html',
  'studio/web/program-studio.html','studio/web/program-logic-controls.html' -Destination $nyxRouteWeb
Write-Host 'Native contract/Studio/generated runtime passed. Browser artifacts staged for real HTTP execution.'
Write-Host 'Serve only this owned web directory; use its Pascal ready driver at desktop/narrow widths.'
Write-Host 'Studio marker: data-program-studio; optional --control-route-export retains exact UI-authored bytes.'
