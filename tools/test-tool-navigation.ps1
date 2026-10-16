# Nyx graph-aware canvas navigation qualification.
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

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$Widgetset = 'win32',
  [switch]$Products,
  [switch]$Regressions
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxNavRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxNavRoot) {
  throw 'Navigation qualification requires a fresh owned evidence directory'
}
$nyxNavTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$nyxNavNative = Join-Path $nyxNavRoot 'native'
$nyxNavWeb = Join-Path $nyxNavRoot 'web'
New-Item -ItemType Directory -Force $nyxNavNative,$nyxNavWeb | Out-Null

# Pascal owns geometry, actual input, revision/worker refusal and retirement.
# This shell only orchestrates tools and stages browser payloads. No HTTP host,
# installed server, protected checkpoint or user project is touched.
function Invoke-NavigationTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -TotalCount 25
    Get-Content -LiteralPath $Log -Tail 10
    throw "Navigation qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxNavFlags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FU$nyxNavNative","-FE$nyxNavNative")
Invoke-NavigationTool $nyxNavTools.FPC ($nyxNavFlags + @(
  'tests/nyx_canvas_navigation_tests.lpr')) (Join-Path $nyxNavRoot 'pure-compile.log')
Invoke-NavigationTool (Join-Path $nyxNavNative 'nyx_canvas_navigation_tests.exe') @() (
  Join-Path $nyxNavRoot 'pure-run.log')
$nyxNavLCL = $nyxNavTools.LAZARUS
$nyxNavPlatform = "$((& $nyxNavTools.LCL_FPC '-iTP').Trim())-$((& $nyxNavTools.LCL_FPC '-iTO').Trim())"
$nyxNavLCLFlags = $nyxNavFlags + @(
  "-Fu$nyxNavLCL/lcl/units/$nyxNavPlatform",
  "-Fu$nyxNavLCL/lcl/units/$nyxNavPlatform/$Widgetset",
  "-Fu$nyxNavLCL/components/lazutils/lib/$nyxNavPlatform",
  "-Fu$nyxNavLCL/packager/units/$nyxNavPlatform")
Invoke-NavigationTool $nyxNavTools.LCL_FPC ($nyxNavLCLFlags + @(
  'tests/nyx_tool_canvas_navigation_controls.lpr')) (Join-Path $nyxNavRoot 'input-compile.log')
foreach ($nyxNavWidth in @('wide','compact')) {
  foreach ($nyxNavScene in @('empty','mixed')) {
    $nyxNavCase = "$nyxNavWidth-$nyxNavScene"
    Invoke-NavigationTool (Join-Path $nyxNavNative 'nyx_tool_canvas_navigation_controls.exe') @(
      (Join-Path $nyxNavRoot "$nyxNavCase.png"),$nyxNavWidth,$nyxNavScene) (
      Join-Path $nyxNavRoot "$nyxNavCase.log")
  }
}
$nyxNavBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxNavWeb")
$nyxNavTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
foreach ($nyxNavConsumer in @('nyx_canvas_navigation_tests','nyx_tool_canvas_navigation_controls')) {
  Invoke-NavigationTool $nyxNavTools.PAS2JS ($nyxNavBrowserFlags + @(
    "tests/$nyxNavConsumer.lpr")) (Join-Path $nyxNavRoot "$nyxNavConsumer-browser-compile.log")
  [IO.File]::WriteAllText((Join-Path $nyxNavWeb "$nyxNavConsumer.html"),
    $nyxNavTemplate.Replace('nyx_program_logic_controls.js',"$nyxNavConsumer.js"),
    [Text.UTF8Encoding]::new($false))
}
Copy-Item -LiteralPath $nyxNavTools.PAS2JS_RUNTIME -Destination (Join-Path $nyxNavWeb 'rtl.js')
if ($Products) {
  Invoke-NavigationTool $nyxNavTools.LCL_FPC ($nyxNavLCLFlags + @(
    'studio/nyx_studio_native.lpr')) (Join-Path $nyxNavRoot 'studio-native-compile.log')
  Invoke-NavigationTool $nyxNavTools.LCL_FPC ($nyxNavLCLFlags + @(
    'studio/nyx_studio_server.lpr')) (Join-Path $nyxNavRoot 'server-compile.log')
  Invoke-NavigationTool $nyxNavTools.PAS2JS ($nyxNavBrowserFlags + @(
    'studio/nyx_studio.lpr')) (Join-Path $nyxNavRoot 'studio-browser-compile.log')
  Invoke-NavigationTool $nyxNavTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
    '-Fusrc','-Fustudio','-Fisrc',"-FE$nyxNavWeb",'studio/nyx_source_worker.lpr') (
    Join-Path $nyxNavRoot 'worker-compile.log')
}
Invoke-NavigationTool $nyxNavTools.LCL_FPC ($nyxNavLCLFlags + @(
  'tests/nyx_canvas_workspace_controls.lpr')) (Join-Path $nyxNavRoot 'workspace-compile.log')
Invoke-NavigationTool (Join-Path $nyxNavNative 'nyx_canvas_workspace_controls.exe') @() (
  Join-Path $nyxNavRoot 'workspace-run.log')
Invoke-NavigationTool $nyxNavTools.PAS2JS ($nyxNavBrowserFlags + @(
  'tests/nyx_canvas_workspace_controls.lpr')) (Join-Path $nyxNavRoot 'workspace-browser-compile.log')
if ($Regressions) {
  & (Join-Path $PSScriptRoot 'test-canvas-routes.ps1') -InternalConnections -Widgetset $Widgetset -EvidenceDirectory (
    Join-Path $nyxNavRoot 'routes')
}
Write-Host 'Portable/native wide/compact navigation passed; browser payloads compiled and staged only.'
