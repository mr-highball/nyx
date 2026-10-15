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
# Orchestration only: Pascal owns every semantic/control assertion. This command
# starts no server and requires an independently preflighted static fixture host.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxCanvasRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxCanvasRoot) { throw 'Use a fresh owned evidence directory.' }
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the preflighted static directory and builds URL together.'
}
$nyxCanvasTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxCanvasPart in @('core','native','web','driver')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxCanvasRoot $nyxCanvasPart) | Out-Null
}
function Invoke-CanvasTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Canvas program qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxCanvasFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxCanvasCore = Join-Path $nyxCanvasRoot 'core'
foreach ($nyxCanvasCase in @('nyx_canvas_scene_tests','nyx_program_canvas_tests')) {
  Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
    "-FU$nyxCanvasCore","-FE$nyxCanvasCore","tests/$nyxCanvasCase.lpr")) (Join-Path $nyxCanvasRoot "$nyxCanvasCase-compile.log")
  Invoke-CanvasTool (Join-Path $nyxCanvasCore "$nyxCanvasCase.exe") @() (Join-Path $nyxCanvasRoot "$nyxCanvasCase-run.log")
}
$nyxCanvasNative = Join-Path $nyxCanvasRoot 'native'
$nyxCanvasLCL = $nyxCanvasTools.LAZARUS
Invoke-CanvasTool $nyxCanvasTools.LCL_FPC ($nyxCanvasFlags + @(
  '-dNYX_CANVAS_PROGRAM_TESTS', "-Fu$nyxCanvasLCL/lcl/units/i386-win32",
  "-Fu$nyxCanvasLCL/lcl/units/i386-win32/win32",
  "-Fu$nyxCanvasLCL/components/lazutils/lib/i386-win32",
  "-Fu$nyxCanvasLCL/packager/units/i386-win32", "-FU$nyxCanvasNative","-FE$nyxCanvasNative",
  'tests/nyx_canvas_workspace_controls.lpr')) (Join-Path $nyxCanvasRoot 'native-compile.log')
Invoke-CanvasTool (Join-Path $nyxCanvasNative 'nyx_canvas_workspace_controls.exe') @(
  (Join-Path $nyxCanvasRoot 'canvas-program.png')) (Join-Path $nyxCanvasRoot 'native-run.log')
$nyxCanvasWeb = Join-Path $nyxCanvasRoot 'web'
foreach ($nyxCanvasCase in @('nyx_canvas_scene_tests','nyx_program_canvas_tests','nyx_canvas_workspace_controls')) {
  Invoke-CanvasTool $nyxCanvasTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-dNYX_CANVAS_PROGRAM_TESTS','-Fusrc','-Fustudio','-Futests','-Fisrc',
    "-FE$nyxCanvasWeb","tests/$nyxCanvasCase.lpr") (Join-Path $nyxCanvasRoot "$nyxCanvasCase-browser-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native canvas program/input passes; browser compilation only.'
  return
}
$nyxCanvasDriver = Join-Path $nyxCanvasRoot 'driver'
Invoke-CanvasTool $nyxCanvasTools.FPC ($nyxCanvasFlags + @(
  "-FU$nyxCanvasDriver","-FE$nyxCanvasDriver",'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxCanvasRoot 'driver-compile.log')
$nyxCanvasChild = 'nyx-canvas-programs-' + [guid]::NewGuid().ToString('N')
$nyxCanvasStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxCanvasChild
New-Item -ItemType Directory $nyxCanvasStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxCanvasWeb 'nyx_canvas_scene_tests.js'),
  (Join-Path $nyxCanvasWeb 'nyx_program_canvas_tests.js'),
  (Join-Path $nyxCanvasWeb 'nyx_canvas_workspace_controls.js'),
  'studio/web/canvas-scene.html','studio/web/program-canvas-tests.html',
  'studio/web/canvas-workspace.html',$nyxCanvasTools.PAS2JS_RUNTIME -Destination $nyxCanvasStage
$nyxCanvasURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxCanvasChild/"
$nyxCanvasURL | Set-Content -LiteralPath (Join-Path $nyxCanvasRoot 'static-url.txt') -Encoding utf8
foreach ($nyxCanvasWidth in @(1280,390)) {
  foreach ($nyxCanvasCase in @(
    @('scene','canvas-scene.html','data-canvas-scene'),
    @('program','program-canvas-tests.html','data-program-canvas'),
    @('controls','canvas-workspace.html','data-canvas-workspace'))) {
    Invoke-CanvasTool (Join-Path $nyxCanvasDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxCanvasURL+$nyxCanvasCase[1]),
      (Join-Path $nyxCanvasRoot "$($nyxCanvasCase[0])-$nyxCanvasWidth"),
      $nyxCanvasCase[2],[string]$nyxCanvasWidth,'900','--fixture-timeout=180') (Join-Path $nyxCanvasRoot "$($nyxCanvasCase[0])-$nyxCanvasWidth.log")
  }
}
Write-Host 'Native and actual HTTP browser canvas program/control publication pass.'
