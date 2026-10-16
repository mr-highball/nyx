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
  [string]$Widgetset='win32'
)
$ErrorActionPreference='Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$boundaryRoot=[IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $boundaryRoot) {
  throw 'Canvas boundary qualification requires a fresh owned evidence directory'
}
$boundaryTools=Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$boundaryCore=Join-Path $boundaryRoot 'core'
$boundaryControls=Join-Path $boundaryRoot 'controls'
$boundaryWeb=Join-Path $boundaryRoot 'web'
$boundaryExports=Join-Path $boundaryRoot 'exports'
New-Item -ItemType Directory -Force $boundaryCore,$boundaryControls,$boundaryWeb | Out-Null

# Pascal owns all assertions, physical input, source admission and whole-pair
# comparison. This runner only orchestrates compilers and owned consumers.
# It starts no server, changes no enrollment and claims no browser execution.
function Invoke-BoundaryTool([string]$Executable,[string[]]$Arguments,[string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Canvas boundary qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained heap allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$boundaryFlags=@('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
Invoke-BoundaryTool $boundaryTools.FPC ($boundaryFlags + @(
  "-FU$boundaryCore","-FE$boundaryCore",'tests/nyx_canvas_boundaries_tests.lpr')) (
  Join-Path $boundaryRoot 'contracts-native-compile.log')
Invoke-BoundaryTool (Join-Path $boundaryCore 'nyx_canvas_boundaries_tests.exe') @(
  $boundaryExports) (Join-Path $boundaryRoot 'contracts-native-run.log')

$boundaryLazarus=$boundaryTools.LAZARUS
$boundaryPlatform="$((& $boundaryTools.LCL_FPC '-iTP').Trim())-$((& $boundaryTools.LCL_FPC '-iTO').Trim())"
$boundaryLCLFlags=$boundaryFlags + @(
  "-Fu$boundaryLazarus/lcl/units/$boundaryPlatform",
  "-Fu$boundaryLazarus/lcl/units/$boundaryPlatform/$Widgetset",
  "-Fu$boundaryLazarus/components/lazutils/lib/$boundaryPlatform",
  "-Fu$boundaryLazarus/packager/units/$boundaryPlatform",
  "-Fu$boundaryExports/original","-FU$boundaryControls","-FE$boundaryControls")
Invoke-BoundaryTool $boundaryTools.LCL_FPC ($boundaryLCLFlags + @(
  'tests/nyx_canvas_boundaries_controls.lpr')) (Join-Path $boundaryRoot 'controls-native-compile.log')
Invoke-BoundaryTool (Join-Path $boundaryControls 'nyx_canvas_boundaries_controls.exe') @(
  (Join-Path $boundaryRoot 'studio-wide')) (Join-Path $boundaryRoot 'controls-wide-run.log')
Invoke-BoundaryTool (Join-Path $boundaryControls 'nyx_canvas_boundaries_controls.exe') @(
  (Join-Path $boundaryRoot 'studio-compact'),'--compact') (
  Join-Path $boundaryRoot 'controls-compact-run.log')

# Preserve the previously qualified unbounded geometry, focus-navigation and
# behavior-program contracts; these consumers do not create application hosts.
foreach ($boundaryTest in @('nyx_canvas_geometry_tests','nyx_canvas_navigation_tests',
    'nyx_program_canvas_tests')) {
  $boundaryRegression=Join-Path $boundaryRoot $boundaryTest
  New-Item -ItemType Directory -Path $boundaryRegression | Out-Null
  Invoke-BoundaryTool $boundaryTools.FPC ($boundaryFlags + @(
    "-FU$boundaryRegression","-FE$boundaryRegression","tests/$boundaryTest.lpr")) (
    Join-Path $boundaryRegression 'compile.log')
  Invoke-BoundaryTool (Join-Path $boundaryRegression "$boundaryTest.exe") @() (
    Join-Path $boundaryRegression 'run.log')
}
$boundaryBrowserFlags=@('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$boundaryWeb")
foreach ($boundaryTest in @('nyx_canvas_boundaries_tests','nyx_canvas_boundaries_controls',
    'nyx_canvas_geometry_tests','nyx_canvas_navigation_tests','nyx_program_canvas_tests')) {
  Invoke-BoundaryTool $boundaryTools.PAS2JS ($boundaryBrowserFlags + @(
    "-Fu$boundaryExports/original","tests/$boundaryTest.lpr")) (
    Join-Path $boundaryRoot "$boundaryTest-browser-compile.log")
}

# Each exact emitted companion uses its own compiler output/search directory.
# Construction is compared to the entire expected design, never a few fields.
foreach ($boundaryCase in @(
    @{Name='original';Source=(Join-Path $boundaryExports 'original')},
    @{Name='center';Source=(Join-Path $boundaryExports 'center')},
    @{Name='studio-wide';Source=(Join-Path $boundaryRoot 'studio-wide/exports/applied')},
    @{Name='studio-compact';Source=(Join-Path $boundaryRoot 'studio-compact/exports/applied')})) {
  $boundarySource=$boundaryCase.Source
  $boundaryNative=Join-Path $boundaryRoot ("companions/" + $boundaryCase.Name + '/native')
  $boundaryBrowser=Join-Path $boundaryRoot ("companions/" + $boundaryCase.Name + '/browser')
  New-Item -ItemType Directory -Force $boundaryNative,$boundaryBrowser | Out-Null
  Invoke-BoundaryTool $boundaryTools.FPC ($boundaryFlags + @(
    "-Fu$boundarySource","-FU$boundaryNative","-FE$boundaryNative",
    'tests/nyx_design_source_consumer.lpr')) (Join-Path $boundaryNative 'compile.log')
  Invoke-BoundaryTool (Join-Path $boundaryNative 'nyx_design_source_consumer.exe') @(
    (Join-Path $boundarySource 'expected.nyx')) (Join-Path $boundaryNative 'run.log')
  Invoke-BoundaryTool $boundaryTools.PAS2JS ($boundaryBrowserFlags + @(
    "-Fu$boundarySource","-FE$boundaryBrowser",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $boundaryBrowser 'compile.log')
}
Copy-Item -LiteralPath $boundaryTools.PAS2JS_RUNTIME -Destination (Join-Path $boundaryWeb 'rtl.js')
Write-Host 'Canvas boundaries: native checks and exact constructors passed; browser compiled only'
