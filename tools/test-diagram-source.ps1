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
$sourceRoot=[IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $sourceRoot) {
  throw 'Diagram source qualification requires fresh owned evidence'
}
New-Item -ItemType Directory -Path $sourceRoot | Out-Null
$sourceTools=Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$sourceLazarus=$sourceTools.LAZARUS
$sourcePlatform="$((& $sourceTools.LCL_FPC '-iTP').Trim())-$((& $sourceTools.LCL_FPC '-iTO').Trim())"
$sourceFixtures=Join-Path $sourceRoot 'fixtures'
New-Item -ItemType Directory -Path $sourceFixtures | Out-Null
$sourceFlags=@('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-Fu$sourceFixtures")
$sourceLCLFlags=$sourceFlags + @(
  "-Fu$sourceLazarus/lcl/units/$sourcePlatform",
  "-Fu$sourceLazarus/lcl/units/$sourcePlatform/$Widgetset",
  "-Fu$sourceLazarus/components/lazutils/lib/$sourcePlatform",
  "-Fu$sourceLazarus/packager/units/$sourcePlatform")
$sourceWeb=Join-Path $sourceRoot 'web'
New-Item -ItemType Directory -Path $sourceWeb | Out-Null
$sourceBrowserFlags=@('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-Fu$sourceFixtures","-FE$sourceWeb")

# Pascal owns the fixtures, source comparisons and real control assertions.
# This runner only orchestrates local tools. No HTTP host, enrollment, installed
# project or foreground-capture workaround is part of this qualification.
function Invoke-SourceTool([string]$Executable,[string[]]$Arguments,[string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 20
    throw "Diagram source qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native heap retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
Invoke-SourceTool $sourceTools.FPC ($sourceFlags + @(
  "-FU$sourceFixtures","-FE$sourceFixtures",'tests/nyx_image_fixtures.lpr')) (
  Join-Path $sourceFixtures 'compile.log')
Invoke-SourceTool (Join-Path $sourceFixtures 'nyx_image_fixtures.exe') @($sourceFixtures) (
  Join-Path $sourceFixtures 'run.log')
foreach ($sourceTest in @('nyx_diagram_source_tests','nyx_canvas_content_tests',
    'nyx_diagram_content_tests')) {
  $sourceOut=Join-Path $sourceRoot $sourceTest
  New-Item -ItemType Directory -Path $sourceOut | Out-Null
  Invoke-SourceTool $sourceTools.FPC ($sourceFlags + @(
    "-FU$sourceOut","-FE$sourceOut","tests/$sourceTest.lpr")) (
    Join-Path $sourceOut 'compile.log')
  $sourceArguments=@()
  if ($sourceTest -eq 'nyx_diagram_source_tests') {
    $sourceArguments=@((Join-Path $sourceOut 'exports'))
  }
  Invoke-SourceTool (Join-Path $sourceOut "$sourceTest.exe") $sourceArguments (
    Join-Path $sourceOut 'run.log')
  Invoke-SourceTool $sourceTools.PAS2JS ($sourceBrowserFlags + @(
    "tests/$sourceTest.lpr")) (Join-Path $sourceOut 'browser-compile.log')
}
# Serialize actual native control journeys. Input-only deliberately adds no
# foreground/display claim; the ordinary strict capture path remains unchanged.
foreach ($sourceTest in @('nyx_diagram_content_controls','nyx_diagram_content_studio_controls')) {
  $sourceOut=Join-Path $sourceRoot $sourceTest
  New-Item -ItemType Directory -Path $sourceOut | Out-Null
  Invoke-SourceTool $sourceTools.LCL_FPC ($sourceLCLFlags + @(
    "-FU$sourceOut","-FE$sourceOut","tests/$sourceTest.lpr")) (
    Join-Path $sourceOut 'compile.log')
  foreach ($sourceWidth in @('wide','compact')) {
    $sourceArguments=@('--input-only',"--$sourceWidth")
    if ($sourceTest -eq 'nyx_diagram_content_studio_controls') {
      $sourceArguments=@((Join-Path $sourceOut "fixture-$sourceWidth"),
        "--$sourceWidth",'--input-only')
    }
    Invoke-SourceTool (Join-Path $sourceOut "$sourceTest.exe") $sourceArguments (
      Join-Path $sourceOut "$sourceWidth-run.log")
  }
  Invoke-SourceTool $sourceTools.PAS2JS ($sourceBrowserFlags + @(
    "tests/$sourceTest.lpr")) (Join-Path $sourceOut 'browser-compile.log')
}
# Exact complete constructors qualify source fidelity, separately from executing
# the resulting application UI. Browser companions below are compiled only.
$sourceCases=@(
  (Join-Path $sourceRoot 'nyx_diagram_source_tests/exports/original'),
  (Join-Path $sourceRoot 'nyx_diagram_source_tests/exports/changed'))
foreach ($sourceWidth in @('wide','compact')) {
  foreach ($sourceStage in @('original','reusable','hosted','source')) {
    $sourceCases += Join-Path $sourceRoot (
      "nyx_diagram_content_studio_controls/fixture-$sourceWidth/$sourceStage")
  }
}
$sourceIndex=0
foreach ($sourceCase in $sourceCases) {
  $sourceIndex++
  $sourceNative=Join-Path $sourceRoot "companions/$sourceIndex/native"
  New-Item -ItemType Directory -Path $sourceNative -Force | Out-Null
  Invoke-SourceTool $sourceTools.FPC ($sourceFlags + @(
    "-Fu$sourceCase","-FU$sourceNative","-FE$sourceNative",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $sourceNative 'compile.log')
  Invoke-SourceTool (Join-Path $sourceNative 'nyx_design_source_consumer.exe') @(
    (Join-Path $sourceCase 'expected.nyx')) (Join-Path $sourceNative 'run.log')
  $sourceBrowser=Join-Path $sourceRoot "companions/$sourceIndex/browser"
  New-Item -ItemType Directory -Path $sourceBrowser -Force | Out-Null
  Invoke-SourceTool $sourceTools.PAS2JS ($sourceBrowserFlags + @(
    "-Fu$sourceCase","-FE$sourceBrowser",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $sourceBrowser 'compile.log')
}
$sourceNative=Join-Path $sourceRoot 'products'
New-Item -ItemType Directory -Path $sourceNative | Out-Null
Invoke-SourceTool $sourceTools.LCL_FPC ($sourceLCLFlags + @(
  "-FU$sourceNative","-FE$sourceNative",'studio/nyx_studio_native.lpr')) (
  Join-Path $sourceNative 'studio-native-compile.log')
Invoke-SourceTool $sourceTools.FPC ($sourceFlags + @(
  "-FU$sourceNative","-FE$sourceNative",'studio/nyx_studio_server.lpr')) (
  Join-Path $sourceNative 'server-compile.log')
Invoke-SourceTool $sourceTools.PAS2JS ($sourceBrowserFlags + @(
  'studio/nyx_studio.lpr')) (Join-Path $sourceNative 'studio-browser-compile.log')
Invoke-SourceTool $sourceTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$sourceWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $sourceNative 'worker-compile.log')
Copy-Item -LiteralPath $sourceTools.PAS2JS_RUNTIME -Destination (Join-Path $sourceWeb 'rtl.js')
Write-Host 'Diagram source: native input and exact constructors passed; browser compiled only'
