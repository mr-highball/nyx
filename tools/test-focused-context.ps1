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
  [string]$MenuSourceDirectory='build/menu-bar/source',
  [switch]$CompilerJobs
)
$ErrorActionPreference='Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$focusRoot=[IO.Path]::GetFullPath($EvidenceDirectory)
$focusMenuOriginal=Join-Path ([IO.Path]::GetFullPath($MenuSourceDirectory)) 'nyx.generated.view.pas'
if (-not (Test-Path -LiteralPath $focusMenuOriginal)) {
  throw 'Supply the existing MCP-exported menu fixture with -MenuSourceDirectory'
}
if (Test-Path -LiteralPath $focusRoot) {
  throw 'Focused context qualification requires fresh owned evidence'
}
New-Item -ItemType Directory -Path $focusRoot | Out-Null
$focusMenuSource=Join-Path $focusRoot 'menu-source'
New-Item -ItemType Directory -Path $focusMenuSource | Out-Null
Copy-Item -LiteralPath $focusMenuOriginal -Destination $focusMenuSource
$focusTools=Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$focusLazarus=$focusTools.LAZARUS
$focusPlatform="$((& $focusTools.LCL_FPC '-iTP').Trim())-$((& $focusTools.LCL_FPC '-iTO').Trim())"
$focusFlags=@('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$focusLCLFlags=$focusFlags + @(
  "-Fu$focusLazarus/lcl/units/$focusPlatform",
  "-Fu$focusLazarus/lcl/units/$focusPlatform/win32",
  "-Fu$focusLazarus/components/lazutils/lib/$focusPlatform",
  "-Fu$focusLazarus/packager/units/$focusPlatform")
$focusWeb=Join-Path $focusRoot 'web'
New-Item -ItemType Directory -Path $focusWeb | Out-Null
$focusBrowserFlags=@('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$focusWeb")

# Pascal owns all source, ownership, compiler and actual input assertions.
# This runner only orchestrates working tools and fresh owned fixtures.
# It starts no HTTP service, refreshes no authority and captures no foreground.
function Invoke-FocusTool([string]$Executable,[string[]]$Arguments,[string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 20
    throw "Focused qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native heap retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetRelativePath($focusRoot,$Log))"
}
foreach ($focusTest in @('nyx_focused_context_tests','nyx_content_tests',
    'nyx_menu_declarations_tests','nyx_menu_bar_declarations_tests',
    'nyx_diagram_document_tests','nyx_logic_document_tests','nyx_collection_authoring_tests')) {
  $focusOut=Join-Path $focusRoot $focusTest
  New-Item -ItemType Directory -Path $focusOut | Out-Null
  $focusTestFlags=$focusFlags
  $focusTestCompiler=$focusTools.FPC
  $focusTestBrowser=$focusBrowserFlags
  if ($focusTest -eq 'nyx_collection_authoring_tests') {
    $focusTestFlags=$focusLCLFlags
    $focusTestCompiler=$focusTools.LCL_FPC
  }
  if ($focusTest -eq 'nyx_menu_bar_declarations_tests') {
    $focusTestFlags += "-Fu$focusMenuSource"
    $focusTestBrowser += "-Fu$focusMenuSource"
  }
  Invoke-FocusTool $focusTestCompiler ($focusTestFlags + @(
    "-FU$focusOut","-FE$focusOut","tests/$focusTest.lpr")) (Join-Path $focusOut 'compile.log')
  $focusArguments=@()
  if ($focusTest -eq 'nyx_focused_context_tests') {
    $focusArguments=@((Join-Path $focusOut 'exports'))
  }
  Invoke-FocusTool (Join-Path $focusOut "$focusTest.exe") $focusArguments (Join-Path $focusOut 'run.log')
  Invoke-FocusTool $focusTools.PAS2JS ($focusTestBrowser + @(
    "tests/$focusTest.lpr")) (Join-Path $focusOut 'browser-compile.log')
}
$focusOut=Join-Path $focusRoot 'controls'
New-Item -ItemType Directory -Path $focusOut | Out-Null
Invoke-FocusTool $focusTools.LCL_FPC ($focusLCLFlags + @(
  "-FU$focusOut","-FE$focusOut",'tests/nyx_focused_context_controls.lpr')) (
  Join-Path $focusOut 'compile.log')
foreach ($focusWidth in @('wide','compact')) {
  Invoke-FocusTool (Join-Path $focusOut 'nyx_focused_context_controls.exe') @("--$focusWidth") (
    Join-Path $focusOut "$focusWidth-run.log")
}
Invoke-FocusTool $focusTools.PAS2JS ($focusBrowserFlags + @(
  'tests/nyx_focused_context_controls.lpr')) (Join-Path $focusOut 'browser-compile.log')
if ($CompilerJobs) {
  $focusOut=Join-Path $focusRoot 'compiler'
  New-Item -ItemType Directory -Path $focusOut | Out-Null
  Invoke-FocusTool $focusTools.FPC ($focusFlags + @(
    "-FU$focusOut","-FE$focusOut",'tests/nyx_focused_context_builds.lpr')) (
    Join-Path $focusOut 'compile.log')
  Invoke-FocusTool (Join-Path $focusOut 'nyx_focused_context_builds.exe') @(
    (Get-Location).Path,(Join-Path (Get-Location).Path '.local/toolchain.json'),
    (Join-Path $focusOut 'runtime'),$focusPlatform) (Join-Path $focusOut 'run.log')
}
# Whole constructors verify exact complete design bytes; they do not claim
# application interaction. Browser counterparts below compile without execution.
foreach ($focusCase in @('home','Wrapper','focused-memo','nested-canvas','unicode')) {
  $focusSource=Join-Path $focusRoot "nyx_focused_context_tests/exports/$focusCase"
  $focusNative=Join-Path $focusRoot "companions/$focusCase/native"
  New-Item -ItemType Directory -Path $focusNative -Force | Out-Null
  Invoke-FocusTool $focusTools.FPC ($focusFlags + @(
    "-Fu$focusSource","-FU$focusNative","-FE$focusNative",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $focusNative 'compile.log')
  Invoke-FocusTool (Join-Path $focusNative 'nyx_design_source_consumer.exe') @(
    (Join-Path $focusSource 'expected.nyx')) (Join-Path $focusNative 'run.log')
  $focusBrowser=Join-Path $focusRoot "companions/$focusCase/browser"
  New-Item -ItemType Directory -Path $focusBrowser -Force | Out-Null
  Invoke-FocusTool $focusTools.PAS2JS ($focusBrowserFlags + @(
    "-Fu$focusSource","-FE$focusBrowser",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $focusBrowser 'compile.log')
}
$focusNative=Join-Path $focusRoot 'products'
New-Item -ItemType Directory -Path $focusNative | Out-Null
Invoke-FocusTool $focusTools.LCL_FPC ($focusLCLFlags + @(
  "-FU$focusNative","-FE$focusNative",'studio/nyx_studio_native.lpr')) (
  Join-Path $focusNative 'studio-native-compile.log')
Invoke-FocusTool $focusTools.FPC ($focusFlags + @(
  "-FU$focusNative","-FE$focusNative",'studio/nyx_studio_server.lpr')) (
  Join-Path $focusNative 'server-compile.log')
Invoke-FocusTool $focusTools.PAS2JS ($focusBrowserFlags + @('studio/nyx_studio.lpr')) (
  Join-Path $focusNative 'studio-browser-compile.log')
Invoke-FocusTool $focusTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$focusWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $focusNative 'worker-compile.log')
Copy-Item -LiteralPath $focusTools.PAS2JS_RUNTIME -Destination (Join-Path $focusWeb 'rtl.js')
Write-Host 'Focused contexts: native input/constructors pass; browser compiled only'
