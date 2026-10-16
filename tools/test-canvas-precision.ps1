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
$precisionRoot=[IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $precisionRoot) {
  throw 'Precision qualification requires a fresh owned evidence directory'
}
New-Item -ItemType Directory -Path $precisionRoot | Out-Null
$precisionTools=Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$precisionLazarus=$precisionTools.LAZARUS
$precisionPlatform="$((& $precisionTools.LCL_FPC '-iTP').Trim())-$((& $precisionTools.LCL_FPC '-iTO').Trim())"
$precisionFlags=@('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$precisionLCLFlags=$precisionFlags + @(
  "-Fu$precisionLazarus/lcl/units/$precisionPlatform",
  "-Fu$precisionLazarus/lcl/units/$precisionPlatform/$Widgetset",
  "-Fu$precisionLazarus/components/lazutils/lib/$precisionPlatform",
  "-Fu$precisionLazarus/packager/units/$precisionPlatform")
$precisionWeb=Join-Path $precisionRoot 'web'
New-Item -ItemType Directory -Path $precisionWeb | Out-Null
$precisionBrowserFlags=@('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$precisionWeb")

# Pascal owns admission, exact pairs/history and installed input assertions.
# This runner only orchestrates working compilers and explicitly owned fixtures;
# it never launches a server, refreshes authority or touches live project files.
function Invoke-PrecisionTool([string]$Executable,[string[]]$Arguments,[string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 20
    throw "Precision qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native heap retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
foreach ($precisionTest in @('nyx_canvas_precision_tests','nyx_canvas_precision_editor_tests',
    'nyx_canvas_precision_source_tests','nyx_canvas_boundaries_tests',
    'nyx_canvas_geometry_tests','nyx_canvas_navigation_tests','nyx_program_canvas_tests')) {
  $precisionOut=Join-Path $precisionRoot $precisionTest
  New-Item -ItemType Directory -Path $precisionOut | Out-Null
  Invoke-PrecisionTool $precisionTools.FPC ($precisionFlags + @(
    "-FU$precisionOut","-FE$precisionOut","tests/$precisionTest.lpr")) (
    Join-Path $precisionOut 'compile.log')
  $precisionArguments=@()
  if ($precisionTest -eq 'nyx_canvas_precision_source_tests') {
    $precisionArguments=@((Join-Path $precisionOut 'exports'))
  }
  Invoke-PrecisionTool (Join-Path $precisionOut "$precisionTest.exe") $precisionArguments (
    Join-Path $precisionOut 'run.log')
  Invoke-PrecisionTool $precisionTools.PAS2JS ($precisionBrowserFlags + @(
    "tests/$precisionTest.lpr")) (Join-Path $precisionOut 'browser-compile.log')
}
# Serialize real native GUI consumers, preserving each fresh host/profile.
foreach ($precisionTest in @('nyx_canvas_precision_controls','nyx_canvas_precision_studio')) {
  $precisionOut=Join-Path $precisionRoot $precisionTest
  New-Item -ItemType Directory -Path $precisionOut | Out-Null
  Invoke-PrecisionTool $precisionTools.LCL_FPC ($precisionLCLFlags + @(
    "-FU$precisionOut","-FE$precisionOut","tests/$precisionTest.lpr")) (
    Join-Path $precisionOut 'compile.log')
  foreach ($precisionWidth in @('wide','compact')) {
    $precisionArguments=@((Join-Path $precisionOut "fixture-$precisionWidth"))
    if ($precisionTest -eq 'nyx_canvas_precision_controls') {
      $precisionArguments=@('--wide',(Join-Path $precisionOut "$precisionWidth.png"))
      if ($precisionWidth -eq 'compact') {
        $precisionArguments[0]='--compact'
      }
    } elseif ($precisionWidth -eq 'compact') {
      $precisionArguments += '--compact'
    }
    Invoke-PrecisionTool (Join-Path $precisionOut "$precisionTest.exe") $precisionArguments (
      Join-Path $precisionOut "$precisionWidth-run.log")
  }
}
foreach ($precisionTest in @('nyx_canvas_precision_studio','nyx_canvas_precision_browser_controls')) {
  Invoke-PrecisionTool $precisionTools.PAS2JS ($precisionBrowserFlags + @(
    "tests/$precisionTest.lpr")) (Join-Path $precisionRoot "$precisionTest-browser-compile.log")
}
# Whole generated constructors compile independently and must reproduce exact
# complete design bytes. Native execution is construction, not UI execution.
$precisionCases=@(
  (Join-Path $precisionRoot 'nyx_canvas_precision_source_tests/exports/original'),
  (Join-Path $precisionRoot 'nyx_canvas_precision_source_tests/exports/changed'))
foreach ($precisionWidth in @('wide','compact')) {
  foreach ($precisionStage in @('applied','moved','removed')) {
    $precisionCases += Join-Path $precisionRoot (
      "nyx_canvas_precision_studio/fixture-$precisionWidth/exports/$precisionStage")
  }
}
$precisionIndex=0
foreach ($precisionSource in $precisionCases) {
  $precisionIndex++
  $precisionNative=Join-Path $precisionRoot "companions/$precisionIndex/native"
  $precisionBrowser=Join-Path $precisionRoot "companions/$precisionIndex/browser"
  New-Item -ItemType Directory -Path $precisionNative -Force | Out-Null
  Invoke-PrecisionTool $precisionTools.FPC ($precisionFlags + @(
    "-Fu$precisionSource","-FU$precisionNative","-FE$precisionNative",
    'tests/nyx_design_source_consumer.lpr')) (Join-Path $precisionNative 'compile.log')
  Invoke-PrecisionTool (Join-Path $precisionNative 'nyx_design_source_consumer.exe') @(
    (Join-Path $precisionSource 'expected.nyx')) (Join-Path $precisionNative 'run.log')
  # Admit each output directory at its point of use; a later compiler must not
  # depend on an empty sibling surviving the earlier native constructor run.
  New-Item -ItemType Directory -Path $precisionBrowser -Force | Out-Null
  Invoke-PrecisionTool $precisionTools.PAS2JS ($precisionBrowserFlags + @(
    "-Fu$precisionSource","-FE$precisionBrowser",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $precisionBrowser 'compile.log')
}
$precisionNative=Join-Path $precisionRoot 'products'
New-Item -ItemType Directory -Path $precisionNative | Out-Null
Invoke-PrecisionTool $precisionTools.LCL_FPC ($precisionLCLFlags + @(
  "-FU$precisionNative","-FE$precisionNative",'studio/nyx_studio_native.lpr')) (
  Join-Path $precisionNative 'studio-native-compile.log')
Invoke-PrecisionTool $precisionTools.FPC ($precisionFlags + @(
  "-FU$precisionNative","-FE$precisionNative",'studio/nyx_studio_server.lpr')) (
  Join-Path $precisionNative 'server-compile.log')
Invoke-PrecisionTool $precisionTools.PAS2JS ($precisionBrowserFlags + @(
  'studio/nyx_studio.lpr')) (Join-Path $precisionNative 'studio-browser-compile.log')
Invoke-PrecisionTool $precisionTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$precisionWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $precisionNative 'worker-compile.log')
Copy-Item -LiteralPath $precisionTools.PAS2JS_RUNTIME -Destination (Join-Path $precisionWeb 'rtl.js')
Write-Host 'Precision: native checks and whole companions passed; browser compiled only'
