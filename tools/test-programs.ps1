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
# Orchestration only. Pascal owns typed language, host/cancellation, source and
# actual control/publication assertions. This script never starts a listener.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxProgramRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxProgramRoot) {
  throw 'Program qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxProgramTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxProgramPart in @('core','native','web','driver')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxProgramRoot $nyxProgramPart) | Out-Null
}
function Invoke-ProgramTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 14
    throw "Program qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxProgramCore = Join-Path $nyxProgramRoot 'core'
$nyxProgramNative = Join-Path $nyxProgramRoot 'native'
$nyxProgramWeb = Join-Path $nyxProgramRoot 'web'
$nyxProgramFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
Invoke-ProgramTool $nyxProgramTools.FPC ($nyxProgramFlags + @(
  "-FU$nyxProgramCore", "-FE$nyxProgramCore", 'tests/nyx_programs_tests.lpr')) (Join-Path $nyxProgramRoot 'core-compile.log')
Invoke-ProgramTool (Join-Path $nyxProgramCore 'nyx_programs_tests.exe') @() (Join-Path $nyxProgramRoot 'core-run.log')
$nyxProgramLazarus = $nyxProgramTools.LAZARUS
Invoke-ProgramTool $nyxProgramTools.LCL_FPC ($nyxProgramFlags + @(
  "-Fu$nyxProgramLazarus/lcl/units/i386-win32",
  "-Fu$nyxProgramLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxProgramLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxProgramLazarus/packager/units/i386-win32",
  "-FU$nyxProgramNative", "-FE$nyxProgramNative",
  'tests/nyx_programs_controls.lpr')) (Join-Path $nyxProgramRoot 'native-compile.log')
Invoke-ProgramTool (Join-Path $nyxProgramNative 'nyx_programs_controls.exe') @(
  (Join-Path $nyxProgramRoot 'native-workbench.png')) (Join-Path $nyxProgramRoot 'native-run.log')
foreach ($nyxProgramTest in @('nyx_programs_tests','nyx_programs_controls')) {
  Invoke-ProgramTool $nyxProgramTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxProgramWeb",
    "tests/$nyxProgramTest.lpr") (Join-Path $nyxProgramRoot "$nyxProgramTest-browser-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native language/application pass; browser execution remains separate.'
  return
}
$nyxProgramDriver = Join-Path $nyxProgramRoot 'driver'
Invoke-ProgramTool $nyxProgramTools.FPC ($nyxProgramFlags + @(
  "-FU$nyxProgramDriver", "-FE$nyxProgramDriver",
  'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxProgramRoot 'driver-compile.log')
$nyxProgramChild = 'nyx-programs-' + [guid]::NewGuid().ToString('N')
$nyxProgramStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxProgramChild
New-Item -ItemType Directory $nyxProgramStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxProgramWeb 'nyx_programs_tests.js'),
  (Join-Path $nyxProgramWeb 'nyx_programs_controls.js'),
  'studio/web/programs-tests.html','studio/web/programs-controls.html',
  $nyxProgramTools.PAS2JS_RUNTIME -Destination $nyxProgramStage
$nyxProgramURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxProgramChild/"
$nyxProgramURL | Set-Content -LiteralPath (Join-Path $nyxProgramRoot 'static-url.txt') -Encoding utf8
foreach ($nyxProgramWidth in @(1280,390)) {
  foreach ($nyxProgramCase in @(
    @('core','programs-tests.html','data-programs'),
    @('application','programs-controls.html','data-program-controls'))) {
    Invoke-ProgramTool (Join-Path $nyxProgramDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxProgramURL+$nyxProgramCase[1]), (Join-Path $nyxProgramRoot "$($nyxProgramCase[0])-$nyxProgramWidth"),
      $nyxProgramCase[2], [string]$nyxProgramWidth, '900', '--fixture-timeout=180') (Join-Path $nyxProgramRoot "$($nyxProgramCase[0])-$nyxProgramWidth.log")
  }
}
Write-Host 'Native and actual HTTP browser behavior programs and guarded application publication pass.'
