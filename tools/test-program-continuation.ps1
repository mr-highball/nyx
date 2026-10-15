# nyx
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
#
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$Toolchain = '.local/toolchain.json',
  [string]$StaticJobsDirectory = '',
  [string]$BuildsBaseURL = ''
)
# Orchestration only. Pascal owns rich-source continuation and exact constructor/
# model equality. Supply an explicitly preflighted isolated static host to execute
# browser companions; this script never starts a server or edits a Studio project.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxProgramRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxProgramRoot) { throw 'Use a fresh owned evidence directory.' }
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the preflighted static directory and builds URL together.'
}
$nyxProgramTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxProgramPart in @('native','browser','export','driver')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxProgramRoot $nyxProgramPart) | Out-Null
}
function Invoke-ProgramTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Program continuation failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxProgramNative = Join-Path $nyxProgramRoot 'native'
$nyxProgramWeb = Join-Path $nyxProgramRoot 'browser'
$nyxProgramExport = Join-Path $nyxProgramRoot 'export'
$nyxProgramFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxProgramBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
Invoke-ProgramTool $nyxProgramTools.FPC ($nyxProgramFlags + @(
  '-Futests/fixtures/program-source',"-FU$nyxProgramNative","-FE$nyxProgramNative",
  'tests/nyx_program_continuation_tests.lpr')) (Join-Path $nyxProgramRoot 'continuation-native-compile.log')
Invoke-ProgramTool (Join-Path $nyxProgramNative 'nyx_program_continuation_tests.exe') @(
  'tests/fixtures/program-source/nyx.generated.view.pas',$nyxProgramExport) (
  Join-Path $nyxProgramRoot 'continuation-native-run.log')
Invoke-ProgramTool $nyxProgramTools.PAS2JS ($nyxProgramBrowserFlags + @(
  '-Futests/fixtures/program-source',"-FE$nyxProgramWeb",
  'tests/nyx_program_continuation_tests.lpr')) (Join-Path $nyxProgramRoot 'continuation-browser-compile.log')
for ($nyxProgramStage = 1; $nyxProgramStage -le 6; $nyxProgramStage++) {
  $nyxProgramName = "stage-$nyxProgramStage"
  $nyxProgramSource = Join-Path $nyxProgramExport $nyxProgramName
  $nyxProgramCompiled = Join-Path $nyxProgramRoot $nyxProgramName
  New-Item -ItemType Directory $nyxProgramCompiled | Out-Null
  Invoke-ProgramTool $nyxProgramTools.FPC ($nyxProgramFlags + @(
    "-Fu$nyxProgramSource","-FU$nyxProgramCompiled","-FE$nyxProgramCompiled",
    'tests/nyx_design_source_consumer.lpr')) (Join-Path $nyxProgramRoot "$nyxProgramName-native-compile.log")
  Invoke-ProgramTool (Join-Path $nyxProgramCompiled 'nyx_design_source_consumer.exe') @(
    (Join-Path $nyxProgramSource 'design.nyx')) (Join-Path $nyxProgramRoot "$nyxProgramName-native-run.log")
  Invoke-ProgramTool $nyxProgramTools.PAS2JS ($nyxProgramBrowserFlags + @(
    "-Fu$nyxProgramSource","-FE$nyxProgramCompiled",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $nyxProgramRoot "$nyxProgramName-browser-compile.log")
}
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native continuation/constructor equality passes; browser compilation only.'
  return
}
$nyxProgramDriver = Join-Path $nyxProgramRoot 'driver'
Invoke-ProgramTool $nyxProgramTools.FPC ($nyxProgramFlags + @(
  "-FU$nyxProgramDriver","-FE$nyxProgramDriver",'tests/nyx_browser_ready_capture.lpr')) (
  Join-Path $nyxProgramRoot 'driver-compile.log')
$nyxProgramChild = 'nyx-program-continuation-' + [guid]::NewGuid().ToString('N')
$nyxProgramStatic = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxProgramChild
New-Item -ItemType Directory $nyxProgramStatic | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxProgramWeb 'nyx_program_continuation_tests.js'),
  'tests/fixtures/program-source/nyx.generated.view.pas',
  'studio/web/program-continuation.html',$nyxProgramTools.PAS2JS_RUNTIME -Destination $nyxProgramStatic
$nyxProgramURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxProgramChild/"
$nyxProgramURL | Set-Content -LiteralPath (Join-Path $nyxProgramRoot 'static-url.txt') -Encoding utf8
foreach ($nyxProgramWidth in @(1280,390)) {
  Invoke-ProgramTool (Join-Path $nyxProgramDriver 'nyx_browser_ready_capture.exe') @(
    ($nyxProgramURL+'program-continuation.html'),
    (Join-Path $nyxProgramRoot "continuation-$nyxProgramWidth"),'data-program-continuation',
    [string]$nyxProgramWidth,'900','--fixture-timeout=180') (
    Join-Path $nyxProgramRoot "continuation-$nyxProgramWidth.log")
}
for ($nyxProgramStage = 1; $nyxProgramStage -le 6; $nyxProgramStage++) {
  $nyxProgramName = "stage-$nyxProgramStage"
  $nyxProgramCompiled = Join-Path $nyxProgramRoot $nyxProgramName
  $nyxProgramStageStatic = Join-Path $nyxProgramStatic $nyxProgramName
  New-Item -ItemType Directory $nyxProgramStageStatic | Out-Null
  Copy-Item -LiteralPath (Join-Path $nyxProgramCompiled 'nyx_design_source_consumer.js'),
    (Join-Path (Join-Path $nyxProgramExport $nyxProgramName) 'design.nyx'),
    'studio/web/design-source-consumer.html',$nyxProgramTools.PAS2JS_RUNTIME -Destination $nyxProgramStageStatic
  Invoke-ProgramTool (Join-Path $nyxProgramDriver 'nyx_browser_ready_capture.exe') @(
    ($nyxProgramURL+$nyxProgramName+'/design-source-consumer.html'),
    (Join-Path $nyxProgramRoot "$nyxProgramName-browser"),'data-design-source','1280','900',
    '--fixture-timeout=180') (Join-Path $nyxProgramRoot "$nyxProgramName-browser-run.log")
}
Write-Host 'Native/executed HTTP browser handwritten program continuation and six complete companions pass.'
