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
# Orchestration only. Pascal owns all semantic, compiler-constructor and actual
# Studio input checks. No server starts and no protected project is selected.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxAuthorRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxAuthorRoot) { throw 'Use a fresh owned evidence directory.' }
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the preflighted static directory and builds URL together.'
}
$nyxAuthorTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxAuthorPart in @('core','native','compiled','web','driver','export','backend')) {
  New-Item -ItemType Directory -Force (Join-Path $nyxAuthorRoot $nyxAuthorPart) | Out-Null
}
function Invoke-AuthorTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Program authoring qualification failed; retained $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxAuthorFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxAuthorCore = Join-Path $nyxAuthorRoot 'core'
$nyxAuthorNative = Join-Path $nyxAuthorRoot 'native'
$nyxAuthorCompiled = Join-Path $nyxAuthorRoot 'compiled'
$nyxAuthorWeb = Join-Path $nyxAuthorRoot 'web'
$nyxAuthorExport = Join-Path $nyxAuthorRoot 'export'
Invoke-AuthorTool $nyxAuthorTools.FPC ($nyxAuthorFlags + @(
  "-FU$nyxAuthorCore","-FE$nyxAuthorCore",'tests/nyx_program_authoring_tests.lpr')) (Join-Path $nyxAuthorRoot 'core-compile.log')
Invoke-AuthorTool (Join-Path $nyxAuthorCore 'nyx_program_authoring_tests.exe') @(
  $nyxAuthorExport) (Join-Path $nyxAuthorRoot 'core-run.log')
Invoke-AuthorTool $nyxAuthorTools.FPC ($nyxAuthorFlags + @(
  "-Fu$nyxAuthorExport","-FU$nyxAuthorCompiled","-FE$nyxAuthorCompiled",
  'tests/nyx_program_document_compiled.lpr')) (Join-Path $nyxAuthorRoot 'compiled-compile.log')
Invoke-AuthorTool (Join-Path $nyxAuthorCompiled 'nyx_program_document_compiled.exe') @(
  (Join-Path $nyxAuthorExport 'design.nyx')) (Join-Path $nyxAuthorRoot 'compiled-run.log')
$nyxAuthorLCL = $nyxAuthorTools.LAZARUS
$nyxAuthorLCLFlags = $nyxAuthorFlags + @(
  "-Fu$nyxAuthorLCL/lcl/units/i386-win32",
  "-Fu$nyxAuthorLCL/lcl/units/i386-win32/win32",
  "-Fu$nyxAuthorLCL/components/lazutils/lib/i386-win32",
  "-Fu$nyxAuthorLCL/packager/units/i386-win32",
  "-FU$nyxAuthorNative","-FE$nyxAuthorNative")
Invoke-AuthorTool $nyxAuthorTools.LCL_FPC ($nyxAuthorLCLFlags + @(
  'tests/nyx_program_studio_controls.lpr')) (Join-Path $nyxAuthorRoot 'native-compile.log')
Invoke-AuthorTool (Join-Path $nyxAuthorNative 'nyx_program_studio_controls.exe') @(
  (Join-Path $nyxAuthorRoot 'native-project'),
  (Join-Path $nyxAuthorRoot 'native-programs.png')) (Join-Path $nyxAuthorRoot 'native-run.log')
Invoke-AuthorTool $nyxAuthorTools.LCL_FPC ($nyxAuthorLCLFlags + @(
  'studio/nyx_studio_native.lpr')) (Join-Path $nyxAuthorRoot 'studio-native-compile.log')
$nyxAuthorBackend = Join-Path $nyxAuthorRoot 'backend'
Invoke-AuthorTool $nyxAuthorTools.FPC ($nyxAuthorFlags + @(
  "-FU$nyxAuthorBackend","-FE$nyxAuthorBackend",'studio/nyx_studio_server.lpr')) (Join-Path $nyxAuthorRoot 'server-compile.log')
$nyxAuthorWebFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js','-Fusrc','-Fustudio','-Futests','-Fisrc',
  "-Fu$nyxAuthorExport","-FE$nyxAuthorWeb")
foreach ($nyxAuthorProgram in @('tests/nyx_program_authoring_tests.lpr',
    'tests/nyx_program_document_compiled.lpr','tests/nyx_program_studio_controls.lpr',
    'studio/nyx_studio.lpr')) {
  Invoke-AuthorTool $nyxAuthorTools.PAS2JS ($nyxAuthorWebFlags + @(
    $nyxAuthorProgram)) (Join-Path $nyxAuthorRoot (
      [IO.Path]::GetFileNameWithoutExtension($nyxAuthorProgram) + '-browser-compile.log'))
}
Invoke-AuthorTool $nyxAuthorTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxAuthorWeb",
  'studio/nyx_source_worker.lpr') (Join-Path $nyxAuthorRoot 'source-worker-compile.log')
if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native authoring/compiler/Studio checks pass; browser compilation only.'
  return
}
$nyxAuthorDriver = Join-Path $nyxAuthorRoot 'driver'
Invoke-AuthorTool $nyxAuthorTools.FPC ($nyxAuthorFlags + @(
  "-FU$nyxAuthorDriver","-FE$nyxAuthorDriver",'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxAuthorRoot 'driver-compile.log')
$nyxAuthorChild = 'nyx-program-authoring-' + [guid]::NewGuid().ToString('N')
$nyxAuthorStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxAuthorChild
New-Item -ItemType Directory $nyxAuthorStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxAuthorWeb 'nyx_program_authoring_tests.js'),
  (Join-Path $nyxAuthorWeb 'nyx_program_document_compiled.js'),
  (Join-Path $nyxAuthorWeb 'nyx_program_studio_controls.js'),
  (Join-Path $nyxAuthorWeb 'nyx_source_worker.js'),
  (Join-Path $nyxAuthorExport 'design.nyx'),
  'studio/web/program-authoring.html','studio/web/program-compiled.html',
  'studio/web/program-studio.html',$nyxAuthorTools.PAS2JS_RUNTIME -Destination $nyxAuthorStage
$nyxAuthorURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxAuthorChild/"
$nyxAuthorURL | Set-Content -LiteralPath (Join-Path $nyxAuthorRoot 'static-url.txt') -Encoding utf8
foreach ($nyxAuthorWidth in @(1280,390)) {
  foreach ($nyxAuthorCase in @(
    @('core','program-authoring.html','data-program-authoring'),
    @('compiled','program-compiled.html','data-program-compiled'),
    @('studio','program-studio.html','data-program-studio'))) {
    Invoke-AuthorTool (Join-Path $nyxAuthorDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxAuthorURL+$nyxAuthorCase[1]),
      (Join-Path $nyxAuthorRoot "$($nyxAuthorCase[0])-$nyxAuthorWidth"),
      $nyxAuthorCase[2],[string]$nyxAuthorWidth,'900','--fixture-timeout=180') (
      Join-Path $nyxAuthorRoot "$($nyxAuthorCase[0])-$nyxAuthorWidth.log")
  }
}
Write-Host 'Native and actual HTTP browser program authoring, compiled constructors and Studio input pass.'
