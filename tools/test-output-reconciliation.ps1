<# nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$Toolchain = '.local/toolchain.json',
  [string]$StaticJobsDirectory = '',
  [string]$BuildsBaseURL = ''
)

# Platform-tool orchestration only. Pascal owns policy and actual-control
# assertions. Service/XHR responses are explicitly substituted; this is neither
# authentication/socket qualification nor a profile write to the installed host.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxOutputRoot = [IO.Path]::GetFullPath($EvidenceDirectory)

if (Test-Path -LiteralPath $nyxOutputRoot) {
  throw 'Output reconciliation requires a fresh owned evidence directory.'
}

if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxOutputTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('core','studio','web','driver')) {
  New-Item -ItemType Directory (Join-Path $nyxOutputRoot $nyxPart) | Out-Null
}
function Invoke-OutputTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log

  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "Output reconciliation failed; retained $Log"
  }
}
$nyxOutputFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxOutputLazarus = $nyxOutputTools.LAZARUS
$nyxOutputLCL = @(
  "-Fu$nyxOutputLazarus/lcl/units/i386-win32",
  "-Fu$nyxOutputLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxOutputLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxOutputLazarus/packager/units/i386-win32")
$nyxOutputWeb = Join-Path $nyxOutputRoot 'web'
$nyxOutputDriver = Join-Path $nyxOutputRoot 'driver'
foreach ($nyxPair in @(@('nyx_output_reconciliation_tests','core'),
  @('nyx_outputs_studio_controls','studio'))) {
  $nyxTest = $nyxPair[0]
  $nyxOut = Join-Path $nyxOutputRoot $nyxPair[1]
  $nyxNativeFlags = $nyxOutputFlags
  $nyxCompiler = $nyxOutputTools.FPC
  $nyxInput = @()

  if ($nyxPair[1] -eq 'studio') {
    $nyxNativeFlags += $nyxOutputLCL
    $nyxCompiler = $nyxOutputTools.LCL_FPC
    $nyxInput = @((Join-Path $nyxOutputRoot 'owned-project'))
  }
  Invoke-OutputTool $nyxCompiler ($nyxNativeFlags + @(
    "-FU$nyxOut", "-FE$nyxOut", "tests/$nyxTest.lpr")) (Join-Path $nyxOutputRoot "$nyxTest-native-compile.log")
  Invoke-OutputTool (Join-Path $nyxOut "$nyxTest.exe") $nyxInput (Join-Path $nyxOutputRoot "$nyxTest-native-run.log")
  Invoke-OutputTool $nyxOutputTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
    '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxOutputWeb",
    "tests/$nyxTest.lpr") (Join-Path $nyxOutputRoot "$nyxTest-browser-compile.log")
}

if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native reconciliation and ordinary controls pass; browser execution remains separate.'
  return
}
Invoke-OutputTool $nyxOutputTools.FPC ($nyxOutputFlags + @(
  "-FU$nyxOutputDriver", "-FE$nyxOutputDriver", 'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxOutputRoot 'driver-compile.log')
$nyxOutputChild = 'nyx-outputs-' + [guid]::NewGuid().ToString('N')
$nyxOutputStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxOutputChild
New-Item -ItemType Directory $nyxOutputStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxOutputWeb 'nyx_output_reconciliation_tests.js'),
  (Join-Path $nyxOutputWeb 'nyx_outputs_studio_controls.js'),
  'studio/web/output-reconciliation.html','studio/web/output-studio.html',
  $nyxOutputTools.PAS2JS_RUNTIME -Destination $nyxOutputStage
$nyxOutputURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxOutputChild/"
foreach ($nyxWidth in @(1280,390)) {
  foreach ($nyxCase in @(@('core','output-reconciliation.html','data-output-reconciliation'),
    @('studio','output-studio.html','data-output-studio'))) {
    Invoke-OutputTool (Join-Path $nyxOutputDriver 'nyx_browser_ready_capture.exe') @(
      ($nyxOutputURL+$nyxCase[1]), (Join-Path $nyxOutputRoot "$($nyxCase[0])-$nyxWidth"),
      $nyxCase[2], [string]$nyxWidth, '900', '--fixture-timeout=240') (Join-Path $nyxOutputRoot "$($nyxCase[0])-$nyxWidth.log")
  }
}
Write-Host 'Native/browser policy and ordinary Studio Outputs pass. No installed profile, service or project is changed.'
