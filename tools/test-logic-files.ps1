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
  [string]$BuildsBaseURL = '',
  [switch]$Activation
)

# This script only orchestrates installed compilers and Pascal qualification
# programs. Optional HTTP execution stages a fresh static child on an already
# running, independently preflighted host. It starts/stops no server or listener.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxFilesRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxFilesRoot) {
  throw 'File-save qualification requires a fresh owned evidence directory.'
}
if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply both the preflighted static jobs directory and its builds URL, or neither.'
}
$nyxFilesTools = Get-Content -LiteralPath $Toolchain -Raw | ConvertFrom-Json
$nyxFilesCore = Join-Path $nyxFilesRoot 'core'
$nyxFilesNative = Join-Path $nyxFilesRoot 'native'
$nyxFilesWeb = Join-Path $nyxFilesRoot 'web'
$nyxFilesExport = Join-Path $nyxFilesRoot 'export'
$nyxFilesDriver = Join-Path $nyxFilesRoot 'driver'
New-Item -ItemType Directory $nyxFilesCore, $nyxFilesNative, $nyxFilesWeb,
  $nyxFilesExport, $nyxFilesDriver | Out-Null

function Invoke-FileCompiler([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 14
    throw "Compiler failed; retained $Log"
  }
}

function Invoke-FileConsumer([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  Get-Content -LiteralPath $Log -TotalCount 2
  if ($LASTEXITCODE -ne 0) {
    throw "Consumer failed; retained $Log"
  }
}

$nyxFilesFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
  '-Fusrc', '-Fustudio', '-Futests', '-Fisrc', "-FU$nyxFilesCore", "-FE$nyxFilesCore")
$nyxFilesCoreTests = @('nyx_logic_file_save_tests', 'nyx_file_save_authoring_tests')
if ($Activation) {
  $nyxFilesCoreTests += 'nyx_save_activation_tests'
}
foreach ($nyxFilesTest in $nyxFilesCoreTests) {
  Invoke-FileCompiler $nyxFilesTools.FPC ($nyxFilesFlags + @("tests/$nyxFilesTest.lpr")) (Join-Path $nyxFilesRoot "$nyxFilesTest-compile.log")
  $nyxFilesArgs = @()
  if ($nyxFilesTest -eq 'nyx_file_save_authoring_tests') {
    $nyxFilesArgs = @($nyxFilesExport)
  }
  Invoke-FileConsumer (Join-Path $nyxFilesCore "$nyxFilesTest.exe") $nyxFilesArgs (Join-Path $nyxFilesRoot "$nyxFilesTest-run.log")
}
Invoke-FileCompiler $nyxFilesTools.FPC ($nyxFilesFlags + @('-dNYX_FILE_SAVE_COMPANION',
  "-Fu$nyxFilesExport", 'tests/nyx_file_save_authoring_tests.lpr')) (Join-Path $nyxFilesRoot 'native-companion-compile.log')
Invoke-FileConsumer (Join-Path $nyxFilesCore 'nyx_file_save_authoring_tests.exe') @() (Join-Path $nyxFilesRoot 'native-companion-run.log')

$nyxFilesLazarus = $nyxFilesTools.LAZARUS
$nyxFilesPlatform = "$((& $nyxFilesTools.LCL_FPC '-iTP').Trim())-$((& $nyxFilesTools.LCL_FPC '-iTO').Trim())"
$nyxFilesLCLFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
  '-Fusrc', '-Futests', '-Fisrc', "-FU$nyxFilesNative", "-FE$nyxFilesNative",
  "-Fu$nyxFilesLazarus/lcl/units/$nyxFilesPlatform",
  "-Fu$nyxFilesLazarus/lcl/units/$nyxFilesPlatform/win32",
  "-Fu$nyxFilesLazarus/components/lazutils/lib/$nyxFilesPlatform",
  "-Fu$nyxFilesLazarus/packager/units/$nyxFilesPlatform")
if ($Activation) {
  $nyxFilesLCLFlags += '-dNYX_SAVE_ACTIVATION'
}
Invoke-FileCompiler $nyxFilesTools.LCL_FPC ($nyxFilesLCLFlags + @('tests/nyx_file_save_controls.lpr')) (Join-Path $nyxFilesRoot 'native-controls-compile.log')
Invoke-FileConsumer (Join-Path $nyxFilesNative 'nyx_file_save_controls.exe') @((Join-Path $nyxFilesRoot 'written')) (Join-Path $nyxFilesRoot 'native-controls-run.log')

$nyxFilesBrowserFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
  '-Fusrc', '-Fustudio', '-Futests', '-Fisrc', "-FE$nyxFilesWeb")
$nyxFilesBrowserTests = @('nyx_logic_file_save_tests', 'nyx_file_save_authoring_tests', 'nyx_file_save_controls')
if ($Activation) {
  $nyxFilesBrowserFlags += '-dNYX_SAVE_ACTIVATION'
  $nyxFilesBrowserTests += 'nyx_save_activation_tests'
}
foreach ($nyxFilesTest in $nyxFilesBrowserTests) {
  $nyxFilesArgs = $nyxFilesBrowserFlags
  if ($nyxFilesTest -eq 'nyx_file_save_authoring_tests') {
    $nyxFilesArgs = $nyxFilesArgs + @('-dNYX_FILE_SAVE_COMPANION', "-Fu$nyxFilesExport")
  }
  Invoke-FileCompiler $nyxFilesTools.PAS2JS ($nyxFilesArgs + @("tests/$nyxFilesTest.lpr")) (Join-Path $nyxFilesRoot "$nyxFilesTest-browser-compile.log")
}
Copy-Item -LiteralPath $nyxFilesTools.PAS2JS_RUNTIME, studio/web/logic-file-save-contract.html,
  studio/web/logic-file-save-controls.html, studio/web/logic-file-save-authoring.html -Destination $nyxFilesWeb
if ($Activation) {
  Copy-Item -LiteralPath studio/web/save-activation.html -Destination $nyxFilesWeb
}
Invoke-FileCompiler $nyxFilesTools.FPC @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
  '-gl', '-gh', '-Fusrc', '-Futests', '-Fisrc', "-FU$nyxFilesDriver", "-FE$nyxFilesDriver",
  'tests/nyx_file_save_browser_consumer.lpr') (Join-Path $nyxFilesRoot 'browser-driver-compile.log')

if ($StaticJobsDirectory -ne '') {
  $nyxFilesChild = 'nyx-file-save-' + [guid]::NewGuid().ToString('N')
  $nyxFilesStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxFilesChild
  if (Test-Path -LiteralPath $nyxFilesStage) {
    throw 'Static fixture child must be fresh.'
  }
  New-Item -ItemType Directory $nyxFilesStage | Out-Null
  foreach ($nyxFilesName in @('rtl.js', 'nyx_logic_file_save_tests.js',
      'nyx_file_save_authoring_tests.js', 'nyx_file_save_controls.js',
      'logic-file-save-contract.html', 'logic-file-save-authoring.html', 'logic-file-save-controls.html')) {
    Copy-Item -LiteralPath (Join-Path $nyxFilesWeb $nyxFilesName) -Destination $nyxFilesStage
  }
  if ($Activation) {
    Copy-Item -LiteralPath (Join-Path $nyxFilesWeb 'nyx_save_activation_tests.js'),
      (Join-Path $nyxFilesWeb 'save-activation.html') -Destination $nyxFilesStage
  }
  $nyxFilesKinds = @('contract', 'authoring', 'controls')
  if ($Activation) {
    $nyxFilesKinds += 'activation-contract'
  }
  foreach ($nyxFilesWidth in @(1280, 390)) {
    foreach ($nyxFilesKind in $nyxFilesKinds) {
      $nyxFilesMode = 'contract'
      if ($nyxFilesKind -eq 'controls') {
        $nyxFilesMode = 'controls'
        if ($Activation) {
          $nyxFilesMode = 'activation'
        }
      }
      $nyxFilesCapture = Join-Path $nyxFilesRoot "browser-$nyxFilesKind-$nyxFilesWidth"
      $nyxFilesURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxFilesChild/logic-file-save-$nyxFilesKind.html"
      if ($nyxFilesKind -eq 'activation-contract') {
        $nyxFilesMode = 'activation-contract'
        $nyxFilesURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxFilesChild/save-activation.html"
      }
      Invoke-FileConsumer (Join-Path $nyxFilesDriver 'nyx_file_save_browser_consumer.exe') @(
        $nyxFilesURL, $nyxFilesCapture, [string]$nyxFilesWidth, $nyxFilesMode) ($nyxFilesCapture + '.log')
    }
  }
  Write-Host 'Executed desktop/CSS-390 HTTP browser checks and exact downloaded bytes. Static fixtures are not admitted compiler jobs or installed release artifacts.'
} else {
  Write-Host 'Browser fixtures compiled and staged only; execution remains unqualified.'
}
if ($Activation) {
  Write-Host 'Prepared save activation, shared/native ownership and actual target input checks passed. Browser picker OS UI, other widgetsets, source/Studio integration and delivery remain separate.'
} else {
  Write-Host 'Native write/cancel/retirement and source/history/local semantic checks passed. Browser picker UI, other widgetsets, long-running activation recovery and observing release delivery remain separate.'
}
