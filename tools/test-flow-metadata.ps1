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

# Platform-tool orchestration only. Pascal checks immutable metadata admission
# and compares actual native glyph allocation with a fresh widgetset oracle.
# These are correctness regressions, not an elapsed-time performance threshold.
# Studio's ordinary paired edit journey remains in test-flow-condition-editor.
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxMetadataRoot = [IO.Path]::GetFullPath($EvidenceDirectory)

if (Test-Path -LiteralPath $nyxMetadataRoot) {
  throw 'Metadata qualification requires a fresh owned evidence directory.'
}

if (($StaticJobsDirectory -eq '') -ne ($BuildsBaseURL -eq '')) {
  throw 'Supply the independently preflighted static directory and URL together.'
}
$nyxMetadataTools = Get-Content -LiteralPath $Toolchain -Raw -Encoding utf8 | ConvertFrom-Json
foreach ($nyxPart in @('core','labels','web','driver')) {
  New-Item -ItemType Directory (Join-Path $nyxMetadataRoot $nyxPart) | Out-Null
}
function Invoke-MetadataTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log

  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 12
    throw "Metadata qualification failed; retained $Log"
  }
}
$nyxMetadataFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
$nyxMetadataCore = Join-Path $nyxMetadataRoot 'core'
$nyxMetadataLabels = Join-Path $nyxMetadataRoot 'labels'
$nyxMetadataWeb = Join-Path $nyxMetadataRoot 'web'
$nyxMetadataDriver = Join-Path $nyxMetadataRoot 'driver'
$nyxMetadataLazarus = $nyxMetadataTools.LAZARUS
$nyxMetadataLCL = @(
  "-Fu$nyxMetadataLazarus/lcl/units/i386-win32",
  "-Fu$nyxMetadataLazarus/lcl/units/i386-win32/win32",
  "-Fu$nyxMetadataLazarus/components/lazutils/lib/i386-win32",
  "-Fu$nyxMetadataLazarus/packager/units/i386-win32")
Invoke-MetadataTool $nyxMetadataTools.FPC ($nyxMetadataFlags + @(
  "-FU$nyxMetadataCore", "-FE$nyxMetadataCore", 'tests/nyx_flow_metadata_tests.lpr')) (Join-Path $nyxMetadataRoot 'metadata-compile.log')
Invoke-MetadataTool (Join-Path $nyxMetadataCore 'nyx_flow_metadata_tests.exe') @() (Join-Path $nyxMetadataRoot 'metadata-run.log')
Invoke-MetadataTool $nyxMetadataTools.LCL_FPC ($nyxMetadataFlags + $nyxMetadataLCL + @(
  '-dNYX_LCL_LAYOUT_PROFILE', "-FU$nyxMetadataLabels", "-FE$nyxMetadataLabels",
  'tests/nyx_label_measurement_tests.lpr')) (Join-Path $nyxMetadataRoot 'labels-compile.log')
Invoke-MetadataTool (Join-Path $nyxMetadataLabels 'nyx_label_measurement_tests.exe') @() (Join-Path $nyxMetadataRoot 'labels-run.log')
Invoke-MetadataTool $nyxMetadataTools.PAS2JS @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc', "-FE$nyxMetadataWeb",
  'tests/nyx_flow_metadata_tests.lpr') (Join-Path $nyxMetadataRoot 'metadata-browser-compile.log')

if ($StaticJobsDirectory -eq '') {
  Write-Host 'Native metadata/admission and actual LCL measurements pass. Browser execution remains separate.'
  return
}
Invoke-MetadataTool $nyxMetadataTools.FPC ($nyxMetadataFlags + @(
  "-FU$nyxMetadataDriver", "-FE$nyxMetadataDriver", 'tests/nyx_browser_ready_capture.lpr')) (Join-Path $nyxMetadataRoot 'driver-compile.log')
$nyxMetadataChild = 'nyx-flow-metadata-' + [guid]::NewGuid().ToString('N')
$nyxMetadataStage = Join-Path ([IO.Path]::GetFullPath($StaticJobsDirectory)) $nyxMetadataChild
New-Item -ItemType Directory $nyxMetadataStage | Out-Null
Copy-Item -LiteralPath (Join-Path $nyxMetadataWeb 'nyx_flow_metadata_tests.js'),
  'studio/web/flow-metadata.html', $nyxMetadataTools.PAS2JS_RUNTIME -Destination $nyxMetadataStage
$nyxMetadataURL = $BuildsBaseURL.TrimEnd('/') + "/$nyxMetadataChild/flow-metadata.html"
foreach ($nyxWidth in @(1280,390)) {
  Invoke-MetadataTool (Join-Path $nyxMetadataDriver 'nyx_browser_ready_capture.exe') @(
    $nyxMetadataURL, (Join-Path $nyxMetadataRoot "browser-$nyxWidth"),
    'data-flow-metadata', [string]$nyxWidth, '900', '--fixture-timeout=120') (Join-Path $nyxMetadataRoot "browser-$nyxWidth.log")
}
Write-Host 'Native/browser metadata admission and actual LCL measurements pass. No service, design or enrollment is replaced.'
