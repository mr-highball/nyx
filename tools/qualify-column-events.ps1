# { nyx
#   Copyright (c) 2020 mr-highball
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE.
# }
# Stage portable/Studio source checks and their own compiled application.
# Browser outputs are served by an existing Pascal QA host; this runner launches
# no listener and changes no Studio project, service or machine configuration.
param(
  [string]$EvidenceDirectory,
  [ValidateSet('All','Native','Browser')]
  [string]$Platform = 'All'
)
$ErrorActionPreference = 'Stop'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
$configuration = Get-Content -LiteralPath (Join-Path $repositoryRoot '.local/toolchain.json') -Raw |
  ConvertFrom-Json
if ([string]::IsNullOrWhiteSpace($EvidenceDirectory)) {
  if ([string]::IsNullOrWhiteSpace($configuration.EVIDENCE_ROOT)) {
    throw 'Supply an external EvidenceDirectory or configure EVIDENCE_ROOT.'
  }
  $EvidenceDirectory = Join-Path $configuration.EVIDENCE_ROOT (
    'column-events-' + [DateTime]::UtcNow.ToString('yyyyMMddTHHmmssfffZ'))
}
$packet = [IO.Path]::GetFullPath($EvidenceDirectory)
if ((Test-Path -LiteralPath $packet) -and
  (Get-ChildItem -LiteralPath $packet -Force | Select-Object -First 1)) {
  throw 'Use a fresh output directory; earlier failures and evidence are preserved.'
}
if ([string]::IsNullOrWhiteSpace($configuration.TEMP_ROOT)) {
  throw 'Configure an external TEMP_ROOT for compiler and test host temporaries.'
}
$previousTemp = $env:TEMP
$previousTmp = $env:TMP
$previousLocation = Get-Location
$native = Join-Path $packet 'native'
$units = Join-Path $packet 'units'
$generated = Join-Path $packet 'generated'
New-Item -ItemType Directory -Path $native,$units,$generated -Force | Out-Null
$compiler = $configuration.LCL_FPC
if ([string]::IsNullOrWhiteSpace($compiler)) { $compiler = $configuration.FPC }
$flags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  "-FE$native","-FU$units","-Fu$units","-Fu$generated",
  '-Fusrc','-Fustudio','-Futests','-Fisrc','-Fistudio','-Fitests')

function Build-ColumnNative([string]$Program, [switch]$LCL) {
  $parameters = $flags
  if ($LCL) {
    $lazarus = $configuration.LAZARUS
    $parameters += @("-Fu$lazarus/lcl/units/i386-win32",
      "-Fu$lazarus/lcl/units/i386-win32/win32",
      "-Fu$lazarus/components/lazutils/lib/i386-win32",
      "-Fu$lazarus/packager/units/i386-win32")
  }
  & $compiler @parameters "tests/$Program.lpr" *> "$packet/$Program-native-build.log"
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath "$packet/$Program-native-build.log" -Tail 25
    throw "Native build failed: $Program"
  }
}
function Run-ColumnNative([string]$Program, [string]$Argument, [string]$Suffix) {
  & "$native/$Program.exe" $Argument *> "$packet/$Program$Suffix-native-run.log"
  $result = $LASTEXITCODE
  Get-Content -LiteralPath "$packet/$Program$Suffix-native-run.log" -Tail 12
  if ($result -ne 0) { throw "Native checks failed: $Program $Suffix" }
}

try {
  Set-Location -LiteralPath $repositoryRoot
  $env:TEMP = $configuration.TEMP_ROOT
  $env:TMP = $configuration.TEMP_ROOT
  # Source generation is Pascal and uses the ordinary Studio session/inspector.
  Build-ColumnNative 'nyx_column_events_source'
  Run-ColumnNative 'nyx_column_events_source' $generated ''
  Build-ColumnNative 'nyx_column_events_tests'
  Run-ColumnNative 'nyx_column_events_tests' '' ''
  if ($Platform -ne 'Browser') {
    Build-ColumnNative 'nyx_column_events_controls' -LCL
    foreach ($width in @(1280,390)) {
      Run-ColumnNative 'nyx_column_events_controls' ([string]$width) "-$width"
    }
  }
  if ($Platform -ne 'Native') {
    foreach ($program in @('nyx_column_events_tests','nyx_column_events_controls')) {
      $browserOutput = Join-Path $packet "web/$program"
      New-Item -ItemType Directory -Path $browserOutput -Force | Out-Null
      & $configuration.PAS2JS '-Mdelphi' '-Tbrowser' '-Jirtl.js' '-Fusrc' '-Fustudio' '-Futests' (
        "-Fu$generated") '-Fisrc' '-Fistudio' "-FE$browserOutput" "-o$browserOutput/app.js" (
        "tests/$program.lpr") *> "$packet/$program-browser-build.log"
      if ($LASTEXITCODE -ne 0) {
        Get-Content -LiteralPath "$packet/$program-browser-build.log" -Tail 25
        throw "Browser build failed: $program"
      }
      Copy-Item -LiteralPath $configuration.PAS2JS_RUNTIME -Destination "$browserOutput/rtl.js"
      [IO.File]::WriteAllText("$browserOutput/index.html",
        '<!doctype html><html lang="en"><head><meta charset="utf-8">' +
        '<meta name="viewport" content="width=device-width,initial-scale=1">' +
        '<title>Nyx column callbacks</title><script src="rtl.js"></script>' +
        '<script src="app.js"></script></head><body><script>rtl.run();</script></body></html>',
        [Text.UTF8Encoding]::new($false))
    }
  }
  Write-Host "Column callback evidence: $packet"
} finally {
  $env:TEMP = $previousTemp
  $env:TMP = $previousTmp
  Set-Location -LiteralPath $previousLocation
}
