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

param(
  [Parameter(Mandatory = $true)][string]$EvidenceDirectory,
  [string]$Widgetset = 'win32',
  [switch]$Capture
)
$ErrorActionPreference = 'Stop'
$imageRoot = [IO.Path]::GetFullPath((Join-Path (Get-Location) $EvidenceDirectory))
if (Test-Path -LiteralPath $imageRoot) {
  throw 'Choose fresh hosted-image evidence output; retain previous receipts.'
}
$imageNative = Join-Path $imageRoot 'native'
$imageBrowser = Join-Path $imageRoot 'browser'
$imageFixtures = Join-Path $imageRoot 'fixtures'
$imageGenerated = Join-Path $imageRoot 'generated'
New-Item -ItemType Directory -Path $imageNative,$imageBrowser,$imageFixtures | Out-Null
$imageTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
function Invoke-ImageTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Hosted image qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$imageFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FU$imageNative","-FE$imageNative")
$imageLCL = $imageTools.LAZARUS
$imagePlatform = "$((& $imageTools.LCL_FPC '-iTP').Trim())-$((& $imageTools.LCL_FPC '-iTO').Trim())"
$imageLCLFlags = $imageFlags + @(
  "-Fu$imageLCL/lcl/units/$imagePlatform",
  "-Fu$imageLCL/lcl/units/$imagePlatform/$Widgetset",
  "-Fu$imageLCL/components/lazutils/lib/$imagePlatform",
  "-Fu$imageLCL/packager/units/$imagePlatform")
# The maintained Pascal generator owns all raster bytes. This script only
# orchestrates platform compilers and serial native executions; no host starts.
Invoke-ImageTool $imageTools.FPC ($imageFlags + @('tests/nyx_image_fixtures.lpr')) (
  Join-Path $imageRoot 'fixtures-compile.log')
Invoke-ImageTool (Join-Path $imageNative 'nyx_image_fixtures.exe') @($imageFixtures) (
  Join-Path $imageRoot 'fixtures-run.log')
$imageLCLFlags += "-Fu$imageFixtures"
foreach ($imageTest in @('nyx_resource_foundation_tests','nyx_resource_loader_tests','nyx_hosted_image_controls')) {
  Invoke-ImageTool $imageTools.LCL_FPC ($imageLCLFlags + @("tests/$imageTest.lpr")) (
    Join-Path $imageRoot "$imageTest-compile.log")
  if ($imageTest -eq 'nyx_hosted_image_controls') {
    foreach ($imageSize in @('wide','compact')) {
      $imageCapture = '--no-capture'
      if ($Capture) { $imageCapture = Join-Path $imageRoot $imageSize }
      $imageArgs = @($imageCapture,"--$imageSize")
      if ($imageSize -eq 'wide') { $imageArgs += $imageGenerated }
      Invoke-ImageTool (Join-Path $imageNative "$imageTest.exe") $imageArgs (
        Join-Path $imageRoot "$imageTest-$imageSize.log")
    }
  } else {
    # No transport/native-network arguments: these consumers qualify their
    # portable deterministic boundary, not live HTTP or cache persistence.
    Invoke-ImageTool (Join-Path $imageNative "$imageTest.exe") @() (
      Join-Path $imageRoot "$imageTest-run.log")
  }
}
# Compile/construct the exact emitted companion against its entire expected
# design, then stage the same bytes for browser compilation. No execution of a
# full generated application or browser is implied by constructor equivalence.
$imageCompanionNative = Join-Path $imageGenerated 'native'
$imageCompanionBrowser = Join-Path $imageGenerated 'browser'
New-Item -ItemType Directory -Path $imageCompanionNative,$imageCompanionBrowser | Out-Null
Invoke-ImageTool $imageTools.FPC ($imageFlags + @("-Fu$imageGenerated",
  "-FU$imageCompanionNative","-FE$imageCompanionNative",'tests/nyx_design_source_consumer.lpr')) (
  Join-Path $imageGenerated 'compile.log')
Invoke-ImageTool (Join-Path $imageCompanionNative 'nyx_design_source_consumer.exe') @(
  (Join-Path $imageGenerated 'expected.nyx')) (Join-Path $imageGenerated 'run.log')
$imageBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-Fu$imageFixtures","-FE$imageBrowser")
Invoke-ImageTool $imageTools.PAS2JS ($imageBrowserFlags + @("-Fu$imageGenerated",
  "-FE$imageCompanionBrowser",'tests/nyx_design_source_consumer.lpr')) (
  Join-Path $imageGenerated 'browser-compile.log')
foreach ($imageTest in @('nyx_resource_foundation_tests','nyx_resource_loader_tests','nyx_hosted_image_controls')) {
  Invoke-ImageTool $imageTools.PAS2JS ($imageBrowserFlags + @("tests/$imageTest.lpr")) (
    Join-Path $imageRoot "$imageTest-browser-compile.log")
}
$imageTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
[IO.File]::WriteAllText((Join-Path $imageBrowser 'nyx_hosted_image_controls.html'),
  $imageTemplate.Replace('nyx_program_logic_controls.js','nyx_hosted_image_controls.js').Replace(
    'Named Pascal program application qualification','Nyx hosted image qualification'),
  [Text.UTF8Encoding]::new($false))
Copy-Item -LiteralPath $imageTools.PAS2JS_RUNTIME -Destination (Join-Path $imageBrowser 'rtl.js')
Write-Host 'Native consumers executed. Browser compilation only; no host was launched.'
