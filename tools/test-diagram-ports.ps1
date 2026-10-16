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

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$Widgetset='win32'
)
$ErrorActionPreference='Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$portRoot=[IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $portRoot) {
  throw 'Choose fresh owned evidence; all previous receipts remain retained'
}
$portNative=Join-Path $portRoot 'native'
$portWeb=Join-Path $portRoot 'browser'
$portFixtures=Join-Path $portRoot 'fixtures'
$portScratch=Join-Path $portRoot 'scratch'
New-Item -ItemType Directory -Path $portNative,$portWeb,$portFixtures,$portScratch | Out-Null
$env:TEMP=$portScratch
$env:TMP=$portScratch
$portTools=Get-Content .local/toolchain.json -Raw | ConvertFrom-Json
$portPlatform="$((& $portTools.LCL_FPC '-iTP').Trim())-$((& $portTools.LCL_FPC '-iTO').Trim())"
$portFlags=@('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-Fu$portFixtures","-FU$portNative","-FE$portNative")
$portLCL=$portFlags + @("-Fu$($portTools.LAZARUS)/lcl/units/$portPlatform",
  "-Fu$($portTools.LAZARUS)/lcl/units/$portPlatform/$Widgetset",
  "-Fu$($portTools.LAZARUS)/components/lazutils/lib/$portPlatform",
  "-Fu$($portTools.LAZARUS)/packager/units/$portPlatform")
$portBrowser=@('-B','-Mdelphi','-Tbrowser','-Jirtl.js','-Fusrc','-Fustudio','-Futests','-Fisrc',
  "-Fu$portFixtures","-FE$portWeb")
# Pascal consumers own every model/source/input assertion. This script only
# orchestrates existing compilers and local fixtures; it launches no HTTP host,
# enrollment, application service or foreground-capture workaround.
function Invoke-PortTool([string]$Executable,[string[]]$Arguments,[string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Port qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native allocations remain; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
Invoke-PortTool $portTools.FPC ($portFlags + @('tests/nyx_image_fixtures.lpr')) (
  Join-Path $portRoot 'fixtures-compile.log')
Invoke-PortTool (Join-Path $portNative 'nyx_image_fixtures.exe') @($portFixtures) (
  Join-Path $portRoot 'fixtures-run.log')
foreach ($portTest in @('nyx_diagram_port_tests','nyx_diagram_port_source_tests',
    'nyx_diagram_tests','nyx_diagram_manipulation_tests','nyx_diagram_authoring_tests',
    'nyx_diagram_document_tests')) {
  Invoke-PortTool $portTools.FPC ($portFlags + @("tests/$portTest.lpr")) (
    Join-Path $portRoot "$portTest-compile.log")
  $portArguments=@()
  if ($portTest -eq 'nyx_diagram_port_source_tests') {
    $portArguments=@((Join-Path $portRoot 'source-pairs'))
  }
  Invoke-PortTool (Join-Path $portNative "$portTest.exe") $portArguments (
    Join-Path $portRoot "$portTest-run.log")
  Invoke-PortTool $portTools.PAS2JS ($portBrowser + @("tests/$portTest.lpr")) (
    Join-Path $portRoot "$portTest-browser-compile.log")
}
foreach ($portTest in @('nyx_diagram_content_controls','nyx_diagram_content_studio_controls')) {
  Invoke-PortTool $portTools.LCL_FPC ($portLCL + @("tests/$portTest.lpr")) (
    Join-Path $portRoot "$portTest-compile.log")
  foreach ($portWidth in @('wide','compact')) {
    $portArguments=@('--input-only',"--$portWidth")
    if ($portTest -eq 'nyx_diagram_content_studio_controls') {
      $portArguments=@((Join-Path $portRoot "studio-$portWidth"),"--$portWidth",'--input-only')
    }
    Invoke-PortTool (Join-Path $portNative "$portTest.exe") $portArguments (
      Join-Path $portRoot "$portTest-$portWidth-run.log")
  }
  Invoke-PortTool $portTools.PAS2JS ($portBrowser + @("tests/$portTest.lpr")) (
    Join-Path $portRoot "$portTest-browser-compile.log")
}
$portCases=@((Join-Path $portRoot 'source-pairs/original'),(Join-Path $portRoot 'source-pairs/changed'))
foreach ($portWidth in @('wide','compact')) {
  $portCases+=Join-Path $portRoot "studio-$portWidth/ports"
}
$portIndex=0
foreach ($portCase in $portCases) {
  $portIndex++
  Invoke-PortTool $portTools.FPC ($portFlags + @("-Fu$portCase",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $portRoot "constructor-$portIndex-compile.log")
  Invoke-PortTool (Join-Path $portNative 'nyx_design_source_consumer.exe') @(
    (Join-Path $portCase 'expected.nyx')) (Join-Path $portRoot "constructor-$portIndex-run.log")
  Invoke-PortTool $portTools.PAS2JS ($portBrowser + @("-Fu$portCase",'tests/nyx_design_source_consumer.lpr')) (
    Join-Path $portRoot "constructor-$portIndex-browser-compile.log")
}
Invoke-PortTool $portTools.LCL_FPC ($portLCL + @('studio/nyx_studio_native.lpr')) (
  Join-Path $portRoot 'studio-native-compile.log')
Invoke-PortTool $portTools.FPC ($portFlags + @('studio/nyx_studio_server.lpr')) (
  Join-Path $portRoot 'server-compile.log')
Invoke-PortTool $portTools.PAS2JS ($portBrowser + @('studio/nyx_studio.lpr')) (
  Join-Path $portRoot 'studio-browser-compile.log')
Invoke-PortTool $portTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$portWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $portRoot 'worker-compile.log')
Write-Host 'Named ports: native contracts/input/constructors passed; browser compiled only'
