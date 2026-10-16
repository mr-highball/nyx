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
  [switch]$Editor
)
$ErrorActionPreference = 'Stop'
$diagramRoot = [IO.Path]::GetFullPath((Join-Path (Get-Location) $EvidenceDirectory))
if (Test-Path -LiteralPath $diagramRoot) {
  throw 'Choose a fresh diagram evidence directory; previous receipts remain retained.'
}
$diagramNative = Join-Path $diagramRoot 'native'
$diagramWeb = Join-Path $diagramRoot 'browser'
New-Item -ItemType Directory -Path $diagramNative,$diagramWeb | Out-Null
$diagramTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
function Invoke-DiagramTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Diagram qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Diagram native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$diagramFlags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Futests','-Fisrc',"-FU$diagramNative","-FE$diagramNative")
Invoke-DiagramTool $diagramTools.FPC ($diagramFlags + @('tests/nyx_diagram_tests.lpr')) (
  Join-Path $diagramRoot 'core-compile.log')
Invoke-DiagramTool (Join-Path $diagramNative 'nyx_diagram_tests.exe') @() (
  Join-Path $diagramRoot 'core-run.log')
$diagramLCL = $diagramTools.LAZARUS
$diagramPlatform = "$((& $diagramTools.LCL_FPC '-iTP').Trim())-$((& $diagramTools.LCL_FPC '-iTO').Trim())"
$diagramLCLFlags = $diagramFlags + @(
  "-Fu$diagramLCL/lcl/units/$diagramPlatform",
  "-Fu$diagramLCL/lcl/units/$diagramPlatform/$Widgetset",
  "-Fu$diagramLCL/components/lazutils/lib/$diagramPlatform",
  "-Fu$diagramLCL/packager/units/$diagramPlatform")
Invoke-DiagramTool $diagramTools.LCL_FPC ($diagramLCLFlags + @('tests/nyx_diagram_controls.lpr')) (
  Join-Path $diagramRoot 'controls-compile.log')
# Actual native UI runs are serial; compilers do not compete with input/layout.
Invoke-DiagramTool (Join-Path $diagramNative 'nyx_diagram_controls.exe') @(
  (Join-Path $diagramRoot 'wide.png')) (Join-Path $diagramRoot 'controls-wide.log')
Invoke-DiagramTool (Join-Path $diagramNative 'nyx_diagram_controls.exe') @(
  (Join-Path $diagramRoot 'compact.png'),'--compact') (Join-Path $diagramRoot 'controls-compact.log')
$diagramConsumers = @('nyx_diagram_tests','nyx_diagram_controls')
if ($Editor) {
  Invoke-DiagramTool $diagramTools.LCL_FPC ($diagramLCLFlags + @('tests/nyx_diagram_editor_controls.lpr')) (
    Join-Path $diagramRoot 'editor-compile.log')
  Invoke-DiagramTool (Join-Path $diagramNative 'nyx_diagram_editor_controls.exe') @(
    (Join-Path $diagramRoot 'editor-wide')) (Join-Path $diagramRoot 'editor-wide.log')
  Invoke-DiagramTool (Join-Path $diagramNative 'nyx_diagram_editor_controls.exe') @(
    (Join-Path $diagramRoot 'editor-compact'),'--compact') (Join-Path $diagramRoot 'editor-compact.log')
  $diagramConsumers += 'nyx_diagram_editor_controls'
}
$diagramBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Futests','-Fisrc',"-FE$diagramWeb")
$diagramTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
foreach ($diagramConsumer in $diagramConsumers) {
  Invoke-DiagramTool $diagramTools.PAS2JS ($diagramBrowserFlags + @("tests/$diagramConsumer.lpr")) (
    Join-Path $diagramRoot "$diagramConsumer-browser-compile.log")
  [IO.File]::WriteAllText((Join-Path $diagramWeb "$diagramConsumer.html"),
    $diagramTemplate.Replace('nyx_program_logic_controls.js',"$diagramConsumer.js").Replace(
      'Named Pascal program application qualification','Nyx diagram qualification'),
    [Text.UTF8Encoding]::new($false))
}
Copy-Item -LiteralPath $diagramTools.PAS2JS_RUNTIME -Destination (Join-Path $diagramWeb 'rtl.js')
Write-Host 'Native consumers executed. Matching browser consumers staged only; no HTTP host started.'
