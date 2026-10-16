# Nyx empty-canvas graph authoring qualification.
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

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$InputEvidence = '',
  [string]$Widgetset = 'win32'
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxEmptyRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxEmptyRoot) {
  throw 'Empty graph qualification requires a fresh owned evidence directory'
}
$nyxEmptyTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$nyxEmptyNative = Join-Path $nyxEmptyRoot 'native'
$nyxEmptyWeb = Join-Path $nyxEmptyRoot 'web'
New-Item -ItemType Directory -Force $nyxEmptyNative,$nyxEmptyWeb | Out-Null

# Pascal owns input, constructor admission, exact paired history and retirement.
# This shell only orchestrates platform tools; it starts no HTTP host.
function Invoke-EmptyTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 18
    throw "Empty graph qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxEmptyLCL = $nyxEmptyTools.LAZARUS
$nyxEmptyPlatform = "$((& $nyxEmptyTools.LCL_FPC '-iTP').Trim())-$((& $nyxEmptyTools.LCL_FPC '-iTO').Trim())"
$nyxEmptyFlags = @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',
  "-Fu$nyxEmptyLCL/lcl/units/$nyxEmptyPlatform",
  "-Fu$nyxEmptyLCL/lcl/units/$nyxEmptyPlatform/$Widgetset",
  "-Fu$nyxEmptyLCL/components/lazutils/lib/$nyxEmptyPlatform",
  "-Fu$nyxEmptyLCL/packager/units/$nyxEmptyPlatform",
  "-FU$nyxEmptyNative","-FE$nyxEmptyNative")
if ($InputEvidence -eq '') {
  $nyxEmptySource = Join-Path $nyxEmptyRoot 'owned-source'
  Invoke-EmptyTool $nyxEmptyTools.LCL_FPC (@('-B') + $nyxEmptyFlags + @(
    'tests/nyx_tool_graph_only_controls.lpr')) (Join-Path $nyxEmptyRoot 'native-compile.log')
  Invoke-EmptyTool (Join-Path $nyxEmptyNative 'nyx_tool_graph_only_controls.exe') @(
    (Get-Location).Path,(Join-Path (Get-Location) '.local/toolchain.json'),$nyxEmptySource) (
    Join-Path $nyxEmptyRoot 'native-run.log')
} else {
  # Explicit immutable reuse is not another input execution. Keep the input log,
  # exact exported pair and its source identity receipt with the original packet.
  $nyxEmptyInput = [IO.Path]::GetFullPath($InputEvidence)
  $nyxEmptySource = Join-Path $nyxEmptyInput 'owned-source'
  foreach ($nyxEmptyFile in @('accepted.nyx','nyx.generated.view.pas','canvas.png')) {
    if (-not (Test-Path -LiteralPath (Join-Path $nyxEmptySource $nyxEmptyFile))) {
      throw 'Explicit input evidence lacks its complete accepted export'
    }
  }
  $nyxEmptyInputLog = Join-Path $nyxEmptyInput 'native-run.log'
  if (-not (Select-String -LiteralPath $nyxEmptyInputLog -Pattern '^PASS [0-9]+ actual graph-only canvas checks$' -Quiet)) {
    throw 'Explicit input evidence did not finish successfully'
  }
  [IO.File]::WriteAllText((Join-Path $nyxEmptyRoot 'input-evidence-private.txt'),$nyxEmptyInput)
}
$nyxEmptyBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxEmptyWeb")
Invoke-EmptyTool $nyxEmptyTools.PAS2JS ($nyxEmptyBrowserFlags + @(
  'tests/nyx_tool_graph_only_controls.lpr')) (Join-Path $nyxEmptyRoot 'browser-compile.log')

# The companion independently compiles the exact accepted source. Its explicit
# harness Run callback executes the saved graph and binds output to a real label;
# this does not claim a persisted user callback or HTTP browser execution.
$nyxEmptyCompiled = Join-Path $nyxEmptyRoot 'compiled'
$nyxEmptySavedWeb = Join-Path $nyxEmptyWeb 'saved'
New-Item -ItemType Directory -Force $nyxEmptyCompiled,$nyxEmptySavedWeb | Out-Null
Invoke-EmptyTool $nyxEmptyTools.LCL_FPC (@("-Fu$nyxEmptySource",'-dNYX_GRAPH_ONLY_COMPANION') +
  $nyxEmptyFlags + @("-FU$nyxEmptyCompiled","-FE$nyxEmptyCompiled",
  'tests/nyx_tool_graph_only_controls.lpr')) (Join-Path $nyxEmptyRoot 'companion-compile.log')
Invoke-EmptyTool (Join-Path $nyxEmptyCompiled 'nyx_tool_graph_only_controls.exe') @(
  (Join-Path $nyxEmptySource 'accepted.nyx')) (Join-Path $nyxEmptyRoot 'companion-run.log')
Invoke-EmptyTool $nyxEmptyTools.PAS2JS (@("-Fu$nyxEmptySource",'-dNYX_GRAPH_ONLY_COMPANION') +
  $nyxEmptyBrowserFlags + @("-FE$nyxEmptySavedWeb",'tests/nyx_tool_graph_only_controls.lpr')) (
  Join-Path $nyxEmptyRoot 'companion-browser-compile.log')

# Stage only: serving and actual browser/observing qualification remain separate.
$nyxEmptyTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
$nyxEmptyHTML = $nyxEmptyTemplate.Replace('nyx_program_logic_controls.js','nyx_tool_graph_only_controls.js')
$nyxEmptyHTML = $nyxEmptyHTML.Replace('Named Pascal program application qualification','Empty canvas graph qualification')
Copy-Item -LiteralPath $nyxEmptyTools.PAS2JS_RUNTIME -Destination (Join-Path $nyxEmptyWeb 'rtl.js')
[IO.File]::WriteAllText((Join-Path $nyxEmptyWeb 'index.html'),$nyxEmptyHTML,[Text.UTF8Encoding]::new($false))
[IO.File]::WriteAllText((Join-Path $nyxEmptySavedWeb 'index.html'),
  $nyxEmptyHTML.Replace('src="rtl.js"','src="../rtl.js"'),[Text.UTF8Encoding]::new($false))
