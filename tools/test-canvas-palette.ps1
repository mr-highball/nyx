# Nyx canvas content palette qualification.
# Copyright (c) 2020 mr-highball
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$EvidenceDirectory,
  [string]$Widgetset = 'win32'
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$nyxPaletteRoot = [IO.Path]::GetFullPath($EvidenceDirectory)
if (Test-Path -LiteralPath $nyxPaletteRoot) {
  throw 'Canvas palette qualification requires a fresh owned evidence directory'
}
$nyxPaletteTools = Get-Content -LiteralPath '.local/toolchain.json' -Raw | ConvertFrom-Json
$nyxPaletteCore = Join-Path $nyxPaletteRoot 'core'
$nyxPaletteControls = Join-Path $nyxPaletteRoot 'controls'
$nyxPaletteWeb = Join-Path $nyxPaletteRoot 'web'
$nyxPaletteExports = Join-Path $nyxPaletteRoot 'exports'
$nyxPaletteProject = Join-Path $nyxPaletteRoot 'owned-studio'
New-Item -ItemType Directory -Force $nyxPaletteCore,$nyxPaletteControls,$nyxPaletteWeb | Out-Null

# Orchestration only: Pascal owns form routing, model/source admission, exact
# constructor comparison and actual widget input. This runner starts no server.
function Invoke-PaletteTool([string]$Executable, [string[]]$Arguments, [string]$Log) {
  & $Executable @Arguments *> $Log
  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $Log -Tail 15
    throw "Canvas palette qualification failed; retained $Log"
  }
  if (Select-String -LiteralPath $Log -Pattern '^\s*[1-9][0-9]* unfreed memory blocks\s*:' -Quiet) {
    throw "Native consumer retained heap allocations; inspect $Log"
  }
  Write-Host "Passed $([IO.Path]::GetFileName($Log))"
}
$nyxPaletteFlags = @('-B','-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
  '-Fusrc','-Fustudio','-Futests','-Fisrc')
Invoke-PaletteTool $nyxPaletteTools.FPC ($nyxPaletteFlags + @(
  "-FU$nyxPaletteCore","-FE$nyxPaletteCore",'tests/nyx_canvas_palette_tests.lpr')) (
  Join-Path $nyxPaletteRoot 'contracts-native-compile.log')
Invoke-PaletteTool (Join-Path $nyxPaletteCore 'nyx_canvas_palette_tests.exe') @(
  $nyxPaletteExports) (Join-Path $nyxPaletteRoot 'contracts-native-run.log')

$nyxPaletteLCL = $nyxPaletteTools.LAZARUS
$nyxPalettePlatform = "$((& $nyxPaletteTools.LCL_FPC '-iTP').Trim())-$((& $nyxPaletteTools.LCL_FPC '-iTO').Trim())"
$nyxPaletteLCLFlags = $nyxPaletteFlags + @(
  "-Fu$nyxPaletteLCL/lcl/units/$nyxPalettePlatform",
  "-Fu$nyxPaletteLCL/lcl/units/$nyxPalettePlatform/$Widgetset",
  "-Fu$nyxPaletteLCL/components/lazutils/lib/$nyxPalettePlatform",
  "-Fu$nyxPaletteLCL/packager/units/$nyxPalettePlatform",
  "-FU$nyxPaletteControls","-FE$nyxPaletteControls")
Invoke-PaletteTool $nyxPaletteTools.LCL_FPC ($nyxPaletteLCLFlags + @(
  'tests/nyx_canvas_palette_studio_controls.lpr')) (Join-Path $nyxPaletteRoot 'studio-controls-native-compile.log')
Invoke-PaletteTool (Join-Path $nyxPaletteControls 'nyx_canvas_palette_studio_controls.exe') @(
  $nyxPaletteProject,(Join-Path $nyxPaletteRoot 'native-palette.png')) (
  Join-Path $nyxPaletteRoot 'studio-controls-native-run.log')

$nyxPaletteBrowserFlags = @('-B','-Mdelphi','-Tbrowser','-Jirtl.js',
  '-Fusrc','-Fustudio','-Futests','-Fisrc',"-FE$nyxPaletteWeb")
$nyxPaletteTemplate = Get-Content -LiteralPath 'studio/web/program-logic-controls.html' -Raw
foreach ($nyxPaletteConsumer in @('nyx_canvas_palette_tests','nyx_canvas_palette_studio_controls')) {
  Invoke-PaletteTool $nyxPaletteTools.PAS2JS ($nyxPaletteBrowserFlags + @(
    "tests/$nyxPaletteConsumer.lpr")) (Join-Path $nyxPaletteRoot "$nyxPaletteConsumer-browser-compile.log")
  [IO.File]::WriteAllText((Join-Path $nyxPaletteWeb "$nyxPaletteConsumer.html"),
    $nyxPaletteTemplate.Replace('nyx_program_logic_controls.js',"$nyxPaletteConsumer.js"),
    [Text.UTF8Encoding]::new($false))
}

foreach ($nyxPaletteCase in @(
    @{Name='new-control';Source=(Join-Path $nyxPaletteExports 'new-control')},
    @{Name='reuse';Source=(Join-Path $nyxPaletteExports 'reuse')},
    @{Name='ui-new';Source=(Join-Path $nyxPaletteProject 'exports/ui-new')},
    @{Name='ui-reuse';Source=(Join-Path $nyxPaletteProject 'exports/ui-reuse')})) {
  $nyxPaletteSource = $nyxPaletteCase.Source
  $nyxPaletteNative = Join-Path $nyxPaletteRoot ("constructors/" + $nyxPaletteCase.Name)
  $nyxPaletteBrowser = Join-Path $nyxPaletteWeb $nyxPaletteCase.Name
  New-Item -ItemType Directory -Force $nyxPaletteNative,$nyxPaletteBrowser | Out-Null
  # Compile each different companion against its own search/output directory.
  # A prior stage's generated unit must never satisfy the current constructor.
  Invoke-PaletteTool $nyxPaletteTools.FPC @('-Mdelphi','-Sa','-Cr','-Co','-Ci','-gl','-gh',
    "-Fu$nyxPaletteSource","-Fu$nyxPaletteControls",'-Fusrc','-Fustudio','-Fisrc',
    "-FU$nyxPaletteNative","-FE$nyxPaletteNative",'tests/nyx_canvas_palette_compiled.lpr') (
    Join-Path $nyxPaletteNative 'compile.log')
  Invoke-PaletteTool (Join-Path $nyxPaletteNative 'nyx_canvas_palette_compiled.exe') @(
    (Join-Path $nyxPaletteSource 'design.nyx')) (Join-Path $nyxPaletteNative 'run.log')
  Invoke-PaletteTool $nyxPaletteTools.PAS2JS ($nyxPaletteBrowserFlags + @(
    "-Fu$nyxPaletteSource","-FE$nyxPaletteBrowser",'tests/nyx_canvas_palette_compiled.lpr')) (
    Join-Path $nyxPaletteBrowser 'compile.log')
  Copy-Item -LiteralPath (Join-Path $nyxPaletteSource 'design.nyx'),$nyxPaletteTools.PAS2JS_RUNTIME -Destination $nyxPaletteBrowser
  [IO.File]::WriteAllText((Join-Path $nyxPaletteBrowser 'index.html'),
    $nyxPaletteTemplate.Replace('nyx_program_logic_controls.js','nyx_canvas_palette_compiled.js'),
    [Text.UTF8Encoding]::new($false))
}
Invoke-PaletteTool $nyxPaletteTools.LCL_FPC ($nyxPaletteLCLFlags + @(
  'studio/nyx_studio_native.lpr')) (Join-Path $nyxPaletteRoot 'studio-native-compile.log')
Invoke-PaletteTool $nyxPaletteTools.PAS2JS ($nyxPaletteBrowserFlags + @(
  'studio/nyx_studio.lpr')) (Join-Path $nyxPaletteRoot 'studio-browser-compile.log')
Invoke-PaletteTool $nyxPaletteTools.PAS2JS @('-B','-Mdelphi','-Tmodule','-Jirtl.js',
  '-Fusrc','-Fustudio','-Fisrc',"-FE$nyxPaletteWeb",'studio/nyx_source_worker.lpr') (
  Join-Path $nyxPaletteRoot 'source-worker-compile.log')
Copy-Item -LiteralPath $nyxPaletteTools.PAS2JS_RUNTIME -Destination $nyxPaletteWeb
Write-Host 'Portable/native Studio and exact native constructor checks passed.'
Write-Host 'Browser consumers and matched worker are compiled/staged only. No HTTP server was started.'
