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
<#
.SYNOPSIS
Prepares and runs the existing Pascal HTTP host for an isolated browser review.
.DESCRIPTION
Uses the existing .local/toolchain.json without installing or configuring tools.
Fresh outputs use .local/qualification-paths-private.json's qualificationRoot
when available, otherwise build/browser-review. OutputRoot overrides that choice.
Only compiled review files enter the served web directory; compiler files,
logs and private receipts stay outside it. No Studio project or enrollment is read.

PrepareOnly compiles/stages files without executing a server or browser.
UsePrepared validates the most recently prepared artifact hashes and skips builds.
The normal run stays in this console until Ctrl+C. The Pascal server binds only
loopback, serves flat read-only files, and refuses an occupied port; this script
never stops an existing process, changes firewall rules or chooses another port.
.EXAMPLE
./tools/start-browser-review.ps1 -PrepareOnly
.EXAMPLE
./tools/start-browser-review.ps1 -UsePrepared
#>
[CmdletBinding(DefaultParameterSetName = 'Prepare')]
param(
  [Parameter(ParameterSetName = 'Prepare')]
  [string]$OutputRoot = '',
  [Parameter(Mandatory = $true, ParameterSetName = 'Prepared')]
  [switch]$UsePrepared,
  [ValidateRange(1024, 65535)]
  [int]$Port = 8766,
  [switch]$PrepareOnly
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$nyxRoot = [IO.Path]::GetFullPath((Split-Path $PSScriptRoot -Parent))
$nyxPointerPath = Join-Path $nyxRoot '.local/browser-review-host-private.json'
$nyxArtifactNames = @('native/nyx_preview_host.exe',
  'web/nyx_canvas_accessibility_review.js', 'web/index.html')

# Walk existing ancestors as well as the leaf. Serving through a junction could
# expose a different folder from the one reviewed in the private packet.
function Assert-NyxPlainPath([string]$Path) {
  $cursor = [IO.Path]::GetFullPath($Path)
  while ($cursor) {

    if (Test-Path -LiteralPath $cursor) {
      $item = Get-Item -LiteralPath $cursor -Force

      if (($item.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
        throw "Refusing a linked review path: $cursor"
      }
    }
    $parent = Split-Path -Path $cursor -Parent

    if ($parent -eq $cursor) {
      break
    }
    $cursor = $parent
  }
}

# Shell orchestration only: the existing Pascal consumer owns every assertion.
# Separate logs and exit checks preserve compiler failures without starting a host.
function Invoke-NyxReviewCompiler([string]$Executable, [string[]]$Arguments,
  [string]$LogPath) {
  & $Executable @Arguments *> $LogPath

  if ($LASTEXITCODE -ne 0) {
    Get-Content -LiteralPath $LogPath -Tail 25 | Write-Host
    throw "Review compilation failed; see $LogPath"
  }
}

if ($UsePrepared) {
  Assert-NyxPlainPath $nyxPointerPath
  $nyxPointer = Get-Content -LiteralPath $nyxPointerPath -Raw | ConvertFrom-Json

  if ($nyxPointer.schema -ne 1) {
    throw 'Unrecognized browser-review pointer; prepare a fresh packet.'
  }
  $nyxPacket = [IO.Path]::GetFullPath([string]$nyxPointer.packet)
}
else {
  $nyxToolPath = Join-Path $nyxRoot '.local/toolchain.json'
  $nyxTools = Get-Content -LiteralPath $nyxToolPath -Raw | ConvertFrom-Json
  foreach ($name in @('FPC', 'PAS2JS', 'PAS2JS_RUNTIME')) {

    if (-not (Test-Path -LiteralPath $nyxTools.$name -PathType Leaf)) {
      throw "Existing $name tool/runtime is unavailable; check $nyxToolPath"
    }
  }

  if (-not $OutputRoot) {
    $nyxStoragePath = Join-Path $nyxRoot '.local/qualification-paths-private.json'

    if (Test-Path -LiteralPath $nyxStoragePath -PathType Leaf) {
      $nyxStorage = Get-Content -LiteralPath $nyxStoragePath -Raw | ConvertFrom-Json
      $OutputRoot = [string]$nyxStorage.qualificationRoot
    }
    else {
      $OutputRoot = Join-Path $nyxRoot 'build/browser-review'
    }
  }
  $nyxStamp = Get-Date -Format 'yyyyMMdd-HHmmss'
  $nyxSuffix = [Guid]::NewGuid().ToString('N').Substring(0, 8)
  $nyxPacket = Join-Path ([IO.Path]::GetFullPath($OutputRoot)) "browser-review-$nyxStamp-$nyxSuffix"
  Assert-NyxPlainPath $nyxPacket
  foreach ($name in @('native', 'web', 'logs', 'scratch')) {
    New-Item -ItemType Directory -Path (Join-Path $nyxPacket $name) | Out-Null
  }
  $nyxNative = Join-Path $nyxPacket 'native'
  $nyxWeb = Join-Path $nyxPacket 'web'
  $nyxPreviousTemp = $env:TEMP
  $nyxPreviousTmp = $env:TMP
  Push-Location -LiteralPath $nyxRoot
  try {
    $env:TEMP = Join-Path $nyxPacket 'scratch'
    $env:TMP = $env:TEMP
    Invoke-NyxReviewCompiler $nyxTools.FPC @('-B', '-Mdelphi', '-Sa', '-Cr',
      '-Co', '-Ci', '-gl', '-Fusrc', '-Fisrc', "-FU$nyxNative", "-FE$nyxNative",
      'tools/nyx_preview_host.lpr') (Join-Path $nyxPacket 'logs/host-compile.log')
    # Tmodule emits the startup call as compiler output. The browser consumer
    # still uses Web/DOM; no handwritten JavaScript bootstrap or framework enters
    # the page. Insert the exact runtime selected beside the existing compiler.
    Invoke-NyxReviewCompiler $nyxTools.PAS2JS @('-B', '-Mdelphi', '-Tmodule',
      '-Fusrc', '-Fustudio', '-Futests', '-Futools', '-Fisrc',
      "-Ji$($nyxTools.PAS2JS_RUNTIME)", "-FE$nyxWeb",
      'tools/nyx_canvas_accessibility_review.lpr') (
      Join-Path $nyxPacket 'logs/review-compile.log')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/canvas-accessibility-review.html') `
      -Destination (Join-Path $nyxWeb 'index.html')
  }
  finally {
    $env:TEMP = $nyxPreviousTemp
    $env:TMP = $nyxPreviousTmp
    Pop-Location
  }
  $nyxArtifacts = foreach ($name in $nyxArtifactNames) {
    $path = Join-Path $nyxPacket $name
    [ordered]@{ name = $name; sha256 = (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash }
  }
  [ordered]@{
    schema = 1
    purpose = 'user-started-browser-review'
    repository = $nyxRoot
    prepared = [DateTime]::UtcNow.ToString('o')
    artifacts = @($nyxArtifacts)
  } | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (
    Join-Path $nyxPacket 'prepared-private.json') -Encoding UTF8
  Assert-NyxPlainPath $nyxPointerPath
  New-Item -ItemType Directory -Force -Path (Split-Path $nyxPointerPath -Parent) | Out-Null
  [ordered]@{ schema = 1; packet = $nyxPacket } | ConvertTo-Json |
    Set-Content -LiteralPath $nyxPointerPath -Encoding UTF8
}

# Prepared launch admits only the fixed three artifacts from this repository.
# A stale or edited executable/page refuses instead of silently rebuilding it.
$nyxReceiptPath = Join-Path $nyxPacket 'prepared-private.json'
Assert-NyxPlainPath $nyxReceiptPath
$nyxReceipt = Get-Content -LiteralPath $nyxReceiptPath -Raw | ConvertFrom-Json

if (($nyxReceipt.schema -ne 1) -or
    ($nyxReceipt.purpose -ne 'user-started-browser-review') -or
    ($nyxReceipt.repository -ne $nyxRoot) -or
    (@($nyxReceipt.artifacts).Count -ne $nyxArtifactNames.Count)) {
  throw 'Prepared browser-review receipt does not match this repository.'
}
foreach ($name in $nyxArtifactNames) {
  $path = Join-Path $nyxPacket $name
  Assert-NyxPlainPath $path
  $entries = @($nyxReceipt.artifacts | Where-Object { $_.name -ceq $name })

  if (($entries.Count -ne 1) -or
      ((Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash -ne $entries[0].sha256)) {
    throw "Prepared artifact changed; prepare a fresh packet: $name"
  }
}

$nyxHostExecutable = Join-Path $nyxPacket 'native/nyx_preview_host.exe'
$nyxWebDirectory = Join-Path $nyxPacket 'web'
$nyxReviewURL = "http://127.0.0.1:$Port/index.html"
Write-Host "Review packet: $nyxPacket"
Write-Host "Review URL:    $nyxReviewURL"

if ($PrepareOnly) {
  Write-Host 'Prepared and hash-checked. No HTTP host or browser was started.'
  Write-Host 'To run in a console: ./tools/start-browser-review.ps1 -UsePrepared'
  return
}

Write-Host 'Starting the read-only Pascal test host. Leave this console open; Ctrl+C stops it.'
Write-Host 'This is an isolated browser review, not Nyx Studio or its LAN service.'
& $nyxHostExecutable $nyxWebDirectory $Port

if ($LASTEXITCODE -ne 0) {
  throw "Preview host exited with code $LASTEXITCODE. No other process was stopped."
}
