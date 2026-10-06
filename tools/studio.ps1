# MIT License
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
#
# Nyx Studio launcher. Compiles through the shared build entry point, then runs
# the Pascal service in the foreground so stopping the terminal stops Studio.
[CmdletBinding()]
param(
  [int]$Port = 8088,
  [string]$BindAddress,
  [switch]$SkipBuild,
  [string]$Server,
  [string]$ToolchainConfiguration,
  # A prepared release launches directly without compiling Studio. RuntimeRoot
  # holds private jobs/profiles/projects; EnrollmentRoot optionally selects the
  # project whose local Codex entry should discover this instance.
  [string]$ReleaseRoot,
  [string]$RuntimeRoot,
  [string]$EnrollmentRoot,
  [int]$MCPPort = 0
)

$ErrorActionPreference = 'Stop'
$nyxRoot = Split-Path -Parent $PSScriptRoot
$nyxConfigPath = Join-Path $nyxRoot '.local/toolchain.json'

if ($ToolchainConfiguration) {
  $nyxConfigPath = $ToolchainConfiguration
}
$nyxConfig = @{}

if (Test-Path -LiteralPath $nyxConfigPath) {
  $nyxConfig = Get-Content -LiteralPath $nyxConfigPath -Raw | ConvertFrom-Json -AsHashtable
}

function Get-NyxConfiguration([string]$Key, [string]$Command = '') {
  $nyxValue = [Environment]::GetEnvironmentVariable('NYX_' + $Key)

  if (-not $nyxValue -and $nyxConfig.ContainsKey($Key)) {
    $nyxValue = $nyxConfig[$Key]
  }

  if (-not $nyxValue -and $Command) {
    $nyxTool = Get-Command $Command -ErrorAction SilentlyContinue

    if ($nyxTool) {
      $nyxValue = $nyxTool.Source
    }
  }

  # Output hints are optional. The Pascal service checks the chosen profile
  # when a build is requested; opening a built Studio needs no output compiler.
  return $nyxValue
}

Push-Location $nyxRoot
try {

  if ([bool]$ReleaseRoot -ne [bool]$RuntimeRoot) {
    throw 'Supply both -ReleaseRoot and -RuntimeRoot for an installed release.'
  }

  if (-not $SkipBuild -and -not $ReleaseRoot) {
    & (Join-Path $PSScriptRoot 'build.ps1') -Target studio
  }
  $env:NYX_PAS2JS = Get-NyxConfiguration 'PAS2JS' 'pas2js'
  $env:NYX_PAS2JS_RUNTIME = Get-NyxConfiguration 'PAS2JS_RUNTIME'
  $env:NYX_LAZARUS = Get-NyxConfiguration 'LAZARUS'
  $nyxNativeCompiler = Get-NyxConfiguration 'LCL_FPC'

  if ($nyxNativeCompiler) {
    $env:NYX_FPC = $nyxNativeCompiler
  }
  $env:NYX_LCL_PLATFORM = Get-NyxConfiguration 'LCL_PLATFORM'
  $env:NYX_LCL_WIDGETSET = Get-NyxConfiguration 'LCL_WIDGETSET'

  if (-not $env:NYX_LCL_PLATFORM -and $env:NYX_FPC -and
      (Test-Path -LiteralPath $env:NYX_FPC -PathType Leaf)) {
    try {
      $env:NYX_LCL_PLATFORM = "$((& $env:NYX_FPC '-iTP').Trim())-$((& $env:NYX_FPC '-iTO').Trim())"
    } catch {
      Write-Warning 'Native platform hint unavailable; configure it in Studio Outputs.'
    }
  }

  if (-not $env:NYX_LCL_WIDGETSET) {
    $env:NYX_LCL_WIDGETSET = 'win32'

    if ($IsLinux) {
      $env:NYX_LCL_WIDGETSET = 'gtk2'
    }
  }
  # Locate an existing service without querying FPC. A packaged or previously
  # built Studio can be launched after compilers have been removed. -Server
  # selects a specific binary when several local build profiles exist.
  $nyxServer = $Server

  if ($ReleaseRoot) {
    $nyxReleaseRoot = (Resolve-Path -LiteralPath $ReleaseRoot).Path
    # Earlier pristine candidates predate the separate-runtime entry point.
    # Refuse them here; only the maintained compiler-source bundle supplies the
    # new host unit and backend together. Pascal verifies all bytes at startup.
    if (-not (Test-Path -LiteralPath (Join-Path $nyxReleaseRoot 'studio/nyx.studio.directories.pas'))) {
      throw 'This candidate predates separate runtime storage; prepare a current Studio release.'
    }
    $nyxPackagedServer = Join-Path $nyxReleaseRoot 'bin/nyx_studio_server.exe'

    if (-not $IsWindows) {
      $nyxPackagedServer = Join-Path $nyxReleaseRoot 'bin/nyx_studio_server'
    }

    if ($nyxServer -and (Resolve-Path -LiteralPath $nyxServer).Path -ne $nyxPackagedServer) {
      throw 'A release launch must use its own staged backend executable.'
    }
    $nyxServer = $nyxPackagedServer
  }
  elseif (-not $nyxServer) {
    $nyxServerFile = Get-ChildItem -LiteralPath (Join-Path $nyxRoot 'build/native') -Recurse -File |
      Where-Object { $_.Name -in @('nyx_studio_server.exe', 'nyx_studio_server') } |
      Sort-Object LastWriteTime -Descending | Select-Object -First 1

    if ($nyxServerFile) {
      $nyxServer = $nyxServerFile.FullName
    }
  }

  if (-not $nyxServer -or -not (Test-Path -LiteralPath $nyxServer -PathType Leaf)) {
    throw 'A built Studio service is required. Build Studio or supply -Server.'
  }
  # Bind configuration belongs to this host, never the portable design. An
  # explicit parameter wins over NYX_STUDIO_BIND and the ignored local hints.

  if (-not $BindAddress) {
    $BindAddress = Get-NyxConfiguration 'STUDIO_BIND'
  }

  if (-not $BindAddress) {
    $BindAddress = '127.0.0.1'
  }
  if ($ReleaseRoot) {
    $nyxRuntimeRoot = [IO.Path]::GetFullPath($RuntimeRoot)
    $nyxEnrollmentRoot = $nyxRuntimeRoot

    if ($EnrollmentRoot) {
      $nyxEnrollmentRoot = [IO.Path]::GetFullPath($EnrollmentRoot)
    }
    $nyxReleaseWeb = Join-Path $nyxReleaseRoot 'web'
    # Pass every slot explicitly: older PowerShell native argument handling can
    # discard an empty string and shift the runtime/enrollment arguments.
    & $nyxServer $nyxReleaseRoot $Port $BindAddress $MCPPort $nyxReleaseWeb $nyxRuntimeRoot $nyxEnrollmentRoot
  }
  else {
    & $nyxServer $nyxRoot $Port $BindAddress $MCPPort
  }

  if ($LASTEXITCODE -ne 0) {
    throw "Studio server exited with $LASTEXITCODE"
  }
} finally {
  Pop-Location
}
