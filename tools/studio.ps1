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
  # A prepared release launches directly without compiling Studio. Repository
  # hosts may also separate writable runtime and staged web from source. Release
  # payloads retain their own manifest-owned web directory.
  [string]$ReleaseRoot,
  [string]$RuntimeRoot,
  [string]$EnrollmentRoot,
  [int]$MCPPort = 0,
  # Appended so existing positional release/port invocations retain their shape.
  [string]$WebRoot
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
$nyxPreviousTemp = $env:TEMP
$nyxPreviousTmp = $env:TMP
$nyxPreviousRuntime = $env:NYX_STUDIO_RUNTIME_ROOT
$nyxPreviousEnrollment = $env:NYX_STUDIO_ENROLLMENT_ROOT
try {
  # Installed defaults remain distinct from explicit repository/test hosts.
  # Supplying -Server or -WebRoot keeps the earlier repository invocation unless
  # the caller explicitly also selects -ReleaseRoot.
  if (-not $ReleaseRoot -and -not $Server -and -not $WebRoot) {
    $ReleaseRoot = Get-NyxConfiguration 'STUDIO_RELEASE_ROOT'
  }
  if ($ReleaseRoot -and -not $RuntimeRoot) {
    $RuntimeRoot = Get-NyxConfiguration 'STUDIO_RUNTIME_ROOT'
  }
  if ($ReleaseRoot -and -not $EnrollmentRoot) {
    $EnrollmentRoot = Get-NyxConfiguration 'STUDIO_ENROLLMENT_ROOT'
  }

  if ($ReleaseRoot -and -not $RuntimeRoot) {
    throw 'Supply -RuntimeRoot for an installed release.'
  }
  if ($ReleaseRoot -and $WebRoot) {
    throw 'A release uses its own verified web payload; omit -WebRoot.'
  }
  $nyxTemp = Get-NyxConfiguration 'TEMP_ROOT'
  if ($nyxTemp) {
    $nyxTemp = [IO.Path]::GetFullPath($nyxTemp)
    New-Item -ItemType Directory -Force -Path $nyxTemp | Out-Null
    $env:TEMP = $nyxTemp
    $env:TMP = $nyxTemp
  }
  $nyxScratch = Get-NyxConfiguration 'SCRATCH_ROOT'
  if (-not $ReleaseRoot) {
    if (-not $RuntimeRoot) { $RuntimeRoot = Get-NyxConfiguration 'STUDIO_RUNTIME_ROOT' }
    # Durable projects move only through an explicit runtime choice. Configuring
    # compiler scratch alone must not silently select an empty project store.
    if (-not $WebRoot) { $WebRoot = Get-NyxConfiguration 'STUDIO_WEB_ROOT' }
    if (-not $WebRoot -and $nyxScratch) { $WebRoot = Join-Path $nyxScratch 'browser' }
    if (-not $WebRoot) { $WebRoot = Join-Path $nyxRoot 'build/browser' }
  }

  if (-not $SkipBuild -and -not $ReleaseRoot) {
    & (Join-Path $PSScriptRoot 'build.ps1') -Target studio -BrowserOutput $WebRoot
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

    if (-not $nyxServer) { $nyxServer = Get-NyxConfiguration 'STUDIO_SERVER' }
    if (-not $nyxServer) { $nyxServer = $nyxPackagedServer }
    $nyxSelectedServer = (Resolve-Path -LiteralPath $nyxServer).Path

    if ($nyxSelectedServer -ne $nyxPackagedServer) {
      # The existing firewall allowance may name a fixed executable slot. Its
      # staged bytes must still belong to this bundle. Do not copy over a live
      # owner, choose a fresh listener path or silently launch another binary.
      $nyxSelectedItem = Get-Item -LiteralPath $nyxSelectedServer
      if ($nyxSelectedItem.PSIsContainer -or
        (($nyxSelectedItem.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) -or
        ((Get-FileHash -LiteralPath $nyxSelectedServer -Algorithm SHA256).Hash -ne
         (Get-FileHash -LiteralPath $nyxPackagedServer -Algorithm SHA256).Hash)) {
        throw 'The fixed release backend must be an ordinary file matching the bundle. Stage it after stopping its owner.'
      }
    }
    # Pascal verifies the complete immutable payload before opening listeners;
    # the external executable choice does not change web/source/runtime roots.
    $nyxServer = $nyxSelectedServer
  }
  elseif (-not $nyxServer) {
    if ($SkipBuild) { $nyxServer = Get-NyxConfiguration 'STUDIO_SERVER' }
    $nyxNativeSearch = Join-Path $nyxRoot 'build/native'
    if ($nyxScratch) { $nyxNativeSearch = Join-Path $nyxScratch 'native' }
    $nyxServerFile = $null
    if (-not $nyxServer -and (Test-Path -LiteralPath $nyxNativeSearch)) {
      $nyxServerFile = Get-ChildItem -LiteralPath $nyxNativeSearch -Recurse -File |
      Where-Object { $_.Name -in @('nyx_studio_server.exe', 'nyx_studio_server') } |
      Sort-Object LastWriteTime -Descending | Select-Object -First 1
    }

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
    # Host-only environment preserves the established positional release CLI.
    # The Pascal entry point applies copied typed directories before any write.
    $env:NYX_STUDIO_RUNTIME_ROOT = $null
    if ($RuntimeRoot) { $env:NYX_STUDIO_RUNTIME_ROOT = [IO.Path]::GetFullPath($RuntimeRoot) }
    $env:NYX_STUDIO_ENROLLMENT_ROOT = $nyxRoot
    if ($EnrollmentRoot) {
      $env:NYX_STUDIO_ENROLLMENT_ROOT = [IO.Path]::GetFullPath($EnrollmentRoot)
    }
    $WebRoot = [IO.Path]::GetFullPath($WebRoot)
    foreach ($nyxWebFile in @('index.html','agent-preview.html','rtl.js',
        'nyx_studio.js','nyx_source_worker.js','nyx_studio_preview.js')) {
      $nyxWebPath = Join-Path $WebRoot $nyxWebFile
      if (-not (Test-Path -LiteralPath $nyxWebPath -PathType Leaf) -or
          (Get-Item -LiteralPath $nyxWebPath).Length -eq 0) {
        throw ('Studio web payload is incomplete: '+$nyxWebFile)
      }
    }
    & $nyxServer $nyxRoot $Port $BindAddress $MCPPort $WebRoot
  }

  if ($LASTEXITCODE -ne 0) {
    throw "Studio server exited with $LASTEXITCODE"
  }
} finally {
  $env:TEMP = $nyxPreviousTemp
  $env:TMP = $nyxPreviousTmp
  $env:NYX_STUDIO_RUNTIME_ROOT = $nyxPreviousRuntime
  $env:NYX_STUDIO_ENROLLMENT_ROOT = $nyxPreviousEnrollment
  Pop-Location
}
