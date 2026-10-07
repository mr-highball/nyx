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
# Windows firewall orchestration for local device review. Run once from an
# administrator PowerShell. The service must separately bind to the LAN using
# studio.ps1 -BindAddress 0.0.0.0; no compiler or design change is involved here.
[CmdletBinding()]
param(
  [ValidateRange(1024, 65535)]
  [int]$Port = 8088,
  [string]$Server
)

$ErrorActionPreference = 'Stop'
$nyxRoot = Split-Path -Parent $PSScriptRoot
$nyxIdentity = [Security.Principal.WindowsIdentity]::GetCurrent()
$nyxPrincipal = [Security.Principal.WindowsPrincipal]::new($nyxIdentity)

if (-not $nyxPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
  throw 'Windows firewall changes require an administrator PowerShell. Run this script there.'
}

if (-not $Server) {
  # Prefer the process actually serving this port. Frozen releases live outside
  # build/native, so the newest development binary need not be the LAN owner.
  # Administrator execution is already required for the scoped firewall update.
  $nyxListeners = @(Get-NetTCPConnection -State Listen -LocalPort $Port -ErrorAction SilentlyContinue |
    Select-Object -ExpandProperty OwningProcess -Unique)
  $nyxServing = @($nyxListeners | ForEach-Object {
    Get-CimInstance Win32_Process -Filter ('ProcessId=' + $_)
  } | Where-Object { $_.Name -eq 'nyx_studio_server.exe' })

  if ($nyxServing.Count -gt 1) {
    throw 'Several Studio processes serve this port. Pass -Server with the intended executable.'
  }

  if ($nyxServing.Count -eq 1) {
    $Server = $nyxServing[0].ExecutablePath
  }
}

if (-not $Server) {
  $nyxServerFile = Get-ChildItem -LiteralPath (Join-Path $nyxRoot 'build/native') -Recurse -File |
    Where-Object { $_.Name -eq 'nyx_studio_server.exe' } |
    Sort-Object LastWriteTime -Descending | Select-Object -First 1

  if ($nyxServerFile) {
    $Server = $nyxServerFile.FullName
  }
}

if (-not $Server -or -not (Test-Path -LiteralPath $Server -PathType Leaf)) {
  throw 'Build Studio first or pass -Server with its executable path.'
}
$nyxServerPath = (Resolve-Path -LiteralPath $Server).Path
$nyxRuleName = 'Nyx-Studio-LAN-' + $Port
$nyxAllowRule = Get-NetFirewallRule -Name $nyxRuleName -ErrorAction SilentlyContinue

# Only the chosen executable/port is admitted, on Private networks and from the
# local subnet. Re-running updates the same named rule after a compiler change.

if ($nyxAllowRule) {
  $nyxAllowRule | Set-NetFirewallRule -Enabled True -Direction Inbound -Action Allow `
    -Program $nyxServerPath -Protocol TCP -LocalPort $Port -Profile Private `
    -RemoteAddress LocalSubnet | Out-Null
} else {
  New-NetFirewallRule -Name $nyxRuleName -DisplayName 'Nyx Studio LAN review' `
    -Enabled True -Direction Inbound -Action Allow -Program $nyxServerPath `
    -Protocol TCP -LocalPort $Port -Profile Private -RemoteAddress LocalSubnet | Out-Null
}

# Windows may have created explicit block rules when its first-run prompt was
# dismissed. Blocks override allows. Remove Private from matching TCP/Any blocks
# while retaining their other network profiles; leave unrelated/UDP rules alone.
$nyxBlocks = Get-NetFirewallApplicationFilter |
  Where-Object { $_.Program -ieq $nyxServerPath } | Get-NetFirewallRule |
  Where-Object { $_.Enabled -eq 'True' -and $_.Direction -eq 'Inbound' -and $_.Action -eq 'Block' }
foreach ($nyxBlock in $nyxBlocks) {
  $nyxProtocol = ($nyxBlock | Get-NetFirewallPortFilter).Protocol

  if ($nyxProtocol -notin @('TCP', '6', 'Any')) {
    continue
  }
  $nyxProfiles = @($nyxBlock.Profile.ToString().Split(',') | ForEach-Object { $_.Trim() })

  if ('Any' -in $nyxProfiles) {
    $nyxProfiles = @('Domain', 'Private', 'Public')
  }

  if ('Private' -notin $nyxProfiles) {
    continue
  }
  $nyxRemainingProfiles = @($nyxProfiles | Where-Object { $_ -ne 'Private' })

  if ($nyxRemainingProfiles.Count -gt 0) {
    $nyxBlock | Set-NetFirewallRule -Profile $nyxRemainingProfiles | Out-Null
  } else {
    $nyxBlock | Disable-NetFirewallRule | Out-Null
  }
}
Write-Host "Studio LAN access allowed for TCP $Port on Private networks (local subnet)."
