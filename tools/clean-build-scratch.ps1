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
Preview or remove stale Nyx compiler intermediates and isolated test caches.
.DESCRIPTION
Uses native PowerShell filesystem operations only. Preview is the default;
-Apply admits deletion and -WhatIf remains available. No directory is removed.
Source, logs, captures, executables, project stores, browser storage/preferences,
sealed packages and runtime/job trees remain outside the deletion list. Cache
cleanup addresses only GUID-named qualification browser profiles, never the
operator's ordinary browser profile. Downloaded browser provenance models count
as cache data; the retired test browser can download them again if reopened.

Normal preview/apply runs write fresh manifests and preservation/deletion receipts;
-WhatIf suppresses those audit writes as well as deletions.
An interrupted deletion can be partial: inspect deleted.csv and summary.json.
The script refuses active Nyx/compiler processes or browsers using either root.
It does not stop a process, launch a host or alter a compiler/configuration.
.EXAMPLE
./tools/clean-build-scratch.ps1
.EXAMPLE
./tools/clean-build-scratch.ps1 -Apply
.EXAMPLE
./tools/clean-build-scratch.ps1 -AuditDirectory <fresh-external-path> -Apply -WhatIf
#>
[CmdletBinding(SupportsShouldProcess=$true, ConfirmImpact='Medium')]
param(
  [string]$SourceRoot = (Split-Path $PSScriptRoot -Parent),
  [string]$QualificationRoot = (Join-Path (
    [Environment]::GetFolderPath('LocalApplicationData')) 'Nyx/qualification'),
  [string]$AuditDirectory,
  [ValidateRange(0,8760)][double]$MinimumAgeHours = 1,
  [switch]$Apply
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$checkedDirectories = [Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)

# Resolve existing roots without following an ancestor junction into another
# workspace. The same guard protects each admitted file immediately before use.
function Assert-NyxPlainPath([string]$Path) {
  $item = Get-Item -LiteralPath $Path -Force
  $cursor = $item
  while ($null -ne $cursor) {

    if ($cursor -is [IO.DirectoryInfo] -and
        -not $checkedDirectories.Add($cursor.FullName)) {
      break
    }

    if (($cursor.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
      throw "Refusing a linked cleanup path: $($cursor.FullName)"
    }

    if ($cursor -is [IO.FileInfo]) {
      $cursor = $cursor.Directory
    }
    else {
      $cursor = $cursor.Parent
    }
  }
  return $item
}

# Native processes must have joined before retiring their outputs. Browsers
# using unrelated personal profiles do not block this narrowly owned cleanup.
function Assert-NyxCleanupIdle([string[]]$Roots) {
  foreach ($process in @(Get-CimInstance Win32_Process)) {

    if ($process.Name -match '^(nyx.*|fpc|ppc[a-z0-9_]+|pas2js.*)\.exe$') {
      throw "Nyx/compiler process $($process.ProcessId) is active; cleanup refused"
    }

    if ($process.Name -match '^(msedge|chrome|brave|chromium)\.exe$') {

      if ([string]::IsNullOrEmpty($process.CommandLine)) {
        throw 'A browser command line is unavailable; profile ownership cannot be checked'
      }
      foreach ($root in $Roots) {

        if ($process.CommandLine.IndexOf($root, [StringComparison]::OrdinalIgnoreCase) -ge 0) {
          throw "Browser $($process.ProcessId) still uses a Nyx cleanup root"
        }
      }
    }
  }
}

$source = (Assert-NyxPlainPath $SourceRoot).FullName

if (-not (Test-Path -LiteralPath (Join-Path $source 'PROJECT.md') -PathType Leaf) -or
    -not (Test-Path -LiteralPath (Join-Path $source 'src') -PathType Container)) {
  throw 'SourceRoot must be the Nyx checkout containing PROJECT.md and src'
}
$roots = [Collections.Generic.List[string]]::new()
foreach ($possible in @((Join-Path $source 'build'), $QualificationRoot)) {

  if (Test-Path -LiteralPath $possible -PathType Container) {
    $ownedRoot = Assert-NyxPlainPath $possible

    if ($ownedRoot.Name -notin @('build', 'qualification')) {
      throw 'Cleanup roots must be the Nyx build or qualification directory'
    }
    foreach ($existing in $roots) {

      if ($ownedRoot.FullName.StartsWith($existing + '\', [StringComparison]::OrdinalIgnoreCase) -or
          $existing.StartsWith($ownedRoot.FullName.TrimEnd('\') + '\', [StringComparison]::OrdinalIgnoreCase) -or
          $existing.Equals($ownedRoot.FullName.TrimEnd('\'), [StringComparison]::OrdinalIgnoreCase)) {
        throw 'Cleanup roots must be distinct and must not contain each other'
      }
    }
    $roots.Add($ownedRoot.FullName.TrimEnd('\'))
  }
}

if ($roots.Count -eq 0) {
  throw 'No Nyx build or qualification root exists'
}
Assert-NyxCleanupIdle $roots.ToArray()

# Machine paths stay in ignored local configuration. A caller on another
# machine can supply a fresh external AuditDirectory instead of that pointer.
if ([string]::IsNullOrWhiteSpace($AuditDirectory)) {
  $pointer = Join-Path $source '.local/qualification-paths-private.json'

  if (-not (Test-Path -LiteralPath $pointer -PathType Leaf)) {
    throw 'Supply a fresh external -AuditDirectory; no local storage pointer exists'
  }
  $storage = Get-Content -LiteralPath $pointer -Raw | ConvertFrom-Json
  $auditParent = Split-Path $storage.auditRoot -Parent
  $auditName = 'manual-cleanup-' + (Get-Date -Format 'yyyyMMdd-HHmmss') + '-' +
    [Guid]::NewGuid().ToString('N').Substring(0,8)
  $AuditDirectory = Join-Path $auditParent $auditName
}
$audit = [IO.Path]::GetFullPath($AuditDirectory).TrimEnd('\')

if (Test-Path -LiteralPath $audit) {
  throw 'AuditDirectory must be fresh; previous receipts are retained'
}
foreach ($root in $roots) {

  if ($audit.Equals($root, [StringComparison]::OrdinalIgnoreCase) -or
      $audit.StartsWith($root + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'AuditDirectory must be outside every cleanup root'
  }
}
$auditAncestor = Split-Path $audit -Parent
while (-not (Test-Path -LiteralPath $auditAncestor)) {
  $auditAncestor = Split-Path $auditAncestor -Parent
}
$null = Assert-NyxPlainPath $auditAncestor
New-Item -ItemType Directory -Path $audit -Force | Out-Null

$cutoff = [DateTime]::UtcNow.AddHours(-$MinimumAgeHours)
$candidates = [Collections.Generic.List[object]]::new()
$preserved = [Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)
$queue = [Collections.Generic.Queue[object]]::new()
foreach ($root in $roots) {
  $queue.Enqueue([pscustomobject]@{ Directory=(Get-Item -LiteralPath $root); Root=$root; Cache=$false })
}

# Only known compiler derivatives are eligible: resource source, archives,
# executables and pas2js outputs are deliberately not inferred as disposable.
$compilerExtensions = @('.o', '.ppu', '.or', '.rsj', '.rst')
$cacheNames = @('component_crx_cache', 'ProvenanceData', 'ShaderCache', 'GrShaderCache',
  'DawnGraphiteCache', 'DawnWebGPUCache')
$defaultCaches = @('Cache', 'Code Cache', 'GPUCache', 'DawnCache',
  'DawnGraphiteCache', 'DawnWebGPUCache')
$scanned = 0
while ($queue.Count -gt 0) {
  $entry = $queue.Dequeue()
  $directory = $entry.Directory

  if (($directory.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0 -or
      $directory.Name -match '^(?i:release.*|.*runtime.*|jobs|frozen|sealed|\.local|\.git|node_modules)$') {
    continue
  }
  $manifest = Join-Path $directory.FullName 'release.nyx'

  if (Test-Path -LiteralPath $manifest -PathType Leaf) {
    $null = $preserved.Add($manifest)
    continue
  }

  if ($directory.Name -match '^profile-\{[0-9a-fA-F-]{36}\}$') {
    # Enter specific caches directly. Never traverse profile storage, history,
    # cookies, settings, extensions or the saved project's IndexedDB/localStorage.
    foreach ($relative in @($cacheNames) + @($defaultCaches | ForEach-Object { 'Default/' + $_ })) {
      $cachePath = Join-Path $directory.FullName $relative

      if (Test-Path -LiteralPath $cachePath -PathType Container) {
        # A linked Default ancestor must not redirect a cache shortcut into a
        # personal profile, even during a read-only inventory.
        $cache = Assert-NyxPlainPath $cachePath
        $queue.Enqueue([pscustomobject]@{ Directory=$cache; Root=$entry.Root; Cache=$true })
      }
    }
    continue
  }

  if ($directory.Name -match '^(?i:profile.*|user[- ]?data.*)$') {
    continue
  }
  $scanned++
  foreach ($file in $directory.EnumerateFiles()) {

    if (($file.Attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
      continue
    }

    if (($entry.Cache -or $file.Extension -in $compilerExtensions) -and
        $file.LastWriteTimeUtc -lt $cutoff) {
      $candidates.Add([pscustomobject]@{
        Path=$file.FullName; Root=$entry.Root; Bytes=$file.Length
        WriteTicks=$file.LastWriteTimeUtc.Ticks
        Category=$(if ($entry.Cache) { 'Retired test cache' } else { 'Compiler intermediate' })
      })
    }
  }
  foreach ($child in $directory.EnumerateDirectories()) {
    $queue.Enqueue([pscustomobject]@{ Directory=$child; Root=$entry.Root; Cache=$entry.Cache })
  }

  if (($scanned % 250) -eq 0) {
    Write-Progress -Activity 'Inventorying owned Nyx build/scratch files' -Status "$scanned directories"
  }
}
Write-Progress -Activity 'Inventorying owned Nyx build/scratch files' -Completed

# Hash the retained project/configuration checkpoints and discovered release
# manifests before/after. Their directories are excluded independently above.
$protectedPaths = @('.codex/config.toml', '.local/studio-outputs.nyx',
  'build/legacy-refresh/lan-runtime/.local/studio-session.nyx',
  'build/legacy-refresh/lan-runtime/.local/studio-outputs.nyx')
foreach ($relative in $protectedPaths) {
  $path = Join-Path $source $relative

  if (Test-Path -LiteralPath $path -PathType Leaf) {
    $null = $preserved.Add($path)
  }
}
$userConfig = Join-Path ([Environment]::GetFolderPath('UserProfile')) '.codex/config.toml'

if (Test-Path -LiteralPath $userConfig -PathType Leaf) {
  $null = $preserved.Add($userConfig)
}
$before = @($preserved | ForEach-Object {
  [pscustomobject]@{ Path=$_; Hash=(Get-FileHash -LiteralPath $_ -Algorithm SHA256).Hash }
})
$before | Export-Csv -LiteralPath (Join-Path $audit 'protected-before.csv') -NoTypeInformation
$candidates | Export-Csv -LiteralPath (Join-Path $audit 'candidates.csv') -NoTypeInformation
Get-PSDrive -PSProvider FileSystem | Select-Object Name,Used,Free |
  Export-Csv -LiteralPath (Join-Path $audit 'drives-before.csv') -NoTypeInformation

# Preflight the complete group before its first deletion. A file must still be
# ordinary, inside its resolved owned root, with the inventoried size and time.
if ($Apply -and -not $WhatIfPreference) {
  $checkedDirectories.Clear()
  foreach ($candidate in $candidates) {
    $absolute = [IO.Path]::GetFullPath($candidate.Path)

    if (-not $absolute.StartsWith($candidate.Root + '\', [StringComparison]::OrdinalIgnoreCase)) {
      throw 'Cleanup candidate escapes its resolved owned root'
    }
    $item = Assert-NyxPlainPath $absolute

    if ($item.PSIsContainer -or $item.Length -ne $candidate.Bytes -or
        $item.LastWriteTimeUtc.Ticks -ne $candidate.WriteTicks) {
      throw "Candidate changed after inventory: $absolute"
    }
  }
}
Assert-NyxCleanupIdle $roots.ToArray()
$deleted = [Collections.Generic.List[object]]::new()
$plannedBytes = [long]0
foreach ($candidate in $candidates) {
  $plannedBytes += $candidate.Bytes
}
$deletedBytes = [long]0
$failure = $null
try {

  if ($Apply -and $candidates.Count -gt 0 -and
      $PSCmdlet.ShouldProcess("$($candidates.Count) verified files ($plannedBytes bytes)",
        'Delete stale Nyx compiler intermediates and isolated test caches')) {
    # Check ancestry afresh after preflight/confirmation, once per directory.
    # Every file still receives its own current size/time/link check.
    $checkedDirectories.Clear()
    foreach ($candidate in $candidates) {
      $item = Assert-NyxPlainPath $candidate.Path

      if ($item.Length -ne $candidate.Bytes -or
          $item.LastWriteTimeUtc.Ticks -ne $candidate.WriteTicks) {
        throw "Candidate changed during cleanup: $($candidate.Path)"
      }
      Remove-Item -LiteralPath $item.FullName -Force
      $deleted.Add($candidate)
      $deletedBytes += $candidate.Bytes

      if (($deleted.Count % 500) -eq 0) {
        Write-Progress -Activity 'Removing verified Nyx build/scratch files' -Status (
          "$($deleted.Count) of $($candidates.Count) files")
      }
    }
  }
}
catch {
  $failure = $_.Exception.Message
  throw
}
finally {
  Write-Progress -Activity 'Removing verified Nyx build/scratch files' -Completed
  $deleted | Export-Csv -LiteralPath (Join-Path $audit 'deleted.csv') -NoTypeInformation
  $after = @($before | ForEach-Object {
    $current = (Get-FileHash -LiteralPath $_.Path -Algorithm SHA256).Hash
    [pscustomobject]@{ Path=$_.Path; Hash=$current; Unchanged=($current -eq $_.Hash) }
  })
  $after | Export-Csv -LiteralPath (Join-Path $audit 'protected-after.csv') -NoTypeInformation
  Get-PSDrive -PSProvider FileSystem | Select-Object Name,Used,Free |
    Export-Csv -LiteralPath (Join-Path $audit 'drives-after.csv') -NoTypeInformation
  $summary = [pscustomobject]@{
    AuditDirectory=$audit; ApplyRequested=[bool]$Apply; WhatIf=[bool]$WhatIfPreference
    CandidateFiles=$candidates.Count; CandidateGiB=[Math]::Round($plannedBytes / 1GB, 3)
    DeletedFiles=$deleted.Count
    DeletedGiB=[Math]::Round($deletedBytes / 1GB, 3)
    ProtectedFiles=$after.Count; ProtectedUnchanged=(@($after | Where-Object { -not $_.Unchanged }).Count -eq 0)
    Failure=$failure
  }
  $summary | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $audit 'summary.json') -Encoding utf8
  $summary

  if (-not $summary.ProtectedUnchanged) {
    throw 'A protected file changed; inspect preservation receipts and retain current bytes'
  }
}
