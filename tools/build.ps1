# Nyx build orchestration. Product, generation and test logic remain Pascal.
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
[CmdletBinding()]
param(
[ValidateSet('core', 'generated', 'collections', 'collection-views', 'collection-authoring', 'collection-inspectors', 'collection-bindings', 'reusables', 'placement', 'designer-drag', 'constraints', 'resize', 'guides', 'move-snapping', 'flow-placement', 'containers', 'native-measurement', 'retained-arrangement', 'responsive', 'presentations', 'manual-presentations', 'selection', 'keyboard', 'catalog-focus', 'properties', 'layout', 'layout-policy', 'designer-controls', 'native-studio', 'semantic-events', 'source-workspace', 'source-editor', 'pascal-imports', 'pascal-routines', 'pascal-declarations', 'agents', 'state-bindings', 'state-inspectors', 'event-inspectors', 'agent-callback-consumers', 'agent-handler-consumers', 'agent-root-consumers', 'review-workspaces', 'review-consumers', 'project-workspaces', 'mcp-client', 'split', 'interactions', 'named-events', 'viewport', 'editing', 'gestures', 'catalog', 'browser', 'studio', 'lcl', 'http', 'visual', 'all')]
  [string]$Target = 'core',
  [string]$Fpc,
  [string]$Pas2js,
  [string]$Pas2jsRuntime,
  [string]$Lazarus,
  [string]$LclFpc,
  [string]$Widgetset = 'win32',
  # Keep checked ownership evidence and ordinary optimized application binaries
  # separate. Release retains assertions/range/overflow/I/O checks, enables -O2
  # and stripping, and omits heap tracing and line-debug instrumentation.
  [ValidateSet('checked', 'release')]
  [string]$NativeStudioConfiguration = 'checked',
  [string]$HttpURL = 'http://127.0.0.1:8088',
  # Stage browser artifacts independently while an older LAN instance is live.
  [string]$BrowserOutput,
  # The keyboard review source is exported through MCP, never handwritten by
  # this orchestration script. Its generated unit must live in this directory.
  [string]$KeyboardSourceDirectory = 'build/keyboard/mcp',
  # Full-catalog source is composed/exported by the Pascal semantic MCP consumer.
  [string]$CatalogFocusSourceDirectory = 'build/catalog-focus/source',
  # Property mutations consume an unchanged MCP-authored catalog/review pair.
  [string]$PropertySourceDirectory = 'build/property-concordance/source',
  # Proportional/hidden layout consumes bounded MCP-exported accepted source.
  [string]$LayoutSourceDirectory = 'build/layout-concordance/source',
  # Optional unchanged companion exported through bounded semantic MCP windows.
  # Empty uses the independent portable contract fixture for offline builds.
  [string]$ResponsiveSourceDirectory,
  # Optional exact base document exported through semantic MCP. The ordinary
  # portable fixture remains available without starting an application server.
  [string]$ArrangementSourceDirectory,
  # Unchanged companion exported through semantic MCP for alignment input review.
  [string]$GuideSourceDirectory = 'build/alignment/mcp-source',
  # Unchanged bounded MCP companion for the actual absolute-movement journey.
  [string]$MoveSourceDirectory = 'build/move-snapping/mcp-source',

  [string]$FlowSourceDirectory = 'build/flow-placement/mcp-source',
  # The container companion is composed and exported through bounded MCP reads.
  [string]$ContainerSourceDirectory = 'build/container-presentations/mcp-source',
  # The maintained semantic callback journey exports two accepted source pairs.
  [string]$CallbackSourceDirectory = 'build/agent-callbacks/mcp',
  # Exact companion exported by the semantic handler/compilation journey.
  [string]$HandlerSourceDirectory = 'build/handler-edits/journey/source',
  # Unchanged companion exported by the isolated semantic root-cleanup journey.
  [string]$RootSourceDirectory = 'build/root-cleanup/journey/source',
  # Exact bounded companion exported by the isolated protected-review journey.
  [string]$ReviewSourceDirectory = 'build/review-workspaces/journey/source',

  [string]$DesignerSourceDirectory = 'build/native-studio/source',
  # Optional explicit enrollment for the Pascal semantic review author. Supplying
  # it creates/compiles/retires an owned review, never the operator's project.
  [string]$DesignerMCPConfig,
  # Explicit actual-editor qualification consumes a pre-exported semantic pair.
  # Ordinary native Studio builds do not require a server or application tools.
  [switch]$VerifyNativeStudio,
  # Actual native HTTP/MCP journey. Enrollment and reusable test contexts are
  # explicit private fixtures; ordinary builds remain independent of a server.
  [switch]$VerifyNativeStudioService,
  [string]$NativeStudioServiceMCPConfig,
  [string]$NativeStudioTestContexts,
  # Real compiler/control qualification through a suspended private protocol
  # engine. This never starts or replaces a listener. Only its immutable jobs
  # are copied into the explicitly selected existing artifact-serving root.
  [switch]$VerifyNativeStudioCompiler,
  [string]$NativeStudioCompilerProfile,
  [string]$NativeStudioArtifactDirectory,
  # Optional Win32 transport qualification through an isolated raw Pascal TCP
  # peer. No Studio/MCP listener, project, enrollment or profile is replaced.
  [switch]$VerifyTransportDeadlines,
  # Actual local Nyx source controls, with native worker/retirement evidence.
  # Does not launch or replace a Studio/HTTP/MCP service.
  [switch]$VerifySourceScheduling,
  # Paired draft coalescing through the real protocol and native memo/timers.
  # Also builds the portable browser counterpart; it starts no listener.
  [switch]$VerifyDraftCapture,
  # Checked logical geometry and actual full-size native scrolling/input.
  # Browser companions are staged; execution needs a separately permitted host.
  [switch]$VerifyLogicalViewport,
  # Detached visual/structural admission, exact compiled companion and actual
  # original-size native controls. Browser programs are staged, not hosted.
  [switch]$VerifyDesignSource
)

$ErrorActionPreference = 'Stop'
$nyxRoot = Split-Path -Parent $PSScriptRoot
$nyxLocalConfig = Join-Path $nyxRoot '.local/toolchain.json'
$nyxConfig = @{}

if (Test-Path -LiteralPath $nyxLocalConfig) {
  $nyxConfig = Get-Content -LiteralPath $nyxLocalConfig -Raw | ConvertFrom-Json -AsHashtable
}

function Resolve-NyxTool([string]$Explicit, [string]$Key, [string]$Command) {
  $nyxCandidate = $Explicit

  if (-not $nyxCandidate) {
    $nyxCandidate = [Environment]::GetEnvironmentVariable('NYX_' + $Key)
  }

  if (-not $nyxCandidate -and $nyxConfig.ContainsKey($Key)) {
    $nyxCandidate = $nyxConfig[$Key]
  }

  if (-not $nyxCandidate -and $Command) {
    $nyxResolved = Get-Command $Command -ErrorAction SilentlyContinue

    if ($nyxResolved) {
      $nyxCandidate = $nyxResolved.Source
    }
  }

  if (-not $nyxCandidate -or -not (Test-Path -LiteralPath $nyxCandidate)) {
    throw "Missing $Key. Supply the build parameter, NYX_$Key, or .local/toolchain.json."
  }
  return (Resolve-Path -LiteralPath $nyxCandidate).Path
}

function Invoke-NyxCompiler([string]$Executable, [string[]]$Arguments) {
  & $Executable @Arguments

  if ($LASTEXITCODE -ne 0) {
    throw "Compiler failed with exit code $LASTEXITCODE"
  }
  # Studio's source processor is a separate Pascal program. Stage its matched
  # embedded RTL whenever a browser Studio is built, including focused test
  # outputs. This only orchestrates the compiler; no service is launched.
  if ($Arguments -contains 'studio/nyx_studio.lpr') {
    $nyxWorkerArguments = @($Arguments | Where-Object {
      $_ -ne 'studio/nyx_studio.lpr' -and $_ -ne '-Tbrowser' -and
      $_ -ne '-Jirtl.js' -and
      -not $_.StartsWith('-o', [StringComparison]::Ordinal)
    }) + @('-Tmodule', '-Jirtl.js', 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $Executable $nyxWorkerArguments
  }
}

function Test-NyxCompilerTypes([string]$Executable, [string[]]$Arguments) {
  # These are Pascal compiler fixtures, not a shell implementation of typing.
  # Require the intended type diagnostic as well as failure: a missing unit or
  # tool must never be mistaken for successful rejection of an invalid argument.
  $nyxTypeCases = @{
    layout = 'TNyxLayoutMode|TNyxLayoutPolicy'
    flow_wrap = 'TNyxFlowWrap'
    cross_alignment = 'TNyxCrossAlignment'
    sizing = 'TNyxSizing'
    layout_policy = 'TNyxFlowWrap'
    constraints = 'TNyxSizeConstraints'
    resize_snap = 'TNyxSizeSnap'
    platform = 'TNyxPlatform'
    split_orientation = 'TNyxSplitOrientation'
    spacing = 'Integer|LongInt'
    boolean = 'Boolean'
    reference = 'TNyxEventRef'
    state_reference = 'TNyx(Text|Boolean|Integer|Number)StateRef'
    state_value = 'Double|TNyx(Text|Boolean|Integer|Number)StateRef'
    binding_reference = 'TNyx(Text|Boolean|Integer|Number)StateRef'
    binding_kind = 'TNyxBooleanStateRef'
    binding_target = 'TNyxBindingProperty'
    extension_reference = 'TNyxExtensionRef'
    extension_value = 'TNyxDataValue'
    event_trigger = 'TNyxTrigger'
    event_handler = 'TNyxEventInfo'
    action = 'TNyxAction'
    part_reference = 'TNyxPartRef'
    override_mode = 'TNyxOverrideMode'
    kind_reference = 'TNyxKindRef'
    contract_domain = 'TNyx(Text|Boolean|Integer|Number)Domain'
    contract_range = '(Integer|LongInt)'
    event_value_ref = 'TNyxEventValueRef'
    control_text_value = 'UTF8String|String|TNyxText'
    control_checked = 'Boolean'
    control_reference = 'TNyxComponentRef'
    control_badge_value = 'Value'
    callback_policy = 'TNyxExecutionPolicy'
    callback_interface = 'INyxEventCallback'
    authored_handler = 'TNyxHandlerRef'
    authored_registration = 'TNyxCallbackRef'
    keyboard_key = 'TNyxKey'
    property_capability = 'TNyxCapability'
    property_meaning = 'TNyxPropertyMeaning'
    named_event = 'TNyxEventRef'
    semantic_event = 'TNyxSemanticEvent'
    named_payload_kind = 'TNyxDataKind'
    wheel_unit = 'TNyxWheelUnit'
    viewport_unit = 'TNyxViewportUnit'
    editing_intent = 'TNyxEditIntent'
    text_direction = 'TNyxTextDirection'
    editing_phase = 'TNyxEditingPhase'
    touch_behavior = 'TNyxTouchBehavior'
    drag_source = 'Boolean'
    drop_operation = 'TNyxDropOperation'
    drop_operations = 'TNyxDropOperations|Set Of TNyxDropOperation'
    transfer_format = 'TNyxTransferFormatRef'
    selection_mode = 'TNyxSelectionMode'
    selection_action = 'TNyxSelectionAction'
    # FPC reports the final numeric overload for two wrong scalar arguments;
    # pas2js reports the matching reference family. Both must reject typed input.
    collection_text = 'TNyxTextFieldRef|UTF8String|String|TNyxText|Double'
    collection_boolean = 'Boolean|TNyxBooleanFieldRef|Double'
    collection_field = 'TNyxIntegerFieldRef'
    collection_item = 'TNyxItemRef'
    collection_scope = 'TNyxCollectionRef|INyxCollectionSnapshot'
    collection_index = 'Integer|LongInt'
    collection_snapshot = '(member|identifier not found).*Move'
    collection_view_field = 'TNyx(Text|Boolean|Integer|Number)FieldRef'
    collection_view_scope = 'TNyxCollectionScope'
    collection_binding = 'TNyxCollectionViewSpec'
    # Overload diagnostics can point at the final numeric field overload.
    collection_cell_mode = 'TNyxCollectionCellMode|TNyxNumberFieldRef'
  }
  foreach ($nyxTypeCase in $nyxTypeCases.Keys) {
    $nyxCompilerOutput = & $Executable @Arguments "tests/compile_fail/nyx_invalid_$nyxTypeCase.lpr" 2>&1
    $nyxCompilerExit = $LASTEXITCODE
    $nyxCompilerText = $nyxCompilerOutput -join [Environment]::NewLine

    if ($nyxCompilerExit -eq 0 -or
      $nyxCompilerText -notmatch ('Error:.*(' + $nyxTypeCases[$nyxTypeCase] + ')')) {
      Write-Host $nyxCompilerText
      throw "Expected Pascal type rejection failed: $nyxTypeCase"
    }
    Write-Host "PASS compiler rejects untyped $nyxTypeCase argument"
  }
}

Push-Location $nyxRoot
try {
  $nyxFpc = Resolve-NyxTool $Fpc 'FPC' 'fpc'
  $nyxVersion = (& $nyxFpc '-iV').Trim()
  $nyxCPU = (& $nyxFpc '-iTP').Trim()
  $nyxOS = (& $nyxFpc '-iTO').Trim()
  $nyxNativeDir = Join-Path $nyxRoot ("build/native/$nyxVersion/$nyxCPU-$nyxOS")
  New-Item -ItemType Directory -Force $nyxNativeDir | Out-Null
  Write-Host "FPC: $nyxFpc / $nyxVersion / $nyxCPU-$nyxOS"
  $nyxNativeFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
    '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxNativeDir", "-FE$nyxNativeDir")

  if ($Target -eq 'mcp-client') {
    # Native Pascal configuration/client tooling needs neither an application
    # output compiler nor a browser. No live document mutation is implicit.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tools/nyx_studio_mcp.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_mcp_config_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_mcp_config_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'MCP configuration preservation checks failed'
    }
    exit 0
  }

  if ($Target -eq 'native-studio') {
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxStudioNativeDirectory = 'build/native-studio/controller'
    $nyxStudioBuildFlags = @('-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh')

    if ($NativeStudioConfiguration -eq 'release') {
      $nyxStudioNativeDirectory = 'build/native-studio/release'
      $nyxStudioBuildFlags = @('-Sa', '-Cr', '-Co', '-Ci', '-O2', '-Xs')
    }
    $nyxStudioNative = Join-Path $nyxRoot $nyxStudioNativeDirectory
    New-Item -ItemType Directory -Force $nyxStudioNative | Out-Null
    $nyxStudioPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxStudioArguments = @('-B', '-Mdelphi') + $nyxStudioBuildFlags + @(
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxStudioPlatform", "-Fu$nyxLazarus/lcl/units/$nyxStudioPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxStudioPlatform", "-Fu$nyxLazarus/packager/units/$nyxStudioPlatform",
      "-FU$nyxStudioNative", "-FE$nyxStudioNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @('studio/nyx_studio_native.lpr'))

    if ($VerifyLogicalViewport) {
      foreach ($nyxViewportProgram in @('nyx_logical_viewport_tests', 'nyx_logical_viewport_controls')) {
        Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("tests/$nyxViewportProgram.lpr"))
      }
      $nyxViewportPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
      $nyxViewportRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
      $nyxViewportBrowser = Join-Path $nyxRoot 'build/logical-viewport/browser'
      New-Item -ItemType Directory -Force $nyxViewportBrowser | Out-Null
      foreach ($nyxViewportProgram in @('nyx_logical_viewport_tests', 'nyx_logical_viewport_controls')) {
        Invoke-NyxCompiler $nyxViewportPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
          '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxViewportBrowser", "tests/$nyxViewportProgram.lpr")
      }
      Copy-Item -LiteralPath $nyxViewportRuntime -Destination (Join-Path $nyxViewportBrowser 'rtl.js') -Force
      & (Join-Path $nyxStudioNative 'nyx_logical_viewport_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Logical viewport geometry checks failed' }
      & (Join-Path $nyxStudioNative 'nyx_logical_viewport_controls.exe') `
        (Join-Path $nyxRoot 'build/logical-viewport')

      if ($LASTEXITCODE -ne 0) { throw 'Actual native logical viewport controls failed' }
    }

    if ($VerifyDesignSource) {
      $nyxDesignArtifactDirectory = 'build/design-source/maintained'

      if ($NativeStudioConfiguration -eq 'release') {
        $nyxDesignArtifactDirectory = 'build/design-source/release'
      }
      $nyxDesignArtifacts = Join-Path $nyxRoot $nyxDesignArtifactDirectory
      $nyxDesignPair = Join-Path $nyxDesignArtifacts 'pair'
      $nyxDesignBrowser = Join-Path $nyxDesignArtifacts 'browser'
      New-Item -ItemType Directory -Force $nyxDesignPair, $nyxDesignBrowser | Out-Null
      foreach ($nyxDesignProgram in @('nyx_design_source_tests', 'nyx_design_queue_tests',
          'nyx_projection_refresh_tests', 'nyx_canvas_queue_controls', 'nyx_design_source_controls')) {
        Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("tests/$nyxDesignProgram.lpr"))
      }
      & (Join-Path $nyxStudioNative 'nyx_design_source_tests.exe') $nyxDesignPair

      if ($LASTEXITCODE -ne 0) { throw 'Detached design/source qualification failed' }
      & (Join-Path $nyxStudioNative 'nyx_design_queue_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Native queued intent/load/presentation qualification failed' }
      & (Join-Path $nyxStudioNative 'nyx_projection_refresh_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Actual retained native projection qualification failed' }
      & (Join-Path $nyxStudioNative 'nyx_canvas_queue_controls.exe') (Join-Path $nyxDesignArtifacts 'canvas-controls')

      if ($LASTEXITCODE -ne 0) { throw 'Actual queued native canvas qualification failed' }
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("-Fu$nyxDesignPair",
        'tests/nyx_design_source_consumer.lpr'))
      & (Join-Path $nyxStudioNative 'nyx_design_source_consumer.exe') (Join-Path $nyxDesignPair 'expected.nyx')

      if ($LASTEXITCODE -ne 0) { throw 'Exact compiled design/source consumer failed' }
      # Compile the newly admitted canvas/default/instance companion independently.
      # Separate units keep a same-named earlier generated builder out of this proof.
      $nyxCanvasPair = Join-Path $nyxDesignPair 'canvas'
      $nyxCanvasConsumer = Join-Path $nyxCanvasPair 'compiled'
      New-Item -ItemType Directory -Force $nyxCanvasConsumer | Out-Null
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("-Fu$nyxCanvasPair",
        "-FU$nyxCanvasConsumer", "-FE$nyxCanvasConsumer", 'tests/nyx_design_source_consumer.lpr'))
      & (Join-Path $nyxCanvasConsumer 'nyx_design_source_consumer.exe') (Join-Path $nyxCanvasPair 'expected.nyx')

      if ($LASTEXITCODE -ne 0) { throw 'Exact compiled canvas design/source consumer failed' }
      $nyxDesignPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
      $nyxDesignRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
      foreach ($nyxDesignProgram in @('tests/nyx_design_source_tests.lpr',
          'tests/nyx_projection_refresh_tests.lpr', 'studio/nyx_studio.lpr')) {
        Invoke-NyxCompiler $nyxDesignPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
          '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxDesignBrowser", $nyxDesignProgram)
      }
      Copy-Item -LiteralPath $nyxDesignRuntime -Destination (Join-Path $nyxDesignBrowser 'rtl.js') -Force
      & (Join-Path $nyxStudioNative 'nyx_design_source_controls.exe') (Join-Path $nyxDesignArtifacts 'controls')

      if ($LASTEXITCODE -ne 0) { throw 'Actual original-size design/source controls failed' }
    }

    if ($VerifySourceScheduling) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @('tests/nyx_source_scheduling_tests.lpr'))
      $nyxSourceControlsDirectory = 'build/source-scheduling/controls'

      if ($NativeStudioConfiguration -eq 'release') {
        $nyxSourceControlsDirectory = 'build/source-scheduling/controls-release'
      }
      & (Join-Path $nyxStudioNative 'nyx_source_scheduling_tests.exe') `
        (Join-Path $nyxRoot $nyxSourceControlsDirectory)

      if ($LASTEXITCODE -ne 0) { throw 'Native source scheduling/control qualification failed' }
    }

    if ($VerifyDraftCapture) {
      foreach ($nyxCaptureProgram in @('nyx_draft_capture_tests', 'nyx_draft_capture_controls')) {
        Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("tests/$nyxCaptureProgram.lpr"))
      }
      $nyxCaptureBrowser = Join-Path $nyxRoot 'build/draft-capture/browser'
      New-Item -ItemType Directory -Force $nyxCaptureBrowser | Out-Null
      $nyxCapturePas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
      Invoke-NyxCompiler $nyxCapturePas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxCaptureBrowser", 'tests/nyx_draft_capture_tests.lpr')
      $nyxCaptureRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
      Copy-Item -LiteralPath $nyxCaptureRuntime -Destination (Join-Path $nyxCaptureBrowser 'rtl.js')
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/draft-capture.html') `
        -Destination $nyxCaptureBrowser
      # Stage both consumers even when an original-size runtime gate refuses.
      # Execution still fails the command; no large-project check is skipped.
      & (Join-Path $nyxStudioNative 'nyx_draft_capture_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Portable paired draft capture qualification failed' }
      & (Join-Path $nyxStudioNative 'nyx_draft_capture_controls.exe') `
        (Join-Path $nyxRoot 'build/draft-capture/controls')

      if ($LASTEXITCODE -ne 0) { throw 'Native original-size draft capture qualification failed' }
    }

    if ($VerifyTransportDeadlines) {
      $nyxTransportRoot = Join-Path $nyxRoot 'build/transport-deadline'
      $nyxTransportBrowser = Join-Path $nyxTransportRoot 'browser'
      New-Item -ItemType Directory -Path $nyxTransportBrowser -Force | Out-Null
      # Compile the socket adapter against stable FPC as well as the installed
      # LCL compiler. Actual timing/controls below qualify the Win32 LCL build.
      Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('studio/nyx.studio.transport.native.pas'))
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @('tests/nyx_transport_deadline_tests.lpr'))
      $nyxTransportConsumer = Join-Path $nyxStudioNative 'nyx_transport_deadline_tests.exe'
      & $nyxTransportConsumer (Join-Path $nyxTransportRoot 'owned-preview')

      if ($LASTEXITCODE -ne 0) { throw 'Native transport deadline/retirement consumer failed' }
      $nyxTransportPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
      $nyxTransportRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
      Invoke-NyxCompiler $nyxTransportPas2js @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-o$nyxTransportBrowser/transport-tests.js", 'tests/nyx_browser_transport_tests.lpr')
      Copy-Item -LiteralPath $nyxTransportRuntime -Destination (Join-Path $nyxTransportBrowser 'rtl.js')
      & $nyxTransportConsumer --prepare-browser $nyxTransportBrowser

      if ($LASTEXITCODE -ne 0) { throw 'Pascal transport browser fixture preparation failed' }
      & $nyxTransportConsumer --browser $nyxTransportBrowser (Join-Path $nyxTransportRoot 'browser-maintained')

      if ($LASTEXITCODE -ne 0) { throw 'Real-clock browser transport deadline/retirement consumer failed' }
    }

    if ($VerifyNativeStudio) {
      $nyxStudioSource = [IO.Path]::GetFullPath($DesignerSourceDirectory)

      if (-not (Test-Path -LiteralPath (Join-Path $nyxStudioSource 'nyx.generated.view.pas'))) {
        throw 'Native editor qualification requires the exact MCP-authored companion export'
      }
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("-Fu$nyxStudioSource", 'tests/nyx_studio_native_tests.lpr'))
      $nyxEditorControlsDirectory = 'build/native-studio/editor-current'

      if ($NativeStudioConfiguration -eq 'release') {
        $nyxEditorControlsDirectory = 'build/native-studio/editor-release'
      }
      & (Join-Path $nyxStudioNative 'nyx_studio_native_tests.exe') $nyxStudioSource `
        (Join-Path $nyxRoot $nyxEditorControlsDirectory)

      if ($LASTEXITCODE -ne 0) { throw 'Actual standalone native Studio journey failed' }
    }

    if ($VerifyNativeStudioService) {
      if (-not $NativeStudioServiceMCPConfig) {
        throw 'Native service qualification requires an explicit private MCP fixture configuration'
      }
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @('tests/nyx_native_workspace_tests.lpr'))
      $nyxServiceArguments = @($HttpURL, [IO.Path]::GetFullPath($NativeStudioServiceMCPConfig),
        (Join-Path $nyxRoot 'build/native-studio/service-current'))

      if ($NativeStudioTestContexts) {
        $nyxServiceArguments += @('reuse-owned', [IO.Path]::GetFullPath($NativeStudioTestContexts))
      }
      & (Join-Path $nyxStudioNative 'nyx_native_workspace_tests.exe') @nyxServiceArguments

      if ($LASTEXITCODE -ne 0) { throw 'Actual native service/workspace journey failed; retain owned-review manifest' }
    }

    if ($VerifyNativeStudioCompiler) {
      $nyxCompilerSource = [IO.Path]::GetFullPath($DesignerSourceDirectory)

      if (-not $NativeStudioCompilerProfile -or -not $NativeStudioArtifactDirectory -or
        -not (Test-Path -LiteralPath (Join-Path $nyxCompilerSource 'nyx.generated.view.pas'))) {
        throw 'Compiler qualification requires an exact semantic export, a private profile and an existing artifact-serving root'
      }
      $nyxCompilerProfile = [IO.Path]::GetFullPath($NativeStudioCompilerProfile)
      $nyxCompilerArtifacts = [IO.Path]::GetFullPath($NativeStudioArtifactDirectory)

      if (-not (Test-Path -LiteralPath $nyxCompilerProfile -PathType Leaf) -or
        -not (Test-Path -LiteralPath $nyxCompilerArtifacts -PathType Container)) {
        throw 'The explicit compiler profile and artifact root must already exist'
      }
      Invoke-NyxCompiler $nyxLclFpc ($nyxStudioArguments + @("-Fu$nyxCompilerSource", 'tests/nyx_native_build_tests.lpr'))
      $nyxCompilerRepository = Join-Path $nyxRoot ('build/native-studio/compiler-current/protocol-' + [Guid]::NewGuid().ToString())
      New-Item -ItemType Directory -Path $nyxCompilerRepository | Out-Null
      # The actual compiler expects the library beside its admitted build root.
      # Read existing sources through private links; never copy a machine path
      # into a portable design or edit dependency/source files through the links.
      foreach ($nyxLibraryDirectory in @('src', 'studio')) {
        New-Item -ItemType Junction -Path (Join-Path $nyxCompilerRepository $nyxLibraryDirectory) `
          -Target (Join-Path $nyxRoot $nyxLibraryDirectory) | Out-Null
      }
      & (Join-Path $nyxStudioNative 'nyx_native_build_tests.exe') $nyxCompilerRepository `
        $nyxCompilerProfile $nyxCompilerSource $nyxCompilerArtifacts $HttpURL

      if ($LASTEXITCODE -ne 0) { throw 'Actual native compiler/preview journey failed; retain its owned artifacts and paired snapshots' }
    }
    exit 0
  }

  if ($Target -eq 'designer-controls') {
    # Compile the semantic author separately; running it needs an explicitly
    # supplied authenticated endpoint. This gate never launches a service or
    # replaces an operator document. Actual adapters consume its unchanged export.
    $nyxDesignerSource = [IO.Path]::GetFullPath($DesignerSourceDirectory)

    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxDesignerAuthor = Join-Path $nyxRoot 'build/native-studio/author'
    $nyxDesignerNative = Join-Path $nyxRoot 'build/native-studio/native'
    New-Item -ItemType Directory -Force $nyxDesignerAuthor, $nyxDesignerNative | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxDesignerAuthor", "-FE$nyxDesignerAuthor",
      'tests/nyx_mcp_designer_review.lpr')

    if ($DesignerMCPConfig) {
      & (Join-Path $nyxDesignerAuthor 'nyx_mcp_designer_review.exe') $DesignerMCPConfig `
        (Join-Path $nyxRoot 'tests/designer-review.operations.json') $nyxDesignerSource

      if ($LASTEXITCODE -ne 0) { throw 'Semantic designer review failed; preserve its receipts' }
    }

    if (-not (Test-Path -LiteralPath (Join-Path $nyxDesignerSource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP-authored designer-review companion first, or supply DesignerMCPConfig'
    }
    $nyxDesignerPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxDesignerSource",
      "-Fu$nyxLazarus/lcl/units/$nyxDesignerPlatform", "-Fu$nyxLazarus/lcl/units/$nyxDesignerPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxDesignerPlatform", "-Fu$nyxLazarus/packager/units/$nyxDesignerPlatform",
      "-FU$nyxDesignerNative", "-FE$nyxDesignerNative", 'tests/nyx_designer_controls_tests.lpr')
    & (Join-Path $nyxDesignerNative 'nyx_designer_controls_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Native designer-purpose/retained-view controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxDesignerBrowser = Join-Path $nyxRoot 'build/native-studio/web'

    if ($BrowserOutput) { $nyxDesignerBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxDesignerBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxDesignerSource", "-FE$nyxDesignerBrowser", 'tests/nyx_designer_controls_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxDesignerBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/designer.html') -Destination $nyxDesignerBrowser
    exit 0
  }

  if ($Target -eq 'project-workspaces') {
    # Stage current portable contracts, actual target view consumers and service
    # artifacts. Starting a listener or resetting an editor is never a build
    # side effect; the maintained Pascal MCP journey takes explicit fixture args.
    $nyxProjectDir = Join-Path $nyxRoot 'build/project-workspaces/orchestrated'
    $nyxProjectNative = Join-Path $nyxProjectDir 'native'
    $nyxProjectViews = Join-Path $nyxProjectDir 'views'
    $nyxProjectProtocol = Join-Path $nyxProjectDir 'protocol'
    $nyxBrowserDir = Join-Path $nyxProjectDir 'web'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxProjectNative, $nyxProjectViews, $nyxProjectProtocol, $nyxBrowserDir | Out-Null
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxProjectFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxProjectNative", "-FE$nyxProjectNative")
    Invoke-NyxCompiler $nyxFpc ($nyxProjectFlags + @('tests/nyx_workspace_tests.lpr'))
    & (Join-Path $nyxProjectNative 'nyx_workspace_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Portable native project ownership/presentation checks failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxProjectFlags + @('studio/nyx_studio_server.lpr'))
    $nyxProjectHostFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxProjectProtocol", "-FE$nyxProjectProtocol")
    Invoke-NyxCompiler $nyxLclFpc ($nyxProjectHostFlags + @('tests/nyx_mcp_workspace_tests.lpr'))
    $nyxProjectPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxProjectViewFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxProjectViews", "-FE$nyxProjectViews",
      "-Fu$nyxLazarus/lcl/units/$nyxProjectPlatform", "-Fu$nyxLazarus/lcl/units/$nyxProjectPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxProjectPlatform", "-Fu$nyxLazarus/packager/units/$nyxProjectPlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxProjectViewFlags + @('tests/nyx_workspace_view_tests.lpr'))
    & (Join-Path $nyxProjectViews 'nyx_workspace_view_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual native project view controls failed' }
    foreach ($nyxProjectProgram in @('tests/nyx_workspace_tests.lpr', 'tests/nyx_workspace_view_tests.lpr',
        'studio/nyx_studio.lpr', 'studio/nyx_studio_review.lpr', 'studio/nyx_studio_preview.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Fustudio',
        "-FE$nyxBrowserDir", $nyxProjectProgram)
    }
    foreach ($nyxProjectHost in @('workspaces.html', 'workspace-view.html', 'index.html',
        'agent-review.html', 'agent-preview.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxProjectHost") -Destination $nyxBrowserDir
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    exit 0
  }

  if ($Target -in @('review-workspaces', 'review-consumers')) {
    # This target stages artifacts only. Launching a separate service, admitting
    # its disposable user fixture and driving MCP remain explicit Pascal steps;
    # an ordinary build must never silently edit a live Studio project.
    $nyxReviewDir = Join-Path $nyxRoot 'build/review-workspaces/orchestrated'
    $nyxReviewNative = Join-Path $nyxReviewDir 'native'
    $nyxReviewProtocol = Join-Path $nyxReviewDir 'protocol'
    $nyxBrowserDir = Join-Path $nyxReviewDir 'web'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxReviewNative, $nyxReviewProtocol, $nyxBrowserDir | Out-Null
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxReviewFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxReviewNative", "-FE$nyxReviewNative")

    if ($Target -eq 'review-workspaces') {
      Invoke-NyxCompiler $nyxFpc ($nyxReviewFlags + @('tests/nyx_review_tests.lpr'))
      & (Join-Path $nyxReviewNative 'nyx_review_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Portable native protected-review checks failed' }
      # The maintained browser-host protocol needs the installed compiler's
      # fpwebsocket units; use the already qualified matching toolchain.
      $nyxReviewHostFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
        '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxReviewProtocol", "-FE$nyxReviewProtocol")
      Invoke-NyxCompiler $nyxLclFpc ($nyxReviewHostFlags + @('tests/nyx_mcp_review_tests.lpr'))
      Invoke-NyxCompiler $nyxFpc ($nyxReviewFlags + @('studio/nyx_studio_server.lpr'))
      foreach ($nyxReviewProgram in @('tests/nyx_review_tests.lpr', 'studio/nyx_studio.lpr',
          'studio/nyx_studio_review.lpr', 'studio/nyx_studio_preview.lpr')) {
        Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Fustudio',
          "-FE$nyxBrowserDir", $nyxReviewProgram)
      }
      foreach ($nyxReviewHost in @('reviews.html', 'index.html', 'agent-review.html', 'agent-preview.html')) {
        Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxReviewHost") -Destination $nyxBrowserDir
      }
    } else {
      $nyxReviewSource = [IO.Path]::GetFullPath($ReviewSourceDirectory)

      if (-not (Test-Path -LiteralPath (Join-Path $nyxReviewSource 'nyx.generated.view.pas'))) {
        throw 'Run the isolated semantic review journey first; see docs/studio-agents.md'
      }
      $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
      $nyxReviewPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
      $nyxReviewControlFlags = $nyxReviewFlags + @("-Fu$nyxReviewSource",
        "-Fu$nyxLazarus/lcl/units/$nyxReviewPlatform", "-Fu$nyxLazarus/lcl/units/$nyxReviewPlatform/$Widgetset",
        "-Fu$nyxLazarus/components/lazutils/lib/$nyxReviewPlatform", "-Fu$nyxLazarus/packager/units/$nyxReviewPlatform")
      Invoke-NyxCompiler $nyxLclFpc ($nyxReviewControlFlags + @('tests/nyx_review_consumer_tests.lpr'))
      & (Join-Path $nyxReviewNative 'nyx_review_consumer_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Compiled native protected-review controls failed' }
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', "-Fu$nyxReviewSource",
        "-FE$nyxBrowserDir", 'tests/nyx_review_consumer_tests.lpr')
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/review-consumers.html') -Destination $nyxBrowserDir
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    exit 0
  }

  if ($Target -eq 'agent-callback-consumers') {
    $nyxCallbackSource = [IO.Path]::GetFullPath($CallbackSourceDirectory)
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxCallbackPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    foreach ($nyxVariant in @('ordered', 'removed')) {
      $nyxCallbackUnit = Join-Path $nyxCallbackSource $nyxVariant

      if (-not (Test-Path -LiteralPath (Join-Path $nyxCallbackUnit 'nyx.generated.view.pas'))) {
        throw 'Run the isolated semantic callback journey first; see docs/studio-agents.md'
      }
      $nyxCallbackNative = Join-Path $nyxRoot "build/callback-consumers/$nyxVariant/native"
      $nyxCallbackBrowser = Join-Path $nyxBrowserDir "agent-callback-$nyxVariant"
      New-Item -ItemType Directory -Force $nyxCallbackNative, $nyxCallbackBrowser | Out-Null
      $nyxCallbackFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
        '-Fusrc', '-Futests', "-Fu$nyxCallbackUnit",
        "-Fu$nyxLazarus/lcl/units/$nyxCallbackPlatform", "-Fu$nyxLazarus/lcl/units/$nyxCallbackPlatform/$Widgetset",
        "-Fu$nyxLazarus/components/lazutils/lib/$nyxCallbackPlatform", "-Fu$nyxLazarus/packager/units/$nyxCallbackPlatform",
        "-FU$nyxCallbackNative", "-FE$nyxCallbackNative")
      Invoke-NyxCompiler $nyxLclFpc ($nyxCallbackFlags + @('tests/nyx_mcp_callback_consumer_tests.lpr'))
      $nyxExpectedCount = if ($nyxVariant -eq 'ordered') { 2 } else { 1 }
      & (Join-Path $nyxCallbackNative 'nyx_mcp_callback_consumer_tests.exe') $nyxExpectedCount

      if ($LASTEXITCODE -ne 0) { throw 'Compiled native semantic callbacks failed' }
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Futests',
        "-Fu$nyxCallbackUnit", "-FE$nyxCallbackBrowser", 'tests/nyx_mcp_callback_consumer_tests.lpr')
      Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxCallbackBrowser 'rtl.js')
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/agent-callback-consumer.html') -Destination $nyxCallbackBrowser
    }
    exit 0
  }

  if ($Target -eq 'agent-handler-consumers') {
    $nyxHandlerSource = [IO.Path]::GetFullPath($HandlerSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxHandlerSource 'nyx.generated.view.pas'))) {
      throw 'Run the isolated semantic handler journey first; see docs/studio-agents.md'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxHandlerPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxHandlerNative = Join-Path $nyxRoot 'build/handler-edits/consumers-native'
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxHandlerNative, $nyxBrowserDir | Out-Null
    $nyxHandlerFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxHandlerSource",
      "-Fu$nyxLazarus/lcl/units/$nyxHandlerPlatform", "-Fu$nyxLazarus/lcl/units/$nyxHandlerPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxHandlerPlatform", "-Fu$nyxLazarus/packager/units/$nyxHandlerPlatform",
      "-FU$nyxHandlerNative", "-FE$nyxHandlerNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxHandlerFlags + @('tests/nyx_handler_consumer_tests.lpr'))
    & (Join-Path $nyxHandlerNative 'nyx_handler_consumer_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Compiled native semantic handlers failed' }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-Fu$nyxHandlerSource", "-FE$nyxBrowserDir", 'tests/nyx_handler_consumer_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/handler-consumers.html') -Destination $nyxBrowserDir
    exit 0
  }

  if ($Target -eq 'agent-root-consumers') {
    $nyxRootSource = [IO.Path]::GetFullPath($RootSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxRootSource 'nyx.generated.view.pas'))) {
      throw 'Run the isolated semantic root journey first; see docs/studio-agents.md'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxRootPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxRootNative = Join-Path $nyxRoot 'build/root-cleanup/consumers-native'
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxRootNative, $nyxBrowserDir | Out-Null
    $nyxRootFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxRootSource",
      "-Fu$nyxLazarus/lcl/units/$nyxRootPlatform", "-Fu$nyxLazarus/lcl/units/$nyxRootPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxRootPlatform", "-Fu$nyxLazarus/packager/units/$nyxRootPlatform",
      "-FU$nyxRootNative", "-FE$nyxRootNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxRootFlags + @('tests/nyx_root_consumer_tests.lpr'))
    & (Join-Path $nyxRootNative 'nyx_root_consumer_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Compiled native semantic root cleanup failed' }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-Fu$nyxRootSource", "-FE$nyxBrowserDir", 'tests/nyx_root_consumer_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/root-consumers.html') -Destination $nyxBrowserDir
    exit 0
  }

  if ($Target -in @('layout', 'layout-policy')) {
    $nyxLayoutSource = [IO.Path]::GetFullPath($LayoutSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxLayoutSource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP-authored layout-review companion first'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLayoutBase = 'build/layout-concordance'
    $nyxLayoutDefinitions = @()
    if ($Target -eq 'layout-policy') {
      $nyxLayoutBase = 'build/layout-policy'
      $nyxLayoutDefinitions = @('-dNYX_LAYOUT_POLICY')
    }
    $nyxLayoutNative = Join-Path $nyxRoot "$nyxLayoutBase/native"
    $nyxLayoutAuthor = Join-Path $nyxRoot "$nyxLayoutBase/author"
    New-Item -ItemType Directory -Force $nyxLayoutNative | Out-Null
    New-Item -ItemType Directory -Force $nyxLayoutAuthor | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxLayoutAuthor", "-FE$nyxLayoutAuthor", 'tests/nyx_mcp_layout_review.lpr')
    $nyxLayoutPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc (@('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxLayoutSource",
      "-Fu$nyxLazarus/lcl/units/$nyxLayoutPlatform", "-Fu$nyxLazarus/lcl/units/$nyxLayoutPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxLayoutPlatform", "-Fu$nyxLazarus/packager/units/$nyxLayoutPlatform",
      "-FU$nyxLayoutNative", "-FE$nyxLayoutNative") + $nyxLayoutDefinitions +
      @('tests/nyx_layout_controls_tests.lpr'))
    & (Join-Path $nyxLayoutNative 'nyx_layout_controls_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Native proportional layout consumer failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxLayoutBrowser = Join-Path $nyxRoot "$nyxLayoutBase/web"

    if ($BrowserOutput) { $nyxLayoutBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxLayoutBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-Fu$nyxLayoutSource", "-FE$nyxLayoutBrowser") + $nyxLayoutDefinitions +
      @('tests/nyx_layout_controls_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxLayoutBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/layout.html') -Destination $nyxLayoutBrowser
    exit 0
  }

  if ($Target -eq 'properties') {
    $nyxPropertySource = [IO.Path]::GetFullPath($PropertySourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxPropertySource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP-authored catalog and property-review page first'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPropertyNative = Join-Path $nyxRoot 'build/property-concordance/native'
    $nyxPropertyAuthor = Join-Path $nyxRoot 'build/property-concordance/author'
    New-Item -ItemType Directory -Force $nyxPropertyNative | Out-Null
    New-Item -ItemType Directory -Force $nyxPropertyAuthor | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxPropertyAuthor", "-FE$nyxPropertyAuthor", 'tests/nyx_mcp_catalog_focus.lpr')
    $nyxPropertyPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxPropertySource",
      "-Fu$nyxLazarus/lcl/units/$nyxPropertyPlatform", "-Fu$nyxLazarus/lcl/units/$nyxPropertyPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxPropertyPlatform", "-Fu$nyxLazarus/packager/units/$nyxPropertyPlatform",
      "-FU$nyxPropertyNative", "-FE$nyxPropertyNative", 'tests/nyx_property_controls_tests.lpr')
    & (Join-Path $nyxPropertyNative 'nyx_property_controls_tests.exe') (Join-Path $nyxPropertyNative 'pictures')

    if ($LASTEXITCODE -ne 0) { throw 'Native property concordance failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxPropertyBrowser = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxPropertyBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxPropertyBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-Fu$nyxPropertySource", "-FE$nyxPropertyBrowser", 'tests/nyx_property_controls_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxPropertyBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/properties.html') -Destination $nyxPropertyBrowser
    exit 0
  }

  if ($Target -eq 'catalog-focus') {
    $nyxFocusSource = [IO.Path]::GetFullPath($CatalogFocusSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxFocusSource 'nyx.generated.view.pas'))) {
      throw 'Compose/export the catalog focus companion through nyx_mcp_catalog_focus first'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxFocusNative = Join-Path $nyxRoot 'build/catalog-focus/native'
    $nyxFocusDriver = Join-Path $nyxRoot 'build/catalog-focus/driver'
    $nyxFocusAuthor = Join-Path $nyxRoot 'build/catalog-focus/author'
    New-Item -ItemType Directory -Force $nyxFocusNative, $nyxFocusDriver, $nyxFocusAuthor | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxFocusAuthor", "-FE$nyxFocusAuthor", 'tests/nyx_mcp_catalog_focus.lpr')
    $nyxFocusPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxFocusSource",
      "-Fu$nyxLazarus/lcl/units/$nyxFocusPlatform", "-Fu$nyxLazarus/lcl/units/$nyxFocusPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxFocusPlatform", "-Fu$nyxLazarus/packager/units/$nyxFocusPlatform",
      "-FU$nyxFocusNative", "-FE$nyxFocusNative", 'tests/nyx_catalog_focus_tests.lpr')
    & (Join-Path $nyxFocusNative 'nyx_catalog_focus_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Native full-catalog focus qualification failed' }
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
      '-Fusrc', '-Futests', "-FU$nyxFocusDriver", "-FE$nyxFocusDriver", 'tests/nyx_catalog_focus_cdp_tests.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxFocusBrowser = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxFocusBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxFocusBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-Fu$nyxFocusSource", "-FE$nyxFocusBrowser", 'tests/nyx_catalog_focus_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxFocusBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/catalog-focus.html') -Destination $nyxFocusBrowser
    exit 0
  }

  if ($Target -eq 'keyboard') {
    $nyxKeyboardSource = [IO.Path]::GetFullPath($KeyboardSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxKeyboardSource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP-authored keyboard review source first; see docs/studio-agents.md'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxKeyboardNative = Join-Path $nyxRoot 'build/keyboard/lcl'
    $nyxKeyboardDriver = Join-Path $nyxRoot 'build/keyboard/driver'
    New-Item -ItemType Directory -Force $nyxKeyboardNative, $nyxKeyboardDriver | Out-Null
    $nyxKeyboardPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxKeyboardFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxKeyboardSource",
      "-Fu$nyxLazarus/lcl/units/$nyxKeyboardPlatform", "-Fu$nyxLazarus/lcl/units/$nyxKeyboardPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxKeyboardPlatform", "-Fu$nyxLazarus/packager/units/$nyxKeyboardPlatform",
      "-FU$nyxKeyboardNative", "-FE$nyxKeyboardNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxKeyboardFlags + @('tests/nyx_keyboard_host_tests.lpr'))
    & (Join-Path $nyxKeyboardNative 'nyx_keyboard_host_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Native MCP-authored keyboard review failed' }
    # fpwebsocket belongs to the matched FPC toolchain used by the existing host
    # gesture driver. This program owns an isolated browser, not the editor tab.
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
      '-Fusrc', '-Futests', "-FU$nyxKeyboardDriver", "-FE$nyxKeyboardDriver", 'tests/nyx_keyboard_cdp_tests.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-Fu$nyxKeyboardSource", "-FE$nyxBrowserDir", 'tests/nyx_keyboard_host_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/keyboard-host.html') -Destination $nyxBrowserDir
    exit 0
  }

  if ($Target -in @('selection', 'all')) {
    # The Pascal journey admits immutable selection state, exercises real widgets
    # and writes the crafted companion before either compiler consumes it.
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxSelectionNative = Join-Path $nyxRoot 'build/selection/lcl'
    New-Item -ItemType Directory -Force $nyxSelectionNative | Out-Null
    $nyxSelectionPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxSelectionFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxSelectionNative",
      "-Fu$nyxLazarus/lcl/units/$nyxSelectionPlatform", "-Fu$nyxLazarus/lcl/units/$nyxSelectionPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxSelectionPlatform", "-Fu$nyxLazarus/packager/units/$nyxSelectionPlatform",
      "-FU$nyxSelectionNative", "-FE$nyxSelectionNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxSelectionFlags + @('tests/nyx_selection_tests.lpr'))
    & (Join-Path $nyxSelectionNative 'nyx_selection_tests.exe') (Join-Path $nyxSelectionNative 'nyx.selection.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Selection source preparation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxSelectionFlags + @('-dNYX_COMPILED_SELECTION', 'tests/nyx_selection_tests.lpr'))
    & (Join-Path $nyxSelectionNative 'nyx_selection_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled native selection controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxSelectionNative", "-FE$nyxBrowserDir", '-dNYX_COMPILED_SELECTION', 'tests/nyx_selection_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/selection.html') -Destination $nyxBrowserDir
    if ($Target -eq 'selection') { exit 0 }
  }

  if ($Target -in @('gestures', 'all')) {
    # Pascal owns the contract, real-control and handwritten companion checks.
    # The optional CDP driver uses FPC's own WebSocket facilities; no Node tools.
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxGestureNative = Join-Path $nyxRoot 'build/gestures/lcl'
    New-Item -ItemType Directory -Force $nyxGestureNative | Out-Null
    $nyxGesturePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxGestureFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxGestureNative",
      "-Fu$nyxLazarus/lcl/units/$nyxGesturePlatform", "-Fu$nyxLazarus/lcl/units/$nyxGesturePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxGesturePlatform", "-Fu$nyxLazarus/packager/units/$nyxGesturePlatform",
      "-FU$nyxGestureNative", "-FE$nyxGestureNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxGestureFlags + @('tests/nyx_gesture_tests.lpr'))
    & (Join-Path $nyxGestureNative 'nyx_gesture_tests.exe') (Join-Path $nyxGestureNative 'nyx.gestures.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Gesture source preparation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxGestureFlags + @('-dNYX_COMPILED_GESTURES', 'tests/nyx_gesture_tests.lpr'))
    & (Join-Path $nyxGestureNative 'nyx_gesture_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled native gesture controls failed' }
    Invoke-NyxCompiler $nyxLclFpc (@('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
      '-Fusrc', "-FU$nyxGestureNative", "-FE$nyxGestureNative", 'tests/nyx_gesture_cdp_tests.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxGestureNative", "-FE$nyxBrowserDir", '-dNYX_COMPILED_GESTURES', 'tests/nyx_gesture_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-FE$nyxBrowserDir", 'tests/nyx_gesture_studio_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-FE$nyxBrowserDir", 'tests/nyx_gesture_physical_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxGestureHost in @('gestures.html', 'gesture-studio.html', 'gestures-physical.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxGestureHost") -Destination $nyxBrowserDir
    }
    if ($Target -eq 'gestures') { exit 0 }
  }

  if ($Target -in @('editing', 'all')) {
    # The Pascal fixture owns actual control tests and crafted-source generation.
    # Keep native pipelines serial and stage browser output away from live Studio.
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxEditingNative = Join-Path $nyxRoot 'build/editing/lcl'
    New-Item -ItemType Directory -Force $nyxEditingNative | Out-Null
    $nyxEditingPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxEditingFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxEditingNative",
      "-Fu$nyxLazarus/lcl/units/$nyxEditingPlatform", "-Fu$nyxLazarus/lcl/units/$nyxEditingPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxEditingPlatform", "-Fu$nyxLazarus/packager/units/$nyxEditingPlatform",
      "-FU$nyxEditingNative", "-FE$nyxEditingNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxEditingFlags + @('tests/nyx_editing_tests.lpr'))
    & (Join-Path $nyxEditingNative 'nyx_editing_tests.exe') (Join-Path $nyxEditingNative 'nyx.editing.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Editing source preparation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxEditingFlags + @('-dNYX_COMPILED_EDITING', 'tests/nyx_editing_tests.lpr'))
    & (Join-Path $nyxEditingNative 'nyx_editing_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled native editing controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxEditingNative", "-FE$nyxBrowserDir", '-dNYX_COMPILED_EDITING', 'tests/nyx_editing_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/editing.html') -Destination $nyxBrowserDir
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-FE$nyxBrowserDir", 'tests/nyx_editing_studio_tests.lpr'))
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/editing-studio.html') -Destination $nyxBrowserDir
    if ($Target -eq 'editing') { exit 0 }
  }

  if ($Target -in @('viewport', 'all')) {
    # The Pascal fixture owns actual control tests and crafted-source generation.
    # Keep native pipelines serial and stage browser output away from live Studio.
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxViewportNative = Join-Path $nyxRoot 'build/viewport/lcl'
    New-Item -ItemType Directory -Force $nyxViewportNative | Out-Null
    $nyxViewportPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxViewportFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxViewportNative",
      "-Fu$nyxLazarus/lcl/units/$nyxViewportPlatform", "-Fu$nyxLazarus/lcl/units/$nyxViewportPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxViewportPlatform", "-Fu$nyxLazarus/packager/units/$nyxViewportPlatform",
      "-FU$nyxViewportNative", "-FE$nyxViewportNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxViewportFlags + @('tests/nyx_viewport_tests.lpr'))
    & (Join-Path $nyxViewportNative 'nyx_viewport_tests.exe') (Join-Path $nyxViewportNative 'nyx.viewport.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Viewport source preparation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxViewportFlags + @('-dNYX_COMPILED_VIEWPORT', 'tests/nyx_viewport_tests.lpr'))
    & (Join-Path $nyxViewportNative 'nyx_viewport_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled native viewport controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxViewportNative", "-FE$nyxBrowserDir", '-dNYX_COMPILED_VIEWPORT', 'tests/nyx_viewport_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/viewport.html') -Destination $nyxBrowserDir
    if ($Target -eq 'viewport') { exit 0 }
  }

  if ($Target -in @('semantic-events', 'all')) {
    # Pascal inventories every default compound and exercises each real action;
    # both compilers then consume its exported, handwritten callback companion.
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxSemanticNative = Join-Path $nyxRoot 'build/semantics/lcl'
    New-Item -ItemType Directory -Force $nyxSemanticNative | Out-Null
    $nyxSemanticPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxSemanticFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxSemanticNative",
      "-Fu$nyxLazarus/lcl/units/$nyxSemanticPlatform", "-Fu$nyxLazarus/lcl/units/$nyxSemanticPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxSemanticPlatform", "-Fu$nyxLazarus/packager/units/$nyxSemanticPlatform",
      "-FU$nyxSemanticNative", "-FE$nyxSemanticNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxSemanticFlags + @('tests/nyx_semantic_events_tests.lpr'))
    & (Join-Path $nyxSemanticNative 'nyx_semantic_events_tests.exe') (Join-Path $nyxSemanticNative 'nyx.semantic.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Semantic event source preparation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxSemanticFlags + @('-dNYX_COMPILED_SEMANTICS', 'tests/nyx_semantic_events_tests.lpr'))
    & (Join-Path $nyxSemanticNative 'nyx_semantic_events_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled native semantic controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxSemanticNative", "-FE$nyxBrowserDir", '-dNYX_COMPILED_SEMANTICS', 'tests/nyx_semantic_events_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/semantic-events.html') -Destination $nyxBrowserDir
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-FE$nyxBrowserDir", 'tests/nyx_semantic_studio_tests.lpr'))
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/semantic-studio.html') -Destination $nyxBrowserDir
    if ($Target -eq 'semantic-events') { exit 0 }
  }

  if ($Target -eq 'named-events') {
    # Pascal fixtures own creator schemas, real controls, Studio/MCP assertions
    # and handwritten companion generation. Shell only orchestrates compilers.
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxNamedNative = Join-Path $nyxRoot 'build/named/lcl'
    New-Item -ItemType Directory -Force $nyxNamedNative | Out-Null
    $nyxNamedPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxNamedFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxNamedNative",
      "-Fu$nyxLazarus/lcl/units/$nyxNamedPlatform", "-Fu$nyxLazarus/lcl/units/$nyxNamedPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxNamedPlatform", "-Fu$nyxLazarus/packager/units/$nyxNamedPlatform",
      "-FU$nyxNamedNative", "-FE$nyxNamedNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxNamedFlags + @('tests/nyx_named_events_tests.lpr'))
    & (Join-Path $nyxNamedNative 'nyx_named_events_tests.exe') (Join-Path $nyxNamedNative 'nyx.named.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Named event companion preparation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxNamedFlags + @('-dNYX_COMPILED_NAMED', 'tests/nyx_named_events_tests.lpr'))
    & (Join-Path $nyxNamedNative 'nyx_named_events_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled native named event controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'
    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxNamedNative", "-FE$nyxBrowserDir", '-dNYX_COMPILED_NAMED', 'tests/nyx_named_events_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/named-events.html') -Destination $nyxBrowserDir
    exit 0
  }

  if ($Target -eq 'split') {
    # Both compiler contracts and real controls are Pascal fixtures. Stage
    # browser artifacts independently of the live LAN application when requested.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('-gh', 'tests/nyx_split_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_split_tests.exe') (Join-Path $nyxNativeDir 'nyx.split.generated.pas')

    if ($LASTEXITCODE -ne 0) { throw 'Split/platform portable checks failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('-gh', "-Fu$nyxNativeDir", 'tests/nyx_split_generated_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_split_generated_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Compiled split reconstruction failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLclPlatform = (& $nyxLclFpc '-iTP').Trim() + '-' + (& $nyxLclFpc '-iTO').Trim()
    $nyxSplitNative = Join-Path $nyxRoot 'build/split/lcl'
    New-Item -ItemType Directory -Force $nyxSplitNative | Out-Null
    $nyxSplitFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-Fusrc', '-Futests', '-Fustudio',
      "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform", "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxLclPlatform", "-Fu$nyxLazarus/packager/units/$nyxLclPlatform",
      "-FU$nyxSplitNative", "-FE$nyxSplitNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxSplitFlags + @('tests/nyx_split_controls_tests.lpr'))
    & (Join-Path $nyxSplitNative 'nyx_split_controls_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Native split controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    $nyxSplitBrowserFlags = @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxNativeDir", "-FE$nyxBrowserDir")
    foreach ($nyxProgram in @('tests/nyx_split_tests.lpr', 'tests/nyx_split_generated_tests.lpr',
      'tests/nyx_split_controls_tests.lpr', 'tests/nyx_studio_split_tests.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js ($nyxSplitBrowserFlags + @($nyxProgram))
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxHost in @('split.html', 'split-generated.html', 'split-controls.html', 'studio-split.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxHost") -Destination $nyxBrowserDir
    }
    exit 0
  }

  if ($Target -eq 'interactions') {
    # A native-created companion is compiled and executed with actual LCL
    # controls, then compiled for the real browser consumer. Assertions and
    # event synthesis are Pascal; this block only orchestrates those programs.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('-gh', 'tests/nyx_interaction_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_interaction_tests.exe') (Join-Path $nyxNativeDir 'nyx.interaction.generated.pas')

    if ($LASTEXITCODE -ne 0) { throw 'Interaction contracts failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLclPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxInteractionNative = Join-Path $nyxRoot 'build/interactions/lcl'
    New-Item -ItemType Directory -Force $nyxInteractionNative | Out-Null
    $nyxInteractionFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxNativeDir", '-dNYX_COMPILED_INTERACTIONS',
      "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform", "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxLclPlatform", "-Fu$nyxLazarus/packager/units/$nyxLclPlatform",
      "-FU$nyxInteractionNative", "-FE$nyxInteractionNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxInteractionFlags + @('tests/nyx_interaction_controls_tests.lpr'))
    & (Join-Path $nyxInteractionNative 'nyx_interaction_controls_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Native interaction controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    $nyxInteractionBrowser = @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxNativeDir", "-FE$nyxBrowserDir")
    Invoke-NyxCompiler $nyxPas2js ($nyxInteractionBrowser + @('tests/nyx_interaction_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxInteractionBrowser + @('-dNYX_COMPILED_INTERACTIONS', 'tests/nyx_interaction_controls_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxHost in @('interactions.html', 'interaction-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxHost") -Destination $nyxBrowserDir
    }
    exit 0
  }

  if ($Target -eq 'collection-inspectors') {
    # Pascal owns typed replay, actual controls and exact companion assertions.
    # Independent outputs preserve checked artifacts and compiler compatibility.
    # No listener, observing project or live service is launched/replaced here.
    $nyxCollectionRoot = Join-Path $nyxRoot 'build/collection-inspectors'
    $nyxCollectionNative = Join-Path $nyxCollectionRoot 'native'
    $nyxCollectionLcl = Join-Path $nyxCollectionRoot 'lcl'
    $nyxCollectionBrowser = Join-Path $nyxCollectionRoot 'browser'
    $nyxCollectionExport = Join-Path $nyxCollectionRoot 'export'
    $nyxCollectionControls = Join-Path $nyxCollectionRoot 'controls'

    if ($BrowserOutput) { $nyxCollectionBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxCollectionNative, $nyxCollectionLcl,
      $nyxCollectionBrowser, $nyxCollectionExport, $nyxCollectionControls | Out-Null
    $nyxCollectionFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxCollectionNative", "-FE$nyxCollectionNative")
    foreach ($nyxCollectionProgram in @('nyx_collection_queue_tests',
        'nyx_collection_pending_tests', 'nyx_design_source_tests', 'nyx_managed_source_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxCollectionFlags + @("tests/$nyxCollectionProgram.lpr"))
      & (Join-Path $nyxCollectionNative "$nyxCollectionProgram.exe") $nyxCollectionExport

      if ($LASTEXITCODE -ne 0) { throw "Collection shared fixture failed: $nyxCollectionProgram" }
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxCollectionPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxCollectionControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxCollectionExport",
      "-Fu$nyxLazarus/lcl/units/$nyxCollectionPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxCollectionPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxCollectionPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxCollectionPlatform",
      "-FU$nyxCollectionLcl", "-FE$nyxCollectionLcl")
    foreach ($nyxCollectionProgram in @('nyx_collection_queue_tests',
        'nyx_collection_pending_tests', 'nyx_collection_queue_controls', 'nyx_collection_queue_generated',
        'nyx_collection_authoring_tests')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxCollectionControlFlags + @("tests/$nyxCollectionProgram.lpr"))
      $nyxCollectionArgument = ''

      if ($nyxCollectionProgram -eq 'nyx_collection_queue_controls') {
        $nyxCollectionArgument = $nyxCollectionControls
      }
      & (Join-Path $nyxCollectionLcl "$nyxCollectionProgram.exe") $nyxCollectionArgument

      if ($LASTEXITCODE -ne 0) { throw "Collection native fixture failed: $nyxCollectionProgram" }
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxCollectionProgram in @('nyx_collection_queue_tests',
        'nyx_collection_pending_tests', 'nyx_collection_queue_browser', 'nyx_collection_queue_generated')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxCollectionExport", "-FE$nyxCollectionBrowser",
        "tests/$nyxCollectionProgram.lpr")
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxCollectionBrowser", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxCollectionBrowser 'rtl.js') -Force
    foreach ($nyxCollectionHost in @('index.html', 'collection-inspectors.html',
        'collection-inspector-controls.html', 'collection-inspector-generated.html',
        'collection-inspector-pending.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxCollectionHost") -Destination $nyxCollectionBrowser
    }
    Write-Host 'Collection consumers and Studio staged; browser execution needs its permitted host.'
    exit 0
  }

  if ($Target -eq 'event-inspectors') {
    # Independent typed callback preparation and actual ordinary controls.
    # Pascal owns the assertions, paired export and reconstruction. This target
    # never launches a listener or replaces a running service or user project.
    $nyxEventRoot = Join-Path $nyxRoot 'build/event-inspectors'
    $nyxEventNative = Join-Path $nyxEventRoot 'native'
    $nyxEventLcl = Join-Path $nyxEventRoot 'lcl'
    $nyxEventBrowser = Join-Path $nyxEventRoot 'browser'
    $nyxEventExport = Join-Path $nyxEventRoot 'export'
    $nyxEventControls = Join-Path $nyxEventRoot 'controls'

    if ($BrowserOutput) { $nyxEventBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxEventNative, $nyxEventLcl,
      $nyxEventBrowser, $nyxEventExport, $nyxEventControls | Out-Null
    $nyxEventFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxEventNative", "-FE$nyxEventNative")
    foreach ($nyxEventProgram in @('nyx_event_queue_tests', 'nyx_state_source_tests',
        'nyx_design_queue_tests', 'nyx_design_source_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxEventFlags + @("tests/$nyxEventProgram.lpr"))
      & (Join-Path $nyxEventNative "$nyxEventProgram.exe") $nyxEventExport

      if ($LASTEXITCODE -ne 0) { throw "Callback shared fixture failed: $nyxEventProgram" }
    }
    Invoke-NyxCompiler $nyxFpc ($nyxEventFlags + @("-Fu$nyxEventExport",
      'tests/nyx_event_queue_generated.lpr'))
    & (Join-Path $nyxEventNative 'nyx_event_queue_generated.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Compiled callback companion reconstruction failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxEventPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxEventControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxEventPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxEventPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxEventPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxEventPlatform",
      "-FU$nyxEventLcl", "-FE$nyxEventLcl")
    foreach ($nyxEventProgram in @('nyx_event_queue_controls', 'nyx_state_authoring_controls')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxEventControlFlags + @("tests/$nyxEventProgram.lpr"))
      & (Join-Path $nyxEventLcl "$nyxEventProgram.exe") $nyxEventControls

      if ($LASTEXITCODE -ne 0) { throw "Actual callback fixture failed: $nyxEventProgram" }
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxEventProgram in @('nyx_event_queue_tests', 'nyx_event_queue_browser',
        'nyx_event_queue_generated')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxEventExport", "-FE$nyxEventBrowser",
        "tests/$nyxEventProgram.lpr")
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxEventBrowser", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxEventBrowser 'rtl.js') -Force
    foreach ($nyxEventHost in @('index.html', 'event-inspectors.html',
        'event-inspector-controls.html', 'event-inspector-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxEventHost") -Destination $nyxEventBrowser
    }
    Write-Host 'Callback consumers and Studio staged; browser execution needs its permitted host.'
    exit 0
  }

  if ($Target -eq 'state-inspectors') {
    # Pascal fixtures own typed admission, real controls, source/history and
    # retirement assertions. This bounded target starts no listener and never
    # relinks a running Studio service or changes its project/enrollment.
    $nyxInspectorRoot = Join-Path $nyxRoot 'build/state-inspectors'
    $nyxInspectorNative = Join-Path $nyxInspectorRoot 'native'
    $nyxInspectorLcl = Join-Path $nyxInspectorRoot 'lcl'
    $nyxInspectorBrowser = Join-Path $nyxInspectorRoot 'browser'
    $nyxInspectorControls = Join-Path $nyxInspectorRoot 'controls'

    if ($BrowserOutput) { $nyxInspectorBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxInspectorNative, $nyxInspectorLcl,
      $nyxInspectorBrowser, $nyxInspectorControls | Out-Null
    $nyxInspectorFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxInspectorNative", "-FE$nyxInspectorNative")
    foreach ($nyxInspectorProgram in @('nyx_state_source_tests', 'nyx_design_queue_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxInspectorFlags + @("tests/$nyxInspectorProgram.lpr"))
      & (Join-Path $nyxInspectorNative "$nyxInspectorProgram.exe")

      if ($LASTEXITCODE -ne 0) { throw "State inspector shared fixture failed: $nyxInspectorProgram" }
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxInspectorPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxInspectorControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxInspectorPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxInspectorPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxInspectorPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxInspectorPlatform",
      "-FU$nyxInspectorLcl", "-FE$nyxInspectorLcl")
    foreach ($nyxInspectorProgram in @('nyx_state_source_controls',
        'nyx_state_authoring_controls', 'nyx_canvas_queue_controls')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxInspectorControlFlags + @("tests/$nyxInspectorProgram.lpr"))
      & (Join-Path $nyxInspectorLcl "$nyxInspectorProgram.exe") $nyxInspectorControls

      if ($LASTEXITCODE -ne 0) { throw "Actual state inspector fixture failed: $nyxInspectorProgram" }
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxInspectorProgram in @('tests/nyx_state_source_tests.lpr',
        'tests/nyx_state_source_browser.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxInspectorBrowser", $nyxInspectorProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxInspectorBrowser 'rtl.js') -Force
    foreach ($nyxInspectorHost in @('index.html', 'state-inspectors.html', 'state-inspector-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxInspectorHost") -Destination $nyxInspectorBrowser
    }
    Write-Host 'Browser Studio, matched worker and portable state checks staged; runtime needs its permitted host.'
    exit 0
  }

  if ($Target -eq 'state-bindings') {
    # Pascal owns bounded semantic/retry/history assertions and exports the exact
    # admitted companion. This target starts no Studio/MCP/HTTP listener and
    # changes no user project, enrollment or existing service artifact.
    $nyxStateRoot = Join-Path $nyxRoot 'build/state-bindings'
    $nyxStateNative = Join-Path $nyxStateRoot 'native'
    $nyxStateExport = Join-Path $nyxStateRoot 'export'
    $nyxStateLcl = Join-Path $nyxStateRoot 'lcl'
    $nyxStateBrowser = Join-Path $nyxStateRoot 'browser'

    if ($BrowserOutput) { $nyxStateBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxStateNative, $nyxStateExport, $nyxStateLcl, $nyxStateBrowser | Out-Null
    $nyxStateFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxStateNative", "-FE$nyxStateNative")
    Invoke-NyxCompiler $nyxFpc ($nyxStateFlags + @('tests/nyx_agent_state_tests.lpr'))
    & (Join-Path $nyxStateNative 'nyx_agent_state_tests.exe') $nyxStateExport

    if ($LASTEXITCODE -ne 0) { throw 'Semantic state/default/binding checks failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxStatePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxStateControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxStateExport",
      "-Fu$nyxLazarus/lcl/units/$nyxStatePlatform", "-Fu$nyxLazarus/lcl/units/$nyxStatePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxStatePlatform", "-Fu$nyxLazarus/packager/units/$nyxStatePlatform",
      "-FU$nyxStateLcl", "-FE$nyxStateLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxStateControlFlags + @('tests/nyx_agent_state_schema.lpr'))
    & (Join-Path $nyxStateLcl 'nyx_agent_state_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Offline semantic state MCP discovery failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxStateControlFlags + @('tests/nyx_agent_state_controls.lpr'))
    & (Join-Path $nyxStateLcl 'nyx_agent_state_controls.exe') (Join-Path $nyxStateExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Compiled semantic native controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxStateBrowserFlags = @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxStateExport", "-FE$nyxStateBrowser")
    foreach ($nyxStateProgram in @('nyx_agent_state_tests', 'nyx_agent_state_browser')) {
      Invoke-NyxCompiler $nyxPas2js ($nyxStateBrowserFlags + @("tests/$nyxStateProgram.lpr"))
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxStateBrowser 'rtl.js')
    foreach ($nyxStateHost in @('agent-state.html', 'agent-state-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxStateHost") -Destination $nyxStateBrowser
    }
    Write-Host 'Browser semantic and exact compiled-control consumers staged; execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'collection-bindings') {
    # Pascal owns semantic admission/refusals/context assertions and exports the
    # unchanged companion. No listener, deployment or operator project mutation.
    $nyxCollectionAgentRoot = Join-Path $nyxRoot 'build/collection-bindings'
    $nyxCollectionAgentNative = Join-Path $nyxCollectionAgentRoot 'native'
    $nyxCollectionAgentExport = Join-Path $nyxCollectionAgentRoot 'export'
    $nyxCollectionAgentLcl = Join-Path $nyxCollectionAgentRoot 'lcl'
    $nyxCollectionAgentBrowser = Join-Path $nyxCollectionAgentRoot 'browser'

    if ($BrowserOutput) { $nyxCollectionAgentBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxCollectionAgentNative, $nyxCollectionAgentExport,
      $nyxCollectionAgentLcl, $nyxCollectionAgentBrowser | Out-Null
    $nyxCollectionAgentFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxCollectionAgentNative", "-FE$nyxCollectionAgentNative")
    Invoke-NyxCompiler $nyxFpc ($nyxCollectionAgentFlags + @('tests/nyx_agent_collection_tests.lpr'))
    & (Join-Path $nyxCollectionAgentNative 'nyx_agent_collection_tests.exe') $nyxCollectionAgentExport

    if ($LASTEXITCODE -ne 0) { throw 'Semantic collection admission checks failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxCollectionAgentFlags + @('tests/nyx_agent_collection_inheritance.lpr'))
    & (Join-Path $nyxCollectionAgentNative 'nyx_agent_collection_inheritance.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Masked collection inheritance checks failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxCollectionAgentPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxCollectionAgentControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxCollectionAgentExport",
      "-Fu$nyxLazarus/lcl/units/$nyxCollectionAgentPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxCollectionAgentPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxCollectionAgentPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxCollectionAgentPlatform",
      "-FU$nyxCollectionAgentLcl", "-FE$nyxCollectionAgentLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxCollectionAgentControlFlags + @('tests/nyx_agent_collection_schema.lpr'))
    & (Join-Path $nyxCollectionAgentLcl 'nyx_agent_collection_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual collection MCP discovery checks failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxCollectionAgentControlFlags + @('tests/nyx_agent_collection_controls.lpr'))
    & (Join-Path $nyxCollectionAgentLcl 'nyx_agent_collection_controls.exe') (Join-Path $nyxCollectionAgentExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled semantic collection controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxCollectionAgentProgram in @('nyx_agent_collection_tests',
        'nyx_agent_collection_inheritance', 'nyx_agent_collection_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-Fu$nyxCollectionAgentExport", "-FE$nyxCollectionAgentBrowser",
        "tests/$nyxCollectionAgentProgram.lpr")
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxCollectionAgentBrowser 'rtl.js')
    foreach ($nyxCollectionAgentHost in @('agent-collections.html', 'agent-collection-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxCollectionAgentHost") -Destination $nyxCollectionAgentBrowser
    }
    Write-Host 'Semantic collection consumers staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'retained-arrangement') {
    # Pascal owns ownership/identity admission and real input assertions. This
    # branch only compiles/runs/stages artifacts and never launches a listener.
    $nyxArrangeRoot = Join-Path $nyxRoot 'build/retained-arrangement/maintained'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxArrangePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxArrangeChecked = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh', '-Fusrc')
    foreach ($nyxArrangeCompiler in @(@($nyxFpc, 'stable'), @($nyxLclFpc, 'matched'))) {
      $nyxArrangeUnits = Join-Path $nyxArrangeRoot $nyxArrangeCompiler[1]
      New-Item -ItemType Directory -Force $nyxArrangeUnits | Out-Null
      Invoke-NyxCompiler $nyxArrangeCompiler[0] ($nyxArrangeChecked + @(
        "-FU$nyxArrangeUnits", "-FE$nyxArrangeUnits", 'tests/nyx_arrangement_tests.lpr'))
      & (Join-Path $nyxArrangeUnits 'nyx_arrangement_tests.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Owned arrangement qualification failed' }
    }
    $nyxArrangeSource = @()
    if ($ArrangementSourceDirectory) {
      $nyxArrangeSourcePath = [IO.Path]::GetFullPath($ArrangementSourceDirectory)
      if (-not (Test-Path -LiteralPath (Join-Path $nyxArrangeSourcePath 'nyx.generated.view.pas'))) {
        throw 'The exact semantic arrangement companion is missing'
      }
      $nyxArrangeSource = @('-dNYX_ARRANGEMENT_MCP', "-Fu$nyxArrangeSourcePath")
    }
    $nyxArrangeNative = Join-Path $nyxArrangeRoot 'controls'
    New-Item -ItemType Directory -Force $nyxArrangeNative | Out-Null
    Invoke-NyxCompiler $nyxLclFpc ($nyxArrangeChecked + $nyxArrangeSource + @('-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxArrangePlatform", "-Fu$nyxLazarus/lcl/units/$nyxArrangePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxArrangePlatform", "-Fu$nyxLazarus/packager/units/$nyxArrangePlatform",
      "-FU$nyxArrangeNative", "-FE$nyxArrangeNative", 'tests/nyx_projection_refresh_tests.lpr'))
    & (Join-Path $nyxArrangeNative 'nyx_projection_refresh_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native retained arrangement failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxArrangeBrowser = Join-Path $nyxArrangeRoot 'web'
    New-Item -ItemType Directory -Force $nyxArrangeBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Futests',
      "-FE$nyxArrangeBrowser", 'tests/nyx_arrangement_tests.lpr')
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Fustudio',
      '-Futests', "-FE$nyxArrangeBrowser", 'tests/nyx_projection_refresh_tests.lpr') + $nyxArrangeSource)
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxArrangeBrowser 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/arrangement-tests.html'),
      (Join-Path $nyxRoot 'studio/web/projection-refresh.html') -Destination $nyxArrangeBrowser -Force
    Write-Host 'Owned/actual native arrangements pass; browser execution still needs its isolated HTTP host.'
    exit 0
  }

  if ($Target -eq 'native-measurement') {
    # Preserve the exact manual-presentation MCP companion used by the ordinary
    # 30-check Studio journey. Pascal owns counters, interaction assertions and
    # heap accounting; this branch only compiles/runs/stages platform artifacts.
    if (-not $ResponsiveSourceDirectory) {
      throw 'Supply -ResponsiveSourceDirectory with the exported manual-presentation companion'
    }
    $nyxMeasurementSource = [IO.Path]::GetFullPath($ResponsiveSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxMeasurementSource 'nyx.generated.view.pas'))) {
      throw 'The exact semantic measurement companion is missing'
    }
    $nyxMeasurementRoot = Join-Path $nyxRoot 'build/native-measurement/maintained'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxMeasurementPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxMeasurementChecked = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh', '-Fusrc')
    foreach ($nyxMeasurementCompiler in @(@($nyxFpc, 'stable'), @($nyxLclFpc, 'matched'))) {
      $nyxMeasurementUnits = Join-Path $nyxMeasurementRoot $nyxMeasurementCompiler[1]
      New-Item -ItemType Directory -Force $nyxMeasurementUnits | Out-Null
      Invoke-NyxCompiler $nyxMeasurementCompiler[0] ($nyxMeasurementChecked + @(
        "-FU$nyxMeasurementUnits", "-FE$nyxMeasurementUnits", 'tests/nyx_text_lookup_tests.lpr'))
      & (Join-Path $nyxMeasurementUnits 'nyx_text_lookup_tests.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Exact portable text lookup failed' }
    }
    $nyxMeasurementNative = Join-Path $nyxMeasurementRoot 'studio'
    $nyxMeasurementProjects = Join-Path $nyxMeasurementRoot 'projects'
    New-Item -ItemType Directory -Force $nyxMeasurementNative, $nyxMeasurementProjects | Out-Null
    Invoke-NyxCompiler $nyxLclFpc ($nyxMeasurementChecked + @('-Fustudio', '-Futests',
      '-dNYX_STUDIO_PROFILE', '-dNYX_LCL_LAYOUT_PROFILE',
      '-dNYX_PRESENTATION_CONSUMER', '-dNYX_MANUAL_CONSUMER', "-Fu$nyxMeasurementSource",
      "-Fu$nyxLazarus/lcl/units/$nyxMeasurementPlatform", "-Fu$nyxLazarus/lcl/units/$nyxMeasurementPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxMeasurementPlatform", "-Fu$nyxLazarus/packager/units/$nyxMeasurementPlatform",
      "-FU$nyxMeasurementNative", "-FE$nyxMeasurementNative", 'tests/nyx_responsive_studio.lpr'))
    & (Join-Path $nyxMeasurementNative 'nyx_responsive_studio.exe') $nyxMeasurementProjects (Join-Path $nyxMeasurementSource 'nyx.generated.view.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native measurement/editor journey failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxMeasurementBrowser = Join-Path $nyxMeasurementRoot 'web'
    if ($BrowserOutput) { $nyxMeasurementBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxMeasurementBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Jirtl.js',
      "-FE$nyxMeasurementBrowser", 'tests/nyx_text_lookup_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMeasurementBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/text-lookup.html') -Destination $nyxMeasurementBrowser
    Write-Host 'Browser lookup execution needs an independently admitted HTTP fixture host.'
    exit 0
  }

  if ($Target -eq 'containers') {
    # Pascal owns allocation, ancestry, admission, history and actual controls.
    # This target stages browser consumers and never launches a service.
    $nyxContainerRoot = Join-Path $nyxRoot 'build/container-presentations'
    $nyxContainerSource = [IO.Path]::GetFullPath($ContainerSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxContainerSource 'nyx.generated.view.pas'))) {
      throw 'Export the container MCP companion before building physical consumers'
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxContainerStable = Join-Path $nyxContainerRoot 'stable'
    $nyxContainerMatched = Join-Path $nyxContainerRoot 'matched'
    $nyxContainerNative = Join-Path $nyxContainerRoot 'native'
    $nyxContainerDriver = Join-Path $nyxContainerRoot 'driver'
    $nyxContainerBrowser = Join-Path $nyxContainerRoot 'web'
    if ($BrowserOutput) { $nyxContainerBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxContainerStable, $nyxContainerMatched,
      $nyxContainerNative, $nyxContainerDriver, $nyxContainerBrowser | Out-Null
    $nyxContainerFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxContainerSource")
    foreach ($nyxContainerCompiler in @(@($nyxFpc, $nyxContainerStable), @($nyxLclFpc, $nyxContainerMatched))) {
      $nyxContainerUnicodeExport = Join-Path $nyxContainerCompiler[1] 'unicode'
      New-Item -ItemType Directory -Force $nyxContainerUnicodeExport | Out-Null
      Invoke-NyxCompiler $nyxContainerCompiler[0] ($nyxContainerFlags + @(
        "-FU$($nyxContainerCompiler[1])", "-FE$($nyxContainerCompiler[1])", 'tests/nyx_container_tests.lpr'))
      & (Join-Path $nyxContainerCompiler[1] 'nyx_container_tests.exe') (Join-Path $nyxContainerUnicodeExport 'nyx.generated.view.pas')
      if ($LASTEXITCODE -ne 0) { throw 'Shared container contract failed' }
    }
    $nyxContainerUnicode = Join-Path $nyxContainerStable 'unicode'
    if ((Get-FileHash -LiteralPath (Join-Path $nyxContainerUnicode 'nyx.generated.view.pas')).Hash -cne
      (Get-FileHash -LiteralPath (Join-Path $nyxContainerMatched 'unicode/nyx.generated.view.pas')).Hash) {
      throw 'Compiler Unicode container exports differ'
    }
    $nyxContainerPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxContainerFlags + @(
      "-Fu$nyxLazarus/lcl/units/$nyxContainerPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxContainerPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContainerPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxContainerPlatform",
      "-FU$nyxContainerNative", "-FE$nyxContainerNative", 'tests/nyx_container_controls.lpr'))
    & (Join-Path $nyxContainerNative 'nyx_container_controls.exe') (Join-Path $nyxContainerNative 'containers.png')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native container allocation failed' }
    # Recompile the same physical consumer against the exact exported Unicode
    # name. Separate units prevent an earlier English companion from satisfying it.
    $nyxContainerUnicodeNative = Join-Path $nyxContainerRoot 'unicode-native'
    New-Item -ItemType Directory -Force $nyxContainerUnicodeNative | Out-Null
    $nyxContainerUnicodeFlags = @($nyxContainerFlags | Where-Object { $_ -ne "-Fu$nyxContainerSource" })
    Invoke-NyxCompiler $nyxLclFpc ($nyxContainerUnicodeFlags + @(
      "-Fu$nyxContainerUnicode", "-Fu$nyxLazarus/lcl/units/$nyxContainerPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxContainerPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContainerPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxContainerPlatform",
      "-FU$nyxContainerUnicodeNative", "-FE$nyxContainerUnicodeNative", 'tests/nyx_container_controls.lpr'))
    & (Join-Path $nyxContainerUnicodeNative 'nyx_container_controls.exe') (Join-Path $nyxContainerUnicodeNative 'containers.png')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native Unicode container name failed' }
    foreach ($nyxContainerDriverProgram in @('tests/nyx_container_mcp_review.lpr', 'tests/nyx_responsive_browser_review.lpr')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxContainerFlags + @(
        "-FU$nyxContainerDriver", "-FE$nyxContainerDriver", $nyxContainerDriverProgram))
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxContainerProgram in @('tests/nyx_container_tests.lpr', 'tests/nyx_container_controls.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxContainerSource", '-Jirtl.js', "-FE$nyxContainerBrowser", $nyxContainerProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxContainerBrowser 'rtl.js')
    $nyxContainerUnicodeBrowser = Join-Path $nyxContainerBrowser 'unicode'
    New-Item -ItemType Directory -Force $nyxContainerUnicodeBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Futests', "-Fu$nyxContainerUnicode", '-Jirtl.js', "-FE$nyxContainerUnicodeBrowser", 'tests/nyx_container_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxContainerUnicodeBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/containers.html') -Destination $nyxContainerUnicodeBrowser
    foreach ($nyxContainerHost in @('containers.html', 'container-tests.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxContainerHost") -Destination $nyxContainerBrowser
    }
    Write-Host 'Container browser consumers, Studio and matched worker staged; execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -in @('responsive','presentations','manual-presentations')) {
    # Pascal owns interval/cascade, semantic/paired and actual control assertions.
    # These artifacts never start or replace a listener or refresh enrollment.
    $nyxResponsiveRoot = Join-Path $nyxRoot "build/$Target"
    $nyxResponsiveContract = 'nyx_responsive_tests'
    $nyxResponsiveDefines = @()
    if ($Target -in @('presentations','manual-presentations')) {
      $nyxResponsiveContract = 'nyx_presentations_tests'
      $nyxResponsiveDefines = @('-dNYX_PRESENTATION_CONSUMER')
    }
    if ($Target -eq 'manual-presentations') {
      $nyxResponsiveDefines += '-dNYX_MANUAL_CONSUMER'
    }
    $nyxResponsiveStable = Join-Path $nyxResponsiveRoot 'stable'
    $nyxResponsiveMatched = Join-Path $nyxResponsiveRoot 'maintained-matched'
    $nyxResponsiveLcl = Join-Path $nyxResponsiveRoot 'lcl'
    $nyxResponsiveDriver = Join-Path $nyxResponsiveRoot 'driver'
    $nyxResponsiveExport = Join-Path $nyxResponsiveRoot 'export-stable'
    $nyxResponsiveMatchedExport = Join-Path $nyxResponsiveRoot 'export-matched'
    $nyxResponsiveBrowser = Join-Path $nyxResponsiveRoot 'staged'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''

    if ($BrowserOutput) { $nyxResponsiveBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxResponsiveStable, $nyxResponsiveMatched,
      $nyxResponsiveLcl, $nyxResponsiveDriver, $nyxResponsiveExport, $nyxResponsiveMatchedExport,
      $nyxResponsiveBrowser | Out-Null
    $nyxResponsiveFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxResponsiveCompiler in @(
      @($nyxFpc, $nyxResponsiveStable, $nyxResponsiveExport),
      @($nyxLclFpc, $nyxResponsiveMatched, $nyxResponsiveMatchedExport))) {
      Invoke-NyxCompiler $nyxResponsiveCompiler[0] ($nyxResponsiveFlags + $nyxResponsiveDefines + @(
        "-FU$($nyxResponsiveCompiler[1])", "-FE$($nyxResponsiveCompiler[1])",
        "tests/$nyxResponsiveContract.lpr"))
      $nyxResponsiveExportFile = Join-Path $nyxResponsiveCompiler[2] 'nyx.generated.view.pas'
      $nyxResponsiveContractArguments = @($nyxResponsiveExportFile)
      if ($Target -in @('presentations','manual-presentations')) {
        $nyxResponsiveUnicode = Join-Path $nyxResponsiveCompiler[1] 'unicode'
        New-Item -ItemType Directory -Force $nyxResponsiveUnicode | Out-Null
        $nyxResponsiveContractArguments += (Join-Path $nyxResponsiveUnicode 'nyx.generated.view.pas')
      }
      & (Join-Path $nyxResponsiveCompiler[1] "$nyxResponsiveContract.exe") @nyxResponsiveContractArguments

      if ($LASTEXITCODE -ne 0) { throw 'Responsive semantic/paired qualification failed' }
      if ($Target -in @('presentations','manual-presentations')) {
        Invoke-NyxCompiler $nyxResponsiveCompiler[0] ($nyxResponsiveFlags + @(
          "-Fu$nyxResponsiveUnicode", "-FU$nyxResponsiveUnicode", "-FE$nyxResponsiveUnicode",
          'tests/nyx_presentation_names.lpr'))
        & (Join-Path $nyxResponsiveUnicode 'nyx_presentation_names.exe')
        if ($LASTEXITCODE -ne 0) { throw 'Compiled Unicode presentation qualification failed' }
      }
    }

    if ((Get-FileHash -LiteralPath (Join-Path $nyxResponsiveExport 'nyx.generated.view.pas')).Hash -cne
      (Get-FileHash -LiteralPath (Join-Path $nyxResponsiveMatchedExport 'nyx.generated.view.pas')).Hash) {
      throw 'Responsive compiler exports differ'
    }
    $nyxResponsiveSource = $nyxResponsiveExport

    if ($ResponsiveSourceDirectory) {
      $nyxResponsiveSource = [IO.Path]::GetFullPath($ResponsiveSourceDirectory)

      if (-not (Test-Path -LiteralPath (Join-Path $nyxResponsiveSource 'nyx.generated.view.pas'))) {
        throw 'Explicit responsive semantic companion is missing'
      }
    }
    $nyxResponsivePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxResponsiveControlFlags = $nyxResponsiveFlags + $nyxResponsiveDefines + @("-Fu$nyxResponsiveSource",
      "-Fu$nyxLazarus/lcl/units/$nyxResponsivePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxResponsivePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxResponsivePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxResponsivePlatform",
      "-FU$nyxResponsiveLcl", "-FE$nyxResponsiveLcl")
    foreach ($nyxResponsiveProgram in @('nyx_responsive_controls', 'nyx_responsive_studio')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxResponsiveControlFlags + @("tests/$nyxResponsiveProgram.lpr"))

      if ($nyxResponsiveProgram -eq 'nyx_responsive_controls') {
        & (Join-Path $nyxResponsiveLcl "$nyxResponsiveProgram.exe") $nyxResponsiveLcl
      } else {
        $nyxResponsiveProjectRoot = Join-Path $nyxResponsiveLcl 'projects'
        $nyxResponsiveSourceFile = Join-Path $nyxResponsiveSource 'nyx.generated.view.pas'
        & (Join-Path $nyxResponsiveLcl "$nyxResponsiveProgram.exe") $nyxResponsiveProjectRoot $nyxResponsiveSourceFile
      }

      if ($LASTEXITCODE -ne 0) { throw 'Actual native responsive qualification failed' }
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxResponsiveProgram in @("tests/$nyxResponsiveContract.lpr",
      'tests/nyx_responsive_controls.lpr', 'tests/nyx_responsive_studio_browser.lpr',
      'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js (@('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxResponsiveSource", '-Jirtl.js', "-FE$nyxResponsiveBrowser",
        $nyxResponsiveProgram) + $nyxResponsiveDefines)
    }
    if ($Target -in @('presentations','manual-presentations')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/presentations.html') -Destination $nyxResponsiveBrowser
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/presentation-names.html') -Destination $nyxResponsiveBrowser
      $nyxResponsiveUnicodeSource = Join-Path $nyxResponsiveStable 'unicode'
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        "-Fu$nyxResponsiveUnicodeSource", '-Jirtl.js', "-FE$nyxResponsiveBrowser",
        'tests/nyx_presentation_names.lpr')
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tmodule', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Jirtl.js', "-FE$nyxResponsiveBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxResponsiveBrowser 'rtl.js')
    foreach ($nyxResponsiveHost in @('responsive.html', 'responsive-contracts.html',
      'responsive-studio.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxResponsiveHost") -Destination $nyxResponsiveBrowser
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxResponsiveFlags + @(
      "-FU$nyxResponsiveDriver", "-FE$nyxResponsiveDriver", 'tests/nyx_responsive_browser_review.lpr'))
    Write-Host 'Responsive consumers/Studio/worker staged; browser execution needs an admitted host.'
    exit 0
  }

  if ($Target -eq 'guides') {
    # The semantic author supplies the exact source; Pascal owns guide rules,
    # actual input/paint and paired worker/history assertions. No service starts.
    $nyxGuideRoot = Join-Path $nyxRoot 'build/alignment'
    $nyxGuideSource = [IO.Path]::GetFullPath($GuideSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxGuideSource 'nyx.generated.view.pas'))) {
      throw 'Export the alignment MCP companion before building its physical consumers'
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxGuideStable = Join-Path $nyxGuideRoot 'shared'
    $nyxGuideMatched = Join-Path $nyxGuideRoot 'matched'
    $nyxGuideNative = Join-Path $nyxGuideRoot 'native'
    $nyxGuideDriver = Join-Path $nyxGuideRoot 'driver'
    $nyxGuideBrowser = Join-Path $nyxGuideRoot 'web'
    if ($BrowserOutput) { $nyxGuideBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxGuideStable, $nyxGuideMatched,
      $nyxGuideNative, $nyxGuideDriver, $nyxGuideBrowser | Out-Null
    $nyxGuideFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxGuideCompiler in @(@($nyxFpc, $nyxGuideStable), @($nyxLclFpc, $nyxGuideMatched))) {
      Invoke-NyxCompiler $nyxGuideCompiler[0] ($nyxGuideFlags + @(
        "-FU$($nyxGuideCompiler[1])", "-FE$($nyxGuideCompiler[1])", 'tests/nyx_resize_tests.lpr'))
      & (Join-Path $nyxGuideCompiler[1] 'nyx_resize_tests.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Shared alignment contract failed' }
    }
    $nyxGuidePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxGuideFlags + @("-Fu$nyxGuideSource",
      "-Fu$nyxLazarus/lcl/units/$nyxGuidePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxGuidePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxGuidePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxGuidePlatform",
      "-FU$nyxGuideNative", "-FE$nyxGuideNative", 'tests/nyx_guides_studio.lpr'))
    & (Join-Path $nyxGuideNative 'nyx_guides_studio.exe') (Join-Path $nyxGuideNative 'projects') (Join-Path $nyxGuideSource 'nyx.generated.view.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native Studio alignment failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxGuideFlags + @("-FU$nyxGuideDriver", "-FE$nyxGuideDriver",
      'tests/nyx_guides_browser_review.lpr'))
    Invoke-NyxCompiler $nyxLclFpc ($nyxGuideFlags + @("-FU$nyxGuideDriver", "-FE$nyxGuideDriver",
      'tests/nyx_responsive_browser_review.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxGuideProgram in @('tests/nyx_resize_tests.lpr',
      'tests/nyx_guides_studio_browser.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', '-Jirtl.js', "-FE$nyxGuideBrowser", $nyxGuideProgram)
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tmodule', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Futests', '-Jirtl.js', "-FE$nyxGuideBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxGuideBrowser 'rtl.js')
    foreach ($nyxGuideHost in @('resize.html', 'guides-studio.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxGuideHost") -Destination $nyxGuideBrowser
    }
    Write-Host 'Alignment consumers staged; execute the pointer driver against the explicit semantic workspace.'
    exit 0
  }
  if ($Target -eq 'move-snapping') {
    # Pascal owns copied policies, strict tickets, real gestures/paint and paired
    # history. This script compiles/stages tools and never starts a service.
    $nyxMoveRoot = Join-Path $nyxRoot 'build/move-snapping'
    $nyxMoveSource = [IO.Path]::GetFullPath($MoveSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxMoveSource 'nyx.generated.view.pas'))) {
      throw 'Export the movement MCP companion before building physical consumers'
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxMoveStable = Join-Path $nyxMoveRoot 'stable'
    $nyxMoveMatched = Join-Path $nyxMoveRoot 'matched'
    $nyxMoveNative = Join-Path $nyxMoveRoot 'native'
    $nyxMoveDriver = Join-Path $nyxMoveRoot 'driver'
    $nyxMoveBrowser = Join-Path $nyxMoveRoot 'web'
    if ($BrowserOutput) { $nyxMoveBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxMoveStable, $nyxMoveMatched,
      $nyxMoveNative, $nyxMoveDriver, $nyxMoveBrowser | Out-Null
    $nyxMoveFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxMoveCompiler in @(@($nyxFpc, $nyxMoveStable), @($nyxLclFpc, $nyxMoveMatched))) {
      Invoke-NyxCompiler $nyxMoveCompiler[0] ($nyxMoveFlags + @(
        "-FU$($nyxMoveCompiler[1])", "-FE$($nyxMoveCompiler[1])", 'tests/nyx_resize_tests.lpr'))
      & (Join-Path $nyxMoveCompiler[1] 'nyx_resize_tests.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Shared movement contract failed' }
    }
    $nyxMovePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxMoveFlags + @("-Fu$nyxMoveSource",
      "-Fu$nyxLazarus/lcl/units/$nyxMovePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxMovePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxMovePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxMovePlatform",
      "-FU$nyxMoveNative", "-FE$nyxMoveNative", 'tests/nyx_move_studio.lpr'))
    & (Join-Path $nyxMoveNative 'nyx_move_studio.exe') (Join-Path $nyxMoveNative 'projects') (Join-Path $nyxMoveSource 'nyx.generated.view.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native Studio movement failed' }
    foreach ($nyxMoveHostProgram in @('tests/nyx_move_browser_review.lpr', 'tests/nyx_responsive_browser_review.lpr')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxMoveFlags + @("-FU$nyxMoveDriver", "-FE$nyxMoveDriver", $nyxMoveHostProgram))
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxMoveProgram in @('tests/nyx_resize_tests.lpr', 'tests/nyx_move_studio_browser.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', '-Jirtl.js', "-FE$nyxMoveBrowser", $nyxMoveProgram)
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tmodule', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Jirtl.js', "-FE$nyxMoveBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMoveBrowser 'rtl.js')
    foreach ($nyxMovePage in @('resize.html', 'move-studio.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxMovePage") -Destination $nyxMoveBrowser
    }
    Write-Host 'Movement consumers staged; run the host driver against the explicit semantic project.'
    exit 0
  }
  if ($Target -eq 'flow-placement') {
    # Pascal owns copied policies, strict tickets, real gestures/paint and paired
    # history. This script compiles/stages tools and never starts a service.
    $nyxFlowRoot = Join-Path $nyxRoot 'build/flow-placement'
    $nyxFlowSource = [IO.Path]::GetFullPath($FlowSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxFlowSource 'nyx.generated.view.pas'))) {
      throw 'Export the flow placement MCP companion before building physical consumers'
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxFlowStable = Join-Path $nyxFlowRoot 'stable'
    $nyxFlowMatched = Join-Path $nyxFlowRoot 'matched'
    $nyxFlowNative = Join-Path $nyxFlowRoot 'native'
    $nyxFlowDriver = Join-Path $nyxFlowRoot 'driver'
    $nyxFlowBrowser = Join-Path $nyxFlowRoot 'web'
    if ($BrowserOutput) { $nyxFlowBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxFlowStable, $nyxFlowMatched,
      $nyxFlowNative, $nyxFlowDriver, $nyxFlowBrowser | Out-Null
    $nyxFlowFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxFlowCompiler in @(@($nyxFpc, $nyxFlowStable), @($nyxLclFpc, $nyxFlowMatched))) {
      Invoke-NyxCompiler $nyxFlowCompiler[0] ($nyxFlowFlags + @(
        "-FU$($nyxFlowCompiler[1])", "-FE$($nyxFlowCompiler[1])", 'tests/nyx_flow_tests.lpr'))
      & (Join-Path $nyxFlowCompiler[1] 'nyx_flow_tests.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Shared flow placement contract failed' }
    }
    $nyxFlowPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxFlowFlags + @("-Fu$nyxFlowSource",
      "-Fu$nyxLazarus/lcl/units/$nyxFlowPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxFlowPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxFlowPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxFlowPlatform",
      "-FU$nyxFlowNative", "-FE$nyxFlowNative", 'tests/nyx_flow_studio.lpr'))
    & (Join-Path $nyxFlowNative 'nyx_flow_studio.exe') (Join-Path $nyxFlowNative ('projects-' + [guid]::NewGuid().ToString('N'))) (Join-Path $nyxFlowSource 'nyx.generated.view.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native Studio flow placement failed' }
    foreach ($nyxFlowHostProgram in @('tests/nyx_flow_browser_review.lpr', 'tests/nyx_responsive_browser_review.lpr')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxFlowFlags + @("-FU$nyxFlowDriver", "-FE$nyxFlowDriver", $nyxFlowHostProgram))
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxFlowProgram in @('tests/nyx_flow_tests.lpr', 'tests/nyx_flow_studio_browser.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', '-Jirtl.js', "-FE$nyxFlowBrowser", $nyxFlowProgram)
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tmodule', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Jirtl.js', "-FE$nyxFlowBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxFlowBrowser 'rtl.js')
    foreach ($nyxFlowPage in @('flow-tests.html', 'flow-studio.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxFlowPage") -Destination $nyxFlowBrowser
    }
    Write-Host 'Flow placement consumers staged; run the host driver against the explicit semantic project.'
    exit 0
  }
  if ($Target -eq 'resize') {
    # Pascal owns gestures, semantic mutations, worker tickets and actual input.
    # Only stage products here; never launch a listener or refresh enrollment.
    $nyxResizeRoot = Join-Path $nyxRoot 'build/resize'
    $nyxResizeStable = Join-Path $nyxResizeRoot 'stable'
    $nyxResizeMatched = Join-Path $nyxResizeRoot 'matched'
    $nyxResizeExport = Join-Path $nyxResizeRoot 'export'
    $nyxResizeMatchedExport = Join-Path $nyxResizeRoot 'export-matched'
    $nyxResizeLcl = Join-Path $nyxResizeRoot 'lcl'
    $nyxResizeStudio = Join-Path $nyxResizeRoot 'studio'
    $nyxResizeBrowser = Join-Path $nyxResizeRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    if ($BrowserOutput) { $nyxResizeBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxResizeStable, $nyxResizeMatched,
      $nyxResizeExport, $nyxResizeMatchedExport, $nyxResizeLcl,
      $nyxResizeStudio, $nyxResizeBrowser | Out-Null
    $nyxResizeFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxResizeCompiler in @(@($nyxFpc, $nyxResizeStable, $nyxResizeExport),
      @($nyxLclFpc, $nyxResizeMatched, $nyxResizeMatchedExport))) {
      Invoke-NyxCompiler $nyxResizeCompiler[0] ($nyxResizeFlags + @(
        "-FU$($nyxResizeCompiler[1])", "-FE$($nyxResizeCompiler[1])",
        'tests/nyx_resize_tests.lpr'))
      & (Join-Path $nyxResizeCompiler[1] 'nyx_resize_tests.exe') $nyxResizeCompiler[2]
      if ($LASTEXITCODE -ne 0) { throw 'Shared resize checks failed' }
    }
    foreach ($nyxResizeFile in @('design.nyx', 'nyx.generated.view.pas', 'project.nyxpair')) {
      if ((Get-FileHash (Join-Path $nyxResizeExport $nyxResizeFile)).Hash -ne
        (Get-FileHash (Join-Path $nyxResizeMatchedExport $nyxResizeFile)).Hash) {
        throw "Resize exports differ between compilers: $nyxResizeFile"
      }
    }
    $nyxResizePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxResizeControlFlags = $nyxResizeFlags + @(
      "-Fu$nyxLazarus/lcl/units/$nyxResizePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxResizePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxResizePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxResizePlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxResizeControlFlags + @("-Fu$nyxResizeExport",
      "-FU$nyxResizeLcl", "-FE$nyxResizeLcl", 'tests/nyx_resize_controls.lpr'))
    & (Join-Path $nyxResizeLcl 'nyx_resize_controls.exe') (Join-Path $nyxResizeExport 'design.nyx')
    if ($LASTEXITCODE -ne 0) { throw 'Unchanged compiled resize controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxResizeControlFlags + @(
      "-FU$nyxResizeStudio", "-FE$nyxResizeStudio", 'tests/nyx_resize_studio.lpr'))
    & (Join-Path $nyxResizeStudio 'nyx_resize_studio.exe') $nyxResizeRoot
    if ($LASTEXITCODE -ne 0) { throw 'Actual Studio resize input failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxResizeProgram in @('tests/nyx_resize_tests.lpr',
      'tests/nyx_resize_controls.lpr', 'tests/nyx_resize_preview_browser.lpr',
      'tests/nyx_projection_refresh_tests.lpr',
      'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxResizeExport", '-Jirtl.js', "-FE$nyxResizeBrowser", $nyxResizeProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxResizeBrowser 'rtl.js')
    foreach ($nyxResizeHost in @('resize.html', 'resize-controls.html', 'resize-preview.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxResizeHost") -Destination $nyxResizeBrowser
    }
    Write-Host 'Resize consumers and Studio staged; browser execution needs its admitted host.'
    exit 0
  }

  if ($Target -eq 'constraints') {
    # Pascal owns copied policies, semantic/isolated admission and actual controls.
    # This orchestration stages artifacts only; no listener or live release is
    # replaced and no private MCP enrollment/configuration is refreshed.
    $nyxBoundsRoot = Join-Path $nyxRoot 'build/constraints'
    $nyxBoundsStable = Join-Path $nyxBoundsRoot 'stable'
    $nyxBoundsMatched = Join-Path $nyxBoundsRoot 'matched'
    $nyxBoundsExport = Join-Path $nyxBoundsRoot 'export'
    $nyxBoundsMatchedExport = Join-Path $nyxBoundsRoot 'export-matched'
    $nyxBoundsLcl = Join-Path $nyxBoundsRoot 'lcl'
    $nyxBoundsStudio = Join-Path $nyxBoundsRoot 'studio'
    $nyxBoundsBrowser = Join-Path $nyxBoundsRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''

    if ($BrowserOutput) { $nyxBoundsBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBoundsStable, $nyxBoundsMatched,
      $nyxBoundsExport, $nyxBoundsMatchedExport, $nyxBoundsLcl,
      $nyxBoundsStudio, $nyxBoundsBrowser | Out-Null
    $nyxBoundsFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxBoundsCompiler in @(@($nyxFpc, $nyxBoundsStable, $nyxBoundsExport),
      @($nyxLclFpc, $nyxBoundsMatched, $nyxBoundsMatchedExport))) {
      Invoke-NyxCompiler $nyxBoundsCompiler[0] ($nyxBoundsFlags + @(
        "-FU$($nyxBoundsCompiler[1])", "-FE$($nyxBoundsCompiler[1])",
        'tests/nyx_constraints_tests.lpr'))
      & (Join-Path $nyxBoundsCompiler[1] 'nyx_constraints_tests.exe') $nyxBoundsCompiler[2]

      if ($LASTEXITCODE -ne 0) { throw 'Shared size constraints failed' }
    }
    foreach ($nyxBoundsFile in @('design.nyx', 'nyx.generated.view.pas', 'project.nyxpair')) {
      if ((Get-FileHash (Join-Path $nyxBoundsExport $nyxBoundsFile)).Hash -ne
        (Get-FileHash (Join-Path $nyxBoundsMatchedExport $nyxBoundsFile)).Hash) {
        throw "Constraint exports differ between compilers: $nyxBoundsFile"
      }
    }
    $nyxBoundsPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxBoundsControlFlags = $nyxBoundsFlags + @(
      "-Fu$nyxLazarus/lcl/units/$nyxBoundsPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxBoundsPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxBoundsPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxBoundsPlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxBoundsControlFlags + @("-Fu$nyxBoundsExport",
      "-FU$nyxBoundsLcl", "-FE$nyxBoundsLcl", 'tests/nyx_constraints_controls.lpr'))
    & (Join-Path $nyxBoundsLcl 'nyx_constraints_controls.exe') (Join-Path $nyxBoundsExport 'design.nyx') (Join-Path $nyxBoundsExport 'constraints.png')

    if ($LASTEXITCODE -ne 0) { throw 'Actual compiled size constraints failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxBoundsControlFlags + @(
      "-FU$nyxBoundsStudio", "-FE$nyxBoundsStudio", 'tests/nyx_constraints_studio.lpr'))
    & (Join-Path $nyxBoundsStudio 'nyx_constraints_studio.exe') $nyxBoundsRoot

    if ($LASTEXITCODE -ne 0) { throw 'Actual Studio size authoring failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxBoundsProgram in @('tests/nyx_constraints_tests.lpr',
      'tests/nyx_constraints_controls.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxBoundsExport", '-Jirtl.js', "-FE$nyxBoundsBrowser", $nyxBoundsProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBoundsBrowser 'rtl.js')
    foreach ($nyxBoundsHost in @('constraints.html', 'constraints-controls.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxBoundsHost") -Destination $nyxBoundsBrowser
    }
    Write-Host 'Size constraints staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'designer-drag') {
    # Pascal owns lease guards, actual Studio input and unchanged compilation.
    # Outputs stay staged: this target launches no listener, changes no private
    # MCP configuration and never replaces the protected observing release.
    $nyxDragRoot = Join-Path $nyxRoot 'build/designer-drag'
    $nyxDragStable = Join-Path $nyxDragRoot 'guards'
    $nyxDragMatched = Join-Path $nyxDragRoot 'guards-matched'
    $nyxDragLcl = Join-Path $nyxDragRoot 'lcl'
    $nyxDragCompiled = Join-Path $nyxDragRoot 'compiled'
    $nyxDragExport = Join-Path $nyxDragRoot 'export'
    $nyxDragBrowser = Join-Path $nyxDragRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''

    if ($BrowserOutput) { $nyxDragBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxDragStable, $nyxDragMatched,
      $nyxDragLcl, $nyxDragCompiled, $nyxDragExport, $nyxDragBrowser | Out-Null
    $nyxDragFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxDragCompiler in @(@($nyxFpc, $nyxDragStable), @($nyxLclFpc, $nyxDragMatched))) {
      Invoke-NyxCompiler $nyxDragCompiler[0] ($nyxDragFlags + @(
        "-FU$($nyxDragCompiler[1])", "-FE$($nyxDragCompiler[1])",
        'tests/nyx_designer_drag_guards.lpr'))
      & (Join-Path $nyxDragCompiler[1] 'nyx_designer_drag_guards.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Designer drag guards failed' }
    }
    $nyxDragPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxDragControlFlags = $nyxDragFlags + @(
      "-Fu$nyxLazarus/lcl/units/$nyxDragPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxDragPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxDragPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxDragPlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxDragControlFlags + @(
      "-FU$nyxDragLcl", "-FE$nyxDragLcl", 'tests/nyx_designer_drag_controls.lpr'))
    & (Join-Path $nyxDragLcl 'nyx_designer_drag_controls.exe') $nyxDragExport

    if ($LASTEXITCODE -ne 0) { throw 'Actual native designer drag controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxDragControlFlags + @("-Fu$nyxDragExport",
      "-FU$nyxDragCompiled", "-FE$nyxDragCompiled", 'tests/nyx_designer_drag_compiled.lpr'))
    & (Join-Path $nyxDragCompiled 'nyx_designer_drag_compiled.exe') (Join-Path $nyxDragExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Unchanged compiled designer drag consumer failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxDragProgram in @('tests/nyx_designer_drag_guards.lpr',
      'tests/nyx_designer_drag_compiled.lpr', 'tests/nyx_designer_drag_browser.lpr',
      'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxDragExport", '-Jirtl.js', "-FE$nyxDragBrowser", $nyxDragProgram)
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tmodule', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Jirtl.js', "-FE$nyxDragBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxDragBrowser 'rtl.js')
    foreach ($nyxDragHost in @('designer-drag-guards.html', 'designer-drag-compiled.html',
      'designer-drag-studio.html', 'index.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxDragHost") -Destination $nyxDragBrowser
    }
    Write-Host 'Designer drag artifacts staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'placement') {
    # Pascal owns semantic/isolated admission and actual editor/control checks.
    # Staged artifacts never launch a listener, replace the observing release or
    # refresh a private MCP configuration. Shell code only invokes platform tools.
    $nyxPlacementRoot = Join-Path $nyxRoot 'build/placement'
    $nyxPlacementStable = Join-Path $nyxPlacementRoot 'stable'
    $nyxPlacementMatched = Join-Path $nyxPlacementRoot 'matched'
    $nyxPlacementLcl = Join-Path $nyxPlacementRoot 'lcl'
    $nyxPlacementExport = Join-Path $nyxPlacementRoot 'export-stable'
    $nyxPlacementMatchedExport = Join-Path $nyxPlacementRoot 'export-matched'
    $nyxPlacementBrowser = Join-Path $nyxPlacementRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''

    if ($BrowserOutput) { $nyxPlacementBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxPlacementStable, $nyxPlacementMatched,
      $nyxPlacementLcl, $nyxPlacementExport, $nyxPlacementMatchedExport,
      $nyxPlacementBrowser | Out-Null
    $nyxPlacementFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    foreach ($nyxPlacementCompiler in @(
      @($nyxFpc, $nyxPlacementStable, $nyxPlacementExport),
      @($nyxLclFpc, $nyxPlacementMatched, $nyxPlacementMatchedExport))) {
      Invoke-NyxCompiler $nyxPlacementCompiler[0] ($nyxPlacementFlags + @(
        "-FU$($nyxPlacementCompiler[1])", "-FE$($nyxPlacementCompiler[1])",
        'tests/nyx_placement_tests.lpr'))
      & (Join-Path $nyxPlacementCompiler[1] 'nyx_placement_tests.exe') $nyxPlacementCompiler[2]

      if ($LASTEXITCODE -ne 0) { throw 'Semantic placement qualification failed' }
    }
    foreach ($nyxPlacementArtifact in @('design.nyx', 'nyx.generated.view.pas', 'project.nyxpair')) {

      if ((Get-FileHash -LiteralPath (Join-Path $nyxPlacementExport $nyxPlacementArtifact)).Hash -cne
        (Get-FileHash -LiteralPath (Join-Path $nyxPlacementMatchedExport $nyxPlacementArtifact)).Hash) {
        throw "Placement compiler artifacts differ: $nyxPlacementArtifact"
      }
    }
    $nyxPlacementPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxPlacementControlFlags = $nyxPlacementFlags + @("-Fu$nyxPlacementExport",
      "-Fu$nyxLazarus/lcl/units/$nyxPlacementPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxPlacementPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxPlacementPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxPlacementPlatform",
      "-FU$nyxPlacementLcl", "-FE$nyxPlacementLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxPlacementControlFlags + @('tests/nyx_placement_controls.lpr'))
    & (Join-Path $nyxPlacementLcl 'nyx_placement_controls.exe') (Join-Path $nyxPlacementRoot 'input') (Join-Path $nyxPlacementExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Actual native placement qualification failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxPlacementControlFlags + @('tests/nyx_agent_reusable_schema.lpr'))
    & (Join-Path $nyxPlacementLcl 'nyx_agent_reusable_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual transaction discovery qualification failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxPlacementProgram in @('nyx_placement_tests', 'nyx_placement_browser')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxPlacementExport", '-Jirtl.js', "-FE$nyxPlacementBrowser",
        "tests/$nyxPlacementProgram.lpr")
    }
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
      '-Jirtl.js', "-FE$nyxPlacementBrowser", 'studio/nyx_studio.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Tmodule', '-Mdelphi', '-Fusrc', '-Fustudio',
      "-FE$nyxPlacementBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxPlacementBrowser 'rtl.js')
    foreach ($nyxPlacementHost in @('placement.html', 'placement-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxPlacementHost") -Destination $nyxPlacementBrowser
    }
    Write-Host 'Placement consumers staged; actual browser execution retains its host gate.'
    exit 0
  }

  if ($Target -eq 'reusables') {
    # Pascal owns command, history, source and physical-control assertions.
    # These independent artifacts never launch/deploy a listener or touch the
    # observing user's project, compiler profile or enrolled MCP configuration.
    $nyxReusableRoot = Join-Path $nyxRoot 'build/reusables'
    $nyxReusableStable = Join-Path $nyxReusableRoot 'stable'
    $nyxReusableMatched = Join-Path $nyxReusableRoot 'matched'
    $nyxReusableLcl = Join-Path $nyxReusableRoot 'lcl'
    $nyxReusableExport = Join-Path $nyxReusableRoot 'export-stable'
    $nyxReusableMatchedExport = Join-Path $nyxReusableRoot 'export-matched'
    $nyxReusableBrowser = Join-Path $nyxReusableRoot 'browser'

    if ($BrowserOutput) { $nyxReusableBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxReusableStable, $nyxReusableMatched,
      $nyxReusableLcl, $nyxReusableExport, $nyxReusableMatchedExport,
      $nyxReusableBrowser | Out-Null
    $nyxReusableFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    Invoke-NyxCompiler $nyxFpc ($nyxReusableFlags + @("-FU$nyxReusableStable",
      "-FE$nyxReusableStable", 'tests/nyx_agent_reusable_tests.lpr'))
    & (Join-Path $nyxReusableStable 'nyx_agent_reusable_tests.exe') $nyxReusableExport

    if ($LASTEXITCODE -ne 0) { throw 'Stable semantic reusable workflow failed' }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    Invoke-NyxCompiler $nyxLclFpc ($nyxReusableFlags + @("-FU$nyxReusableMatched",
      "-FE$nyxReusableMatched", 'tests/nyx_agent_reusable_tests.lpr'))
    & (Join-Path $nyxReusableMatched 'nyx_agent_reusable_tests.exe') $nyxReusableMatchedExport

    if ($LASTEXITCODE -ne 0) { throw 'Matched semantic reusable workflow failed' }
    foreach ($nyxReusableArtifact in @('design.nyx', 'nyx.generated.view.pas', 'project.nyxpair')) {

      if ((Get-FileHash -LiteralPath (Join-Path $nyxReusableExport $nyxReusableArtifact)).Hash -cne
        (Get-FileHash -LiteralPath (Join-Path $nyxReusableMatchedExport $nyxReusableArtifact)).Hash) {
        throw "Reusable compiler artifacts differ: $nyxReusableArtifact"
      }
    }
    $nyxReusablePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxReusableControlFlags = $nyxReusableFlags + @("-Fu$nyxReusableExport",
      "-Fu$nyxLazarus/lcl/units/$nyxReusablePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxReusablePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxReusablePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxReusablePlatform",
      "-FU$nyxReusableLcl", "-FE$nyxReusableLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxReusableControlFlags + @('tests/nyx_agent_reusable_schema.lpr'))
    & (Join-Path $nyxReusableLcl 'nyx_agent_reusable_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual reusable discovery checks failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxReusableControlFlags + @('tests/nyx_agent_reusable_controls.lpr'))
    & (Join-Path $nyxReusableLcl 'nyx_agent_reusable_controls.exe') (Join-Path $nyxReusableExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Unchanged compiled reusable controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxReusableProgram in @('nyx_agent_reusable_tests', 'nyx_agent_reusable_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio',
        '-Futests', "-Fu$nyxReusableExport", '-Jirtl.js', "-FE$nyxReusableBrowser",
        "tests/$nyxReusableProgram.lpr")
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxReusableBrowser 'rtl.js')
    foreach ($nyxReusableHost in @('agent-reusables.html', 'agent-reusable-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxReusableHost") -Destination $nyxReusableBrowser
    }
    Write-Host 'Reusable consumers staged; actual browser execution requires an admitted HTTP host.'
    exit 0
  }


  if ($Target -eq 'source-editor') {
    # Pascal qualifies retained physical controls, modal resizing and strict
    # per-project presentation migration. This builds ordinary Studio binaries
    # without starting a listener or refreshing private MCP configuration.
    $nyxSourceEditorRoot = Join-Path $nyxRoot 'build/source-editor'
    $nyxSourceEditorNative = Join-Path $nyxSourceEditorRoot 'native'
    $nyxSourceEditorLcl = Join-Path $nyxSourceEditorRoot 'lcl'
    $nyxSourceEditorBrowser = Join-Path $nyxSourceEditorRoot 'browser'

    if ($BrowserOutput) { $nyxSourceEditorBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxSourceEditorNative,
      $nyxSourceEditorLcl, $nyxSourceEditorBrowser | Out-Null
    $nyxSourceEditorFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxSourceEditorNative", "-FE$nyxSourceEditorNative")
    Invoke-NyxCompiler $nyxFpc ($nyxSourceEditorFlags + @('tests/nyx_workspace_tests.lpr'))
    & (Join-Path $nyxSourceEditorNative 'nyx_workspace_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Native per-project presentation qualification failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxSourceEditorPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxSourceEditorLclFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxSourceEditorPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxSourceEditorPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxSourceEditorPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxSourceEditorPlatform",
      "-FU$nyxSourceEditorLcl", "-FE$nyxSourceEditorLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxSourceEditorLclFlags + @('tests/nyx_workspace_tests.lpr'))
    & (Join-Path $nyxSourceEditorLcl 'nyx_workspace_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Matched per-project presentation qualification failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxSourceEditorLclFlags + @('tests/nyx_source_workspace_controls.lpr'))
    & (Join-Path $nyxSourceEditorLcl 'nyx_source_workspace_controls.exe') $nyxSourceEditorLcl

    if ($LASTEXITCODE -ne 0) { throw 'Actual native source workspace qualification failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxSourceEditorLclFlags + @('studio/nyx_studio_native.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxSourceEditorProgram in @('tests/nyx_workspace_tests.lpr',
        'tests/nyx_source_workspace_browser.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-FE$nyxSourceEditorBrowser", $nyxSourceEditorProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxSourceEditorBrowser 'rtl.js')
    foreach ($nyxSourceEditorHost in @('index.html', 'workspaces.html', 'source-editor.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxSourceEditorHost") -Destination $nyxSourceEditorBrowser
    }
    Write-Host 'Source editor consumers staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'pascal-imports') {
    # Pascal owns lexical/semantic/refusal assertions and the exact companion.
    # This target starts no listener and touches no observing project/config.
    $nyxImportRoot = Join-Path $nyxRoot 'build/pascal-imports'
    $nyxImportNative = Join-Path $nyxImportRoot 'native'
    $nyxImportExport = Join-Path $nyxImportRoot 'export'
    $nyxImportLcl = Join-Path $nyxImportRoot 'lcl'
    $nyxImportBrowser = Join-Path $nyxImportRoot 'browser'

    if ($BrowserOutput) { $nyxImportBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxImportNative, $nyxImportExport,
      $nyxImportLcl, $nyxImportBrowser | Out-Null
    $nyxImportFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxImportNative", "-FE$nyxImportNative")
    foreach ($nyxImportProgram in @('nyx_import_lexical_tests', 'nyx_import_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxImportFlags + @("tests/$nyxImportProgram.lpr"))
      & (Join-Path $nyxImportNative "$nyxImportProgram.exe") $nyxImportExport

      if ($LASTEXITCODE -ne 0) { throw 'Pascal import qualification failed' }
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxImportPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxImportControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImportExport",
      "-Fu$nyxLazarus/lcl/units/$nyxImportPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxImportPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxImportPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxImportPlatform",
      "-FU$nyxImportLcl", "-FE$nyxImportLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxImportControlFlags + @('tests/nyx_import_schema.lpr'))
    & (Join-Path $nyxImportLcl 'nyx_import_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual Pascal import discovery failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxImportControlFlags + @('tests/nyx_import_controls.lpr'))
    & (Join-Path $nyxImportLcl 'nyx_import_controls.exe') (Join-Path $nyxImportExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled import controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxImportProgram in @('nyx_import_lexical_tests', 'nyx_import_tests', 'nyx_import_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-Fu$nyxImportExport", "-FE$nyxImportBrowser", "tests/$nyxImportProgram.lpr")
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxImportBrowser 'rtl.js')
    foreach ($nyxImportHost in @('import-lexical.html', 'import-edits.html', 'import-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxImportHost") -Destination $nyxImportBrowser
    }
    Write-Host 'Pascal import consumers staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'pascal-routines') {
    # Pascal owns lexical/semantic/refusal assertions and the exact companion.
    # This target starts no listener and touches no observing project/config.
    $nyxRoutineRoot = Join-Path $nyxRoot 'build/pascal-routines'
    $nyxRoutineNative = Join-Path $nyxRoutineRoot 'native'
    $nyxRoutineExport = Join-Path $nyxRoutineRoot 'export'
    $nyxRoutineLcl = Join-Path $nyxRoutineRoot 'lcl'
    $nyxRoutineBrowser = Join-Path $nyxRoutineRoot 'browser'

    if ($BrowserOutput) { $nyxRoutineBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxRoutineNative, $nyxRoutineExport,
      $nyxRoutineLcl, $nyxRoutineBrowser | Out-Null
    $nyxRoutineFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxRoutineNative", "-FE$nyxRoutineNative")
    foreach ($nyxRoutineProgram in @('nyx_routine_lexical_tests', 'nyx_routine_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxRoutineFlags + @("tests/$nyxRoutineProgram.lpr"))
      & (Join-Path $nyxRoutineNative "$nyxRoutineProgram.exe") $nyxRoutineExport

      if ($LASTEXITCODE -ne 0) { throw 'Pascal routine qualification failed' }
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxRoutinePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxRoutineControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxRoutineExport",
      "-Fu$nyxLazarus/lcl/units/$nyxRoutinePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxRoutinePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxRoutinePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxRoutinePlatform",
      "-FU$nyxRoutineLcl", "-FE$nyxRoutineLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxRoutineControlFlags + @('tests/nyx_routine_schema.lpr'))
    & (Join-Path $nyxRoutineLcl 'nyx_routine_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual Pascal routine discovery failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxRoutineControlFlags + @('tests/nyx_routine_controls.lpr'))
    & (Join-Path $nyxRoutineLcl 'nyx_routine_controls.exe') (Join-Path $nyxRoutineExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled routine controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxRoutineProgram in @('nyx_routine_lexical_tests', 'nyx_routine_tests', 'nyx_routine_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-Fu$nyxRoutineExport", "-FE$nyxRoutineBrowser", "tests/$nyxRoutineProgram.lpr")
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxRoutineBrowser 'rtl.js')
    foreach ($nyxRoutineHost in @('routine-lexical.html', 'routine-edits.html', 'routine-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxRoutineHost") -Destination $nyxRoutineBrowser
    }
    Write-Host 'Pascal routine consumers staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'pascal-declarations') {
    # Pascal owns lexical/semantic/refusal assertions and the exact companion.
    # This target starts no listener and touches no observing project/config.
    $nyxDeclarationRoot = Join-Path $nyxRoot 'build/pascal-declarations'
    $nyxDeclarationNative = Join-Path $nyxDeclarationRoot 'native'
    $nyxDeclarationExport = Join-Path $nyxDeclarationRoot 'export'
    $nyxDeclarationLcl = Join-Path $nyxDeclarationRoot 'lcl'
    $nyxDeclarationBrowser = Join-Path $nyxDeclarationRoot 'browser'

    if ($BrowserOutput) { $nyxDeclarationBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxDeclarationNative, $nyxDeclarationExport,
      $nyxDeclarationLcl, $nyxDeclarationBrowser | Out-Null
    $nyxDeclarationFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxDeclarationNative", "-FE$nyxDeclarationNative")
    foreach ($nyxDeclarationProgram in @('nyx_declaration_lexical_tests', 'nyx_declaration_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxDeclarationFlags + @("tests/$nyxDeclarationProgram.lpr"))
      & (Join-Path $nyxDeclarationNative "$nyxDeclarationProgram.exe") $nyxDeclarationExport

      if ($LASTEXITCODE -ne 0) { throw 'Pascal declaration qualification failed' }
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxDeclarationPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxDeclarationControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxDeclarationExport",
      "-Fu$nyxLazarus/lcl/units/$nyxDeclarationPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxDeclarationPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxDeclarationPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxDeclarationPlatform",
      "-FU$nyxDeclarationLcl", "-FE$nyxDeclarationLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxDeclarationControlFlags + @('tests/nyx_declaration_schema.lpr'))
    & (Join-Path $nyxDeclarationLcl 'nyx_declaration_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Actual Pascal declaration discovery failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxDeclarationControlFlags + @('tests/nyx_declaration_controls.lpr'))
    & (Join-Path $nyxDeclarationLcl 'nyx_declaration_controls.exe') (Join-Path $nyxDeclarationExport 'design.nyx')

    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled declaration controls failed' }
    # The exact new public signature compiles above. Diagnose a separately
    # maintained old caller with both native compilers; tool/unit failures must
    # never count as the intended parameter-contract rejection.
    foreach ($nyxDeclarationDiagnosticCompiler in @($nyxFpc, $nyxLclFpc)) {
      $nyxDeclarationDiagnostic = & $nyxDeclarationDiagnosticCompiler @nyxDeclarationFlags "-Fu$nyxDeclarationExport" 'tests/compile_fail/nyx_outdated_helper_call.lpr' 2>&1
      $nyxDeclarationDiagnosticExit = $LASTEXITCODE
      $nyxDeclarationDiagnosticText = $nyxDeclarationDiagnostic -join [Environment]::NewLine

      if ($nyxDeclarationDiagnosticExit -eq 0 -or
        $nyxDeclarationDiagnosticText -notmatch 'Error:.*(number of parameters|arguments|Incompatible types).*EnglishCaption') {
        Write-Host $nyxDeclarationDiagnosticText
        throw 'Expected native stale-caller signature diagnostic was not established'
      }
      Write-Host 'PASS native compiler diagnoses the outdated public helper caller'
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxDeclarationProgram in @('nyx_declaration_lexical_tests', 'nyx_declaration_tests', 'nyx_declaration_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-Fu$nyxDeclarationExport", "-FE$nyxDeclarationBrowser", "tests/$nyxDeclarationProgram.lpr")
    }
    $nyxDeclarationDiagnostic = & $nyxPas2js '-B' '-Tbrowser' '-Mdelphi' '-Fusrc' '-Fustudio' "-Fu$nyxDeclarationExport" "-FE$nyxDeclarationBrowser" 'tests/compile_fail/nyx_outdated_helper_call.lpr' 2>&1
    $nyxDeclarationDiagnosticExit = $LASTEXITCODE
    $nyxDeclarationDiagnosticText = $nyxDeclarationDiagnostic -join [Environment]::NewLine

    if ($nyxDeclarationDiagnosticExit -eq 0 -or
      $nyxDeclarationDiagnosticText -notmatch 'Error:.*(number of parameters|arguments|Incompatible types).*EnglishCaption') {
      Write-Host $nyxDeclarationDiagnosticText
      throw 'Expected browser stale-caller signature diagnostic was not established'
    }
    Write-Host 'PASS pas2js diagnoses the outdated public helper caller'
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxDeclarationBrowser 'rtl.js')
    foreach ($nyxDeclarationHost in @('declaration-lexical.html', 'declaration-edits.html', 'declaration-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxDeclarationHost") -Destination $nyxDeclarationBrowser
    }
    Write-Host 'Pascal declaration consumers staged; browser execution requires an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'agents') {
    # Pascal owns protocol, atomic-edit and observer assertions. HTTP consumers
    # are compiled here and run explicitly against the selected live service;
    # the portable fixture needs neither a browser nor a running MCP endpoint.
    foreach ($nyxAgentProgram in @('nyx_agent_tests', 'nyx_agent_callback_tests', 'nyx_handler_edit_tests', 'nyx_root_tests', 'nyx_agent_build_tests', 'nyx_build_compiler_fixture', 'nyx_build_job_tests', 'nyx_mcp_http_tests', 'nyx_mcp_observer_tests', 'nyx_mcp_diagnostic_tests', 'nyx_browser_capture')) {
      Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @("tests/$nyxAgentProgram.lpr"))
    }
    & (Join-Path $nyxNativeDir 'nyx_agent_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Semantic agent model checks failed'
    }
    & (Join-Path $nyxNativeDir 'nyx_agent_callback_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Semantic callback model checks failed'
    }
    & (Join-Path $nyxNativeDir 'nyx_handler_edit_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Semantic Pascal implementation checks failed'
    }
    & (Join-Path $nyxNativeDir 'nyx_root_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Reviewed root removal checks failed'
    }
    & (Join-Path $nyxNativeDir 'nyx_agent_build_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Semantic build admission checks failed'
    }
    # The maintained observing-editor journey uses the shared Pascal CDP owner.
    # Its FPC WebSocket units come from the matched Lazarus toolchain.
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxAgentHostUnits = Join-Path $nyxRoot 'build/agent-host-units'
    New-Item -ItemType Directory -Force $nyxAgentHostUnits | Out-Null
    foreach ($nyxAgentDriver in @('nyx_mcp_callback_tests', 'nyx_mcp_build_tests', 'nyx_mcp_handler_tests', 'nyx_mcp_root_tests')) {
      Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
        '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxAgentHostUnits", "-FE$nyxNativeDir", "tests/$nyxAgentDriver.lpr")
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxResourceRoot = Join-Path $nyxRoot 'build/agent-build-jobs'
    & (Join-Path $nyxNativeDir 'nyx_build_job_tests.exe') $nyxResourceRoot (Join-Path $nyxNativeDir 'nyx_build_compiler_fixture.exe') $nyxRuntime

    if ($LASTEXITCODE -ne 0) {
      throw 'Compiler job resource checks failed'
    }
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) {
      $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput)
    }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    $nyxAgentFlags = @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxBrowserDir")
    foreach ($nyxAgentProgram in @('tests/nyx_agent_tests.lpr', 'tests/nyx_agent_callback_tests.lpr', 'tests/nyx_handler_edit_tests.lpr', 'tests/nyx_handler_observer_tests.lpr', 'tests/nyx_root_tests.lpr', 'tests/nyx_root_observer_tests.lpr', 'tests/nyx_agent_build_tests.lpr', 'tests/nyx_agent_build_observer_tests.lpr', 'tests/nyx_agent_callback_observer_tests.lpr', 'tests/nyx_agent_observer_tests.lpr', 'tests/nyx_agent_bridge_tests.lpr',
      'tests/nyx_studio_compiler_tests.lpr', 'studio/nyx_studio_preview.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js ($nyxAgentFlags + @($nyxAgentProgram))
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxAgentHost in @('index.html', 'agents.html', 'agent-callbacks.html', 'handler-edits.html', 'handler-observer.html', 'roots.html', 'root-observer.html', 'agent-builds.html', 'agent-build-observer.html', 'agent-callback-observer.html', 'agent-observer.html', 'agent-preview.html', 'agent-bridge.html', 'studio-compiler.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxAgentHost") -Destination $nyxBrowserDir
    }
    exit 0
  }

  if ($Target -eq 'source-workspace') {
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('-gh', 'tests/nyx_source_diagnostic_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_source_diagnostic_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Source diagnostic admission failed'
    }
    # Timing is a separate, explicitly selected run; compile the same public
    # fixture for both targets without hiding a slow large case in verification.
    # Heap tracing remains on functional checks, outside ordinary timing runs.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tools/nyx_source_benchmark.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tools/nyx_property_benchmark.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) {
      $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput)
    }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    $nyxSourceFlags = @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxBrowserDir")
    Invoke-NyxCompiler $nyxPas2js ($nyxSourceFlags + @('tests/nyx_source_diagnostic_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxSourceFlags + @('tools/nyx_source_benchmark.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxSourceFlags + @('tools/nyx_property_benchmark.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxSourceHost in @('source-diagnostics.html', 'source-benchmark.html', 'property-benchmark.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxSourceHost") -Destination $nyxBrowserDir
    }
    exit 0
  }

  if ($Target -eq 'collection-authoring') {
    # Pascal fixtures own schema/source/interaction assertions. This shell only
    # stages compiler outputs and runs consumers in a strict serial pipeline.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_collection_unicode_binding_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_collection_unicode_binding_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Unicode collection binding boundaries failed'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLclVersion = (& $nyxLclFpc '-iV').Trim()
    $nyxLclPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxAuthoringDir = Join-Path $nyxRoot "build/collection-authoring/$nyxLclVersion/$nyxLclPlatform"
    New-Item -ItemType Directory -Force $nyxAuthoringDir | Out-Null
    $nyxAuthoringFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxLclPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxLclPlatform",
      "-FU$nyxAuthoringDir", "-FE$nyxAuthoringDir")
    Invoke-NyxCompiler $nyxLclFpc ($nyxAuthoringFlags + @('tests/nyx_collection_authoring_tests.lpr'))
    & (Join-Path $nyxAuthoringDir 'nyx_collection_authoring_tests.exe') $nyxNativeDir

    if ($LASTEXITCODE -ne 0) {
      throw 'Collection authoring/control fixtures failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @("-Fu$nyxNativeDir",
      'tests/nyx_collection_authoring_generated_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_collection_authoring_generated_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Compiled collection authoring fixtures failed'
    }
    Test-NyxCompilerTypes $nyxFpc $nyxNativeFlags
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) {
      $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput)
    }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    $nyxBrowserFlags = @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxBrowserDir")
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_collection_authoring_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_collection_unicode_binding_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @("-Fu$nyxNativeDir",
      'tests/nyx_collection_authoring_generated_tests.lpr'))
    Test-NyxCompilerTypes $nyxPas2js $nyxBrowserFlags
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxHost in @('collection-authoring.html', 'collection-authoring-generated.html', 'collection-unicode.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxHost") -Destination $nyxBrowserDir
    }
    # The final compiler intentionally failed in the rejection matrix. Return
    # the pipeline's admitted result rather than that expected child exit code.
    exit 0
  }

  if ($Target -in @('collections', 'collection-views')) {
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_collection_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_collection_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Typed collection fixtures failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_collection_view_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_collection_view_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Typed collection view fixtures failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tools/nyx_collection_benchmark.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_collection_benchmark.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Collection runtime measurement refused incorrect results'
    }
  }

  if ($Target -in @('core', 'generated', 'all')) {
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_core_tests.lpr'))
    Test-NyxCompilerTypes $nyxFpc $nyxNativeFlags
    & (Join-Path $nyxNativeDir 'nyx_core_tests.exe') $nyxNativeDir

    if ($LASTEXITCODE -ne 0) {
      throw 'Portable core fixtures failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_scheduler_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_scheduler_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Scheduler and multiple-registration fixtures failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_project_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_project_tests.exe') $nyxNativeDir

    if ($LASTEXITCODE -ne 0) {
      throw 'Paired project admission and disk recovery fixtures failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @("-Fu$nyxNativeDir",
      'tests/nyx_generated_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_generated_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Generated Pascal round-trip failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @("-Fu$nyxNativeDir",
      'tests/nyx_collection_generated_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_collection_generated_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Compiled collection reconstruction failed'
    }
  }

  if ($Target -eq 'catalog') {
    # Metadata/reference logic belongs to Pascal. Emit under ignored build/ so
    # regenerating an artifact never silently rewrites reviewed documentation.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tools/nyx_catalog_reference.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_catalog_reference.exe') (Join-Path $nyxRoot 'build/catalog-reference.md')

    if ($LASTEXITCODE -ne 0) {
      throw 'Catalog reference generation failed'
    }
  }

  if ($Target -eq 'http') {
    # The foreground Studio service must already be running. This Pascal harness
    # exercises optional/late output profiles and delegated compiler paths over HTTP.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_http_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_http_tests.exe') $HttpURL

    if ($LASTEXITCODE -ne 0) {
      throw 'HTTP compiler-service journey failed'
    }
  }

  if ($Target -in @('browser', 'studio', 'generated', 'collections', 'collection-views', 'all')) {
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxRoot 'build/browser'

    if ($BrowserOutput) {
      $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput)
    }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Write-Host "pas2js: $nyxPas2js / $(& $nyxPas2js '-iV')"
    $nyxBrowserFlags = @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
      "-FE$nyxBrowserDir")

    if ($Target -in @('collections', 'collection-views')) {
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_collection_tests.lpr'))
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_collection_view_tests.lpr'))
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tools/nyx_collection_benchmark.lpr'))
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tools/nyx_collection_view_benchmark.lpr'))
      Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/collections.html') -Destination $nyxBrowserDir
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/collection-benchmark.html') -Destination $nyxBrowserDir
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/collection-views.html') -Destination $nyxBrowserDir
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/collection-view-benchmark.html') -Destination $nyxBrowserDir
      # The focused target publishes only its Pascal consumers; the ordinary
      # browser/generated target below retains the complete application checks.
      if ($Target -eq 'collections') {
        return
      }
    }
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_browser_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_collection_view_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_collection_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_controls_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_scheduler_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_callback_tests.lpr'))
    Test-NyxCompilerTypes $nyxPas2js $nyxBrowserFlags
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_browser_journey.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_binding_browser_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_browser_visual.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_studio_layout_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_studio_authoring_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_studio_palette_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_project_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_studio_project_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('studio/nyx_studio.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('studio/nyx_studio_preview.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_agent_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_agent_observer_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_agent_bridge_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @('tests/nyx_studio_compiler_tests.lpr'))

    if ($Target -in @('generated', 'all')) {
      # Execute native-generated source on both runtimes. This target creates its
      # own fixture through Pascal; ordinary browser builds need no native fixture.
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @("-Fu$nyxNativeDir",
        'tests/nyx_generated_browser_tests.lpr'))
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @("-Fu$nyxNativeDir",
        'tests/nyx_callback_controls_tests.lpr'))
      Invoke-NyxCompiler $nyxPas2js ($nyxBrowserFlags + @("-Fu$nyxNativeDir",
        'tests/nyx_collection_generated_tests.lpr'))
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/generated.html') -Destination $nyxBrowserDir
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/callback-controls.html') -Destination $nyxBrowserDir
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/collection-generated.html') -Destination $nyxBrowserDir
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxHost in @('index.html', 'tests.html', 'collections.html', 'collection-views.html', 'controls.html', 'scheduler.html', 'callbacks.html', 'projects.html', 'studio-projects.html', 'journey.html', 'bindings.html', 'authoring.html', 'palette.html', 'visual.html', 'studio-layout.html', 'agents.html', 'agent-observer.html', 'agent-preview.html', 'agent-bridge.html', 'studio-compiler.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxHost") -Destination $nyxBrowserDir
    }
  }

  if ($Target -in @('studio', 'all')) {
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('studio/nyx_studio_server.lpr'))
    Write-Host "Studio server: $(Join-Path $nyxNativeDir 'nyx_studio_server.exe')"
  }

  if ($Target -in @('lcl', 'visual', 'collection-views', 'all')) {
    # Emit the callback companion through the Pascal authoring harness. A native
    # control journey can run independently of a previous core/generated build.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_callback_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_callback_tests.exe') $nyxNativeDir

    if ($LASTEXITCODE -ne 0) {
      throw 'Callback companion preparation failed'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLclVersion = (& $nyxLclFpc '-iV').Trim()
    $nyxLclPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxLclDir = Join-Path $nyxRoot "build/lcl/$nyxLclVersion/$nyxLclPlatform"
    New-Item -ItemType Directory -Force $nyxLclDir | Out-Null
    $nyxLclFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxLclPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxLclPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxLclPlatform",
      "-FU$nyxLclDir", "-FE$nyxLclDir")
    if ($Target -eq 'collection-views') {
      Invoke-NyxCompiler $nyxLclFpc ($nyxLclFlags + @('tests/nyx_collection_controls_tests.lpr'))
      & (Join-Path $nyxLclDir 'nyx_collection_controls_tests.exe')

      if ($LASTEXITCODE -ne 0) {
        throw 'Native collection control journey failed'
      }
      Invoke-NyxCompiler $nyxLclFpc ($nyxLclFlags + @('tools/nyx_collection_view_benchmark.lpr'))
      $nyxMountedMeasurements = & (Join-Path $nyxLclDir 'nyx_collection_view_benchmark.exe')

      if ($LASTEXITCODE -ne 0) {
        throw 'Mounted collection measurement refused incorrect results'
      }
      $nyxMountedMeasurements | Set-Content -Encoding utf8 (Join-Path $nyxRoot 'build/collections-view-benchmark-native.csv')
      Write-Output $nyxMountedMeasurements
      return
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxLclFlags + @('tests/nyx_lcl_tests.lpr'))
    & (Join-Path $nyxLclDir 'nyx_lcl_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'LCL control journey failed'
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxLclFlags + @("-Fu$nyxNativeDir",
      'tests/nyx_callback_controls_tests.lpr'))
    & (Join-Path $nyxLclDir 'nyx_callback_controls_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Compiled native callback controls failed'
    }

    if ($Target -eq 'visual') {
      Invoke-NyxCompiler $nyxLclFpc ($nyxLclFlags + @('tests/nyx_lcl_visual.lpr'))
      & (Join-Path $nyxLclDir 'nyx_lcl_visual.exe') $nyxLclDir

      if ($LASTEXITCODE -ne 0) {
        throw 'Native visual capture failed'
      }
    }
  }
} finally {
  Pop-Location
}
