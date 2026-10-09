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
[ValidateSet('core', 'generated', 'collections', 'collection-views', 'collection-authoring', 'collection-inspectors', 'collection-bindings', 'collection-refresh', 'collection-query', 'collection-query-editor', 'collection-query-workflow', 'project-transactions', 'data-read', 'reusables', 'placement', 'designer-drag', 'constraints', 'resize', 'guides', 'move-snapping', 'flow-placement', 'containers', 'view-sections', 'studio-section-recovery', 'native-measurement', 'retained-arrangement', 'content-recipes', 'content-editor', 'content-revisions', 'responsive', 'presentations', 'manual-presentations', 'selection', 'tree-hierarchy', 'slider-fields', 'host-space', 'typeahead', 'typeahead-policy', 'typeahead-workflow', 'grid-navigation', 'menu', 'menu-bar', 'menu-bar-authoring', 'menu-bar-editor', 'menu-bar-workflow', 'menu-companion', 'menu-authoring', 'menu-editor', 'popover', 'popover-companion', 'confirmation', 'resource-images', 'resource-image-authoring', 'resource-workbench', 'image-presentation', 'image-authoring', 'resources', 'resource-loading', 'resource-authoring', 'resource-workflow', 'resource-runtime', 'application-resources', 'resource-publication', 'resource-mappings', 'theme-authoring', 'color-fields', 'time-values', 'time-fields', 'time-policy', 'clock-review', 'date-fields', 'date-policy', 'legacy-snapshot', 'release-observer', 'browser-worker', 'scheduler-pool', 'native-form', 'keyboard', 'catalog-focus', 'properties', 'layout', 'layout-policy', 'designer-controls', 'native-studio', 'semantic-events', 'source-workspace', 'source-editor', 'pascal-views', 'pascal-imports', 'pascal-routines', 'pascal-declarations', 'agents', 'compiled-preview-lifetime', 'compiler-lifecycle', 'state-bindings', 'state-inspectors', 'event-inspectors', 'agent-callback-consumers', 'agent-handler-consumers', 'agent-root-consumers', 'review-workspaces', 'review-consumers', 'project-workspaces', 'mcp-client', 'studio-release', 'split', 'interactions', 'named-events', 'viewport', 'editing', 'gestures', 'catalog', 'browser', 'studio', 'lcl', 'http', 'visual', 'all')]
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
  # Optional authenticated clock authoring qualification on an existing service.
  # The Pascal owner refuses existing output directories and uses only a review.
  # Empty compiles the Windows browser-pipe author; no connection/server starts.
  [string]$ClockReviewMCPConfig,
  [string]$ClockReviewDirectory = 'build/clock-review/review',
  # Explicit qualification against a freshly owned runtime started by the
  # Pascal test server. Empty only compiles tools/wrappers; it starts no server.
  [string]$ResourceRuntimeHome,
  # A caller-owned static child on an already running HTTP host. Empty only
  # compiles the resource/image consumer; this target starts no listener.
  [string]$ResourceImageStage,
  # Explicit semantic execution qualification in an independently owned runtime.
  # It requests exact successful jobs through MCP; no operator Run substitutes.
  [switch]$VerifySemanticLaunch,
  # Stage browser artifacts independently while an older LAN instance is live.
  [string]$BrowserOutput,
  # Release preparation creates a NEW frozen compiler-source/artifact bundle.
  # This output is never a running service root; existing destinations refuse.
  [string]$ReleaseOutput = 'build/studio-release/package',
  # Explicit installed-root qualification consumes a pristine previously staged
  # release plus the unchanged MCP/Studio-authored recipe companion. No listener.
  [switch]$VerifyReleaseRuntime,
  [string]$ReleaseRuntimeProfile,
  [string]$ReleaseRuntimeSourceDirectory = 'build/content-editor/maintained/result',
  # The keyboard review source is exported through MCP, never handwritten by
  # this orchestration script. Its generated unit must live in this directory.
  [string]$KeyboardSourceDirectory = 'build/keyboard/mcp',
  # Exact collection review source exported through bounded Nyx MCP queries.
  [string]$TypeAheadSourceDirectory = 'build/typeahead/source',
  # Optional exact English tree companion exported through one semantic MCP
  # transaction. Empty builds the public offline fixture; no server is launched.
  [string]$TreeSourceDirectory,
  # Typed local numeric-policy candidate enriched from the semantic seed.
  [string]$SliderSourceDirectory,
  # Same accepted confirmation template on both targets, exported through MCP.
  [string]$ConfirmationSourceDirectory = 'build/confirmation/source',
  # Exact English content is composed/exported by the persistent Pascal MCP tool.
  [string]$PopoverSourceDirectory = 'build/popover/source',
  # Exact English menu companion, composed through the persistent semantic client.
  [string]$MenuSourceDirectory = 'build/menu/source',
  # Empty uses the public portable fixture. An explicit directory consumes its
  # exact MCP-exported nyx.generated.view and also exercises ordinary Studio.
  [string]$MenuAuthoringSourceDirectory,
  # Exact semantic date companion. Typed bounds/state enrichment is explicitly
  # performed by the public Pascal fixture, never handwritten editor mutations.
  [string]$DateSourceDirectory = 'build/date-fields/companion',
  # Unchanged English date seed exported with bounded semantic MCP reads.
  [string]$DatePolicySourceDirectory = 'build/date-policy/source',
  # Exact public-Pascal clock companion produced by the prerequisite build.
  # This form/semantic consumer does not re-run its foundation or edit Studio.
  [string]$TimePolicySourceDirectory = 'build/time-fields/maintained/source',
  # Exact English review companion exported through bounded authenticated MCP.
  # Current typed RGB policies are explicitly enriched by the Pascal fixture.
  [string]$ColorSourceDirectory = 'build/color-fields/seed',
  # Exact English image workshop exported from an owned semantic MCP review.
  # Preserve the existing seed by default; qualification can select a fresh one.
  [string]$ImageSourceDirectory = 'build/image-presentation/seed',
  # Exact paired Resource workbench companion exported through an owned MCP review.
  [string]$ResourceSourceDirectory = 'build/resource-workbench/seed',
  # Opt-in native timing with the same semantic companion. Open isolates the
  # first Resources presentation; full retains the complete authoring journey.
  [ValidateSet('none', 'open', 'full')]
  [string]$ResourceWorkbenchProfile = 'none',
  # Exact English bound-table source composed/exported through authenticated MCP.
  [string]$GridSourceDirectory = 'build/grid-navigation/source',
  # Full-catalog source is composed/exported by the Pascal semantic MCP consumer.
  [string]$CatalogFocusSourceDirectory = 'build/catalog-focus/source',
  # Property mutations consume an unchanged MCP-authored catalog/review pair.
  [string]$PropertySourceDirectory = 'build/property-concordance/source',
  # Optional explicit enrollment composes/compiles/retires an owned review on
  # the existing service. Its source directory must be fresh; no server starts.
  [string]$PropertyMCPConfig,
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
  # Exact English seed exported from content-editor.operations.json through MCP.
  # Optional DesignerMCPConfig composes/builds/retires its owned review first.
  [string]$ContentEditorSourceDirectory = 'build/content-editor/source',
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
  # Optional already-built Pascal compiler fixture exercises ordinary queued /
  # running Cancel controls while preserving an actual accepted native preview.
  [string]$NativeStudioCompilerFixture,
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

function Test-NyxCompilerTypes([string]$Executable, [string[]]$Arguments,
  [string[]]$Cases = @()) {
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
    typeahead_policy = 'TNyxTypeAheadOptions'
    # Overload diagnostics can point at the final numeric field overload.
    collection_cell_mode = 'TNyxCollectionCellMode|TNyxNumberFieldRef'
  }
  foreach ($nyxTypeCase in $nyxTypeCases.Keys) {

    if ($Cases.Count -gt 0 -and $nyxTypeCase -notin $Cases) {
      continue
    }
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

  if ($Target -eq 'compiler-lifecycle') {
    # Actual Pascal children, native process handles and semantic host assertions
    # belong to the Pascal harness. This script only orchestrates platform tools.
    # No listeners, protected roots, profiles or enrollment are refreshed.
    if (-not $IsWindows) { throw 'Actual child-handle qualification currently requires Windows.' }
    $nyxLifecycle = Join-Path $nyxRoot 'build/compiler-lifecycle/maintained'
    New-Item -ItemType Directory -Force $nyxLifecycle | Out-Null
    $nyxLifecycleFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxLifecycle", "-FE$nyxLifecycle")
    foreach ($nyxProgram in @('nyx_build_compiler_fixture', 'nyx_compiler_lifecycle_tests',
      'nyx_build_job_tests', 'nyx_agent_build_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxLifecycleFlags + @("tests/$nyxProgram.lpr"))
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxLifecycleRuntime = Join-Path $nyxLifecycle ('runtime-' + [Guid]::NewGuid().ToString())
    & (Join-Path $nyxLifecycle 'nyx_compiler_lifecycle_tests.exe') $nyxLifecycleRuntime (Join-Path $nyxLifecycle 'nyx_build_compiler_fixture.exe') $nyxRuntime
    if ($LASTEXITCODE -ne 0) { throw 'Actual compiler cancellation/join qualification failed.' }
    & (Join-Path $nyxLifecycle 'nyx_build_job_tests.exe') ($nyxLifecycleRuntime + '-retention') (Join-Path $nyxLifecycle 'nyx_build_compiler_fixture.exe') $nyxRuntime
    if ($LASTEXITCODE -ne 0) { throw 'Compiler retention/profile/retry regression failed.' }
    & (Join-Path $nyxLifecycle 'nyx_agent_build_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Portable compiler admission/state qualification failed.' }
    Invoke-NyxCompiler $nyxFpc ($nyxLifecycleFlags + @('studio/nyx_studio_server.lpr'))
    $nyxLifecycleBrowser = Join-Path $nyxLifecycle 'browser'
    New-Item -ItemType Directory -Force $nyxLifecycleBrowser | Out-Null
    foreach ($nyxProgram in @('tests/nyx_agent_build_tests.lpr',
      'tests/nyx_studio_build_controls_tests.lpr', 'studio/nyx_studio.lpr',
      'studio/nyx_studio_preview.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxLifecycleBrowser", $nyxProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxLifecycleBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/agent-builds.html') -Destination $nyxLifecycleBrowser
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/studio-build-controls.html') -Destination $nyxLifecycleBrowser
    Write-Host 'Compiler lifecycle qualified; browser artifacts staged for an admitted host.'
    exit 0
  }

  if ($Target -eq 'clock-review') {
    $nyxClockReviewBin = Join-Path $nyxRoot 'build/clock-review/maintained/bin'
    New-Item -ItemType Directory -Path $nyxClockReviewBin -Force | Out-Null
    $nyxClockReviewFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxClockReviewBin", "-FE$nyxClockReviewBin")
    Invoke-NyxCompiler $nyxFpc ($nyxClockReviewFlags + @('tools/nyx_clock_authoring_review.lpr'))

    if ($ClockReviewMCPConfig) {
      & (Join-Path $nyxClockReviewBin 'nyx_clock_authoring_review.exe') `
        ([IO.Path]::GetFullPath($ClockReviewMCPConfig)) `
        ([IO.Path]::GetFullPath($ClockReviewDirectory)) $HttpURL

      if ($LASTEXITCODE -ne 0) {
        throw 'Authenticated clock authoring qualification failed; retained receipts describe the boundary'
      }
    }
    exit 0
  }

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

  if ($Target -eq 'compiled-preview-lifetime') {
    # Keep live editor lifetime evidence separate from protected LAN artifacts.
    # Pascal owns authenticated semantic composition, actual operator builds and
    # browser input/document identity. This shell starts no listener or browser.
    $nyxLifetime = Join-Path $nyxRoot 'build/compiled-preview-lifetime'
    $nyxLifetimeUnits = Join-Path $nyxLifetime 'units'
    $nyxLifetimeBin = Join-Path $nyxLifetime 'bin'
    $nyxLifetimeWeb = Join-Path $nyxLifetime 'web'
    if ($BrowserOutput) { $nyxLifetimeWeb = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxLifetimeUnits, $nyxLifetimeBin, $nyxLifetimeWeb | Out-Null
    $nyxLifetimeFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxLifetimeUnits", "-FE$nyxLifetimeBin")
    Invoke-NyxCompiler $nyxFpc ($nyxLifetimeFlags + @('tests/nyx_resource_runtime_server.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxLifetimeFlags + @('tests/nyx_compiled_studio_lifetime.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxLifetimeFlags + @('tests/nyx_studio_deployment_check.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxLifetimeFlags + @('tests/nyx_studio_deployment_observer.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Fustudio',
      "-FE$nyxLifetimeWeb", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxLifetimeWeb 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/index.html') -Destination $nyxLifetimeWeb
    if ($ResourceRuntimeHome) {
      $nyxLifetimeRun = @($HttpURL, $ResourceRuntimeHome, $nyxLocalConfig)
      if ($VerifySemanticLaunch) { $nyxLifetimeRun += 'semantic' }
      & (Join-Path $nyxLifetimeBin 'nyx_compiled_studio_lifetime.exe') @nyxLifetimeRun
      if ($LASTEXITCODE -ne 0) { throw 'Ordinary compiled Studio lifetime qualification failed' }
    }
    exit 0
  }

  if ($Target -eq 'studio-release') {
    # Pascal owns source admission, privacy boundaries, artifact closure and
    # byte verification. Shell work only invokes compilers in that new snapshot.
    # Preparation changes no service, enrollment, profile or editor project.
    # Optional runtime qualification owns a new private profile/enrollment/jobs
    # subtree; its protocol engine and host are never started as listeners.
    $nyxReleaseOutput = [IO.Path]::GetFullPath((Join-Path $nyxRoot $ReleaseOutput))

    if ([IO.Path]::IsPathRooted($ReleaseOutput)) {
      $nyxReleaseOutput = [IO.Path]::GetFullPath($ReleaseOutput)
    }

    if (Test-Path -LiteralPath $nyxReleaseOutput) {
      throw 'Release output already exists; select a new -ReleaseOutput directory.'
    }
    $nyxReleaseBuild = Join-Path $nyxRoot 'build/studio-release'
    $nyxReleaseTool = Join-Path $nyxReleaseBuild 'tool'
    $nyxReleaseServerUnits = Join-Path $nyxReleaseBuild 'server-units'
    New-Item -ItemType Directory -Force $nyxReleaseTool, $nyxReleaseServerUnits,
      (Split-Path -Parent $nyxReleaseOutput) | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl',
      '-Fusrc', '-Fustudio', "-FU$nyxReleaseTool", "-FE$nyxReleaseTool",
      'tools/nyx_studio_release.lpr')
    $nyxReleaseProgram = Join-Path $nyxReleaseTool 'nyx_studio_release.exe'

    if (-not $IsWindows) {
      $nyxReleaseProgram = Join-Path $nyxReleaseTool 'nyx_studio_release'
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxReleaseRevision = (& git rev-parse HEAD).Trim()

    if ($LASTEXITCODE -ne 0) { throw 'A source checkpoint is required for release preparation.' }
    & $nyxReleaseProgram prepare $nyxRoot $nyxRuntime $nyxReleaseOutput

    if ($LASTEXITCODE -ne 0) { throw 'Pascal release preparation refused the destination or sources.' }
    Push-Location $nyxReleaseOutput
    try {
      Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
        '-O2', '-Xs', '-Fusrc', '-Fustudio', "-FU$nyxReleaseServerUnits", '-FEbin',
        'studio/nyx_studio_server.lpr')
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Fusrc',
        '-Fustudio', '-Jirtl.js', '-FEweb', 'studio/nyx_studio.lpr')
      # Invoke-NyxCompiler also builds the compiled independent module worker.
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Fusrc',
        '-Fustudio', '-Jirtl.js', '-FEweb', 'studio/nyx_studio_preview.lpr')
    } finally {
      Pop-Location
    }
    & $nyxReleaseProgram seal $nyxReleaseOutput $nyxReleaseRevision $nyxVersion (
      (& $nyxPas2js '-iV').Trim())

    if ($LASTEXITCODE -ne 0) { throw 'Release closure or byte verification failed.' }
    # Exercise corruption/refusal against a separate inert fixture and read the
    # real sealed bundle without mutation. Compile against the frozen unit set.
    $nyxReleaseChecks = $nyxReleaseOutput + '.qualification'
    New-Item -ItemType Directory $nyxReleaseChecks | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      "-Fu$nyxReleaseOutput/src", "-Fu$nyxReleaseOutput/studio",
      "-FU$nyxReleaseChecks", "-FE$nyxReleaseChecks", 'tests/nyx_studio_release_tests.lpr')
    $nyxReleaseTest = Join-Path $nyxReleaseChecks 'nyx_studio_release_tests.exe'

    if (-not $IsWindows) {
      $nyxReleaseTest = Join-Path $nyxReleaseChecks 'nyx_studio_release_tests'
    }
    & $nyxReleaseTest $nyxReleaseOutput (Join-Path $nyxReleaseChecks 'fixture')

    if ($LASTEXITCODE -ne 0) { throw 'Release preparation/integrity qualification failed.' }

    if ($VerifyReleaseRuntime) {
      if (-not $ReleaseRuntimeProfile -or -not (Test-Path -LiteralPath $ReleaseRuntimeProfile)) {
        throw 'Runtime qualification requires an explicit existing private -ReleaseRuntimeProfile.'
      }
      $nyxRuntimeSource = [IO.Path]::GetFullPath($ReleaseRuntimeSourceDirectory)
      Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
        "-Fu$nyxReleaseOutput/src", "-Fu$nyxReleaseOutput/studio", "-Fu$nyxRuntimeSource",
        "-FU$nyxReleaseChecks", "-FE$nyxReleaseChecks", 'tests/nyx_studio_runtime_tests.lpr')
      $nyxRuntimeTest = Join-Path $nyxReleaseChecks 'nyx_studio_runtime_tests.exe'

      if (-not $IsWindows) {
        $nyxRuntimeTest = Join-Path $nyxReleaseChecks 'nyx_studio_runtime_tests'
      }
      & $nyxRuntimeTest $nyxReleaseOutput (Join-Path $nyxReleaseChecks 'runtime') (
        [IO.Path]::GetFullPath($ReleaseRuntimeProfile)) $nyxRuntimeSource

      if ($LASTEXITCODE -ne 0) { throw 'Integrated frozen release/runtime qualification failed.' }
      # The same frozen host model admits exact sessions/history and rolls back
      # denied durable mutations. Its process fixture terminates only its own
      # producer handle, then resumes in a fresh process; no listener is opened.
      foreach ($nyxRecoveryName in @('nyx_studio_recovery_tests', 'nyx_studio_recovery_shared')) {
        Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
          "-Fu$nyxReleaseOutput/src", "-Fu$nyxReleaseOutput/studio", "-Fu$nyxRuntimeSource",
          "-FU$nyxReleaseChecks", "-FE$nyxReleaseChecks", "tests/$nyxRecoveryName.lpr")
        $nyxRecoveryTest = Join-Path $nyxReleaseChecks $nyxRecoveryName

        if ($IsWindows) { $nyxRecoveryTest += '.exe' }

        if ($nyxRecoveryName -eq 'nyx_studio_recovery_tests') {
          & $nyxRecoveryTest (Join-Path $nyxReleaseChecks 'recovery-runtime') $nyxRuntimeSource
          if ($LASTEXITCODE -ne 0) { throw 'Frozen protocol session recovery qualification failed.' }
          & $nyxRecoveryTest '--process' (Join-Path $nyxReleaseChecks 'recovery-process') $nyxRuntimeSource
          if ($LASTEXITCODE -ne 0) { throw 'Frozen abrupt-process recovery qualification failed.' }
        } else {
          & $nyxRecoveryTest
          if ($LASTEXITCODE -ne 0) { throw 'Frozen shared recovery ownership qualification failed.' }
        }
      }
    }
    Write-Host 'Frozen Studio release verified. No listener was launched or live release replaced.'
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
      $nyxCompilerFixture = ''

      if ($NativeStudioCompilerFixture) {
        $nyxCompilerFixture = [IO.Path]::GetFullPath($NativeStudioCompilerFixture)

        if (-not (Test-Path -LiteralPath $nyxCompilerFixture -PathType Leaf)) {
          throw 'The optional Pascal compiler fixture must already exist'
        }
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
      $nyxCompilerRunArguments = @($nyxCompilerRepository, $nyxCompilerProfile,
        $nyxCompilerSource, $nyxCompilerArtifacts, $HttpURL)

      if ($nyxCompilerFixture) {
        if ($VerifySemanticLaunch) { throw 'Choose semantic launches or compiler-fixture controls for this journey.' }
        $nyxCompilerRunArguments += @('build-controls', $nyxCompilerFixture)
      }
      elseif ($VerifySemanticLaunch) {
        $nyxCompilerRunArguments += 'semantic-launch'
      }
      & (Join-Path $nyxStudioNative 'nyx_native_build_tests.exe') @nyxCompilerRunArguments

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
      Invoke-NyxCompiler $nyxFpc ($nyxReviewFlags + @('tests/nyx_agent_authority_tests.lpr'))
      & (Join-Path $nyxReviewNative 'nyx_agent_authority_tests.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Portable connection authority checks failed' }
      Invoke-NyxCompiler $nyxFpc ($nyxReviewFlags + @('tests/nyx_mcp_authority_tests.lpr'))
      # Every maintained run owns a separate runtime. Existing paths are never
      # deleted or reused; the suspended protocol opens no listener.
      $nyxAuthorityRuntime = Join-Path $nyxReviewDir ('authority-' + [guid]::NewGuid().ToString('N'))
      & (Join-Path $nyxReviewNative 'nyx_mcp_authority_tests.exe') $nyxAuthorityRuntime

      if ($LASTEXITCODE -ne 0) { throw 'Native protocol connection authority checks failed' }
      # The maintained browser-host protocol needs the installed compiler's
      # fpwebsocket units; use the already qualified matching toolchain.
      $nyxReviewHostFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
        '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxReviewProtocol", "-FE$nyxReviewProtocol")
      Invoke-NyxCompiler $nyxLclFpc ($nyxReviewHostFlags + @('tests/nyx_mcp_review_tests.lpr'))
      Invoke-NyxCompiler $nyxFpc ($nyxReviewFlags + @('studio/nyx_studio_server.lpr'))
      foreach ($nyxReviewProgram in @('tests/nyx_review_tests.lpr', 'tests/nyx_agent_authority_tests.lpr',
          'studio/nyx_studio.lpr',
          'studio/nyx_studio_review.lpr', 'studio/nyx_studio_preview.lpr')) {
        Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Fusrc', '-Fustudio',
          "-FE$nyxBrowserDir", $nyxReviewProgram)
      }
      foreach ($nyxReviewHost in @('reviews.html', 'authority.html', 'index.html',
          'agent-review.html', 'agent-preview.html')) {
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

    if (-not $PropertyMCPConfig -and
      -not (Test-Path -LiteralPath (Join-Path $nyxPropertySource 'nyx.generated.view.pas'))) {
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

    if ($PropertyMCPConfig) {
      & (Join-Path $nyxPropertyAuthor 'nyx_mcp_catalog_focus.exe') `
        ([IO.Path]::GetFullPath($PropertyMCPConfig)) $nyxPropertySource 'review-properties'
      if ($LASTEXITCODE -ne 0) { throw 'Owned semantic property qualification failed' }
    }
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

  if ($Target -eq 'release-observer') {
    # Compile only. The Pascal consumer requires an explicit owned workspace;
    # it never claims the primary project or changes enrollment/permissions.
    # Its actual editor navigation is host input; design edits/builds use MCP.
    $nyxObserverOutput = Join-Path $nyxRoot 'build/release-observer/maintained'
    New-Item -ItemType Directory -Force $nyxObserverOutput | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxObserverOutput", "-FE$nyxObserverOutput",
      'tests/nyx_studio_release_observer.lpr')
    Write-Host 'Observer built. Supply enrolled repository, owned calendar workspace, fresh evidence directory and CSS width.'
    exit 0
  }

  if ($Target -eq 'legacy-snapshot') {
    # Explicit legacy admission; this neither snapshots a live editor nor starts
    # a listener. Pascal owns history-policy refusal, paired data and handle tests.
    $nyxLegacyRoot = Join-Path $nyxRoot 'build/legacy-refresh/maintained'
    $nyxLegacyNative = Join-Path $nyxLegacyRoot 'native'
    $nyxLegacyWeb = Join-Path $nyxLegacyRoot 'web'
    New-Item -ItemType Directory -Force $nyxLegacyNative, $nyxLegacyWeb | Out-Null
    $nyxLegacyFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxLegacyNative", "-FE$nyxLegacyNative")
    Invoke-NyxCompiler $nyxFpc ($nyxLegacyFlags + @('tests/nyx_studio_legacy_tests.lpr'))
    & (Join-Path $nyxLegacyNative 'nyx_studio_legacy_tests.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Legacy snapshot admission checks failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxLegacyFlags + @('tools/nyx_studio_seed.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxLegacyWeb", 'tests/nyx_studio_legacy_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxLegacyWeb 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/legacy-snapshot.html') -Destination $nyxLegacyWeb -Force
    Write-Host 'Legacy bootstrap built; runtime seeding requires an explicit policy and fresh destination.'
    exit 0
  }

  if ($Target -eq 'color-fields') {
    # Pascal owns admission, source/history and actual native picker behavior.
    # Shell only compiles/runs consumers and stages matching browser artifacts.
    # Nothing starts a listener/browser or replaces an operator project.
    $nyxColorRoot = Join-Path $nyxRoot 'build/color-fields/maintained'
    $nyxColorSeed = [IO.Path]::GetFullPath($ColorSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxColorSeed 'nyx.generated.view.pas'))) {
      throw 'Supply the exact MCP-exported English color workshop; see docs/colors.md'
    }
    $nyxColorValues = Join-Path $nyxColorRoot 'values'
    $nyxColorNative = Join-Path $nyxColorRoot 'native'
    $nyxColorGenerated = Join-Path $nyxColorRoot 'generated'
    $nyxColorBrowser = Join-Path $nyxColorRoot 'browser'
    New-Item -ItemType Directory -Force $nyxColorValues, $nyxColorNative,
      $nyxColorGenerated, $nyxColorBrowser | Out-Null
    $nyxColorUnits = @('-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxColorSeed")
    $nyxColorChecks = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh')
    $nyxColorValueFlags = $nyxColorChecks + $nyxColorUnits +
      @("-FU$nyxColorValues", "-FE$nyxColorValues")
    Invoke-NyxCompiler $nyxFpc ($nyxColorValueFlags + @('tests/nyx_color_values_tests.lpr'))
    & (Join-Path $nyxColorValues 'nyx_color_values_tests.exe') $nyxColorGenerated

    if ($LASTEXITCODE -ne 0) {
      throw 'RGB value/source/history/semantic checks failed'
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxColorPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxColorNativeFlags = $nyxColorChecks + $nyxColorUnits + @(
      "-Fu$nyxColorGenerated", "-FU$nyxColorNative", "-FE$nyxColorNative",
      "-Fu$nyxLazarus/lcl/units/$nyxColorPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxColorPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxColorPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxColorPlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxColorNativeFlags + @('tests/nyx_color_controls.lpr'))
    & (Join-Path $nyxColorNative 'nyx_color_controls.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Actual native RGB picker controls failed'
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxColorNativeFlags + @('studio/nyx_studio_native.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxColorValueFlags + @('studio/nyx_studio_server.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxColorBrowserFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js') +
      $nyxColorUnits + @("-Fu$nyxColorGenerated", "-FE$nyxColorBrowser")
    foreach ($nyxColorProgram in @('tests/nyx_color_values_tests.lpr',
      'tests/nyx_color_controls.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js ($nyxColorBrowserFlags + @($nyxColorProgram))
    }
    foreach ($nyxColorFamily in @('text', 'boolean', 'number')) {
      $nyxColorFailure = "tests/compile_fail/nyx_invalid_rgb_$nyxColorFamily.lpr"
      $nyxColorNativeLog = Join-Path $nyxColorRoot "refused-native-$nyxColorFamily.log"
      & $nyxFpc @nyxColorValueFlags $nyxColorFailure *> $nyxColorNativeLog

      if ($LASTEXITCODE -eq 0 -or -not (Select-String -LiteralPath $nyxColorNativeLog `
        -Pattern 'Error:.*Incompatible type.*expected.*(Integer|LongInt)' -Quiet)) {
        throw "Native compiler failed to refuse RGB $nyxColorFamily channel"
      }
      $nyxColorBrowserLog = Join-Path $nyxColorRoot "refused-browser-$nyxColorFamily.log"
      & $nyxPas2js @nyxColorBrowserFlags $nyxColorFailure *> $nyxColorBrowserLog

      if ($LASTEXITCODE -eq 0 -or -not (Select-String -LiteralPath $nyxColorBrowserLog `
        -Pattern 'Error:.*Incompatible type.*expected.*(Integer|LongInt)' -Quiet)) {
        throw "Browser compiler failed to refuse RGB $nyxColorFamily channel"
      }
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxColorBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/color-fields.html') -Destination $nyxColorBrowser
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/color-values.html') -Destination $nyxColorBrowser
    Write-Host 'RGB source/native controls qualified; browser consumers staged without execution.'
    exit 0
  }

  if ($Target -in @('time-values', 'time-fields')) {
    # A checked public Pascal contract and its exact generated companion. This
    # stages browser consumers without starting a browser, listener or project.
    $nyxTimeRoot = Join-Path $nyxRoot 'build/time-fields/maintained'
    $nyxTimeNative = Join-Path $nyxTimeRoot 'native'
    $nyxTimeSource = Join-Path $nyxTimeRoot 'source'
    $nyxTimeWeb = Join-Path $nyxTimeRoot 'web'
    New-Item -ItemType Directory -Force $nyxTimeNative, $nyxTimeSource, $nyxTimeWeb | Out-Null
    $nyxTimeNativeFlags = @('-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxTimeSource", "-FU$nyxTimeNative", "-FE$nyxTimeNative")
    Invoke-NyxCompiler $nyxFpc (@('-B') + $nyxTimeNativeFlags + @('tests/nyx_time_values_tests.lpr'))
    & (Join-Path $nyxTimeNative 'nyx_time_values_tests.exe') $nyxTimeSource

    if ($LASTEXITCODE -ne 0) {
      throw 'Checked portable clock authoring/admission failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxTimeNativeFlags + @('tests/nyx_time_reconstruction.lpr'))
    & (Join-Path $nyxTimeNative 'nyx_time_reconstruction.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Exact compiled clock reconstruction failed'
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxTimeWebFlags = @('-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Futests', '-Fustudio',
      "-Fu$nyxTimeSource", "-FE$nyxTimeWeb")
    foreach ($nyxTimeConsumer in @('tests/nyx_time_values_tests.lpr', 'tests/nyx_time_reconstruction.lpr')) {
      Invoke-NyxCompiler $nyxPas2js (@('-B') + $nyxTimeWebFlags + @($nyxTimeConsumer))
    }
    # Qualify the expected argument family, not merely a failed build. A missing
    # unit or invalid compiler flag must never count as strong-type evidence.
    $nyxTimeTypes = [ordered]@{
      bound = 'TNyxClockTime'
      precision = 'TNyxTimePrecision'
      step = 'Integer|LongInt'
      choice = 'TNyxClockTime'
      base = 'TNyxValueDomain'
      value = 'TNyxClockTime'
    }
    foreach ($nyxTimeFamily in $nyxTimeTypes.Keys) {
      $nyxTimeFailure = "tests/compile_fail/nyx_invalid_time_$nyxTimeFamily.lpr"
      $nyxTimeTypeError = 'Error:.*Incompatible type.*expected.*(' + $nyxTimeTypes[$nyxTimeFamily] + ')'
      $nyxTimeNativeLog = Join-Path $nyxTimeRoot "refused-native-$nyxTimeFamily.log"
      & $nyxFpc @nyxTimeNativeFlags $nyxTimeFailure *> $nyxTimeNativeLog

      if ($LASTEXITCODE -eq 0 -or -not (Select-String -LiteralPath $nyxTimeNativeLog -Pattern $nyxTimeTypeError -Quiet)) {
        throw "Native compiler did not refuse the wrong clock argument family: $nyxTimeFamily"
      }
      $nyxTimeWebLog = Join-Path $nyxTimeRoot "refused-browser-$nyxTimeFamily.log"
      & $nyxPas2js @nyxTimeWebFlags $nyxTimeFailure *> $nyxTimeWebLog

      if ($LASTEXITCODE -eq 0 -or -not (Select-String -LiteralPath $nyxTimeWebLog -Pattern $nyxTimeTypeError -Quiet)) {
        throw "Browser compiler did not refuse the wrong clock argument family: $nyxTimeFamily"
      }
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxTimeWeb 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/time-values.html') -Destination $nyxTimeWeb -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/time-reconstruction.html') -Destination $nyxTimeWeb -Force

    if ($Target -eq 'time-fields') {
      # Consume the exact compiled public-Pascal companion through ordinary
      # native controls. The Pascal fixture owns drafts, popup input, domain
      # admission, geometry and retirement checks. No active MCP project changes.
      $nyxTimeLcl = Join-Path $nyxTimeRoot 'lcl'
      $nyxTimePrints = Join-Path $nyxTimeRoot 'native-prints'
      New-Item -ItemType Directory -Force $nyxTimeLcl, $nyxTimePrints | Out-Null
      $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
      $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
      $nyxTimePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
      Invoke-NyxCompiler $nyxLclFpc (@('-B') + $nyxTimeNativeFlags + @(
        "-Fu$nyxLazarus/lcl/units/$nyxTimePlatform",
        "-Fu$nyxLazarus/lcl/units/$nyxTimePlatform/$Widgetset",
        "-Fu$nyxLazarus/components/lazutils/lib/$nyxTimePlatform",
        "-FU$nyxTimeLcl", "-FE$nyxTimeLcl", 'tests/nyx_time_controls.lpr'))
      & (Join-Path $nyxTimeLcl 'nyx_time_controls.exe') $nyxTimePrints

      if ($LASTEXITCODE -ne 0) {
        throw 'Actual native clock controls failed'
      }
      Invoke-NyxCompiler $nyxPas2js (@('-B') + $nyxTimeWebFlags + @('tests/nyx_time_controls.lpr'))
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/time-controls.html') -Destination $nyxTimeWeb -Force
      Write-Host 'Clock controls qualified natively; printed images are diagnostic. Browser controls are staged, not executed.'
    }
    Write-Host 'Clock contract/reconstruction staged; twelve wrong-family compiler cases refused. Browser execution remains separate.'
    exit 0
  }

  if ($Target -eq 'time-policy') {
    # Pascal owns bounded semantic/schema checks, real Inspector/queue input and
    # exact compiled source reconstruction. Stage both-target consumers without
    # starting a browser/listener or replacing an observing user's project.
    $nyxTimePolicyRoot = Join-Path $nyxRoot 'build/time-policy/maintained'
    $nyxTimePolicySource = [IO.Path]::GetFullPath($TimePolicySourceDirectory)
    $nyxTimePolicySeed = Join-Path $nyxTimePolicySource 'nyx.generated.time.pas'

    if (-not (Test-Path -LiteralPath $nyxTimePolicySeed -PathType Leaf)) {
      throw 'Build the typed public clock companion first; see docs/time-fields.md'
    }
    $nyxTimePolicyNative = Join-Path $nyxTimePolicyRoot 'native'
    $nyxTimePolicyLcl = Join-Path $nyxTimePolicyRoot 'lcl'
    $nyxTimePolicyResult = Join-Path $nyxTimePolicyRoot 'result'
    $nyxTimePolicyReplay = Join-Path $nyxTimePolicyRoot 'reconstruction'
    $nyxTimePolicyWeb = Join-Path $nyxTimePolicyRoot 'web'
    New-Item -ItemType Directory -Force $nyxTimePolicyNative, $nyxTimePolicyLcl,
      $nyxTimePolicyResult, $nyxTimePolicyReplay, $nyxTimePolicyWeb | Out-Null
    $nyxTimePolicyFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    Invoke-NyxCompiler $nyxFpc ($nyxTimePolicyFlags + @(
      "-FU$nyxTimePolicyNative", "-FE$nyxTimePolicyNative", 'tests/nyx_date_policy_schema.lpr'))
    & (Join-Path $nyxTimePolicyNative 'nyx_date_policy_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Clock value-domain discovery schema checks failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxTimePolicyFlags + @(
      "-Fu$nyxTimePolicySource", "-FU$nyxTimePolicyNative", "-FE$nyxTimePolicyNative",
      'tests/nyx_time_policy_tests.lpr'))
    & (Join-Path $nyxTimePolicyNative 'nyx_time_policy_tests.exe') $nyxTimePolicySeed

    if ($LASTEXITCODE -ne 0) { throw 'Local semantic clock-policy admission failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxTimePolicyPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxTimePolicyFlags + @(
      "-Fu$nyxTimePolicySource", "-Fu$nyxLazarus/lcl/units/$nyxTimePolicyPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxTimePolicyPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxTimePolicyPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxTimePolicyPlatform",
      "-FU$nyxTimePolicyLcl", "-FE$nyxTimePolicyLcl", 'tests/nyx_time_policy_controls.lpr'))
    & (Join-Path $nyxTimePolicyLcl 'nyx_time_policy_controls.exe') $nyxTimePolicySeed $nyxTimePolicyResult

    if ($LASTEXITCODE -ne 0) { throw 'Actual native clock Inspector/queue controls failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxTimePolicyFlags + @(
      "-Fu$nyxTimePolicyResult", "-FU$nyxTimePolicyReplay", "-FE$nyxTimePolicyReplay",
      'tests/nyx_time_policy_generated.lpr'))
    & (Join-Path $nyxTimePolicyReplay 'nyx_time_policy_generated.exe') (Join-Path $nyxTimePolicyResult 'design.nyx.json')

    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled clock-policy reconstruction failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxTimePolicyWebFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxTimePolicyWeb")
    Invoke-NyxCompiler $nyxPas2js ($nyxTimePolicyWebFlags + @(
      "-Fu$nyxTimePolicySource", 'tests/nyx_time_policy_controls.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxTimePolicyWebFlags + @(
      "-Fu$nyxTimePolicySource", 'tests/nyx_time_studio_browser.lpr'))
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxTimePolicyWeb", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js ($nyxTimePolicyWebFlags + @(
      "-Fu$nyxTimePolicyResult", 'tests/nyx_time_policy_generated.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxTimePolicyWeb 'rtl.js') -Force
    Copy-Item -LiteralPath $nyxTimePolicySeed -Destination (Join-Path $nyxTimePolicyWeb 'seed.pas.txt') -Force
    Copy-Item -LiteralPath (Join-Path $nyxTimePolicyResult 'design.nyx.json'),
      (Join-Path $nyxRoot 'studio/web/time-policy-controls.html'),
      (Join-Path $nyxRoot 'studio/web/time-studio-browser.html'),
      (Join-Path $nyxRoot 'studio/web/time-policy-generated.html') -Destination $nyxTimePolicyWeb -Force
    Write-Host 'Clock Inspector/queue and compiled pair qualified natively. Browser controls/worker/replay staged, not executed.'
    exit 0
  }

  if ($Target -eq 'date-policy') {
    # Pascal owns typed policy admission, actual controls, paired history and
    # exact executed reconstruction. Orchestration starts no server/listener.
    $nyxDatePolicyRoot = Join-Path $nyxRoot 'build/date-policy/maintained'
    $nyxDatePolicySource = [IO.Path]::GetFullPath($DatePolicySourceDirectory)
    $nyxDatePolicySeed = Join-Path $nyxDatePolicySource 'nyx.generated.view.pas'
    $nyxDatePolicyNative = Join-Path $nyxDatePolicyRoot 'native'
    $nyxDatePolicyResult = Join-Path $nyxDatePolicyRoot 'result'
    $nyxDatePolicySchema = Join-Path $nyxDatePolicyRoot 'schema'
    $nyxDatePolicyReplay = Join-Path $nyxDatePolicyRoot 'reconstruction'
    $nyxDatePolicyWeb = Join-Path $nyxDatePolicyRoot 'web'

    if (-not (Test-Path -LiteralPath $nyxDatePolicySeed -PathType Leaf)) {
      throw 'Export the semantic date review first; see docs/date-fields.md'
    }
    New-Item -ItemType Directory -Force $nyxDatePolicyNative, $nyxDatePolicyResult,
      $nyxDatePolicySchema, $nyxDatePolicyReplay, $nyxDatePolicyWeb | Out-Null
    $nyxDatePolicyFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    Invoke-NyxCompiler $nyxFpc ($nyxDatePolicyFlags + @(
      "-FU$nyxDatePolicySchema", "-FE$nyxDatePolicySchema", 'tests/nyx_date_policy_schema.lpr'))
    & (Join-Path $nyxDatePolicySchema 'nyx_date_policy_schema.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Value-domain discovery schema checks failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxDatePolicyPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxDatePolicyFlags + @(
      "-Fu$nyxDatePolicySource",
      "-Fu$nyxLazarus/lcl/units/$nyxDatePolicyPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxDatePolicyPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxDatePolicyPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxDatePolicyPlatform",
      "-FU$nyxDatePolicyNative", "-FE$nyxDatePolicyNative", 'tests/nyx_date_policy_controls.lpr'))
    & (Join-Path $nyxDatePolicyNative 'nyx_date_policy_controls.exe') $nyxDatePolicySeed $nyxDatePolicyResult

    if ($LASTEXITCODE -ne 0) { throw 'Actual native Studio date-policy controls failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxDatePolicyFlags + @(
      "-Fu$nyxDatePolicyResult", "-FU$nyxDatePolicyReplay", "-FE$nyxDatePolicyReplay",
      'tests/nyx_date_policy_generated.lpr'))
    & (Join-Path $nyxDatePolicyReplay 'nyx_date_policy_generated.exe') (Join-Path $nyxDatePolicyResult 'design.nyx.json')

    if ($LASTEXITCODE -ne 0) { throw 'Exact executed date-policy reconstruction failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxDatePolicyBrowserFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxDatePolicyWeb")
    Invoke-NyxCompiler $nyxPas2js ($nyxDatePolicyBrowserFlags + @(
      "-Fu$nyxDatePolicySource", 'tests/nyx_date_policy_controls.lpr'))
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxDatePolicyWeb", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js ($nyxDatePolicyBrowserFlags + @(
      "-Fu$nyxDatePolicyResult", 'tests/nyx_date_policy_generated.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxDatePolicyWeb 'rtl.js') -Force
    Copy-Item -LiteralPath $nyxDatePolicySeed -Destination (Join-Path $nyxDatePolicyWeb 'seed.pas.txt') -Force
    Copy-Item -LiteralPath (Join-Path $nyxDatePolicyResult 'design.nyx.json'),
      (Join-Path $nyxRoot 'studio/web/date-policy-controls.html'),
      (Join-Path $nyxRoot 'studio/web/date-policy-generated.html') -Destination $nyxDatePolicyWeb -Force
    Write-Host 'Date-policy consumers staged; browser execution needs an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'date-fields') {
    # Compile/replay current typed generated source, then exercise real LCL
    # controls and stage the matching browser consumer. No listener or rollout.
    $nyxDateRoot = Join-Path $nyxRoot 'build/date-fields/maintained'
    $nyxDateSource = [IO.Path]::GetFullPath($DateSourceDirectory)
    $nyxDateExport = Join-Path $nyxDateRoot 'export'
    $nyxDateTyped = Join-Path $nyxDateRoot 'typed'
    $nyxDateReplay = Join-Path $nyxDateRoot 'reconstruction'
    $nyxDateNative = Join-Path $nyxDateRoot 'native'
    $nyxDateBrowser = Join-Path $nyxDateRoot 'web'

    if (-not (Test-Path -LiteralPath (Join-Path $nyxDateSource 'nyx.generated.view.pas'))) {
      throw 'Export the semantic date review first; see docs/date-fields.md'
    }
    New-Item -ItemType Directory -Force $nyxDateExport, $nyxDateTyped,
      $nyxDateReplay, $nyxDateNative, $nyxDateBrowser | Out-Null
    $nyxFpc = Resolve-NyxTool $Fpc 'FPC' 'fpc'
    $nyxDatePortableFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxDateSource")
    Invoke-NyxCompiler $nyxFpc ($nyxDatePortableFlags +
      @("-FU$nyxDateExport", "-FE$nyxDateExport", 'tests/nyx_date_export.lpr'))
    & (Join-Path $nyxDateExport 'nyx_date_export.exe') $nyxDateTyped

    if ($LASTEXITCODE -ne 0) { throw 'Typed date export failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxDatePortableFlags +
      @("-Fu$nyxDateTyped", "-FU$nyxDateReplay", "-FE$nyxDateReplay",
        'tests/nyx_date_reconstruction.lpr'))
    & (Join-Path $nyxDateReplay 'nyx_date_reconstruction.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Typed date reconstruction failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxDatePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc ($nyxDatePortableFlags + @(
      "-Fu$nyxLazarus/lcl/units/$nyxDatePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxDatePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxDatePlatform",
      "-FU$nyxDateNative", "-FE$nyxDateNative", 'tests/nyx_date_controls.lpr'))
    & (Join-Path $nyxDateNative 'nyx_date_controls.exe') (Join-Path $nyxDateRoot 'english')

    if ($LASTEXITCODE -ne 0) { throw 'Actual native date controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxDateBrowserFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', "-Fu$nyxDateSource", "-FE$nyxDateBrowser")
    Invoke-NyxCompiler $nyxPas2js ($nyxDateBrowserFlags + @('tests/nyx_date_controls.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxDateBrowserFlags +
      @("-Fu$nyxDateTyped", 'tests/nyx_date_reconstruction.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxDateBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/date-fields.html'),
      (Join-Path $nyxRoot 'studio/web/date-reconstruction.html') -Destination $nyxDateBrowser
    Write-Host 'Date consumers staged; execute on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'native-form') {
    # Pascal qualifies actual parked HWNDs, retained focus/input, nested logical
    # scrolling, usable captioned-group content and ordinary Studio source/history
    # at wide and narrow widths.
    # Printing is diagnostic; displayed capture requires a separate foreground
    # qualification. This target launches no service/browser or existing project.
    $nyxFormRoot = Join-Path $nyxRoot 'build/native-form/maintained'
    $nyxFormNative = Join-Path $nyxFormRoot 'native'
    $nyxFormWeb = Join-Path $nyxFormRoot 'web'
    $nyxFormControls = Join-Path $nyxFormRoot 'controls'
    New-Item -ItemType Directory -Force $nyxFormNative, $nyxFormWeb, $nyxFormControls | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxFormPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxFormFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio',
      "-Fu$nyxLazarus/lcl/units/$nyxFormPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxFormPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxFormPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxFormPlatform",
      "-FU$nyxFormNative", "-FE$nyxFormNative")
    foreach ($nyxFormProgram in @('nyx_logical_viewport_tests',
      'nyx_group_controls_tests', 'nyx_logical_viewport_controls', 'nyx_source_scheduling_tests')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxFormFlags + @("tests/$nyxFormProgram.lpr"))
      & (Join-Path $nyxFormNative ($nyxFormProgram + '.exe')) $nyxFormControls
      if ($LASTEXITCODE -ne 0) { throw ('Native viewport/Studio consumer failed: ' + $nyxFormProgram) }
    }
    foreach ($nyxFormProgram in @('tests/nyx_logical_viewport_tests.lpr',
      'tests/nyx_group_controls_tests.lpr', 'tests/nyx_logical_viewport_controls.lpr',
      'studio/nyx_studio.lpr',
      'studio/nyx_source_worker.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxFormWeb", $nyxFormProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxFormWeb 'rtl.js')
    Write-Host 'Native form projection qualified; browser consumers compiled without execution.'
    exit 0
  }

  if ($Target -eq 'scheduler-pool') {
    # Pascal establishes actual worker identities, FIFO/backpressure, cancellation
    # and independent lifetime. A separate ordinary native Studio consumer runs
    # source preparation/callbacks. Browser consumers are compiled/staged only;
    # this target starts no service/browser and changes no existing project pair.
    $nyxPoolRoot = Join-Path $nyxRoot 'build/scheduler-pool/maintained'
    $nyxPoolNative = Join-Path $nyxPoolRoot 'native'
    $nyxPoolLcl = Join-Path $nyxPoolRoot 'lcl'
    $nyxPoolWeb = Join-Path $nyxPoolRoot 'web'
    $nyxPoolControls = Join-Path $nyxPoolRoot 'source-controls'
    New-Item -ItemType Directory -Force $nyxPoolNative, $nyxPoolLcl,
      $nyxPoolWeb, $nyxPoolControls | Out-Null
    $nyxFpc = Resolve-NyxTool $Fpc 'FPC' 'fpc'
    $nyxPoolFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxPoolNative", "-FE$nyxPoolNative")
    foreach ($nyxPoolProgram in @('nyx_scheduler_pool_tests', 'nyx_scheduler_tests',
      'nyx_interaction_tests')) {
      Invoke-NyxCompiler $nyxFpc ($nyxPoolFlags + @("tests/$nyxPoolProgram.lpr"))
      & (Join-Path $nyxPoolNative ($nyxPoolProgram + '.exe')) `
        (Join-Path $nyxPoolNative 'nyx.interaction.generated.pas')
      if ($LASTEXITCODE -ne 0) { throw ('Native scheduler consumer failed: ' + $nyxPoolProgram) }
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPoolPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxPoolLclFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxPoolNative",
      "-Fu$nyxLazarus/lcl/units/$nyxPoolPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxPoolPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxPoolPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxPoolPlatform",
      "-FU$nyxPoolLcl", "-FE$nyxPoolLcl")
    Invoke-NyxCompiler $nyxLclFpc ($nyxPoolLclFlags + @('-dNYX_COMPILED_INTERACTIONS',
      'tests/nyx_interaction_controls_tests.lpr'))
    & (Join-Path $nyxPoolLcl 'nyx_interaction_controls_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native interaction controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxPoolLclFlags + @('tests/nyx_source_scheduling_tests.lpr'))
    & (Join-Path $nyxPoolLcl 'nyx_source_scheduling_tests.exe') $nyxPoolControls
    if ($LASTEXITCODE -ne 0) { throw 'Ordinary native Studio source scheduling failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxPoolProgram in @('tests/nyx_scheduler_tests.lpr',
      'tests/nyx_interaction_tests.lpr', 'tests/nyx_interaction_controls_tests.lpr',
      'studio/nyx_studio.lpr', 'studio/nyx_source_worker.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxPoolNative", "-FE$nyxPoolWeb",
        '-dNYX_COMPILED_INTERACTIONS', $nyxPoolProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxPoolWeb 'rtl.js')
    foreach ($nyxPoolHost in @('scheduler.html', 'interactions.html', 'interaction-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot ('studio/web/' + $nyxPoolHost)) `
        -Destination $nyxPoolWeb
    }
    Write-Host 'Bounded native workers and Studio qualified; browser consumers staged without execution.'
    exit 0
  }

  if ($Target -eq 'browser-worker') {
    # Real worker qualification uses a maintained Pascal driver with anonymous
    # Chromium pipes. Stage only: no new listener or observing service changes.
    $nyxWorkerCheckRoot = Join-Path $nyxRoot 'build/browser-worker-observation/maintained'
    $nyxWorkerCheckNative = Join-Path $nyxWorkerCheckRoot 'driver'
    $nyxWorkerCheckBrowser = Join-Path $nyxWorkerCheckRoot 'web'
    New-Item -ItemType Directory -Force $nyxWorkerCheckNative,
      $nyxWorkerCheckBrowser | Out-Null
    $nyxFpc = Resolve-NyxTool $Fpc 'FPC' 'fpc'
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-FU$nyxWorkerCheckNative", "-FE$nyxWorkerCheckNative",
      'tests/nyx_browser_ready_capture.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxWorkerCheckBrowser",
      'tests/nyx_event_queue_browser.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxWorkerCheckBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxWorkerCheckBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/event-inspector-controls.html') -Destination $nyxWorkerCheckBrowser
    Write-Host 'Real-worker fixture staged; use ready capture on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'menu-bar-workflow') {
    # Native Pascal owns semantic composition, exact source/HTTP compiler checks
    # and actual ordinary Studio input. Building this observer opens no service,
    # enrolls no client and creates no project; its explicit invocation selects
    # an isolated endpoint, fresh evidence directory and new/bar scenario.
    $nyxBarObserver = Join-Path $nyxRoot 'build/menu-bar-workflow/observer'
    New-Item -ItemType Directory -Force $nyxBarObserver | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxBarObserver", "-FE$nyxBarObserver",
      'tests/nyx_studio_menu_editor_observer.lpr')
    Write-Host 'Ordinary bar workflow observer built; select an explicit isolated host and new/bar.'
    exit 0
  }

  if ($Target -eq 'menu-bar-editor') {
    # The public form and ordinary Studio consume the exact accepted semantic
    # companion. Pascal owns input, paired work and draft/history assertions.
    # Staging starts no service and never imports into an observing project.
    if ([string]::IsNullOrWhiteSpace($MenuAuthoringSourceDirectory)) {
      throw 'Supply -MenuAuthoringSourceDirectory with the accepted saved bar export.'
    }
    $nyxBarSource = [IO.Path]::GetFullPath($MenuAuthoringSourceDirectory)
    $nyxBarSourceFile = Join-Path $nyxBarSource 'nyx.generated.view.pas'

    if (-not (Test-Path -LiteralPath $nyxBarSourceFile)) {
      throw 'The accepted saved menu-bar companion unit is missing.'
    }
    $nyxBarRoot = Join-Path $nyxRoot 'build/menu-bar-editor/maintained'
    $nyxBarNative = Join-Path $nyxBarRoot 'lcl'
    $nyxBarBrowser = Join-Path $nyxBarRoot 'browser'
    New-Item -ItemType Directory -Force $nyxBarNative, $nyxBarBrowser | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxBarPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxBarSource",
      "-Fu$nyxLazarus/lcl/units/$nyxBarPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxBarPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxBarPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxBarPlatform",
      "-FU$nyxBarNative", "-FE$nyxBarNative", 'tests/nyx_menu_bar_editor_controls.lpr')
    & (Join-Path $nyxBarNative 'nyx_menu_bar_editor_controls.exe') $nyxBarSourceFile (
      Join-Path $nyxBarRoot 'studio-projects') (Join-Path $nyxBarRoot 'captures')

    if ($LASTEXITCODE -ne 0) {
      throw 'Actual menu-bar editor/ordinary Studio input failed'
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxBarSource", "-FE$nyxBarBrowser",
      'tests/nyx_menu_bar_editor_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxBarBrowser", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxBarBrowser", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBarBrowser 'rtl.js')
    Copy-Item -LiteralPath $nyxBarSourceFile -Destination (Join-Path $nyxBarBrowser 'seed.pas.txt')
    Copy-Item -LiteralPath studio/web/menu-bar-editor.html -Destination $nyxBarBrowser
    Write-Host 'Public bar editor and full browser Studio staged; qualify on an existing admitted host.'
    exit 0
  }

  if ($Target -eq 'menu-editor') {
    if ([string]::IsNullOrWhiteSpace($MenuAuthoringSourceDirectory)) {
      throw 'Supply -MenuAuthoringSourceDirectory with the exact semantic menu export.'
    }
    $nyxMenuSource = [IO.Path]::GetFullPath($MenuAuthoringSourceDirectory)
    $nyxMenuSourceFile = Join-Path $nyxMenuSource 'nyx.generated.view.pas'

    if (-not (Test-Path -LiteralPath $nyxMenuSourceFile)) {
      throw 'The exported companion unit is missing; compose it through semantic MCP first.'
    }
    $nyxMenuRoot = Join-Path $nyxRoot 'build/menu-editor/maintained'
    $nyxMenuNative = Join-Path $nyxMenuRoot 'lcl'
    $nyxMenuBrowser = Join-Path $nyxMenuRoot 'browser'
    $nyxMenuObserver = Join-Path $nyxMenuRoot 'observer'
    New-Item -ItemType Directory -Force $nyxMenuNative, $nyxMenuBrowser,
      $nyxMenuObserver | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxMenuPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxMenuSource",
      "-Fu$nyxLazarus/lcl/units/$nyxMenuPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxMenuPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxMenuPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxMenuPlatform",
      "-FU$nyxMenuNative", "-FE$nyxMenuNative", 'tests/nyx_menu_editor_controls.lpr')
    & (Join-Path $nyxMenuNative 'nyx_menu_editor_controls.exe') $nyxMenuSourceFile (
      Join-Path $nyxMenuRoot 'studio-projects') (Join-Path $nyxMenuRoot 'captures')

    if ($LASTEXITCODE -ne 0) { throw 'Actual native menu editor/Studio journey failed' }
    # Build the ordinary full-host input consumer without creating a project or
    # opening a browser. Its explicit invocation names an isolated enrollment,
    # editor endpoint and fresh evidence directory; Pascal owns semantic setup.
    Invoke-NyxCompiler $nyxFpc @('-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxMenuObserver", "-FE$nyxMenuObserver",
      'tests/nyx_studio_menu_editor_observer.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-Fu$nyxMenuSource", "-FE$nyxMenuBrowser",
      'tests/nyx_menu_editor_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxMenuBrowser", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxMenuBrowser", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMenuBrowser 'rtl.js')
    Copy-Item -LiteralPath $nyxMenuSourceFile -Destination (Join-Path $nyxMenuBrowser 'seed.pas.txt')
    Copy-Item -LiteralPath studio/web/menu-editor.html -Destination $nyxMenuBrowser
    Write-Host 'Public menu editor staged; execute its desktop/narrow journey on an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'menu-authoring') {
    $nyxMenuRoot = Join-Path $nyxRoot 'build/menu-authoring/maintained'
    $nyxMenuCore = Join-Path $nyxMenuRoot 'core'
    $nyxMenuNative = Join-Path $nyxMenuRoot 'lcl'
    $nyxMenuBrowser = Join-Path $nyxMenuRoot 'browser'
    $nyxMenuGenerated = Join-Path $nyxMenuRoot 'generated'
    $nyxMenuTool = Join-Path $nyxMenuRoot 'tool'
    New-Item -ItemType Directory -Force $nyxMenuCore, $nyxMenuNative,
      $nyxMenuBrowser, $nyxMenuGenerated, $nyxMenuTool | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxMenuCore", "-FE$nyxMenuCore",
      'tests/nyx_menu_declarations_tests.lpr')
    & (Join-Path $nyxMenuCore 'nyx_menu_declarations_tests.exe') (
      Join-Path $nyxMenuGenerated 'nyx.generated.menu.pas')

    if ($LASTEXITCODE -ne 0) { throw 'Saved menu contracts failed' }
    $nyxMenuSource = $nyxMenuGenerated
    $nyxMenuDefines = @()

    if (-not [string]::IsNullOrWhiteSpace($DesignerMCPConfig)) {
      if ([string]::IsNullOrWhiteSpace($MenuAuthoringSourceDirectory)) {
        throw 'Supply an owned -MenuAuthoringSourceDirectory for MCP export.'
      }
      Invoke-NyxCompiler $nyxFpc @('-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
        '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxMenuTool", "-FE$nyxMenuTool",
        'tools/nyx_menu_authoring_review.lpr')
      & (Join-Path $nyxMenuTool 'nyx_menu_authoring_review.exe') $DesignerMCPConfig `
        $MenuAuthoringSourceDirectory $HttpURL.TrimEnd('/')

      if ($LASTEXITCODE -ne 0) { throw 'Authenticated saved menu authoring failed' }
    }

    if (-not [string]::IsNullOrWhiteSpace($MenuAuthoringSourceDirectory)) {
      $nyxMenuSource = [IO.Path]::GetFullPath($MenuAuthoringSourceDirectory)

      if (-not (Test-Path -LiteralPath (Join-Path $nyxMenuSource 'nyx.generated.view.pas'))) {
        throw 'An exact semantic menu export is required.'
      }
      $nyxMenuDefines = @('-dNYX_MCP_MENU')
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxMenuPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxMenuFlags = @('-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxMenuSource",
      "-Fu$nyxLazarus/lcl/units/$nyxMenuPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxMenuPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxMenuPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxMenuPlatform",
      "-FU$nyxMenuNative", "-FE$nyxMenuNative") + $nyxMenuDefines
    Invoke-NyxCompiler $nyxLclFpc ($nyxMenuFlags + @('tests/nyx_menu_declarations_controls.lpr'))

    if ($nyxMenuDefines.Count -eq 0) {
      & (Join-Path $nyxMenuNative 'nyx_menu_declarations_controls.exe')
    } else {
      & (Join-Path $nyxMenuNative 'nyx_menu_declarations_controls.exe') (
        Join-Path $nyxMenuSource 'nyx.generated.view.pas') (Join-Path $nyxMenuRoot 'studio-projects')
    }

    if ($LASTEXITCODE -ne 0) { throw 'Generated menu application/Studio failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxMenuBrowser", 'tests/nyx_menu_declarations_tests.lpr')
    Invoke-NyxCompiler $nyxPas2js (@('-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-Fu$nyxMenuSource", "-FE$nyxMenuBrowser") +
      $nyxMenuDefines + @('tests/nyx_menu_declarations_controls.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMenuBrowser 'rtl.js')
    Copy-Item -LiteralPath studio/web/menu-declarations.html,
      studio/web/menu-declarations-controls.html -Destination $nyxMenuBrowser
    Write-Host 'Saved menu fixtures staged; execute them on an admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'menu-companion') {
    if ([string]::IsNullOrWhiteSpace($DesignerMCPConfig)) {
      throw 'Supply -DesignerMCPConfig with an explicitly enrolled MCP configuration.'
    }
    $nyxMenuTool = Join-Path $nyxRoot 'build/menu/companion-tool'
    New-Item -ItemType Directory -Force $nyxMenuTool | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxMenuTool",
      "-FE$nyxMenuTool", 'tools/nyx_menu_companion.lpr')
    & (Join-Path $nyxMenuTool 'nyx_menu_companion.exe') $DesignerMCPConfig $MenuSourceDirectory

    if ($LASTEXITCODE -ne 0) { throw 'Semantic menu companion failed' }
    exit 0
  }

  if ($Target -eq 'menu-bar-authoring') {
    # The immutable English content was composed through MCP. Pascal owns typed
    # candidate/history admission and exports the exact paired application unit.
    # This target starts no listener and never edits an active Studio project.
    $nyxBarRoot = Join-Path $nyxRoot 'build/menu-bar-saved/maintained'
    $nyxBarCore = Join-Path $nyxBarRoot 'core'
    $nyxBarNative = Join-Path $nyxBarRoot 'lcl'
    $nyxBarBrowser = Join-Path $nyxBarRoot 'browser'
    $nyxBarGenerated = Join-Path $nyxBarRoot 'generated'
    $nyxBarTool = Join-Path $nyxBarRoot 'tool'
    $nyxBarSource = [IO.Path]::GetFullPath($MenuSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxBarSource 'nyx.generated.view.pas'))) {
      throw 'Export the semantic menu-bar companion first; see docs/menu.md'
    }
    New-Item -ItemType Directory -Force $nyxBarCore, $nyxBarNative, $nyxBarBrowser,
      $nyxBarGenerated, $nyxBarTool | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxBarSource",
      "-FU$nyxBarCore", "-FE$nyxBarCore", 'tests/nyx_menu_bar_declarations_tests.lpr')
    & (Join-Path $nyxBarCore 'nyx_menu_bar_declarations_tests.exe') $nyxBarGenerated

    if ($LASTEXITCODE -ne 0) {
      throw 'Saved menu-bar candidate/history qualification failed'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxBarPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxBarFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxBarGenerated",
      "-Fu$nyxLazarus/lcl/units/$nyxBarPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxBarPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxBarPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxBarPlatform",
      "-FU$nyxBarNative", "-FE$nyxBarNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxBarFlags +
      @('tests/nyx_menu_bar_declarations_controls.lpr'))
    & (Join-Path $nyxBarNative 'nyx_menu_bar_declarations_controls.exe') (
      Join-Path $nyxBarRoot 'native.png')

    if ($LASTEXITCODE -ne 0) {
      throw 'Automatically bound native menu-bar application failed'
    }
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', "-FU$nyxBarTool", "-FE$nyxBarTool",
      'tests/nyx_menu_bar_observer.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxBarSource", "-FE$nyxBarBrowser",
      'tests/nyx_menu_bar_declarations_tests.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', "-Fu$nyxBarGenerated", "-FE$nyxBarBrowser",
      'tests/nyx_menu_bar_declarations_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBarBrowser 'rtl.js')
    Copy-Item -LiteralPath studio/web/menu-bar-declarations.html,
      studio/web/menu-bar-declarations-controls.html -Destination $nyxBarBrowser
    Write-Host 'Saved menu-bar consumers staged; execute on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'menu-bar') {
    # Pascal owns bar coordination and actual-control qualification. The exact
    # English companion is composed/exported through semantic MCP beforehand.
    # This orchestration starts no HTTP/MCP service and edits no active project.
    $nyxBarRoot = Join-Path $nyxRoot 'build/menu-bar/maintained'
    $nyxBarNative = Join-Path $nyxBarRoot 'lcl'
    $nyxBarBrowser = Join-Path $nyxBarRoot 'browser'
    $nyxBarTool = Join-Path $nyxBarRoot 'tool'
    $nyxBarSource = [IO.Path]::GetFullPath($MenuSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxBarSource 'nyx.generated.view.pas'))) {
      throw 'Export the current semantic menu companion first; see docs/menu.md'
    }
    New-Item -ItemType Directory -Force $nyxBarNative, $nyxBarBrowser,
      $nyxBarTool | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxBarPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxBarFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxBarSource",
      "-Fu$nyxLazarus/lcl/units/$nyxBarPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxBarPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxBarPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxBarPlatform",
      "-FU$nyxBarNative", "-FE$nyxBarNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxBarFlags + @('tests/nyx_menu_bar_controls.lpr'))
    & (Join-Path $nyxBarNative 'nyx_menu_bar_controls.exe') (Join-Path $nyxBarRoot 'native.png')

    if ($LASTEXITCODE -ne 0) {
      throw 'Actual native menu bar controls failed'
    }
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', "-FU$nyxBarTool", "-FE$nyxBarTool",
      'tests/nyx_menu_bar_observer.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', "-Fu$nyxBarSource", "-FE$nyxBarBrowser",
      'tests/nyx_menu_bar_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBarBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/menu-bar.html') -Destination $nyxBarBrowser
    Write-Host 'Menu bar consumers staged; execute on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'menu') {
    # Compile actual controls/Studio and browser consumers with one exact source.
    # Serving/capturing uses an existing admitted host; no listener is started.
    $nyxMenuRoot = Join-Path $nyxRoot 'build/menu/maintained'
    $nyxMenuNative = Join-Path $nyxMenuRoot 'lcl'
    $nyxMenuBrowser = Join-Path $nyxMenuRoot 'browser'
    $nyxMenuTool = Join-Path $nyxMenuRoot 'tool'
    $nyxMenuSource = [IO.Path]::GetFullPath($MenuSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxMenuSource 'nyx.generated.view.pas'))) {
      throw 'Export the isolated MCP companion first; see docs/menu.md'
    }
    New-Item -ItemType Directory -Force $nyxMenuNative,
      $nyxMenuBrowser, $nyxMenuTool | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxMenuPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxMenuFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxMenuSource",
      "-Fu$nyxLazarus/lcl/units/$nyxMenuPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxMenuPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxMenuPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxMenuPlatform",
      "-FU$nyxMenuNative", "-FE$nyxMenuNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxMenuFlags + @('tests/nyx_menu_controls.lpr'))
    & (Join-Path $nyxMenuNative 'nyx_menu_controls.exe') (Join-Path $nyxMenuRoot 'native.png')

    if ($LASTEXITCODE -ne 0) { throw 'Native menu controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxMenuFlags + @('tests/nyx_studio_menu_controls.lpr'))
    & (Join-Path $nyxMenuNative 'nyx_studio_menu_controls.exe') `
      (Join-Path $nyxMenuSource 'nyx.generated.view.pas') (Join-Path $nyxMenuRoot 'studio-projects')

    if ($LASTEXITCODE -ne 0) { throw 'Native Studio command menu failed' }
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxMenuTool",
      "-FE$nyxMenuTool", 'tests/nyx_studio_menu_observer.lpr')
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', "-FU$nyxMenuTool",
      "-FE$nyxMenuTool", 'tests/nyx_browser_ready_capture.lpr')
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxMenuTool",
      "-FE$nyxMenuTool", 'tests/nyx_studio_workspace_observer.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxMenuSource",
      "-FE$nyxMenuBrowser", 'tests/nyx_menu_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxMenuBrowser", 'studio/nyx_studio.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxMenuBrowser", 'tests/nyx_studio_workspace_conflict.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMenuBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/menu.html') -Destination $nyxMenuBrowser
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/index.html') -Destination $nyxMenuBrowser
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/studio-workspace-conflict.html') -Destination $nyxMenuBrowser
    Write-Host 'Menu consumers staged; execute on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'popover-companion') {
    # One authenticated transport retains the temporary review's owner through
    # grouped composition, exact bounded source export and retirement.

    if ([string]::IsNullOrWhiteSpace($DesignerMCPConfig)) {
      throw 'Supply -DesignerMCPConfig with an explicitly enrolled MCP configuration.'
    }
    $nyxPopoverTool = Join-Path $nyxRoot 'build/popover/companion-tool'
    New-Item -ItemType Directory -Force $nyxPopoverTool | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxPopoverTool",
      "-FE$nyxPopoverTool", 'tools/nyx_popover_companion.lpr')
    & (Join-Path $nyxPopoverTool 'nyx_popover_companion.exe') $DesignerMCPConfig $PopoverSourceDirectory

    if ($LASTEXITCODE -ne 0) { throw 'Semantic popover companion failed' }
    exit 0
  }

  if ($Target -eq 'popover') {
    # Build/run actual native controls and stage the same semantic companion for
    # HTTP browser execution. No listener, enrollment or primary mutation occurs.
    $nyxPopoverRoot = Join-Path $nyxRoot 'build/popover/maintained'
    $nyxPopoverNative = Join-Path $nyxPopoverRoot 'lcl'
    $nyxPopoverBrowser = Join-Path $nyxPopoverRoot 'browser'
    $nyxPopoverTool = Join-Path $nyxPopoverRoot 'tool'
    $nyxPopoverSource = [IO.Path]::GetFullPath($PopoverSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxPopoverSource 'nyx.generated.view.pas'))) {
      throw 'Export the isolated MCP companion first; see docs/popover.md'
    }
    New-Item -ItemType Directory -Force $nyxPopoverNative,
      $nyxPopoverBrowser, $nyxPopoverTool | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPopoverPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxPopoverFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxPopoverSource",
      "-Fu$nyxLazarus/lcl/units/$nyxPopoverPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxPopoverPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxPopoverPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxPopoverPlatform",
      "-FU$nyxPopoverNative", "-FE$nyxPopoverNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxPopoverFlags + @('tests/nyx_popover_controls.lpr'))
    & (Join-Path $nyxPopoverNative 'nyx_popover_controls.exe') (Join-Path $nyxPopoverRoot 'native.png')

    if ($LASTEXITCODE -ne 0) { throw 'Native popover controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxPopoverFlags + @('tests/nyx_studio_help_controls.lpr'))
    $nyxPopoverCompanion = Join-Path $nyxPopoverSource 'nyx.generated.view.pas'
    $nyxPopoverProjects = Join-Path $nyxPopoverRoot 'studio-projects'
    & (Join-Path $nyxPopoverNative 'nyx_studio_help_controls.exe') $nyxPopoverCompanion $nyxPopoverProjects

    if ($LASTEXITCODE -ne 0) { throw 'Native Studio component help failed' }
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxPopoverTool",
      "-FE$nyxPopoverTool", 'tools/nyx_popover_companion.lpr')
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Futests', '-Fustudio', "-FU$nyxPopoverTool",
      "-FE$nyxPopoverTool", 'tests/nyx_studio_help_observer.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxPopoverSource",
      "-FE$nyxPopoverBrowser", 'tests/nyx_popover_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxPopoverBrowser", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxPopoverBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/popover.html') -Destination $nyxPopoverBrowser
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/index.html') -Destination $nyxPopoverBrowser
    Write-Host 'Popover consumer staged; execute on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -eq 'confirmation') {
    # Only orchestrate installed compilers and the Pascal control consumer.
    # No listener, enrollment, active-project replacement or handwritten source.
    $nyxConfirmationRoot = Join-Path $nyxRoot 'build/confirmation/maintained'
    $nyxConfirmationNative = Join-Path $nyxConfirmationRoot 'lcl'
    $nyxConfirmationBrowser = Join-Path $nyxConfirmationRoot 'browser'
    $nyxConfirmationSource = [IO.Path]::GetFullPath($ConfirmationSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxConfirmationSource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP confirmation review first; see docs/confirmation.md'
    }
    New-Item -ItemType Directory -Force $nyxConfirmationNative,
      $nyxConfirmationBrowser | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxConfirmationPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxConfirmationFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxConfirmationSource",
      "-Fu$nyxLazarus/lcl/units/$nyxConfirmationPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxConfirmationPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxConfirmationPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxConfirmationPlatform",
      "-FU$nyxConfirmationNative", "-FE$nyxConfirmationNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxConfirmationFlags + @('tests/nyx_confirmation_controls.lpr'))
    & (Join-Path $nyxConfirmationNative 'nyx_confirmation_controls.exe') (Join-Path $nyxConfirmationRoot 'english-native.png')

    if ($LASTEXITCODE -ne 0) { throw 'Native confirmation controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxConfirmationSource",
      "-FE$nyxConfirmationBrowser", 'tests/nyx_confirmation_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxConfirmationBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/confirmation.html') -Destination $nyxConfirmationBrowser
    Write-Host 'Confirmation consumer staged; execute on an existing admitted HTTP host.'
    exit 0
  }

  if ($Target -in @('typeahead-policy', 'typeahead-workflow')) {
    # Pascal owns saved-policy admission, history and actual adapter checks.
    # Reuse the unchanged exported semantic seed and enrich its typed bindings
    # locally. This target starts no listener and publishes no active project.
    $nyxPolicyRoot = Join-Path $nyxRoot 'build/typeahead-policy/maintained'
    $nyxWorkflow = $Target -eq 'typeahead-workflow'
    $nyxPolicyDefines = @('-dNYX_SAVED_TYPEAHEAD')
    $nyxPolicyReplayProgram = 'tests/nyx_typeahead_policy_generated.lpr'
    $nyxPolicyReplayExecutable = 'nyx_typeahead_policy_generated.exe'
    $nyxPolicyReplayPage = 'studio/web/typeahead-policy.html'

    if ($nyxWorkflow) {
      # The Pascal dispatcher owns semantic editing and exports its accepted
      # pair. Ordinary adapters consume that exact design; the separate compiler
      # executes its exact source and retained helper. No server is launched.
      $nyxPolicyRoot = Join-Path $nyxRoot 'build/typeahead-workflow/maintained'
      $nyxPolicyDefines += '-dNYX_TYPEAHEAD_WORKFLOW'
      $nyxPolicyReplayProgram = 'tests/nyx_typeahead_workflow_generated.lpr'
      $nyxPolicyReplayExecutable = 'nyx_typeahead_workflow_generated.exe'
      $nyxPolicyReplayPage = 'studio/web/typeahead-workflow.html'
    }
    $nyxPolicyNative = Join-Path $nyxPolicyRoot 'native'
    $nyxPolicyGenerated = Join-Path $nyxPolicyRoot 'generated'
    $nyxPolicyReplay = Join-Path $nyxPolicyRoot 'replay'
    $nyxPolicySource = [IO.Path]::GetFullPath($TypeAheadSourceDirectory)
    $nyxPolicyBrowser = Join-Path $nyxPolicyRoot 'browser'

    if ($BrowserOutput) { $nyxPolicyBrowser = [IO.Path]::GetFullPath($BrowserOutput) }

    if (-not (Test-Path -LiteralPath (Join-Path $nyxPolicySource 'nyx.generated.view.pas'))) {
      throw 'Supply the exact exported typeahead review through TypeAheadSourceDirectory'
    }
    New-Item -ItemType Directory -Force $nyxPolicyNative, $nyxPolicyGenerated,
      $nyxPolicyReplay, $nyxPolicyBrowser | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxPolicyPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxPolicyUnits = @('-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxPolicySource")
    $nyxPolicyChecks = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh')
    $nyxPolicyLcl = @("-Fu$nyxLazarus/lcl/units/$nyxPolicyPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxPolicyPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxPolicyPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxPolicyPlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxPolicyChecks + $nyxPolicyUnits + $nyxPolicyLcl +
      $nyxPolicyDefines + @("-FU$nyxPolicyNative", "-FE$nyxPolicyNative",
        'tests/nyx_typeahead_tests.lpr'))
    $nyxPolicyReviewArgs = @((Join-Path $nyxPolicyRoot 'english-native.png'), $nyxPolicyGenerated)
    & (Join-Path $nyxPolicyNative 'nyx_typeahead_tests.exe') @nyxPolicyReviewArgs

    if ($LASTEXITCODE -ne 0) { throw 'Saved typeahead control/source/history checks failed' }
    $nyxPolicyReplayFlags = $nyxPolicyChecks + $nyxPolicyUnits +
      @("-Fu$nyxPolicyGenerated", "-FU$nyxPolicyReplay", "-FE$nyxPolicyReplay")
    Invoke-NyxCompiler $nyxFpc ($nyxPolicyReplayFlags +
      @($nyxPolicyReplayProgram))
    & (Join-Path $nyxPolicyReplay $nyxPolicyReplayExecutable)

    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted saved-policy reconstruction failed' }
    Test-NyxCompilerTypes $nyxFpc $nyxPolicyReplayFlags @('typeahead_policy')

    if ($nyxWorkflow) {
      Invoke-NyxCompiler $nyxFpc ($nyxPolicyChecks + $nyxPolicyUnits +
        @("-FU$nyxPolicyReplay", "-FE$nyxPolicyReplay", 'tests/nyx_agent_collection_schema.lpr'))
      & (Join-Path $nyxPolicyReplay 'nyx_agent_collection_schema.exe')

      if ($LASTEXITCODE -ne 0) { throw 'Actual semantic search discovery checks failed' }
      Invoke-NyxCompiler $nyxFpc ($nyxPolicyChecks + $nyxPolicyUnits +
        @("-FU$nyxPolicyReplay", "-FE$nyxPolicyReplay", 'studio/nyx_studio_server.lpr'))
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxPolicyChecks + $nyxPolicyUnits + $nyxPolicyLcl +
      @("-FU$nyxPolicyNative", "-FE$nyxPolicyNative", 'studio/nyx_studio_native.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxPolicyBrowserFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js') +
      $nyxPolicyUnits + @("-Fu$nyxPolicyGenerated", "-FE$nyxPolicyBrowser")
    Invoke-NyxCompiler $nyxPas2js ($nyxPolicyBrowserFlags +
      $nyxPolicyDefines + @('tests/nyx_typeahead_tests.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxPolicyBrowserFlags +
      @($nyxPolicyReplayProgram))
    Test-NyxCompilerTypes $nyxPas2js $nyxPolicyBrowserFlags @('typeahead_policy')
    Invoke-NyxCompiler $nyxPas2js ($nyxPolicyBrowserFlags + @('studio/nyx_studio.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxPolicyBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/typeahead.html') -Destination $nyxPolicyBrowser
    Copy-Item -LiteralPath (Join-Path $nyxRoot $nyxPolicyReplayPage) -Destination $nyxPolicyBrowser
    Write-Host 'Saved typeahead qualified natively; browser consumers/Studios compile only.'
    exit 0
  }

  if ($Target -eq 'typeahead') {
    # Reproduce pinned Unicode data with Pascal, then compile the SAME semantic
    # review for both adapters. This target never starts/replaces a listener.
    $nyxTypeAheadRoot = Join-Path $nyxRoot 'build/typeahead/maintained'
    $nyxTypeAheadGenerator = Join-Path $nyxTypeAheadRoot 'generator'
    $nyxTypeAheadNative = Join-Path $nyxTypeAheadRoot 'lcl'
    $nyxTypeAheadSource = [IO.Path]::GetFullPath($TypeAheadSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxTypeAheadSource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP typeahead review first; see docs/collection-views.md'
    }
    New-Item -ItemType Directory -Force $nyxTypeAheadGenerator, $nyxTypeAheadNative | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', "-FU$nyxTypeAheadGenerator", "-FE$nyxTypeAheadGenerator", 'tools/nyx_unicode_casefold.lpr')
    $nyxTypeAheadTable = Join-Path $nyxTypeAheadGenerator 'casefold.inc'
    $nyxTypeAheadInputs = @('data/unicode/17.0.0/CaseFolding.txt',
      'data/unicode/17.0.0/LICENSE.txt', $nyxTypeAheadTable)
    & (Join-Path $nyxTypeAheadGenerator 'nyx_unicode_casefold.exe') @nyxTypeAheadInputs

    if ($LASTEXITCODE -ne 0) { throw 'Unicode case-fold generator failed' }

    if ((Get-FileHash -LiteralPath $nyxTypeAheadTable).Hash -ne
        (Get-FileHash -LiteralPath (Join-Path $nyxRoot 'src/nyx.text.casefold.inc')).Hash) {
      throw 'Checked-in Unicode table differs from pinned Pascal generation'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxTypeAheadPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxTypeAheadSource",
      "-Fu$nyxLazarus/lcl/units/$nyxTypeAheadPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxTypeAheadPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxTypeAheadPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxTypeAheadPlatform",
      "-FU$nyxTypeAheadNative", "-FE$nyxTypeAheadNative", 'tests/nyx_typeahead_tests.lpr')
    & (Join-Path $nyxTypeAheadNative 'nyx_typeahead_tests.exe') (Join-Path $nyxTypeAheadRoot 'english-native.png')

    if ($LASTEXITCODE -ne 0) { throw 'Native typeahead controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxBrowserDir = Join-Path $nyxTypeAheadRoot 'browser'

    if ($BrowserOutput) { $nyxBrowserDir = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxBrowserDir | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxTypeAheadSource", "-FE$nyxBrowserDir",
      'tests/nyx_typeahead_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/typeahead.html') -Destination $nyxBrowserDir
    exit 0
  }

  if ($Target -eq 'collection-refresh') {
    # Pascal owns refresh admission, actual widget/draft assertions and timing
    # gates. Consume exact already-exported MCP source; never change a design,
    # enrollment, running service, frozen payload or browser process here.
    $nyxRefreshRoot = Join-Path $nyxRoot 'build/collection-refresh/maintained'
    $nyxRefreshNative = Join-Path $nyxRefreshRoot 'native'
    $nyxRefreshMeasure = Join-Path $nyxRefreshRoot 'measure'
    $nyxRefreshBrowser = Join-Path $nyxRefreshRoot 'browser'
    $nyxRefreshSource = [IO.Path]::GetFullPath($GridSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxRefreshSource 'nyx.generated.view.pas'))) {
      throw 'Supply the exact previously authenticated table companion with GridSourceDirectory'
    }
    New-Item -ItemType Directory -Force $nyxRefreshNative,
      $nyxRefreshMeasure, $nyxRefreshBrowser | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxRefreshPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxRefreshUnits = @('-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxRefreshSource",
      "-Fu$nyxLazarus/lcl/units/$nyxRefreshPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxRefreshPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxRefreshPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxRefreshPlatform")
    $nyxRefreshFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      "-FU$nyxRefreshNative", "-FE$nyxRefreshNative") + $nyxRefreshUnits
    Invoke-NyxCompiler $nyxLclFpc ($nyxRefreshFlags + @('tests/nyx_collection_window_tests.lpr'))
    & (Join-Path $nyxRefreshNative 'nyx_collection_window_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Measured row geometry checks failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxRefreshFlags + @('tests/nyx_collection_refresh_tests.lpr'))
    & (Join-Path $nyxRefreshNative 'nyx_collection_refresh_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Incremental collection plan/controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxRefreshFlags + @('tests/nyx_collection_query_controls.lpr'))
    & (Join-Path $nyxRefreshNative 'nyx_collection_query_controls.exe') `
      (Join-Path $nyxRefreshRoot 'native-query.png')
    if ($LASTEXITCODE -ne 0) { throw 'Exact semantic table/query regression failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxRefreshFlags + @('tests/nyx_virtual_table_controls.lpr'))
    & (Join-Path $nyxRefreshNative 'nyx_virtual_table_controls.exe') `
      (Join-Path $nyxRefreshRoot 'native-virtual-table.png')
    if ($LASTEXITCODE -ne 0) { throw 'On-demand native table paint/read/lifetime checks failed' }
    # Timing excludes heap/debug instrumentation; keep checked ownership tests
    # above separate. Workload correctness gates run outside measured intervals.
    Invoke-NyxCompiler $nyxLclFpc (@('-B', '-Mdelphi', '-O2', '-Sa', '-Cr', '-Co', '-Ci',
      "-FU$nyxRefreshMeasure", "-FE$nyxRefreshMeasure") + $nyxRefreshUnits +
      @('tools/nyx_collection_view_benchmark.lpr'))
    $nyxRefreshSample = & (Join-Path $nyxRefreshMeasure 'nyx_collection_view_benchmark.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Mounted incremental measurement lost control values' }
    $nyxRefreshSample | Set-Content -LiteralPath (Join-Path $nyxRefreshRoot 'native-sample.csv') -Encoding utf8
    Write-Output $nyxRefreshSample
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxRefreshProgram in @('tests/nyx_collection_refresh_tests.lpr',
      'tests/nyx_collection_query_controls.lpr', 'tests/nyx_collection_window_tests.lpr',
      'tests/nyx_virtual_table_browser.lpr', 'tools/nyx_collection_view_benchmark.lpr',
      'studio/nyx_studio.lpr', 'studio/nyx_source_worker.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxRefreshSource", "-FE$nyxRefreshBrowser",
        $nyxRefreshProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxRefreshBrowser 'rtl.js')
    foreach ($nyxRefreshHost in @('collection-refresh.html', 'collection-query-controls.html',
      'collection-window.html', 'virtual-table.html', 'collection-view-benchmark.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot ('studio/web/' + $nyxRefreshHost)) `
        -Destination $nyxRefreshBrowser
    }
    Write-Host 'Incremental controls qualified natively; browser consumers staged without execution.'
    exit 0
  }

  if ($Target -eq 'data-read') {
    # Keep wall-clock samples separate from heap-traced ownership regressions.
    # Pascal owns the workload and exact-value checks. Staging never launches a
    # browser/service or changes an operator document/enrollment.
    $nyxDataReadRoot = Join-Path $nyxRoot 'build/data-index/maintained'
    $nyxDataReadNative = Join-Path $nyxDataReadRoot 'native'
    $nyxDataReadBrowser = Join-Path $nyxDataReadRoot 'browser'
    New-Item -ItemType Directory -Force $nyxDataReadNative, $nyxDataReadBrowser | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-O2', '-Sa', '-Cr', '-Co', '-Ci',
      '-Fusrc', "-FU$nyxDataReadNative", "-FE$nyxDataReadNative",
      'tools/nyx_data_read_bench.lpr')
    & (Join-Path $nyxDataReadNative 'nyx_data_read_bench.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Structured-value read benchmark lost exact values/order'
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', "-FE$nyxDataReadBrowser", 'tools/nyx_data_read_bench.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxDataReadBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/data-read.html') `
      -Destination $nyxDataReadBrowser
    Write-Host 'Native read sample completed; browser sample staged without execution.'
    exit 0
  }

  if ($Target -eq 'project-transactions') {
    # Pascal owns all admission/history, schema, ownership and compiled source
    # assertions. This target stages browser consumers and builds the explicit
    # MCP companion; it enrolls no client and starts no service or browser.
    $nyxTransactionRoot = Join-Path $nyxRoot 'build/combined-transactions/maintained'
    $nyxTransactionNative = Join-Path $nyxTransactionRoot 'native'
    $nyxTransactionBrowser = Join-Path $nyxTransactionRoot 'browser'
    $nyxTransactionSource = Join-Path $nyxTransactionRoot 'source'
    $nyxTransactionTools = Join-Path $nyxTransactionRoot 'tools'
    New-Item -ItemType Directory -Force $nyxTransactionNative,
      $nyxTransactionBrowser, $nyxTransactionSource, $nyxTransactionTools | Out-Null
    $nyxTransactionFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    Invoke-NyxCompiler $nyxFpc ($nyxTransactionFlags + @(
      "-FU$nyxTransactionNative", "-FE$nyxTransactionNative",
      'tests/nyx_agent_transaction_tests.lpr'))
    & (Join-Path $nyxTransactionNative 'nyx_agent_transaction_tests.exe') $nyxTransactionSource

    if ($LASTEXITCODE -ne 0) {
      throw 'Combined semantic transaction checks failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxTransactionFlags + @(
      "-Fu$nyxTransactionSource", "-FU$nyxTransactionNative", "-FE$nyxTransactionNative",
      'tests/nyx_transaction_generated_tests.lpr'))
    & (Join-Path $nyxTransactionNative 'nyx_transaction_generated_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Exact compiled transaction companion failed'
    }
    Invoke-NyxCompiler $nyxFpc ($nyxTransactionFlags + @(
      "-FU$nyxTransactionTools", "-FE$nyxTransactionTools", 'tools/nyx_grid_companion.lpr'))
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxTransactionProgram in @('tests/nyx_agent_transaction_tests.lpr',
      'tests/nyx_transaction_generated_tests.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxTransactionSource",
        "-FE$nyxTransactionBrowser", $nyxTransactionProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxTransactionBrowser 'rtl.js')
    foreach ($nyxTransactionHost in @('agent-transactions.html', 'transaction-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot ('studio/web/' + $nyxTransactionHost)) `
        -Destination $nyxTransactionBrowser
    }
    Write-Host 'Transaction consumers staged; browser execution remains a separate qualification.'
    Write-Host 'Grid companion needs an explicit owned backend/config and new export destination.'
    exit 0
  }

  if ($Target -eq 'collection-query-workflow') {
    # Pascal owns the query admission/context/history assertions. Compile the
    # explicit authenticated companions, but never infer authority to launch a
    # listener, enroll a client or mutate an operator project from a build.
    $nyxQueryWorkflowRoot = Join-Path $nyxRoot 'build/query-workflow/maintained'
    $nyxQueryWorkflowNative = Join-Path $nyxQueryWorkflowRoot 'native'
    $nyxQueryWorkflowBrowser = Join-Path $nyxQueryWorkflowRoot 'browser'
    $nyxQueryWorkflowTool = Join-Path $nyxQueryWorkflowRoot 'tool'
    New-Item -ItemType Directory -Force $nyxQueryWorkflowNative,
      $nyxQueryWorkflowBrowser, $nyxQueryWorkflowTool | Out-Null
    $nyxQueryWorkflowFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    Invoke-NyxCompiler $nyxFpc ($nyxQueryWorkflowFlags + @('-dNYX_QUERY_WORKFLOW_ONLY',
      "-FU$nyxQueryWorkflowNative", "-FE$nyxQueryWorkflowNative",
      'tests/nyx_agent_collection_tests.lpr'))
    & (Join-Path $nyxQueryWorkflowNative 'nyx_agent_collection_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Focused query admission/context/history checks failed'
    }
    foreach ($nyxQueryWorkflowProgram in @('tools/nyx_grid_companion.lpr',
      'tools/nyx_query_companion.lpr')) {
      Invoke-NyxCompiler $nyxFpc ($nyxQueryWorkflowFlags + @(
        "-FU$nyxQueryWorkflowTool", "-FE$nyxQueryWorkflowTool", $nyxQueryWorkflowProgram))
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-dNYX_QUERY_WORKFLOW_ONLY', '-Fusrc', '-Fustudio', '-Futests',
      "-FE$nyxQueryWorkflowBrowser", 'tests/nyx_agent_collection_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxQueryWorkflowBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/agent-collections.html') `
      -Destination $nyxQueryWorkflowBrowser
    Write-Host 'Query companions built; authenticated execution needs an explicit owned workspace/configuration.'
    Write-Host 'The browser consumer is staged, not executed; see docs/collection-queries.md.'
    exit 0
  }

  if ($Target -eq 'collection-query-editor') {
    # Public Nyx query controls consume the unchanged semantic companion and
    # ordinary paired Studio queue. This owns no server/project/enrollment.
    $nyxQueryEditorRoot = Join-Path $nyxRoot 'build/collection-query-editor/maintained'
    $nyxQueryEditorNative = Join-Path $nyxQueryEditorRoot 'native'
    $nyxQueryEditorBrowser = Join-Path $nyxQueryEditorRoot 'browser'
    $nyxQueryEditorSource = [IO.Path]::GetFullPath($GridSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxQueryEditorSource 'nyx.generated.view.pas'))) {
      throw 'Supply the authenticated grid companion with GridSourceDirectory'
    }
    New-Item -ItemType Directory -Force $nyxQueryEditorNative, $nyxQueryEditorBrowser | Out-Null
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxQueryEditorPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxQueryEditorFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxQueryEditorSource",
      "-FU$nyxQueryEditorNative", "-FE$nyxQueryEditorNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxQueryEditorFlags + @(
      "-Fu$nyxLazarus/lcl/units/$nyxQueryEditorPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxQueryEditorPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxQueryEditorPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxQueryEditorPlatform",
      'tests/nyx_collection_query_editor_controls.lpr'))
    & (Join-Path $nyxQueryEditorNative 'nyx_collection_query_editor_controls.exe') `
      (Join-Path $nyxQueryEditorSource 'nyx.generated.view.pas') `
      (Join-Path $nyxQueryEditorRoot 'studio-runtime') (Join-Path $nyxQueryEditorRoot 'captures')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native query authoring controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxQueryEditorSource", "-FE$nyxQueryEditorBrowser",
      'tests/nyx_collection_query_editor_controls.lpr')
    foreach ($nyxQueryEditorProgram in @('studio/nyx_source_worker.lpr', 'studio/nyx_studio.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', "-FE$nyxQueryEditorBrowser", $nyxQueryEditorProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxQueryEditorBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxQueryEditorSource 'nyx.generated.view.pas') `
      -Destination (Join-Path $nyxQueryEditorBrowser 'seed.pas.txt')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/collection-query-editor.html') `
      -Destination $nyxQueryEditorBrowser
    Invoke-NyxCompiler $nyxFpc ($nyxQueryEditorFlags + @('tests/nyx_browser_ready_capture.lpr'))
    Write-Host 'Query authoring controls built; execute the browser consumer over HTTP.'
    exit 0
  }

  if ($Target -eq 'collection-query') {
    # Portable policies, strict persistence/source and actual target controls
    # share the maintained toolchain. The UI consumer uses an already exported
    # authenticated companion; this build changes no project, service or config.
    $nyxQueryRoot = Join-Path $nyxRoot 'build/collection-query/maintained'
    $nyxQueryNative = Join-Path $nyxQueryRoot 'native'
    $nyxQueryLcl = Join-Path $nyxQueryRoot 'lcl'
    $nyxQueryBrowser = Join-Path $nyxQueryRoot 'browser'
    New-Item -ItemType Directory -Force $nyxQueryNative, $nyxQueryLcl, $nyxQueryBrowser | Out-Null
    $nyxQueryFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-FU$nyxQueryNative", "-FE$nyxQueryNative")
    Invoke-NyxCompiler $nyxFpc ($nyxQueryFlags + @('tests/nyx_collection_query_tests.lpr'))
    & (Join-Path $nyxQueryNative 'nyx_collection_query_tests.exe') `
      (Join-Path $nyxQueryNative 'nyx.query.fixture.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Portable query checks failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxQueryFlags + @('-dNYX_COMPILED_QUERY',
      "-Fu$nyxQueryNative", 'tests/nyx_collection_query_tests.lpr'))
    & (Join-Path $nyxQueryNative 'nyx_collection_query_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Compiled query builder checks failed' }
    $nyxQuerySource = [IO.Path]::GetFullPath($GridSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxQuerySource 'nyx.generated.view.pas'))) {
      throw 'Supply the previously authenticated grid companion with GridSourceDirectory'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxQueryPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxQuerySource", "-Fu$nyxLazarus/lcl/units/$nyxQueryPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxQueryPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxQueryPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxQueryPlatform", "-FU$nyxQueryLcl", "-FE$nyxQueryLcl",
      'tests/nyx_collection_query_controls.lpr')
    & (Join-Path $nyxQueryLcl 'nyx_collection_query_controls.exe') (Join-Path $nyxQueryRoot 'native.png')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native collection query controls failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-dNYX_COMPILED_QUERY',
      '-Fusrc', "-Fu$nyxQueryNative", "-FE$nyxQueryBrowser", 'tests/nyx_collection_query_tests.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc',
      "-Fu$nyxQuerySource", "-FE$nyxQueryBrowser", 'tests/nyx_collection_query_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxQueryBrowser 'rtl.js')
    foreach ($nyxQueryBootstrap in @('collection-query.html', 'collection-query-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxQueryBootstrap") -Destination $nyxQueryBrowser
    }
    Invoke-NyxCompiler $nyxFpc ($nyxQueryFlags + @('tests/nyx_browser_ready_capture.lpr'))
    Write-Host 'Queries built; execute both browser pages over HTTP for target evidence.'
    exit 0
  }

  if ($Target -eq 'grid-navigation') {
    # Semantic source remains the ordinary companion on both targets. Optional
    # explicit enrollment creates one owned project; no listener/enrollment or
    # active project is replaced. Each semantic phase keeps its own paired Undo.
    $nyxGridRoot = Join-Path $nyxRoot 'build/grid-navigation/maintained'
    $nyxGridTool = Join-Path $nyxGridRoot 'tool'
    $nyxGridNative = Join-Path $nyxGridRoot 'lcl'
    $nyxGridBrowser = Join-Path $nyxGridRoot 'browser'
    New-Item -ItemType Directory -Force $nyxGridTool, $nyxGridNative, $nyxGridBrowser | Out-Null
    if ($DesignerMCPConfig) {
      Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
        '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxGridTool", "-FE$nyxGridTool", 'tools/nyx_grid_companion.lpr')
      & (Join-Path $nyxGridTool 'nyx_grid_companion.exe') $DesignerMCPConfig $GridSourceDirectory
      if ($LASTEXITCODE -ne 0) { throw 'Authenticated semantic grid companion failed' }
    }
    $nyxGridSource = [IO.Path]::GetFullPath($GridSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxGridSource 'nyx.generated.view.pas'))) {
      throw 'Export the MCP grid companion or supply an explicit enrolled DesignerMCPConfig'
    }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxGridPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-Fu$nyxGridSource", "-Fu$nyxLazarus/lcl/units/$nyxGridPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxGridPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxGridPlatform", "-Fu$nyxLazarus/packager/units/$nyxGridPlatform",
      "-FU$nyxGridNative", "-FE$nyxGridNative", 'tests/nyx_grid_navigation_controls.lpr')
    & (Join-Path $nyxGridNative 'nyx_grid_navigation_controls.exe') (Join-Path $nyxGridRoot 'native.png')
    if ($LASTEXITCODE -ne 0) { throw 'Ordinary native grid navigation failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Futests',
      "-Fu$nyxGridSource", "-FE$nyxGridBrowser", 'tests/nyx_grid_navigation_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxGridBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/grid-navigation.html') -Destination $nyxGridBrowser
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-FU$nyxGridTool", "-FE$nyxGridTool", 'tests/nyx_browser_ready_capture.lpr')
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', "-FU$nyxGridTool", "-FE$nyxGridTool", 'tests/nyx_grid_navigation_observer.lpr')
    Write-Host 'Grid consumers built; actual HTTP browser execution remains explicit.'
    exit 0
  }

  if ($Target -eq 'tree-hierarchy') {
    # Pascal owns shared/runtime assertions. The optional managed view is the
    # exact bounded MCP export, never a rewritten demo. Native widgets run here;
    # browser artifacts stage only. Existing service processes remain untouched.
    $nyxTreeRoot = Join-Path $nyxRoot 'build/tree-disclosure/maintained'
    $nyxTreeNative = Join-Path $nyxTreeRoot 'native'
    $nyxTreeBrowser = Join-Path $nyxTreeRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxTreePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxTreeSourceFlags = @()

    if ($TreeSourceDirectory) {
      $nyxTreeSource = [IO.Path]::GetFullPath($TreeSourceDirectory)
      if (-not (Test-Path -LiteralPath (Join-Path $nyxTreeSource 'nyx.generated.view.pas'))) {
        throw 'Supply the exact semantic tree export with TreeSourceDirectory'
      }
      $nyxTreeSourceFlags = @("-Fu$nyxTreeSource", '-dNYX_COMPILED_TREE')
    }
    New-Item -ItemType Directory -Force $nyxTreeNative | Out-Null
    $nyxTreeFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxLazarus/lcl/units/$nyxTreePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxTreePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxTreePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxTreePlatform", "-FU$nyxTreeNative", "-FE$nyxTreeNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxTreeFlags + $nyxTreeSourceFlags + @('tests/nyx_tree_hierarchy_tests.lpr'))
    & (Join-Path $nyxTreeNative 'nyx_tree_hierarchy_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Native tree hierarchy consumer failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    if ($BrowserOutput) { $nyxTreeBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxTreeBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', '-Fustudio', "-FE$nyxTreeBrowser") + $nyxTreeSourceFlags + @('tests/nyx_tree_hierarchy_tests.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxTreeBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/tree-hierarchy.html') -Destination $nyxTreeBrowser
    exit 0
  }





  if ($Target -eq 'resource-mappings') {
    # Pascal owns saved/source admission, ordinary tables and semantic edits.
    # Fresh suspended runtime and independent outputs never launch listeners,
    # refresh enrollment or replace the observing Studio.
    $nyxMappingRoot = Join-Path $nyxRoot 'build/resource-mappings/maintained'
    foreach ($nyxMappingDirectory in @('native', 'browser', 'source', 'studio-native',
      'backend', 'studio-browser', 'worker')) {
      New-Item -ItemType Directory -Force (Join-Path $nyxMappingRoot $nyxMappingDirectory) | Out-Null
    }
    $nyxMappingSource = Join-Path $nyxMappingRoot 'source'
    $nyxMappingNative = Join-Path $nyxMappingRoot 'native'
    $nyxMappingBrowser = Join-Path $nyxMappingRoot 'browser'
    $nyxMappingRuntime = Join-Path $nyxMappingRoot ('runtime-' + [Guid]::NewGuid().ToString('N'))
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxMappingPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxMappingFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxMappingPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxMappingPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxMappingPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxMappingPlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxMappingFlags +
      @("-FU$nyxMappingNative", "-FE$nyxMappingNative", 'tests/nyx_resource_mapping_controls.lpr'))
    & (Join-Path $nyxMappingNative 'nyx_resource_mapping_controls.exe') $nyxMappingSource $nyxMappingRuntime
    if ($LASTEXITCODE -ne 0) { throw 'Saved resource mapping/control/semantic checks failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxMappingFlags +
      @("-FU$nyxMappingNative", "-FE$nyxMappingNative", 'tests/nyx_resource_live_controls.lpr'))
    & (Join-Path $nyxMappingNative 'nyx_resource_live_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Coordinated application resource/control journey failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxMappingBrowser",
      'tests/nyx_resource_mapping_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMappingBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-mappings.html') -Destination $nyxMappingBrowser
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', "-FE$nyxMappingBrowser", 'tests/nyx_resource_live_controls.lpr')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-live.html') -Destination $nyxMappingBrowser
    foreach ($nyxMappingScope in @('full', 'page', 'reusable')) {
      $nyxMappingEmitted = Join-Path $nyxMappingSource $nyxMappingScope
      $nyxMappingCompiled = Join-Path $nyxMappingRoot "generated-$nyxMappingScope"
      $nyxMappingWeb = Join-Path $nyxMappingCompiled 'browser'
      New-Item -ItemType Directory -Force $nyxMappingCompiled, $nyxMappingWeb | Out-Null
      Invoke-NyxCompiler $nyxLclFpc ($nyxMappingFlags +
        @("-FU$nyxMappingCompiled", "-FE$nyxMappingCompiled", "-Fu$nyxMappingEmitted",
        'tests/nyx_resource_mapping_generated.lpr'))
      & (Join-Path $nyxMappingCompiled 'nyx_resource_mapping_generated.exe')
      if ($LASTEXITCODE -ne 0) { throw "Exact $nyxMappingScope resource builder/control checks failed" }
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', "-Fu$nyxMappingEmitted", "-FE$nyxMappingWeb",
        'tests/nyx_resource_mapping_generated.lpr')
      Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxMappingWeb 'rtl.js')
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-mappings-generated.html') -Destination $nyxMappingWeb
    }
    $nyxMappingStudio = Join-Path $nyxMappingRoot 'studio-native'
    Invoke-NyxCompiler $nyxLclFpc ($nyxMappingFlags +
      @("-FU$nyxMappingStudio", "-FE$nyxMappingStudio", 'studio/nyx_studio_native.lpr'))
    $nyxMappingBackend = Join-Path $nyxMappingRoot 'backend'
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxMappingBackend", "-FE$nyxMappingBackend", 'studio/nyx_studio_server.lpr')
    foreach ($nyxMappingBuild in @(
      @{Output='worker'; Program='studio/nyx_source_worker.lpr'},
      @{Output='studio-browser'; Program='studio/nyx_studio.lpr'})) {
      $nyxMappingOutput = Join-Path $nyxMappingRoot $nyxMappingBuild.Output
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', "-FE$nyxMappingOutput", $nyxMappingBuild.Program)
    }
    Write-Host 'Saved mappings/joint native loading/source/semantic checks passed; browser execution remains separate.'
    exit 0
  }

  if ($Target -eq 'resource-publication') {
    # Pascal qualifies prepared resource datasets and mounted tables. Isolated
    # artifacts neither launch/replace listeners nor refresh client enrollment.
    $nyxPublicationRoot = Join-Path $nyxRoot 'build/resource-publication/maintained'
    foreach ($nyxPublicationDirectory in @('native', 'browser', 'studio-native',
      'backend', 'studio-browser', 'worker')) {
      New-Item -ItemType Directory -Force (Join-Path $nyxPublicationRoot $nyxPublicationDirectory) | Out-Null
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxPublicationPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxPublicationFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxPublicationPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxPublicationPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxPublicationPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxPublicationPlatform")
    $nyxPublicationNative = Join-Path $nyxPublicationRoot 'native'
    Invoke-NyxCompiler $nyxLclFpc ($nyxPublicationFlags +
      @("-FU$nyxPublicationNative", "-FE$nyxPublicationNative",
      'tests/nyx_collection_publication_tests.lpr'))
    & (Join-Path $nyxPublicationNative 'nyx_collection_publication_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Coordinated resource dataset/control checks failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxPublicationBrowser = Join-Path $nyxPublicationRoot 'browser'
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', "-FE$nyxPublicationBrowser",
      'tests/nyx_collection_publication_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxPublicationBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-publication.html') `
      -Destination $nyxPublicationBrowser
    $nyxPublicationStudio = Join-Path $nyxPublicationRoot 'studio-native'
    Invoke-NyxCompiler $nyxLclFpc ($nyxPublicationFlags +
      @("-FU$nyxPublicationStudio", "-FE$nyxPublicationStudio", 'studio/nyx_studio_native.lpr'))
    $nyxPublicationBackend = Join-Path $nyxPublicationRoot 'backend'
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxPublicationBackend", "-FE$nyxPublicationBackend",
      'studio/nyx_studio_server.lpr')
    foreach ($nyxPublicationBuild in @(
      @{Output='worker'; Program='studio/nyx_source_worker.lpr'},
      @{Output='studio-browser'; Program='studio/nyx_studio.lpr'})) {
      $nyxPublicationOutput = Join-Path $nyxPublicationRoot $nyxPublicationBuild.Output
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', "-FE$nyxPublicationOutput", $nyxPublicationBuild.Program)
    }
    Write-Host 'Prepared publication/native controls passed; browser/observing execution remains separate.'
    exit 0
  }

  if ($Target -eq 'application-resources') {
    # Pascal qualifies full applications, using an existing read-only HTTP health
    # endpoint. Independent outputs stage without listener/browser launches.
    $nyxAppResourceRoot = Join-Path $nyxRoot 'build/application-resources/maintained'
    foreach ($nyxAppResourceDirectory in @('native', 'browser', 'studio-native',
      'backend', 'studio-browser', 'worker')) {
      New-Item -ItemType Directory -Force (Join-Path $nyxAppResourceRoot $nyxAppResourceDirectory) | Out-Null
    }
    $nyxAppResourceNative = Join-Path $nyxAppResourceRoot 'native'
    $nyxAppResourceBrowser = Join-Path $nyxAppResourceRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxAppResourcePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxAppResourceFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxAppResourcePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxAppResourcePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxAppResourcePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxAppResourcePlatform")
    Invoke-NyxCompiler $nyxLclFpc ($nyxAppResourceFlags +
      @("-FU$nyxAppResourceNative", "-FE$nyxAppResourceNative",
      'tests/nyx_application_resource_controls.lpr'))
    & (Join-Path $nyxAppResourceNative 'nyx_application_resource_controls.exe') $HttpURL
    if ($LASTEXITCODE -ne 0) { throw 'Application resource controls/lifetime failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxAppResourceBrowser",
      'tests/nyx_application_resource_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxAppResourceBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/application-resources.html') `
      -Destination $nyxAppResourceBrowser
    $nyxAppResourceLCL = Join-Path $nyxAppResourceRoot 'studio-native'
    Invoke-NyxCompiler $nyxLclFpc ($nyxAppResourceFlags +
      @("-FU$nyxAppResourceLCL", "-FE$nyxAppResourceLCL", 'studio/nyx_studio_native.lpr'))
    $nyxAppResourceBackend = Join-Path $nyxAppResourceRoot 'backend'
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxAppResourceBackend", "-FE$nyxAppResourceBackend",
      'studio/nyx_studio_server.lpr')
    foreach ($nyxAppResourceBuild in @(
      @{Output='worker'; Program='studio/nyx_source_worker.lpr'},
      @{Output='studio-browser'; Program='studio/nyx_studio.lpr'})) {
      $nyxAppResourceOutput = Join-Path $nyxAppResourceRoot $nyxAppResourceBuild.Output
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', "-FE$nyxAppResourceOutput", $nyxAppResourceBuild.Program)
    }
    Write-Host 'Application/native checks passed; browser/cache/observing execution remains separate.'
    exit 0
  }

  if ($Target -eq 'resource-runtime') {
    # Compile the actual producer wrappers and reusable HTTP qualification.
    # The Pascal tools own all semantic, protocol, process and expiry behavior.
    $nyxProducerRoot = Join-Path $nyxRoot 'build/resource-runtime/maintained'
    $nyxProducerBackend = Join-Path $nyxProducerRoot 'backend'
    $nyxProducerClient = Join-Path $nyxProducerRoot 'http-client'
    $nyxProducerWrapper = Join-Path $nyxProducerRoot 'wrappers'
    New-Item -ItemType Directory -Force -Path $nyxProducerBackend,
      $nyxProducerClient, $nyxProducerWrapper | Out-Null
    $nyxProducerFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests')
    Invoke-NyxCompiler $nyxFpc ($nyxProducerFlags + @("-FU$nyxProducerBackend",
      "-FE$nyxProducerBackend", 'tests/nyx_resource_runtime_server.lpr'))
    Invoke-NyxCompiler $nyxFpc ($nyxProducerFlags + @("-FU$nyxProducerWrapper",
      "-FE$nyxProducerWrapper", 'tests/nyx_resource_preview_builds.lpr'))
    $nyxProducerNewRuntime = Join-Path $nyxProducerRoot ('wrapper-' + [Guid]::NewGuid().ToString('N'))
    & (Join-Path $nyxProducerWrapper 'nyx_resource_preview_builds.exe') $nyxRoot `
      (Join-Path $nyxRoot '.local/toolchain.json') $nyxProducerNewRuntime 'i386-win32'
    if ($LASTEXITCODE -ne 0) { throw 'Actual resource producer wrappers failed compilation' }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    Invoke-NyxCompiler $nyxLclFpc ($nyxProducerFlags + @("-FU$nyxProducerClient",
      "-FE$nyxProducerClient", "-Fu$nyxLazarus/lcl/units/i386-win32",
      "-Fu$nyxLazarus/lcl/units/i386-win32/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/i386-win32",
      "-Fu$nyxLazarus/packager/units/i386-win32", 'tests/nyx_resource_runtime_http.lpr'))
    if ($ResourceRuntimeHome) {
      & (Join-Path $nyxProducerClient 'nyx_resource_runtime_http.exe') $HttpURL `
        $ResourceRuntimeHome (Join-Path $nyxRoot '.local/toolchain.json')
      if ($LASTEXITCODE -ne 0) { throw 'Authenticated resource producer qualification failed' }
    }
    Write-Host 'Resource producer tools and wrappers passed; HTTP execution requires an explicit isolated runtime.'
    exit 0
  }

  if ($Target -eq 'resource-workflow') {
    # Pascal owns semantic mutations, bounded queries, exact emitted source and
    # history. The actual MCP engine stays suspended in a NEW owned runtime.
    # This gate neither starts a listener/browser nor enrolls a client.
    $nyxResourceFlowRoot = Join-Path $nyxRoot 'build/resource-workflow/maintained'
    $nyxResourceFlowNative = Join-Path $nyxResourceFlowRoot 'native'
    $nyxResourceFlowBrowser = Join-Path $nyxResourceFlowRoot 'browser'
    $nyxResourceFlowSource = Join-Path $nyxResourceFlowRoot 'source'
    New-Item -ItemType Directory -Force -Path $nyxResourceFlowNative,
      $nyxResourceFlowBrowser, $nyxResourceFlowSource | Out-Null
    $nyxResourceFlowRuntime = Join-Path $nyxResourceFlowRoot ('runtime-' + [Guid]::NewGuid().ToString('N'))
    $nyxResourceFlowFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxResourceFlowNative", "-FE$nyxResourceFlowNative")
    Invoke-NyxCompiler $nyxFpc ($nyxResourceFlowFlags + @('tests/nyx_agent_resource_tests.lpr'))
    & (Join-Path $nyxResourceFlowNative 'nyx_agent_resource_tests.exe') $nyxResourceFlowRuntime `
      (Join-Path $nyxResourceFlowSource 'nyx.generated.view.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Semantic Resources qualification failed' }
    Invoke-NyxCompiler $nyxFpc ($nyxResourceFlowFlags +
      @("-Fu$nyxResourceFlowSource", 'tests/nyx_agent_resource_generated.lpr'))
    & (Join-Path $nyxResourceFlowNative 'nyx_agent_resource_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact semantic Resources builder failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxResourceFlowProgram in @('tests/nyx_agent_resource_tests.lpr',
      'tests/nyx_agent_resource_generated.lpr')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxResourceFlowSource",
        "-FE$nyxResourceFlowBrowser", $nyxResourceFlowProgram)
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxResourceFlowBrowser 'rtl.js')
    foreach ($nyxResourceFlowHost in @('resource-workflow.html', 'resource-workflow-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxResourceFlowHost") `
        -Destination $nyxResourceFlowBrowser
    }
    # Rebuild ordinary consumers of the public typed patch. Keep their unit and
    # executable directories separate from the suspended-engine qualification.
    $nyxResourceFlowLCL = Join-Path $nyxResourceFlowRoot 'studio-native'
    $nyxResourceFlowBackend = Join-Path $nyxResourceFlowRoot 'backend'
    $nyxResourceFlowStudio = Join-Path $nyxResourceFlowRoot 'studio-browser'
    $nyxResourceFlowWorker = Join-Path $nyxResourceFlowRoot 'worker'
    New-Item -ItemType Directory -Force -Path $nyxResourceFlowLCL,
      $nyxResourceFlowBackend, $nyxResourceFlowStudio, $nyxResourceFlowWorker | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxResourceFlowPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-Fu$nyxLazarus/lcl/units/$nyxResourceFlowPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxResourceFlowPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxResourceFlowPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxResourceFlowPlatform",
      "-FU$nyxResourceFlowLCL", "-FE$nyxResourceFlowLCL", 'studio/nyx_studio_native.lpr')
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxResourceFlowBackend", "-FE$nyxResourceFlowBackend",
      'studio/nyx_studio_server.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxResourceFlowWorker", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxResourceFlowStudio", 'studio/nyx_studio.lpr')
    Write-Host 'Resource semantic/source checks passed; authenticated HTTP/browser/observing qualification remains separate.'
    exit 0
  }

  if ($Target -eq 'resource-authoring') {
    # Public Pascal fixtures own file contents, ordinary native Studio input,
    # paired admission/history and exact source replay. This gate never starts
    # a server, browser, OS chooser or active semantic project mutation.
    $nyxAuthorRoot = Join-Path $nyxRoot 'build/resource-editor'
    $nyxAuthorNative = Join-Path $nyxAuthorRoot 'native'
    $nyxAuthorBrowser = Join-Path $nyxAuthorRoot 'browser'
    $nyxAuthorGenerated = Join-Path $nyxAuthorRoot 'generated'
    $nyxAuthorWorkspace = Join-Path $nyxAuthorRoot 'workspace'
    $nyxAuthorWorker = Join-Path $nyxAuthorRoot 'worker'
    $nyxAuthorBackend = Join-Path $nyxAuthorRoot 'backend'
    $nyxAuthorStudio = Join-Path $nyxAuthorRoot 'consumer-browser'
    New-Item -ItemType Directory -Force -Path $nyxAuthorNative, $nyxAuthorBrowser,
      $nyxAuthorGenerated, $nyxAuthorWorkspace, $nyxAuthorWorker,
      $nyxAuthorBackend, $nyxAuthorStudio | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxAuthorPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxAuthorFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxAuthorPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxAuthorPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxAuthorPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxAuthorPlatform",
      "-FU$nyxAuthorNative", "-FE$nyxAuthorNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxAuthorFlags + @('tests/nyx_resource_authoring_controls.lpr'))
    & (Join-Path $nyxAuthorNative 'nyx_resource_authoring_controls.exe') `
      (Join-Path $nyxAuthorGenerated 'nyx.generated.view.pas') `
      (Join-Path $nyxAuthorRoot 'desktop.png') (Join-Path $nyxAuthorRoot 'compact-native.png')
    if ($LASTEXITCODE -ne 0) { throw 'Common Resources native Studio qualification failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxAuthorFlags +
      @("-Fu$nyxAuthorGenerated", 'tests/nyx_resource_authoring_generated.lpr'))
    & (Join-Path $nyxAuthorNative 'nyx_resource_authoring_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact resource authoring source failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxAuthorFlags + @('studio/nyx_studio_native.lpr'))
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxAuthorWorkspace", "-FE$nyxAuthorWorkspace",
      'tests/nyx_workspace_tests.lpr')
    & (Join-Path $nyxAuthorWorkspace 'nyx_workspace_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Resource preference/project regression failed' }
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-FU$nyxAuthorBackend", "-FE$nyxAuthorBackend",
      'studio/nyx_studio_server.lpr')
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxAuthorBrowserFlags = @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxAuthorBrowser")
    Invoke-NyxCompiler $nyxPas2js ($nyxAuthorBrowserFlags + @('tests/nyx_resource_authoring_controls.lpr'))
    Invoke-NyxCompiler $nyxPas2js ($nyxAuthorBrowserFlags +
      @("-Fu$nyxAuthorGenerated", 'tests/nyx_resource_authoring_generated.lpr'))
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxAuthorWorker", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxAuthorStudio", 'studio/nyx_studio.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxAuthorBrowser 'rtl.js')
    foreach ($nyxAuthorHost in @('resource-authoring.html', 'resource-authoring-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxAuthorHost") -Destination $nyxAuthorBrowser
    }
    Write-Host 'Resources and both Studios built; browser/trusted chooser/observing execution remains separate.'
    exit 0
  }

  if ($Target -eq 'resource-loading') {
    # Pascal owns hosted policy, actual bytes/control publication, worker
    # retirement and assertions. Reuse an existing health endpoint read-only;
    # this gate never launches a server/browser or authors an active project.
    $nyxLoaderRoot = Join-Path $nyxRoot 'build/resource-loader'
    $nyxLoaderNative = Join-Path $nyxLoaderRoot 'native'
    $nyxLoaderBrowser = Join-Path $nyxLoaderRoot 'browser'
    New-Item -ItemType Directory -Force -Path $nyxLoaderNative, $nyxLoaderBrowser | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLoaderPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxLoaderNative", "-FE$nyxLoaderNative",
      "-Fu$nyxLazarus/lcl/units/$nyxLoaderPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxLoaderPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxLoaderPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxLoaderPlatform",
      'tests/nyx_resource_loader_tests.lpr')
    & (Join-Path $nyxLoaderNative 'nyx_resource_loader_tests.exe') `
      ($HttpURL.TrimEnd('/') + '/api/health') (Join-Path $nyxLoaderRoot 'desktop.png')

    if ($LASTEXITCODE -ne 0) {
      throw 'Hosted resource/native control qualification failed'
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', "-FE$nyxLoaderBrowser", 'tests/nyx_resource_loader_tests.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxLoaderBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-loading.html') -Destination $nyxLoaderBrowser
    Write-Host 'Hosted browser/control artifacts staged; browser/cache/observing execution remains separate.'
    exit 0
  }

  if ($Target -eq 'resources') {
    # Pascal owns qualification data, cache policy/storage checks, real controls,
    # exact source output and assertions. This gate stages browser artifacts.
    # It never launches a listener/browser or authors an active MCP project.
    $nyxResourceRoot = Join-Path $nyxRoot 'build/resources'
    $nyxResourceNative = Join-Path $nyxResourceRoot 'native'
    $nyxResourceBrowser = Join-Path $nyxResourceRoot 'browser'
    $nyxResourceGenerated = Join-Path $nyxResourceRoot 'generated'
    New-Item -ItemType Directory -Force -Path $nyxResourceNative, $nyxResourceBrowser, $nyxResourceGenerated | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxResourcePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxResourceFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxResourcePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxResourcePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxResourcePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxResourcePlatform",
      "-FU$nyxResourceNative", "-FE$nyxResourceNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxResourceFlags + @('tests/nyx_resource_controls.lpr'))
    & (Join-Path $nyxResourceNative 'nyx_resource_controls.exe') `
      (Join-Path $nyxResourceGenerated 'nyx.generated.view.pas') (Join-Path $nyxResourceRoot 'desktop.png')
    if ($LASTEXITCODE -ne 0) { throw 'Resource binding/cache/native controls failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxResourceFlags + @("-Fu$nyxResourceGenerated", 'tests/nyx_resource_generated.lpr'))
    & (Join-Path $nyxResourceNative 'nyx_resource_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled resource source failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-FE$nyxResourceBrowser", 'tests/nyx_resource_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', "-Fu$nyxResourceGenerated", "-FE$nyxResourceBrowser", 'tests/nyx_resource_generated.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxResourceBrowser 'rtl.js')
    foreach ($nyxResourceHost in @('resource-bindings.html', 'resource-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxResourceHost") -Destination $nyxResourceBrowser
    }
    Write-Host 'Resource counterparts staged; browser/cache/HTTP/observing execution remains separate.'
    exit 0
  }

  if ($Target -eq 'resource-images') {
    $nyxResourceImageRoot = Join-Path $nyxRoot 'build/resource-images'
    $nyxResourceImageNative = Join-Path $nyxResourceImageRoot 'native'
    $nyxResourceImageBrowser = Join-Path $nyxResourceImageRoot 'browser'
    $nyxResourceImageGenerated = Join-Path $nyxResourceImageRoot 'generated'
    New-Item -ItemType Directory -Force -Path $nyxResourceImageNative,
      $nyxResourceImageBrowser, $nyxResourceImageGenerated | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxResourceImagePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxResourceImageFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$ImageSourceDirectory",
      "-Fu$nyxRoot/build/image-presentation/fixtures",
      "-Fu$nyxLazarus/lcl/units/$nyxResourceImagePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxResourceImagePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxResourceImagePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxResourceImagePlatform",
      "-FU$nyxResourceImageNative", "-FE$nyxResourceImageNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxResourceImageFlags + @('tests/nyx_resource_image_controls.lpr'))
    if ($ResourceImageStage -ne '') {
      $nyxResourceImageChild = [IO.Path]::GetFullPath($ResourceImageStage)
      $nyxResourceImageBuild = [IO.Path]::GetFullPath((Join-Path $nyxRoot 'build'))
      if (-not $nyxResourceImageChild.StartsWith($nyxResourceImageBuild +
          [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase) -or
        (Split-Path $nyxResourceImageChild -Leaf) -notmatch '^resource-images-[a-f0-9]{32}$' -or
        -not (Test-Path -LiteralPath $nyxResourceImageChild -PathType Container)) {
        throw 'Resource image qualification requires an explicitly owned static child under build'
      }
      & (Join-Path $nyxResourceImageNative 'nyx_resource_image_controls.exe') $HttpURL `
        (Join-Path $nyxResourceImageGenerated 'nyx.generated.resource.images.pas') `
        (Join-Path $nyxResourceImageRoot 'native.png') $nyxResourceImageChild
      if ($LASTEXITCODE -ne 0) { throw 'Actual HTTP/native image resource controls failed' }
      Invoke-NyxCompiler $nyxLclFpc ($nyxResourceImageFlags +
        @("-Fu$nyxResourceImageGenerated", 'tests/nyx_resource_image_generated.lpr'))
      & (Join-Path $nyxResourceImageNative 'nyx_resource_image_generated.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Exact compiled image resource source failed' }
    }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$ImageSourceDirectory",
      "-Fu$nyxRoot/build/image-presentation/fixtures", "-FE$nyxResourceImageBrowser",
      'tests/nyx_resource_image_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxResourceImageBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-images.html') `
      -Destination $nyxResourceImageBrowser
    if ($ResourceImageStage -ne '') {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
        '-Fusrc', "-Fu$nyxResourceImageGenerated", "-FE$nyxResourceImageBrowser",
        'tests/nyx_resource_image_generated.lpr')
      Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/resource-images-generated.html') `
        -Destination $nyxResourceImageBrowser
    }
    Write-Host 'Resource image counterpart staged; HTTP browser/observing execution is separate.'
    exit 0
  }

  if ($Target -eq 'resource-workbench') {
    # Reuse the authenticated English companion. Pascal owns the common form's
    # real Studio import/binding, paired history and source checks.
    # Independent artifacts stage only; no service/browser/OS chooser launches.
    $nyxResourceRoot = Join-Path $nyxRoot 'build/resource-workbench'
    # Profile runs never overwrite the qualified normal source/capture closure.
    # This directory and mode affect orchestration only; Pascal owns the workload.

    if ($ResourceWorkbenchProfile -ne 'none') {
      $nyxResourceRoot = Join-Path $nyxResourceRoot ('profile-' + $ResourceWorkbenchProfile)
    }
    $nyxResourceSeed = [IO.Path]::GetFullPath($ResourceSourceDirectory)

    if (-not (Test-Path -LiteralPath (Join-Path $nyxResourceSeed 'nyx.generated.view.pas'))) {
      throw 'Export the English Resource workbench through an owned MCP review first'
    }
    $nyxResourceNativeName = 'native'
    $nyxResourceBuildFlags = @('-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh')

    if ($NativeStudioConfiguration -eq 'release') {
      $nyxResourceNativeName = 'native-release'
      $nyxResourceBuildFlags = @('-Sa', '-Cr', '-Co', '-Ci', '-O2', '-Xs')
    }

    if ($ResourceWorkbenchProfile -ne 'none') {
      $nyxResourceBuildFlags += '-dNYX_STUDIO_PROFILE'
    }
    $nyxResourceNative = Join-Path $nyxResourceRoot $nyxResourceNativeName
    $nyxResourceBrowser = Join-Path $nyxResourceRoot 'browser'
    $nyxResourceGenerated = Join-Path $nyxResourceRoot 'generated'
    New-Item -ItemType Directory -Force -Path $nyxResourceNative, $nyxResourceBrowser, $nyxResourceGenerated | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxResourcePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxResourceFlags = @('-B', '-Mdelphi') + $nyxResourceBuildFlags + @(
      '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxResourcePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxResourcePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxResourcePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxResourcePlatform",
      "-FU$nyxResourceNative", "-FE$nyxResourceNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxResourceFlags + @("-Fu$nyxResourceSeed", 'tests/nyx_resource_workbench_controls.lpr'))
    $nyxResourceRun = @((Join-Path $nyxResourceGenerated 'nyx.generated.view.pas'),
      (Join-Path $nyxResourceRoot 'desktop.png'))

    if ($ResourceWorkbenchProfile -eq 'open') {
      $nyxResourceRun += '--profile-open'
    }
    & (Join-Path $nyxResourceNative 'nyx_resource_workbench_controls.exe') $nyxResourceRun

    if ($LASTEXITCODE -ne 0) { throw 'Native resource workbench authoring failed' }

    if ($ResourceWorkbenchProfile -eq 'open') {
      Write-Host 'Native first-open timing completed. Full authoring/source and browser qualification are separate.'
      exit 0
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxResourceFlags + @("-Fu$nyxResourceGenerated", 'tests/nyx_resource_workbench_generated.lpr'))
    & (Join-Path $nyxResourceNative 'nyx_resource_workbench_generated.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted resource workbench authoring failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxResourceSeed",
      "-FE$nyxResourceBrowser", 'tests/nyx_resource_workbench_browser.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxResourceGenerated", "-FE$nyxResourceBrowser",
      'tests/nyx_resource_workbench_generated.lpr')
    # The ordinary browser controller prepares Apply/Undo through this Pascal
    # worker. Stage its current matched closure, not only the visible test page.
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxResourceBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxResourceBrowser 'rtl.js')
    foreach ($nyxResourceHost in @('resource-workbench.html', 'resource-workbench-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxResourceHost") -Destination $nyxResourceBrowser
    }
    Write-Host 'Resource workbench authoring staged; browser/trusted chooser execution remains separate.'
    exit 0
  }

  if ($Target -eq 'resource-image-authoring') {
    # Reuse the authenticated English companion. Pascal owns the common form's
    # migration, real Studio import/binding, paired history and source checks.
    # Independent artifacts stage only; no service/browser/OS chooser launches.
    $nyxImageRoot = Join-Path $nyxRoot 'build/resource-image-authoring'
    $nyxImageSeed = [IO.Path]::GetFullPath($ImageSourceDirectory)
    $nyxImageFixtures = Join-Path $nyxRoot 'build/image-presentation/fixtures'
    if (-not (Test-Path -LiteralPath (Join-Path $nyxImageSeed 'nyx.generated.view.pas'))) {
      throw 'Export the English Image workshop through an owned MCP review first'
    }
    if (-not (Test-Path -LiteralPath (Join-Path $nyxImageFixtures 'nyx.image.fixtures.pas'))) {
      throw 'Run the portable image-presentation prerequisite to create the Pascal raster fixtures'
    }
    $nyxImageNative = Join-Path $nyxImageRoot 'native'
    $nyxImageBrowser = Join-Path $nyxImageRoot 'browser'
    $nyxImageGenerated = Join-Path $nyxImageRoot 'generated'
    New-Item -ItemType Directory -Force -Path $nyxImageNative, $nyxImageBrowser, $nyxImageGenerated | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxImagePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxImageFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImageFixtures",
      "-Fu$nyxLazarus/lcl/units/$nyxImagePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxImagePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxImagePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxImagePlatform",
      "-FU$nyxImageNative", "-FE$nyxImageNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @("-Fu$nyxImageSeed", 'tests/nyx_resource_image_authoring_controls.lpr'))
    & (Join-Path $nyxImageNative 'nyx_resource_image_authoring_controls.exe') `
      (Join-Path $nyxImageGenerated 'nyx.generated.view.pas') `
      (Join-Path $nyxImageRoot 'desktop.png')
    if ($LASTEXITCODE -ne 0) { throw 'Native resource image authoring failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @("-Fu$nyxImageGenerated", 'tests/nyx_resource_image_authoring_generated.lpr'))
    & (Join-Path $nyxImageNative 'nyx_resource_image_authoring_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted resource image authoring failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImageSeed", "-Fu$nyxImageFixtures",
      "-FE$nyxImageBrowser", 'tests/nyx_resource_image_authoring_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', "-Fu$nyxImageGenerated", "-Fu$nyxImageFixtures", "-FE$nyxImageBrowser",
      'tests/nyx_resource_image_authoring_generated.lpr')
    # The ordinary browser controller prepares Apply/Undo through this Pascal
    # worker. Stage its current matched closure, not only the visible test page.
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxImageBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxImageBrowser 'rtl.js')
    foreach ($nyxImageHost in @('resource-image-authoring.html', 'resource-image-authoring-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxImageHost") -Destination $nyxImageBrowser
    }
    Write-Host 'Resource image authoring staged; browser/trusted chooser execution remains separate.'
    exit 0
  }

  if ($Target -eq 'image-authoring') {
    # Reuse the exact authenticated English image companion. Pascal owns file
    # admission, ordinary Studio interaction, history and emitted-source checks.
    # Independent artifacts stage only; no service/browser/OS chooser launches.
    $nyxImageRoot = Join-Path $nyxRoot 'build/image-authoring'
    $nyxImageSeed = [IO.Path]::GetFullPath($ImageSourceDirectory)
    $nyxImageFixtures = Join-Path $nyxRoot 'build/image-presentation/fixtures'
    if (-not (Test-Path -LiteralPath (Join-Path $nyxImageSeed 'nyx.generated.view.pas'))) {
      throw 'Export the English Image workshop through an owned MCP review first'
    }
    if (-not (Test-Path -LiteralPath (Join-Path $nyxImageFixtures 'nyx.image.fixtures.pas'))) {
      throw 'Run the portable image-presentation prerequisite to create the Pascal raster fixtures'
    }
    $nyxImageNative = Join-Path $nyxImageRoot 'native'
    $nyxImageBrowser = Join-Path $nyxImageRoot 'browser'
    $nyxImageGenerated = Join-Path $nyxImageRoot 'generated'
    New-Item -ItemType Directory -Force -Path $nyxImageNative, $nyxImageBrowser, $nyxImageGenerated | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxImagePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxImageFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImageFixtures",
      "-Fu$nyxLazarus/lcl/units/$nyxImagePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxImagePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxImagePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxImagePlatform",
      "-FU$nyxImageNative", "-FE$nyxImageNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @("-Fu$nyxImageSeed", 'tests/nyx_image_authoring_controls.lpr'))
    & (Join-Path $nyxImageNative 'nyx_image_authoring_controls.exe') `
      (Join-Path $nyxImageGenerated 'nyx.generated.view.pas') `
      (Join-Path $nyxImageRoot 'desktop.png') (Join-Path $nyxImageRoot 'compact.png')
    if ($LASTEXITCODE -ne 0) { throw 'Native image authoring failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @("-Fu$nyxImageGenerated", 'tests/nyx_image_authoring_generated.lpr'))
    & (Join-Path $nyxImageNative 'nyx_image_authoring_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted image authoring failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImageSeed", "-Fu$nyxImageFixtures",
      "-FE$nyxImageBrowser", 'tests/nyx_image_authoring_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', "-Fu$nyxImageGenerated", "-Fu$nyxImageFixtures", "-FE$nyxImageBrowser",
      'tests/nyx_image_authoring_generated.lpr')
    # The ordinary browser controller prepares Apply/Undo through this Pascal
    # worker. Stage its current matched closure, not only the visible test page.
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-FE$nyxImageBrowser", 'studio/nyx_source_worker.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxImageBrowser 'rtl.js')
    foreach ($nyxImageHost in @('image-authoring.html', 'image-authoring-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxImageHost") -Destination $nyxImageBrowser
    }
    Write-Host 'Image authoring counterparts staged; browser/trusted chooser execution remains separate.'
    exit 0
  }

  if ($Target -eq 'image-presentation') {
    # Require the unchanged bounded semantic seed. Pascal owns byte fixtures,
    # typed candidate enrichment, decoding, assertions and exact source output.
    # Browser artifacts stage only; no listener or browser is launched.
    $nyxImageRoot = Join-Path $nyxRoot 'build/image-presentation'
    $nyxImageSeed = [IO.Path]::GetFullPath($ImageSourceDirectory)
    if (-not (Test-Path -LiteralPath (Join-Path $nyxImageSeed 'nyx.generated.view.pas'))) {
      throw 'Export the English Image workshop through a temporary MCP review before this check'
    }
    $nyxImageFixtures = Join-Path $nyxImageRoot 'fixtures'
    $nyxImageTool = Join-Path $nyxImageRoot 'maintained/tool'
    $nyxImageNative = Join-Path $nyxImageRoot 'maintained/native'
    $nyxImageBrowser = Join-Path $nyxImageRoot 'maintained/browser'
    $nyxImageGenerated = Join-Path $nyxImageRoot 'maintained/generated'
    New-Item -ItemType Directory -Force -Path $nyxImageFixtures, $nyxImageTool, $nyxImageNative, $nyxImageBrowser, $nyxImageGenerated | Out-Null
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', "-FU$nyxImageTool", "-FE$nyxImageTool", 'tests/nyx_image_fixtures.lpr')
    & (Join-Path $nyxImageTool 'nyx_image_fixtures.exe') $nyxImageFixtures
    if ($LASTEXITCODE -ne 0) { throw 'Pascal image fixture generation failed' }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxImagePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxImageFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImageSeed", "-Fu$nyxImageFixtures",
      "-Fu$nyxLazarus/lcl/units/$nyxImagePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxImagePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxImagePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxImagePlatform",
      "-FU$nyxImageNative", "-FE$nyxImageNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @('tests/nyx_image_presentation_tests.lpr'))
    & (Join-Path $nyxImageNative 'nyx_image_presentation_tests.exe') `
      (Join-Path $nyxImageGenerated 'nyx.generated.images.pas') `
      (Join-Path $nyxImageGenerated 'images-desktop.png') `
      (Join-Path $nyxImageGenerated 'images-compact.png') `
      (Join-Path $nyxImageGenerated 'nyx.generated.image.lifecycle.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Native image presentation failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @("-Fu$nyxImageGenerated", 'tests/nyx_image_generated.lpr'))
    & (Join-Path $nyxImageNative 'nyx_image_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted image execution failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxImageFlags + @("-Fu$nyxImageGenerated", 'tests/nyx_image_lifecycle_generated.lpr'))
    & (Join-Path $nyxImageNative 'nyx_image_lifecycle_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted image lifecycle failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxImageSeed", "-Fu$nyxImageFixtures",
      "-FE$nyxImageBrowser", 'tests/nyx_image_presentation_tests.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', "-Fu$nyxImageGenerated", "-Fu$nyxImageFixtures",
      "-FE$nyxImageBrowser", 'tests/nyx_image_generated.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxImageBrowser 'rtl.js')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', "-Fu$nyxImageGenerated", '-Fustudio',
      "-FE$nyxImageBrowser", 'tests/nyx_image_lifecycle_generated.lpr')
    foreach ($nyxImageHost in @('image-presentation.html', 'image-generated.html', 'image-lifecycle-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxImageHost") -Destination $nyxImageBrowser
    }
    Write-Host 'Image consumers staged. Browser execution and observing rollout remain separate.'
    exit 0
  }

  if ($Target -eq 'theme-authoring') {
    # The Pascal consumers own theme/source/history/UI assertions. Require the
    # exact bounded English semantic export; keep its generated enrichment local.
    $nyxThemeRoot = Join-Path $nyxRoot 'build/theme-authoring'
    $nyxThemeSeed = Join-Path $nyxThemeRoot 'seed'
    if (-not (Test-Path -LiteralPath (Join-Path $nyxThemeSeed 'nyx.generated.view.pas'))) {
      throw 'Export the English Theme workshop through a temporary MCP review before this check'
    }
    $nyxThemeNative = Join-Path $nyxThemeRoot 'maintained/native'
    $nyxThemeBrowser = Join-Path $nyxThemeRoot 'maintained/browser'
    $nyxThemeGenerated = Join-Path $nyxThemeRoot 'maintained/generated'
    New-Item -ItemType Directory -Force -Path $nyxThemeNative, $nyxThemeBrowser, $nyxThemeGenerated | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxThemePlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxThemeFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxThemeSeed",
      "-Fu$nyxLazarus/lcl/units/$nyxThemePlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxThemePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxThemePlatform",
      "-Fu$nyxLazarus/packager/units/$nyxThemePlatform",
      "-FU$nyxThemeNative", "-FE$nyxThemeNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxThemeFlags + @('tests/nyx_theme_authoring_controls.lpr'))
    & (Join-Path $nyxThemeNative 'nyx_theme_authoring_controls.exe') `
      (Join-Path $nyxThemeGenerated 'nyx.generated.theme.pas') `
      (Join-Path $nyxThemeGenerated 'theme-desktop.png') `
      (Join-Path $nyxThemeGenerated 'theme-compact.png')
    if ($LASTEXITCODE -ne 0) { throw 'Native theme authoring failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxThemeFlags + @("-Fu$nyxThemeGenerated", 'tests/nyx_theme_generated.lpr'))
    & (Join-Path $nyxThemeNative 'nyx_theme_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Exact emitted theme execution failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxThemeSeed", "-FE$nyxThemeBrowser",
      'tests/nyx_theme_authoring_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Fustudio', "-Fu$nyxThemeGenerated", "-FE$nyxThemeBrowser",
      'tests/nyx_theme_generated.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxThemeBrowser 'rtl.js')
    foreach ($nyxThemeHost in @('theme-authoring.html', 'theme-generated.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxThemeHost") -Destination $nyxThemeBrowser
    }
    Write-Host 'Theme consumers staged. Browser execution and observing rollout remain separate.'
    exit 0
  }

  if ($Target -eq 'host-space') {
    # Pascal owns host geometry, admission and actual native lifetime checks.
    # Stage matching browser bytes only; no listener or browser is launched.
    $nyxHostRoot = Join-Path $nyxRoot 'build/host-space/maintained'
    $nyxHostNative = Join-Path $nyxHostRoot 'native'
    $nyxHostBrowser = Join-Path $nyxHostRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxHostPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    New-Item -ItemType Directory -Force $nyxHostNative | Out-Null
    $nyxHostFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxLazarus/lcl/units/$nyxHostPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxHostPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxHostPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxHostPlatform", "-FU$nyxHostNative", "-FE$nyxHostNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxHostFlags + @('tests/nyx_host_space_controls.lpr'))
    & (Join-Path $nyxHostNative 'nyx_host_space_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Native host-space consumer failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    if ($BrowserOutput) { $nyxHostBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxHostBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', '-Fustudio', "-FE$nyxHostBrowser", 'tests/nyx_host_space_controls.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxHostBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/host-space.html') -Destination $nyxHostBrowser
    exit 0
  }

  if ($Target -eq 'slider-fields') {
    # Pascal owns assertions and typed policy enrichment. Native controls run;
    # browser artifacts stage only. This does not launch or update a service.
    $nyxSliderRoot = Join-Path $nyxRoot 'build/slider-values/maintained'
    $nyxSliderNative = Join-Path $nyxSliderRoot 'native'
    $nyxSliderBrowser = Join-Path $nyxSliderRoot 'browser'
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxSliderPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxSliderSourceFlags = @()

    if ($SliderSourceDirectory) {
      $nyxSliderSource = [IO.Path]::GetFullPath($SliderSourceDirectory)
      if (-not (Test-Path -LiteralPath (Join-Path $nyxSliderSource 'nyx.generated.slider.pas'))) {
        throw 'Supply the typed slider candidate with SliderSourceDirectory'
      }
      $nyxSliderSourceFlags = @("-Fu$nyxSliderSource", '-dNYX_COMPILED_SLIDER')
    }
    New-Item -ItemType Directory -Force $nyxSliderNative | Out-Null
    $nyxSliderFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Futests', '-Fustudio', "-Fu$nyxLazarus/lcl/units/$nyxSliderPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxSliderPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxSliderPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxSliderPlatform", "-FU$nyxSliderNative", "-FE$nyxSliderNative")
    Invoke-NyxCompiler $nyxLclFpc ($nyxSliderFlags + $nyxSliderSourceFlags + @('tests/nyx_slider_controls.lpr'))
    & (Join-Path $nyxSliderNative 'nyx_slider_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Native slider consumer failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    if ($BrowserOutput) { $nyxSliderBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxSliderBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-Fusrc', '-Futests', '-Fustudio', "-FE$nyxSliderBrowser") + $nyxSliderSourceFlags + @('tests/nyx_slider_controls.lpr'))
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxSliderBrowser 'rtl.js')
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/slider-fields.html') -Destination $nyxSliderBrowser
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

  if ($Target -eq 'content-revisions') {
    # The unchanged semantic export supplies the English application. Pascal
    # owns typed enrichment, real Studio input, paired admission and assertions.
    # Each output is isolated from retained baseline evidence and live services.
    $nyxRevisionRoot = Join-Path $nyxRoot 'build/content-revisions/maintained'
    $nyxRevisionSource = [IO.Path]::GetFullPath($ContentEditorSourceDirectory)
    $nyxRevisionSeed = Join-Path $nyxRevisionSource 'nyx.generated.view.pas'
    if (-not (Test-Path -LiteralPath $nyxRevisionSeed -PathType Leaf)) {
      throw 'Supply the unchanged content-editor semantic source export'
    }
    $nyxRevisionNative = Join-Path $nyxRevisionRoot 'native'
    $nyxRevisionResult = Join-Path $nyxRevisionRoot 'result'
    $nyxRevisionGenerated = Join-Path $nyxRevisionRoot 'generated'
    $nyxRevisionWeb = Join-Path $nyxRevisionRoot 'browser'
    New-Item -ItemType Directory -Force $nyxRevisionNative, $nyxRevisionResult,
      $nyxRevisionGenerated, $nyxRevisionWeb | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxRevisionPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxRevisionSource",
      "-Fu$nyxLazarus/lcl/units/$nyxRevisionPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxRevisionPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxRevisionPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxRevisionPlatform",
      "-FU$nyxRevisionNative", "-FE$nyxRevisionNative", 'tests/nyx_content_revisions_controls.lpr')
    & (Join-Path $nyxRevisionNative 'nyx_content_revisions_controls.exe') $nyxRevisionResult
    if ($LASTEXITCODE -ne 0) { throw 'Actual Studio recipe revision checks failed' }
    Copy-Item -LiteralPath (Join-Path $nyxRevisionResult 'accepted.pas.txt') `
      -Destination (Join-Path $nyxRevisionGenerated 'nyx.generated.view.pas') -Force
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-dNYX_RECIPE_REVISIONS', '-Fusrc', '-Fustudio', "-Fu$nyxRevisionGenerated",
      "-FU$nyxRevisionGenerated", "-FE$nyxRevisionGenerated", 'tests/nyx_content_editor_generated.lpr')
    & (Join-Path $nyxRevisionGenerated 'nyx_content_editor_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Executed revised recipe source failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc',
      '-Fustudio', '-Futests', "-Fu$nyxRevisionSource", "-FE$nyxRevisionWeb",
      'tests/nyx_content_revisions_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js', '-Fusrc',
      '-Fustudio', "-FE$nyxRevisionWeb", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js',
      '-dNYX_RECIPE_REVISIONS', '-Fusrc', "-Fu$nyxRevisionGenerated", "-FE$nyxRevisionWeb",
      'tests/nyx_content_editor_generated.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxRevisionWeb 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/content-revisions.html'),
      (Join-Path $nyxRoot 'studio/web/content-editor-generated.html') -Destination $nyxRevisionWeb -Force
    Write-Host 'Native revision/source checks passed; browser staging requires actual admitted HTTP execution.'
    exit 0
  }

  if ($Target -eq 'content-editor') {
    # The shared Pascal consumer owns all input, admission, history and checks.
    # This branch starts no listener and changes no operator project/service.
    $nyxContentEditorRoot = Join-Path $nyxRoot 'build/content-editor/maintained'
    $nyxContentEditorSource = $ContentEditorSourceDirectory
    if (-not [IO.Path]::IsPathRooted($nyxContentEditorSource)) {
      $nyxContentEditorSource = Join-Path $nyxRoot $nyxContentEditorSource
    }
    $nyxContentEditorSource = [IO.Path]::GetFullPath($nyxContentEditorSource)
    $nyxContentEditorNative = Join-Path $nyxContentEditorRoot 'native'
    $nyxContentEditorWeb = Join-Path $nyxContentEditorRoot 'web'
    $nyxContentEditorResult = Join-Path $nyxContentEditorRoot 'result'
    New-Item -ItemType Directory -Force $nyxContentEditorNative, $nyxContentEditorWeb,
      $nyxContentEditorResult | Out-Null
    if ($DesignerMCPConfig) {
      $nyxContentEditorMCP = Join-Path $nyxContentEditorRoot 'mcp'
      New-Item -ItemType Directory -Force $nyxContentEditorMCP | Out-Null
      Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        "-FU$nyxContentEditorMCP", "-FE$nyxContentEditorMCP", 'tests/nyx_mcp_designer_review.lpr')
      & (Join-Path $nyxContentEditorMCP 'nyx_mcp_designer_review.exe') $DesignerMCPConfig `
        (Join-Path $nyxRoot 'tests/content-editor.operations.json') $nyxContentEditorSource
      if ($LASTEXITCODE -ne 0) { throw 'Semantic content editor seed/export/build journey failed' }
    }
    $nyxContentEditorSeed = Join-Path $nyxContentEditorSource 'nyx.generated.view.pas'
    if (-not (Test-Path -LiteralPath $nyxContentEditorSeed -PathType Leaf)) {
      throw 'Export the exact content editor MCP seed or supply DesignerMCPConfig'
    }
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxContentEditorPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxContentEditorSource",
      "-Fu$nyxLazarus/lcl/units/$nyxContentEditorPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxContentEditorPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContentEditorPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxContentEditorPlatform",
      "-FU$nyxContentEditorNative", "-FE$nyxContentEditorNative", 'tests/nyx_content_editor_controls.lpr')
    & (Join-Path $nyxContentEditorNative 'nyx_content_editor_controls.exe') $nyxContentEditorSeed `
      (Join-Path $nyxContentEditorResult 'nyx.generated.view.pas') $nyxContentEditorResult
    if ($LASTEXITCODE -ne 0) { throw 'Actual native content editor/queue qualification failed' }
    # Execute the exact newly generated unit against shared recipe expectations.
    Invoke-NyxCompiler $nyxFpc @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', "-Fu$nyxContentEditorResult", "-FU$nyxContentEditorResult",
      "-FE$nyxContentEditorResult", 'tests/nyx_content_editor_generated.lpr')
    & (Join-Path $nyxContentEditorResult 'nyx_content_editor_generated.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Executed content editor Pascal companion failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Fustudio',
      '-Futests', "-Fu$nyxContentEditorSource", "-FE$nyxContentEditorWeb", 'tests/nyx_content_editor_controls.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tmodule', '-Jirtl.js', '-Fusrc', '-Fustudio',
      "-FE$nyxContentEditorWeb", 'studio/nyx_source_worker.lpr')
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc',
      "-Fu$nyxContentEditorResult", "-FE$nyxContentEditorWeb", 'tests/nyx_content_editor_generated.lpr')
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxContentEditorWeb 'rtl.js') -Force
    Copy-Item -LiteralPath $nyxContentEditorSeed -Destination (Join-Path $nyxContentEditorWeb 'seed.pas.txt') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/content-editor-controls.html'),
      (Join-Path $nyxRoot 'studio/web/content-editor-generated.html') `
      -Destination $nyxContentEditorWeb -Force
    Write-Host 'Native recipe editor passed; staged browser consumer still requires actual execution on an owned HTTP host.'
    exit 0
  }

  if ($Target -eq 'content-recipes') {
    # Pascal owns the contract, semantic grouped history and actual controls.
    # This branch only orchestrates compiler/run/staging tools. It neither
    # launches listeners nor changes existing projects or service workspaces.
    $nyxContentRoot = Join-Path $nyxRoot 'build/content-recipes/maintained'
    $nyxContentSource = Join-Path $nyxContentRoot 'source'
    New-Item -ItemType Directory -Force $nyxContentSource | Out-Null
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxContentPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxContentChecked = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh', '-Fusrc', '-Fustudio')
    foreach ($nyxContentCompiler in @(@($nyxFpc, 'stable'), @($nyxLclFpc, 'matched'))) {
      $nyxContentUnits = Join-Path $nyxContentRoot $nyxContentCompiler[1]
      New-Item -ItemType Directory -Force $nyxContentUnits | Out-Null
      Invoke-NyxCompiler $nyxContentCompiler[0] ($nyxContentChecked + @(
        "-FU$nyxContentUnits", "-FE$nyxContentUnits", 'tests/nyx_content_tests.lpr'))
      & (Join-Path $nyxContentUnits 'nyx_content_tests.exe')
      if ($LASTEXITCODE -ne 0) { throw 'Content recipe contract/semantic qualification failed' }
    }
    # The exact exported Pascal includes supplementary recipe names and shared
    # typed bindings. The actual consumer compiles it unchanged on each target.
    & (Join-Path $nyxContentRoot 'stable/nyx_content_tests.exe') (Join-Path $nyxContentSource 'nyx.generated.view.pas')
    if ($LASTEXITCODE -ne 0) { throw 'Content recipe companion export failed' }
    $nyxContentNative = Join-Path $nyxContentRoot 'controls'
    New-Item -ItemType Directory -Force $nyxContentNative | Out-Null
    Invoke-NyxCompiler $nyxLclFpc ($nyxContentChecked + @("-Fu$nyxContentSource",
      "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform", "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContentPlatform", "-Fu$nyxLazarus/packager/units/$nyxContentPlatform",
      "-FU$nyxContentNative", "-FE$nyxContentNative", 'tests/nyx_content_controls.lpr'))
    & (Join-Path $nyxContentNative 'nyx_content_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native content recipe qualification failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxContentChecked + @("-Fu$nyxContentSource",
      "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform", "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContentPlatform", "-Fu$nyxLazarus/packager/units/$nyxContentPlatform",
      "-FU$nyxContentNative", "-FE$nyxContentNative", 'tests/nyx_content_live_controls.lpr'))
    & (Join-Path $nyxContentNative 'nyx_content_live_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native live content publication failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxContentChecked + @(
      "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform", "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContentPlatform", "-Fu$nyxLazarus/packager/units/$nyxContentPlatform",
      "-FU$nyxContentNative", "-FE$nyxContentNative", 'tests/nyx_content_settling_controls.lpr'))
    & (Join-Path $nyxContentNative 'nyx_content_settling_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native nested content admission failed' }
    Invoke-NyxCompiler $nyxLclFpc ($nyxContentChecked + @("-Fu$nyxContentSource",
      "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform", "-Fu$nyxLazarus/lcl/units/$nyxContentPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxContentPlatform", "-Fu$nyxLazarus/packager/units/$nyxContentPlatform",
      "-FU$nyxContentNative", "-FE$nyxContentNative", 'tests/nyx_content_publication_controls.lpr'))
    & (Join-Path $nyxContentNative 'nyx_content_publication_controls.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native reversible publication failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxContentBrowser = Join-Path $nyxContentRoot 'web'
    New-Item -ItemType Directory -Force $nyxContentBrowser | Out-Null
    foreach ($nyxContentProgram in @('nyx_content_tests', 'nyx_content_controls', 'nyx_content_live_controls', 'nyx_content_settling_controls', 'nyx_content_publication_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Fustudio',
        "-Fu$nyxContentSource", "-FE$nyxContentBrowser", "tests/$nyxContentProgram.lpr")
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxContentBrowser 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/content-contracts.html'),
      (Join-Path $nyxRoot 'studio/web/content-controls.html'),
      (Join-Path $nyxRoot 'studio/web/content-live-controls.html'),
      (Join-Path $nyxRoot 'studio/web/content-settling-controls.html'),
      (Join-Path $nyxRoot 'studio/web/content-publication-controls.html') -Destination $nyxContentBrowser -Force
    $nyxContentDriver = Join-Path $nyxContentRoot 'driver'
    New-Item -ItemType Directory -Force $nyxContentDriver | Out-Null
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-Fusrc', '-Futests',
      "-FU$nyxContentDriver", "-FE$nyxContentDriver", 'tests/nyx_responsive_browser_review.lpr')
    Write-Host 'Content contracts/native controls pass. Browser execution still requires an isolated HTTP host.'
    exit 0
  }

  if ($Target -in @('view-sections', 'studio-section-recovery')) {
    # Pascal owns grouped publication, rollback, real controls and lifetime
    # assertions. This orchestration stages artifacts; it launches no listener.
    $nyxSectionProgram = 'nyx_view_section_controls'
    $nyxSectionHtml = 'view-sections.html'
    if ($Target -eq 'studio-section-recovery') {
      $nyxSectionProgram = 'nyx_studio_section_controls'
      $nyxSectionHtml = 'studio-section-recovery.html'
    }
    $nyxSectionRoot = Join-Path $nyxRoot "build/$Target/maintained"
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxSectionPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxSectionNative = Join-Path $nyxSectionRoot 'native'
    New-Item -ItemType Directory -Force $nyxSectionNative | Out-Null
    Invoke-NyxCompiler $nyxLclFpc @('-B', '-Mdelphi', '-O2', '-Sa', '-Cr', '-Co', '-Ci',
      '-gl', '-gh', '-Fusrc', '-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxSectionPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxSectionPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxSectionPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxSectionPlatform",
      "-FU$nyxSectionNative", "-FE$nyxSectionNative", "tests/$nyxSectionProgram.lpr")
    & (Join-Path $nyxSectionNative "$nyxSectionProgram.exe") (Join-Path $nyxSectionRoot 'native-live.png')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native section publication failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxSectionBrowser = Join-Path $nyxSectionRoot 'web'
    if ($BrowserOutput) { $nyxSectionBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxSectionBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Fustudio', '-Futests',
      "-FE$nyxSectionBrowser", "tests/$nyxSectionProgram.lpr")
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxSectionBrowser 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxSectionHtml") -Destination $nyxSectionBrowser -Force
    Write-Host 'Native sections pass. Execute browser artifacts on an independently admitted existing HTTP host.'
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
    Invoke-NyxCompiler $nyxLclFpc ($nyxArrangeChecked + $nyxArrangeSource + @('-Fustudio', '-Futests',
      "-Fu$nyxLazarus/lcl/units/$nyxArrangePlatform", "-Fu$nyxLazarus/lcl/units/$nyxArrangePlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxArrangePlatform", "-Fu$nyxLazarus/packager/units/$nyxArrangePlatform",
      "-FU$nyxArrangeNative", "-FE$nyxArrangeNative", 'tests/nyx_bound_arrangement_tests.lpr'))
    & (Join-Path $nyxArrangeNative 'nyx_bound_arrangement_tests.exe')
    if ($LASTEXITCODE -ne 0) { throw 'Actual native bound arrangement failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    $nyxArrangeBrowser = Join-Path $nyxArrangeRoot 'web'
    New-Item -ItemType Directory -Force $nyxArrangeBrowser | Out-Null
    Invoke-NyxCompiler $nyxPas2js @('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Futests',
      "-FE$nyxArrangeBrowser", 'tests/nyx_arrangement_tests.lpr')
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Fustudio',
      '-Futests', "-FE$nyxArrangeBrowser", 'tests/nyx_projection_refresh_tests.lpr') + $nyxArrangeSource)
    Invoke-NyxCompiler $nyxPas2js (@('-B', '-Mdelphi', '-Tbrowser', '-Jirtl.js', '-Fusrc', '-Fustudio',
      '-Futests', "-FE$nyxArrangeBrowser", 'tests/nyx_bound_arrangement_tests.lpr') + $nyxArrangeSource)
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxArrangeBrowser 'rtl.js') -Force
    Copy-Item -LiteralPath (Join-Path $nyxRoot 'studio/web/arrangement-tests.html'),
      (Join-Path $nyxRoot 'studio/web/projection-refresh.html'),
      (Join-Path $nyxRoot 'studio/web/bound-arrangement.html') -Destination $nyxArrangeBrowser -Force
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

  if ($Target -eq 'pascal-views') {
    # Pascal owns semantic admission/refusal/history and exact compiler output.
    # No backend, enrollment, observing project or compiler profile is changed.
    $nyxViewsRoot = Join-Path $nyxRoot 'build/views-source'
    $nyxViewsNative = Join-Path $nyxViewsRoot 'native'
    $nyxViewsExport = Join-Path $nyxViewsRoot 'export'
    $nyxViewsLcl = Join-Path $nyxViewsRoot 'lcl'
    $nyxViewsBrowser = Join-Path $nyxViewsRoot 'browser'

    if ($BrowserOutput) { $nyxViewsBrowser = [IO.Path]::GetFullPath($BrowserOutput) }
    New-Item -ItemType Directory -Force $nyxViewsNative, $nyxViewsExport,
      $nyxViewsLcl, $nyxViewsBrowser | Out-Null
    $nyxViewsFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-FU$nyxViewsNative", "-FE$nyxViewsNative")
    Invoke-NyxCompiler $nyxFpc ($nyxViewsFlags + @('tests/nyx_views_tests.lpr'))
    & (Join-Path $nyxViewsNative 'nyx_views_tests.exe') $nyxViewsExport

    if ($LASTEXITCODE -ne 0) { throw 'Semantic view source qualification failed' }
    $nyxLazarus = Resolve-NyxTool $Lazarus 'LAZARUS' ''
    $nyxLclFpc = Resolve-NyxTool $LclFpc 'LCL_FPC' 'fpc'
    $nyxViewsPlatform = "$((& $nyxLclFpc '-iTP').Trim())-$((& $nyxLclFpc '-iTO').Trim())"
    $nyxViewsControlFlags = @('-B', '-Mdelphi', '-Sa', '-Cr', '-Co', '-Ci', '-gl', '-gh',
      '-Fusrc', '-Fustudio', '-Futests', "-Fu$nyxViewsExport", "-Fi$nyxViewsExport",
      "-Fu$nyxLazarus/lcl/units/$nyxViewsPlatform",
      "-Fu$nyxLazarus/lcl/units/$nyxViewsPlatform/$Widgetset",
      "-Fu$nyxLazarus/components/lazutils/lib/$nyxViewsPlatform",
      "-Fu$nyxLazarus/packager/units/$nyxViewsPlatform",
      "-FU$nyxViewsLcl", "-FE$nyxViewsLcl")
    foreach ($nyxViewsSchema in @('nyx_import_schema', 'nyx_routine_schema', 'nyx_declaration_schema')) {
      Invoke-NyxCompiler $nyxLclFpc ($nyxViewsControlFlags + @("tests/$nyxViewsSchema.lpr"))
      & (Join-Path $nyxViewsLcl "$nyxViewsSchema.exe")

      if ($LASTEXITCODE -ne 0) { throw 'Current Pascal discovery qualification failed' }
    }
    Invoke-NyxCompiler $nyxLclFpc ($nyxViewsControlFlags + @('tests/nyx_views_controls.lpr'))
    & (Join-Path $nyxViewsLcl 'nyx_views_controls.exe')

    if ($LASTEXITCODE -ne 0) { throw 'Exact compiled native view source failed' }
    $nyxPas2js = Resolve-NyxTool $Pas2js 'PAS2JS' 'pas2js'
    $nyxRuntime = Resolve-NyxTool $Pas2jsRuntime 'PAS2JS_RUNTIME' ''
    foreach ($nyxViewsProgram in @('nyx_views_tests', 'nyx_views_controls')) {
      Invoke-NyxCompiler $nyxPas2js @('-B', '-Tbrowser', '-Mdelphi', '-Fusrc', '-Fustudio', '-Futests',
        '-Jirtl.js', "-Fu$nyxViewsExport", "-Fi$nyxViewsExport", "-FE$nyxViewsBrowser",
        "tests/$nyxViewsProgram.lpr")
    }
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxViewsBrowser 'rtl.js')
    foreach ($nyxViewsHost in @('views-edits.html', 'views-controls.html')) {
      Copy-Item -LiteralPath (Join-Path $nyxRoot "studio/web/$nyxViewsHost") -Destination $nyxViewsBrowser
    }
    Write-Host 'Semantic view source consumers staged; browser execution needs an admitted HTTP host.'
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
