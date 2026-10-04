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
  [ValidateSet('core', 'generated', 'collections', 'collection-views', 'collection-authoring', 'selection', 'keyboard', 'semantic-events', 'source-workspace', 'agents', 'agent-callback-consumers', 'agent-handler-consumers', 'agent-root-consumers', 'mcp-client', 'split', 'interactions', 'named-events', 'viewport', 'editing', 'gestures', 'catalog', 'browser', 'studio', 'lcl', 'http', 'visual', 'all')]
  [string]$Target = 'core',
  [string]$Fpc,
  [string]$Pas2js,
  [string]$Pas2jsRuntime,
  [string]$Lazarus,
  [string]$LclFpc,
  [string]$Widgetset = 'win32',
  [string]$HttpURL = 'http://127.0.0.1:8088',
  # Stage browser artifacts independently while an older LAN instance is live.
  [string]$BrowserOutput,
  # The keyboard review source is exported through MCP, never handwritten by
  # this orchestration script. Its generated unit must live in this directory.
  [string]$KeyboardSourceDirectory = 'build/keyboard/mcp',
  # The maintained semantic callback journey exports two accepted source pairs.
  [string]$CallbackSourceDirectory = 'build/agent-callbacks/mcp',
  # Exact companion exported by the semantic handler/compilation journey.
  [string]$HandlerSourceDirectory = 'build/handler-edits/journey/source',
  # Unchanged companion exported by the isolated semantic root-cleanup journey.
  [string]$RootSourceDirectory = 'build/root-cleanup/journey/source'
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
}

function Test-NyxCompilerTypes([string]$Executable, [string[]]$Arguments) {
  # These are Pascal compiler fixtures, not a shell implementation of typing.
  # Require the intended type diagnostic as well as failure: a missing unit or
  # tool must never be mistaken for successful rejection of an invalid argument.
  $nyxTypeCases = @{
    layout = 'TNyxLayoutMode'
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
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tests/nyx_source_diagnostic_tests.lpr'))
    & (Join-Path $nyxNativeDir 'nyx_source_diagnostic_tests.exe')

    if ($LASTEXITCODE -ne 0) {
      throw 'Source diagnostic admission failed'
    }
    # Timing is a separate, explicitly selected run; compile the same public
    # fixture for both targets without hiding a slow large case in verification.
    Invoke-NyxCompiler $nyxFpc ($nyxNativeFlags + @('tools/nyx_source_benchmark.lpr'))
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
    Copy-Item -LiteralPath $nyxRuntime -Destination (Join-Path $nyxBrowserDir 'rtl.js')
    foreach ($nyxSourceHost in @('source-diagnostics.html', 'source-benchmark.html')) {
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
