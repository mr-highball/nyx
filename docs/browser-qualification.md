# Real browser worker qualification

[Building](building.md) · [Events](events.md) · [Evidence](../WORK.md#real-browser-worker-and-contextual-warnings--2026-10-06)

Studio prepares source changes in a compiled Pascal worker. Loading its host page
or compiling its JavaScript does not establish that a queued command published.
The maintained `browser-worker` target builds a native Pascal observer and the
ordinary browser Studio callback fixture, including the matched module worker.

## Build and run

Run `tools/build.ps1 -Target browser-worker` with the installed local toolchain.
It stages these artifacts without launching a server:

- `build/browser-worker-observation/maintained/driver/nyx_browser_ready_capture.exe`
- `build/browser-worker-observation/maintained/web/event-inspector-controls.html`
- The fixture, `nyx_source_worker.js` and matched `rtl.js` beside that HTML.

The worker uses pas2js `-Tmodule -Jirtl.js`, including its program entry. An
earlier ad hoc `-Tbrowser` staging lacked that entry and timed out without
installing its message listener. Rebuild the module; do not patch generated
JavaScript or lengthen production timeouts to compensate.

Serve the staged web files through an already admitted loopback HTTP host. Use
the explicit HTML filename and `?capture=1`. Invoke the observer with four
arguments: the fixture URL, an owned artifact directory, `data-event-controls`
and the desired CSS width. Run separate directories for desktop and narrow
widths. The target does not select, replace or restart an observing Studio service.

## Observation and ownership

The Windows observer owns one headless Edge process, a unique profile and two
anonymous debugger pipes. Its implementation follows Chromium's
[Windows pipe transport](https://chromium.googlesource.com/chromium/src/+/main/chrome/test/chromedriver/net/pipe_builder.cc).
There is no debugging TCP listener or connection to an existing user tab. Native
Pascal reads NUL-delimited UTF-8 protocol packets, with 4 MiB packet and 1 MiB
diagnostic limits. Commands use a 15-second real-clock deadline; fixture completion
uses 180 seconds. A Runtime exception or failed fixture marker is a failure.

The observer reads bounded body attributes. At `warning` and `retained-draft`
checkpoints it saves actual PNG/DOM evidence before acknowledging the fixture.
That acknowledgment releases the fixture's next test command; product workers
continue on their ordinary clocks. The driver injects no scripts and sends no
editor commands. Semantic MCP remains the primary design and build workflow;
actual-control consumers establish input, worker publication and teardown that
document queries cannot prove.

The owning process closes its own browser and handles on success or failure.
It retains the unique profile and evidence for diagnosis. It never stops another
Studio process, changes enrollment or reads browser recovery from a user project.

Both Studio hosts expose `SourceBusy` on the UI thread. It observes active and
queued source work even when compact layouts omit the source status label.
False means the source queue is quiescent; successful admission still requires
the command result and accepted pair. Native consumers also wait for their
presentation refresh. Visible status text is not a readiness contract.

## Current evidence and limits

The 2026-10-06 ordinary browser callback journey passes **64** checks at each
actual CSS width, **1100** and **390** pixels. Native Win32 Studio passes **59**
checks. Checked native runs and observer traces report zero leaks. The browser
executes its real compiled source worker, including addition, queued policy,
warning, Keep, confirmed removal, paired Undo, independent draft refusal and
actual source-control retirement.

The shared inline warning appears inside the exact callback row, with current
owner/context/event/registration/handler checks. Its public Nyx confirmation
recipe and actions remain the same on both targets. Desktop, narrow and native
warning captures and both browser draft checkpoints have been inspected.

This qualifies a local ordinary Studio consumer, not authenticated observing
HTTP synchronization or a new LAN release. The preserved observing service is
older. Physical phones, hardware input, IME, assistive technology, other browser
engines/widgetsets, full accessibility and production performance remain open.
See the linked evidence packet for semantic receipts and preservation checks.
