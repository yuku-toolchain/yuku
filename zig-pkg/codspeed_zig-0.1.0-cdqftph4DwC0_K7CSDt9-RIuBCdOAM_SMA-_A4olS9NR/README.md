# codspeed-zig

`codspeed-zig` is a Zig wrapper for CodSpeed's `instrument-hooks` runtime.
It lets you mark benchmark boundaries from Zig and send benchmark metadata to CodSpeed.

## Installation

Add the dependency:

```sh
zig fetch --save git+https://github.com/arshad-yaseen/codspeed-zig.git
```

In `build.zig`, import the module:

```zig
const codspeed_dep = b.dependency("codspeed_zig", .{
    .target = target,
    .optimize = optimize,
});
exe.root_module.addImport("codspeed", codspeed_dep.module("codspeed"));
```

## Benchmarking Functions

### Recommended (high-level API)

```zig
const std = @import("std");
const codspeed = @import("codspeed");

pub fn main() !void {
    var session = try codspeed.initSession(std.heap.c_allocator); // or page_allocator
    defer session.deinit();

    // Optional metadata to identify your integration in CodSpeed.
    try session.setIntegration("zig", "0.1.0");

    try session.bench("example/busy_work", busyWork);
    try session.bench("example/parse_int_work", parseIntWork);
}

fn busyWork() void {
    var sum: u64 = 0;
    var i: u64 = 0;
    while (i < 1_000_000) : (i += 1) {
        sum +%= i;
    }
}

fn parseIntWork() void {
    const input = "123456";
    _ = std.fmt.parseInt(u64, input, 10) catch unreachable;
}
```

`session.bench()` does this sequence for you:

1. `startBenchmark(handle)`
2. run your function
3. `stopBenchmark(handle)`
4. `setExecutedBenchmark(handle, current_pid, benchmark_id)`

### Low-level explicit API

Use this when you want full control and zero hidden allocations.

```zig
const codspeed = @import("codspeed");

const handle = try codspeed.init();
defer codspeed.deinit(handle);

try codspeed.setIntegration(handle, "zig", "0.1.0");
try codspeed.bench(handle, "example/busy_work", busyWork);
```

For dynamic names/versions in low-level mode, convert to null-terminated first (`dupeZ`, `allocPrintZ`, etc.).

If needed, you can call methods manually:

1. `startBenchmark(handle)`
2. run your function
3. `stopBenchmark(handle)`
4. `setExecutedBenchmark(handle, current_pid, benchmark_id)`

## Integration with CI (GitHub Actions)

CodSpeed measurements are generated in CI by wrapping your benchmark command with `CodSpeedHQ/action`.

### Recommended workflow (OIDC)

```yaml
name: CodSpeed Benchmarks

on:
  push:
    branches: ["main"]
  pull_request:
  workflow_dispatch:

permissions:
  contents: read
  id-token: write

jobs:
  benchmarks:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v5
      - uses: mlugg/setup-zig@v2
        with:
          version: master
      - name: Run benchmarks with CodSpeed
        uses: CodSpeedHQ/action@v4
        with:
          mode: simulation
          run: zig build test
```

`zig build test` is only an example command.  

It produces benchmark results only for code paths that call:

- `session.bench(...)` or `codspeed.bench(handle, ...)`, or
- the manual sequence `startBenchmark(handle)` -> `stopBenchmark(handle)` -> `setExecutedBenchmark(handle, ...)`.

So if your test suite has regular tests plus a few benchmark-marked tests, only those benchmark-marked tests are reported to CodSpeed.

If you prefer separation, create a dedicated benchmark entrypoint/step (for example `zig build bench`) and use that in `run:` instead.

## References

- CodSpeed docs: https://docs.codspeed.io
- CodSpeed GitHub Action docs: https://docs.codspeed.io/ci-cd/github-action
- CodSpeed CPU simulation mode: https://docs.codspeed.io/instruments/cpu-simulation
- CodSpeed action repository: https://github.com/CodSpeedHQ/action
