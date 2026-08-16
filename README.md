# meowc

Build tool for C and C++ that derives the build graph and dependencies
automatically, without needing Makefiles or generated build files

meowc tracks dependencies at the action level and only reruns actions whose
inputs have changed. Outputs are compared by content so downstream actions are
skipped when recompilation produces byte identical object files

```
dune build && ln -sf _build/default/bin/meowc.exe meowc
cd examples/raytracer && ../../meowc build && ../../meowc test
```

## TODO

- Model cross-compilation through target triples, sysroots and per-target toolchain configuration
- Remove ELF and MachO assumptions from shared library naming, linker invocation, and platform specific compiler flags
- Emit response files when command lines approach the platform argument length limit
- Integrate precompiled headers into dependency tracking and invalidation
- Replace periodic mtime polling in `watch` with filesystem event notifications such as `inotify`
- Propagate build failures through the scheduler and cancel in flight work rather than preventing new actions from starting
