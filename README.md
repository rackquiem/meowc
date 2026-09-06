Build tool for C and C++ that derives the build graph and dependencies automatically, without needing Makefiles or generated build files

![meowc](assets/kitty.gif)

meowc tracks dependencies at the action level and only reruns actions whose inputs have changed. Outputs are compared by content so downstream actions are skipped when recompilation produces byte identical object files

Adding a declaration to a header that most of the project includes fans out to fourteen translation units across four targets, and nothing relinks, because a declaration emits no code and the objects come back byte identical

![editing a header](assets/emacs.gif)

```
dune build && ln -sf _build/default/bin/meowc.exe meowc
cd examples/raytracer && ../../meowc build && ../../meowc test
```

Cross compilation is driven by a target triple. Tools are taken from the triple
prefix when a matching toolchain is installed, otherwise the host compiler is
invoked with `--target`. Each triple gets its own build directory and probe
cache, so host and cross trees do not invalidate each other, and the triple
drives `platform`, `arch`, `format` and the conditionals built on them

```
meowc --target x86_64-w64-mingw32 build
meowc --target aarch64-linux-gnu --sysroot /opt/sysroots/aarch64 build
```

Output names and link flags follow the object format rather than the operating
system. ELF gets `-fPIC`, `-shared` and `-Wl,-soname`, MachO gets `-dynamiclib`
and `-Wl,-install_name`, and COFF drops both the position independent code flag
and the soname, writing `rt.dll` next to the `librt.dll.a` that dependents link
against

## TODO

- Emit response files when command lines approach the platform argument length limit
- Integrate precompiled headers into dependency tracking and invalidation
- Replace periodic mtime polling in `watch` with filesystem event notifications such as `inotify`