# meowc

A build tool for C and C++. Declare targets in a `build.meow` file; there is no
makefile and no separate configure script.

Staleness is decided by content, not timestamps. Every action is keyed on its
sources, the headers the compiler actually opened, and the exact argument
vector, so a `touch` or a checkout that rewrites files rebuilds nothing.

```
dune build && ln -sf _build/default/bin/meowc.exe meowc
cd examples/hello && ../../meowc build && ../../meowc test
```
