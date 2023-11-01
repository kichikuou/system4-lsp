# System4 Language Server

This is a work-in-progress language server implementation for AliceSoft's
System 4 programming language.

## Features

- [x] Diagnostics
- [x] Hover
- [ ] Jump to Definition
- [ ] Find References
- [ ] Code Completion
- [ ] Signature Help
- [ ] Semantic Tokens

## Installation

### Windows

Download the latest release from [Releases](https://github.com/kichikuou/system4-lsp/releases) page.

### Other Platforms

To build and install `system4-lsp` from source code, you need to have OCaml and
opam installed. Then, run the following commands:

```sh
$ git clone https://github.com/kichikuou/system4-lsp.git
$ cd system4-lsp
$ opam install . --deps-only --with-test
$ dune build
$ dune install
```

## Usage

You don't need to run `system4-lsp` manually, as your editor should do that for
you. If you're using Visual Studio Code, you can install the
[System4 language extension](https://marketplace.visualstudio.com/items?itemName=kichikuou.system4)
to get started.

## Limitations

Some features of the System4 programming language are not yet implemented.
This implementation works pretty well for source code decompiled with
AinDecompiler, but may not work for source code of regular projects developed
with the System4 SDK.

## Credits

System4-lsp is derived from the
[sys4c compiler](https://github.com/nunuhara/sys4c) created by
[@nunuhara](https://github.com/nunuhara).
