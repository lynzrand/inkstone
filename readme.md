![Inkstone Logo](res/logo-long.svg)

> _Inkstone is **not** a markup language._

Inkstone is an experimental general-purpose scripting language to make scripting less frustrating to work with. It was created as the author finds markup languages for game scripts are overwhelmed with unnecessary symbols, tags, or being too domain specific.

## Crates

- [`inkstone-syn`][syn] contains code and structures to parse and represent Inkstone source code.
- [`inkstone-bytecode`][bc] contains the definition of an instruction set for the Inkstone virtual machine.
- [`inkstone-codegen`][cg] contains code for generating bytecode from an AST.
- [`inkstone-vm`][vm] contains implementation of a virtual machine that can run bytecode.
- The root crate, `inkstone`, wraps the aforementioned crates and exports an executable.

[syn]: crates/inkstone-syn
[bc]: crates/inkstone-bytecode
[cg]: crates/inkstone-codegen
[vm]: crates/inkstone-vm

### Planned

- `inkstone-typings` for type-checking Inkstone code.
- `inkstone-jit` for JIT-ting efficient machine code from bytecode and probably type information, probably using `cranelift`.

## License

The source code of Inkstone's reference implementation is licensed under [Mozilla Public License 2.0][MPL]. 

[MPL]: https://www.mozilla.org/en-US/MPL/
