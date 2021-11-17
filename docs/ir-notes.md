# Inkstone VM & Bytecode design

## Bytecode Design

- Single-byte
- Zero or one **integer** operand
- Operand is variable-length encoded
  - Either LEB128 or a compact 1/2/4/8 byte schema

## Metadata Design

Mappings between bytecode and text are stored as an ordered array. Retrieving text offset data is just binary searching.

Mappings between scope slots and variable names is stored as a consecutive array. A reverse index is constructed at runtime.

## Scopes

In Inkstone, functions can have variables referenced in closures. Variables are stored in fixed-size data structures called scopes.

Scopes have their size determined at creation time. Each local variable is stored in a slot of its corresponding scope. Scopes are reference types in IR. In rare cases (e.g. REPLs) the scope can be expanded at runtime. This is done by resizing the underlying array "manually" each time.

```ruby
# main.ist
let foo = 123

def add val = begin
    let bar = val + 3
    print bar
    \a -> begin
        let baz = val + a
        val = baz
        baz + foo
    end
end
```

Exported data are seen from the outside as an object the module exports. For example, the following two declarations are equivalent:

```ruby
# lib.ist
let foo = ...
pub let bar = ...

def baz = ...
pub def qux = ...
```

```ruby
# lib.ist
let foo = ...
let bar = ...
let baz = \-> ...
let qux = \-> ...

<exported> = { bar, qux }
```

### Code

```ruby
# main.ist
use std.result.Result
let a = Result.ok "hi"
let b = std.result.Result.ok "hi"

pub def foo = Result.ok "hi"
```

translates into IR as

```
function .init:
    # use std.result.Result
    PushModuleScope
    PushGlobalScope
    PushConst :std
    PushConst :result
    PushConst :Result
    LoadScopeChain 3    # std.result.Result
    StoreScope :Result  # Module.Result

    # let a = Result.ok "hi"
    PushModuleScope
    LoadLocal r1        # Result
    LoadField :ok
    PushConst "hi"
    Call 1
    StoreScope :a       # a

    # let b = std.result.Result.ok "hi"
    PushModuleScope
    PushGlobalScope
    PushConst :std
    PushConst :result
    PushConst :Result
    PushConst :ok
    LoadScopeChain 4    # std.result.Result.ok
    PushConst "hi"
    Call 1
    StoreScope :b       # b

    PushModuleScope
    LoadScope :.exports
    PushModuleScope
    LoadScope :foo
    StoreField :foo

function foo:
    PushModuleScope
    LoadScope :Result
    LoadField :ok

    PushConst "hi"
    Call 1
    PushConst :b

    Return

```
