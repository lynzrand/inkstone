# Inkstone VM & Bytecode design

## Scopes

In Inkstone, functions can have variables referenced in closures.

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

All variables that are referenced outside the current function body (e.g. from closures) are separated from rest of the scope, into another scope called `.escaped`, which is stored as a variable in the current scope. This creates a chain of scopes:

```
<ModuleScope>
    .escaped <Scope> #1
        foo
    add <FunctionScope>
        bar
        .escaped <Scope> #2
            val
        <closure> <FunctionScope>
            .1 -> #1
            .2 -> #2
            baz
```

and the module compiles into the following pseudocode:

```
function .init:
    # add is already declared by the runtime
    declare .escaped <- new Scope
    declare foo in .escaped <- nil

function add:
    declare .escaped <- new Scope
    declare val in .escaped <- arg1
    declare bar <- val + 3

    call print bar

    declare _1_scope <- new Scope
    declare _2 in _1_scope <- .ModuleScope.escaped
    declare _3 in _1_scope <- .escaped
    declare _1 <- new Closure closure@add_1 with _1.scope
    return _1

function closure@add_1:
    declare a <- arg1
    declare local_foo <- load foo from _2
    declare local_val <- load val from _3
    declare baz <- val + a
    store val in _3 <- baz
    declare _4 <- baz + local_foo
    return _4
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
