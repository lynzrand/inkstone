# Design of Inkstone

## Introduction

_Inkstone is NOT a markup language._

Inkstone was created as the author finds markup languages for game scripts are overwhelmed with unnecessary special symbols and tags or just being too domain-specific. These languages include:

- [RenPy](https://www.renpy.org/)
- [Ink](https://github.com/inkie/ink)
- [Kirikiri](http://kirikirikag.sourceforge.net/contents/index.html)

As a result, Inkstone is here for an experimental general-purpose language, that tries to make scripts less frustrating to work with. Inkstone tries to achieve this through a careful selection of language features, grammar and some clever library design.

Inkstone takes ideas from many other languages, like Lisp, OCaml, Elixir, Ruby, Haskell and Javascript.

## Features

- Duck typing & prototype-based OOP
- First class functions & continuations (fully async)
- ADT
  - Tuple, Vector, Map
- Implicit params

## OOP

OOP is achieved by using prototypes. Tuples and Maps can set their prototypes to use object methods.

```ruby
mod std.result

def new val = std.make val Result
def ok val = new (:ok, val)
def err err = new (:err, err)
def map self fn =
  match self 
    (:ok, val) -> (:ok, fn val)
    (:err, e) -> (:err, e)
  end

pub let Result = {
  ok,
  err,
  map
}

mod sample 
    let my_result = Result.ok "Yay!"
    let my_err = Result.err "Error!"

    my_result.map \x -> x + " -- from a result"
end
```

## Typings

Inkstone has the following primitive types:

- Int
- Float
- Boolean
- Symbol (aka Atom)
- Function
- Nil

Inkstone also has the following compound types:

- Tuple (A heterogeneous immutable list of values)
- Array (A probably homogeneous mutable list of values)
- Map (A list of 2-tuples indexed on their first value)

Tuples and Maps also has an optional value bound to them called the `prototype`. The prototype provides support for object methods and dynamic dispatching. If a property doesn't exist on an object, it will be searched in its prototype.

Operator overloading is achieved via magic methods.

### Type Annotation

Inkstone _should_ include an optional type annotation system. However, it might not be included in the homework since time limit is tight.

Type annotation is written as `::` followed by a Type (when the type can be expressed in a single identifier), or a type wrapped in parenthesis. The return type of a function is written as an arrow `->` followed by a Type.

A type's name is an identifier, like `Result`. If the type has additional parameters, it's written like a function call, like `Result T E`. Nested types are written in parentheses.

Internal types have their own type annotation. Tuples are represented as `()`, `(T1,)`, and `(T1, T2, ...)`. Vectors are represented as `Vec T`. Homogeneous maps are represented as `Map K V`, and heterogeneous maps are represented as `{key1: V1, ...}`.

Types are declared as `'pub'? 'type' Ty (= Definition)?`. If a variable at the same scope has the same name as the type, that variable is viewed as the prototype of this type.

The above example, with types annotated:

```ruby
type ResultTag = :ok | :err

def new val::((:ok, T) | (:err, E)) -> Result T E = std.make val Result
def ok val::T -> Result T E = new (:ok, val)
def err err::E -> Result T E = new (:err, err)
def map self::(Result T E) fn::(Fn T -> U) -> Result U E =
  match self 
    (:ok, val) -> (:ok, fn val)
    (:err, e) -> (:err, e)
  end

pub type Result T E = (:ok, T) | (:err, E)
pub let Result = {
  ok,
  err,
  map
}
```

## Closures

The following pseudocode should do the job.

```
#[repr(C)]
struct Closure<ReturnTy, Params...> {
    env: Gc<ClosureCapture>,
    func: Fn(Gc<ClosureCapture>, Params...) -> ReturnTy
}

impl<ReturnTy, Params...> Fn(Params...) for Closure<ReturnTy, Params...> {
    fn call(&self, params: (Params...)) -> ReturnTy {
        self.func(self.env.clone(), ...params)
    }
}
```

## Continuations and Coroutines


## Iterators and other interfaces

TODO: How to design interfaces in a 
