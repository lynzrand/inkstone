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


## Spec

```
EOL -> '\n'
Ident -> [a-zA-Z0-9_?!]
Namespace -> Ident ('::' Ident)*

Type -> Namespace
Var -> Namespace | Ident

UseStmt -> 'use' Namespace EOL

LetStmt -> 'let' Ident<Var> (':' Type) '=' Expr

FuncCallParam -> Expr | Ident<FuncParam> ':' Expr
FuncCallExpr -> '(' Expr<type=Function> ')' | Expr<type=Function> FuncCallParam+
# Functions with no params must be called inside a pair of parenthesis.
# This is to avoid ambiguity: is `f` the value of variable `f`, or calling `f` with no params?

BinaryOp -> '+' | '-' | '*' | '/' | '**' | '%' | '&&' | '||' | '&' | '|' | '^' | '<' | '>' | '<=' | '>=' | '==' | '!=' | 'and' | 'or'
BinaryExpr -> Expr BinaryOp Expr

UnaryOp -> '!'
UnaryExpr -> UnaryOp Expr

# ParenExpr: -> Expr::Ty
ParenExpr -> '(' Expr ')'

IntLiteral -> _
FloatLiteral -> _

# StringLiteral doesn't check for validity of escape sequences.
# It just let the next character after '\' carry on as a normal char, instead of
# an interpolated sequence or the ending quote of the string.
# InterpolatedStringChar -> [^\r\n\\"$] | '\' <AnyChar>
StringChar -> [^\r\n\\"] | '\' <AnyChar>
# InterpolatedStringStart -> '"' InterpolatedStringChar* '$'
# InterpolatedPart -> Ident | '{' Expr '}'
# InterpolatedStringMiddle -> InterpolatedStringChar* '$'
# InterpolatedStringEnd -> InterpolatedStringChar* '"'
# InterpolatedString -> 
#     InterpolatedStringStart 
#     (InterpolatedPart InterpolatedStringMiddle)* 
#     InterpolatedPart InterpolatedStringEnd
StringLiteral -> '"' InterpolatedStringChar* '"'
# StringLiteral -> NoninterpolatedString | InterpolatedString

# SymbolLiteral: -> Symbol
SymbolLiteral -> ':'<>Ident

# TupleLiteral: if Single exists; (-> Tuple<Single::Ty>) else (-> Tuple<First::Ty, ...Other::Ty>)
TupleLiteral -> '(' First=Expr (',' Other=Expr)+ ','? ')' | '(' Single=Expr ',' ')'

# ArrayLiteral: -> type_upper_bound(...Expr::Ty)
ArrayLiteral -> '[' (Expr (',' Expr)* ','?)? ']'

# ObjectLiteral: -> Object
ObjectKey -> Ident | '[' Expr ']'
ObjectLiteral -> '{' (ObjectKey ':' Expr (',' ObjectKey ':' Expr)* ','? ) '}'

LiteralExpr -> 
    | IntLiteral 
    | FloatLiteral 
    | StringLiteral 
    | TupleLiteral 
    | ArrayLiteral 
    | ObjectLiteral
    | SymbolLiteral

# IfExpr: -> type_upper_bound(IfBlock::Ty, ...ElseIfBlock::Ty, ElseBlock::Ty)
IfExpr -> 'if' Expr<Ty=bool> (EOL | ':') IfBlock=BlockInnerExpr
    ('else' 'if' Expr<Ty=bool> (EOL | ':') ElseIfBlock=BlockInnerExpr)?
    ('else' ':' ElseBlock=BlockInnerExpr)?
    'end'

# WhileExpr: if no `'break' Value` inside body: (-> nil) else (-> type_upperbound(...Value))
WhileExpr -> 'while’ Expr<Ty=bool> (EOL | ':') BlockInnerExpr 'end'

# ClosureExpr: -> Fn(...FuncParamList::Ty) -> Expr::Ty
ClosureExpr -> '\' FuncParamList? '->' Expr

# ReturnExpr: -> !
ReturnExpr -> 'return' Expr

# VarExpr: -> Scope::ident(Ident)::Ty
VarExpr -> Ident

# DotExpr: -> Expr.ident::Ty
DotExpr -> Expr '.' Ident

# SubscriptExpr: -> Receiver::operator[](Subscript::Ty)::Ty
SubscriptExpr -> Receiver=Expr '[' Subscript=Expr ']'

LValue -> VarExpr | DotChildExpr | SubscriptExpr
# AssignExpr -> nil
AssignExpr -> LValue '=' Expr

Expr -> 
    | ParenExpr
    | UnaryExpr
    | BinaryExpr
    | FuncCallExpr
    | VarExpr
    | DotExpr
    | SubscriptExpr
    | LiteralExpr
    | IfExpr
    | BlockExpr

# BlockInnerExpr: if len(Stmt) > 0: (-> Stmt[-1]::Ty) else: (-> nil)
BlockInnerExpr -> Stmt*

# BlockExpr: BlockInnerExpr::Ty
BlockExpr -> 'begin' BlockInnerExpr 'end'

ExprStmt -> Expr

StmtInner -> ExprStmt | UseStmt | FuncDef | ModuleDef
Stmt -> StmtInner (EOL | ';')


FuncParam -> Ident
FuncParamList -> FuncParam (',' FuncParam)*
FuncDef -> 'def' Ident FuncParamList BlockExpr

ModuleDef -> 'mod' Ident Block?
```

### Precedence

From high to low:

- Primary Expr: Var, Literal, Namespace, Blocks, If/While/For loops
- Dot/Subscript Expr `x.a` `x[a]`
- Lambda `\x -> block`, 
- Func call `func a b`
- Unary Op `!x`
- Power Op `x ** y`
- Multiplicative Op `x * y` `x / y` `x % y`
- Bitwise Op `x & y` `x | y` `x ^ y`
- Additive Op `x + y` `x - y`
- Comparison Op `x < y` `x > y` `x <= y` `x >= y` `x == y` `x != y`
- Unary Logical Op `not x`
- Binary Logical Op `x and y` `x or y`
- Assignment Op `x = y`

## OOP

OOP is achieved by prototype-based 

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

## Continuations


