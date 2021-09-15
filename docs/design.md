# Design of Inkstone

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
FuncCallExpr -> Expr<type=Function> FuncCallParam*

BinaryOp -> '+' | '-' | '*' | '/' | '**' | '%' | '&&' | '||' | '&' | '|' | '^' | 'and' | 'or'
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
InterpolatedStringChar -> [^\r\n\\"$] | '\' <AnyChar>
InterpolatedStringStart -> '"' InterpolatedStringChar* '$'
InterpolatedPart -> Ident | '(' Expr ')'
InterpolatedStringMiddle -> InterpolatedStringChar* '$'
InterpolatedStringEnd -> InterpolatedStringChar* '"'
InterpolatedString -> 
    InterpolatedStringStart 
    (InterpolatedPart InterpolatedStringMiddle)* 
    InterpolatedPart InterpolatedStringEnd
NoninterpolatedString -> '"' InterpolatedStringChar* '"'
StringLiteral -> NoninterpolatedString | InterpolatedString

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
WhileExpr -> 'while’ Expr<Ty=bool> (EOL | ':') Stmt* Expr? 'end'

Expr -> 
    | ParenExpr
    | UnaryExpr
    | BinaryExpr
    | FuncCallExpr
    | LiteralExpr
    | IfExpr
    | BlockExpr

# BlockInnerExpr: if Expr exists: (-> Expr::Ty) else: (-> nil)
BlockInnerExpr -> Stmt* Expr?

# BlockExpr: BlockInnerExpr::Ty
BlockExpr -> 'do' BlockInnerExpr 'end'

ExprStmt -> Expr

StmtInner -> ExprStmt | UseStmt | FuncDef | ModuleDef
Stmt -> StmtInner (EOL | ';')


FuncParam -> 'implicit'? Ident (':' Type)? ('=' Expr<Const>)?
FuncParamList -> FuncParam (',' FuncParam)*
# FuncDef: Fn(...FuncParamList::Ty) -> Return::Ty ?? nil
FuncDef -> 'def' Ident '(' FuncParamList ')' ('->' Return=Ty)? BlockExpr

ModuleDef -> 'mod' Ident Block?
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

## Continuations
