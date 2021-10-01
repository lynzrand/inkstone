//! Pratt parsing utilities and definitions.
//!
//! This module contains utilities for pratt-parsing Inkstone's expressions.

use crate::node::SynTag;

/*
   Precedence from high to low:

       - Primary Expr: Var, Literal, Namespace, Blocks, If/While/For loops
       - Dot/Subscript Expr `x.a` `x[a]`

       - Func call `func a b`

       - Unary Op `!x`
       - Power Op `x ** y`
       - Multiplicative Op `x * y` `x / y` `x % y`
       - Bitwise Op `x & y` `x | y` `x ^ y`
       - Additive Op `x + y` `x - y`

       - Comparison Op `x < y` `x > y` `x <= y` `x >= y` `x == y` `x != y`
       - Unary Logical Op `not x`
       - Binary And Op `x and y`
       - Binary Or Op`x or y`
       - Assignment Op `x = y`

   Binding power are precedence combined with associativity. The basic unit in
   precedence is 10, and every left/right associativity alternate this by 1.
*/

use SynTag::*;

#[derive(Debug, Clone, Copy)]
pub enum Infix {
    Left(i32),
    Right(i32),
}

impl Infix {
    pub fn binding_power(self) -> (i32, i32) {
        match self {
            Infix::Left(i) => (i + 1, i),
            Infix::Right(i) => (i, i + 1),
        }
    }
}

pub fn prefix_binding_power(op: SynTag) -> Option<i32> {
    match op {
        NotKw => Some(40),
        Add | Sub | Not => Some(100),
        _ => None,
    }
}

pub fn postfix_binding_power(_op: SynTag) -> Option<i32> {
    match _op {
        // Start a function call; but this operation does not need an additional operator
        // op if op.can_start_expr() && infix_binding_power(op).is_none() => Some(100),
        _ => None,
    }
}

pub fn infix_binding_power(op: SynTag) -> Option<Infix> {
    match op {
        Assign => Infix::Right(10).into(),
        OrKw => Infix::Right(20).into(),
        AndKw => Infix::Left(30).into(),

        // comparison operators should not be directly chainable, but here are precedences anyway
        Lt | Gt | Le | Ge | Eq | Neq => Infix::Right(50).into(),

        Add | Sub => Infix::Left(60).into(),
        Amp | Bar | BitXor => Infix::Left(70).into(),
        Mul | Div | Rem => Infix::Left(80).into(),
        Pow => Infix::Left(90).into(),

        Dot | LBracket => Infix::Right(120).into(),

        _ => None,
    }
}

pub const FUNCTION_CALL_PRECEDENCE: i32 = 110;
