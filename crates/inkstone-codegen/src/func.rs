use std::collections::{BTreeMap, HashMap};

use crate::scope::{LexicalScope, ScopeMap, ScopeType};
use inkstone_syn::ast::{AstNode, BlockScope, Expr, FuncDef, IdentExpr, Stmt};

/// Type used to build a constant table
#[derive(Debug, Default)]
pub struct ConstantTableBuilder {}

/// The context used when building a function
#[derive(Debug)]
pub struct FunctionCompileCtx<'a> {
    constants: ConstantTableBuilder,
    scope_map: ScopeMap<'a>,
    errors: Vec<String>,
}

impl<'a> FunctionCompileCtx<'a> {
    pub fn in_module_scope(scope: &BlockScope) -> Self {
        let id = scope.node().text_range().start().into();
        let scope = ScopeMap::new(id, ScopeType::Module, None);

        FunctionCompileCtx {
            scope_map: scope,
            constants: Default::default(),
            errors: vec![],
        }
    }

    pub fn in_function_scope(scope: FuncDef) -> Self {
        let id = scope.node().text_range().start().into();
        let scope = ScopeMap::new(id, ScopeType::Function, None);

        FunctionCompileCtx {
            scope_map: scope,
            constants: Default::default(),
            errors: vec![],
        }
    }

    pub fn compile_block_scope(&mut self, scope: BlockScope) {
        for stmt in scope.stmt() {
            self.compile_stmt(stmt);
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(v) => {
                self.compile_expr(v.expr().unwrap());
                // TODO: pop
            }
            Stmt::Def(v) => todo!(),
            Stmt::Let(v) => todo!(),
            Stmt::Use(v) => todo!(),
        }
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Binary(v) => todo!(),
            Expr::Unary(v) => todo!(),
            Expr::FunctionCall(v) => todo!(),
            Expr::Ident(v) => todo!(),
            Expr::Namespace(v) => todo!(),
            Expr::Subscript(v) => todo!(),
            Expr::Dot(v) => todo!(),
            Expr::If(v) => todo!(),
            Expr::While(v) => todo!(),
            Expr::For(v) => todo!(),
            Expr::Block(v) => todo!(),
            Expr::Literal(v) => todo!(),
        }
    }

    fn compile_ident_expr(&mut self, id: IdentExpr) {
        let name = id.ident();
    }
}
