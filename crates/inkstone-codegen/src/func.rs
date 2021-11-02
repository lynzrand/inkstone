use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::scope::{LexicalScope, Scope, ScopeType};
use crate::SymbolListBuilder;
use inkstone_bytecode::inst::{write_inst, IParamType, Inst};
use inkstone_syn::ast::{AstNode, BlockScope, Expr, FuncDef, IdentExpr, Stmt};

#[derive(Debug)]
struct BasicBlock {
    inst: Vec<u8>,
    jmp: Inst,
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            inst: vec![],
            jmp: Inst::Nop,
        }
    }

    pub fn emit_p(&mut self, inst: Inst, param: impl IParamType) {
        write_inst(&mut self.inst, inst, param)
    }

    pub fn emit(&mut self, inst: Inst) {
        write_inst(&mut self.inst, inst, ())
    }

    pub fn set_jmp(&mut self, inst: Inst) {
        self.jmp = inst;
    }

    /// Validates the underlying buffer to be valid bytecode.
    ///
    /// Returns `None` if valid. If invalid, the first invalid data offset is returned as `Some(offset)`.
    pub fn validate(&self) -> Option<usize> {
        let mut buf = &self.inst[..];
        while !buf.is_empty() {
            let inst = Inst::from_ordinal(buf[0]);
            if let Some(inst) = inst {
                if let Some(param) = inst.param_type() {
                    if !param.validate(&mut buf) {
                        return Some(self.inst.len() - buf.len());
                    }
                }
            } else {
                return Some(self.inst.len() - buf.len());
            }
        }
        None
    }
}

/// Type used to build a constant table
#[derive(Debug, Default)]
pub struct ConstantTableBuilder {}

/// The context used when building a function
#[derive(Debug)]
pub struct FunctionCompileCtx<'a> {
    symbol_list: Rc<RefCell<SymbolListBuilder>>,
    constants: ConstantTableBuilder,
    scope_map: Scope<'a>,
    errors: Vec<String>,

    basic_blocks: Vec<BasicBlock>,
}

impl<'a> FunctionCompileCtx<'a> {
    pub fn new(scope: Scope, symbol_list: Rc<RefCell<SymbolListBuilder>>) -> FunctionCompileCtx {
        FunctionCompileCtx {
            symbol_list,
            scope_map: scope,
            constants: Default::default(),
            errors: vec![],

            basic_blocks: vec![],
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
                self.compile_expr(v.expr());
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
            Expr::Ident(v) => self.compile_ident_expr(v),
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
        if let Some((scope, offset, entry)) = self.scope_map.get(name.text()) {
            todo!()
        }
    }
}
