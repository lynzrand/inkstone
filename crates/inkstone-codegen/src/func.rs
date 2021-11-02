use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::scope::{LexicalScope, Scope, ScopeEntry, ScopeType};
use crate::SymbolListBuilder;
use fnv::FnvHashMap;
use inkstone_bytecode::inst::{write_inst, IParamType, Inst};
use inkstone_syn::ast::{
    AstNode, BlockScope, Expr, ExprStmt, FuncDef, IdentExpr, LetStmt, LiteralExpr, Stmt,
};

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

    pub fn write_param(&mut self, param: impl IParamType) {
        param.write(&mut self.inst)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Int64(i64),
    Float64(u64),
    String,
    Symbol,
    Closure,
}

/// Type used to build a constant table
#[derive(Debug, Default)]
pub struct ConstantTableBuilder {
    constants: Vec<Constant>,
    reverse_map: FnvHashMap<Constant, u32>,
}

impl ConstantTableBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, v: Constant) -> u32 {
        if let Some(&v) = self.reverse_map.get(&v) {
            return v;
        }

        let id = self.constants.len();
        assert!(
            id < u32::MAX as usize,
            "Cannot allocate more than 32 constants"
        );
        let id = id as u32;

        self.constants.push(v.clone());
        self.reverse_map.insert(v, id);
        id
    }
}

/// The context used when building a function
#[derive(Debug)]
pub struct FunctionCompileCtx<'a> {
    symbol_list: Rc<RefCell<SymbolListBuilder>>,
    constants: ConstantTableBuilder,
    scope_map: Scope<'a>,
    errors: Vec<String>,

    basic_blocks: Vec<BasicBlock>,
    curr_bb: usize,
}

impl<'a> FunctionCompileCtx<'a> {
    pub fn new(scope: Scope, symbol_list: Rc<RefCell<SymbolListBuilder>>) -> FunctionCompileCtx {
        FunctionCompileCtx {
            symbol_list,
            scope_map: scope,
            constants: Default::default(),
            errors: vec![],

            basic_blocks: vec![BasicBlock::new()],
            curr_bb: 0,
        }
    }

    pub fn compile_block_scope(&mut self, scope: BlockScope) {
        for stmt in scope.stmt() {
            self.compile_stmt(stmt);
        }
    }

    fn curr_bb(&mut self) -> &mut BasicBlock {
        self.basic_blocks
            .get_mut(self.curr_bb)
            .expect("`curr_bb` refers to an non-existant basic block. What?")
    }

    fn emit_error(&mut self, e: String) {
        self.errors.push(e);
    }
}

impl<'a> FunctionCompileCtx<'a> {
    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(v) => self.compile_expr_stmt(v),
            Stmt::Def(v) => todo!(),
            Stmt::Let(v) => self.compile_let_stmt(v),
            Stmt::Use(v) => todo!(),
        }
    }

    fn compile_expr_stmt(&mut self, v: ExprStmt) {
        self.compile_expr(v.expr());
        // pop the remaining value and call it a day
        self.curr_bb().emit(Inst::Pop);
    }

    fn compile_let_stmt(&mut self, let_stmt: LetStmt) {
        let binding = let_stmt.binding();
        if let Some(name) = binding.name() {
            // this definition binds to a name
            let name = name.text();

            let local_slot = self.scope_map.insert(name.into(), ScopeEntry {});
            let val = let_stmt.expr();

            self.compile_expr(val);

            self.curr_bb().emit_p(Inst::StoreLocal, local_slot as u32);
        } else {
            self.emit_error(format!("No such binding as {}", binding));
        }
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Binary(v) => todo!(),
            Expr::Assign(v) => todo!(),
            Expr::Unary(v) => todo!(),
            Expr::FunctionCall(v) => todo!(),
            Expr::Ident(v) => self.compile_ident_expr(v),
            Expr::Subscript(v) => todo!(),
            Expr::Dot(v) => todo!(),
            Expr::If(v) => todo!(),
            Expr::While(v) => todo!(),
            Expr::For(v) => todo!(),
            Expr::Block(v) => todo!(),
            Expr::Literal(v) => self.compile_literal_expr(v),
        }
    }

    fn compile_ident_expr(&mut self, id: IdentExpr) {
        let name = id.ident();
        if let Some((scope, offset, entry)) = self.scope_map.get(name.text()) {
            todo!()
        }
    }

    fn compile_literal_expr(&mut self, lit: LiteralExpr) {
        if let Some(i) = lit.as_int() {
            let i = i
                .text()
                .parse::<i64>()
                .expect("TODO: emit an error for this");
            if i >= i32::MIN as i64 && i <= i32::MAX as i64 {
                self.curr_bb().emit_p(Inst::PushI32, i as i32);
            } else {
                let id = self.constants.insert(Constant::Int64(i));
                self.curr_bb().emit_p(Inst::PushConst, id);
            }
        } else if let Some(f) = lit.as_float() {
            let f = f
                .text()
                .parse::<f64>()
                .expect("TODO: emit an error for this");
            let id = self.constants.insert(Constant::Float64(f.to_bits()));
            self.curr_bb().emit_p(Inst::PushConst, id);
        } else if lit.as_true().is_some() {
            self.curr_bb().emit(Inst::PushTrue);
        } else if lit.as_false().is_some() {
            self.curr_bb().emit(Inst::PushFalse);
        } else if let Some(s) = lit.as_string() {
            todo!("match string")
        } else if let Some(sym) = lit.as_symbol() {
            todo!("match symbol")
        } else {
            panic!("LiteralExpr has unknown variant")
        }
    }
}
