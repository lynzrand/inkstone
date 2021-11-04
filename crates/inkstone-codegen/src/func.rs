use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::scope::{self, LexicalScope, Scope, ScopeEntry, ScopeType};
use crate::SymbolListBuilder;
use fnv::FnvHashMap;
use inkstone_bytecode::inst::{write_inst, IParamType, Inst};
use inkstone_syn::ast::{
    AstNode, BlockScope, Expr, ExprStmt, FuncDef, IdentExpr, LetStmt, LiteralExpr, Stmt,
};

#[derive(Debug)]
struct BasicBlock {
    inst: Vec<u8>,
    jmp: JumpInst,
}

/// A jump instruction that is
#[derive(Debug)]
enum JumpInst {
    Unknown,
    Unconditional(usize),
    Conditional(usize, usize),
    Return,
    ReturnNil,
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            inst: vec![],
            jmp: JumpInst::Unknown,
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

    pub fn set_jmp(&mut self, inst: JumpInst) {
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

    fn curr_bb(&mut self) -> &mut BasicBlock {
        self.basic_blocks
            .get_mut(self.curr_bb)
            .expect("`curr_bb` refers to an non-existant basic block. What?")
    }

    fn set_curr_bb(&mut self, id: usize) {}

    fn emit_error(&mut self, e: String) {
        self.errors.push(e);
    }
}

impl<'a> FunctionCompileCtx<'a> {
    pub fn compile_module_scope(&mut self, scope: BlockScope) {
        // Compiling module scope needs to scan for public definitions
        // before compiling anything else
        for stmt in scope.stmt() {
            match stmt {
                Stmt::Expr(_) => {} // no-op: expressions aren't public
                Stmt::Def(v) => self.scope_scan_def(v),
                Stmt::Let(v) => self.scope_scan_let(v),
                Stmt::Use(_v) => {
                    // `use foo` is not yet getting parsed
                }
                Stmt::Mod(v) => {}
            }
        }

        self.compile_block_scope(scope)
    }

    fn scope_scan_let(&mut self, v: LetStmt) {
        let is_public = v.vis().and_then(|v| v.public());
        if is_public.is_some() {
            let name = v.binding().name();
            let name = name.as_ref().map(|t| t.text());
            if let Some(t) = name {
                self.scope_map.insert(
                    t.into(),
                    ScopeEntry {
                        kind: scope::ScopeEntryKind::Variable,
                        is_public: true,
                    },
                );
            }
        }
    }

    fn scope_scan_def(&mut self, def: FuncDef) {
        let is_public = def.vis().and_then(|v| v.public());
        if is_public.is_some() {
            let name = def.name().name();
            let name = name.text();
            self.scope_map.insert(
                name.into(),
                ScopeEntry {
                    kind: scope::ScopeEntryKind::Function,
                    is_public: true,
                },
            );
        }
    }

    pub fn compile_block_scope(&mut self, scope: BlockScope) {
        for stmt in scope.stmt() {
            self.compile_stmt(stmt);
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(v) => self.compile_expr_stmt(v),
            Stmt::Def(v) => todo!(),
            Stmt::Let(v) => self.compile_let_stmt(v),
            Stmt::Use(v) => todo!(),
            Stmt::Mod(v) => todo!(),
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

            let vis = let_stmt.vis();
            let pub_vis = vis.and_then(|v| v.public());

            let local_slot = if pub_vis.is_none() || self.scope_map.get_local(name).is_none() {
                // Note:
                //
                // If the variable is not public, subsequent values can mask
                // previous values.
                //
                // If it's public and at the top-level scope, we would have
                // already inserted it at the initial scope scan.

                self.scope_map.insert(
                    name.into(),
                    ScopeEntry {
                        kind: scope::ScopeEntryKind::Variable,
                        is_public: pub_vis.is_some(),
                    },
                )
            } else {
                self.emit_error(format!("Redefinition of public variable `{}`", name));
                return;
            };

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
        match lit.kind() {
            inkstone_syn::ast::LiteralKind::Int => {
                let i = lit
                    .token()
                    .text()
                    .parse::<i64>()
                    .expect("TODO: emit an error for this");
                if i >= i32::MIN as i64 && i <= i32::MAX as i64 {
                    self.curr_bb().emit_p(Inst::PushI32, i as i32);
                } else {
                    let id = self.constants.insert(Constant::Int64(i));
                    self.curr_bb().emit_p(Inst::PushConst, id);
                }
            }
            inkstone_syn::ast::LiteralKind::Float => {
                let f = lit
                    .token()
                    .text()
                    .parse::<f64>()
                    .expect("TODO: emit an error for this");
                let id = self.constants.insert(Constant::Float64(f.to_bits()));
                self.curr_bb().emit_p(Inst::PushConst, id);
            }
            inkstone_syn::ast::LiteralKind::True => self.curr_bb().emit(Inst::PushTrue),
            inkstone_syn::ast::LiteralKind::False => self.curr_bb().emit(Inst::PushFalse),
            inkstone_syn::ast::LiteralKind::Nil => self.curr_bb().emit(Inst::PushNil),
            inkstone_syn::ast::LiteralKind::String => todo!(),
            inkstone_syn::ast::LiteralKind::Symbol => todo!(),
        }
    }
}
