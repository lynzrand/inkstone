use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::error::CompileError;
use crate::scope::{self, LexicalScope, Scope, ScopeEntry, ScopeType};
use crate::SymbolListBuilder;
use fnv::FnvHashMap;
use inkstone_bytecode::inst::{self, write_inst, IParamType, Inst};
use inkstone_syn::ast::{
    AssignExpr, AstNode, BinaryExpr, BinaryOpKind, BlockExpr, BlockScope, DotExpr, Expr, ExprStmt,
    ForLoopExpr, FuncDef, FunctionCallExpr, IdentExpr, IfExpr, LambdaExpr, LetStmt, LiteralExpr,
    Stmt, SubscriptExpr, UnaryExpr, WhileLoopExpr,
};

/// A block of code with no branch inside
#[derive(Debug, Default)]
struct BasicBlock {
    inst: Vec<u8>,
    jmp: JumpInst,

    /// If true, this block will be ignored in topological sorting and deferred
    /// until all other non-deferred blocks have been already emitted.
    deferred: bool,

    /// If true, this block will be ignored as the predecessor of its successors
    ignored: bool,
}

/// A jump instruction that has not yet been transformed into bytecode
#[derive(Debug)]
enum JumpInst {
    Unknown,
    Unconditional(usize),
    Conditional(usize, usize, TopoSortAffinity),
    Return,
    ReturnNil,
}

impl Default for JumpInst {
    fn default() -> Self {
        JumpInst::Unknown
    }
}

/// Decide which branch gets emitted first when topological sorting
#[derive(Debug, PartialEq, Eq)]
enum TopoSortAffinity {
    /// Don't care which branch get emitted first
    IDontCare,
    /// Visit True branch first in topological sorting
    TrueBranch,
    /// Visit False branch first in topological sorting
    FalseBranch,
}

impl Default for TopoSortAffinity {
    fn default() -> Self {
        TopoSortAffinity::IDontCare
    }
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        Default::default()
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
    errors: Vec<CompileError>,

    basic_blocks: Vec<BasicBlock>,
    curr_bb: usize,
    // feature-specific structures
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

    fn set_curr_bb(&mut self, id: usize) {
        assert!(id <= self.basic_blocks.len());
        self.curr_bb = id;
    }

    fn new_bb(&mut self) -> usize {
        let id = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlock::default());
        id
    }

    fn emit_error(&mut self, e: CompileError) {
        self.errors.push(e);
    }
}

#[derive(Debug)]
enum LValue {
    LocalVariable(u32),
    SuperVariable,
    Subscript,
    DynamicSubscript,
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
                self.emit_error(CompileError::new("pub_let_redefinition", binding.span()));
                return;
            };

            let val = let_stmt.expr();

            self.compile_expr(val);

            self.curr_bb().emit_p(Inst::StoreLocal, local_slot as u32);
        } else {
            self.emit_error(CompileError::new("bad_binding", binding.span()));
        }
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Binary(v) => self.compile_binary_expr(v),
            Expr::Assign(v) => self.compile_assign_expr(v),
            Expr::Unary(v) => self.compile_unary_expr(v),
            Expr::FunctionCall(v) => self.compile_function_call_expr(v),
            Expr::Ident(v) => self.compile_ident_expr(v),
            Expr::Subscript(v) => self.compile_subscript_expr(v),
            Expr::Dot(v) => self.compile_dot_expr(v),
            Expr::If(v) => self.compile_if_expr(v),
            Expr::While(v) => self.compile_while_loop_expr(v),
            Expr::For(v) => self.compile_for_loop_expr(v),
            Expr::Block(v) => self.compile_block_expr(v),
            Expr::Literal(v) => self.compile_literal_expr(v),
            Expr::Lambda(v) => self.compile_lambda_expr(v),
        }
    }

    fn compile_binary_expr(&mut self, v: BinaryExpr) {
        let op_kind = v.op().kind();
        if matches!(
            op_kind,
            BinaryOpKind::ShortCircuitOr | BinaryOpKind::ShortCircuitAnd
        ) {
            return self.compile_shortcircuit_binary_expr(v);
        }

        self.compile_expr(v.lhs());
        self.compile_expr(v.rhs());

        // all these operators emit a single instruction with no parameter
        let emitted = match op_kind {
            BinaryOpKind::Lt => Inst::Lt,
            BinaryOpKind::Gt => Inst::Gt,
            BinaryOpKind::Le => Inst::Le,
            BinaryOpKind::Ge => Inst::Ge,
            BinaryOpKind::Eq => Inst::Eq,
            BinaryOpKind::Ne => Inst::Ne,
            BinaryOpKind::Add => Inst::Add,
            BinaryOpKind::Sub => Inst::Sub,
            BinaryOpKind::BitAnd => Inst::BitAnd,
            BinaryOpKind::BitOr => Inst::BitOr,
            BinaryOpKind::BitXor => Inst::BitXor,
            BinaryOpKind::Mul => Inst::Mul,
            BinaryOpKind::Div => Inst::Div,
            BinaryOpKind::Rem => Inst::Rem,
            BinaryOpKind::Pow => Inst::Pow,

            // these two are specially treated
            BinaryOpKind::ShortCircuitOr | BinaryOpKind::ShortCircuitAnd => unreachable!(),
        };

        self.curr_bb().emit(emitted);
    }

    /// Compile a short-circuiting binary expression, like `a and b` and `a or b`.
    ///
    /// If the `short_circuiting` param is `Some((a, b))`, then `a` is the basic
    /// block the code should jump to if this value evaluates to `true`, and `b`
    /// is that if it evaluates to `false`.
    fn compile_shortcircuit_binary_expr(&mut self, v: BinaryExpr) {
        /*
            The way short circuiting is implemented is very simple:

            For `and`s:
                evaluate lhs
                dup
                jump if_false -> end, if_true -> if_true
            if_true:
                pop
                evaluate rhs
            end:
                (end)

            for `or`s:
                evaluate lhs
                dup
                jump if_false->if_false, if_true -> end
            if_false:
                pop
                evaluate rhs
            end:
                (end)
        */
        let i_bb = self.new_bb();
        let next_bb = self.new_bb();

        if v.op().kind() == BinaryOpKind::ShortCircuitAnd {
            self.compile_expr(v.lhs());
            self.curr_bb().emit(Inst::Dup);
            self.curr_bb().set_jmp(JumpInst::Conditional(
                i_bb,
                next_bb,
                TopoSortAffinity::TrueBranch,
            ));

            self.set_curr_bb(i_bb);
            self.curr_bb().emit(Inst::Pop);
            self.compile_expr(v.rhs());
            self.curr_bb().set_jmp(JumpInst::Unconditional(next_bb));
        } else if v.op().kind() == BinaryOpKind::ShortCircuitOr {
            self.compile_expr(v.lhs());
            self.curr_bb().emit(Inst::Dup);
            self.curr_bb().set_jmp(JumpInst::Conditional(
                next_bb,
                i_bb,
                TopoSortAffinity::FalseBranch,
            ));

            self.set_curr_bb(i_bb);
            self.curr_bb().emit(Inst::Pop);
            self.compile_expr(v.rhs());
            self.curr_bb().set_jmp(JumpInst::Unconditional(next_bb));
        } else {
            unreachable!("Should not pass non-short-circuiting operator here");
        }
        self.set_curr_bb(next_bb);
    }

    fn compile_assign_expr(&mut self, v: AssignExpr) {
        let l_val = match self.compile_left_value(v.tgt()) {
            Some(l_val) => l_val,
            None => return,
        };
        self.compile_expr(v.val());

        match l_val {
            LValue::LocalVariable(slot) => {
                self.curr_bb().emit_p(Inst::StoreLocal, slot);
            }
            LValue::SuperVariable => todo!(),
            LValue::Subscript => todo!(),
            LValue::DynamicSubscript => todo!(),
        }
    }

    /// Compile the given expression as a LValue. If failed, emit the error
    /// inside and returns `None`.
    fn compile_left_value(&mut self, v: Expr) -> Option<LValue> {
        match v {
            Expr::Ident(i) => self.compile_ident_left_value(i),
            Expr::Subscript(v) => self.compile_subscript_left_value(v),
            Expr::Dot(v) => self.compile_dot_left_value(v),
            _ => {
                self.emit_error(CompileError::new("not_left_value", v.span()));
                None
            }
        }
    }

    fn compile_ident_left_value(&mut self, id: IdentExpr) -> Option<LValue> {
        let name = id.ident();
        if let Some((scope, offset, _entry)) = self.scope_map.get(name.text()) {
            if scope == self.scope_map.id() {
                // local variable
                Some(LValue::LocalVariable(offset))
            } else {
                // captured variable
                self.emit_error(
                    CompileError::new("unsupported_capture", id.span())
                        .with_message("Capturing external variables is not yet supported"),
                );
                Some(LValue::SuperVariable) // TODO: add scope here
            }
        } else {
            self.emit_error(CompileError::new("unknown_ident", id.span()));
            None
        }
    }

    fn compile_subscript_left_value(&mut self, v: SubscriptExpr) -> Option<LValue> {
        self.compile_expr(v.parent());
        self.compile_expr(v.subscript());
        Some(LValue::DynamicSubscript)
    }

    fn compile_dot_left_value(&mut self, v: DotExpr) -> Option<LValue> {
        self.compile_expr(v.parent());
        let name = v.subscript().text();
        // TODO: insert name into symbol list
        Some(LValue::Subscript)
    }

    fn compile_unary_expr(&mut self, v: UnaryExpr) {
        let op = v.op();
        match op.kind() {
            inkstone_syn::ast::UnaryOpKind::NotKw | inkstone_syn::ast::UnaryOpKind::Not => {
                self.compile_expr(v.lhs());
                self.curr_bb().emit(Inst::Not);
            }
            inkstone_syn::ast::UnaryOpKind::Pos => {
                self.compile_expr(v.lhs());
            }
            inkstone_syn::ast::UnaryOpKind::Neg => {
                self.curr_bb().emit_p(Inst::PushI32, 0i32);
                self.compile_expr(v.lhs());
                self.curr_bb().emit(Inst::Sub);
            }
        }
    }

    fn compile_function_call_expr(&mut self, v: FunctionCallExpr) {
        todo!()
    }

    /// Compile an identifier expression (RValue).
    fn compile_ident_expr(&mut self, id: IdentExpr) {
        let name = id.ident();
        if let Some((scope, offset, _entry)) = self.scope_map.get(name.text()) {
            if scope == self.scope_map.id() {
                // local variable
                self.curr_bb().emit_p(Inst::LoadLocal, offset);
            } else {
                // captured variable
                self.emit_error(
                    CompileError::new("unsupported_capture", id.span())
                        .with_message("Capturing external variables is not yet supported"),
                )
            }
        } else {
            self.emit_error(CompileError::new("unknown_ident", id.span()));
            // error recovery: insert this variable. It is now undefined.
            self.scope_map.insert(
                name.text().into(),
                ScopeEntry {
                    kind: scope::ScopeEntryKind::Variable,
                    is_public: false,
                },
            );
        }
    }

    fn compile_subscript_expr(&mut self, v: SubscriptExpr) {
        todo!()
    }

    fn compile_dot_expr(&mut self, v: DotExpr) {
        todo!()
    }

    fn compile_if_expr(&mut self, v: IfExpr) {
        todo!()
    }

    fn compile_while_loop_expr(&mut self, v: WhileLoopExpr) {
        todo!()
    }

    fn compile_for_loop_expr(&mut self, v: ForLoopExpr) {
        todo!()
    }

    fn compile_block_expr(&mut self, v: BlockExpr) {
        self.compile_block_scope(v.scope());
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

    fn compile_lambda_expr(&mut self, v: LambdaExpr) {
        todo!()
    }
}

impl FunctionCompileCtx<'_> {
    pub fn finalize(self) {}
}
