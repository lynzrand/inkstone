use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use crate::error::CompileError;
use crate::scope::{self, LexicalScope, Scope, ScopeEntry, ScopeType, ScopeVariable};
use crate::SymbolListBuilder;
use fnv::{FnvHashMap, FnvHashSet};
use inkstone_bytecode::inst::{self, write_inst, IParamType, Inst};
use inkstone_bytecode::{Constant, Function};
use inkstone_syn::ast::{
    AssignExpr, AstNode, BinaryExpr, BinaryOpKind, BlockExpr, BlockScope, DotExpr, Expr, ExprStmt,
    ForLoopExpr, FuncDef, FunctionCallExpr, IdentExpr, IfExpr, LambdaExpr, LetStmt, LiteralExpr,
    Stmt, SubscriptExpr, UnaryExpr, WhileLoopExpr,
};
use itertools::Itertools;
use smol_str::SmolStr;

pub struct SmolStrInterner {
    v: RefCell<FnvHashSet<SmolStr>>,
}

impl std::fmt::Debug for SmolStrInterner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SmolStrInterner").finish()
    }
}

impl SmolStrInterner {
    pub fn new() -> Self {
        Self {
            v: Default::default(),
        }
    }

    pub fn intern(&self, s: &str) -> SmolStr {
        if s.len() <= 22 {
            SmolStr::new_inline(s)
        } else {
            let mut m = self.v.borrow_mut();
            m.get(s).cloned().unwrap_or_else(|| {
                let s = SmolStr::new(s);
                m.insert(s.clone());
                s
            })
        }
    }
}

impl Default for SmolStrInterner {
    fn default() -> Self {
        Self::new()
    }
}

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

    pub fn emit_p(&mut self, inst: Inst, param: impl IParamType) -> &mut Self {
        write_inst(&mut self.inst, inst, param);
        self
    }

    pub fn write_param(&mut self, param: impl IParamType) {
        param.write(&mut self.inst)
    }

    pub fn emit(&mut self, inst: Inst) -> &mut Self {
        write_inst(&mut self.inst, inst, ());
        self
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

/// Type used to build a constant table
#[derive(Debug, Default)]
pub struct ConstantTableBuilder {
    string_interner: Rc<SmolStrInterner>,
    constants: Vec<Constant>,
    reverse_map: FnvHashMap<Constant, u32>,
    reverse_string_map: FnvHashMap<SmolStr, u32>,
}

impl ConstantTableBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_string(&mut self, s: &str) -> u32 {
        if let Some(v) = self.reverse_string_map.get(s) {
            *v
        } else {
            self.do_insert_constant(Constant::String(s.into()))
        }
    }

    pub fn insert_symbol(&mut self, s: &str) -> u32 {
        todo!()
    }

    pub fn insert(&mut self, mut v: Constant) -> u32 {
        if let Some(&v) = self.reverse_map.get(&v) {
            return v;
        }

        if let Constant::String(s) | Constant::Symbol(s) = &v {
            let new_val = self.string_interner.intern(s);
            v = match v {
                Constant::String(_) => Constant::String(new_val),
                Constant::Symbol(_) => Constant::Symbol(new_val),
                _ => unreachable!(),
            }
        }

        self.do_insert_constant(v)
    }

    fn do_insert_constant(&mut self, v: Constant) -> u32 {
        let id = self.constants.len();
        assert!(
            id < u32::MAX as usize,
            "Cannot allocate more than 2^32 constants"
        );
        let id = id as u32;
        self.constants.push(v.clone());
        match &v {
            Constant::String(s) => {
                self.reverse_string_map.insert(s.clone(), id);
            }
            _ => {}
        }
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
    UpValue(u32),
    Subscript(u32),
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

    pub fn compile_function_scope(&mut self, scope: FuncDef) {
        // ensure the params are the first n local values
        for it in scope.param_list().params() {
            self.scope_map.insert(
                it.name().text().into(),
                ScopeEntry {
                    kind: scope::ScopeEntryKind::Variable,
                    is_public: false,
                },
            );
        }
        self.compile_expr(scope.body());
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
        let mut first = true;
        for stmt in scope.stmt() {
            if first {
                first = false
            } else {
                // pop the last result out
                self.curr_bb().emit(Inst::Pop);
            }

            self.compile_stmt(stmt);
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(v) => self.compile_expr_stmt(v),
            Stmt::Def(v) => self.compile_def_stmt(v),
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

    fn compile_def_stmt(&mut self, v: FuncDef) {
        let mut cx = FunctionCompileCtx::new(
            Scope::new(
                v.node().text_range().start().into(),
                ScopeType::Function,
                Some(&self.scope_map),
            ),
            self.symbol_list.clone(),
        );
        cx.compile_function_scope(v);

        let f = Arc::new(cx.finalize());

        let c_entry = todo!("Add f to constant table");
        self.curr_bb().emit_p(Inst::PushConst, c_entry);

        let captures = [todo!("function's upvalue capture")];
        for capture in captures {
            self.curr_bb().emit_p(Inst::WithUpvalue, capture);
        }

        self.curr_bb()
            .emit_p(Inst::ClosureNew, todo!("Capture count"));

        todo!()
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
            Expr::Paren(v) => self.compile_expr(v.inner()),
            Expr::Subscript(v) => self.compile_subscript_expr(v),
            Expr::Dot(v) => self.compile_dot_expr(v),
            Expr::If(v) => self.compile_if_expr(v),
            Expr::While(v) => self.compile_while_loop_expr(v),
            Expr::For(v) => self.compile_for_loop_expr(v),
            Expr::Block(v) => self.compile_block_expr(v),
            Expr::Literal(v) => self.compile_literal_expr(v),
            Expr::Lambda(v) => self.compile_lambda_expr(v),
            Expr::Tuple(_) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Object(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Break(_) => todo!(),
            Expr::Continue(_) => todo!(),
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
            LValue::UpValue(slot) => {
                self.curr_bb().emit_p(Inst::StoreUpvalue, slot);
            }
            LValue::Subscript(const_id) => {
                self.curr_bb().emit_p(Inst::StoreField, const_id);
            }
            LValue::DynamicSubscript => {
                self.curr_bb().emit(Inst::StoreFieldDyn);
            }
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
        match self.scope_map.get_local_or_add_upvalue(name.text()) {
            Some(val) => match val {
                ScopeVariable::Local(slot) => Some(LValue::LocalVariable(slot)),
                ScopeVariable::Upvalue(slot) => Some(LValue::UpValue(slot)),
                ScopeVariable::Module => {
                    self.curr_bb().emit(Inst::PushModuleObject);
                    let const_id = self.constants.insert_string(name.text());
                    Some(LValue::Subscript(const_id))
                }
            },
            None => {
                self.emit_error(CompileError::new("unknown_ident", id.span()));
                None
            }
        }
    }

    fn compile_subscript_left_value(&mut self, v: SubscriptExpr) -> Option<LValue> {
        self.compile_expr(v.parent());
        self.compile_expr(v.subscript());
        Some(LValue::DynamicSubscript)
    }

    fn compile_dot_left_value(&mut self, v: DotExpr) -> Option<LValue> {
        self.compile_expr(v.parent());
        let subscript = v.subscript();
        let const_id = self.constants.insert_string(subscript.text());
        Some(LValue::Subscript(const_id))
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
        let expr = v.func();
        self.compile_expr(expr.clone());

        let is_method_call = matches!(expr, Expr::Subscript(_) | Expr::Dot(_));
        let param_cnt = v.params().count();
        for param in v.params() {
            self.compile_expr(param);
        }

        if is_method_call {
            self.curr_bb().emit_p(Inst::Call, param_cnt as u32);
        } else {
            self.curr_bb().emit_p(Inst::CallMethod, param_cnt as u32);
        }
    }

    /// Compile an identifier expression (RValue).
    fn compile_ident_expr(&mut self, id: IdentExpr) {
        let name = id.ident();
        match self.scope_map.get_local_or_add_upvalue(name.text()) {
            Some(ScopeVariable::Local(slot)) => {
                self.curr_bb().emit_p(Inst::LoadLocal, slot);
            }
            Some(ScopeVariable::Upvalue(slot)) => {
                // captured variable
                self.curr_bb().emit_p(Inst::LoadUpvalue, slot);
            }
            Some(ScopeVariable::Module) => {
                let c = self.constants.insert_string(name.text());
                self.curr_bb()
                    .emit(Inst::PushModuleObject)
                    .emit_p(Inst::LoadField, c);
            }
            None => {
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
    }

    fn compile_subscript_expr(&mut self, v: SubscriptExpr) {
        self.compile_expr(v.parent());
        self.compile_expr(v.subscript());
        self.curr_bb().emit(Inst::LoadFieldDyn);
    }

    fn compile_dot_expr(&mut self, v: DotExpr) {
        self.compile_expr(v.parent());
        let subscript = v.subscript();
        let const_id = self.constants.insert_string(subscript.text());
        self.curr_bb().emit_p(Inst::LoadField, const_id);
    }

    fn compile_if_expr(&mut self, v: IfExpr) {
        let mut if_bb;
        let mut else_bb = self.curr_bb;
        let next_bb = self.new_bb();

        for branch in v.branches() {
            // create a new block to hold branch body
            if_bb = self.new_bb();

            if let Some(cond) = branch.condition() {
                // compile the condition
                self.compile_expr(cond.expr());

                // create a new block to hold the else branch
                else_bb = self.new_bb();

                // create a conditional jump to both branches
                self.curr_bb().set_jmp(JumpInst::Conditional(
                    if_bb,
                    else_bb,
                    TopoSortAffinity::TrueBranch,
                ))
            } else {
                // create an unconditional jump since it's the only destination
                self.curr_bb().set_jmp(JumpInst::Unconditional(if_bb))
            }

            // compile branch body
            self.set_curr_bb(if_bb);
            self.compile_expr(branch.body());

            // jump to the end
            self.curr_bb().set_jmp(JumpInst::Unconditional(next_bb));

            // switch to else branch if needed
            if branch.condition().is_some() {
                self.set_curr_bb(else_bb);
            }
        }

        self.set_curr_bb(next_bb);
    }

    fn compile_while_loop_expr(&mut self, v: WhileLoopExpr) {
        let loop_header_bb = self.new_bb();
        let loop_body_bb = self.new_bb();
        let next_bb = self.new_bb();

        self.curr_bb()
            .set_jmp(JumpInst::Unconditional(loop_header_bb));
        self.set_curr_bb(loop_header_bb);

        self.compile_expr(v.condition().expr());
        self.curr_bb().set_jmp(JumpInst::Conditional(
            loop_body_bb,
            next_bb,
            TopoSortAffinity::TrueBranch,
        ));

        self.set_curr_bb(loop_body_bb);
        self.compile_block_scope(v.body());

        self.curr_bb()
            .set_jmp(JumpInst::Unconditional(loop_header_bb));

        self.set_curr_bb(next_bb);
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
            inkstone_syn::ast::LiteralKind::True => {
                self.curr_bb().emit(Inst::PushTrue);
            }
            inkstone_syn::ast::LiteralKind::False => {
                self.curr_bb().emit(Inst::PushFalse);
            }
            inkstone_syn::ast::LiteralKind::Nil => {
                self.curr_bb().emit(Inst::PushNil);
            }
            inkstone_syn::ast::LiteralKind::String => {
                let const_id = self.constants.insert_string(lit.token().text());
                self.curr_bb().emit_p(Inst::PushConst, const_id);
            }
            inkstone_syn::ast::LiteralKind::Symbol => todo!(),
        }
    }

    fn compile_lambda_expr(&mut self, v: LambdaExpr) {
        todo!()
    }
}

fn bb_scheduling(bbs: &[BasicBlock]) -> Vec<usize> {
    use itertools::Itertools;
    use petgraph::graphmap::*;

    let mut graph = DiGraphMap::new();
    for (idx, bb) in bbs.iter().enumerate() {
        match &bb.jmp {
            JumpInst::Unconditional(t) => {
                graph.add_edge(idx, *t, 1);
            }
            JumpInst::Conditional(t, f, order) => {
                let (t_weight, f_weight) = match order {
                    TopoSortAffinity::IDontCare => (1, 1),
                    TopoSortAffinity::TrueBranch => (1, 0),
                    TopoSortAffinity::FalseBranch => (0, 1),
                };
                graph.add_edge(idx, *t, t_weight);
                graph.add_edge(idx, *f, f_weight);
            }
            JumpInst::Unknown | JumpInst::Return | JumpInst::ReturnNil => {}
        }
    }

    struct EdgeWeighted<'a, X, Y, Z>(&'a GraphMap<X, Y, Z>);

    impl<'a, X, Y, Z> Clone for EdgeWeighted<'a, X, Y, Z> {
        fn clone(&self) -> Self {
            Self(self.0)
        }
    }

    impl<'a, X, Y, Z> Copy for EdgeWeighted<'a, X, Y, Z> {}

    impl<'a, X: NodeTrait, Y: Copy + Eq, Z: petgraph::EdgeType> petgraph::visit::GraphBase
        for EdgeWeighted<'a, X, Y, Z>
    {
        type EdgeId = Y;
        type NodeId = X;
    }

    impl<'a, X: NodeTrait, Y: Copy + Eq, Z: petgraph::EdgeType> petgraph::visit::GraphRef
        for EdgeWeighted<'a, X, Y, Z>
    {
    }

    impl<'a, X: NodeTrait, Y: Copy + Eq, Z: petgraph::EdgeType> Deref for EdgeWeighted<'a, X, Y, Z> {
        type Target = GraphMap<X, Y, Z>;

        fn deref(&self) -> &Self::Target {
            self.0
        }
    }

    impl<'a, X: NodeTrait, Y: Copy + Eq + Ord, Z: petgraph::EdgeType> petgraph::visit::IntoNeighbors
        for EdgeWeighted<'a, X, Y, Z>
    {
        type Neighbors = std::vec::IntoIter<X>;

        fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
            self.0.neighbors(a).sorted_by(|x1, x2| {
                self.0
                    .edge_weight(a, *x1)
                    .unwrap()
                    .cmp(self.0.edge_weight(a, *x2).unwrap())
            })
        }
    }

    impl<'a, X: NodeTrait, Y: Copy + Eq + Ord, Z: petgraph::EdgeType> petgraph::visit::Visitable
        for EdgeWeighted<'a, X, Y, Z>
    {
        type Map = FnvHashSet<X>;

        fn visit_map(&self) -> Self::Map {
            FnvHashSet::default()
        }

        fn reset_map(&self, map: &mut Self::Map) {
            map.clear();
        }
    }

    let graph_ref = EdgeWeighted(&graph);

    let mut visitor = petgraph::visit::DfsPostOrder::new(graph_ref, 0);
    let mut result = vec![];
    while let Some(n) = visitor.next(graph_ref) {
        result.push(n);
    }

    result.reverse();
    result
}

fn condense_basic_blocks(bbs: Vec<BasicBlock>) -> Vec<u8> {
    let bb_seq = bb_scheduling(&bbs);
    let bb_lengths = bbs.iter().map(|bb| bb.inst.len()).collect::<Vec<_>>();

    // Fusion between basic blocks
    let fused_bb = bb_seq
        .iter()
        .copied()
        .tuple_windows()
        .map(|(f, s)| match bbs[f].jmp {
            JumpInst::Unconditional(t) => {
                if t == s {
                    Some(true)
                } else {
                    None
                }
            }
            JumpInst::Conditional(t, f, _) => {
                if t == s {
                    Some(true)
                } else if f == s {
                    Some(false)
                } else {
                    None
                }
            }
            JumpInst::Unknown | JumpInst::Return | JumpInst::ReturnNil => None,
        })
        .collect_vec();

    // We simply assume all jump instructions take their maximum size, i.e. 6 bytes.

    todo!()
}

impl FunctionCompileCtx<'_> {
    pub fn finalize(self) -> Function {
        todo!()
    }
}
