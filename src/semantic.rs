use std::sync::{ Arc, Mutex };
use std::collections::{ HashMap };
use super::{
    ast::*,
    types::{ Type, TypeKind, NULL_TYPE },
    trace::prelude::*,
};

#[allow(dead_code)]
pub type SymbolTable = Arc<Mutex<SymTable>>;

#[derive(Debug, Clone)]
pub struct SymTable
{
    pub symbol: HashMap<String, Symbol>,
    pub locals: Vec<Symbol>,
    // pub scopes: HashMap<StmtId, Scopes>,
}

impl SymTable
{
    pub fn new() -> Self
    {
        SymTable {
            symbol: HashMap::with_capacity(255),
            locals: Vec::with_capacity(64),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol
{
    pub idx: usize,
    pub name: String,
    pub types: Option<Type>,
    pub span: CodeSpan,
    pub depth: u32,
}

use std::cell::Cell;
use std::cell::RefCell;
#[derive(Debug, Clone)]
struct Locals 
{
    current_block: Cell<u32>,
    stack: RefCell<Vec<Symbol>>,
}

impl Locals
{
    fn deepen(&self)
    {
        self.current_block.set(self.current_block.get() + 1);
    }

    fn shallowen(&self)
    {
        let new_depth = self.current_block.get() - 1;
        self.current_block.set(new_depth);
        self.stack.borrow_mut().retain(|x| x.depth <= new_depth);
    }

    fn clear(&self)
    {
        self.current_block.set(0);
        self.stack.borrow_mut().clear();
    }

    fn search(&self, name: &str) -> Option<usize>
    {
        let locals = self.stack.borrow();
        if locals.is_empty() { return None; }
        
        let last_idx = locals.len() - 1;
        for (idx, local) in locals.iter().rev().enumerate()
        {
            let real_idx = last_idx - idx;
            if local.name == name
            {
                return Some(real_idx);
            }
        }
        None
    }
    fn is_declared_within(&self, name: &str) -> bool
    {
        let depth = self.current_block.get();
        let locals = self.stack.borrow();

        for local in locals.iter().rev()
        {
            if local.depth < depth { return false; }
            if local.depth == depth && local.name == name {
                return true;
            }
        }
        return false;
    }
}

/*
pub fn new_symboltable() -> SymbolTable
{
    let table = SymTable {
        symbol: HashMap::with_capacity(255),
        locals: Vec::with_capacity(64),
        global_idx: 0,
    };

    return Arc::new(Mutex::new(table));
}
*/

pub fn analysis(table: &mut SymTable, ast: &mut ASTree<'_>) -> Result<(), ()>
{
    // resolve_directive(ast)?;
    let root_stmt = ast.root.clone();

    resolve_toplevel(table, &root_stmt, ast)?;
    resolve_function_and_body(table, ast)?;
    
    let locals = Locals {
        stack: RefCell::new(Vec::with_capacity(64)),
        current_block: Cell::new(0),
    };

    resolve_fully(table, &locals, &root_stmt, ast)?;
    Ok(())
}

fn resolve_toplevel(table: &mut SymTable, root: &[StmtId], ast: &mut ASTree<'_>) -> Result<(), ()>
{
    let mut global_idx: usize = 0;

    for stmt in root
    {
        let root = expect_opt!(ast.stmt.get(stmt.0 as usize), "ルートに指定された文が見つかりませんでした。");
        match root.data
        {
            Stmt::Declaration(ref decl) =>
            {
                // Check Global declaration, and spit an error if it exists.
                if table.symbol.contains_key(&decl.name)
                {
                    let message = format!("変数 `{}` が複数回定義されています。", &decl.name);
                    root.report("Duplicated Declaration", &message);
                    return Err(());
                }

                // Infer or Check the type.
                // TODO - @Imcomplete: It can be separated into different code.
                let declared_type = if decl.is_annotated()
                {
                    match maybe_parse_annotated_type(&decl.dectype)
                    {
                        Ok(x) => x,
                        Err((title, message)) =>
                        {
                            root.report(title, message);
                            return Err(());
                        }
                    }
                }
                else
                {
                    None
                };

                let expr_type = if let Some(expr_id) = decl.expr
                {
                    if let Some(ref mut expr) = ast.expr.get_mut(expr_id.0 as usize)
                    {
                        // try_folding_literal(expr, 0)?;
                        solve_type(table, None, expr)?;
                        expr.end_type.clone()
                    }
                    else
                    {
                        // TODO: It can be okay if it's optional.
                        root.report(
                            "internal",
                            "初期化時に式を期待しましたが、取得できませんでした。",
                        );
                        panic!();
                    }
                }
                else
                {
                    Some(Type::null())
                };

                global_idx += 1;
                match (declared_type, expr_type)
                {
                    (None, None) => 
                    {
                        root.report(
                            "Empty Annotation/Initialization",
                            "変数の初期化も型指定もされていないため、型の推論が出来ません。",
                        );
                        return Err(());
                    }

                    (Some(x), None) =>
                    {
                        let symbol = Symbol {
                            idx: global_idx,
                            name: decl.name.clone(),
                            span: root.span,
                            types: Some(x),
                            depth: 0,
                        };
                        table.symbol.insert(decl.name.clone(), symbol);
                    }
                    (None, Some(x)) =>
                    {
                        let symbol = Symbol {
                            idx: global_idx,
                            name: decl.name.clone(),
                            span: root.span,
                            types: Some(x),
                            depth: 0,
                        };
                        table.symbol.insert(decl.name.clone(), symbol);
                    }
                    (Some(x), Some(y)) => 
                    {
                        if type_match(&x, &y)
                        {
                            let symbol = Symbol {
                                idx: global_idx,
                                name: decl.name.clone(),
                                span: root.span,
                                types: Some(x),
                                depth: 0,
                            };
                            table.symbol.insert(decl.name.clone(), symbol);
                        }
                        else
                        {
                            let message = &format!("変数の型 ({}) と式の結果型 ({}) が一致しませんでした。", x, y);
                            root.report("Type Mismatch", message);
                            return Err(());
                        }
                    }
                }
            }

            Stmt::Function(targ) => 
            {
                let func = expect_opt!(ast.functions.get(targ), "関数のインデックスが不正です。");

                // Check global declaration, and spit an error if it exists.
                // TODO - @Incomplete: Do something with _exists, because it has Span too,
                // maybe you can report in "info" style?
                if let Some(_exists) = table.symbol.get(&func.it.name)
                {
                    let message = format!("変数 {} が複数回定義されています。", &func.it.name);
                    root.report("Duplicated Declaration", &message);
                    return Err(());
                }

                // Check annotated return type, or just say None, which means that 
                // it'll get inferred by function's body.
                let mut annotated_return_type = None;
                if func.it.is_annotated() 
                {
                    annotated_return_type = match maybe_parse_annotated_type(&func.it.dectype) 
                    {
                        Ok(uncertain_type) => uncertain_type,
                        Err((title, message)) => {
                            root.report(title, message);
                            return Err(());
                        }
                    }
                }

                // Determine each argument's type.
                // TODO - @Incomplete: we forget the name of argument here: it has same problem
                // as the local variables. how should we handle this?
                // TODO - @Incomplete: Default argument support...?
                let arg_types: Vec<Type> = if func.args.is_empty() 
                {
                    vec![]
                }
                else
                {
                    let mut arg_type_holder = Vec::with_capacity(func.args.len());

                    for arg_decl in func.args.iter() 
                    {
                        match maybe_parse_annotated_type(&arg_decl.dectype) 
                        {
                             Ok(Some(trivial_type)) => arg_type_holder.push(trivial_type),
                             Ok(None) => {   
                                 let message = format!("関数の引数 {} に型指定がありません。\n
                                                       関数の引数には必ず型指定をして下さい。",
                                                       arg_decl.name);
                                 root.report("Untyped Argument Declaration", &message);
                                 return Err(());
                             }
                             Err((title, message)) => {
                                 root.report(title, message);
                                 return Err(());
                            }
                        }
                    }
                    arg_type_holder
                };

                let final_type = Type::function(annotated_return_type.map(Box::new),
                                                arg_types);

                global_idx += 1;
                let symbol = Symbol {
                    idx: global_idx,
                    name: func.it.name.clone(),
                    types: Some(final_type),
                    span: root.span,
                    depth: 0,
                };
                table.symbol.insert(func.it.name.clone(), symbol);
            }

            // Stmt::Expression(ref expr) => 
            // {
            //     let expr = expect_opt!(ast.expr.get_mut(expr.0 as usize), "式のインデックスが不正です。");
            //     solve_type(table, None, expr)?;
            // }

            // Stmt::Print(ref expr) =>
            // {
            //     let expr = expect_opt!(ast.expr.get_mut(expr.0 as usize), "式のインデックスが不正です。");
            //     solve_type(table, None, expr)?;
            // }

            // Stmt::If(expr, _, _) =>
            // {
            //     let expr = expect_opt!(ast.expr.get_mut(expr.0 as usize), "式のインデックスが不正です。");
            //     solve_type(table, None, expr)?;
            // }

            // Stmt::While(expr, _) =>
            // {
            //     let expr = expect_opt!(ast.expr.get_mut(expr.0 as usize), "式のインデックスが不正です。");
            //     solve_type(table, None, expr)?; 
            // }
            _ => (),
        }
    }
    Ok(())
}

fn resolve_fully(table: &mut SymTable, local: &Locals, root: &[StmtId], ast: &mut ASTree) -> Result<(), ()>
{
    for stmt_id in root
    {
        resolve_statement(table, local, ast, *stmt_id)?;
        local.clear();
    }
    Ok(())
}

fn resolve_statement(table: &mut SymTable, locals: &Locals, ast: &mut ASTree, stmt_id: StmtId) -> Result<(), ()>
{
    let stmt = expect_opt!(ast.stmt.get(stmt_id.0 as usize), 
        "指定された文({:?})が見つかりませんでした。", stmt_id);

    match stmt.data
    {
        // Expr Type:
        Stmt::Expression(expr_id) => 
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(locals), expr)?;
        }
        Stmt::Print(expr_id) =>
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(locals), expr)?;
        }
        Stmt::Return(Some(expr_id)) => 
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(locals), expr)?;
        }
        Stmt::Declaration(ref decl) if locals.current_block.get() > 0 =>
        {
            if locals.is_declared_within(&decl.name)
            {
                let message = format!("変数 `{}` が複数回定義されています。", &decl.name);
                stmt.report("Duplicated Declaration", &message);
                return Err(());
            }
            // Infer or Check the type.
            // TODO - @Imcomplete: It can be separated into different code.
            let declared_type = if decl.is_annotated()
            {
                match maybe_parse_annotated_type(&decl.dectype)
                {
                    Ok(x) => x,
                    Err((title, message)) =>
                    {
                        stmt.report(title, message);
                        return Err(());
                    }
                }
            }
            else
            {
                None
            };

            let local_idx = locals.stack.borrow().len();
            let local_depth = locals.current_block.get();
            let expr_type = if let Some(expr_id) = decl.expr
            {
                if let Some(ref mut expr) = ast.expr.get_mut(expr_id.0 as usize)
                {
                    // try_folding_literal(expr, 0)?;
                    solve_type(table, Some(locals), expr)?;
                    expr.local_idx = Some(local_idx as u32);
                    expr.end_type.clone()
                }
                else
                {
                    // TODO: It can be okay if it's optional.
                    stmt.report(
                        "internal",
                        "初期化時に式を期待しましたが、取得できませんでした。",
                    );
                    panic!();
                }
            }
            else
            {
                Some(Type::null())
            };

            let mut symbol = Symbol {
                idx: local_idx,
                name: decl.name.clone(),
                span: stmt.span,
                types: None,
                depth: local_depth,
            };

            match (declared_type, expr_type)
            {
                (None, None) => 
                {
                    stmt.report(
                        "Empty Annotation/Initialization",
                        "変数の初期化も型指定もされていないため、型の推論が出来ません。",
                    );
                    return Err(());
                }

                (Some(x), None) =>
                {
                    symbol.types = Some(x);
                    locals.stack.borrow_mut().push(symbol);
                }
                (None, Some(x)) =>
                {
                    symbol.types = Some(x);
                    locals.stack.borrow_mut().push(symbol);
                }
                (Some(x), Some(y)) => 
                {
                    if type_match(&x, &y)
                    {
                        symbol.types = Some(x);
                        locals.stack.borrow_mut().push(symbol);
                    }
                    else
                    {
                        let message = &format!("変数の型 ({}) と式の結果型 ({}) が一致しませんでした。", x, y);
                        stmt.report("Type Mismatch", message);
                        return Err(());
                    }
                }
            }

            return Ok(());
        }

        Stmt::If(expr_id, if_block_id, opt_else_block_id) =>
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(locals), expr)?;

            resolve_statement(table, locals, ast, if_block_id)?;
            if let Some(else_block_id) = opt_else_block_id {
                resolve_statement(table, locals, ast, else_block_id)?;
            }
        }

        Stmt::While(expr_id, while_block_id) =>
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(locals), expr)?;
            resolve_statement(table, locals, ast, while_block_id)?;
        }

        Stmt::Function(func_id) =>
        {
            let func_block_id = {
                let func_data = expect_opt!(ast.functions.get(func_id),
                "指定された関数({})が見つかりませんでした。", func_id);
                func_data.block_id
            };

            resolve_statement(table, locals, ast, func_block_id)?;
            // ensure_function_declaration_type(table, locals, ast, func_block_id)?;
        }

        Stmt::Block(ref block_data) =>
        {
            locals.deepen();
            let block_inner = block_data.statements.clone();
            for inner_stmt in block_inner.iter() 
            {
                resolve_statement(table, locals, ast, *inner_stmt)?;
            }
            locals.shallowen();
        }

        Stmt::Empty => 
        {
            stmt.report("Empty Statement", "空の文が支給されました。");
            return Err(());
        }

        _ => ()
    }

    Ok(())
}

fn maybe_parse_annotated_type(ptype: &ParsedType) -> Result<Option<Type>, (&str, &str)>
{
    use ParsedType::*;
    match ptype
    {
        Int => Ok(Some(Type::int())),
        Str => Ok(Some(Type::string())),
        Float => Ok(Some(Type::float())),
        Boolean => Ok(Some(Type::boolean())),
        Array(of, size) =>
        {
            if let Some(type_of) = maybe_parse_annotated_type(of)?
            {
                let type_of = Box::new(type_of);
                Ok(Some(Type::array(type_of, *size)))
            }
            else
            {
                todo!("Error Logging");
            }
        }
        Pointer(of) =>
        {
            if let Some(type_of) = maybe_parse_annotated_type(of)?
            {
                let pointer_of = Box::new(type_of);
                Ok(Some(Type::ptr(pointer_of)))
            }
            else
            {
                todo!("Error Logging");
            }
        }
        Optional(of) =>
        {
            if let Some(type_of) = maybe_parse_annotated_type(of)?
            {
                Ok(Some(Type::optional(type_of)))
            }
            else
            {
                todo!("Error Logging");
            }
        }
        Struct(_) => Err(("internal", "structは未実装です。")),
        Userdef(_) => Err(("internal", "userdefは未実装です。")),
        Unknown => Err((
            "internal",
            "アノテーションがあるべき関数内で推論を必要とする定義に接触しました。",
        )),
    }
}


/*
TODO - @Improvement: Currently, Literal folding is impossible due to the way Literal is
structured - It carries a Token around instead of an actual value.
It should be switched to "enum Literal { Int(i64), Str(String) .. }" if possible.

It's supposed to be used as a source for error reporting when we need to.
but since this data is always wrapped around Expression and never passed around as-is,
this design decision is completely redundant.

const MAX_FOLD_DEPTH: u32 = 3;
fn try_folding_literal(expr: &mut Expression<'_>, depth: u32) -> Result<bool, ()> {

    fn unexpected_path(path: &mut Expression<'_>, additional_info: &str) -> ! {
        path.report("internal", &format!("式の形式が理解できませんでした。追加要素: {}", additional_info));
        panic!()
    }

    if depth > MAX_FOLD_DEPTH {
        return Ok(false);
    }

    match expr.kind {
        ExprKind::Empty => unexpected_path(expr, "Empty Expr"),

        ExprKind::Variable
        | ExprKind::Assign
        | ExprKind::FunctionCall => Ok(false),

        ExprKind::Grouping => {
            if let Some(ref mut lhs) = expr.lhs {
                try_folding_literal(&mut *lhs, depth + 1)
            } else {
                unexpected_path(expr, "Empty Grouping")
            }
        }


        ExprKind::Binary => {
            match (expr.lhs, expr.rhs) {
                (Some(ref mut lhs), Some(ref mut rhs)) => {
                    let lhs_is_literal = try_folding_literal(&mut *lhs, depth + 1)?;
                    let rhs_is_literal = try_folding_literal(&mut *rhs, depth + 1)?;

                    if !(lhs_is_literal && rhs_is_literal) {
                        return Ok(false)
                    }

                    if !type_match(&*lhs, &*rhs) {
                        let lhs_type = format!("{:?}", lhs.end_type);
                        let rhs_type = format!("{:?}", rhs.end_type);
                        expr.report("Type Mismatch", &format!("{} != {}: 右辺値と左辺値で型が一致しませんでした。", lhs_type, rhs_type));
                        panic!();
                    }

                    match (lhs.literal, rhs.literal, expr.oper) {
                        (Some(ref mut x), Some(ref mut y), Some(oper)) => {
                            let new_literal = fold_literal_binop(x, y, oper);
                            expr.lhs = None;
                            expr.rhs = None;
                            expr.oper = None;
                        }

                        (_, _, None) => unexpected_path(expr, "Empty Operator in Binary Expr"),
                        _ => unexpected_path(expr, "One or More Literal are None in Binary Expr"),
                    }

                }
                _ => unexpected_path(expr),
            }
        }
        _ => unexpected_path(expr)
    }
}
fn fold_literal_binop<'b>(base: &'b Expression<'b>, lhs: &Literal<'_>, rhs: &Literal<'_>, oper: Operator) -> Literal<'b> {
}
*/

fn resolve_function_and_body(table: &mut SymTable, ast: &mut ASTree) -> Result<(), ()>
{
    for func in ast.functions.iter()
    {
        let statements = expect_opt!(ast.stmt.get(func.block_id.0 as usize), 
            "指定された文({:?})が見つかりませんでした。", func.block_id);

        let mut actually_returned = false;
        let mut func_return_type = None;

        if let Stmt::Block(ref block_data) = statements.data
        {
            let cloned = block_data.statements.clone();
            for inner_stmt_id in cloned.iter()
            {
                let inner_stmt = ast.stmt.get(inner_stmt_id.0 as usize).expect("Resolve Func stmt error");
                match inner_stmt.data
                {
                    Stmt::Return(Some(expr_id)) => 
                    {
                        let expr = ast.expr.get_mut(expr_id.0 as usize).expect("No Expr Error");
                        solve_type(table, None, expr)?;
                        if !actually_returned
                        {
                            func_return_type = expr.end_type.clone();
                            actually_returned = true;
                        }
                        else
                        {
                            if !type_match(func_return_type.as_ref().unwrap_or(&NULL_TYPE), expr.end_type.as_ref().unwrap_or(&NULL_TYPE))
                            {
                                let message = format!("関数の戻り値の型に一貫性がありません\n(複数の違う型: 最初に{}が、次に{}が返されました)",
                                    func_return_type.as_ref().unwrap_or(&NULL_TYPE), expr.end_type.as_ref().unwrap_or(&NULL_TYPE));
                                inner_stmt.report("Type Mismatch", &message);
                                return Err(());
                            }
                        }
                    }
                    Stmt::Return(None) =>
                    {
                        if !actually_returned { actually_returned = true; }
                        if func_return_type.is_some()
                        {
                            let message = format!("関数の戻り値の型に一貫性がありません\n(複数の違う型: 最初に{}が、次にnullが返されました)",
                                func_return_type.as_ref().unwrap());
                            inner_stmt.report("Type Mismatch", &message);
                            return Err(());
                        }
                    }
                    _ => (),
                }
            }
        }
        else
        {
            statements.report("internal", "関数の内部ブロックを期待しましたが、ブロック以外が見つかりました。");
            panic!()
        }


        if let Some(ref mut symbol) = table.symbol.get_mut(&func.it.name)
        {
            match symbol.types
            {
                Some(Type { kind: TypeKind::Function, return_type: Some(ref ret_type), .. }) =>
                {
                    if !type_match(&*ret_type, func_return_type.as_ref().unwrap_or(&NULL_TYPE))
                    {
                        let message = format!(
                            "関数 {} の戻り値 ({}) の定義と実際の戻り値 ({}) が一致していません。", 
                            &symbol.name, &*ret_type, func_return_type.as_ref().unwrap_or(&NULL_TYPE)
                        );
                        statements.report("Type Mismatch", &message);
                        return Err(());
                    }
                }
                Some(ref mut inner_type @ Type { kind: TypeKind::Function, return_type: None, .. }) =>
                {
                    inner_type.return_type = Some(Box::new(func_return_type.unwrap_or(Type::null())));
                }
                Some(_) | None => 
                {
                    unreachable!();
                }
            }
        }
        else
        {
            let message = format!(
                "セマンティクス解析中に関数 {} が見つかりませんでした。", 
                &func.it.name
            );
            report("internal", &message);
            return Err(());
        }
    }
    Ok(())
}

fn type_match(lhs: &Type, rhs: &Type) -> bool
{
    use TypeKind::*;
    match (lhs.kind, rhs.kind)
    {
        (Struct, Struct) | (Union, Union) =>
        {
            if lhs.struct_members.len() != rhs.struct_members.len() { return false; }

            lhs.struct_members.iter()
                              .zip(rhs.struct_members.iter())
                              .all(|(lht, rht)| type_match(lht, rht))

        }
        (Enum, Enum) =>
        {
            lhs.struct_name == rhs.struct_name
        }
        (Function, Function) =>
        {
            let lhs_ret = expect_opt!(lhs.return_type.as_ref(), "左辺値 関数 の戻り値が解決していません。");
            let rhs_ret = expect_opt!(rhs.return_type.as_ref(), "右辺値 関数 の戻り値が解決していません。");
            if !type_match(&*lhs_ret, &*rhs_ret) { return false; }
            if lhs.arg_type.len() != rhs.arg_type.len() { return false; }

            lhs.arg_type.iter().zip(rhs.arg_type.iter()).all(|(lht, rht)| type_match(lht, rht))
        }
        (Ptr, Ptr) =>
        {
            let lhs_ptr = expect_opt!(lhs.pointer_to.as_ref(), "LHS ポインタ型が正常に解決されませんでした。");
            let rhs_ptr = expect_opt!(rhs.pointer_to.as_ref(), "RHS ポインタ型が正常に解決されませんでした。");
            type_match(&*lhs_ptr, &*rhs_ptr)
        }
        (Union, _) => 
        {
            for lhs_type in lhs.struct_members.iter()
            {
                if type_match(lhs_type, rhs)
                {
                    return true;
                }
            }
            return false;
        }
        (Array, Array) => 
        {
            if lhs.is_array_dynamic != rhs.is_array_dynamic { return false; }
            if lhs.array_size != rhs.array_size { return false; }
            let lhs_inner_type = expect_opt!(lhs.array_type.as_ref(), "Array型が正常に解決されませんでした。");
            let rhs_inner_type = expect_opt!(rhs.array_type.as_ref(), "Array型が正常に解決されませんでした。");

            type_match(&*lhs_inner_type, &*rhs_inner_type)
        }
        (x, y) => x == y
    }
}

fn solve_type(table: &mut SymTable, locals: Option<&Locals>, expr: &mut Expression<'_>) -> Result<(), ()>
{
    if expr.end_type.is_some() { return Ok(()); }
    use ExprKind::*;

    match expr.kind
    {
        Literal =>  // if it hits here, it should be a problem on it's own
        {
            expr.report("internal", "リテラル値に型が設定されていません。パーサー上の実装に問題があります。");
            panic!()
        }
        Binary | Assign =>
        {
            let lhs = expect_opt!(expr.lhs.as_mut(), "バイナリ/アサイン演算時に必要なデータが足りません。");
            let rhs = expect_opt!(expr.rhs.as_mut(), "バイナリ/アサイン演算時に必要なデータが足りません。");

            if lhs.end_type.is_none() { solve_type(table, locals, lhs.as_mut())?; }
            if rhs.end_type.is_none() { solve_type(table, locals, rhs.as_mut())?; }

            let lhs_type = expect_opt!(lhs.end_type.as_ref(), "SolveTypeが正常にLHSの型を解決していません。");
            let rhs_type = expect_opt!(rhs.end_type.as_ref(), "SolveTypeが正常にRHSの型を解決していません。");
            if type_match(lhs_type, rhs_type)
            {
                expr.end_type = lhs.end_type.clone();
                Ok(())
            }
            else
            {
                let message = format!(
                        "左辺値の型 ({}) と右辺値の型 ({}) が一致しませんでした。",
                        lhs_type, rhs_type
                    );
                expr.report(
                    "Type Mismatch",
                    &message,
                );
                Err(())
            }
        }

        Logical =>
        {
            let lhs = expect_opt!(expr.lhs.as_mut(), "Logical演算に必要なデータが足りません。");
            let rhs = expect_opt!(expr.rhs.as_mut(), "Logical演算に必要なデータが足りません。");

            if lhs.end_type.is_none()
            {
                solve_type(table, locals, lhs.as_mut())?;
            }

            if rhs.end_type.is_none()
            {
                solve_type(table, locals, rhs.as_mut())?;
            }

            let lhs_type = expect_opt!(lhs.end_type.as_ref(), "SolveTypeが正常にLHSの型を解決していません。");
            let rhs_type = expect_opt!(rhs.end_type.as_ref(), "SolveTypeが正常にRHSの型を解決していません。");
            if type_match(lhs_type, rhs_type)
            {
                expr.end_type = Some(Type::boolean());
                Ok(())
            }
            else
            {
                let message = format!(
                        "左辺値の型 ({}) と右辺値の型 ({}) が一致しませんでした。",
                        lhs_type, rhs_type
                    );
                expr.report(
                    "Type Mismatch",
                    &message,
                );
                Err(())
            }
        }
        Unary =>
        {
            let var = expect_opt!(expr.rhs.as_mut(), "Unary演算に必要なデータが足りません。");
            if var.end_type.is_none()
            {
                solve_type(table, locals, var.as_mut())?;
            }
            expr.end_type = var.end_type.clone();
            Ok(())
        }
        FunctionCall =>
        {
            let callee = expect_opt!(expr.lhs.as_mut(), "関数呼び出しに必要なデータが足りません。");
            if callee.end_type.is_none() { solve_type(table, locals, callee.as_mut())?; }

            let callee_type = expect_opt!(callee.end_type.as_ref(), "呼び出される関数の型が解決していません。");
            if callee_type.kind != TypeKind::Function
            {
                let message = format!("呼び出そうとした対象 ({}) は関数ではありません。", callee_type);
                expr.report("Type Mismatch", &message);
                return Err(());
            }

            if expr.arg_expr.len() != callee_type.arg_type.len() 
            {
                expr.report("Insufficient Argument", "引数の数が一致していません。");
                return Err(());
            }

            for (ref mut arg_expr, ref intended_type) in expr.arg_expr.iter_mut().zip(callee_type.arg_type.iter())
            {
                if arg_expr.end_type.is_none() { solve_type(table, locals, arg_expr)?; }
                let arg_type = expect_opt!(arg_expr.end_type.as_ref(), "引数の型が解決できませんでした。");

                if !type_match(arg_type, intended_type)
                {
                    let message = format!("引数で定義された型 ({}) と実際の型 ({}) が一致していません。",
                        intended_type, arg_type);
                    arg_expr.report("Type Mismatch", &message);
                    return Err(());
                }
            }

            let return_type = expect_opt!(callee_type.return_type.clone(), "関数の戻り値が解決していません。");
            expr.end_type = Some(*return_type);
            Ok(())
        }
        Grouping =>
        {
            let lhs = expect_opt!(expr.lhs.as_mut(), "Grouping演算に必要なデータが足りません。");
            solve_type(table, locals, lhs.as_mut())?;
            expr.end_type = lhs.end_type.clone();
            Ok(())
        }
        Variable =>
        {
            let var_name = expect_opt!(expr.variable_name.as_ref(), "式データは変数として登録されていますが、変数名が存在しません。");
            if let Some(locals) = locals
            {
                if let Some(symbol_idx) = locals.search(var_name)
                {
                    {
                        let mut local_borrowed = locals.stack.borrow_mut();
                        let symbol = local_borrowed.get_mut(symbol_idx).unwrap(); // locals.search provides index that exists, Safe.
                        if symbol.types.is_some()
                        {
                            expr.end_type = symbol.types.clone();
                            expr.local_idx = Some(symbol_idx as u32);
                            return Ok(());
                        }
                        else
                        {
                            let message = format!("変数 {} の型の推論に失敗しました。 ", var_name);
                            expr.report("Type Inference Failed", &message);
                            return Err(());
                        }
                    }
                }
            }

            if let Some(symbol) = table.symbol.get(var_name)
            {
                if symbol.types.is_some()
                {
                    expr.end_type = symbol.types.clone();
                    Ok(())
                }
                else
                {
                    let message = format!("変数 {} の型の推論に失敗しました。 ", var_name);
                    expr.report("Type Inference Failed", &message);
                    Err(())
                }
            }
            else
            {
                let message = format!("変数 {} は定義されていません。", var_name);
                expr.report("Undefined Variable", &message);
                Err(())
            }
        }
        Empty =>
        {
            expr.report("internal", "コンパイラーが式の形式を認識できませんでした。");
            Err(())
        }
    }
}

