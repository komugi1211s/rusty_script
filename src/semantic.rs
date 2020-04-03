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
    // pub scopes: HashMap<StmtId, Scopes>,
}

impl SymTable
{
    pub fn new() -> Self
    {
        SymTable {
            symbol: HashMap::with_capacity(255),
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

#[derive(Debug, Clone)]
struct Sema 
{
    current_block: u32,
    locals: Vec<Symbol>,

    last_return: Option<Type>,
}

impl Sema
{
    fn deepen(&mut self)
    {
        self.current_block += 1;
    }

    fn shallowen(&mut self)
    {
        self.current_block -= 1;
        
        let depth = self.current_block;
        self.locals.retain(|x| x.depth <= depth);
    }

    fn clear(&mut self)
    {
        self.current_block = 0;
        self.locals.clear();
    }

    fn search(&self, name: &str) -> Option<usize>
    {
        if self.locals.is_empty() { return None; }
        
        let last_idx = self.locals.len() - 1;
        for (idx, local) in self.locals.iter().rev().enumerate()
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
        let depth = self.current_block;

        for local in self.locals.iter().rev()
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
        sema: Vec::with_capacity(64),
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

    let mut locals = Sema {
        locals: Vec::with_capacity(64),
        current_block: 0,
        last_return: None,
    };

    resolve_fully(table, &mut locals, &root_stmt, ast)?;
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

fn resolve_fully(table: &mut SymTable, local: &mut Sema, root: &[StmtId], ast: &mut ASTree) -> Result<(), ()>
{
    for stmt_id in root
    {
        resolve_statement(table, local, ast, *stmt_id)?;
        local.clear();
    }
    Ok(())
}

fn resolve_statement(table: &mut SymTable, sema: &mut Sema, ast: &mut ASTree, stmt_id: StmtId) -> Result<(), ()>
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

            solve_type(table, Some(&sema), expr)?;
        }

        Stmt::Print(expr_id) =>
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(&sema), expr)?;
        }
        

        Stmt::Declaration(ref decl) if sema.current_block > 0 =>
        {
            if sema.is_declared_within(&decl.name)
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

            let local_idx = sema.locals.len();
            let local_depth = sema.current_block;
            let expr_type = if let Some(expr_id) = decl.expr
            {
                if let Some(ref mut expr) = ast.expr.get_mut(expr_id.0 as usize)
                {
                    // try_folding_literal(expr, 0)?;
                    solve_type(table, Some(&sema), expr)?;
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
                    sema.locals.push(symbol);
                }
                (None, Some(x)) =>
                {
                    symbol.types = Some(x);
                    sema.locals.push(symbol);
                }
                (Some(x), Some(y)) => 
                {
                    if type_match(&x, &y)
                    {
                        symbol.types = Some(x);
                        sema.locals.push(symbol);
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

            solve_type(table, Some(&sema), expr)?;

            resolve_statement(table, sema, ast, if_block_id)?;
            if let Some(else_block_id) = opt_else_block_id {
                resolve_statement(table, sema, ast, else_block_id)?;
            }
        }

        Stmt::While(expr_id, while_block_id) =>
        {
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(&sema), expr)?;
            resolve_statement(table, sema, ast, while_block_id)?;
        }

        Stmt::Function(func_id) => 
        {
            resolve_function_and_body(table, sema, ast, func_id)?;
        }

        Stmt::Block(ref block_data) =>
        {
            sema.deepen();
            let block_inner = block_data.statements.clone();
            for inner_stmt in block_inner.iter() 
            {
                resolve_statement(table, sema, ast, *inner_stmt)?;
            }
            sema.shallowen();
        }

        Stmt::Return(Some(expr_id)) => 
        {
            if let Some(parent) = stmt.parent 
            {
                if let Stmt::Function(idx) = ast.stmt.get(parent.0 as usize).unwrap().data
                {
                    ast.functions.get(idx).unwrap().implicit_return_required.set(false);
                }
            }
            let expr = expect_opt!(ast.expr.get_mut(expr_id.0 as usize),
                    "指定された式({:?})が見つかりませんでした。", expr_id);

            solve_type(table, Some(&sema), expr)?;
            match sema.last_return
            {
                None => sema.last_return = expr.end_type.clone(),
                Some(ref ret_type) =>
                {
                    let end_type = expr.end_type.as_ref().unwrap_or(&*NULL_TYPE);
                    if !type_match(ret_type, end_type)
                    {
                        let message = format!(
                            "関数の戻す型に一貫性がありません。\n\n最初: {} が返されました \n二度目: {}が返されました",
                            ret_type, end_type);

                        stmt.report("Return type inconsistent", &message);
                        return Err(());
                    }
                }
            }
        }
        
        Stmt::Return(None) => 
        {
            if let Some(parent) = stmt.parent 
            {
                if let Stmt::Function(idx) = ast.stmt.get(parent.0 as usize).unwrap().data
                {
                    ast.functions.get(idx).unwrap().implicit_return_required.set(false);
                }
            }
            match sema.last_return
            {
                None => sema.last_return = Some(Type::null()),
                Some(ref ret_type) =>
                {
                    if !type_match(ret_type, &*NULL_TYPE)
                    {
                        let message = format!(
                            "関数の戻す型に一貫性がありません。\n\n最初: {} が返されました \n二度目: {}が返されました",
                            ret_type, &*NULL_TYPE);

                        stmt.report("Return type inconsistent", &message);
                        return Err(());
                    }
                }
            }
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

// TODO - @Improvement: DeclarationData should get it's own CodeSpan really.
// this Span argument exists because I didn't give DeclarationData it's own codespan.
fn resolve_function_and_body(table: &mut SymTable, sema: &mut Sema, ast: &mut ASTree, func_id: usize) -> Result<(), ()>
{
    sema.deepen();
    let func_name: String;
    let func_stmt_id: StmtId;
    let func_body_vector: Vec<StmtId>;

    // Entry Locals.
    {
        let func = expect_opt!(ast.functions.get(func_id), "関数 {} が見つかりません。", func_id);
        func_name = func.it.name.clone();
        func_stmt_id = func.own_stmt;
        func_body_vector = func.body.clone();

        let declaration_span = ast.get_stmt(func_stmt_id).span;
        let table_entry = expect_opt!(table.symbol.get(&func.it.name), "関数 {} が未解決です。", func.it.name);
        {
            if let Some(Type { kind: TypeKind::Function, ref arg_type, .. }) = table_entry.types
            {
                assert!(func.args.len() == arg_type.len());
                let local_depth = sema.current_block;
                for (decl, dtype) in func.args.iter().zip(arg_type.iter())
                {
                    let local_idx = sema.locals.len();
                    sema.locals.push(Symbol {
                        idx: local_idx,
                        name: decl.name.clone(),
                        types: Some(dtype.clone()),
                        span: declaration_span,
                        depth: local_depth
                    });
                }
            }
            else
            {
                let message = format!("関数 {} は関数ではなく {} として解決されています。", 
                    &func.it.name, table_entry.types.as_ref().unwrap_or(&*NULL_TYPE));
                report_compiler_bug(&message, ::std::file!(), ::std::line!(), 
                    "Some(Type { kind: TypeKind::Function, arg_type, ... }) = table_entry.types");
            }
        }
    }

    sema.last_return = None;
    for inner_id in func_body_vector.iter()
    {
        resolve_statement(table, sema, ast, *inner_id)?;
    }

    let implicit_return_required = ast.functions.get(func_id).unwrap().implicit_return_required.get();


    // Determine final type. infer if needed.
    {
        if let Some(ref mut symbol) = table.symbol.get_mut(&func_name)
        {
            let mut block_return_type = sema.last_return.take();
            let declared_return_type = &mut symbol.types.as_mut().unwrap().return_type;

            if implicit_return_required && block_return_type.is_some()
            {
                block_return_type = block_return_type.map(Type::optional);
            }
            match (&declared_return_type, &block_return_type)
            {
                (Some(x), Some(y)) =>
                {
                    if !type_match(&x, &y)
                    {
                        let message = format!(
                            "関数 {} の戻り値が一致していません。\n\
                            (定義された型: {}, 型推論器の結論: {})",
                            &func_name, &x, &y
                        );

                        ast.get_stmt(func_stmt_id).report("Type mismatch", &message);
                        if implicit_return_required
                        {
                            info("関数のトップレベルブロック内にreturnが見つかりませんでした。\n\
                                 関数の最後にreturn文を付け忘れていませんか？");
                        }
                        return Err(());
                    }
                }

                (None, Some(_)) => *declared_return_type = block_return_type.map(Box::new),

                (Some(x), None) => 
                {
                    if !type_match(&x, &*NULL_TYPE)
                    {
                        let message = format!("関数 {} の戻り値が一致していません。\n(定義された型: {}, 型推論器の結論: null)",
                            &func_name, &x);

                        ast.get_stmt(func_stmt_id).report("Type mismatch", &message);
                        if implicit_return_required
                        {
                            info("関数のトップレベルブロック内にreturnが見つかりませんでした。\n\
                                 関数の最後にreturn文を付け忘れていませんか？");
                        }
                        return Err(());
                    }
                }
                (None, None) =>
                {
                    *declared_return_type = Some(Box::new(Type::null()));
                }
            }
        }
        else
        {
            unreachable!();
        }
    }
    sema.shallowen();
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
        Array(of, _) =>
        {
            if let Some(type_of) = maybe_parse_annotated_type(of)?
            {
                let type_of = Box::new(type_of);
                Ok(Some(Type::array(type_of)))
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
            let lhs_inner_type = expect_opt!(lhs.array_type.as_ref(), "Array型が正常に解決されませんでした。");
            let rhs_inner_type = expect_opt!(rhs.array_type.as_ref(), "Array型が正常に解決されませんでした。");
            type_match(&*lhs_inner_type, &*rhs_inner_type)
        }
        (x, y) => x == y
    }
}

fn solve_type(table: &mut SymTable, sema: Option<&Sema>, expr: &mut Expression<'_>) -> Result<(), ()>
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

            if lhs.end_type.is_none() { solve_type(table, sema, lhs.as_mut())?; }
            if rhs.end_type.is_none() { solve_type(table, sema, rhs.as_mut())?; }

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
                solve_type(table, sema, lhs.as_mut())?;
            }

            if rhs.end_type.is_none()
            {
                solve_type(table, sema, rhs.as_mut())?;
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
                solve_type(table, sema, var.as_mut())?;
            }
            expr.end_type = var.end_type.clone();
            Ok(())
        }

        ArrayRef =>
        {
            let variable = expect_opt!(expr.lhs.as_mut(), "ArrayRef演算に必要なデータが足りません。");
            let indexing = expect_opt!(expr.rhs.as_mut(), "ArrayRef演算に必要なデータが足りません。");

            if variable.end_type.is_none() { solve_type(table, sema, variable.as_mut())?; }
            if indexing.end_type.is_none() { solve_type(table, sema, indexing.as_mut())?; }

            let variable_type = expect_opt!(variable.end_type.as_mut(), "SolveTypeが正常に配列？の型を解決していません。");
            let indexing_type = expect_opt!(indexing.end_type.as_mut(), "SolveTypeが正常にアクセスインデックスの型を解決していません。");

            if variable_type.kind != TypeKind::Array
            {
                let message = format!("配列型を期待しましたが、配列以外の型({})にアクセスしようとしました。", variable_type);
                variable.report("Type Mismatch", &message);
                return Err(());
            }
            if indexing_type.kind != TypeKind::Int // TODO - This should not be hard coded.
            {
                let message = format!("整数型を期待しましたが、代わりに {} が支給されました。", indexing_type);
                indexing.report("Type Mismatch", &message);
                return Err(());
            }

            let return_type = expect_opt!(variable_type.array_type.clone(), "配列の戻り値が解決していません。");
            expr.end_type = Some(*return_type);
            Ok(())
        }

        ArrayInst =>
        {
            let array_length = expr.array_expr.len() as u32;
            if array_length == 0
            {
                expr.end_type = Some(Type::array(Box::new(Type::null())));
            }

            let mut consistent_type = None;
            for entry_expr in expr.array_expr.iter_mut()
            {
                if entry_expr.end_type.is_none() { solve_type(table, sema, entry_expr)?; }

                match consistent_type
                {
                    None => consistent_type = entry_expr.end_type.clone(),
                    Some(ref root_type) =>
                    {
                        let expr_type = entry_expr.end_type.as_ref().unwrap();
                        if !type_match(root_type, expr_type)
                        {
                            let message = format!(
                                "配列の要素に一貫性がありません。\n\n最初: {} が返されました \n二度目: {}が返されました",
                                root_type, expr_type
                            );
                            
                            entry_expr.report("Array Type Inconsistent", &message);
                            return Err(())
                        }
                    }
                }
            }

            expr.end_type = Some(Type::array(consistent_type.map(Box::new).unwrap()));
            Ok(())
        }
        FunctionCall =>
        {
            let callee = expect_opt!(expr.lhs.as_mut(), "関数呼び出しに必要なデータが足りません。");
            if callee.end_type.is_none() { solve_type(table, sema, callee.as_mut())?; }

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
                if arg_expr.end_type.is_none() { solve_type(table, sema, arg_expr)?; }
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
            solve_type(table, sema, lhs.as_mut())?;
            expr.end_type = lhs.end_type.clone();
            Ok(())
        }
        Variable =>
        {
            let var_name = expect_opt!(expr.variable_name.as_ref(), "式データは変数として登録されていますが、変数名が存在しません。");
            if let Some(sema) = sema
            {
                if let Some(symbol_idx) = sema.search(var_name)
                {
                    {
                        let symbol = sema.locals.get(symbol_idx).unwrap(); // locals.search provides index that exists, Safe.
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
                    return Ok(());
                }
                else
                {
                    let message = format!("変数 {} の型の推論に失敗しました。 ", var_name);
                    expr.report("Type Inference Failed", &message);
                    return Err(());
                }
            }

            let message = format!("変数 {} は定義されていません。", var_name);
            expr.report("Undefined Variable", &message);
            Err(())
        }
        Empty =>
        {
            expr.report("internal", "コンパイラーが式の形式を認識できませんでした。");
            Err(())
        }
    }
}

