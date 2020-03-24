use std::collections::{HashMap, HashSet};
use super::{
    ast::*,
    types::Type,
    trace::prelude::*,
};

#[derive(Debug)]
struct SymbolTable
{
    symbol: HashMap<String, Symbol>,
    locals: Vec<Symbol>,
    last_local_scope: usize,
    // pub scopes: HashMap<StmtId, Scopes>,
}

#[derive(Debug)]
struct Symbol
{
    name: String,
    types: Option<Type>,
    span: CodeSpan,
    is_global: bool,
}

pub fn analysis(ast: &mut ASTree<'_>) -> Result<(), ()>
{
    let mut table = SymbolTable {
        symbol: HashMap::with_capacity(255),
        locals: Vec::with_capacity(64),
        last_local_scope: 0,
    };

    // resolve_directive(ast)?;
    resolve_toplevel(&mut table, ast)?;
    println!("{:#?}", &table);

    Ok(())
}

fn resolve_toplevel(table: &mut SymbolTable, ast: &mut ASTree<'_>) -> Result<(), ()>
{
    for stmt in ast.root.iter()
    {
        let root = ast.stmt.get(stmt.0 as usize).unwrap();

        match root.data
        {
            Stmt::Declaration(ref decl) =>
            {
                // Check Global declaration, and spit an error if it exists.
                if table.symbol.contains_key(&decl.name)
                {
                    let message = format!("変数 {} が複数回定義されています。", &decl.name);
                    root.report("Duplicated Declaration", &message);
                    return Err(());
                }

                // Infer or Check the type.
                // TODO - @Imcomplete: It can be separated into different code.
                let maybe_type = if decl.is_annotated()
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
                    if let Some(expr_id) = decl.expr
                    {
                        if let Some(ref mut expr) = ast.expr.get_mut(expr_id.0 as usize)
                        {
                            // try_folding_literal(expr, 0)?;
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
                        root.report(
                            "Empty Annotation/Initialization",
                            "変数の初期化も型指定もされていないため、型の推論が出来ません。",
                        );
                        return Err(());
                    }
                };

                let symbol = Symbol {
                    name: decl.name.clone(),
                    is_global: true,
                    span: root.span,
                    types: maybe_type,
                };

                table.symbol.insert(decl.name.clone(), symbol);
            }

            Stmt::Function(targ) => 
            {
                let func = ast.functions.get_mut(targ).unwrap();

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

                let symbol = Symbol {
                    name: func.it.name.clone(),
                    types: Some(final_type),
                    span: root.span,
                    is_global: true,
                };
                table.symbol.insert(func.it.name.clone(), symbol);
            }
            _ => (),
        }
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
                let pointer_of = Box::new(type_of);
                Ok(Some(Type::ptr(pointer_of)))
            }
            else
            {
                todo!("Error Logging");
            }
        }
        Struct(defs) => Err(("internal", "structは未実装です。")),
        Userdef(defs) => Err(("internal", "userdefは未実装です。")),
        Unknown => Err((
            "internal",
            "アノテーションがあるべき関数内で推論を必要とする定義に接触しました。",
        )),
    }
}


/*


// TODO - @Improvement: Currently, Literal folding is impossible to implement thanks to the
structure of Literal data - It carries Token around instead of an actual value.
It should be switched into " enum Literal { Int(i64), Str(String) .. } " if possible.

Binary,
Logical,
FunctionCall,
Assign,

Literal,
Grouping,
Unary,
Variable,
Empty,

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

fn type_match(lhs: &Expression<'_>, rhs: &Expression<'_>) -> bool
{
    lhs.end_type == rhs.end_type
}

fn solve_type(table: &mut SymbolTable, expr: &mut Expression<'_>) -> Result<(), ()>
{
    use ExprKind::*;
    match expr.kind
    {
        Literal => Ok(()),
        Assign => 
        {
            // TODO: Actually emit something
            Err(())
        }        
        Binary =>
        {
            let mut lhs = expr.lhs.as_mut().unwrap();
            let mut rhs = expr.rhs.as_mut().unwrap();

            if lhs.end_type.is_none() { solve_type(table, lhs.as_mut())?; }
            if rhs.end_type.is_none() { solve_type(table, rhs.as_mut())?; }

            if type_match(lhs.as_ref(), rhs.as_ref())
            {
                expr.end_type = lhs.end_type.clone();
                Ok(())
            }
            else
            {
                let message = format!(
                        "右辺値({})と左辺値({})で型が一致しませんでした。",
                        lhs.end_type.as_ref().unwrap(), rhs.end_type.as_ref().unwrap()
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
            let mut lhs = expr.lhs.as_mut().unwrap();
            let mut rhs = expr.rhs.as_mut().unwrap();

            if lhs.end_type.is_none()
            {
                solve_type(table, lhs.as_mut())?;
            }

            if rhs.end_type.is_none()
            {
                solve_type(table, rhs.as_mut())?;
            }

            if type_match(lhs.as_ref(), rhs.as_ref())
            {
                expr.end_type = Some(Type::boolean());
                Ok(())
            }
            else
            {
                let message = format!(
                        "{} != {}: 右辺値と左辺値で型が一致しませんでした。",
                        lhs.end_type.as_ref().unwrap(), rhs.end_type.as_ref().unwrap()
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
            let mut var = expr.lhs.as_mut().unwrap();
            if var.end_type.is_none()
            {
                solve_type(table, var.as_mut())?;
            }
            var.report("Unimplemented", "Unaryはまだ実装できていません。");
            Err(())
        }
        FunctionCall =>
        {
            let mut lhs = expr.lhs.as_mut().unwrap();
            if lhs.kind != Variable
            {
                lhs.report("Uncallable", "呼び出そうとした対象は関数ではありません。");
                return Err(());
            }

            expr.end_type = lhs.end_type.clone();
            Err(())
        }
        Grouping =>
        {
            let mut lhs = expr.lhs.as_mut().unwrap();
            solve_type(table, lhs.as_mut())?;
            expr.end_type = lhs.end_type.clone();
            Ok(())
        }
        Variable =>
        {
            expr.report("Unimplemented", "変数は実装されていません。");
            Err(())
        }
        Empty =>
        {
            expr.report("internal", "コンパイラーが式の形式を認識できませんでした。");
            Err(())
        }
    }
}

