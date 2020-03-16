use std::collections::{HashMap, HashSet};
use syntax_ast::ast::*;
use types::Type;

#[derive(Debug)]
struct SymbolTable
{
    symbol: HashMap<String, Symbol>,
    imports: HashSet<String>,
}

#[derive(Debug)]
struct Symbol
{
    name: String,
    types: Option<Type>,
    is_global: bool,
}

pub fn analysis(ast: &mut ASTree<'_>) -> Result<(), ()>
{
    let mut table = SymbolTable {
        symbol: HashMap::with_capacity(255),
        imports: HashSet::with_capacity(255),
    };

    resolve_directive(ast)?;
    resolve_root_symbols(&mut table, ast)?;
    resolve_function_symbols(&mut table, ast)?;
    Ok(())
}

fn resolve_directive(ast: &mut ASTree<'_>) -> Result<(), ()>
{
    Ok(())
}

fn resolve_function_symbols(table: &mut SymbolTable, ast: &mut ASTree<'_>) -> Result<(), ()>
{
    Ok(())
}

fn maybe_parse_annotated_type(ptype: &ParsedType) -> Result<Option<Type>, (&str, &str)>
{
    use ParsedType::*;
    match ptype
    {
        pInt => Ok(Some(Type::int())),
        pStr => Ok(Some(Type::string())),
        pFloat => Ok(Some(Type::float())),
        pBoolean => Ok(Some(Type::boolean())),
        pArray(of, size) =>
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
        pPointer(of) =>
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
        pOptional(of) =>
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
        pStruct(defs) => Err(("internal", "structは未実装です。")),
        pUserdef(defs) => Err(("internal", "userdefは未実装です。")),
        pUnknown => Err((
            "internal",
            "アノテーションがあるべき関数内で推論を必要とする定義に接触しました。",
        )),
    }
}

fn resolve_root_symbols(table: &mut SymbolTable, ast: &mut ASTree<'_>) -> Result<(), ()>
{
    for stmt in ast.root.iter()
    {
        let mut root = ast.stmt.get_mut(stmt.0 as usize).unwrap();

        match root.data
        {
            Stmt::Declaration(ref decl) =>
            {
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
                    types: maybe_type,
                };

                table.symbol.insert(decl.name.clone(), symbol);
            }
            _ => (),
        }
    }
    Ok(())
}

/*
 *
 *
 * // TODO - @Improvement: Currently, Literal folding is impossible to implement thanks to the
 * structure of Literal data - It carries Token around instead of an actual value.
 * It should be switched into " enum Literal { Int(i64), Str(String) .. } " if possible.
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
                let lhs_type = format!("{:?}", lhs.end_type);
                let rhs_type = format!("{:?}", rhs.end_type);
                expr.report(
                    "Type Mismatch",
                    &format!(
                        "{} != {}: 右辺値と左辺値で型が一致しませんでした。",
                        lhs_type, rhs_type
                    ),
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
                let lhs_type = format!("{:?}", lhs.end_type);
                let rhs_type = format!("{:?}", rhs.end_type);
                expr.report(
                    "Type Mismatch",
                    &format!(
                        "{} != {}: 右辺値と左辺値で型が一致しませんでした。",
                        lhs_type, rhs_type
                    ),
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
