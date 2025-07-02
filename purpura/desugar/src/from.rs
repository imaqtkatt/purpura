use location::{Location, Spanned};
use parser::expr::{self};

use crate::expr::{self as desugar};

impl From<expr::Fn> for desugar::FnClause {
    fn from(expr_fn: expr::Fn) -> Self {
        let body = expr_fn.body.into();

        let params = expr_fn
            .params
            .into_iter()
            .map(spanned_pattern_kind_to_desugar)
            .collect::<Vec<_>>();

        Self { params, body }
    }
}

impl From<expr::FnBody> for desugar::FnBody {
    fn from(fn_body: expr::FnBody) -> Self {
        desugar::FnBody(Box::new(desugar::Expr {
            value: fn_body.value.into(),
            location: fn_body.location,
        }))
    }
}

impl From<expr::PatternKind> for desugar::PatternKind {
    fn from(pattern_kind: parser::expr::PatternKind) -> Self {
        use expr::PatternKind;

        match pattern_kind {
            PatternKind::Wildcard => desugar::PatternKind::Wildcard,
            PatternKind::Identifier(id) => desugar::PatternKind::Identifier(id),
            PatternKind::Number(n) => desugar::PatternKind::Number(n),
            PatternKind::String(s) => desugar::PatternKind::String(s),
            PatternKind::Application(name, args) => {
                let name = name;

                let args = args
                    .into_iter()
                    .map(spanned_pattern_kind_to_desugar)
                    .collect::<Vec<_>>();

                desugar::PatternKind::Application(name, args)
            }
        }
    }
}

impl From<expr::StatementKind> for desugar::StatementKind {
    fn from(stmt_kind: expr::StatementKind) -> Self {
        match stmt_kind {
            expr::StatementKind::Expr(e) => e.value.into(),
            expr::StatementKind::Let(bind, e) => {
                desugar::StatementKind::Let(bind, spanned_expr_kind_to_desugar(e))
            }
        }
    }
}

impl From<expr::ExprKind> for desugar::ExprKind {
    fn from(expr_kind: expr::ExprKind) -> Self {
        use expr::ExprKind;

        match expr_kind {
            ExprKind::Number(n) => desugar::ExprKind::Number(n),
            ExprKind::String(s) => desugar::ExprKind::String(s),
            ExprKind::Identifier(id) => desugar::ExprKind::Identifier(id),
            ExprKind::Binary(op, e1, e2) => {
                let spanned_op = Box::new(Spanned {
                    value: op.into(),
                    location: Location::ghost(),
                });

                let e1 = Spanned {
                    value: e1.value.into(),
                    location: e1.location,
                };
                let e2 = Spanned {
                    value: e2.value.into(),
                    location: e2.location,
                };

                desugar::ExprKind::Application(spanned_op, vec![e1, e2])
            }
            ExprKind::Lambda(x, e) => {
                let expr = box_spanned_expr_to_desugar(*e);
                desugar::ExprKind::Lambda(x, expr)
            }
            ExprKind::Application(e, args) => {
                let expr = box_spanned_expr_to_desugar(*e);

                let args = args
                    .into_iter()
                    .map(spanned_expr_kind_to_desugar)
                    .collect::<Vec<_>>();

                desugar::ExprKind::Application(expr, args)
            }
            ExprKind::Match(e, arms) => {
                let expr = box_spanned_expr_to_desugar(*e);

                let arms = arms.into_iter().map(Into::into).collect::<Vec<_>>();

                desugar::ExprKind::Match(expr, arms)
            }
            ExprKind::Block(stmts) => {
                let stmts = stmts
                    .into_iter()
                    .map(spanned_stmt_kind_to_desugar)
                    .collect::<Vec<_>>();

                desugar::ExprKind::Block(stmts)
            }
        }
    }
}

impl From<expr::Arm> for desugar::Arm {
    fn from(arm: expr::Arm) -> Self {
        let left = arm.left.value.into();
        let left_location = arm.left.location;

        let right = arm.right.value.into();
        let right_location = arm.right.location;

        desugar::Arm {
            pattern: Box::new(Spanned {
                value: left,
                location: left_location,
            }),
            body: Box::new(Spanned {
                value: right,
                location: right_location,
            }),
        }
    }
}

fn spanned_expr_kind_to_desugar(e: Spanned<expr::ExprKind>) -> Spanned<desugar::ExprKind> {
    Spanned {
        value: e.value.into(),
        location: e.location,
    }
}

fn box_spanned_expr_to_desugar(e: Spanned<expr::ExprKind>) -> Box<Spanned<desugar::ExprKind>> {
    Box::new(Spanned {
        value: e.value.into(),
        location: e.location,
    })
}

impl From<expr::Operator> for desugar::ExprKind {
    fn from(op: expr::Operator) -> Self {
        use desugar::ExprKind::*;

        match op {
            expr::Operator::Mul => Identifier("_mul".into()),
            expr::Operator::Div => Identifier("_div".into()),
            expr::Operator::Sum => Identifier("_add".into()),
            expr::Operator::Min => Identifier("_sub".into()),
            expr::Operator::Greater => Identifier("_greater".into()),
            expr::Operator::Lesser => Identifier("_lesser".into()),
            expr::Operator::GreaterEqual => Identifier("_ge".into()),
            expr::Operator::LessEqual => Identifier("_le".into()),
            expr::Operator::And => Identifier("_and".into()),
            expr::Operator::Or => Identifier("_or".into()),
        }
    }
}

impl From<expr::ExprKind> for desugar::StatementKind {
    fn from(e: expr::ExprKind) -> Self {
        use desugar::StatementKind::Expr;

        let expr = Box::new(Spanned {
            location: Location::ghost(),
            value: e.into(),
        });
        Expr(expr)
    }
}

impl From<expr::Signature> for desugar::Signature {
    fn from(sig: expr::Signature) -> Self {
        let name = sig.name.clone();
        let return_type = spanned_type_kind_to_desugar(sig.return_type);

        let params = sig
            .params
            .into_iter()
            .map(spanned_type_kind_to_desugar)
            .collect::<Vec<_>>();

        Self {
            name,
            params,
            return_type,
        }
    }
}

impl From<expr::TypeKind> for desugar::TypeKind {
    fn from(type_kind: expr::TypeKind) -> Self {
        match type_kind {
            expr::TypeKind::TypeVariable(name) => desugar::TypeKind::TypeVariable(name),
            expr::TypeKind::Arrow(t1, t2) => {
                let desugar_t1 = box_spanned_type_kind_to_desugar(*t1);

                let desugar_t2 = box_spanned_type_kind_to_desugar(*t2);

                desugar::TypeKind::Arrow(desugar_t1, desugar_t2)
            }
            expr::TypeKind::Generic(name, type_kinds) => {
                let tks = type_kinds
                    .into_iter()
                    .map(spanned_type_kind_to_desugar)
                    .collect::<Vec<_>>();

                desugar::TypeKind::Generic(name, tks)
            }
        }
    }
}

impl From<expr::Data> for desugar::Data {
    fn from(data: expr::Data) -> Self {
        let expr::Data {
            name,
            ctors,
            params,
        } = data;

        let ctors = ctors
            .into_iter()
            .map(|ctor| desugar::Constructor {
                name: ctor.name,
                types: ctor
                    .types
                    .into_iter()
                    .map(spanned_type_kind_to_desugar)
                    .collect(),
            })
            .collect::<Vec<_>>();

        desugar::Data {
            name,
            params,
            ctors,
        }
    }
}

fn box_spanned_type_kind_to_desugar(
    tk: Spanned<expr::TypeKind>,
) -> Box<Spanned<desugar::TypeKind>> {
    Box::new(Spanned {
        value: tk.value.into(),
        location: tk.location,
    })
}

fn spanned_type_kind_to_desugar(tk: Spanned<expr::TypeKind>) -> Spanned<desugar::TypeKind> {
    Spanned {
        value: tk.value.into(),
        location: tk.location,
    }
}

fn spanned_pattern_kind_to_desugar(
    pk: Spanned<expr::PatternKind>,
) -> Spanned<desugar::PatternKind> {
    desugar::Pattern {
        value: pk.value.into(),
        location: pk.location,
    }
}

fn spanned_stmt_kind_to_desugar(
    sk: Spanned<expr::StatementKind>,
) -> Spanned<desugar::StatementKind> {
    Spanned {
        value: sk.value.into(),
        location: sk.location,
    }
}
