use desugar::expr::ExprKind;
use report::ToDiagnostic;

use crate::{
    elaborated,
    infer::{typ, Infer},
    types::MonoType,
};

use super::Check;

struct CheckError {
    inferred: crate::types::Type,
    expected: crate::types::Type,
}

impl Check for ExprKind {
    type Out = elaborated::ExprKind;

    fn check(self, env: crate::env::Env, t: crate::types::Type) -> Self::Out {
        match (self, &*t) {
            (ExprKind::Number(n), MonoType::Ctor(ref name, _)) if name == "Number" => {
                elaborated::ExprKind::Number(n)
            }
            (ExprKind::Number(_), _) => {
                env.reporter.report(CheckError {
                    inferred: typ::type_number(),
                    expected: t,
                });
                elaborated::ExprKind::Error
            }

            (ExprKind::String(_), MonoType::Ctor(ref name, _)) if name == "String" => todo!(),
            (ExprKind::String(_), _) => {
                env.reporter.report(CheckError {
                    inferred: typ::type_string(),
                    expected: t,
                });
                elaborated::ExprKind::Error
            }

            (lambda @ ExprKind::Lambda(_, _), MonoType::Arrow(..)) => {
                let (lambda, lambda_t) = lambda.infer(env.clone());

                if lambda_t != t {
                    env.reporter.report(CheckError {
                        inferred: lambda_t,
                        expected: t,
                    });
                    return elaborated::ExprKind::Error;
                }

                lambda
            }

            (expr, _) => {
                let (expr, expr_t) = expr.infer(env.clone());

                if expr_t != t {
                    env.reporter.report(CheckError {
                        inferred: expr_t,
                        expected: t,
                    });
                    return elaborated::ExprKind::Error;
                }

                expr
            }
        }
    }
}

impl ToDiagnostic for CheckError {
    fn message(&self) -> String {
        format!("type error between {} and {}", self.inferred, self.expected)
    }

    fn markers(&self) -> Vec<report::Marker> {
        vec![]
    }

    fn severity(&self) -> report::Severity {
        report::Severity::Error
    }

    fn location(&self) -> location::Location {
        location::Location::ghost()
    }
}
