//! Desugars expressions to easier forms from [crate::tree::parse] to [crate::tree::desugared].

use crate::{
    report,
    tree::{self, desugared, parse},
};

#[derive(Debug)]
pub struct Context {
    datatypes: indexmap::IndexMap<String, desugared::Data>,
    definitions: indexmap::IndexMap<String, parse::Def>,
    signatures: indexmap::IndexMap<String, desugared::Signature>,

    reporter: report::Reporter,
}

impl Context {
    pub fn new(reporter: report::Reporter) -> Self {
        Self {
            datatypes: indexmap::IndexMap::new(),
            definitions: indexmap::IndexMap::new(),
            signatures: indexmap::IndexMap::new(),
            reporter,
        }
    }
}

enum DesugarError {
    RepeatedDatatype(crate::location::Location),
    RepeatedSignature(crate::location::Location),
    RepeatedDefinition(crate::location::Location),
    MissingDef(tree::Symbol, crate::location::Location),
    MissingSig(tree::Symbol, crate::location::Location),
}

impl report::Diag for DesugarError {
    fn severity(&self) -> report::Severity {
        match self {
            DesugarError::RepeatedDatatype(..) => report::Severity::Warning,
            DesugarError::RepeatedSignature(..) => report::Severity::Warning,
            DesugarError::RepeatedDefinition(..) => report::Severity::Warning,
            DesugarError::MissingDef(..) => report::Severity::Error,
            DesugarError::MissingSig(..) => report::Severity::Warning,
        }
    }

    fn message(&self) -> String {
        // change this message?
        "desugar error".to_string()
    }

    fn markers(&self) -> Vec<report::Marker> {
        match self {
            DesugarError::RepeatedDatatype(location) => {
                vec![report::Marker::new(
                    "repeated data type".to_string(),
                    *location,
                )]
            }
            DesugarError::RepeatedSignature(location) => vec![report::Marker::new(
                "repeated signature".to_string(),
                *location,
            )],
            DesugarError::RepeatedDefinition(location) => {
                vec![report::Marker::new(
                    "repeated definition".to_string(),
                    *location,
                )]
            }
            DesugarError::MissingDef(symbol, location) => vec![report::Marker::new(
                format!("missing definition for signature '{}'", symbol.as_str()),
                *location,
            )],
            DesugarError::MissingSig(symbol, location) => vec![report::Marker::new(
                format!("missing signature for definition '{}'", symbol.as_str()),
                *location,
            )],
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            DesugarError::RepeatedDatatype(..) => None,
            DesugarError::RepeatedSignature(..) => None,
            DesugarError::RepeatedDefinition(..) => None,
            DesugarError::MissingDef(..) => Some("create a placehold def".to_string()),
            DesugarError::MissingSig(..) => None,
        }
    }

    fn location(&self) -> crate::location::Location {
        match self {
            DesugarError::RepeatedDatatype(location) => *location,
            DesugarError::RepeatedSignature(location) => *location,
            DesugarError::RepeatedDefinition(location) => *location,
            DesugarError::MissingDef(_, location) => *location,
            DesugarError::MissingSig(_, location) => *location,
        }
    }
}

impl Context {
    pub fn desugar_program(mut self, program: parse::Program) -> desugared::Program {
        for top_level in program.definitions.into_iter() {
            match top_level {
                parse::TopLevel::Data(data) if self.datatypes.contains_key(data.name.as_str()) => {
                    self.reporter
                        .report(DesugarError::RepeatedDatatype(data.name.location))
                }
                parse::TopLevel::Data(data) => {
                    self.datatypes
                        .insert(data.name.name.clone(), desugar_data(data));
                }

                parse::TopLevel::Sig(sig) if self.datatypes.contains_key(sig.name.as_str()) => {
                    self.reporter
                        .report(DesugarError::RepeatedSignature(sig.name.location));
                }
                parse::TopLevel::Sig(sig) => {
                    self.signatures
                        .insert(sig.name.name.clone(), desugar_signature(sig));
                }

                parse::TopLevel::Def(def) if self.definitions.contains_key(def.name.as_str()) => {
                    self.reporter
                        .report(DesugarError::RepeatedDefinition(def.name.location));
                }
                parse::TopLevel::Def(def) => {
                    self.definitions.insert(def.name.name.clone(), def);
                }
            }
        }

        let mut program = desugared::Program {
            definitions: vec![],
        };

        for (_, data) in std::mem::take(&mut self.datatypes).into_iter() {
            program.definitions.push(desugared::TopLevel::Data(data));
        }

        for (name, sig) in std::mem::take(&mut self.signatures).into_iter() {
            if let Some(def) = self.definitions.swap_remove(&name) {
                program
                    .definitions
                    .push(desugared::TopLevel::Def(desugar_def(def, sig)));
            } else {
                self.reporter.report(DesugarError::MissingDef(
                    sig.name.clone(),
                    sig.name.location,
                ));
            }
        }

        for (_, program) in std::mem::take(&mut self.definitions).into_iter() {
            self.reporter.report(DesugarError::MissingSig(
                program.name.clone(),
                program.name.location,
            ));
        }

        program
    }
}

pub fn desugar_data(data: parse::Data) -> desugared::Data {
    desugared::Data {
        name: data.name,
        generics: data.generics,
        constructors: data
            .constructors
            .into_iter()
            .map(|c| desugared::Constructor {
                name: c.name,
                types: c.types.into_iter().map(desugar_type).collect(),
            })
            .collect(),
    }
}

pub fn desugar_signature(sig: parse::Sig) -> desugared::Signature {
    desugared::Signature {
        name: sig.name,
        r#type: desugar_type(sig.r#type),
    }
}

pub fn desugar_def(def: parse::Def, sig: desugared::Signature) -> desugared::Def {
    let parameters = def.parameters;
    let body = desugar_expression(def.body);
    let body = parameters
        .into_iter()
        .rfold(body, |acc, next| desugared::Expression {
            location: next.location.merge(acc.location),
            kind: Box::new(desugared::ExpressionKind::Lambda(next, acc)),
        });
    desugared::Def {
        name: def.name,
        signature: sig,
        body,
    }
}

pub fn desugar_expression(e: parse::Expression) -> desugared::Expression {
    let location = e.location;
    let kind = match *e.kind {
        parse::ExpressionKind::Ident(symbol) => desugared::ExpressionKind::Ident(symbol),
        parse::ExpressionKind::Number(n) => desugared::ExpressionKind::Number(n),
        parse::ExpressionKind::String(s) => desugared::ExpressionKind::String(s),
        parse::ExpressionKind::Constructor(symbol, expressions) => {
            desugared::ExpressionKind::Constructor(
                symbol,
                expressions.into_iter().map(desugar_expression).collect(),
            )
        }
        parse::ExpressionKind::Binary(op, lhs, rhs) => {
            let internal_name = op.inner.internal_name();
            let symbol = tree::Symbol {
                location: op.location,
                name: internal_name.to_string(),
            };
            let expr_symbol = desugared::Expression {
                location,
                kind: Box::new(desugared::ExpressionKind::Ident(symbol)),
            };
            let lhs = desugar_expression(lhs);
            let apply1 = desugared::Expression {
                location: expr_symbol.location.merge(lhs.location),
                kind: Box::new(desugared::ExpressionKind::Application(expr_symbol, lhs)),
            };
            let rhs = desugar_expression(rhs);
            desugared::ExpressionKind::Application(apply1, rhs)
        }
        parse::ExpressionKind::Lambda(symbols, expression) => {
            let expression = desugar_expression(expression);
            return symbols
                .into_iter()
                .rfold(expression, |acc, next| desugared::Expression {
                    location: next.location.merge(acc.location),
                    kind: Box::new(desugared::ExpressionKind::Lambda(next, acc)),
                });
        }
        parse::ExpressionKind::Application(function, argument) => {
            let function = desugar_expression(function);
            let argument = desugar_expression(argument);
            desugared::ExpressionKind::Application(function, argument)
        }
        parse::ExpressionKind::Match(scrutinee, arms) => {
            let scrutinee = desugar_expression(scrutinee);
            let arms = arms.into_iter().map(desugar_arm).collect();
            desugared::ExpressionKind::Match(scrutinee, arms)
        }
        parse::ExpressionKind::Block(statements) => desugared::ExpressionKind::Block(
            statements.into_iter().map(desugar_statement).collect(),
        ),
    };
    desugared::Expression {
        location,
        kind: Box::new(kind),
    }
}

pub fn desugar_pattern(pattern: parse::Pattern) -> desugared::Pattern {
    let location = pattern.location;
    let kind = match *pattern.kind {
        parse::PatternKind::Ident(symbol) => desugared::PatternKind::Ident(symbol),
        parse::PatternKind::Number(n) => desugared::PatternKind::Number(n),
        parse::PatternKind::String(s) => desugared::PatternKind::String(s),
        parse::PatternKind::Constructor(symbol, patterns) => desugared::PatternKind::Constructor(
            symbol,
            patterns.into_iter().map(desugar_pattern).collect(),
        ),
        parse::PatternKind::Annot(pattern, r#type) => {
            desugared::PatternKind::Annot(desugar_pattern(pattern), desugar_type(r#type))
        }
    };
    desugared::Pattern {
        location,
        kind: Box::new(kind),
    }
}

pub fn desugar_type(r#type: parse::Type) -> desugared::Type {
    let location = r#type.location;
    let kind = match *r#type.kind {
        parse::TypeKind::Var(symbol) => desugared::TypeKind::Var(symbol),
        parse::TypeKind::Generic(symbol, items) => {
            desugared::TypeKind::Generic(symbol, items.into_iter().map(desugar_type).collect())
        }
        parse::TypeKind::Arrow(a, b) => {
            desugared::TypeKind::Arrow(desugar_type(a), desugar_type(b))
        }
    };
    desugared::Type {
        location,
        kind: Box::new(kind),
    }
}

pub fn desugar_statement(statement: parse::Statement) -> desugared::Statement {
    let location = statement.location;
    let kind = match statement.kind {
        parse::StatementKind::Let(symbol, expression) => {
            desugared::StatementKind::Let(symbol, desugar_expression(expression))
        }
        parse::StatementKind::Expression(expression) => {
            desugared::StatementKind::Expression(desugar_expression(expression))
        }
    };
    desugared::Statement { location, kind }
}

pub fn desugar_arm(arm: parse::Arm) -> desugared::Arm {
    desugared::Arm {
        pattern: desugar_pattern(arm.pattern),
        expression: desugar_expression(arm.expression),
    }
}
