//! All kinds of trees used through the compiler.

#[derive(Clone, Debug)]
pub struct Symbol {
    pub location: crate::location::Location,
    pub name: String,
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl parse::Op {
    pub fn internal_name(self) -> &'static str {
        match self {
            parse::Op::Add => "%add",
            parse::Op::Sub => "%sub",
            parse::Op::Mul => "%mul",
            parse::Op::Div => "%div",
            parse::Op::Lth => "%lth",
            parse::Op::Lte => "%lte",
            parse::Op::Gth => "%gth",
            parse::Op::Gte => "%gte",
            parse::Op::Eql => "%eql",
            parse::Op::Neq => "%neq",
        }
    }
}

pub mod parse {
    use crate::location::Located;
    use crate::tree::Symbol;

    #[derive(Debug)]
    pub enum ExpressionKind {
        Ident(Symbol),
        Number(i32),
        String(String),
        Constructor(Symbol, Vec<Expression>),

        Binary(Located<Op>, Expression, Expression),

        Lambda(Vec<Symbol>, Expression),
        Application(Expression, Expression),
        Match(Expression, Vec<Arm>),

        Block(Vec<Statement>),
    }

    #[derive(Debug)]
    pub struct Arm {
        pub pattern: Pattern,
        pub expression: Expression,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum Op {
        Add,
        Sub,
        Mul,
        Div,
        Lth,
        Lte,
        Gth,
        Gte,
        Eql,
        Neq,
    }

    #[derive(Debug)]
    pub struct Expression {
        pub location: crate::location::Location,
        pub kind: Box<ExpressionKind>,
    }

    #[derive(Debug)]
    pub enum PatternKind {
        Ident(Symbol),
        Number(i32),
        String(String),
        Constructor(Symbol, Vec<Pattern>),
        Annot(Pattern, Type),
    }

    #[derive(Debug)]
    pub struct Pattern {
        pub location: crate::location::Location,
        pub kind: Box<PatternKind>,
    }

    #[derive(Debug)]
    pub enum StatementKind {
        Let(Symbol, Expression),
        Expression(Expression),
    }

    #[derive(Debug)]
    pub struct Statement {
        pub location: crate::location::Location,
        pub kind: StatementKind,
    }

    #[derive(Debug)]
    pub enum TypeKind {
        Var(Symbol),
        Generic(Symbol, Vec<Type>),
        Arrow(Type, Type),
    }

    #[derive(Debug)]
    pub struct Type {
        pub location: crate::location::Location,
        pub kind: Box<TypeKind>,
    }

    #[derive(Debug)]
    pub struct Data {
        pub name: Symbol,
        pub generics: Vec<Symbol>,
        pub constructors: Vec<Constructor>,
    }

    #[derive(Debug)]
    pub struct Constructor {
        pub name: Symbol,
        pub types: Vec<Type>,
    }

    #[derive(Debug)]
    pub struct Sig {
        pub name: Symbol,
        pub r#type: Type,
    }

    #[derive(Debug)]
    pub struct Def {
        pub name: Symbol,
        pub parameters: Vec<Symbol>,
        pub body: Expression,
    }

    #[derive(Debug)]
    pub enum TopLevel {
        Data(Data),
        Sig(Sig),
        Def(Def),
    }

    #[derive(Debug)]
    pub struct Program {
        pub definitions: Vec<TopLevel>,
    }
}

pub mod desugared {
    use crate::tree::Symbol;

    #[derive(Debug)]
    pub enum ExpressionKind {
        Ident(Symbol),
        Number(i32),
        String(String),
        Constructor(Symbol, Vec<Expression>),

        Lambda(Symbol, Expression),
        Application(Expression, Expression),
        Match(Expression, Vec<Arm>),

        Block(Vec<Statement>),
    }

    #[derive(Debug)]
    pub struct Expression {
        pub location: crate::location::Location,
        pub kind: Box<ExpressionKind>,
    }

    #[derive(Debug)]
    pub struct Arm {
        pub pattern: Pattern,
        pub expression: Expression,
    }

    #[derive(Debug)]
    pub enum PatternKind {
        Ident(Symbol),
        Number(i32),
        String(String),
        Constructor(Symbol, Vec<Pattern>),
        Annot(Pattern, Type),
    }

    #[derive(Debug)]
    pub struct Pattern {
        pub location: crate::location::Location,
        pub kind: Box<PatternKind>,
    }

    #[derive(Debug)]
    pub enum StatementKind {
        Let(Symbol, Expression),
        Expression(Expression),
    }

    #[derive(Debug)]
    pub struct Statement {
        pub location: crate::location::Location,
        pub kind: StatementKind,
    }

    #[derive(Clone, Debug)]
    pub enum TypeKind {
        Var(Symbol),
        Generic(Symbol, Vec<Type>),
        Arrow(Type, Type),
    }

    #[derive(Clone, Debug)]
    pub struct Type {
        pub location: crate::location::Location,
        pub kind: Box<TypeKind>,
    }

    #[derive(Debug)]
    pub enum TopLevel {
        Data(Data),
        Def(Def),
    }

    #[derive(Debug)]
    pub struct Data {
        pub name: Symbol,
        pub generics: Vec<Symbol>,
        pub constructors: Vec<Constructor>,
    }

    #[derive(Clone, Debug)]
    pub struct Constructor {
        pub name: Symbol,
        pub types: Vec<Type>,
    }

    #[derive(Debug)]
    pub struct Signature {
        pub name: Symbol,
        pub r#type: Type,
    }

    #[derive(Debug)]
    pub struct Def {
        pub name: Symbol,
        pub signature: Signature,
        pub body: Expression,
    }

    #[derive(Debug)]
    pub struct Program {
        pub definitions: Vec<TopLevel>,
    }

    impl Type {
        pub fn free_variables(&self) -> indexmap::IndexSet<&str> {
            self.kind.free_variables()
        }
    }

    impl TypeKind {
        pub fn free_variables(&self) -> indexmap::IndexSet<&str> {
            match self {
                TypeKind::Var(symbol) => indexmap::indexset![symbol.as_str()],
                TypeKind::Generic(_, items) => {
                    items.iter().flat_map(|i| i.free_variables()).collect()
                }
                TypeKind::Arrow(a, b) => [a, b].iter().flat_map(|i| i.free_variables()).collect(),
            }
        }
    }
}

pub mod elaborated {
    use crate::{checker, tree::Symbol};

    #[derive(Debug)]
    pub enum ExpressionKind {
        Ident(Symbol),
        Number(i32),
        String(String),
        Constructor(Symbol, Vec<Expression>),

        Lambda(Symbol, Expression),
        Application(Expression, Expression),
        Match(Expression, CaseTree, Vec<Expression>),

        Block(Vec<Statement>),

        Error,
    }

    #[derive(Clone, Debug)]
    pub enum CaseTree {
        Failure,
        Leaf(checker::exhaustive::Row<checker::exhaustive::Case>),
        Switch(Vec<(String, CaseTree)>, Option<Box<CaseTree>>),
    }

    #[derive(Debug)]
    pub enum Case {
        Number(i32),
        String(String),
        Constructor(String, usize),
    }

    #[derive(Debug)]
    pub struct Expression {
        pub location: crate::location::Location,
        pub kind: Box<ExpressionKind>,
        pub r#type: checker::Type,
    }

    #[derive(Debug)]
    pub struct Arm {
        pub pattern: Pattern,
        pub expression: Expression,
    }

    #[derive(Clone, Debug)]
    pub enum PatternKind {
        Ident(Symbol),
        Number(i32),
        String(String),
        Constructor(Symbol, Vec<Pattern>),
        Error,
    }

    #[derive(Clone, Debug)]
    pub struct Pattern {
        pub location: crate::location::Location,
        pub kind: Box<PatternKind>,
        pub r#type: checker::Type,
    }

    #[derive(Debug)]
    pub enum StatementKind {
        Let(Symbol, Expression),
        Expression(Expression),
    }

    #[derive(Debug)]
    pub struct Statement {
        pub location: crate::location::Location,
        pub kind: StatementKind,
        pub r#type: checker::Type,
    }
}
