use crate::{
    report,
    tree::{desugared, elaborated},
};

#[derive(Debug)]
pub enum TypeKind {
    Var(String),
    Generalized(usize),
    Hole(Hole),
    Arrow(Type, Type),
    Generic(String, Vec<Type>),
    Error,
}

impl TypeKind {
    pub fn unit() -> Type {
        Type::new(TypeKind::Generic("Unit".to_string(), vec![]))
    }

    pub fn number() -> Type {
        Type::new(TypeKind::Generic("Number".to_string(), vec![]))
    }

    pub fn string() -> Type {
        Type::new(TypeKind::Generic("String".to_string(), vec![]))
    }

    pub fn arith() -> Type {
        Type::new(TypeKind::Arrow(
            TypeKind::number(),
            Type::new(TypeKind::Arrow(TypeKind::number(), TypeKind::number())),
        ))
    }
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn need_parens(r#type: &Type) -> bool {
            match &**r#type {
                TypeKind::Arrow(..) => true,
                TypeKind::Hole(hole) => match hole.get() {
                    HoleInner::Bound(ref inner) => need_parens(inner),
                    HoleInner::Unbound(..) => false,
                },
                TypeKind::Var(_)
                | TypeKind::Generalized(_)
                | TypeKind::Generic(..)
                | TypeKind::Error => false,
            }
        }
        match self {
            TypeKind::Var(name) => write!(f, "{name}"),
            TypeKind::Generalized(id) => write!(f, "#{id}"),
            TypeKind::Hole(hole) => match hole.get() {
                HoleInner::Bound(inner) => write!(f, "{inner}"),
                HoleInner::Unbound(name, _) => write!(f, "?{name}"),
            },
            TypeKind::Arrow(a, b) if need_parens(a) => write!(f, "({a}) -> {b}"),
            TypeKind::Arrow(a, b) => write!(f, "{a} -> {b}"),
            TypeKind::Generic(name, types) if types.is_empty() => write!(f, "{name}"),
            TypeKind::Generic(name, types) => {
                write!(
                    f,
                    "{name}({})",
                    types
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            TypeKind::Error => write!(f, "<Error>"),
        }
    }
}

impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(a), Self::Var(b)) => a == b,
            (Self::Generalized(a), Self::Generalized(b)) => a == b,
            (Self::Hole(a), Self::Hole(b)) if a == b => true,
            (Self::Hole(a), b) => match a.get() {
                HoleInner::Bound(inner) => &*inner == b,
                HoleInner::Unbound(..) => false,
            },
            (a, Self::Hole(b)) => match b.get() {
                HoleInner::Bound(inner) => &*inner == a,
                HoleInner::Unbound(..) => false,
            },
            (Self::Arrow(a, b), Self::Arrow(c, d)) => a == c && b == d,
            (Self::Generic(a, b), Self::Generic(c, d)) => a == c && b == d,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum HoleInner {
    Bound(Type),
    Unbound(usize, usize),
}

#[derive(Clone, Debug)]
pub struct Hole(std::rc::Rc<std::cell::RefCell<HoleInner>>);

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        std::rc::Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Hole {}

impl Hole {
    pub fn new(inner: HoleInner) -> Self {
        Self(std::rc::Rc::new(std::cell::RefCell::new(inner)))
    }

    pub fn get(&self) -> HoleInner {
        self.0.borrow().clone()
    }

    pub fn get_mut(&self) -> std::cell::RefMut<'_, HoleInner> {
        self.0.borrow_mut()
    }
}

pub type Type = std::rc::Rc<TypeKind>;

impl TypeKind {
    pub fn instantiate(self: Type, subs: &[Type]) -> Type {
        match &*self {
            TypeKind::Var(_) => self.clone(),
            TypeKind::Generalized(id) => subs[*id].clone(),
            TypeKind::Hole(hole) => match hole.get() {
                HoleInner::Bound(inner) => inner.instantiate(subs),
                HoleInner::Unbound(..) => self.clone(),
            },
            TypeKind::Arrow(a, b) => {
                let a = a.clone().instantiate(subs);
                let b = b.clone().instantiate(subs);
                Type::new(TypeKind::Arrow(a, b))
            }
            TypeKind::Generic(name, types) => {
                let types = types.iter().cloned().map(|t| t.instantiate(subs)).collect();
                Type::new(TypeKind::Generic(name.clone(), types))
            }
            TypeKind::Error => self.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub binds: Vec<String>,
    pub r#type: Type,
}

impl Scheme {
    pub fn new(binds: Vec<String>, r#type: Type) -> Self {
        Self { binds, r#type }
    }

    pub fn skolemize(&self) -> Type {
        let mut subs = vec![];

        for bind in self.binds.iter() {
            subs.push(Type::new(TypeKind::Var(bind.clone())));
        }

        self.r#type.clone().instantiate(&subs)
    }
}

#[derive(Clone, Debug)]
pub struct TypeEnv {
    pub(crate) variables: rpds::HashTrieMap<String, Scheme>,
    pub(crate) constructors: rpds::HashTrieMap<String, (Scheme, usize)>,
    pub(crate) definitions: rpds::HashTrieMap<String, Scheme>,
    pub(crate) datatypes: rpds::HashTrieMap<String, Data>,
    pub(crate) type_vars: rpds::HashTrieMap<String, Type>,

    name: std::rc::Rc<std::cell::RefCell<usize>>,
    level: std::cell::RefCell<usize>,

    unit_type: Type,
    number_type: Type,
    string_type: Type,

    reporter: report::Reporter,
}

#[derive(Clone, Debug)]
pub struct Data {
    pub constructors: Vec<desugared::Constructor>,
    pub arity: usize,
}

pub(crate) fn builtin_definitions() -> rpds::HashTrieMap<String, Scheme> {
    rpds::ht_map![
        "%add".to_string() => Scheme::new(vec![], TypeKind::arith()),
        "%sub".to_string() => Scheme::new(vec![], TypeKind::arith()),
        "%mul".to_string() => Scheme::new(vec![], TypeKind::arith()),
        "%div".to_string() => Scheme::new(vec![], TypeKind::arith())
    ]
}

impl TypeEnv {
    pub fn new(
        unit_type: Type,
        number_type: Type,
        string_type: Type,
        builtins: rpds::HashTrieMap<String, Scheme>,
        reporter: report::Reporter,
    ) -> Self {
        let datatypes = rpds::ht_map![
            "Unit".to_string() => Data { constructors: vec![], arity: 0 },
            "Number".to_string() => Data { constructors: vec![], arity: 0 },
            "String".to_string() => Data { constructors: vec![], arity: 0 }
        ];
        Self {
            variables: Default::default(),
            constructors: Default::default(),
            definitions: builtins,
            datatypes,
            type_vars: Default::default(),
            name: Default::default(),
            level: Default::default(),
            unit_type,
            number_type,
            string_type,
            reporter,
        }
    }

    pub fn instantiate(&self, scheme: Scheme) -> Type {
        let holes = scheme
            .binds
            .iter()
            .map(|_| self.new_hole())
            .collect::<Vec<_>>();
        scheme.r#type.instantiate(&holes)
    }

    pub fn enter_level(&self) {
        *self.level.borrow_mut() += 1;
    }

    pub fn leave_level(&self) {
        *self.level.borrow_mut() -= 1;
    }

    pub fn new_name(&self) -> usize {
        let mut mut_name = self.name.borrow_mut();
        let name = *mut_name;
        *mut_name += 1;
        name
    }

    pub fn new_hole(&self) -> Type {
        let name = self.new_name();
        let level = *self.level.borrow();
        Type::new(TypeKind::Hole(Hole::new(HoleInner::Unbound(name, level))))
    }
}

pub trait Infer {
    type Out;

    fn infer(self, env: TypeEnv, location: crate::location::Location) -> (Self::Out, Type);
}

mod unification {
    use crate::{
        checker::{Hole, HoleInner, Type, TypeEnv, TypeKind},
        report,
    };

    struct UnifyError {
        between: (Type, Type),
        location: crate::location::Location,
    }

    impl report::Diag for UnifyError {
        fn severity(&self) -> report::Severity {
            report::Severity::Error
        }

        fn message(&self) -> String {
            "unification error".to_string()
        }

        fn markers(&self) -> Vec<report::Marker> {
            let (a, b) = &self.between;
            vec![report::Marker::new(
                format!("type mismatch between '{a}' and '{b}'",),
                self.location,
            )]
        }

        fn hint(&self) -> Option<String> {
            None
        }

        fn location(&self) -> crate::location::Location {
            self.location
        }
    }

    pub fn unify(a: Type, b: Type, env: &TypeEnv, location: crate::location::Location) {
        if !_unify(a.clone(), b.clone(), env) {
            env.reporter.report(UnifyError {
                between: (a, b),
                location,
            });
        }
    }

    fn _unify(a: Type, b: Type, env: &TypeEnv) -> bool {
        if std::rc::Rc::ptr_eq(&a, &b) {
            return true;
        }

        match (&*a, &*b) {
            (TypeKind::Var(a), TypeKind::Var(b)) if a == b => true,

            (TypeKind::Generalized(a), TypeKind::Generalized(b)) if a == b => true,

            (TypeKind::Hole(a), TypeKind::Hole(b)) if a == b => true,
            (TypeKind::Hole(a), _) => unify_hole(a.clone(), b.clone(), env, false),
            (_, TypeKind::Hole(b)) => unify_hole(b.clone(), a.clone(), env, true),

            (TypeKind::Arrow(a, b), TypeKind::Arrow(c, d)) => {
                _unify(a.clone(), c.clone(), env) & _unify(b.clone(), d.clone(), env)
            }

            (TypeKind::Generic(a, b), TypeKind::Generic(c, d))
                if (a == c) & (b.len() == d.len()) =>
            {
                b.iter()
                    .zip(d)
                    .all(|(x, y)| _unify(x.clone(), y.clone(), env))
            }

            (TypeKind::Error, _) | (_, TypeKind::Error) => true,

            (_, _) => false,
        }
    }

    fn unify_hole(hole: Hole, r#type: Type, env: &TypeEnv, flip: bool) -> bool {
        match hole.get() {
            HoleInner::Bound(inner) if flip => _unify(r#type, inner, env),
            HoleInner::Bound(inner) => _unify(inner, r#type, env),
            HoleInner::Unbound(name, level) => {
                if occurs(name, level, r#type.clone()) {
                    // env.reporter.report(OccursCheck { between: () });
                    false
                } else {
                    *hole.get_mut() = HoleInner::Bound(r#type);
                    true
                }
            }
        }
    }

    fn occurs(name: usize, level: usize, r#type: Type) -> bool {
        match &*r#type {
            TypeKind::Var(_) => false,
            TypeKind::Generalized(_) => false,
            TypeKind::Hole(hole) => match hole.get() {
                crate::checker::HoleInner::Bound(inner) => occurs(name, level, inner),
                crate::checker::HoleInner::Unbound(name_, level_) => {
                    *hole.get_mut() = HoleInner::Unbound(name_, level.min(level_));
                    name == name_
                }
            },
            TypeKind::Arrow(a, b) => {
                occurs(name, level, a.clone()) || occurs(name, level, b.clone())
            }
            TypeKind::Generic(_, types) => types.iter().cloned().any(|t| occurs(name, level, t)),
            TypeKind::Error => false,
        }
    }
}

mod errors {
    use crate::{checker::Type, report};

    pub struct ArityError {
        pub arity: usize,
        pub location: crate::location::Location,
    }

    impl report::Diag for ArityError {
        fn severity(&self) -> report::Severity {
            report::Severity::Error
        }

        fn message(&self) -> String {
            "arity error".to_string()
        }

        fn markers(&self) -> Vec<report::Marker> {
            vec![report::Marker::new(
                format!("expected arity: {}", self.arity),
                self.location,
            )]
        }

        fn hint(&self) -> Option<String> {
            None
        }

        fn location(&self) -> crate::location::Location {
            self.location
        }
    }

    pub struct UnboundName {
        pub name: String,
        pub location: crate::location::Location,
    }

    impl report::Diag for UnboundName {
        fn severity(&self) -> report::Severity {
            report::Severity::Error
        }

        fn message(&self) -> String {
            "unbound name".to_string()
        }

        fn markers(&self) -> Vec<report::Marker> {
            vec![report::Marker::new(
                format!("unbound name '{}'", self.name),
                self.location,
            )]
        }

        fn hint(&self) -> Option<String> {
            None
        }

        fn location(&self) -> crate::location::Location {
            self.location
        }
    }

    pub struct TypeMismatch {
        pub got: Type,
        pub expected: Type,
        pub location: crate::location::Location,
    }

    impl report::Diag for TypeMismatch {
        fn severity(&self) -> report::Severity {
            report::Severity::Error
        }

        fn message(&self) -> String {
            "type mismatch".to_string()
        }

        fn markers(&self) -> Vec<report::Marker> {
            vec![report::Marker::new(
                format!("got {} but expected {}", self.got, self.expected),
                self.location,
            )]
        }

        fn hint(&self) -> Option<String> {
            None
        }

        fn location(&self) -> crate::location::Location {
            self.location
        }
    }
}

impl Infer for desugared::Expression {
    type Out = elaborated::Expression;

    fn infer(self, env: TypeEnv, _location: crate::location::Location) -> (Self::Out, Type) {
        let location = self.location;
        let (elab, r#type) = self.kind.infer(env, location);

        let elab = elaborated::Expression {
            location,
            kind: Box::new(elab),
            r#type: r#type.clone(),
        };
        (elab, r#type)
    }
}

fn infer_expr_error() -> (elaborated::ExpressionKind, Type) {
    (
        elaborated::ExpressionKind::Error,
        Type::new(TypeKind::Error),
    )
}

impl Infer for desugared::ExpressionKind {
    type Out = elaborated::ExpressionKind;

    fn infer(self, env: TypeEnv, location: crate::location::Location) -> (Self::Out, Type) {
        match self {
            desugared::ExpressionKind::Ident(symbol) => {
                let mut scope = vec![&env.definitions, &env.variables];

                let mut scheme = None;

                while let Some(scope) = scope.pop()
                    && scheme.is_none()
                {
                    scheme = scope.get(symbol.as_str());
                }

                match scheme.cloned() {
                    Some(s) => {
                        let t = env.instantiate(s);
                        (elaborated::ExpressionKind::Ident(symbol), t)
                    }
                    None => {
                        env.reporter.report(errors::UnboundName {
                            name: symbol.name.clone(),
                            location,
                        });
                        infer_expr_error()
                    }
                }
            }
            desugared::ExpressionKind::Number(n) => (
                elaborated::ExpressionKind::Number(n),
                env.number_type.clone(),
            ),
            desugared::ExpressionKind::String(s) => (
                elaborated::ExpressionKind::String(s),
                env.string_type.clone(),
            ),
            desugared::ExpressionKind::Constructor(symbol, expressions) => {
                match env.constructors.get(symbol.as_str()).cloned() {
                    Some((_, arity)) if expressions.len() != arity => {
                        env.reporter.report(errors::ArityError { arity, location });
                        infer_expr_error()
                    }
                    Some((s, _)) => {
                        let mut t = env.instantiate(s);

                        let mut elab_expressions = vec![];
                        for expression in expressions {
                            let expression_location = expression.location;
                            let (elab_expression, expression_type) =
                                expression.infer(env.clone(), expression_location);
                            elab_expressions.push(elab_expression);

                            if let TypeKind::Arrow(a, b) = &*t.clone() {
                                t = b.clone();
                                unification::unify(
                                    a.clone(),
                                    expression_type,
                                    &env,
                                    expression_location,
                                );
                            }
                        }

                        let elab =
                            elaborated::ExpressionKind::Constructor(symbol, elab_expressions);
                        (elab, t)
                    }
                    None => {
                        env.reporter.report(errors::UnboundName {
                            name: symbol.name.clone(),
                            location: symbol.location,
                        });
                        infer_expr_error()
                    }
                }
            }
            desugared::ExpressionKind::Lambda(symbol, body) => {
                let hole = env.new_hole();
                let s = Scheme::new(vec![], hole.clone());

                let mut new_env = env.clone();
                new_env.variables.insert_mut(symbol.name.clone(), s);

                let (elab_body, body_type) = body.infer(new_env, location);
                let arrow_type = Type::new(TypeKind::Arrow(hole, body_type));

                let elab = elaborated::ExpressionKind::Lambda(symbol, elab_body);
                (elab, arrow_type)
            }
            desugared::ExpressionKind::Application(fun, arg) => {
                let fun_location = fun.location;
                let (elab_fun, fun_type) = fun.infer(env.clone(), location);
                let (elab_arg, arg_type) = arg.infer(env.clone(), location);

                let return_type = env.new_hole();
                let arrow_type = Type::new(TypeKind::Arrow(arg_type, return_type.clone()));

                unification::unify(fun_type, arrow_type, &env, fun_location);

                let elab = elaborated::ExpressionKind::Application(elab_fun, elab_arg);
                (elab, return_type)
            }
            desugared::ExpressionKind::Match(scrutinee, arms) => {
                let return_type = env.new_hole();

                let (elab_scrutinee, scrutinee_type) = scrutinee.infer(env.clone(), location);

                let mut elab_arms = vec![];

                for arm in arms {
                    let pattern_location = arm.pattern.location;
                    let ((elab_pattern, bindings), pattern_type) =
                        arm.pattern.infer(env.clone(), location);

                    unification::unify(
                        pattern_type,
                        scrutinee_type.clone(),
                        &env,
                        pattern_location,
                    );

                    let mut env = env.clone();
                    for (bind, t) in bindings.iter() {
                        env.variables
                            .insert_mut(bind.clone(), Scheme::new(vec![], t.clone()));
                    }

                    let expression_location = arm.expression.location;
                    let (elab_expression, expression_type) =
                        arm.expression.infer(env.clone(), location);

                    unification::unify(
                        expression_type,
                        return_type.clone(),
                        &env,
                        expression_location,
                    );

                    elab_arms.push(elaborated::Arm {
                        pattern: elab_pattern,
                        expression: elab_expression,
                    });
                }

                let elab = elaborated::ExpressionKind::Match(
                    elab_scrutinee,
                    elaborated::CaseTree::Failure,
                    vec![],
                );
                (elab, return_type)
            }
            desugared::ExpressionKind::Block(statements) => {
                let mut return_type = env.unit_type.clone();

                let mut elab_statements = vec![];
                let mut env = env;
                for statement in statements {
                    let ((elab_statement, next_env), statement_type) =
                        statement.infer(env, location);
                    elab_statements.push(elab_statement);
                    env = next_env;
                    return_type = statement_type;
                }

                (
                    elaborated::ExpressionKind::Block(elab_statements),
                    return_type,
                )
            }
        }
    }
}

impl Infer for desugared::Pattern {
    type Out = (elaborated::Pattern, rpds::HashTrieMap<String, Type>);

    fn infer(self, env: TypeEnv, _location: crate::location::Location) -> (Self::Out, Type) {
        let location = self.location;
        let ((pattern, binds), r#type) = self.kind.infer(env, location);

        let elab = elaborated::Pattern {
            location,
            kind: Box::new(pattern),
            r#type: r#type.clone(),
        };
        ((elab, binds), r#type)
    }
}

fn infer_pattern_error() -> (
    (elaborated::PatternKind, rpds::HashTrieMap<String, Type>),
    Type,
) {
    (
        (elaborated::PatternKind::Error, rpds::ht_map![]),
        Type::new(TypeKind::Error),
    )
}

impl Infer for desugared::PatternKind {
    type Out = (elaborated::PatternKind, rpds::HashTrieMap<String, Type>);

    fn infer(self, env: TypeEnv, location: crate::location::Location) -> (Self::Out, Type) {
        match self {
            desugared::PatternKind::Ident(symbol) => {
                let hole = env.new_hole();
                let binds = rpds::ht_map![symbol.name.clone() => hole.clone()];
                let elab = elaborated::PatternKind::Ident(symbol);
                ((elab, binds), hole)
            }
            desugared::PatternKind::Number(n) => (
                (elaborated::PatternKind::Number(n), rpds::ht_map![]),
                env.number_type.clone(),
            ),
            desugared::PatternKind::String(s) => (
                (elaborated::PatternKind::String(s), rpds::ht_map![]),
                env.string_type.clone(),
            ),
            desugared::PatternKind::Constructor(symbol, _)
                if !env.constructors.contains_key(symbol.as_str()) =>
            {
                env.reporter.report(errors::UnboundName {
                    name: symbol.name.clone(),
                    location,
                });
                infer_pattern_error()
            }
            desugared::PatternKind::Constructor(symbol, patterns) => {
                let Some((s, arity)) = env.constructors.get(symbol.as_str()).cloned() else {
                    unreachable!()
                };
                if patterns.len() != arity {
                    env.reporter.report(errors::ArityError { arity, location });
                    return infer_pattern_error();
                }

                let mut instantiated = env.instantiate(s);
                let mut elab_patterns = vec![];
                let mut m = rpds::ht_map![];

                for pattern in patterns.into_iter() {
                    let pattern_location = pattern.location;
                    let ((elab_pattern, binds), pattern_type) =
                        pattern.infer(env.clone(), location);
                    elab_patterns.push(elab_pattern);

                    if let TypeKind::Arrow(a, b) = &*instantiated.clone() {
                        instantiated = b.clone();

                        for (bind, t) in binds.iter() {
                            if m.contains_key(bind) {
                                env.reporter.report(report::generic_report(
                                    report::Severity::Warning,
                                    format!("repeated bind '{bind}'"),
                                    location,
                                ));
                            } else {
                                m.insert_mut(bind.clone(), t.clone());
                            }
                        }

                        unification::unify(pattern_type, a.clone(), &env, pattern_location);
                    } else {
                        unreachable!()
                    }
                }

                let elab = elaborated::PatternKind::Constructor(symbol, elab_patterns);
                ((elab, m), instantiated)
            }

            desugared::PatternKind::Annot(pattern, r#type) => {
                let (_, inferred_type) = r#type.infer(env.clone(), location);
                let ((elab_pattern, bindings), t) = pattern.check(inferred_type, &env, location);

                ((*elab_pattern.kind, bindings), t)
            }
        }
    }
}

impl Infer for desugared::Statement {
    type Out = (elaborated::Statement, TypeEnv);

    fn infer(self, env: TypeEnv, _location: crate::location::Location) -> (Self::Out, Type) {
        let location = self.location;
        let ((elab, env), r#type) = self.kind.infer(env, location);

        let elab = elaborated::Statement {
            location,
            kind: elab,
            r#type: r#type.clone(),
        };
        ((elab, env), r#type)
    }
}

impl Infer for desugared::StatementKind {
    type Out = (elaborated::StatementKind, TypeEnv);

    fn infer(self, env: TypeEnv, location: crate::location::Location) -> (Self::Out, Type) {
        match self {
            desugared::StatementKind::Let(symbol, expression) => {
                let unit = env.unit_type.clone();
                env.enter_level();
                let (elab_expression, expression_type) = expression.infer(env.clone(), location);
                env.leave_level();

                // let generalized =
                let mut env = env.clone();
                env.variables
                    .insert_mut(symbol.name.clone(), Scheme::new(vec![], expression_type));

                let elab = elaborated::StatementKind::Let(symbol, elab_expression);
                ((elab, env), unit)
            }
            desugared::StatementKind::Expression(expression) => {
                let (elab_expression, expression_type) = expression.infer(env.clone(), location);
                let elab = elaborated::StatementKind::Expression(elab_expression);
                ((elab, env), expression_type)
            }
        }
    }
}

impl Infer for desugared::Type {
    type Out = ();

    fn infer(self, env: TypeEnv, _location: crate::location::Location) -> (Self::Out, Type) {
        self.kind.infer(env, self.location)
    }
}

impl Infer for desugared::TypeKind {
    type Out = ();

    fn infer(self, env: TypeEnv, location: crate::location::Location) -> (Self::Out, Type) {
        match self {
            desugared::TypeKind::Var(symbol) => match env.type_vars.get(symbol.as_str()).cloned() {
                Some(t) => ((), t),
                None => {
                    env.reporter.report(errors::UnboundName {
                        name: symbol.name.clone(),
                        location,
                    });
                    ((), Type::new(TypeKind::Error))
                }
            },
            desugared::TypeKind::Generic(symbol, items) => {
                match env.datatypes.get(symbol.as_str()).cloned() {
                    Some(data) if data.arity != items.len() => {
                        env.reporter.report(errors::ArityError {
                            arity: data.arity,
                            location,
                        });
                        ((), Type::new(TypeKind::Error))
                    }
                    Some(_) => {
                        let ((), items): ((), Vec<Type>) = items
                            .into_iter()
                            .map(|i| i.infer(env.clone(), location))
                            .unzip();

                        ((), Type::new(TypeKind::Generic(symbol.name.clone(), items)))
                    }
                    None => todo!("{}", symbol.as_str()),
                }
            }
            desugared::TypeKind::Arrow(a, b) => {
                let ((), a_type) = a.infer(env.clone(), location);
                let ((), b_type) = b.infer(env, location);
                ((), Type::new(TypeKind::Arrow(a_type, b_type)))
            }
        }
    }
}

pub trait Check {
    type Out;

    fn check(
        self,
        r#type: Type,
        env: &TypeEnv,
        location: crate::location::Location,
    ) -> (Self::Out, Type);
}

mod check_impl {
    use crate::{
        checker::{Check, Infer, Type, TypeEnv, TypeKind, errors, unification},
        tree::{desugared, elaborated},
    };

    impl Check for desugared::Pattern {
        type Out = (elaborated::Pattern, rpds::HashTrieMap<String, Type>);

        fn check(
            self,
            r#type: Type,
            env: &TypeEnv,
            _location: crate::location::Location,
        ) -> (Self::Out, Type) {
            let location = self.location;
            let ((elab_pattern, binds), r#type) = self.kind.check(r#type, env, location);

            let elab = elaborated::Pattern {
                location,
                kind: Box::new(elab_pattern),
                r#type: r#type.clone(),
            };
            ((elab, binds), r#type)
        }
    }

    impl Check for desugared::PatternKind {
        type Out = (elaborated::PatternKind, rpds::HashTrieMap<String, Type>);

        fn check(
            self,
            r#type: Type,
            env: &TypeEnv,
            location: crate::location::Location,
        ) -> (Self::Out, Type) {
            match (&self, &*r#type) {
                (desugared::PatternKind::Number(n), t) if t == &*env.number_type => (
                    (elaborated::PatternKind::Number(*n), rpds::ht_map![]),
                    env.number_type.clone(),
                ),
                (desugared::PatternKind::String(s), t) if t == &*env.string_type => (
                    (
                        elaborated::PatternKind::String(s.to_owned()),
                        rpds::ht_map![],
                    ),
                    env.string_type.clone(),
                ),
                (_, _) => {
                    let ((elab_pattern, bindings), pattern_type) =
                        self.infer(env.clone(), location);

                    unification::unify(pattern_type.clone(), r#type.clone(), env, location);

                    if pattern_type == r#type {
                        ((elab_pattern, bindings), r#type)
                    } else {
                        env.reporter.report(errors::TypeMismatch {
                            got: pattern_type,
                            expected: r#type,
                            location,
                        });
                        (
                            (elaborated::PatternKind::Error, bindings),
                            Type::new(TypeKind::Error),
                        )
                    }
                }
            }
        }
    }
}

pub trait Declare {
    fn declare(&self, env: TypeEnv) -> TypeEnv;
}

pub trait Define {
    fn define(self, env: TypeEnv) -> TypeEnv;
}

impl Declare for desugared::Data {
    fn declare(&self, mut env: TypeEnv) -> TypeEnv {
        let name = self.name.name.clone();
        let arity = self.generics.len();
        env.datatypes.insert_mut(
            name,
            Data {
                constructors: self.constructors.clone(),
                arity,
            },
        );
        env
    }
}

impl Declare for desugared::Signature {
    fn declare(&self, env: TypeEnv) -> TypeEnv {
        let name = self.name.name.clone();
        let mut next_env = env.clone();

        // let mut env_mut = env.clone();
        let free_variables = self.r#type.free_variables();
        for (i, name) in free_variables.iter().enumerate() {
            next_env
                .type_vars
                .insert_mut(name.to_string(), Type::new(TypeKind::Generalized(i)));
        }

        // env_mut.enter_level();
        let ((), internal_type) = self
            .r#type
            .clone()
            .infer(next_env.clone(), self.name.location);
        // env_mut.leave_level();

        let scheme = Scheme::new(
            free_variables
                .into_iter()
                .map(|var| var.to_string())
                .collect::<Vec<_>>(),
            internal_type,
        );
        next_env.definitions.insert_mut(name, scheme);
        next_env
    }
}

impl Define for desugared::Data {
    fn define(self, env: TypeEnv) -> TypeEnv {
        let name = self.name.clone();
        let mut next_env = env.clone();

        let scheme = self
            .generics
            .iter()
            .map(|s| s.name.clone())
            .collect::<Vec<_>>();
        let vars = scheme
            .iter()
            .enumerate()
            .map(|(i, _)| Type::new(TypeKind::Generalized(i)))
            .collect::<Vec<_>>();

        scheme.iter().zip(vars.clone()).for_each(|(name, t)| {
            next_env.type_vars.insert_mut(name.clone(), t);
        });
        let return_type = Type::new(TypeKind::Generic(name.name, vars));

        for ctor in self.constructors {
            let arity = ctor.types.len();

            let (_, types): ((), Vec<Type>) = ctor
                .types
                .into_iter()
                .map(|t| t.infer(next_env.clone(), name.location))
                .unzip();
            let arrow = types.into_iter().rfold(return_type.clone(), |acc, next| {
                Type::new(TypeKind::Arrow(next, acc))
            });

            let scheme = Scheme::new(scheme.clone(), arrow);
            next_env
                .constructors
                .insert_mut(ctor.name.name, (scheme, arity));
        }
        next_env
    }
}

impl Define for desugared::Def {
    fn define(self, env: TypeEnv) -> TypeEnv {
        let name = self.name;
        let signature = env.definitions.get(name.as_str()).unwrap();
        let signature = signature.skolemize();

        let body_location = self.body.location;
        let (_elab_body, body_type) = self.body.infer(env.clone(), name.location);

        unification::unify(signature, body_type, &env, body_location);

        env
    }
}
