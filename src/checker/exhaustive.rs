use std::collections::VecDeque;

use crate::{checker, tree::elaborated};

#[derive(Clone, Debug)]
pub enum Case {
    Wildcard,
    Number(i32),
    String(String),
    Constructor(String, Vec<Case>),
}

impl std::fmt::Display for Case {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Case::Wildcard => write!(f, "_"),
            Case::Number(n) => write!(f, "{n}"),
            Case::String(s) => write!(f, "{s:?}"),
            Case::Constructor(name, cases) => {
                write!(f, "{name}")?;
                write!(f, "(")?;
                let cases = cases
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{cases}")?;
                write!(f, ")")
            }
        }
    }
}

fn from_pattern(value: elaborated::Pattern) -> Option<Case> {
    match *value.kind {
        elaborated::PatternKind::Ident(_) => Some(Case::Wildcard),
        elaborated::PatternKind::Number(n) => Some(Case::Number(n)),
        elaborated::PatternKind::String(s) => Some(Case::String(s)),
        elaborated::PatternKind::Constructor(symbol, patterns) => Some(Case::Constructor(
            symbol.name,
            patterns
                .into_iter()
                .map(from_pattern)
                .collect::<Option<Vec<_>>>()?,
        )),
        elaborated::PatternKind::Error => None,
    }
}

#[derive(Clone, Debug)]
pub struct Row<T: Clone> {
    index: usize,
    inner: VecDeque<T>,
}

impl<T: Clone + std::fmt::Display> std::fmt::Display for Row<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, t) in self.inner.iter().enumerate() {
            write!(f, "{t}")?;
            if i != 0 {
                write!(f, ",")?;
            }
        }
        Ok(())
    }
}

impl<T: Clone> Row<T> {
    fn pop_front(&self) -> Self {
        let mut row = self.clone();
        row.inner.pop_front();
        row
    }

    fn preppend(&self, item: T) -> Self {
        let mut row = self.clone();
        row.inner.push_front(item);
        row
    }

    fn split(&self, at: usize) -> (Self, Self) {
        let mut inner = self.inner.clone();
        let splitted = inner.split_off(at);
        (
            Self {
                index: self.index,
                inner,
            },
            Self {
                index: self.index,
                inner: splitted,
            },
        )
    }

    pub(crate) fn first(&self) -> &T {
        &self.inner[0]
    }

    fn inline(&self, mut other: VecDeque<T>) -> Self {
        let mut row = self.clone();
        row.inner.pop_front();
        other.extend(row.inner);
        Self {
            index: self.index,
            inner: other,
        }
    }
}

impl Row<Case> {
    fn specialize(&self, case: Case) -> Vec<Row<Case>> {
        let first = self.first();
        match (first, case) {
            (Case::Wildcard, Case::Wildcard) => vec![self.pop_front()],

            (Case::Wildcard, Case::Constructor(_, args)) => {
                vec![self.inline(vec![Case::Wildcard; args.len()].into())]
            }
            (Case::Constructor(a, b), Case::Constructor(c, _)) if a == c.as_str() => {
                vec![self.inline(b.clone().into())]
            }

            (Case::Wildcard, Case::Number(_)) => vec![self.pop_front()],
            (Case::Number(a), Case::Number(b)) if *a == b => vec![self.pop_front()],

            (Case::Wildcard, Case::String(_)) => vec![self.pop_front()],
            (Case::String(a), Case::String(b)) if *a == b => vec![self.pop_front()],

            (_, _) => vec![],
        }
    }

    fn default(self) -> Vec<Row<Case>> {
        match self.first() {
            Case::Wildcard => vec![self.pop_front()],
            _ => vec![],
        }
    }

    fn is_irrefutable(&self) -> bool {
        self.inner.iter().all(|case| matches!(case, Case::Wildcard))
    }

    fn is_wildcard(&self) -> bool {
        matches!(self.first(), Case::Wildcard)
    }

    fn constructor(&self) -> Option<String> {
        if let Case::Constructor(name, _) = self.first() {
            Some(name.clone())
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
struct Matrix<T: Clone> {
    rows: Vec<Row<T>>,
}

impl Matrix<Case> {
    fn is_wildcard(&self) -> bool {
        self.rows.iter().all(Row::is_wildcard)
    }

    fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    fn is_irrefutable(&self) -> Option<&Row<Case>> {
        if let Some(first) = self.rows.first()
            && first.is_irrefutable()
        {
            return Some(first);
        }
        None
    }

    fn constructors(&self) -> indexmap::IndexSet<String> {
        self.rows.iter().flat_map(|row| row.constructor()).collect()
    }

    fn specialize(self, case: Case) -> Self {
        let rows = self
            .rows
            .into_iter()
            .flat_map(|row| row.specialize(case.clone()))
            .collect();
        Matrix { rows }
    }

    fn default(self) -> Self {
        let rows = self.rows.into_iter().flat_map(Row::default).collect();
        Matrix { rows }
    }
}

#[derive(Debug)]
pub enum Witness {
    Exhaustive(elaborated::CaseTree),
    NonExhaustive(Row<Case>),
}

impl Witness {
    fn prepend(self, case: Case) -> Self {
        let Witness::NonExhaustive(row) = self else {
            return self;
        };

        Witness::NonExhaustive(row.preppend(case))
    }

    fn expand(self, name: String, size: usize) -> Self {
        let Witness::NonExhaustive(row) = self else {
            return self;
        };

        let (left, right) = row.split(size);

        let row = right.preppend(Case::Constructor(name, left.inner.into_iter().collect()));
        Self::NonExhaustive(row)
    }
}

#[derive(Debug)]
pub enum Finitude<T> {
    Infinite,
    Finite(T),
}

#[derive(Debug)]
pub enum Completeness {
    Complete(Finitude<indexmap::IndexSet<String>>),
    Incomplete(
        Finitude<indexmap::IndexSet<String>>,
        indexmap::IndexSet<String>,
    ),
}

#[derive(Clone)]
struct Problem {
    types: Row<checker::Type>,
    case: Row<Case>,
    matrix: Matrix<Case>,
}

pub fn is_exhaustive(
    env: &checker::TypeEnv,
    patterns: Vec<elaborated::Pattern>,
    types: Vec<checker::Type>,
) -> Witness {
    let mut matrix = vec![];
    for (index, pattern) in patterns.into_iter().enumerate() {
        if let Some(case) = from_pattern(pattern) {
            matrix.push(Row {
                index,
                inner: vec![case].into(),
            });
        } else {
            break;
        }
    }

    Problem {
        types: Row {
            index: 0,
            inner: types.into(),
        },
        case: Row {
            index: 0,
            inner: vec![Case::Wildcard].into(),
        },
        matrix: Matrix { rows: matrix },
    }
    .exhaustive(env)
}

impl Problem {
    fn specialize(
        self,
        env: &checker::TypeEnv,
        types: Vec<checker::Type>,
        cases: Vec<Case>,
        case: Case,
    ) -> Witness {
        Self {
            types: self.types.inline(types.into()),
            case: self.case.inline(cases.into()),
            matrix: self.matrix.specialize(case),
        }
        .exhaustive(env)
    }

    fn default(self) -> Self {
        Self {
            types: self.types.pop_front(),
            case: self.case.pop_front(),
            matrix: self.matrix.default(),
        }
    }

    fn exhaustive(self, env: &checker::TypeEnv) -> Witness {
        if self.matrix.is_empty() {
            Witness::NonExhaustive(self.case)
        } else if let Some(row) = self.matrix.is_irrefutable() {
            Witness::Exhaustive(elaborated::CaseTree::Leaf(row.clone()))
        } else {
            self.match_exhaustiveness(env)
        }
    }

    fn completeness(&self, env: &checker::TypeEnv, name: &str) -> Completeness {
        match name {
            "Number" | "String" => {
                return Completeness::Incomplete(Finitude::Infinite, indexmap::indexset! {});
            }
            _ => {}
        }

        if let Some(data) = env.datatypes.get(name) {
            let constructors: indexmap::IndexSet<String> = data
                .constructors
                .iter()
                .map(|c| c.name.name.clone())
                .collect();
            let used = self.matrix.constructors();
            let difference = constructors
                .difference(&used)
                .cloned()
                .collect::<indexmap::IndexSet<_>>();
            if difference.is_empty() || self.matrix.is_wildcard() {
                Completeness::Complete(Finitude::Finite(used))
            } else {
                Completeness::Incomplete(Finitude::Finite(used), difference)
            }
        } else {
            Completeness::Incomplete(Finitude::Infinite, indexmap::indexset! {})
        }
    }

    fn specialize_wildcard(self, env: &checker::TypeEnv) -> Witness {
        self.default().exhaustive(env).prepend(Case::Wildcard)
    }

    fn match_exhaustiveness(self, env: &checker::TypeEnv) -> Witness {
        let case = self.case.first();
        let current = self.types.first().clone().force();

        match (case, &*current) {
            // TODO: make Number and String as internal types instead of generics?
            (Case::Wildcard, checker::TypeKind::Generic(name, types))
                if env.datatypes.contains_key(name) =>
            {
                self.ehxaustiveness_wildcard(env, name.clone(), types.clone())
            }
            (Case::Wildcard, _) => self.specialize_wildcard(env),

            (Case::Number(n), t) if t == &*env.number_type => {
                println!("here number");
                let n = *n;
                self.specialize(env, vec![], vec![], Case::Number(n))
            }
            (Case::String(s), t) if t == &*env.string_type => {
                let s = s.clone();
                self.specialize(env, vec![], vec![], Case::String(s))
            }

            (Case::Constructor(a, b), checker::TypeKind::Generic(_, types)) => {
                let a = a.clone();
                let b = b.clone();
                self.specialize_constructor(env, &a, b, types.clone())
            }

            (a, b) => panic!("reached {a:?} : {b}"),
        }
    }

    fn synthetize(&self, env: &checker::TypeEnv, name: String) -> Case {
        let (_, arity) = env.constructors.get(&name).unwrap();
        Case::Constructor(name, vec![Case::Wildcard; *arity])
    }

    fn ehxaustiveness_wildcard(
        self,
        env: &checker::TypeEnv,
        name: String,
        types: Vec<checker::Type>,
    ) -> Witness {
        if self.matrix.is_wildcard() {
            self.specialize_wildcard(env)
        } else {
            match self.completeness(env, &name) {
                Completeness::Complete(Finitude::Finite(names)) => {
                    self.split(env, names, types, true)
                }
                Completeness::Complete(Finitude::Infinite) => unreachable!(),
                Completeness::Incomplete(Finitude::Finite(names), missing) => {
                    let first = std::mem::take(&mut missing.into_iter().collect::<Vec<_>>()[0]);
                    let case = self.synthetize(env, first);
                    self.clone().split(env, names, types, false).prepend(case)
                }
                Completeness::Incomplete(Finitude::Infinite, _) => {
                    self.specialize_wildcard(env).prepend(Case::Wildcard)
                }
            }
        }
    }

    // Produce a specialized matrix for each collected name. If the signature is not complete,
    // then compute a default matrix as a default case.
    fn split(
        self,
        env: &checker::TypeEnv,
        names: indexmap::IndexSet<String>,
        types: Vec<checker::Type>,
        is_complete: bool,
    ) -> Witness {
        let mut cases = vec![];

        for name in names.into_iter() {
            let (_, arity) = env.constructors.get(&name).unwrap();
            let wildcards = vec![Case::Wildcard; *arity];

            let witness =
                self.clone()
                    .specialize_constructor(env, name.as_str(), wildcards, types.clone());

            match witness {
                Witness::Exhaustive(case_tree) => cases.push((name, case_tree)),
                Witness::NonExhaustive(_) => {
                    return witness.expand(name, *arity);
                }
            }
        }

        let default = if is_complete {
            None
        } else {
            match self.default().exhaustive(env) {
                Witness::Exhaustive(case_tree) => Some(Box::new(case_tree)),
                Witness::NonExhaustive(row) => return Witness::NonExhaustive(row),
            }
        };

        Witness::Exhaustive(elaborated::CaseTree::Switch(cases, default))
    }

    fn specialize_constructor(
        self,
        env: &checker::TypeEnv,
        name: &str,
        cases: Vec<Case>,
        types: Vec<checker::Type>,
    ) -> Witness {
        let case = Case::Constructor(name.to_string(), cases.clone());
        self.specialize(env, types, cases, case)
    }
}
