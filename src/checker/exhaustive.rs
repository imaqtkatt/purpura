use std::collections::VecDeque;

use crate::{checker, tree::elaborated};

#[derive(Clone, Debug)]
pub enum Case {
    Wildcard,
    Number(i32),
    String(String),
    Constructor(crate::symbol::Sym, Vec<Case>),
}

impl std::fmt::Display for Case {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Case::Wildcard => write!(f, "_"),
            Case::Number(n) => write!(f, "{n}"),
            Case::String(s) => write!(f, "{s:?}"),
            Case::Constructor(name, cases) => {
                write!(f, "{}", name.get())?;
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
            symbol.inner,
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
    pub index: usize,
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

    fn prepend(&self, item: T) -> Self {
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
            (Case::Constructor(a, b), Case::Constructor(c, _)) if *a == c => {
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

    fn constructor(&self) -> Option<crate::symbol::Sym> {
        if let Case::Constructor(name, _) = self.first() {
            Some(*name)
        } else {
            None
        }
    }

    fn number(&self) -> Option<i32> {
        if let Case::Number(n) = self.first() {
            Some(*n)
        } else {
            None
        }
    }

    fn string(&self) -> Option<String> {
        if let Case::String(s) = self.first() {
            Some(s.clone())
        } else {
            None
        }
    }

    fn expand(self, name: crate::symbol::Sym, size: usize) -> Self {
        let (left, right) = self.split(size);
        right.prepend(Case::Constructor(name, left.inner.into_iter().collect()))
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

    fn constructors(&self) -> indexmap::IndexSet<crate::symbol::Sym> {
        self.rows.iter().flat_map(|row| row.constructor()).collect()
    }

    fn numbers(&self) -> indexmap::IndexSet<i32> {
        self.rows.iter().flat_map(|row| row.number()).collect()
    }

    fn strings(&self) -> indexmap::IndexSet<String> {
        self.rows.iter().flat_map(|row| row.string()).collect()
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

struct Witness;

impl Witness {
    fn prepend(slf: ProblemResult, case: Case) -> ProblemResult {
        slf.map_err(|row| row.prepend(case))
    }

    fn expand(slf: ProblemResult, name: crate::symbol::Sym, size: usize) -> ProblemResult {
        slf.map_err(|row| row.expand(name, size))
    }
}

#[derive(Debug)]
pub enum Finitude<T> {
    Infinite,
    Finite(T),
}

#[derive(Debug)]
pub enum Completeness {
    Complete(Finitude<indexmap::IndexSet<crate::symbol::Sym>>),
    Incomplete(
        Finitude<indexmap::IndexSet<crate::symbol::Sym>>,
        indexmap::IndexSet<crate::symbol::Sym>,
    ),
}

#[derive(Clone)]
struct Problem {
    types: Row<checker::Type>,
    case: Row<Case>,
    matrix: Matrix<Case>,
    occurrences: Row<Occurrence>,
}

#[derive(Clone, Debug)]
pub struct Occurrence(String, Vec<usize>);

impl Occurrence {
    pub fn with(self, next: usize) -> Self {
        let mut indexes = self.1;
        indexes.push(next);
        Self(self.0, indexes)
    }
}

pub fn is_exhaustive(
    env: &checker::TypeEnv,
    patterns: Vec<elaborated::Pattern>,
    types: Vec<checker::Type>,
) -> ProblemResult {
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
        occurrences: Row {
            index: 0,
            inner: VecDeque::from([Occurrence("scrut".to_string(), vec![])]),
        },
    }
    .exhaustive(env)
}

type ProblemResult = std::result::Result<elaborated::CaseTree, Row<Case>>;

impl Problem {
    fn specialize(
        self,
        env: &checker::TypeEnv,
        types: Vec<checker::Type>,
        cases: Vec<Case>,
        case: Case,
        occurrences: Vec<Occurrence>,
    ) -> ProblemResult {
        Self {
            types: self.types.inline(types.into()),
            case: self.case.inline(cases.into()),
            matrix: self.matrix.specialize(case),
            occurrences: self.occurrences.inline(occurrences.into()),
        }
        .exhaustive(env)
    }

    fn default(self) -> Self {
        Self {
            types: self.types.pop_front(),
            case: self.case.pop_front(),
            matrix: self.matrix.default(),
            occurrences: self.occurrences.pop_front(),
        }
    }

    fn exhaustive(self, env: &checker::TypeEnv) -> ProblemResult {
        if self.matrix.is_empty() {
            Err(self.case)
        } else if let Some(row) = self.matrix.is_irrefutable() {
            Ok(elaborated::CaseTree::Leaf(row.index))
        } else {
            self.match_exhaustiveness(env)
        }
    }

    fn completeness(&self, env: &checker::TypeEnv, name: crate::symbol::Sym) -> Completeness {
        if let Some(data) = env.datatypes.get(&name) {
            let constructors: indexmap::IndexSet<crate::symbol::Sym> =
                data.constructors.iter().map(|c| c.name.inner).collect();
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

    fn specialize_wildcard(self, env: &checker::TypeEnv) -> ProblemResult {
        Witness::prepend(self.default().exhaustive(env), Case::Wildcard)
    }

    fn match_exhaustiveness(self, env: &checker::TypeEnv) -> ProblemResult {
        let case = self.case.first();
        let current = self.types.first().clone().force();

        match (case, &*current) {
            (Case::Wildcard, t) if t == &*env.number_type => {
                Witness::prepend(self.split_numbers(env), Case::Wildcard)
            }
            (Case::Wildcard, t) if t == &*env.string_type => {
                Witness::prepend(self.split_strings(env), Case::Wildcard)
            }
            // TODO: make Number and String as internal types instead of generics?
            (Case::Wildcard, checker::TypeKind::Generic(name, types))
                if env.datatypes.contains_key(name) =>
            {
                self.ehxaustiveness_wildcard(env, *name, types.clone())
            }
            (Case::Wildcard, _) => self.specialize_wildcard(env),

            (Case::Constructor(a, b), checker::TypeKind::Generic(_, types)) => {
                let a = *a;
                let b = b.clone();
                let last_occurrence = self.occurrences.first();
                let occurrences = (0..b.len())
                    .map(|i| last_occurrence.clone().with(i))
                    .collect();
                self.specialize_constructor(env, a, b, types.clone(), occurrences)
            }

            (a, b) => panic!("reached {a:?} : {b}"),
        }
    }

    fn synthetize(&self, env: &checker::TypeEnv, name: crate::symbol::Sym) -> Case {
        let (_, arity) = env.constructors.get(&name).unwrap();
        Case::Constructor(name, vec![Case::Wildcard; *arity])
    }

    fn ehxaustiveness_wildcard(
        self,
        env: &checker::TypeEnv,
        name: crate::symbol::Sym,
        types: Vec<checker::Type>,
    ) -> ProblemResult {
        if self.matrix.is_wildcard() {
            self.specialize_wildcard(env)
        } else {
            match self.completeness(env, name) {
                Completeness::Complete(Finitude::Finite(names)) => {
                    self.split(env, names, types, true)
                }
                Completeness::Complete(Finitude::Infinite) => unreachable!(),
                Completeness::Incomplete(Finitude::Finite(names), missing) => {
                    let first = missing.into_iter().collect::<Vec<_>>()[0];
                    let case = self.synthetize(env, first);
                    Witness::prepend(self.clone().split(env, names, types, false), case)
                }
                Completeness::Incomplete(Finitude::Infinite, _) => {
                    Witness::prepend(self.specialize_wildcard(env), Case::Wildcard)
                }
            }
        }
    }

    // Produce a specialized matrix for each collected name. If the signature is not complete,
    // then compute a default matrix as a default case.
    fn split(
        self,
        env: &checker::TypeEnv,
        names: indexmap::IndexSet<crate::symbol::Sym>,
        types: Vec<checker::Type>,
        is_complete: bool,
    ) -> ProblemResult {
        let scrutinee = self.occurrences.first().clone();
        let mut cases = vec![];

        for name in names.into_iter() {
            let (_, arity) = env.constructors.get(&name).unwrap();
            let wildcards = vec![Case::Wildcard; *arity];
            let occurrences = (0..*arity)
                .map(|i| scrutinee.clone().with(i))
                .collect::<Vec<_>>();

            let witness = self.clone().specialize_constructor(
                env,
                name,
                wildcards,
                types.clone(),
                occurrences,
            );

            let case_tree = Witness::expand(witness, name.clone(), *arity)?;
            cases.push((elaborated::Case::Constructor(name, *arity), case_tree));
        }

        let default = if is_complete {
            None
        } else {
            let case_tree = self.default().exhaustive(env)?;
            Some(Box::new(case_tree))
        };

        Ok(elaborated::CaseTree::Switch(scrutinee, cases, default))
    }

    fn specialize_constructor(
        self,
        env: &checker::TypeEnv,
        name: crate::symbol::Sym,
        cases: Vec<Case>,
        types: Vec<checker::Type>,
        occurrences: Vec<Occurrence>,
    ) -> ProblemResult {
        let case = Case::Constructor(name, cases.clone());
        self.specialize(env, types, cases, case, occurrences)
    }

    fn split_numbers(self, env: &checker::TypeEnv) -> ProblemResult {
        let scrutinee = self.occurrences.first().clone();
        let numbers = self.matrix.numbers();

        let mut cases = vec![];

        for n in numbers.into_iter() {
            let case = Case::Number(n);
            let occurrences: Vec<Occurrence> = self.occurrences.inner.clone().into();
            let witness = self
                .clone()
                .specialize(env, vec![], vec![], case, occurrences);

            let case_tree = witness?;
            cases.push((elaborated::Case::Number(n), case_tree));
        }

        let default = Some(Box::new(self.default().exhaustive(env)?));

        Ok(elaborated::CaseTree::Switch(scrutinee, cases, default))
    }

    fn split_strings(self, env: &checker::TypeEnv) -> ProblemResult {
        let scrutinee = self.occurrences.first().clone();
        let strings = self.matrix.strings();

        let mut cases = vec![];

        for s in strings.into_iter() {
            let case = Case::String(s.clone());
            let occurrences: Vec<Occurrence> = self.occurrences.inner.clone().into();
            let witness = self
                .clone()
                .specialize(env, vec![], vec![], case, occurrences);

            let case_tree = witness?;
            cases.push((elaborated::Case::String(s), case_tree));
        }

        let default = Some(Box::new(self.default().exhaustive(env)?));

        Ok(elaborated::CaseTree::Switch(scrutinee, cases, default))
    }
}
