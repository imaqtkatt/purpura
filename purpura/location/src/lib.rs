//! This module exposes some structures that are useful to locate tokens inside source code and
//! other data structures.

/// The byte position
#[derive(Debug, Clone, Copy)]
pub struct Byte(pub usize);

/// A range between two bytes
#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub start: Byte,
    pub end: Byte,
}

impl Location {
    pub fn ghost() -> Self {
        Self {
            start: Byte(0),
            end: Byte(0),
        }
    }
}

/// A value with a location
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub location: Location,
}

impl<T> Spanned<T> {
    pub fn map<U>(self, m: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: m(self.value),
            location: self.location,
        }
    }
}

impl Location {
    pub fn mix(self, other: Location) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}
