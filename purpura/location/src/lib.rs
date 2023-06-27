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

/// A value with a location
#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub location: Location,
}

impl Location {
    pub fn mix(self, other: Location) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}
