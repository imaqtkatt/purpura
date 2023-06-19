//! This module exposes some structures that are useful to locate tokens inside source code and
//! other data structures.

/// The byte position
pub struct Byte(pub usize);

/// A range between two bytes
pub struct Location {
    pub start: Byte,
    pub end: Byte,
}

/// A value with a location
pub struct Spanned<T> {
    pub value: T,
    pub location: Location,
}
