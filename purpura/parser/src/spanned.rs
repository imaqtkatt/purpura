use location::{Spanned, Location};

/// Provides a function to create a Spanned structure.
pub trait Span<T> {

    /// Creates a new [Spanned] value with a [Location].
    fn new(value: T, location: Location) -> Spanned<T>;
}

/// Represents a structure that can be located.
pub trait Locatable {

    /// Returns the [Location] of a structure.
    fn locate(&self) -> Location;
}

/// Provides a method to combine two locatable structures.
pub trait LocWith {

    /// Combines the current structure with another locatable structure.
    fn with(self, other: &dyn Locatable) -> Location;
}

impl<T> Span<T> for Spanned<T> {
    fn new(value: T, location: Location) -> Spanned<T> {
        Spanned { value, location }
    }
}

impl<T> Locatable for Spanned<T> {
    fn locate(&self) -> Location {
        self.location
    }
}

impl Locatable for Location {
    fn locate(&self) -> Location {
        self.clone()
    }
}

impl<T> LocWith for Spanned<T> {
    fn with(self, other: &dyn Locatable) -> Location {
        Location {
            start: self.location.start,
            end: other.locate().end,
        }
    }
}

impl LocWith for Location {
    fn with(self, other: &dyn Locatable) -> Location {
        Location {
            start: self.start,
            end: other.locate().end,
        }
    }
}
