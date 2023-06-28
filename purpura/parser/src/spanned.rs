use location::{Spanned, Location};

pub trait Span<T> {
    fn new(value: T, location: Location) -> Spanned<T>;
}

pub trait Locatable {
    fn locate(&self) -> Location;
}

pub trait LocWith {
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
