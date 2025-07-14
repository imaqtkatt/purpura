#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub file_id: crate::files::FileId,
}

impl Location {
    pub fn merge(self, with: Self) -> Self {
        assert_eq!(self.file_id, with.file_id);

        Self {
            start: self.start.min(with.start),
            end: self.end.max(with.end),
            file_id: self.file_id,
        }
    }
}

#[derive(Debug)]
pub struct Located<T: Sized> {
    pub location: Location,
    pub inner: T,
}

impl<T: Sized> Located<T> {
    pub fn new(inner: T, location: Location) -> Self {
        Self { location, inner }
    }
}
