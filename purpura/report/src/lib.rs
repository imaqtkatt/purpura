//! Module for diagnosing compile-time errors.

use location::Location;

/// The severity of an error.
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// Implements the conversion of [Severity] into [ariadne::ReportKind].
impl Into<ariadne::ReportKind<'_>> for Severity {
    fn into(self) -> ariadne::ReportKind<'static> {
        match self {
            Severity::Error => ariadne::ReportKind::Error,
            Severity::Warning => ariadne::ReportKind::Warning,
            Severity::Info => ariadne::ReportKind::Advice,
        }
    }
}

/// Holds an error message with a location.
pub struct Marker {
    pub message: String,
    pub location: Location,
}

/// Defines an interface for errors during compilation process.
pub trait ToDiagnostic {
    fn code(&self) -> Option<usize> { None }

    fn hint(&self) -> Option<String> { None }

    fn message(&self) -> String;

    fn markers(&self) -> Vec<Marker>;

    fn severity(&self) -> Severity;

    fn location(&self) -> Location;
}
