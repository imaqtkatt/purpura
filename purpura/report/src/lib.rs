//! Module for diagnosing compile-time errors.

use std::sync::mpsc;

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
    fn code(&self) -> Option<usize> {
        None
    }

    fn hint(&self) -> Option<String> {
        None
    }

    fn message(&self) -> String;

    fn markers(&self) -> Vec<Marker>;

    fn severity(&self) -> Severity;

    fn location(&self) -> Location;
}

#[derive(Clone)]
pub struct Reporter(mpsc::Sender<Box<dyn ToDiagnostic>>);

impl Reporter {
    pub fn new() -> (Self, mpsc::Receiver<Box<dyn ToDiagnostic>>) {
        let (sender, receiver) = mpsc::channel();

        (Self(sender), receiver)
    }

    pub fn report(&self, diag: impl ToDiagnostic + 'static) {
        self.0.send(Box::new(diag)).unwrap();
    }

    pub fn to_stdout(receiver: mpsc::Receiver<Box<dyn ToDiagnostic>>) {
        for diag in receiver.try_iter() {
            println!("{}", diag.message());
        }
    }
}
