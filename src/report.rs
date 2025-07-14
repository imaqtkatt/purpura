//! Defines diagnostics for compile-time errors.

use std::sync::mpsc;

#[derive(Clone, Copy, Debug)]
pub enum Severity {
    Info,
    Warning,
    Error,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Info => write!(f, "INFO"),
            Severity::Warning => write!(f, "WARNING"),
            Severity::Error => write!(f, "ERROR"),
        }
    }
}

impl From<Severity> for ariadne::ReportKind<'_> {
    fn from(value: Severity) -> Self {
        match value {
            Severity::Info => Self::Advice,
            Severity::Warning => Self::Warning,
            Severity::Error => Self::Error,
        }
    }
}

pub trait Diag {
    fn severity(&self) -> Severity;
    fn message(&self) -> String;
    fn markers(&self) -> Vec<Marker>;
    fn hint(&self) -> Option<String>;
    fn location(&self) -> crate::location::Location;
}

pub struct Marker {
    message: String,
    location: crate::location::Location,
}

impl Marker {
    pub fn new(message: String, location: crate::location::Location) -> Self {
        Self { message, location }
    }
}

#[derive(Clone, Debug)]
pub struct Reporter(mpsc::Sender<Box<dyn Diag>>);

impl Reporter {
    pub fn new() -> (Self, mpsc::Receiver<Box<dyn Diag>>) {
        let (sender, receiver) = mpsc::channel();
        (Self(sender), receiver)
    }

    pub fn report(&self, diag: impl Diag + 'static) {
        self.0.send(Box::new(diag)).unwrap();
    }

    pub fn to_stdout(receiver: mpsc::Receiver<Box<dyn Diag>>, files: &crate::Files) {
        for diag in receiver.try_iter() {
            let severity = diag.severity();
            let message = diag.message();
            let markers = diag.markers();
            let hint = diag.hint();
            let location = diag.location();

            let file_path = files.get_path(location.file_id);
            let file_path_str = file_path.to_str().unwrap_or("?");

            let source = files.get_source(location.file_id);

            let mut builder = ariadne::Report::build(
                severity.into(),
                (file_path_str, (location.start..location.end)),
            )
            .with_message(message)
            .with_labels(markers.into_iter().map(|marker| {
                ariadne::Label::new((file_path_str, (marker.location.start..marker.location.end)))
                    .with_message(marker.message)
            }));

            if let Some(hint) = hint {
                builder = builder.with_help(hint);
            }

            builder
                .finish()
                .print((file_path_str, ariadne::Source::from(source.get().as_ref())))
                .unwrap();
        }
    }
}

pub fn generic_report(
    severity: Severity,
    message: String,
    location: crate::location::Location,
) -> impl Diag + 'static {
    (severity, message, location)
}

impl Diag for (Severity, String, crate::location::Location) {
    fn severity(&self) -> Severity {
        self.0
    }

    fn message(&self) -> String {
        String::new()
    }

    fn markers(&self) -> Vec<Marker> {
        vec![Marker::new(self.1.to_owned(), self.2)]
    }

    fn hint(&self) -> Option<String> {
        None
    }

    fn location(&self) -> crate::location::Location {
        self.2
    }
}
