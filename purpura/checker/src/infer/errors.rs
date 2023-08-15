//! Error types for inference or definition.

use report::ToDiagnostic;

/// A generic structure containing an inference error message.
pub struct InferError(pub String);

/// A generic structure containing an define error message.
pub struct DefineError(pub String, pub location::Location);

impl ToDiagnostic for InferError {
    fn message(&self) -> String {
        self.0.clone()
    }

    fn markers(&self) -> Vec<report::Marker> {
        vec![]
    }

    fn severity(&self) -> report::Severity {
        report::Severity::Error
    }

    fn location(&self) -> location::Location {
        location::Location::ghost()
    }
}

impl ToDiagnostic for DefineError {
    fn message(&self) -> String {
        self.0.clone()
    }

    fn markers(&self) -> Vec<report::Marker> {
        vec![]
    }

    fn severity(&self) -> report::Severity {
        report::Severity::Error
    }

    fn location(&self) -> location::Location {
        self.1.clone()
    }
}
