//! A simple file system for caching, a [FileId] is a representation of a file that exists.

use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

#[derive(Clone, Debug)]
pub struct Source(pub std::sync::Arc<String>);

impl Source {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn get(&self) -> std::sync::Arc<String> {
        self.0.clone()
    }
}

#[derive(Debug, Default)]
pub struct Files {
    paths: BTreeMap<FileId, std::path::PathBuf>,
    inner: BTreeMap<std::path::PathBuf, FileId>,
    sources: Vec<Source>,
}

impl Files {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, file: &std::path::PathBuf) -> FileId {
        let id = FileId(self.sources.len() as u32);

        let source = std::fs::read_to_string(file).expect("file exists");
        let source = Source(std::sync::Arc::new(source));

        self.paths.insert(id, file.clone());
        self.inner.insert(file.clone(), id);
        self.sources.push(source);

        id
    }

    pub fn get_source(&self, FileId(id): FileId) -> &Source {
        &self.sources[id as usize]
    }

    pub fn get_path(&self, file_id: FileId) -> std::path::PathBuf {
        self.paths[&file_id].clone()
    }
}
