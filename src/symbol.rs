// This is a simple string interner taken from here: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sym(u32);

impl Sym {
    pub fn new(s: &str) -> Self {
        INTERNER.write().unwrap().intern(s)
    }

    pub fn get(&self) -> &str {
        INTERNER.read().unwrap().lookup(self)
    }
}

impl std::fmt::Debug for Sym {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Sym({:?})", self.get())
    }
}

static INTERNER: std::sync::LazyLock<std::sync::RwLock<Interner>> =
    std::sync::LazyLock::new(|| std::sync::RwLock::new(Interner::default()));

#[derive(Default)]
pub struct Interner {
    map: std::collections::HashMap<&'static str, u32>,
    str: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    fn intern(&mut self, name: &str) -> Sym {
        if let Some(&idx) = self.map.get(name) {
            return Sym(idx);
        }

        let name = unsafe { self.alloc(name) };
        let idx = self.str.len() as u32;
        self.map.insert(name, idx);
        self.str.push(name);

        Sym(idx)
    }

    fn lookup(&self, Sym(id): &Sym) -> &'static str {
        self.str[*id as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = std::mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        unsafe { &*(interned as *const str) }
    }
}
