// Unless explicitly stated otherwise all files in this repository are licensed under the MIT License.
// This product includes software developed at Datadog (https://www.datadoghq.com/) Copyright 2026-Present Datadog, Inc.

//! Generator of Elixir modules and calls. Valid by construction: variables are
//! only referenced in scope, guards use guard-safe calls only, local calls only
//! go to functions defined earlier (no recursion, so every call terminates).
//! Small value domains so patterns, keys and literals collide.

use crate::ast::{lit, n, K, N, S};
use ephyra::Source;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum H {
    Any,
    Map,
    Tuple,
    List,
    Int,
    Atom,
    Bin,
}

pub const ATOMS: &[&str] = &[":a", ":b", ":ok", ":error", "nil", "true", "false"];
pub const INTS: &[&str] = &["0", "1", "-1", "2"];
pub const FLOATS: &[&str] = &["1.0", "0.5"];
pub const BINS: &[&str] = &["\"k\"", "\"\""];
/// Map keys: mostly atoms (fields), some domain keys (integers, binaries).
pub const KEYS: &[(u32, &str)] = &[(5, ":a"), (4, ":b"), (2, ":c"), (2, "1"), (1, "\"k\""), (1, "2.0")];

/// Remote captures of typed stdlib functions (Apply.signature table or Kernel), arity <= 2 so
/// the worker can apply the function values (oracle 5).
pub const CAPTURES: &[(&str, usize)] = &[
    ("Map.get", 2), ("Map.fetch", 2), ("Map.fetch!", 2), ("Map.keys", 1), ("Map.values", 1), ("Map.to_list", 1),
    ("Map.delete", 2), ("Map.has_key?", 2), ("Map.merge", 2), ("Tuple.to_list", 1), ("Tuple.delete_at", 2),
    ("elem", 2), ("hd", 1), ("tl", 1), ("length", 1), ("is_map", 1), ("is_atom", 1), ("map_size", 1),
    ("tuple_size", 1), ("List.wrap", 1), ("List.first", 1), ("Enum.reverse", 1), ("max", 2), ("min", 2),
    ("+", 2), ("==", 2), ("++", 2), (":erlang.element", 2), (":erlang.map_get", 2), (":maps.get", 2),
    (":maps.find", 2), (":erlang.tuple_to_list", 1), ("List.flatten", 1), ("Atom.to_string", 1), ("abs", 1),
    ("not", 1), ("Map.pop", 2), ("Map.take", 2), ("Map.drop", 2), ("Enum.at", 2), ("List.last", 1),
    ("Map.pop!", 2), (":maps.remove", 2), (":maps.is_key", 2), (":maps.take", 2), (":maps.values", 1),
    (":maps.keys", 1), ("Tuple.duplicate", 2),
];

#[derive(Clone, Debug)]
pub struct Call {
    pub f: String,
    pub args: Vec<N>,
}

#[derive(Clone, Debug)]
pub struct Case {
    pub module: N,
    pub calls: Vec<Call>,
    /// extra arguments for applying returned function values (oracle 5), from fn clause patterns
    pub funargs: Vec<N>,
    /// oracle 7 variant: 0 none, 1 wrap expressions in CaseDyn.id/1, 2 compile with infer_signatures off;
    /// (mode, permille of wrapped nodes, seed)
    pub gvar: (u8, u32, u64),
}

impl Case {
    pub fn size(&self) -> usize {
        self.module.size() + self.calls.iter().map(|c| c.args.iter().map(N::size).sum::<usize>() + 1).sum::<usize>()
    }

    pub fn to_json(&self) -> serde_json::Value {
        let calls: Vec<serde_json::Value> = self
            .calls
            .iter()
            .map(|c| {
                let args: Vec<String> = c.args.iter().map(N::render).collect();
                serde_json::json!({"f": c.f, "args": format!("[{}]", args.join(", "))})
            })
            .collect();
        let (src, probes, lines, posts) = self.module.render_probed();
        let probes: Vec<serde_json::Value> =
            probes.into_iter().map(|(id, line, kind)| serde_json::json!([id, line, kind])).collect();
        let lines: Vec<serde_json::Value> = lines.into_iter().map(|(l, id)| serde_json::json!([l, id])).collect();
        let posts: Vec<serde_json::Value> = posts.into_iter().map(|(l, id)| serde_json::json!([l, id])).collect();
        let funargs: Vec<String> = self.funargs.iter().map(N::render).collect();
        let funargs = format!("[{}]", funargs.join(", "));
        let mut j = serde_json::json!({"src": src, "calls": calls, "probes": probes, "lines": lines, "posts": posts, "funargs": funargs});
        // oracle 9 (H.Expr) draws its trials from this seed only, so they survive shrinking
        j["eseed"] = self.gvar.2.into();
        match self.gvar {
            (1, permille, seed) => {
                j["gvar"] = "wrap".into();
                j["gsrc"] = self.module.render_wrapped(seed, permille).into();
            }
            (2, _, _) => j["gvar"] = "noinfer".into(),
            _ => {}
        }
        j
    }
}

struct Fun {
    name: String,
    arity: usize,
    private: bool,
}

pub struct G<'a> {
    s: &'a mut Source,
    vars: Vec<(String, H)>,
    funs: Vec<Fun>,
    next: usize,
    funargs: Vec<N>,
}

fn e(k: K, kids: Vec<N>) -> N {
    n(k, S::E, kids)
}
fn p(k: K, kids: Vec<N>) -> N {
    n(k, S::P, kids)
}
fn x(k: K, kids: Vec<N>) -> N {
    n(k, S::X, kids)
}
fn gd(k: K, kids: Vec<N>) -> N {
    n(k, S::G, kids)
}
/// fn -> body end
fn fn0(body: N) -> N {
    let cl = x(K::Clause, vec![x(K::Params, vec![]), x(K::Guards, vec![]), body]);
    e(K::Fn, vec![cl])
}

fn call(f: &str, kids: Vec<N>) -> N {
    e(K::Call(f.into()), kids)
}

impl<'a> G<'a> {
    pub fn new(s: &'a mut Source) -> Self {
        G { s, vars: vec![], funs: vec![], next: 0, funargs: vec![] }
    }

    fn pick<'b>(&mut self, xs: &'b [&'b str]) -> &'b str {
        xs[self.s.index(xs.len())]
    }
    fn chance(&mut self, p: f64) -> bool {
        self.s.bool_with(p)
    }
    fn key(&mut self) -> String {
        self.s.weighted(KEYS).to_string()
    }
    fn fresh(&mut self) -> String {
        // few names: rebinding and repeated variables happen
        let v = format!("v{}", self.next % 12);
        self.next += 1;
        v
    }
    fn var_with(&mut self, h: H) -> Option<String> {
        let c: Vec<String> = self.vars.iter().filter(|(_, vh)| *vh == h).map(|(v, _)| v.clone()).collect();
        if c.is_empty() {
            None
        } else {
            Some(c[self.s.index(c.len())].clone())
        }
    }
    fn any_var(&mut self) -> Option<String> {
        if self.vars.is_empty() {
            None
        } else {
            let i = self.s.index(self.vars.len());
            Some(self.vars[i].0.clone())
        }
    }
    fn bind(&mut self, v: &str, h: H) {
        self.vars.retain(|(w, _)| w != v);
        self.vars.push((v.to_string(), h));
    }

    // ---------- values (call arguments, literals) ----------

    pub fn value(&mut self, depth: usize) -> N {
        let w = if depth == 0 { 0 } else { 3 };
        match *self.s.weighted(&[(4, 0), (2, 1), (1, 2), (1, 3), (w, 4), (w, 5), (w, 6), (w / 2, 7)]) {
            0 => lit(self.pick(ATOMS)),
            1 => lit(self.pick(INTS)),
            2 => lit(self.pick(FLOATS)),
            3 => lit(self.pick(BINS)),
            4 => {
                let k = self.s.int(0..=3usize);
                e(K::Tuple, (0..k).map(|_| self.value(depth - 1)).collect())
            }
            5 => {
                let k = self.s.int(0..=3usize);
                let mut keys: Vec<String> = (0..k).map(|_| self.key()).collect();
                keys.sort();
                keys.dedup();
                if self.chance(0.15) {
                    let kids = if self.chance(0.5) { vec![e(K::Pair(":a".into()), vec![self.value(depth - 1)])] } else { vec![] };
                    return e(K::Struct(self.pick(&["CaseS", "CaseT"]).into()), kids);
                }
                e(K::Map, keys.into_iter().map(|k| e(K::Pair(k), vec![self.value(depth - 1)])).collect())
            }
            6 => {
                let k = self.s.int(0..=3usize);
                e(K::List, (0..k).map(|_| self.value(depth - 1)).collect())
            }
            _ => e(K::Cons, vec![self.value(depth - 1), self.value(0)]),
        }
    }

    /// A value matching pattern `pat` (best effort: repeated variables and
    /// guards may still reject it).
    fn instantiate(&mut self, pat: &N) -> N {
        match &pat.k {
            K::Lit(t) => lit(t),
            K::Tuple => e(K::Tuple, pat.kids.iter().map(|k| self.instantiate(k)).collect()),
            K::List => e(K::List, pat.kids.iter().map(|k| self.instantiate(k)).collect()),
            K::Cons => {
                let h = self.instantiate(&pat.kids[0]);
                let t = match pat.kids[1].k {
                    K::Var(_) | K::Wild => {
                        let k = self.s.int(0..=2usize);
                        e(K::List, (0..k).map(|_| self.value(1)).collect())
                    }
                    _ => self.instantiate(&pat.kids[1]),
                };
                e(K::Cons, vec![h, t])
            }
            K::Map => {
                let mut pairs: Vec<N> = pat
                    .kids
                    .iter()
                    .map(|pr| match &pr.k {
                        K::Pair(key) => e(K::Pair(key.clone()), vec![self.instantiate(&pr.kids[0])]),
                        _ => unreachable!(),
                    })
                    .collect();
                if self.chance(0.4) {
                    let key = self.key();
                    if !pairs.iter().any(|pr| pr.k == K::Pair(key.clone())) {
                        pairs.push(e(K::Pair(key), vec![self.value(1)]));
                    }
                }
                e(K::Map, pairs)
            }
            K::Struct(name) => {
                let name = if name == "_" { self.pick(&["CaseS", "CaseT"]).to_string() } else { name.clone() };
                let pairs = pat
                    .kids
                    .iter()
                    .filter(|pr| name == "CaseS" || pr.k == K::Pair(":a".into()))
                    .map(|pr| match &pr.k {
                        K::Pair(key) => e(K::Pair(key.clone()), vec![self.instantiate(&pr.kids[0])]),
                        _ => unreachable!(),
                    })
                    .collect();
                e(K::Struct(name), pairs)
            }
            K::Bin => lit(self.pick(&["\"k\"", "\"\"", "\"kk\"", "<<1, 2, 3>>", "<<107>>", "<<0, 0, 0, 0, 0, 0, 240, 63>>", "<<1::3>>"])),
            K::Bind => self.instantiate(&pat.kids[0]),
            _ => self.value(2),
        }
    }

    // ---------- patterns ----------

    fn pattern(&mut self, depth: usize) -> (N, H) {
        let w = if depth == 0 { 0 } else { 3 };
        match *self.s.weighted(&[(6, 0), (2, 1), (3, 2), (w, 3), (w, 4), (w, 5), (w / 2 + 1, 6), (w / 2, 7), (w / 3, 8), (w / 3, 9)]) {
            9 => {
                // bitstring pattern: fixed-size segments, then optionally a binary/bits tail
                let k = self.s.int(1..=3usize);
                let mut segs = vec![];
                for i in 0..k {
                    let last = i + 1 == k;
                    let spec = if last && self.chance(0.5) {
                        self.pick(&["binary", "bits"])
                    } else {
                        self.pick(&["integer", "size(8)", "integer-size(4)", "float", "utf8", "binary-size(1)", "signed"])
                    };
                    let kid = match self.s.int(0..=3u8) {
                        0 => p(K::Wild, vec![]),
                        1 if spec.starts_with("integer") || spec == "size(8)" || spec == "signed" => p(K::Lit(self.pick(&["0", "1", "107"]).into()), vec![]),
                        _ => {
                            let v = self.fresh();
                            let h = if spec.starts_with("binary") { H::Bin } else if spec == "float" || spec == "bits" { H::Any } else { H::Int };
                            self.bind(&v, h);
                            p(K::Var(v), vec![])
                        }
                    };
                    segs.push(p(K::Seg(spec.into()), vec![kid]));
                }
                (p(K::Bin, segs), H::Bin)
            }
            8 => {
                // struct patterns (structs CaseS{a, b} and CaseT{a} are defined by worker.exs)
                let name = self.pick(&["CaseS", "CaseS", "CaseT", "_"]);
                let fields: &[&str] = if name == "CaseS" { &[":a", ":b"] } else { &[":a"] };
                let mut kids = vec![];
                for f in fields {
                    if self.chance(0.5) {
                        kids.push(p(K::Pair((*f).into()), vec![self.pattern(depth - 1).0]));
                    }
                }
                (p(K::Struct(name.into()), kids), H::Map)
            }
            0 => {
                let v = self.fresh();
                self.bind(&v, H::Any);
                (p(K::Var(v), vec![]), H::Any)
            }
            1 => (p(K::Wild, vec![]), H::Any),
            2 => {
                let (t, h) = match self.s.int(0..=3u8) {
                    0 => (self.pick(ATOMS), H::Atom),
                    1 => (self.pick(INTS), H::Int),
                    2 => (self.pick(BINS), H::Bin),
                    _ => (self.pick(&[":ok", ":error", "nil", "0"]), H::Any),
                };
                (p(K::Lit(t.into()), vec![]), h)
            }
            3 => {
                let k = self.s.int(0..=3usize);
                (p(K::Tuple, (0..k).map(|_| self.pattern(depth - 1).0).collect()), H::Tuple)
            }
            4 => {
                let k = self.s.int(0..=2usize);
                let mut keys: Vec<String> = (0..k).map(|_| self.key()).collect();
                keys.sort();
                keys.dedup();
                let kids = keys.into_iter().map(|k| p(K::Pair(k), vec![self.pattern(depth - 1).0])).collect();
                (p(K::Map, kids), H::Map)
            }
            5 => {
                let h = self.pattern(depth - 1).0;
                let t = if self.chance(0.7) { self.pattern(0).0 } else { p(K::List, vec![]) };
                (p(K::Cons, vec![h, t]), H::List)
            }
            6 => {
                let k = self.s.int(0..=2usize);
                (p(K::List, (0..k).map(|_| self.pattern(depth.saturating_sub(1)).0).collect()), H::List)
            }
            _ => {
                let (inner, h) = self.pattern(depth - 1);
                let v = self.fresh();
                self.bind(&v, h);
                (p(K::Bind, vec![inner, p(K::Var(v), vec![])]), h)
            }
        }
    }

    // ---------- guards ----------

    fn guard_operand(&mut self) -> N {
        match self.any_var() {
            Some(v) if self.chance(0.97) => gd(K::Var(v), vec![]),
            _ => n(K::Lit(self.pick(ATOMS).into()), S::G, vec![]),
        }
    }

    fn guard_lit(&mut self) -> N {
        let t = match self.s.int(0..=2u8) {
            0 => self.pick(ATOMS),
            1 => self.pick(INTS),
            _ => self.pick(&["1.0", "\"k\"", "{}", "[]"]),
        };
        n(K::Lit(t.into()), S::G, vec![])
    }

    fn guard(&mut self, depth: usize) -> N {
        if depth > 0 && self.chance(0.3) {
            let op = self.pick(&["and", "or", "and", "or", "not"]);
            if op == "not" {
                return gd(K::Un("not".into()), vec![self.guard(depth - 1)]);
            }
            return gd(K::Op(op.into()), vec![self.guard(depth - 1), self.guard(depth - 1)]);
        }
        let v = self.guard_operand();
        let gc = |f: &str, kids: Vec<N>| gd(K::Call(f.into()), kids);
        match self.s.int(0..=13u8) {
            0..=3 => {
                let f = self.pick(&[
                    "is_integer", "is_atom", "is_map", "is_tuple", "is_list", "is_binary", "is_float", "is_number",
                    "is_boolean", "is_nil", "is_function", "is_bitstring",
                ]);
                if f == "is_function" && self.chance(0.5) {
                    let a = self.pick(&["0", "1", "2"]);
                    return gc(f, vec![v, n(K::Lit(a.into()), S::G, vec![])]);
                }
                gc(f, vec![v])
            }
            4 | 5 => {
                let op = self.pick(&["==", "!=", "===", "!==", "<", ">", "=<", ">="]);
                let op = if op == "=<" { "<=" } else { op };
                let rhs = self.guard_lit();
                gd(K::Op(op.into()), vec![v, rhs])
            }
            6 => {
                let k = self.pick(&["0", "1", "2", "3"]);
                let op = self.pick(&["==", "==", "!=", "<", ">", "<=", ">="]);
                gd(K::Op(op.into()), vec![gc("tuple_size", vec![v]), n(K::Lit(k.into()), S::G, vec![])])
            }
            7 => {
                let k = self.pick(&["0", "1", "2"]);
                let op = self.pick(&["==", ">", "<", "!=", "<=", ">="]);
                gd(K::Op(op.into()), vec![gc("map_size", vec![v]), n(K::Lit(k.into()), S::G, vec![])])
            }
            8 => {
                let key = self.key();
                gc("is_map_key", vec![v, n(K::Lit(key), S::G, vec![])])
            }
            9 => {
                let key = self.key();
                let rhs = self.guard_lit();
                let op = self.pick(&["==", "!=", "==="]);
                gd(K::Op(op.into()), vec![gc(":erlang.map_get", vec![n(K::Lit(key), S::G, vec![]), v]), rhs])
            }
            10 => {
                let i = self.pick(&["0", "1"]);
                let rhs = self.guard_lit();
                gd(K::Op("==".into()), vec![gc("elem", vec![v, n(K::Lit(i.into()), S::G, vec![])]), rhs])
            }
            11 => {
                let f = self.pick(&["hd", "length", "tl", "byte_size", "bit_size"]);
                let rhs = self.guard_lit();
                let op = self.pick(&["==", ">", "!="]);
                gd(K::Op(op.into()), vec![gc(f, vec![v]), rhs])
            }
            12 => {
                let w = self.guard_operand();
                let op = self.pick(&["==", "!=", "<", ">"]);
                gd(K::Op(op.into()), vec![v, w])
            }
            _ => {
                let l: Vec<N> = (0..self.s.int(1..=3usize)).map(|_| self.guard_lit()).collect();
                gd(K::Op("in".into()), vec![v, n(K::List, S::G, l)])
            }
        }
    }

    fn guards(&mut self, p: f64) -> N {
        if self.chance(p) {
            let g = self.guard(2);
            self.refine(&g, true);
            x(K::Guards, vec![g])
        } else {
            x(K::Guards, vec![])
        }
    }

    /// Hints from a guard that holds: `is_map(v)`, `is_map_key(v, k)`, ... on
    /// the conjunctive spine (under `or`/`not` nothing is known).
    fn refine(&mut self, g: &N, pos: bool) {
        match &g.k {
            K::Op(op) if op == "and" && pos => {
                self.refine(&g.kids[0], true);
                self.refine(&g.kids[1], true);
            }
            K::Call(f) => {
                let h = match f.as_str() {
                    "is_map" | "is_map_key" | "map_size" => H::Map,
                    "is_tuple" | "tuple_size" => H::Tuple,
                    "is_list" => H::List,
                    "is_integer" => H::Int,
                    "is_atom" | "is_boolean" => H::Atom,
                    "is_binary" => H::Bin,
                    _ => return,
                };
                if let Some(K::Var(v)) = g.kids.first().map(|k| &k.k) {
                    let v = v.clone();
                    if self.vars.iter().any(|(w, _)| *w == v) {
                        self.bind(&v, h);
                    }
                }
            }
            K::Op(_) => {
                // tuple_size(v) == n, map_size(v) > n
                if let Some(c) = g.kids.first() {
                    if matches!(&c.k, K::Call(f) if f == "tuple_size" || f == "map_size") {
                        self.refine(c, true);
                    }
                }
            }
            _ => {}
        }
    }

    /// Hints from a condition that is false: `not g`, `a or b` (both false), `!g`.
    fn refine_neg(&mut self, g: &N) {
        match &g.k {
            K::Un(op) if op == "not" || op == "!" => self.refine(&g.kids[0], true),
            K::Op(op) if op == "or" || op == "||" => {
                self.refine_neg(&g.kids[0]);
                self.refine_neg(&g.kids[1]);
            }
            _ => {}
        }
    }

    // ---------- expressions ----------

    fn leaf(&mut self, want: H) -> N {
        if want != H::Any && self.chance(0.7) {
            if let Some(v) = self.var_with(want) {
                return e(K::Var(v), vec![]);
            }
        }
        match want {
            H::Map => e(K::Map, vec![]),
            H::Tuple => e(K::Tuple, vec![]),
            H::List => e(K::List, vec![]),
            H::Int => lit(self.pick(INTS)),
            H::Atom => lit(self.pick(ATOMS)),
            H::Bin => lit(self.pick(BINS)),
            H::Any => {
                // Variables bound by bare patterns have type dynamic(): returning one
                // makes the inferred return dynamic() and the soundness check vacuous.
                // Prefer variables whose type a guard or a binding refined.
                let hinted: Vec<String> =
                    self.vars.iter().filter(|(_, h)| *h != H::Any).map(|(v, _)| v.clone()).collect();
                if !hinted.is_empty() && self.chance(0.6) {
                    return e(K::Var(hinted[self.s.index(hinted.len())].clone()), vec![]);
                }
                if let Some(v) = self.any_var() {
                    if self.chance(0.25) {
                        return e(K::Var(v), vec![]);
                    }
                }
                self.value(1)
            }
        }
    }

    /// A map-ish expression (a var hinted as map, a literal, an update, a Map call).
    fn map_expr(&mut self, depth: usize) -> N {
        if depth == 0 || self.chance(0.6) {
            if let Some(v) = self.var_with(H::Map) {
                return e(K::Var(v), vec![]);
            }
            // an unhinted variable may still hold a map at run time
            if let Some(v) = self.var_with(H::Any) {
                if self.chance(0.4) {
                    return e(K::Var(v), vec![]);
                }
            }
        }
        if depth == 0 {
            return self.leaf(H::Map);
        }
        match self.s.int(0..=4u8) {
            0 | 1 => {
                let k = self.s.int(0..=3usize);
                let mut keys: Vec<String> = (0..k).map(|_| self.key()).collect();
                keys.sort();
                keys.dedup();
                if self.chance(0.2) {
                    let name = self.pick(&["CaseS", "CaseT"]);
                    let fields: &[&str] = if name == "CaseS" { &[":a", ":b"] } else { &[":a"] };
                    let mut kids = vec![];
                    for f in fields {
                        if self.chance(0.6) {
                            kids.push(e(K::Pair((*f).into()), vec![self.expr(depth - 1, H::Any)]));
                        }
                    }
                    let s = e(K::Struct(name.into()), kids);
                    return if self.chance(0.3) { call("Map.from_struct", vec![s]) } else { s };
                }
                let mut kids: Vec<N> =
                    keys.into_iter().map(|k| e(K::Pair(k), vec![self.expr(depth - 1, H::Any)])).collect();
                if self.chance(0.1) {
                    if let Some(v) = self.any_var() {
                        kids.push(e(K::PairE, vec![e(K::Var(v), vec![]), self.expr(depth - 1, H::Any)]));
                    }
                }
                e(K::Map, kids)
            }
            2 => {
                let base = self.map_expr(depth - 1);
                let k = self.s.int(1..=2usize);
                let mut keys: Vec<String> = (0..k).map(|_| self.key()).collect();
                keys.sort();
                keys.dedup();
                let mut kids = vec![base];
                for k in keys {
                    kids.push(e(K::Pair(k), vec![self.expr(depth - 1, H::Any)]));
                }
                e(K::MapUpd, kids)
            }
            3 => {
                let m = self.map_expr(depth - 1);
                let k = lit(&self.key());
                let v = self.expr(depth - 1, H::Any);
                match self.s.int(0..=7u8) {
                    0 => call("Map.put", vec![m, k, v]),
                    1 => call("Map.replace", vec![m, k, v]),
                    2 => call("Map.put_new", vec![m, k, v]),
                    3 => call("Map.replace!", vec![m, k, v]),
                    4 => call("Map.put_new_lazy", vec![m, k, fn0(v)]),
                    5 => call(":maps.put", vec![k, v, m]),
                    6 => call(":maps.update", vec![k, v, m]),
                    _ => call(":maps.remove", vec![k, m]),
                }
            }
            _ => {
                let m = self.map_expr(depth - 1);
                match self.s.int(0..=4u8) {
                    0 => call("Map.delete", vec![m, lit(&self.key())]),
                    1 => {
                        let m2 = self.map_expr(depth - 1);
                        call("Map.merge", vec![m, m2])
                    }
                    2 => {
                        let v = self.fresh();
                        let body = self.with_var(&v, H::Any, |g| g.expr(depth - 1, H::Any));
                        let f = self.fn1(&v, body);
                        let k = lit(&self.key());
                        match self.s.int(0..=3u8) {
                            0 => call("Map.update!", vec![m, k, f]),
                            1 => call("Map.replace_lazy", vec![m, k, f]),
                            _ => {
                                let d = self.expr(0, H::Any);
                                call("Map.update", vec![m, k, d, f])
                            }
                        }
                    }
                    3 => {
                        let keys: Vec<N> = (0..self.s.int(0..=2usize)).map(|_| lit(&self.key())).collect();
                        if self.chance(0.25) {
                            let v = self.expr(0, H::Any);
                            return call(":maps.from_keys", vec![e(K::List, keys), v]);
                        }
                        call(self.pick(&["Map.take", "Map.drop"]), vec![m, e(K::List, keys)])
                    }
                    _ => {
                        let k = lit(&self.key());
                        let v = self.expr(depth - 1, H::Any);
                        call("Map.put", vec![m, k, v])
                    }
                }
            }
        }
    }

    fn with_var<T>(&mut self, v: &str, h: H, f: impl FnOnce(&mut Self) -> T) -> T {
        let saved = self.vars.clone();
        self.bind(v, h);
        let r = f(self);
        self.vars = saved;
        r
    }

    fn fn1(&self, v: &str, body: N) -> N {
        let cl = x(K::Clause, vec![x(K::Params, vec![p(K::Var(v.into()), vec![])]), x(K::Guards, vec![]), body]);
        e(K::Fn, vec![cl])
    }

    /// Arms for case/fn/else: patterns bind in the arm only. Optionally a catch-all last.
    fn arm(&mut self, depth: usize, want: H, scrut: Option<&str>) -> N {
        let saved = self.vars.clone();
        let (pat, h) = self.pattern(2);
        // the arm's body may use the case subject as what its pattern says (occurrence typing)
        if let Some(v) = scrut {
            if h != H::Any && self.vars.iter().any(|(w, _)| w == v) {
                self.bind(v, h);
            }
        }
        let g = self.guards(0.3);
        let body = self.expr(depth, want);
        self.vars = saved;
        x(K::Arm, vec![pat, g, body])
    }

    fn arms(&mut self, depth: usize, want: H) -> Vec<N> {
        self.arms_on(depth, want, None)
    }

    fn arms_on(&mut self, depth: usize, want: H, scrut: Option<&str>) -> Vec<N> {
        let k = self.s.int(1..=3usize);
        let mut arms: Vec<N> = (0..k).map(|_| self.arm(depth, want, scrut)).collect();
        if self.chance(0.7) {
            let body = self.expr(depth, want);
            arms.push(x(K::Arm, vec![p(K::Wild, vec![]), x(K::Guards, vec![]), body]));
        }
        arms
    }

    pub fn expr(&mut self, depth: usize, want: H) -> N {
        if depth == 0 || self.chance(0.15) {
            return self.leaf(want);
        }
        let d = depth - 1;
        match want {
            H::Map if self.chance(0.7) => return self.map_expr(depth),
            H::Tuple if self.chance(0.5) => {
                let k = self.s.int(0..=3usize);
                return e(K::Tuple, (0..k).map(|_| self.expr(d, H::Any)).collect());
            }
            H::List if self.chance(0.5) => {
                let k = self.s.int(0..=2usize);
                return e(K::List, (0..k).map(|_| self.expr(d, H::Any)).collect());
            }
            _ => {}
        }
        let w: &[(u32, u8)] = &[
            (3, 0),  // tuple
            (2, 1),  // list / cons
            (4, 2),  // map
            (3, 3),  // map read
            (2, 4),  // tuple ops
            (2, 5),  // list ops
            (2, 6),  // ops
            (5, 7),  // case
            (2, 8),  // if
            (2, 9),  // cond
            (3, 10), // block with match
            (2, 11), // fn call
            (2, 12), // with
            (3, 13), // local call
            (1, 14), // Enum
            (1, 15), // is_* / kernel
            (if want == H::Any { 2 } else { 0 }, 16), // function value
            (2, 17), // for
            (2, 18), // try
            (1, 19), // receive ... after 0
        ];
        match *self.s.weighted(w) {
            0 => {
                let k = self.s.int(0..=3usize);
                e(K::Tuple, (0..k).map(|_| self.expr(d, H::Any)).collect())
            }
            1 => {
                if self.chance(0.5) {
                    let k = self.s.int(0..=3usize);
                    e(K::List, (0..k).map(|_| self.expr(d, H::Any)).collect())
                } else {
                    let t = self.expr(d, H::List);
                    e(K::Cons, vec![self.expr(d, H::Any), t])
                }
            }
            2 => self.map_expr(depth),
            3 => {
                let m = self.map_expr(d);
                let k = lit(&self.key());
                match self.s.int(0..=10u8) {
                    7 => {
                        let d = self.expr(0, H::Any);
                        match self.s.int(0..=2u8) {
                            0 => call("Map.get_lazy", vec![m, k, fn0(d)]),
                            1 => call("Map.pop_lazy", vec![m, k, fn0(d)]),
                            _ => call(":maps.get", vec![k, m, d]),
                        }
                    }
                    8 => call(self.pick(&[":maps.get", ":maps.find", ":maps.is_key", ":maps.take"]), vec![k, m]),
                    9 => call("Map.pop!", vec![m, k]),
                    10 => call(self.pick(&[":maps.keys", ":maps.values", ":maps.to_list"]), vec![m]),
                    0 => call("Map.get", vec![m, k]),
                    1 => {
                        let dflt = self.expr(0, H::Any);
                        call("Map.get", vec![m, k, dflt])
                    }
                    2 => call("Map.fetch", vec![m, k]),
                    3 => call("Map.fetch!", vec![m, k]),
                    4 => call("Map.has_key?", vec![m, k]),
                    5 => call(self.pick(&["Map.keys", "Map.values", "Map.to_list", "map_size"]), vec![m]),
                    _ => {
                        if let K::Lit(t) = &k.k {
                            if t.starts_with(':') && t.len() > 1 {
                                // m.key (atom field access)
                                return call(&format!("Map.fetch!"), vec![m, k]);
                            }
                        }
                        call("Map.pop", vec![m, k])
                    }
                }
            }
            4 => {
                let t = self.expr(d, H::Tuple);
                let i = lit(self.pick(&["0", "1", "2"]));
                match self.s.int(0..=4u8) {
                    0 | 1 => call("elem", vec![t, i]),
                    2 => {
                        let v = self.expr(d, H::Any);
                        call("put_elem", vec![t, i, v])
                    }
                    3 => {
                        if self.chance(0.3) {
                            let v = self.expr(d, H::Any);
                            return call("Tuple.duplicate", vec![v, i]);
                        }
                        if self.chance(0.3) {
                            let v = self.expr(d, H::Any);
                            let j = lit(self.pick(&["1", "2", "3"]));
                            return call(":erlang.setelement", vec![j, t, v]);
                        }
                        call(self.pick(&["tuple_size", "Tuple.to_list"]), vec![t])
                    }
                    _ => {
                        let v = self.expr(d, H::Any);
                        if self.chance(0.5) {
                            call("Tuple.insert_at", vec![t, i, v])
                        } else {
                            call("Tuple.delete_at", vec![t, i])
                        }
                    }
                }
            }
            5 => {
                let l = self.expr(d, H::List);
                match self.s.int(0..=4u8) {
                    0 => call(self.pick(&["hd", "tl", "length", "List.first", "List.last"]), vec![l]),
                    1 => {
                        let r = self.expr(d, H::Any);
                        e(K::Op("++".into()), vec![l, r])
                    }
                    2 => call(self.pick(&["Enum.reverse", "List.flatten", "Enum.sort"]), vec![l]),
                    3 => {
                        let i = lit(self.pick(&["0", "1", "-1"]));
                        call("Enum.at", vec![l, i])
                    }
                    _ => call("List.wrap", vec![self.expr(d, H::Any)]),
                }
            }
            6 => {
                let op = self.pick(&["==", "!=", "===", "<", ">", "and", "or", "&&", "||", "+", "-", "*", "<>"]);
                if self.chance(0.12) {
                    let k = self.s.int(1..=3usize);
                    let mut segs = vec![];
                    for _ in 0..k {
                        let spec = self.pick(&["integer", "binary", "bits", "float", "utf8", "size(4)", "bytes"]);
                        let want = match spec { "binary" | "bytes" | "bits" => H::Bin, _ => H::Int };
                        // a literal string is only allowed in binary segments
                        let kid = if want == H::Bin && self.chance(0.5) {
                            self.expr(d.min(1), want)
                        } else {
                            match self.var_with(want).or_else(|| self.any_var()) {
                                Some(v) if self.chance(0.7) => e(K::Var(v), vec![]),
                                _ => lit(if want == H::Bin { self.pick(BINS) } else { self.pick(INTS) }),
                            }
                        };
                        segs.push(e(K::Seg(spec.into()), vec![kid]));
                    }
                    return e(K::Bin, segs);
                }
                if op == "<>" {
                    // <> is a macro: literal operands must be binaries at compile time
                    let a = self.leaf(H::Bin);
                    let b = self.leaf(H::Bin);
                    return e(K::Op(op.into()), vec![a, b]);
                }
                let a = self.expr(d, H::Any);
                let b = self.expr(d, H::Any);
                if self.chance(0.15) {
                    return call(self.pick(&["min", "max"]), vec![a, b]);
                }
                e(K::Op(op.into()), vec![a, b])
            }
            7 => {
                let (scrut, var) = match self.any_var() {
                    Some(v) if self.chance(0.7) => (e(K::Var(v.clone()), vec![]), Some(v)),
                    _ => (self.expr(d, H::Any), None),
                };
                let mut kids = vec![scrut];
                kids.extend(self.arms_on(d, want, var.as_deref()));
                e(K::Case, kids)
            }
            8 => {
                let c = if self.chance(0.5) { self.guard_as_expr() } else { self.expr(d, H::Any) };
                // occurrence typing: the branches use what the condition tells about variables
                let saved = self.vars.clone();
                self.refine(&c, true);
                let a = self.expr(d, want);
                self.vars = saved.clone();
                self.refine_neg(&c);
                let b = self.expr(d, want);
                self.vars = saved;
                e(K::If, vec![c, a, b])
            }
            9 => {
                let k = self.s.int(1..=2usize);
                let mut arms: Vec<N> = (0..k)
                    .map(|_| {
                        let c = if self.chance(0.6) { self.guard_as_expr() } else { self.expr(d, H::Any) };
                        let saved = self.vars.clone();
                        self.refine(&c, true);
                        let b = self.expr(d, want);
                        self.vars = saved;
                        x(K::CondArm, vec![c, b])
                    })
                    .collect();
                if self.chance(0.8) {
                    let b = self.expr(d, want);
                    arms.push(x(K::CondArm, vec![lit("true"), b]));
                }
                e(K::Cond, arms)
            }
            10 => {
                let saved = self.vars.clone();
                let k = self.s.int(1..=2usize);
                let mut kids = vec![];
                for _ in 0..k {
                    let rhs = self.expr(d, H::Any);
                    let (pat, _) = if self.chance(0.6) {
                        let v = self.fresh();
                        let h = guess(&rhs);
                        self.bind(&v, h);
                        (p(K::Var(v), vec![]), h)
                    } else {
                        self.pattern(1)
                    };
                    kids.push(x(K::Match, vec![pat, rhs]));
                }
                kids.push(self.expr(d, want));
                self.vars = saved;
                e(K::Block, kids)
            }
            11 if self.chance(0.3) => {
                // a capture applied: (&f/n).(args), typed through the capture's fun type
                match self.capture(None) {
                    Some(c) => {
                        let a: usize = match &c.k {
                            K::Capture(t) => t.rsplit('/').next().unwrap().parse().unwrap(),
                            _ => unreachable!(),
                        };
                        let mut kids = vec![c];
                        for _ in 0..a {
                            kids.push(self.expr(d, H::Any));
                        }
                        e(K::FnCall, kids)
                    }
                    None => self.expr(d, want),
                }
            }
            11 => {
                let arity = self.s.int(1..=2usize);
                let saved = self.vars.clone();
                let k = self.s.int(1..=2usize);
                let mut clauses = vec![];
                for _ in 0..k {
                    self.vars = saved.clone();
                    let pats: Vec<N> = (0..arity).map(|_| self.pattern(1).0).collect();
                    let g = self.guards(0.3);
                    let body = self.expr(d, want);
                    clauses.push(x(K::Clause, vec![x(K::Params, pats), g, body]));
                }
                if self.chance(0.6) {
                    self.vars = saved.clone();
                    let pats: Vec<N> = (0..arity).map(|_| p(K::Wild, vec![])).collect();
                    let body = self.expr(d, want);
                    clauses.push(x(K::Clause, vec![x(K::Params, pats), x(K::Guards, vec![]), body]));
                }
                self.vars = saved;
                let mut kids = vec![e(K::Fn, clauses)];
                for _ in 0..arity {
                    kids.push(self.expr(d, H::Any));
                }
                e(K::FnCall, kids)
            }
            12 => {
                let saved = self.vars.clone();
                let k = self.s.int(1..=2usize);
                let mut kids = vec![];
                let mut outer = saved.clone();
                for _ in 0..k {
                    let rhs = self.expr(d, H::Any);
                    // patterns of `with` bind for the following clauses and the body
                    let (pat, _) = self.pattern(2);
                    kids.push(x(K::WithArm, vec![pat, rhs]));
                }
                kids.push(self.expr(d, want));
                std::mem::swap(&mut self.vars, &mut outer);
                if self.chance(0.4) {
                    let arms = self.arms(d, want);
                    kids.push(x(K::Else, arms));
                }
                self.vars = saved;
                e(K::With, kids)
            }
            13 => {
                let callees: Vec<(String, usize)> = self.funs.iter().map(|f| (f.name.clone(), f.arity)).collect();
                if callees.is_empty() {
                    return self.expr(d, want);
                }
                let (f, a) = callees[self.s.index(callees.len())].clone();
                let args = (0..a).map(|_| self.expr(d, H::Any)).collect();
                // a public function called remotely through its own module: typed from the
                // inferred signature in the export cache (Apply.export), not the local handler
                if f.starts_with('f') && self.chance(0.3) {
                    return call(&format!("CaseMod.{f}"), args);
                }
                call(&f, args)
            }
            14 => {
                let l = self.expr(d, H::List);
                let f = match if self.chance(0.3) { self.capture(Some(1)) } else { None } {
                    Some(c) => c,
                    None => {
                        let v = self.fresh();
                        let body = self.with_var(&v, H::Any, |g| g.expr(d, H::Any));
                        self.fn1(&v, body)
                    }
                };
                call(self.pick(&["Enum.map", "Enum.filter", "Enum.flat_map"]), vec![l, f])
            }
            16 => self.fun_value(d),
            17 => {
                let saved = self.vars.clone();
                let mut kids = vec![];
                for i in 0..self.s.int(1..=2usize) {
                    let src = if self.chance(0.2) { self.map_expr(d) } else { self.expr(d, H::List) };
                    if i == 0 {
                        // gradual-set-theoretic-types.md:246: comprehensions are assumed to run at
                        // least once, so the checker refines outer variables from code that may never
                        // run (documented, intentional). Only the first generator's source always runs:
                        // later sources, filters and the body see the generators' variables only.
                        self.vars.clear();
                    }
                    let (pat, _) = self.pattern(1);
                    kids.push(x(K::WithArm, vec![pat, src]));
                }
                if self.chance(0.3) {
                    let c = if self.chance(0.5) { self.guard_as_expr() } else { self.expr(d, H::Any) };
                    kids.push(c);
                }
                let opt = *self.s.weighted(&[(3, ""), (1, "uniq: true"), (2, "into: %{}"), (2, "reduce")]);
                match opt {
                    "into: %{}" => {
                        let a = self.expr(d, H::Any);
                        let b = self.expr(d, H::Any);
                        kids.push(e(K::Tuple, vec![a, b]));
                    }
                    "reduce" => {
                        let outer = saved.clone();
                        let inner = std::mem::replace(&mut self.vars, outer);
                        let init = self.expr(d, want);
                        self.vars = inner;
                        let acc = self.fresh();
                        self.bind(&acc, H::Any);
                        let body = self.expr(d, want);
                        kids.push(init);
                        kids.push(x(K::Arm, vec![p(K::Var(acc), vec![]), x(K::Guards, vec![]), body]));
                    }
                    _ => {
                        let b = self.expr(d, H::Any);
                        kids.push(b);
                    }
                }
                self.vars = saved;
                e(K::For(opt.into()), kids)
            }
            18 => {
                let mut kids = vec![self.expr(d, want)];
                if self.chance(0.7) {
                    let arms: Vec<N> = (0..self.s.int(1..=2usize))
                        .map(|_| {
                            let pt = self.pick(&["e in [ArgumentError, KeyError, MatchError]", "_", "e in BadMapError", "_e"]);
                            let body = self.expr(d, want);
                            x(K::Arm, vec![p(K::Lit(pt.into()), vec![]), x(K::Guards, vec![]), body])
                        })
                        .collect();
                    kids.push(x(K::TryPart("rescue".into()), arms));
                }
                if kids.len() == 1 || self.chance(0.3) {
                    let pt = self.pick(&["_, _", ":error, _", ":throw, v99", ":exit, _"]);
                    let body = self.expr(d, want);
                    let arm = x(K::Arm, vec![p(K::Lit(pt.into()), vec![]), x(K::Guards, vec![]), body]);
                    kids.push(x(K::TryPart("catch".into()), vec![arm]));
                }
                if self.chance(0.3) {
                    let arms = self.arms(d, want);
                    kids.push(x(K::TryPart("else".into()), arms));
                }
                if self.chance(0.3) {
                    let a = self.expr(d, H::Any);
                    kids.push(x(K::TryPart("after".into()), vec![a]));
                }
                e(K::Try, kids)
            }
            19 => {
                let mut kids: Vec<N> = (0..self.s.int(1..=2usize)).map(|_| self.arm(d, want, None)).collect();
                let a = self.expr(d, want);
                kids.push(x(K::TryPart("after0".into()), vec![a]));
                e(K::Receive, kids)
            }
            _ => {
                let a = self.expr(d, H::Any);
                match self.s.int(0..=3u8) {
                    0 => call(self.pick(&["is_map", "is_atom", "is_tuple", "is_list", "is_integer"]), vec![a]),
                    1 => {
                        let b = self.expr(d, H::Any);
                        call(self.pick(&["max", "min"]), vec![a, b])
                    }
                    2 => e(K::Un(self.pick(&["not", "!", "-"]).into()), vec![a]),
                    _ => call(self.pick(&["is_nil", "to_string", "inspect"]), vec![a]),
                }
            }
        }
    }

    /// A capture `&f/n`: an earlier local function, the module's own public function called
    /// remotely (`&CaseMod.f/n`, typed from its inferred signature), or a stdlib function.
    /// `arity`: wanted arity (None: any).
    fn capture(&mut self, arity: Option<usize>) -> Option<N> {
        let locals: Vec<(String, usize, bool)> = self
            .funs
            .iter()
            .filter(|f| arity.map_or(true, |a| a == f.arity))
            .map(|f| (f.name.clone(), f.arity, f.private))
            .collect();
        if !locals.is_empty() && self.chance(0.5) {
            let (f, a, private) = locals[self.s.index(locals.len())].clone();
            let name = if !private && self.chance(0.5) { format!("CaseMod.{f}") } else { f };
            return Some(e(K::Capture(format!("{name}/{a}")), vec![]));
        }
        let std: Vec<&(&str, usize)> = CAPTURES.iter().filter(|(_, a)| arity.map_or(true, |w| w == *a)).collect();
        if std.is_empty() {
            return None;
        }
        let (f, a) = std[self.s.index(std.len())];
        Some(e(K::Capture(format!("{f}/{a}")), vec![]))
    }

    /// A function value: an `fn` literal (closing over variables in scope) or a capture.
    fn fun_value(&mut self, depth: usize) -> N {
        if self.chance(0.5) {
            if let Some(c) = self.capture(None) {
                return c;
            }
        }
        let arity = *self.s.weighted(&[(6, 1usize), (3, 2), (1, 0)]);
        let saved = self.vars.clone();
        let k = self.s.int(1..=2usize);
        let mut clauses = vec![];
        for _ in 0..k {
            self.vars = saved.clone();
            let pats: Vec<N> = (0..arity).map(|_| self.pattern(1).0).collect();
            if self.funargs.len() < 8 {
                for pt in &pats {
                    let v = self.instantiate(pt);
                    self.funargs.push(v);
                }
            }
            let g = self.guards(if arity == 0 { 0.0 } else { 0.3 });
            let body = self.expr(depth, H::Any);
            clauses.push(x(K::Clause, vec![x(K::Params, pats), g, body]));
            if arity == 0 {
                break; // `fn -> ...` has one clause
            }
        }
        if arity > 0 && self.chance(0.5) {
            self.vars = saved.clone();
            let pats: Vec<N> = (0..arity).map(|_| p(K::Wild, vec![])).collect();
            let body = self.expr(depth, H::Any);
            clauses.push(x(K::Clause, vec![x(K::Params, pats), x(K::Guards, vec![]), body]));
        }
        self.vars = saved;
        e(K::Fn, clauses)
    }

    /// A guard rendered in expression position (if/cond conditions).
    fn guard_as_expr(&mut self) -> N {
        retag(self.guard(1), S::E)
    }

    // ---------- module ----------

    pub fn case(&mut self) -> Case {
        let nf = self.s.int(1..=3usize);
        let mut funs = vec![];
        for i in 0..nf {
            let arity = *self.s.weighted(&[(5, 1usize), (3, 2), (1, 0)]);
            let private = i + 1 < nf && self.chance(0.25);
            let name = format!("{}{}", if private { "h" } else { "f" }, i);
            let nc = *self.s.weighted(&[(3, 1usize), (3, 2), (2, 3)]);
            let mut clauses = vec![];
            for _ in 0..nc {
                self.vars.clear();
                let pats: Vec<N> = (0..arity).map(|_| self.pattern(2).0).collect();
                let g = self.guards(0.5);
                let depth = self.s.int(1..=4usize);
                let mut body = self.expr(depth, H::Any);
                if matches!(body.k, K::Var(_)) && self.chance(0.8) {
                    body = self.expr(depth + 1, H::Any);
                }
                if self.chance(0.15) {
                    let fv = self.fun_value(depth.min(3));
                    body = if self.chance(0.5) { fv } else { e(K::Tuple, vec![fv, body]) };
                }
                clauses.push(x(K::Clause, vec![x(K::Params, pats), g, body]));
            }
            funs.push(x(K::Fun(name.clone(), private), clauses));
            self.funs.push(Fun { name, arity, private });
        }
        let module = x(K::Module, funs);
        // calls: public functions, arguments mostly derived from clause patterns
        let mut calls = vec![];
        for (fi, f) in module.kids.iter().enumerate() {
            let meta = &self.funs[fi];
            if meta.private {
                continue;
            }
            let name = meta.name.clone();
            let arity = meta.arity;
            let k = if arity == 0 { 1 } else { self.s.int(3..=6usize) };
            for _ in 0..k {
                let args: Vec<N> = if self.chance(0.75) {
                    let c = &f.kids[self.s.index(f.kids.len())];
                    let pats = c.kids[0].kids.clone();
                    pats.iter().map(|pt| self.instantiate(pt)).collect()
                } else {
                    (0..arity).map(|_| self.value(2)).collect()
                };
                calls.push(Call { f: name.clone(), args });
            }
        }
        // drawn last, so the cases above are what they were before oracle 7
        let mode = *self.s.weighted(&[(5, 1u8), (2, 2), (3, 0)]);
        let permille = *self.s.weighted(&[(2, 150u32), (2, 400), (1, 1000)]);
        let seed = self.s.int(0..=u32::MAX as usize) as u64;
        Case { module, calls, funargs: std::mem::take(&mut self.funargs), gvar: (mode, permille, seed) }
    }
}

/// The hint for a variable bound to `rhs`.
fn guess(rhs: &N) -> H {
    match &rhs.k {
        K::Map | K::MapUpd | K::Struct(_) => H::Map,
        K::Call(f) if f.starts_with("Map.") && !matches!(f.as_str(), "Map.get" | "Map.fetch" | "Map.fetch!" | "Map.keys" | "Map.values" | "Map.to_list" | "Map.has_key?" | "Map.pop" | "Map.pop!" | "Map.pop_lazy" | "Map.get_lazy") => H::Map,
        K::Call(f) if matches!(f.as_str(), ":maps.put" | ":maps.update" | ":maps.remove" | ":maps.from_keys") => H::Map,
        K::Tuple => H::Tuple,
        K::List | K::Cons => H::List,
        _ => H::Any,
    }
}

fn retag(mut g: N, s: S) -> N {
    if g.s == S::G {
        g.s = s;
    }
    g.kids = g.kids.into_iter().map(|k| retag(k, s)).collect();
    g
}
