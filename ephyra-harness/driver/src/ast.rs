// Unless explicitly stated otherwise all files in this repository are licensed under the MIT License.
// This product includes software developed at Datadog (https://www.datadoghq.com/) Copyright 2026-Present Datadog, Inc.

//! A generic Elixir syntax tree: one node type, rendered by kind. Every node
//! carries its sort (expression, pattern, guard, structure) so the shrinker can
//! replace a node by a smaller node of the same sort without knowing the kind.

/// Sort of a node: what may stand in its place.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum S {
    E, // expression (also literal values in calls)
    P, // pattern
    G, // guard expression
    X, // structure (module, clause, params, arms, ...)
}

#[derive(Clone, Debug, PartialEq)]
pub enum K {
    Module,
    Fun(String, bool), // name, private?; kids: Clause*
    Clause,            // kids: [Params, Guards, body]; in fn: same
    Params,            // kids: patterns
    Guards,            // kids: 0 or 1 guard
    Var(String),
    Lit(String),
    Wild,
    #[allow(dead_code)] // rendered and shrunk, not generated yet
    Pin(String),
    Tuple,
    List,
    Cons,         // [h | t]
    Map,          // kids: Pair* / PairE*
    Struct(String), // %Name{Pair*} (Name = CaseS, CaseT or _ in patterns)
    Bin,            // <<Seg, ...>>
    Seg(String),    // kid0::spec
    Pair(String), // literal key => kid0
    PairE,        // kid0 => kid1 (expression keys)
    Bind,         // pattern kid0 = kid1 (kid1 a Var pattern)
    MapUpd,       // %{kid0 | pairs}
    Call(String), // name(kids)
    Op(String),   // (kid0 op kid1)
    Un(String),   // (op kid0)
    Case,         // kids: [scrutinee, Arm*]
    Arm,          // kids: [pattern, Guards, body]
    If,           // [cond, then, else]
    Cond,         // CondArm*
    CondArm,      // [cond, body]
    Block,        // Match* then a final expression
    Match,        // [pattern, expr]
    Fn,           // Clause*
    FnCall,       // [Fn, args...]
    With,         // [WithArm+, body, Else?]
    WithArm,      // [pattern, expr]
    Else,         // Arm*
    Capture(String), // &name/arity (leaf)
    For(String),     // [WithArm+, filter*, body]; "reduce": [WithArm+, filter*, init, Arm]; else "", "uniq: true", "into: %{}"
    Try,             // [body, TryPart*]
    TryPart(String), // "rescue" | "catch" | "else": Arm*; "after": [expr]
    Receive,         // [Arm*, TryPart("after")] (after 0)
}

#[derive(Clone, Debug, PartialEq)]
pub struct N {
    pub k: K,
    pub s: S,
    pub kids: Vec<N>,
}

pub fn n(k: K, s: S, kids: Vec<N>) -> N {
    N { k, s, kids }
}
pub fn lit(t: &str) -> N {
    n(K::Lit(t.to_string()), S::E, vec![])
}

impl N {
    pub fn size(&self) -> usize {
        1 + self.kids.iter().map(N::size).sum::<usize>()
    }

    pub fn render(&self) -> String {
        let mut out = Out::default();
        self.r(&mut out);
        out.s
    }

    /// Render with a probe `Process.put({:h, id}, 1)` as the first statement of
    /// every def clause, case/else arm and fn clause, whose head (pattern and
    /// guard only) is alone on its line. Returns the source and (id, line, kind).
    ///
    /// Also returns (line, probe id) for every line whose code is all evaluated
    /// unconditionally once that probe's body runs (not in a nested clause, branch,
    /// head, guard or short-circuit right operand): when the probe was hit by a call
    /// that returned normally, every expression on the line evaluated without raising.
    ///
    /// And (line, statement probe id) for those lines that belong to a block statement: a hit of
    /// that probe (in any call) proves the line evaluated without raising.
    pub fn render_probed(&self) -> Probed {
        let mut out = Out { probe: true, instrument: true, ..Default::default() };
        self.r(&mut out);
        let lines = out.owned_lines();
        let posts = out.post_lines(&lines);
        (out.s, out.probes, lines, posts)
    }

    fn join(&self, out: &mut Out, kids: &[N], sep: &str) {
        for (i, k) in kids.iter().enumerate() {
            if i > 0 {
                out.push_str(sep);
            }
            k.r(out);
        }
    }

    /// Render without probes and without owned lines (code whose completion a later probe
    /// hit does not prove).
    fn unprobed(out: &mut Out, f: impl FnOnce(&mut Out)) {
        let saved = out.set_ctx(None);
        let was = std::mem::replace(&mut out.probe, false);
        f(out);
        out.probe = was;
        out.set_ctx(saved);
    }

    fn guard(g: &N, out: &mut Out) {
        if let Some(x) = g.kids.first() {
            out.push_str(" when ");
            x.r(out);
        }
    }

    /// Render the same probed module with some expressions wrapped in `CaseDyn.id(...)` (an
    /// identity whose result is typed dynamic()): `permille` of the eligible nodes, chosen by a
    /// hash of `seed` and the node's rendering order. Wrapping adds no newline, so every line
    /// keeps its number. Not wrapped: patterns, guards, variables, structure, `for`, and anything in
    /// positions that refine variables (case subject, if/cond condition, `with` and `=` right side):
    /// hiding a variable there drops a refinement, not just precision, and the checker does not
    /// report clauses that never match a literal subject (`case {0.5, :b} do %{} -> ...` is silent,
    /// `case {0.5, CaseDyn.id(:b)}` warns).
    pub fn render_wrapped(&self, seed: u64, permille: u32) -> String {
        let mut out = Out { probe: true, instrument: true, wrap: Some((seed, permille)), ..Default::default() };
        self.r(&mut out);
        out.s
    }

    /// Render in a refining position (`on`): nothing is wrapped there.
    fn refining(&self, out: &mut Out, on: bool) {
        out.refine += on as u32;
        self.r(out);
        out.refine -= on as u32;
    }

    fn r(&self, out: &mut Out) {
        if let Some((seed, permille)) = out.wrap {
            if out.nowrap == 0
                && self.s == S::E
                && !matches!(
                    self.k,
                    K::Var(_) | K::Wild | K::Pin(_) | K::Pair(_) | K::PairE | K::Seg(_) | K::Match | K::WithArm
                        | K::CondArm | K::Arm | K::Clause | K::Else | K::TryPart(_) | K::Params | K::Guards
                )
                && out.refine == 0
            {
                out.wctr += 1;
                let mut h = seed ^ out.wctr.wrapping_mul(0x9e37_79b9_7f4a_7c15);
                h ^= h >> 29;
                h = h.wrapping_mul(0xbf58_476d_1ce4_e5b9);
                h ^= h >> 32;
                if (h % 1000) < permille as u64 {
                    out.push_str("CaseDyn.id(");
                    self.r_inner(out);
                    out.push(')');
                    return;
                }
            }
            let sup = matches!(self.s, S::P | S::G) || matches!(self.k, K::For(_));
            out.nowrap += sup as u32;
            self.r_inner(out);
            out.nowrap -= sup as u32;
            return;
        }
        self.r_inner(out)
    }

    fn r_inner(&self, out: &mut Out) {
        let k = &self.kids;
        match &self.k {
            K::Module => {
                out.push_str("defmodule CaseMod do\n");
                for f in k {
                    f.r(out);
                }
                out.push_str("end\n");
            }
            K::Fun(name, private) => {
                for c in k {
                    let line = out.line();
                    let saved = out.set_ctx(None);
                    out.push_str(if *private { "  defp " } else { "  def " });
                    out.push_str(name);
                    out.push('(');
                    self.join(out, &c.kids[0].kids, ", ");
                    out.push(')');
                    N::guard(&c.kids[1], out);
                    out.push_str(" do\n    ");
                    out.probe(line, "def");
                    c.kids[2].r(out);
                    out.set_ctx(None);
                    out.push_str("\n  end\n");
                    out.set_ctx(saved);
                }
            }
            K::Clause => {
                let line = out.line();
                let saved = out.set_ctx(None);
                self.join(out, &k[0].kids, ", ");
                N::guard(&k[1], out);
                out.push_str(" ->\n");
                out.probe(line, "fn");
                k[2].r(out);
                out.set_ctx(saved);
            }
            K::Params | K::Guards => self.join(out, k, ", "),
            K::Var(v) => out.push_str(v),
            K::Lit(t) => out.push_str(t),
            K::Wild => out.push('_'),
            K::Pin(v) => {
                out.push('^');
                out.push_str(v);
            }
            K::Tuple => {
                out.push('{');
                self.join(out, k, ", ");
                out.push('}');
            }
            K::List => {
                out.push('[');
                self.join(out, k, ", ");
                out.push(']');
            }
            K::Cons => {
                out.push('[');
                k[0].r(out);
                out.push_str(" | ");
                k[1].r(out);
                out.push(']');
            }
            K::Map => {
                out.push_str("%{");
                self.join(out, k, ", ");
                out.push('}');
            }
            K::Struct(name) => {
                out.push('%');
                out.push_str(name);
                out.push('{');
                self.join(out, k, ", ");
                out.push('}');
            }
            K::Bin => {
                // spaces: `<<<<` would lex as `<<<`
                out.push_str("<< ");
                self.join(out, k, ", ");
                out.push_str(" >>");
            }
            K::Seg(spec) => {
                k[0].r(out);
                out.push_str("::");
                out.push_str(spec);
            }
            K::Pair(key) => {
                out.push_str(key);
                out.push_str(" => ");
                k[0].r(out);
            }
            K::PairE => {
                k[0].r(out);
                out.push_str(" => ");
                k[1].r(out);
            }
            K::Bind => {
                out.push('(');
                k[0].r(out);
                out.push_str(" = ");
                k[1].r(out);
                out.push(')');
            }
            K::MapUpd => {
                out.push_str("%{");
                k[0].r(out);
                out.push_str(" | ");
                self.join(out, &k[1..], ", ");
                out.push('}');
            }
            K::Call(f) => {
                out.push_str(f);
                out.push('(');
                self.join(out, k, ", ");
                out.push(')');
            }
            K::Op(op) => {
                out.push('(');
                k[0].r(out);
                out.push(' ');
                out.push_str(op);
                out.push(' ');
                // short-circuit: the right operand may not be evaluated
                let lazy = matches!(op.as_str(), "and" | "or" | "&&" | "||");
                let saved = if lazy { out.set_ctx(None) } else { out.ctx };
                k[1].r(out);
                out.set_ctx(saved);
                out.push(')');
            }
            K::Un(op) => {
                out.push('(');
                out.push_str(op);
                // `(- -1)`, not the `--` operator
                if op.chars().all(char::is_alphabetic) || op == "-" || op == "+" {
                    out.push(' ');
                }
                k[0].r(out);
                out.push(')');
            }
            K::Case => {
                out.push_str("(case ");
                k[0].refining(out, true);
                out.push_str(" do\n");
                for a in &k[1..] {
                    a.r(out);
                    out.push('\n');
                }
                out.push_str("end)");
            }
            K::Arm => {
                let line = out.line();
                let saved = out.set_ctx(None);
                k[0].r(out);
                N::guard(&k[1], out);
                out.push_str(" ->\n");
                if std::mem::take(&mut out.catch_arm) && out.instrument {
                    // a rescue/catch arm ran: the worker does not take this call's probe hits as
                    // completed bodies (a raise in a called function may have been rescued here)
                    out.push_str("Process.put({:h, :caught}, 1)\n");
                }
                let kind = if std::mem::take(&mut out.else_arm) { "else" } else { "case" };
                out.probe(line, kind);
                k[2].r(out);
                out.set_ctx(saved);
            }
            K::If => {
                out.push_str("(if ");
                k[0].refining(out, true);
                out.push_str(" do\n");
                let saved = out.ctx;
                // branch probes (line 0: no clause claim refers to them)
                out.probe(0, "if");
                k[1].r(out);
                out.set_ctx(None);
                out.push_str("\nelse\n");
                out.probe(0, "if");
                k[2].r(out);
                out.set_ctx(saved);
                out.push_str("\nend)");
            }
            K::Cond => {
                out.push_str("(cond do\n");
                let saved = out.set_ctx(None);
                for a in k {
                    a.r(out);
                    out.push('\n');
                }
                out.set_ctx(saved);
                out.push_str("end)");
            }
            K::CondArm => {
                let saved = out.set_ctx(None);
                k[0].refining(out, true);
                if out.probe {
                    out.push_str(" ->\n");
                    out.probe(0, "cond");
                } else {
                    out.push_str(" -> ");
                }
                k[1].r(out);
                out.set_ctx(saved);
            }
            K::Block => {
                out.push_str("(\n");
                for (i, x) in k.iter().enumerate() {
                    let from = out.line();
                    x.r(out);
                    out.push('\n');
                    if i + 1 < k.len() {
                        // statement probe: hit only once the statement completed
                        out.post_probe(from);
                    }
                }
                out.push(')');
            }
            K::Match => {
                k[0].r(out);
                out.push_str(" = ");
                k[1].refining(out, true);
            }
            K::Fn => {
                out.push_str("fn\n");
                for c in k {
                    c.r(out);
                    out.push('\n');
                }
                out.push_str("end");
            }
            K::FnCall => {
                out.push('(');
                k[0].r(out);
                out.push_str(").(");
                self.join(out, &k[1..], ", ");
                out.push(')');
            }
            K::With => {
                out.push_str("(with ");
                let saved = out.set_ctx(None);
                let arms: Vec<&N> = k.iter().take_while(|x| x.k == K::WithArm).collect();
                for (i, a) in arms.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    a.r(out);
                }
                out.push_str(" do\n");
                out.probe(0, "with");
                k[arms.len()].r(out);
                out.set_ctx(None);
                if let Some(e) = k.get(arms.len() + 1) {
                    out.push_str("\nelse\n");
                    for a in &e.kids {
                        out.else_arm = true;
                        a.r(out);
                        out.push('\n');
                    }
                } else {
                    out.push('\n');
                }
                out.set_ctx(saved);
                out.push_str("end)");
            }
            K::WithArm => {
                k[0].r(out);
                out.push_str(" <- ");
                k[1].refining(out, true);
            }
            K::Else => self.join(out, k, "\n"),
            K::For(opt) => {
                // a `for` runs its body zero, one or many times, and the checker documents its
                // warnings there as possible false positives: no probes, no owned lines inside
                out.push_str("(for ");
                N::unprobed(out, |out| {
                    let tail = if opt == "reduce" { 2 } else { 1 };
                    for (i, x) in k[..k.len() - tail].iter().enumerate() {
                        if i > 0 {
                            out.push_str(", ");
                        }
                        x.r(out);
                    }
                    if opt == "reduce" {
                        out.push_str(", reduce: ");
                        k[k.len() - 2].r(out);
                        out.push_str(" do\n");
                        k[k.len() - 1].r(out);
                    } else {
                        // always an option last: a trailing list literal filter would be merged
                        // with the do block (`for x <- l, [a | b] do`)
                        out.push_str(", ");
                        out.push_str(if opt.is_empty() { "uniq: false" } else { opt });
                        out.push_str(" do\n");
                        k[k.len() - 1].r(out);
                    }
                });
                out.push_str("\nend)");
            }
            K::Try => {
                out.push_str("(try do\n");
                // a rescued raise in the body would look like a completed body: no probes there
                N::unprobed(out, |out| k[0].r(out));
                let saved = out.set_ctx(None);
                for part in &k[1..] {
                    part.r(out);
                }
                out.set_ctx(saved);
                out.push_str("\nend)");
            }
            K::TryPart(kind) => {
                out.push('\n');
                if kind == "after0" {
                    out.push_str("after\n0 ->\n");
                    out.probe(0, "after");
                    k[0].r(out);
                    out.set_ctx(None);
                    return;
                }
                out.push_str(kind);
                out.push('\n');
                if kind == "after" {
                    k[0].r(out);
                    return;
                }
                for a in k {
                    if kind == "else" {
                        out.else_arm = true;
                    }
                    out.catch_arm = kind == "rescue" || kind == "catch";
                    a.r(out);
                    out.push('\n');
                }
            }
            K::Receive => {
                out.push_str("(receive do\n");
                let saved = out.set_ctx(None);
                for a in k {
                    if a.k == K::Arm {
                        a.r(out);
                        out.push('\n');
                    } else {
                        a.r(out);
                    }
                }
                out.set_ctx(saved);
                out.push_str("\nend)");
            }
            K::Capture(f) => {
                // parenthesized: `&==/2 && x` would capture the whole operator expression
                out.push_str("(&");
                out.push_str(f);
                out.push(')');
            }
        }
    }
}

pub type Probed = (String, Vec<(usize, usize, &'static str)>, Vec<(usize, usize)>, Vec<(usize, usize)>);

#[derive(Default)]
pub struct Out {
    s: String,
    probe: bool,
    /// rendering for the worker (render_probed), also inside unprobed code
    instrument: bool,
    else_arm: bool,
    catch_arm: bool,
    probes: Vec<(usize, usize, &'static str)>,
    /// probe whose body unconditionally evaluates the code being written (None: conditional)
    ctx: Option<usize>,
    /// (byte offset, ctx from there on)
    marks: Vec<(usize, Option<usize>)>,
    /// statement probes: (first line, last line, ctx of the statement, probe id)
    posts: Vec<(usize, usize, Option<usize>, usize)>,
    /// render_wrapped: (seed, permille); depth of no-wrap regions, of refining positions; node counter
    wrap: Option<(u64, u32)>,
    nowrap: u32,
    refine: u32,
    wctr: u64,
}

impl std::ops::Deref for Out {
    type Target = String;
    fn deref(&self) -> &String {
        &self.s
    }
}
impl std::ops::DerefMut for Out {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.s
    }
}

impl Out {
    /// Called right after an arm head; `line` is the head's line.
    fn probe(&mut self, line: usize, kind: &'static str) {
        if self.probe {
            let id = self.probes.len();
            self.probes.push((id, line, kind));
            self.s.push_str(&format!("Process.put({{:h, {id}}}, 1)\n"));
            self.set_ctx(Some(id));
        }
    }
    /// A probe after the statement that started on line `from` (in the current ctx).
    fn post_probe(&mut self, from: usize) {
        if self.probe && self.ctx.is_some() {
            let id = self.probes.len();
            self.probes.push((id, 0, "stmt"));
            self.posts.push((from, self.line() - 1, self.ctx, id));
            self.s.push_str(&format!("Process.put({{:h, {id}}}, 1)\n"));
        }
    }
    /// (line, statement probe id) for owned lines of statements followed by a probe: the line
    /// evaluated without raising once that probe was hit, even if the call raised later.
    fn post_lines(&self, owned: &[(usize, usize)]) -> Vec<(usize, usize)> {
        let mut r = vec![];
        for &(l, owner) in owned {
            for &(a, b, c, id) in &self.posts {
                if a <= l && l <= b && c == Some(owner) {
                    r.push((l, id));
                }
            }
        }
        r
    }
    /// Switch the evaluation context; returns the previous one.
    fn set_ctx(&mut self, c: Option<usize>) -> Option<usize> {
        let old = self.ctx;
        if self.probe {
            self.ctx = c;
            self.marks.push((self.s.len(), c));
        }
        old
    }
    /// Lines whose non-blank characters were all written in one probe's unconditional context.
    fn owned_lines(&self) -> Vec<(usize, usize)> {
        // per line: None = unset, Some(None) = mixed/conditional, Some(Some(id))
        let mut st: Vec<Option<Option<usize>>> = vec![None];
        let (mut line, mut ctx, mut mi) = (0usize, None, 0usize);
        for (i, b) in self.s.bytes().enumerate() {
            while mi < self.marks.len() && self.marks[mi].0 <= i {
                ctx = self.marks[mi].1;
                mi += 1;
            }
            if b == b'\n' {
                line += 1;
                st.push(None);
            } else if !b.is_ascii_whitespace() {
                st[line] = match st[line] {
                    None => Some(ctx),
                    Some(c) if c == ctx => Some(c),
                    _ => Some(None),
                };
            }
        }
        st.iter().enumerate().filter_map(|(l, c)| c.flatten().map(|id| (l + 1, id))).collect()
    }
    fn line(&self) -> usize {
        self.s.bytes().filter(|b| *b == b'\n').count() + 1
    }
}

/// Minimum number of kids a node of this kind must keep when one is removed;
/// None when kids cannot be removed (fixed positions).
fn removable(k: &K, kids: &[N]) -> Option<usize> {
    match k {
        K::Module | K::Fun(..) | K::Fn | K::Cond | K::Else => Some(1),
        K::Tuple | K::List | K::Map | K::Struct(_) => Some(0),
        K::Guards => Some(0),
        K::Case => Some(2),
        K::MapUpd => Some(2),
        K::Block => Some(1),
        K::With => {
            let arms = kids.iter().take_while(|x| x.k == K::WithArm).count();
            if arms > 1 {
                Some(1)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Descendants of `n` with sort `s`, nearest first (not `n` itself).
fn same_sort_descendants<'a>(n: &'a N, s: S, out: &mut Vec<&'a N>) {
    for k in &n.kids {
        if k.s == s {
            out.push(k);
        }
    }
    for k in &n.kids {
        if k.s != s {
            same_sort_descendants(k, s, out);
        }
    }
}

/// Strictly smaller variants of `n`, each passed to `visit` (whole-subtree
/// edits: drop a kid, replace a node by a same-sort descendant, replace an
/// expression by `nil` or a pattern by `_`, then recurse into kids).
pub fn shrink(n: &N, visit: &mut dyn FnMut(N)) {
    // drop a kid
    if let Some(min) = removable(&n.k, &n.kids) {
        if n.kids.len() > min {
            for i in 0..n.kids.len() {
                if n.k == K::With && n.kids[i].k != K::WithArm {
                    continue;
                }
                if n.k == K::Block && i == n.kids.len() - 1 {
                    continue;
                }
                if (n.k == K::Case || n.k == K::MapUpd) && i == 0 {
                    continue;
                }
                let mut m = n.clone();
                m.kids.remove(i);
                visit(m);
            }
        }
    }
    if n.k == K::With && n.kids.last().map(|x| x.k == K::Else) == Some(true) {
        let mut m = n.clone();
        m.kids.pop();
        visit(m);
    }
    // replace by a smaller node of the same sort
    if n.s != S::X {
        let mut ds = Vec::new();
        same_sort_descendants(n, n.s, &mut ds);
        for d in ds.into_iter().take(6) {
            visit(d.clone());
        }
        match n.s {
            S::E | S::G if n.size() > 1 => visit(lit("nil")),
            S::P if n.k != K::Wild => visit(n_wild()),
            _ => {}
        }
    }
    // recurse
    for i in 0..n.kids.len() {
        shrink(&n.kids[i], &mut |c| {
            let mut m = n.clone();
            m.kids[i] = c;
            visit(m);
        });
    }
}

fn n_wild() -> N {
    n(K::Wild, S::P, vec![])
}
