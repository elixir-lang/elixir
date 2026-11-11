// Unless explicitly stated otherwise all files in this repository are licensed under the MIT License.
// This product includes software developed at Datadog (https://www.datadoghq.com/) Copyright 2026-Present Datadog, Inc.

//! elixir-fuzz --build <dir> --seed <n> --secs <s> --out <dir> [--workers <k>]
//!
//! Generates modules (gen.rs), sends them to long-lived Elixir workers
//! (worker.exs) that compile them, read the inferred signatures and run the
//! calls, shrinks failures greedily and writes self-contained findings.

mod ast;
mod gen;
mod worker;

use ephyra::Source;
use gen::{Case, G};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use worker::{Answer, Worker};

const ASK_LIMIT: Duration = Duration::from_secs(70);
const SHRINK_BUDGET: Duration = Duration::from_secs(90);
const PER_SIG: usize = 3;

#[derive(Default)]
struct Shared {
    stats: BTreeMap<String, u64>,
    saved: BTreeMap<String, usize>,
    skip_logged: BTreeMap<String, usize>,
}

struct Ctx {
    build: String,
    out: PathBuf,
    deadline: Instant,
    shared: Mutex<Shared>,
}

impl Ctx {
    fn bump(&self, k: &str, by: u64) {
        let mut s = self.shared.lock().unwrap();
        *s.stats.entry(k.to_string()).or_default() += by;
    }

    fn write_stats(&self) {
        let s = self.shared.lock().unwrap();
        let mut m = serde_json::Map::new();
        m.insert("cases".into(), (*s.stats.get("cases").unwrap_or(&0)).into());
        m.insert("failures".into(), (*s.stats.get("failures").unwrap_or(&0)).into());
        for (k, v) in &s.stats {
            if k != "cases" && k != "failures" {
                m.insert(k.clone(), (*v).into());
            }
        }
        let tmp = self.out.join("stats.json.tmp");
        std::fs::write(&tmp, serde_json::to_string_pretty(&serde_json::Value::Object(m)).unwrap()).unwrap();
        std::fs::rename(&tmp, self.out.join("stats.json")).unwrap();
    }

    fn log_skip(&self, why: &str, msg: &str, case: &Case) {
        let mut s = self.shared.lock().unwrap();
        let c = s.skip_logged.entry(why.to_string()).or_default();
        if *c < 20 {
            *c += 1;
            let dir = self.out.join("skips");
            let _ = std::fs::create_dir_all(&dir);
            let f = dir.join(format!("{why}-{c}.txt"));
            let _ = std::fs::write(f, format!("{msg}\n----\n{}\n", case.to_json()));
        }
    }
}

fn arg(args: &[String], name: &str) -> Option<String> {
    args.iter().position(|a| a == name).and_then(|i| args.get(i + 1).cloned())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let build = arg(&args, "--build").expect("--build");
    let seed: u64 = arg(&args, "--seed").unwrap_or("1".into()).parse().unwrap();
    let secs: u64 = arg(&args, "--secs").unwrap_or("60".into()).parse().unwrap();
    let out = PathBuf::from(arg(&args, "--out").expect("--out"));
    let workers: usize = arg(&args, "--workers").unwrap_or("3".into()).parse().unwrap();
    if let Some(n) = arg(&args, "--sample") {
        // print generated cases (for looking at what we generate)
        for i in 0..n.parse::<u64>().unwrap() {
            let mut src = Source::new(seed.wrapping_add(i), 10);
            let c = G::new(&mut src).case();
            println!("{}", serde_json::to_string(&c.to_json()).unwrap());
        }
        return;
    }
    std::fs::create_dir_all(out.join("findings")).unwrap();
    let start = Instant::now();
    let ctx = Arc::new(Ctx {
        build: std::fs::canonicalize(&build).unwrap().to_string_lossy().into_owned(),
        out: out.clone(),
        deadline: start + Duration::from_secs(secs),
        shared: Mutex::new(Shared::default()),
    });
    ctx.write_stats();
    let mut handles = vec![];
    for w in 0..workers {
        let ctx = ctx.clone();
        handles.push(std::thread::spawn(move || search(&ctx, seed.wrapping_mul(1_000_003).wrapping_add(w as u64))));
    }
    let ticker = {
        let ctx = ctx.clone();
        std::thread::spawn(move || {
            while Instant::now() < ctx.deadline + Duration::from_secs(SHRINK_BUDGET.as_secs() + 30) {
                std::thread::sleep(Duration::from_secs(5));
                ctx.write_stats();
            }
        })
    };
    for h in handles {
        let _ = h.join();
    }
    drop(ticker);
    ctx.bump("elapsed_s", start.elapsed().as_secs());
    ctx.write_stats();
}

fn search(ctx: &Ctx, stream: u64) {
    let mut w = Worker::new(&ctx.build);
    let mut rng = Source::new(stream, 0);
    while Instant::now() < ctx.deadline {
        let case_seed: u64 = rng.int(..);
        let mut src = Source::new(case_seed, 10);
        let case = G::new(&mut src).case();
        let ans = w.ask(&case.to_json(), ASK_LIMIT);
        ctx.bump("cases", 1);
        match ans {
            Answer::Lost(why) => {
                ctx.bump("skip_lost", 1);
                ctx.log_skip("lost", &why, &case);
            }
            Answer::Json(v) => {
                // numeric fields of the answer are counters (calls ok/raised, warnings, ...)
                if let Some(o) = v.as_object() {
                    for (k, x) in o {
                        if let (Some(n), false) = (x.as_u64(), k == "call") {
                            ctx.bump(&format!("n_{k}"), n);
                        }
                    }
                }
                match v["v"].as_str() {
                    Some("pass") => ctx.bump("passed", 1),
                    Some("skip") => {
                        let why = v["skip"].as_str().unwrap_or("unknown").to_string();
                        ctx.bump(&format!("skip_{why}"), 1);
                        ctx.log_skip(&why, v["msg"].as_str().unwrap_or(""), &case);
                    }
                    Some("fail") => {
                        ctx.bump("failures", 1);
                        let sig = v["sig"].as_str().unwrap().to_string();
                        let slot = {
                            let mut s = ctx.shared.lock().unwrap();
                            let c = s.saved.entry(sig.clone()).or_default();
                            *c += 1;
                            *c
                        };
                        if slot <= PER_SIG {
                            let (min, msg) = shrink(ctx, &mut w, case, &sig, &v);
                            save(ctx, &sig, slot, &min, &msg, case_seed);
                        }
                    }
                    _ => ctx.bump("skip_bad_answer", 1),
                }
            }
        }
    }
    w.stop();
}

fn fails_with(w: &mut Worker, c: &Case, sig: &str) -> Option<serde_json::Value> {
    match w.ask(&c.to_json(), ASK_LIMIT) {
        Answer::Json(v) if v["v"] == "fail" && v["sig"] == sig => Some(v),
        _ => None,
    }
}

/// Greedy shrinking: take the first strictly smaller candidate that still
/// fails with the same signature, until none does or the budget runs out.
fn shrink(ctx: &Ctx, w: &mut Worker, case: Case, sig: &str, first: &serde_json::Value) -> (Case, String) {
    let mut best = case;
    let mut msg = first["msg"].as_str().unwrap_or("").to_string();
    if sig.starts_with("hang__") {
        return (best, msg); // each check costs the whole time limit
    }
    if first.get("noshrink").and_then(|x| x.as_bool()) == Some(true) {
        return (best, msg); // oracle 9: the trial depends on eseed only, the module is irrelevant
    }
    let until = (Instant::now() + SHRINK_BUDGET).min(ctx.deadline.max(Instant::now() + Duration::from_secs(20)));
    // the failing call alone first
    if let Some(i) = first.get("call").and_then(|x| x.as_u64()) {
        let mut c = best.clone();
        c.calls = vec![c.calls[i as usize].clone()];
        if let Some(v) = fails_with(w, &c, sig) {
            best = c;
            msg = v["msg"].as_str().unwrap_or("").to_string();
        }
    }
    'outer: while Instant::now() < until {
        let size = best.size();
        let mut cands: Vec<Case> = vec![];
        for i in 0..best.calls.len() {
            let mut c = best.clone();
            c.calls.remove(i);
            cands.push(c);
        }
        ast::shrink(&best.module, &mut |m| {
            cands.push(Case { module: m, calls: best.calls.clone(), funargs: best.funargs.clone(), gvar: best.gvar });
        });
        for (ci, call) in best.calls.iter().enumerate() {
            for (ai, a) in call.args.iter().enumerate() {
                ast::shrink(a, &mut |m| {
                    let mut c = best.clone();
                    c.calls[ci].args[ai] = m;
                    cands.push(c);
                });
            }
        }
        for c in cands {
            if Instant::now() >= until {
                break 'outer;
            }
            if c.size() >= size {
                continue;
            }
            if let Some(v) = fails_with(w, &c, sig) {
                best = c;
                msg = v["msg"].as_str().unwrap_or("").to_string();
                continue 'outer;
            }
        }
        break;
    }
    (best, msg)
}

fn save(ctx: &Ctx, sig: &str, slot: usize, case: &Case, msg: &str, seed: u64) {
    let dir = ctx.out.join("findings").join(sig).join(slot.to_string());
    std::fs::create_dir_all(&dir).unwrap();
    let j = case.to_json();
    std::fs::write(dir.join("case.json"), serde_json::to_string(&j).unwrap()).unwrap();
    let mut readable = j["src"].as_str().unwrap().to_string();
    for c in j["calls"].as_array().unwrap() {
        readable.push_str(&format!("# CaseMod.{}({})\n", c["f"].as_str().unwrap(), {
            let a = c["args"].as_str().unwrap();
            &a[1..a.len() - 1]
        }));
    }
    std::fs::write(dir.join("case.ex"), readable).unwrap();
    std::fs::copy(Path::new(&ctx.build).join("harness/worker.exs"), dir.join("worker.exs")).unwrap();
    let oracle = sig.split("__").next().unwrap();
    let f = serde_json::json!({"signature": sig, "oracle": oracle, "message": msg, "seed": seed});
    std::fs::write(dir.join("finding.json"), serde_json::to_string_pretty(&f).unwrap()).unwrap();
    let repro = format!(
        r#"#!/usr/bin/env bash
# Reproduces {sig}: exit 1 if it reproduces against <build-dir>, 0 if not, 2 if undecidable.
# case.ex is the module and the calls in readable form; case.json is what worker.exs checks.
cd "$(dirname "$0")" || exit 2
B=$(realpath "${{1:?usage: repro.sh <build-dir>}}") || exit 2
[ -n "${{OTP_BIN:-}}" ] && export PATH="$OTP_BIN:$PATH"
V=$("$B/elixir/bin/elixir" worker.exs --repro case.json '{sig}' 2>/dev/null | tail -1)
echo "$V"
case "$V" in "REPRO yes") exit 1 ;; "REPRO no") exit 0 ;; *) exit 2 ;; esac
"#
    );
    let rp = dir.join("repro.sh");
    std::fs::write(&rp, repro).unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&rp, std::fs::Permissions::from_mode(0o755)).unwrap();
}
