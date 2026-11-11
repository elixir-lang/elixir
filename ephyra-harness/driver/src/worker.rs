// Unless explicitly stated otherwise all files in this repository are licensed under the MIT License.
// This product includes software developed at Datadog (https://www.datadoghq.com/) Copyright 2026-Present Datadog, Inc.

//! One long-lived Elixir VM running worker.exs, speaking JSON lines, with a
//! watchdog that kills it when an answer takes too long.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

pub struct Worker {
    build: String,
    child: Option<(Child, ChildStdin, BufReader<ChildStdout>)>,
    pub restarts: u64,
}

pub enum Answer {
    Json(serde_json::Value),
    /// no answer within the limit, or the VM died
    Lost(String),
}

impl Worker {
    pub fn new(build: &str) -> Self {
        Worker { build: build.to_string(), child: None, restarts: 0 }
    }

    fn start(&mut self) {
        // OTP_BIN (optional): the Erlang/OTP bin directory to use; otherwise erl from PATH.
        let path = match std::env::var("OTP_BIN") {
            Ok(otp) if !otp.is_empty() => format!("{otp}:{}", std::env::var("PATH").unwrap_or_default()),
            _ => std::env::var("PATH").unwrap_or_default(),
        };
        let mut child = Command::new(format!("{}/elixir/bin/elixir", self.build))
            .arg(format!("{}/harness/worker.exs", self.build))
            .env("PATH", path)
            .env("ELIXIR_ERL_OPTIONS", "+S 2:2 +sbwt none")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("cannot start elixir worker");
        let stdin = child.stdin.take().unwrap();
        let stdout = BufReader::new(child.stdout.take().unwrap());
        self.child = Some((child, stdin, stdout));
    }

    pub fn ask(&mut self, case: &serde_json::Value, limit: Duration) -> Answer {
        if self.child.is_none() {
            self.start();
        }
        let (child, stdin, stdout) = self.child.as_mut().unwrap();
        let line = serde_json::to_string(case).unwrap();
        if writeln!(stdin, "{line}").and_then(|_| stdin.flush()).is_err() {
            self.kill();
            return Answer::Lost("write failed (worker died)".into());
        }
        let pid = child.id().to_string();
        let (done, wait) = mpsc::channel::<()>();
        let watchdog = std::thread::spawn(move || {
            let late = wait.recv_timeout(limit) == Err(mpsc::RecvTimeoutError::Timeout);
            if late {
                let _ = Command::new("kill").args(["-9", &pid]).status();
            }
            late
        });
        let mut answer = String::new();
        let read = stdout.read_line(&mut answer);
        drop(done);
        let late = watchdog.join().unwrap();
        if late || read.is_err() || answer.is_empty() {
            self.kill();
            return Answer::Lost(if late { "timeout".into() } else { "worker died".into() });
        }
        match serde_json::from_str(answer.trim()) {
            Ok(v) => Answer::Json(v),
            Err(_) => {
                self.kill();
                Answer::Lost(format!("bad answer: {}", &answer[..answer.len().min(200)]))
            }
        }
    }

    fn kill(&mut self) {
        if let Some((mut child, _, _)) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
            self.restarts += 1;
        }
    }

    /// Close stdin so the VM exports cover data and exits cleanly.
    pub fn stop(&mut self) {
        if let Some((mut child, stdin, _)) = self.child.take() {
            drop(stdin);
            for _ in 0..200 {
                if let Ok(Some(_)) = child.try_wait() {
                    return;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        self.stop();
    }
}
