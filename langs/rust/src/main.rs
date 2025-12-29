#![warn(clippy::pedantic, clippy::cargo)]

mod ringbuf;
mod tap;

#[cfg(test)]
mod tests;

use core::num::IntErrorKind;
use std::{
    env,
    io::{self, BufRead, Read, Write},
    process::ExitCode,
};

use tap::Tapper;

const DEFAULT_BUF_CAP: u16 = 10;
const DEFAULT_BOUNDED: bool = true;
const BPM_PRECISION: usize = 1;
const INPUT_BUF_SIZE: u16 = 1024;

fn main() -> ExitCode {
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    if env::args().len() > 1 {
        _ = writeln!(stderr, "usage: {}", env!("CARGO_BIN_NAME"));
        return ExitCode::FAILURE;
    }

    let result = try_main(&mut stdout);
    if let Err(error) = result {
        _ = writeln!(stderr, "fatal: {error}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn try_main<W: Write>(mut stdout: W) -> io::Result<()> {
    let mut tapper = Tapper::new(DEFAULT_BUF_CAP, DEFAULT_BOUNDED);
    let mut input = String::with_capacity(INPUT_BUF_SIZE.into());

    // splash text
    writeln!(
        stdout,
        "{} {}: {}",
        env!("CARGO_BIN_NAME"),
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_DESCRIPTION"),
    )?;
    writeln!(stdout, r#"type "h" for help"#)?;

    loop {
        // print the bpm and buffer stats
        writeln!(stdout)?;
        writeln!(
            stdout,
            "{}/{}{} samples in buffer",
            tapper.count(),
            tapper.capacity(),
            if tapper.is_bounded() { "" } else { "+" }
        )?;
        writeln!(stdout, "{:.BPM_PRECISION$} BPM", tapper.bpm())?;

        // read and parse command
        readln(
            &mut stdout,
            &mut input,
            if tapper.is_recording() { " * " } else { " ; " },
        )?;
        let cmd = if input.is_empty() {
            // quit on EOF
            Command::Quit
        } else if let Some(cmd) = Command::from_str(input.trim()) {
            cmd
        } else {
            writeln!(stdout)?;
            writeln!(stdout, r#" unrecognized command. try "h" for help."#)?;
            continue;
        };

        // perform command
        match cmd {
            Command::Help => {
                writeln!(stdout)?;
                for cmd in Command::iter() {
                    writeln!(
                        stdout,
                        " {} or {}. {}.",
                        cmd.long_name(),
                        cmd.short_name(),
                        cmd.description()
                    )?;
                }
            }

            Command::Tap => tapper.tap(),

            Command::Clear => tapper.clear(),

            Command::Size => {
                writeln!(stdout)?;
                readln(&mut stdout, &mut input, " new buffer size? ")?;
                let trimmed = input.trim();
                if !trimmed.is_empty() {
                    let cap = match trimmed.parse::<u16>() {
                        Ok(cap) => Some(cap),
                        Err(error) => {
                            if *error.kind() == IntErrorKind::PosOverflow {
                                Some(u16::MAX)
                            } else {
                                writeln!(stdout, " invalid integer")?;
                                None
                            }
                        }
                    };
                    if let Some(cap) = cap {
                        tapper.resize(cap);
                        let reported = tapper.capacity();
                        if reported < cap {
                            writeln!(stdout, " size too large, clamped to {reported}")?;
                        }
                    }
                }
            }

            Command::Bound => tapper.toggle_bounded(),

            Command::Print => {
                writeln!(stdout)?;
                writeln!(stdout, " {tapper:.BPM_PRECISION$}")?;
            }

            Command::Quit => {
                writeln!(stdout)?;
                writeln!(stdout, " goodbye")?;
                writeln!(stdout)?;
                break;
            }
        }
    }

    Ok(())
}

fn readln(mut out: impl Write, input: &mut String, prompt: &str) -> io::Result<()> {
    write!(out, "{prompt}")?;
    out.flush()?;
    input.clear();
    io::stdin()
        .lock()
        .take(INPUT_BUF_SIZE.into())
        .read_line(input)?;
    Ok(())
}

#[derive(Clone, Copy, Debug)]
enum Command {
    Help,
    Tap,
    Clear,
    Size,
    Bound,
    Print,
    Quit,
}

impl Command {
    pub fn from_str(s: &str) -> Option<Self> {
        for cmd in Self::iter() {
            for test in [cmd.literal(), cmd.long_name()] {
                if str::eq_ignore_ascii_case(s, test) {
                    return Some(*cmd);
                }
            }
        }
        None
    }

    pub const fn iter() -> &'static [Self] {
        &[
            Self::Help,
            Self::Tap,
            Self::Clear,
            Self::Size,
            Self::Bound,
            Self::Print,
            Self::Quit,
        ]
    }

    pub const fn literal(self) -> &'static str {
        match self {
            Self::Help => "h",
            Self::Tap => "",
            Self::Clear => "c",
            Self::Size => "s",
            Self::Bound => "b",
            Self::Print => "p",
            Self::Quit => "q",
        }
    }

    pub const fn short_name(self) -> &'static str {
        match self {
            Self::Tap => "<enter>",
            other => other.literal(),
        }
    }

    pub const fn long_name(self) -> &'static str {
        match self {
            Self::Help => "help",
            Self::Tap => "tap",
            Self::Clear => "clear",
            Self::Size => "size",
            Self::Bound => "bound",
            Self::Print => "print",
            Self::Quit => "quit",
        }
    }

    pub const fn description(self) -> &'static str {
        match self {
            Self::Help => "describe commands",
            Self::Tap => "register a beat",
            Self::Clear => "clear buffer contents",
            Self::Size => "adjust buffer size",
            Self::Bound => "toggle whether buffer is bounded to size",
            Self::Print => "print buffer contents in order from newest to oldest",
            Self::Quit => "quit",
        }
    }
}
