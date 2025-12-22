// TODOO: tests

use ascii;
use encoding::utf8;
use fmt;
use io;
use os;
use ringbuf;
use strconv;
use strings;
use tapper;
use types;

def TEMPO_NAME: str = "tempo";
def TEMPO_VERSION: str = "0.1.0";
def TEMPO_DESCRIPTION: str = "terminal tempo tapper";

def DEFAULT_CAPACITY: u16 = 10;
def DEFAULT_BOUNDED: bool = true;
def INPUT_BUF_SIZE: u16 = 1024;

type command = enum {
	HELP,
	TAP,
	CLEAR,
	SIZE,
	BOUND,
	PRINT,
	QUIT,
	INVALID,
};

const command_literal: [_]str = ["h", "", "c", "s", "b", "p", "q"];
const command_longname: [_]str = ["help", "tap", "clear", "size", "bound", "print", "quit"];

const command_description: [_]str = [
	"describe commands",
	"register a beat",
	"clear buffer contents",
	"adjust buffer size",
	"toggle whether buffer is bounded to size",
	"print buffer contents in order from newest to oldest",
	"quit",
];

fn command_shortname(c: command) str = {
	if (c == command::TAP)
		return "<enter>"
	else
		return command_literal[c];
};

fn command_iter() [command::INVALID]command = {
	let out: [command::INVALID]command = [0...];
	for (let c = 0; c < command::INVALID: int; c += 1) {
		out[c] = c: command;
	};
	return out;
};

fn command_fromstr(s: str) command = {
	for (let c .. command_iter()) {
		for (let repr .. [command_literal[c], command_longname[c]]) {
			if (ascii::strcasecmp(s, repr) == 0)
				return c: command;
		};
	};
	return command::INVALID;
};

// Uses a static buffer for output, not thread safe, input overridden
// for successive calls.
fn readln(prompt: str) (str | encoding::utf8::invalid | io::EOF | io::error) = {
	static let buf: [INPUT_BUF_SIZE]u8 = [0...];

	// print prompt
	fmt::print(prompt)?;
	io::close(os::stdout)?;

	// TODO: should we be reading until a line feed or EOF, rather
	// than just trusting that one syscall corresponds to one
	// "user typed text and hit enter"? there's not an obvious way
	// to do this in stdlib...

	match (io::read(os::stdin, &buf)) {
	case let count: size =>
		let input = strings::fromutf8(buf[0..count])?;
		return strings::trim(input);
	case let err: io::error => return err;
	case let eof: io::EOF => return eof;
	};
};

fn print_splash() (void | io::error) = {
	fmt::printfln("{} {}: {}", TEMPO_NAME, TEMPO_VERSION, TEMPO_DESCRIPTION)?;
	fmt::println("type \"h\" for help")?;
};

fn print_prompt(t: *const tapper::tapper) (void | io::error) = {
	let bounded = if (tapper::is_bounded(t)) {
		yield "";
	} else {
		yield "+";
	};
	fmt::printfln("{}/{}{} samples in buffer",
		tapper::count(t),
		tapper::capacity(t),
		bounded)?;
	tapper::format_sample(os::stdout, tapper::bpm(t))?;
	fmt::println(" BPM")?;
};

fn print_help() (void | io::error) = {
	for (let c .. command_iter()) {
		fmt::printfln(" {} or {}. {}.",
			command_longname[c],
			command_shortname(c),
			command_description[c])?;
	};
};

fn interactive_resize(tapper: *const tapper::tapper) (void | io::error) = {
	fmt::println()?;
	let input = match (readln(" new buffer size? ")) {
	case let err: io::error => return err;
	case io::EOF => yield "";
	case let input: str => yield input;
	};

	if (len(input) == 0) {
		return;
	};

	let cap: i32 = match (strconv::stoi32(input)) {
	case strconv::invalid =>
		fmt::println(" invalid integer")?;
		return;
	case strconv::overflow =>
		// the return type doesn't tell me in which
		// direction it overflows
		yield if (strings::contains(input, '-')) {
			yield types::I32_MIN;
		} else {
			yield types::I32_MAX;
		};
	case let n: i32 => yield n;
	};

	let clamped: u16 = if (cap < 1) {
		yield 1;
	} else if (types::U16_MAX: i32 < cap) {
		yield types::U16_MAX;
	} else {
		yield cap: u16;
	};

	tapper::resize(tapper, clamped);
	let reported = tapper::capacity(tapper): i32;
	if (reported != cap) {
		let adj = if (reported < cap) {
			yield "large";
		} else {
			yield "small";
		};
		fmt::printfln(" size too {}, clamped to {}", adj, reported)?;
	};
};

fn repl() (void | encoding::utf8::invalid | io::error) = {
	let tapper = tapper::new(DEFAULT_CAPACITY, DEFAULT_BOUNDED);
	defer tapper::tapper_free(&tapper);

	// Print splash text
	print_splash()?;

	for (true) {
		// Print the BPM and buffer stats
		fmt::println()?;
		print_prompt(&tapper)?;

		let cmd = command::QUIT;

		let prompt = if (tapper::is_recording(&tapper)) {
			yield " * ";
		} else {
			yield " ; ";
		};
		match (readln(prompt)) {
		case io::EOF => void;
		case let err: io::error => return err;
		case let err: encoding::utf8::invalid => return err;
		case let input: str => cmd = command_fromstr(input);
		};

		switch (cmd) {
		case command::INVALID =>
			fmt::println()?;
			fmt::println(" unrecognized command. try \"h\" for help.")?;

		case command::HELP =>
			fmt::println()?;
			print_help()?;

		case command::TAP =>
			tapper::tap(&tapper);

		case command::CLEAR =>
			tapper::clear(&tapper);

		case command::SIZE =>
			interactive_resize(&tapper)?;

		case command::BOUND =>
			tapper::toggle_bounded(&tapper);

		case command::PRINT =>
			fmt::println()?;
			fmt::print(" ")?;
			tapper::format(os::stdout, &tapper)?;
			fmt::println()?;

		case command::QUIT =>
			fmt::println()?;
			fmt::println(" goodbye")?;
			fmt::println()?;
			break;
		};
	};
};

export fn main() void = {
	if (1 < len(os::args)) {
		let unused = fmt::errorfln("usage: {}", os::args[0]);
		os::exit(os::status::FAILURE);
	};

	match (repl()) {
	case let err: io::error =>
	     fmt::fatalf("fatal error: input/output error ({})", io::strerror(err));
	case encoding::utf8::invalid =>
	     fmt::fatalf("fatal error: invalid UTF-8 input");
	case void => void;
	};
};
