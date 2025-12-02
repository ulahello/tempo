# Installing the Rust variant

Install the [Rust toolchain](https://www.rust-lang.org/tools/install).

To build, run, and install,
```console
$ cargo build --release
$ cargo run --release
$ cargo install --path .
```

Or, to specify the installation prefix/root,
```console
$ cargo install --path . --root FIXME
```
