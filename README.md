# tempo

`tempo` is a tempo tapper that runs in the terminal as a shell.
While you tap, it collects BPMs into a buffer and displays the average.

```
...

6/10 samples in buffer
130.4 BPM
 * <enter>

7/10 samples in buffer
129.9 BPM
 * <enter>

8/10 samples in buffer
130.1 BPM
 * █
```

## Usage

Tap the enter key to the desired tempo, and `tempo` will display the average BPM.

For fine tuning, you can also adjust the size of the buffer.
The BPM will get more accurate with more samples, supposing the source's tempo doesn't change.

```
[120.5, 124.3, 122.6, _, _, _, _, _, _] _, _, ...
 ^                                    ^
new samples inserted here       old samples removed here
```

| Command | Input     | Description                                          |
|---------|-----------|------------------------------------------------------|
| Help    | h         | Describe commands                                    |
| Tap     | \<enter\> | Register a beat                                      |
| Clear   | c         | Clear buffer contents                                |
| Size    | s         | Adjust buffer size                                   |
| Bound   | b         | Toggle whether buffer is bounded to size             |
| Print   | p         | Print buffer contents in order from newest to oldest |
| Quit    | q         | Quit                                                 |

## Installation

I've written `tempo` in several languages for learning and joy purposes.
See [`/langs`](./langs) for each variant, and check their respective `INSTALL.md` for instructions.

- [Ada](./langs/ada)
- [Rust](./langs/rust)

### Documentation

Building the [manual page](./docs/tempo.1.scd) requires [scdoc](https://git.sr.ht/~sircmpwn/scdoc/), and optionally [just](https://just.systems/).

```console
$ cd docs
$ just --list
Available recipes:
    build     # Generate the manual page for tempo(1). Requires scdoc.
    clean     # Remove local build artifacts.
    install   # Copy the manual page to $PREFIX.
    uninstall # Remove the manual page from $PREFIX.
```

To install to $PREFIX (which is `/usr/local` by default),

```console
$ cd docs
$ PREFIX="FIXME" just install
```

And to uninstall,

```console
$ cd docs
$ PREFIX="FIXME" just uninstall
```
