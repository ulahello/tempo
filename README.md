*Note!* this is literally the first Ada code I've written so please bear with me as I stumble through idioms.

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

`tempo` is written in Ada and uses the [Alire](https://alire.ada.dev/docs/#installation) package manager.

After installing Alire, make sure to select a compiler toolchain.
Alire can automatically download versions of [GNAT](https://www.gnu.org/software/gnat/) if you don't have a system-wide installation.

```console
$ alr toolchain --select
...
```

Then, to build `tempo`,

```console
$ alr build
```

This produces an executable in the `bin` directory.

To install,

```console
$ alr install --prefix FIXME
```

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
