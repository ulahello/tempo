# Installing the Ada variant

This variant is written in Ada and uses the [Alire](https://alire.ada.dev/docs/#installation) package manager.

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
