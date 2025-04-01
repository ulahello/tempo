tempo(1)

# NAME

tempo - tempo tapper

# SYNOPSIS

*tempo*

# DESCRIPTION

*tempo* is a command-line tempo tapper that runs as an interactive shell. While
you tap, it collects BPM samples into a buffer and displays the average.

The buffer is resizable, and may also be "unbounded" to ignore the size limit.
When the buffer has too many samples, the newest are kept.

# COMMANDS

*tempo* is controlled by issuing commands to the shell.

[[ Command
:[ Input
:[ Description
|[ Help
:  h
:  Describe commands
|  Tap
:  <Enter>
:  Register a tap
|  Clear
:  c
:  Clear buffer contents
|  Size
:  s
:  Adjust buffer size
|  Bound
:  b
:  Toggle whether buffer is bounded to size
|  Print
:  p
:  Print buffer contents in order from newest to oldest
|  Quit
:  q
:  Quit

# AUTHORS

Maintained by Ula Shipman <ula.hello@mailbox.org>.

Source code and issue tracker can be found at
<https://github.com/ulahello/tempo>.
