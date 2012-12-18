Spy
===

Spy is a compact file system watcher for Mac OS X using the [File System Events API](https://developer.apple.com/library/mac/#documentation/Darwin/Conceptual/FSEvents_ProgGuide/Introduction/Introduction.html) via [hfsevents](https://github.com/luite/hfsevents).


Usage
=====

Spy expects a single argument, the directory or a single file to watch. Spy currently supports two different modes: watch and run mode.

Watch mode
-----------

In output mode Spy will print the path to a modified file to STDOUT (followed by a newline) whenever a file modification (new file added, file modified, file deleted) occurs.

    $> spy watch .
    path/to/modified/file

Because watch is the default mode you can omit the `watch` command if you want:

    $> spy .

It's possible to watch a single file (this obviously only shows changes to that particular file):

    $> spy /path/to/file

The default format is the full path to the modified file followed by a newline. To make it easier to parse the output, the `--format=json` changes the output to be printed formatted as a JSON object (again followed by a newline).

    $> spy watch --format=json .
    {"path": "path/to/modified/file", "flags": ["ItemModified"], "id": 123143234}

For directories the following options apply:

An optional second argument can be used to filter the files in the given directory using a glob pattern:

    $> spy watch /path/to/directory "*.md"


Run mode
--------

In run mode Spy will execute a given command whenever a file modification occurs without printing modifications to stdout. The command will be executed with the path to the modified file as the last argument.

    $> spy run "./run-build.sh"

In the example above the shell script `run-build.sh` would be executed with the path to the modified file as the first argument.

If the command to be executed does not expect any (additional) arguments the `--notify-only` flag can be used. This will cause spy to execute the command without passing the path as an argument:

    $> spy run --notify-only "rake test" .


Installation
============

Spy only works on Mac OS X >= 10.7 (Lion and above)!

Binary distribution
-------------------

The binary distribution contains a 64bit binary compiled for Mac OS X > 10.7.

Download the tarball and run "make install" to copy the binary and the man page into the correct target directories:

    $> curl -OL https://bitbucket.org/ssaasen/spy/downloads/spy-osx-x86_64-v0.4.tar.gz
    $> tar xfz spy-osx-x86_64-v0.4.tar.gz
    $> cd spy
    $> make install

The user manual should now be available via `man spy` and the `spy` executable should be on your `$PATH`.


Source distribution
-------------------

To install spy from source you need the Haskell platform installed and cabal-install available on your $PATH:

    $> cabal install --only-dependencies
    $> cabal configure
    $> cabal build

This will create the spy binary in the ./dist/build/spy directory.

To copy the spy binary to the cabal bin directory (which should be available on your PATH) use:

    $> cabal copy


