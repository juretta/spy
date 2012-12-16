Spy
===

Spy is a compact file system watcher for Mac OS X based on hfsevent.


Usage
=====

Spy expects a single argument, the directory to watch. Spy currently supports two different modes: watch and run mode.

Watch mode
-----------

In output mode Spy will print the path to a modified file to STDOUT (followed by a newline) whenever a file modification (new file added, file modified) occurs.

    $> spy watch .
    path/to/modified/file

Because watch is the default mode you can omit the `watch` command if you want:

    $> spy .
    
The default format is the full path to the modified file followed by a newline. To make it easier to parse the output, the `--format=json` changes the output to be printed formatted as a JSON object (again followed by a newline).

    $> spy --format=json watch .
    {"path": "path/to/modified/file", "flags": ["ItemModified"], "id": 123143234}
   
TODO: explain the JSON object

Spy requires the path to the directory to watch as its first argument. An optional second argument can be used to filter the files using a glob pattern:
   
    $> spy watch /path/to/directory "*.md"
   
By default the given directory and all its sub directories will be watched. With `--recursive=false` only the files in the given directory will be watched without recursing into any sub directories.

    $> spy watch --recursive=false .

Run mode
------------

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

    $> curl -OL http://..../tar.gz
    $> tar xfz spy-1.0.tar.gz
    $> cd spy-1.0
    $> make install


Source distribution
-------------------

To install spy from source you need the Haskell platform installed and cabal-install available on your $PATH:

    $> cabal install spy


