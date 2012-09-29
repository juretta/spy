Spy
===

Spy is a compact file system watcher for Mac OS X based on hfsevent.


Usage
=====

Spy expects a single argument, the directory to watch. Spy will print the path
to a modified file to STDOUT (followed by a newline) whenever a file
modification (new file added, file modified) happened.

    $> spy .
    path/to/modified/file

    $> spy --include-deletions .

    $> spy --format=json .
    {"path": "path/to/modified/file", "flags": ["ItemModified"], "id": 123143234}
   
    $> spy /path/to/directory "*.md"

    $> spy --recursive=false .

    $> spy . --silent --cmd="cabal build"
