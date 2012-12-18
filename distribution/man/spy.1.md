% SPY(1) Spy User Manuals
% Stefan Saasen
% December, 2012

# NAME

spy - file system event watcher for Mac OS X

# SYNOPSIS

spy [*COMMAND*] ... [*OPTIONS*]

# DESCRIPTION

Spy is a compact file system watcher for Mac OS X using the File System Events API via hfsevents.

# OPTIONS

spy `watch` supports the following options:

-i, \--hidden=*true|false*
:   If set to `false` (the default) hidden directories/files won't be added to the output. Set to `true` to include hidden files/directories.

-f, \--format=*json|plain*
:   If set to `plain` (the default) spy prints the path to the file that was changed, followed by a newline. If set to `json` the output will be formatted as a JSON object with the following fields:

* path: The path to the file that was changed
* flags: List of flags that indicate what kind of change event occured (e.g. ItemCreated, ItemModified, ItemRemoved)
* id: The id of the change event

spy `run` supports the following options:

-i, \--hidden=*true|false*
:   If set to `false` (the default) hidden directories/files won't be added to the output. Set to `true` to include hidden files/directories.

\--notify-only=*true|false*
:   If set to `false` (the default) spy will pass the path to the modified file to the command that will be executed. To execute the given command without passing the path, set this option to `true`.


# SEE ALSO

The *README.md* file distributed with spy contains usage information.

The spy source code and all documentation may be downloaded from
<https://bitbucket.org/ssaasen/spy>.

