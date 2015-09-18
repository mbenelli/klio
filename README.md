KLIO
====

A collection of scheme tools and libraries for gambit scheme.

Klio is a collection of standard scheme libraries gathered from the net.
These libraries has been modified and reorganized in order to improve the
overall consistency and to take advantage of Gambit's specifics strongs.
It particular they fit a static compilation model, in which a single executable
is created.
If you are looking for Gambit's library, you may find something that better
fit your need on [Gambit's wiki](http://gambitscheme.org/).
The distinctive "features" of klio are:

  - it doesn't use hygienic macros, only gambit's define-macro
  - it doesn't require installation of any tool, and doesn't try to
    be portable between implementations.

Klio includes also a web server that is simple but powerful.
It's main goals are:

  - portability
  - reliable ad efficient implementation of HTTP 1.1
  - integraton with ssax-sxml for dynamic pages
  - good performance for small sites (few users) with heavy load
    (pages refreshed periodically)



Installation
------------

Running:

    $ gsc -i build.scm

from Klio's directory will build the libraries.
If you want to include/exclude some libs you must edit `build.scm`.

The file `klio/kliows.scm` contains a minimal launcher for klio web server.
There is a working progress for a scripts that build an executable.


Usage
-----

There is no search path, or autoloading or similar.
The preferred way for developing with klio is the C-like way allowed by
gambit's include and namespace.
Be aware that some "header" file (those whose name end with "#") contains
also macro definition.

For those who like "batteries-included" philosophy it should be possible to
compile all files in a single loadable object, then load it at gambit's startup
using `.gambcini` or `gambcext` (see gambit's documentation).
Be aware that header files must be included.

The function `kws#kws` starts the web-server.  A sample usage can be seen in
`klio/kliows.scm`.


License
-------

Klio is released under LLGPL, see file COPYING.
A lot of klio's contents is derived from other works.
All these works are in public domain or open sourced, see individual files for
detailed information.
A list of original authors is in file `credits.txt` on this directory.

