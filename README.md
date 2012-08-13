# filesystem-trie

A Unix filesystem-based implementation of Edward Fredkin's trie data structure.

Data is stored, indexed by an arbitrary key (a GUUID) which is created by
this library and returned to the caller.  The caller may use that key to
retrieve or delete the data.  No updates are permitted so the data may be treated
as immutable.

The implementation is entirely in the Unix filesystem.

cf. http://en.wikipedia.org/wiki/Trie


## Usage

FIXME

## License

Copyright © 2012 Craig Brent Ludington

Distributed under the Eclipse Public License, the same as Clojure.
