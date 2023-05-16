.. _library_ulid:

``ulid``
========

This library implements a Universally Unique Lexicographically Sortable
Identifier (ULID) generator.

https://github.com/ulid/spec

Note that some backends provide time stamps with lower granularity than
required (i.e. seconds but not milliseconds).

The generation of ULIDs uses the ``/dev/urandom`` random number
generator when available. This includes macOS, Linux, \*BSD, and other
POSIX operating-systems. On Windows, a pseudo-random generator is used
but randomized using the current wall time.

ULIDs can be generated as atoms, lists of characters, or lists of
character codes.

See also the ``ids`` and ``uuid`` libraries.

API documentation
-----------------

Open the
`../../docs/library_index.html#ulid <../../docs/library_index.html#ulid>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ulid(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ulid(tester)).

Generating ULIDs
----------------

By default, ULIDs are generated as atoms. For example:

::

   | ?- ulid::generate(ULID).
   ULID = '01H0J31SYQXHJZWPRAKHQ6YVYH'
   yes

To generate a ULID using a list of characters representation, use
instead the ``ulid/1`` parametric object:

::

   | ?- ulid(chars)::generate(ULID).
   ULID = ['0','1','H','0','J','3','2','Y','V','5','V','S','P','K','5','P','4','5','G','G','0','9','8','8','M','2']
   yes

Similar to get a ULID using a list of character codes representation:

::

   | ?- ulid(codes)::generate(ULID).
   ULID = [48,49,72,48,74,51,52,66,54,48,55,57,54,49,67,82,70,65,67,51,67,67,86,82,48,66]
   yes

It's also possible to generate ULIDs from a given timestamp, i.e. the
number of seconds since the Unix epoch (00:00:00 UTC on January 1,
1970). The timestamp can be either an integer or a float value. In the
later case, the fractional part allow representing milliseconds. For
example, assuming a backend Prolog system providing a ``time_stamp/1``
predicate returning the Unix epoch:

::

   | ?- time_stamp(Seconds), ulid(atom)::generate(Seconds, ULID).
   Seconds = 1684245175.344532, ULID = '01H0JDBQ1GAWJF35C44Y5S97DX'
   yes

You can also use the ``iso8601`` library to compute a ULID for a
specific date. For example:

::

   | ?- iso8601::(date(Start, 1970, 1, 1), date(End, 1999, 9, 13)),
        Seconds is (End - Start) * 86400,
        ulid(atom)::generate(Seconds, ULID).
   Start = 2440588, End = 2451435, Seconds = 937180800, ULID = '00V8T58900C2946QER73XXYW8Q'
   yes
