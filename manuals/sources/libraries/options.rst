.. _options:

``options``
===========

This library provides useful predicates for managing developer tool and
application options.

API documentation
-----------------

Open the
`../../docs/library_index.html#options <../../docs/library_index.html#options>`__
file in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(options(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(options(tester)).

Usage
-----

The ``options`` category is usually imported by the root object of the
developer tool or application. The importing object should define the
``default_option/1`` predicate and, if option type-checking is required,
the ``valid_option/1`` predicate must be defined for each option. This
library requires options to be represented by compound terms where the
functor is the option name (e.g. ``trim(true)`` or (``box(0,2)``)). The
``option/2-3`` can be used to get or test an option given a list of
options. When an option appears multiple times in a list, the
``option/2-3`` predicates get or test the first (leftmost) occurrence.

The library also supports a user-defined ``fix_option/2`` predicate. An
usage example is when an option value can be a relative file path that
should be expanded before used. Another usage example would be
converting from a user-friendly option to a form more suitable for
internal processing. When a call to the ``fix_option/2`` predicate
fails, the option is used as-is.

A simple example:

::

   :- object(foo,
       imports(options)).

       :- uses(type, [
           valid/2
       ]).

       :- public(p/0).
       p :-
           % use default options
           p([]).

       :- public(p/1).
       p(UserOptions) :-
           ^^check_options(UserOptions),
           % construct the full set of options from
           % the user options and the default options
           ^^merge_options(UserOptions, Options),
           ...
           % query an option
           ^^option(baz(Boolean), Options),
           q(Boolean),
           ...

       default_option(baz(true)).
       ...

       valid_option(baz(Boolean)) :-
           valid(boolean, Boolean).
       ...

   :- end_object.

Note that you can use protected or private import of the ``options``
category if you don't want to add its public predicates to the object
protocol.
