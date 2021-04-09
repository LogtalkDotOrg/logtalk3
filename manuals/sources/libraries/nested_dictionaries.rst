``nested_dictionaries``
=======================

This library provides nested dictionary implementations based on private
extensions to the ``dictionaries`` library objects. The representations
of a nested dictionary should be regarded as opaque terms and only
accessed using the library predicates.

This library is experimental, a work in progress, and future versions
can introduce incompatible changes.

API documentation
-----------------

Open the
`../../docs/library_index.html#nested_dictionaries <../../docs/library_index.html#nested_dictionaries>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(nested_dictionaries(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(nested_dictionaries(tester)).

Usage
-----

First, select the nested dictionary implementation that you want to use.
For cases where the number of elements is relatively small and
performance is not critical, ``nbintree`` can be a good choice. For
other cases, ``navltree`` or ``nrbtree`` are likely better choices. If
you want to compare the performance of the implementations, either
define an object alias or use an ``uses/2`` directive so that you can
switch between implementations by simply changing the alias definition
or the first argument of the directive. Note that you can switch between
implementations at runtime without code changes by using a parameter
variable in the first argument of an ``uses/2`` directive.

To create an empty nested dictionary, you can use the ``new/1``
predicate. For example:

::

   | ?- navltree::new(Dictionary).
   Dictionary = ...
   yes

You can also create a new nested dictionary from a *curly bracketed term
representation* (see below) by using the predicate
``as_nested_dictionary/2``. For example:

::

   | ?- navltree::as_nested_dictionary(
           {a-1, b-{c-3, d-{e-7,f-8}}},
           Dictionary
        ).

   Dictionary = ...
   yes

Several predicates are provided to insert, lookup, update, and delete
key-value pairs given a list of keys interpreted as an *access path* to
a nested dictionary. For example:

::

   | ?- navltree::as_nested_dictionary(
           {a-1, b-{c-3, d-{e-7,f-8}}},
           Dictionary
        ),
        navltree::lookup_in([b,d,f], Value, Dictionary).

   Dictionary = ...
   Value = 8
   yes

For details on these and other provided predicates, consult the library
API documentation.

Curly term representation
-------------------------

To simplify importing and exporting data into a nested dictionary, the
library provides ``as_nested_dictionary/2`` and ``as_curly_bracketed/2``
predicates that work with a *curly term representation*. This format is
based on the JSON data interchange format.

A dictionary is represented by the ``{Pairs}`` term where Pairs is a
conjunction of ``Key-Value`` or ``Key:Value`` pairs and ``Value`` can be
a nested dictionary or lists of pairs. An empty dictionary is
represented using the ``{}`` term.
