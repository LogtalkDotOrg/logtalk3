.. _grammars:

``grammars``
============

This library provides Definite Clause Grammars (DCGs) for common parsing
tasks. The DCGs support parsing both lists of characters (aka chars) and
lists of character codes (aka codes).

Currently, three groups of DCGs are available, each defined in its own
file:

-  blanks (``blank_grammars.lgt``)
-  numbers (``number_grammars.lgt``)
-  IP addresses (``ip_grammars.lgt``; depends on
   ``number_grammars.lgt``)

API documentation
-----------------

Open the
`../../docs/library_index.html#grammars <../../docs/library_index.html#grammars>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(grammars(loader)).

Testing
-------

Minimal tests for this library predicates can be run by loading the
``tester.lgt`` file:

::

   | ?- logtalk_load(grammars(tester)).

Usage
-----

The library uses parametric objects where the single parameter can be
either ``chars`` or ``codes``. The parameter must be bound when using
the DCGs. For example, when using implicit message sending:

::

   :- uses(blank_grammars(chars), [
       white_spaces//0, new_lines//0
   ]).
