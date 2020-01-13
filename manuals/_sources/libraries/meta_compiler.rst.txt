``meta_compiler``
=================

This library supports implementations optimized compilation of
meta-calls for the predicates defined in the ``meta`` library.

API documentation
-----------------

Open the
`../../docs/library_index.html#meta_compiler <../../docs/library_index.html#meta_compiler>`__
file in a web browser.

Loading
-------

To load the main entities in this library, load the ``loader.lgt``
utility file:

::

   | ?- logtalk_load(meta_compiler(loader)).

Usage
-----

To use the meta-predicates compiler, declare ``meta_compiler`` as the
default hook object or use the ``hook(meta_compiler)`` and
``optimize(on)`` complier options when compiling and loading the code
that you want to optimize.

See also the ``metapredicates_compiled`` example and unit tests.
