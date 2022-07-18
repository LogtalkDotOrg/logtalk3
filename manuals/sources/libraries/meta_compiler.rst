.. _library_meta_compiler:

``meta_compiler``
=================

This library supports implementations optimized compilation of
meta-calls for the predicates defined in the ``meta`` library.

API documentation
-----------------

Open the
`../../docs/library_index.html#meta-compiler <../../docs/library_index.html#meta-compiler>`__
link in a web browser.

Loading
-------

To load the main entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(meta_compiler(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(meta_compiler(tester)).

Usage
-----

If ``meta_compiler`` is the only hook object you are using, you can set
it as the default hook object (but note that the optimizations are only
applied to entities compiled with the ``optimize`` flag turned on):

::

   | ?- set_logtalk_flag(hook, meta_compiler).
   ...

Otherwise, use the ``hook(meta_compiler)`` and ``optimize(on)`` complier
options when compiling and loading the code that you want to optimize.
For example:

::

   | ?- logtalk_load(my_source_file, [hook(meta_compiler), optimize(on)]).
   ...

See also the ``metapredicates_compiled`` example and unit tests.
