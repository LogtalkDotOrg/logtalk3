.. _library_reader:

``reader``
==========

The ``reader`` object provides portable predicates for reading text file
and text stream contents to lists of terms, characters, or character
codes and for reading binary files to lists of bytes. The text file API
is loosely based on the SWI-Prolog ``readutil`` module.

API documentation
-----------------

Open the
`../../docs/library_index.html#reader <../../docs/library_index.html#reader>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(reader(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(reader(tester)).
