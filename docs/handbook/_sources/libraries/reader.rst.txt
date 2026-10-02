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
`../../apis/library_index.html#reader <../../apis/library_index.html#reader>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(reader(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(reader(tester)).
