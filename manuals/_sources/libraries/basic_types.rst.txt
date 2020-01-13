``basic_types``
===============

The ``basic_types`` library is a virtual library that loads only basic
types from the ``types`` library: ``termp``,
term\ ``,``\ atomic\ ``,``\ atom\ ``,``\ number\ ``,``\ float\ ``,``\ integer\ ``,``\ compound\ ``,``\ listp\ ``,``\ list\ ``, and``\ type`.

API documentation
-----------------

Open the
`../../docs/library_index.html#types <../../docs/library_index.html#types>`__
file in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(basic_types(loader)).

