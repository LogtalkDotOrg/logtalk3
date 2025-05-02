.. _library_recorded_database:

``recorded_database``
=====================

The ``recorded_database`` library aims to help port Prolog code using
the legacy recorded database. Ported applications should still consider
migrating to more standard solutions to handle dynamic data that must
survive backtracking.

API documentation
-----------------

Open the
`../../apis/library_index.html#recorded_database <../../apis/library_index.html#recorded_database>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(recorded_database(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(recorded_database(tester)).

Usage
-----

The ``recorded_database_core`` category implements the library
predicates. This category is imported by the default
``recorded_database`` object to provide application global database. To
make the database local and thus minimize potential record clashes, the
category can be imported by one or more application objects. Use
protected or private import to restrict the scope of the library
predicates. For example:

::

   :- object(foo,
       imports(private::recorded_database_core)).

       bar :-
           ^^recorda(key, value(1)),
           ...

   :- end_object.

Known issues
------------

Currently, references are non-negative integers. They still must be
regarded as opaque terms and subject to change without notice. But using
integers can result in integer overflows when running on backends with
bounded integers in applications performing a large number of database
updates.
