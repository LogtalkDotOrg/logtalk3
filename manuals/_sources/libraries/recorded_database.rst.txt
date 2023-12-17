.. _library_recorded_database:

``recorded_database``
=====================

The ``recorded_database`` library aims to help port Prolog code using
the legacy recorded database.

API documentation
-----------------

Open the
`../../docs/library_index.html#recorded_database <../../docs/library_index.html#recorded_database>`__
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
category can be imported by one of more application objects. Use
protected or private import to restrict the scope of the library
predicates. For example:

::

   :- object(foo,
       imports(private::recorded_database_core)).

       bar :-
           ^^recorda(key, value(1)),
           ...

   :- end_object.
