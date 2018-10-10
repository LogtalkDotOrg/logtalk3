
.. index:: include/1
.. _directives_include_1:

include/1
=========

Description
-----------

::

   include(File)

Includes a file contents, which must be valid terms, at the place of
occurrence of the directive. The file can be specified as a relative
path, an absolute path, or using library notation and is expanded as a
source file name. Relative paths are interpreted as relative to the path
of the file containing the directive.

When using the reflection API, predicates from an included file can be
distinguished from predicates from the main file by looking for the
``include/1`` predicate declaration or definition property. For the
included predicates, the ``line_count/1`` property stores the term line
number in the included file.

This directive can be used as either a source file directive or an
entity directive. As an entity directive, it can be used both in
entities defined in source files and with the entity creation built-in
predicates.

Note that when using this directive as an argument in a call to the
:ref:`predicates_create_object_4` and :ref:`predicates_create_category_4`
predicates, the objects will not be recreated or redefined when the
included file(s) are modified and the :ref:`predicates_logtalk_make_0`
predicate or the :ref:`predicates_logtalk_make_1` predicate (with target
``all``) are called.

Template and modes
------------------

::

   include(@source_file_name)

Examples
--------

::

   :- include(data('raw_1.txt')).

   :- include('factbase.pl').

   :- include('/home/me/databases/cities.pl').

   | ?- create_object(cities, [], [public(city/4), include('/home/me/dbs/cities.pl')], []).
