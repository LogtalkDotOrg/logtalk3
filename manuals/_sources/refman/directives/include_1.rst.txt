..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


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
path, an absolute path, or using :term:`library notation` and is expanded
as a source file name. Relative paths are interpreted as relative to the
path of the file containing the directive.

When using the :ref:`reflection API <reflection_reflection>`, predicates
from an included file can be distinguished from predicates from the main
file by looking for the ``include/1`` predicate declaration or predicate
definition property. For the included predicates, the ``line_count/1``
property stores the term line number in the included file.

This directive can be used as either a source file directive or an
entity directive. As an entity directive, it can be used both in
entities defined in source files and with the entity creation built-in
predicates. In the latter case, the file should be specified using an
absolute path or using library notation (which expands to a full path).

.. warning::

   When using this directive as an argument in calls to the
   :ref:`predicates_create_object_4` and :ref:`predicates_create_category_4`
   predicates, the objects and categories will not be recreated or redefined
   when the included file(s) are modified and the :ref:`predicates_logtalk_make_0`
   predicate or the :ref:`predicates_logtalk_make_1` (with target ``all``)
   predicates are called.

Template and modes
------------------

::

   include(@source_file_name)

Examples
--------

::

   % include the "raw_1.txt" text file found
   % on the "data" library directory:
   :- include(data('raw_1.txt')).

   % include a "factbase.pl" file in the
   % current directory:
   :- include('factbase.pl').

   % include a file given its absolute path:
   :- include('/home/me/databases/countries.pl').

   % create a wrapper object for a Prolog file:
   | ?- create_object(cities, [], [public(city/4), include('cities.pl')], []).
