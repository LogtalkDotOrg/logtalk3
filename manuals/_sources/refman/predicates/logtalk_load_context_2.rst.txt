..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: logtalk_load_context/2
.. _predicates_logtalk_load_context_2:

logtalk_load_context/2
======================

Description
-----------

::

   logtalk_load_context(Key, Value)

Provides access to the Logtalk compilation/loading context. The
following keys are currently supported: ``entity_identifier``,
``entity_prefix``, ``entity_type`` (returns the value ``module`` when
compiling a module as an object), ``source``, ``file`` (the actual file
being compiled, which is different from ``source`` only when processing
an ``include/1`` directive), ``basename``, ``directory``, ``stream``,
``target`` (the full path of the intermediate Prolog file), ``flags``
(the list of the explicit flags used for the compilation of the source
file), ``term`` (the term being expanded), ``term_position``
(``StartLine-EndLine``), and ``variable_names``
(``[Name1=Variable1, ...]``). The ``term_position`` key is only
supported in back-end Prolog compilers that provide access to the start
and end lines of a read term.

The ``logtalk_load_context/2`` predicate can also be called
:ref:`directives_initialization_1`
directives in a source file. A common scenario is to use the
``directory`` key to define library aliases.

Currently, any variables in the values of the ``term`` and
``variable_names`` keys are not shared with, respectively, the term and
goal arguments of the ``term_expansion/2`` and ``goal_expansion/2``
methods.

Using the ``variable_names`` key requires calling the standard built-in
predicate ``term_variables/2`` on the term read and unifying the term
variables with the variables in the names list. This, however, may rise
portability issues with those Prolog compilers that don't return the
variables in the same order for the ``term_variables/2`` predicate and
the option ``variable_names/1`` of the ``read_term/3`` built-in
predicate, which is used by the Logtalk compiler to read source files.

Modes and number of proofs
--------------------------

::

   logtalk_load_context(?atom, -nonvar) - zero_or_more

Errors
------

(none)

Examples
--------

::

   term_expansion(Term, ExpandedTerms) :-
       ...
       logtalk_load_context(entity_identifier, Entity),
       ....

   :- initialization((
       logtalk_load_context(directory, Directory),
       assertz(logtalk_library_path(my_app, Directory))
   )).

.. seealso::

   :ref:`methods_goal_expansion_2`,
   :ref:`methods_term_expansion_2`
