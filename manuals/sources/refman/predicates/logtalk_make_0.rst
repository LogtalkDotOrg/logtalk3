..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: logtalk_make/0; Built-in predicate
.. _predicates_logtalk_make_0:

``logtalk_make/0``
==================

Description
-----------

::

   logtalk_make

Reloads all Logtalk source files that have been modified since the time
they are last loaded. Only source files loaded using the
:ref:`predicates_logtalk_load_1` and :ref:`predicates_logtalk_load_2`
predicates are reloaded. Non-modified files will
also be reloaded when there is a change to the compilation mode (i.e.
when the files were loaded without explicit :ref:`debug <flag_debug>` or
:ref:`optimize <flag_optimize>` flags and the default values of these
flags changed after loading; no check is made, however, for other implicit
compiler flags that may have changed since loading). When an included file
is modified, this predicate reloads its main file (i.e. the file that
contains the :ref:`directives_include_1` directive).

Depending on the :term:`backend Prolog compiler`, the shortcut ``{*}`` may
be used in alternative. Check the :term:`adapter files <adapter file>` for
the availability of the shortcut as it is not part of the language.

.. warning::

   Only use the ``{*}`` shortcut at the top-level interpreter and
   never in source files.

This predicate can be extended by the user by defining clauses for the
:ref:`predicates_logtalk_make_target_action_1` multifile and dynamic hook
predicate using the argument ``all``. The additional user defined actions
are run after the default one.

Modes and number of proofs
--------------------------

::

   logtalk_make - one

Errors
------

(none)

Examples
--------

::

   % reload all files modified since last loaded:
   | ?- logtalk_make.

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_1`,
   :ref:`predicates_logtalk_load_2`,
   :ref:`predicates_logtalk_make_1`,
   :ref:`predicates_logtalk_make_target_action_1`
