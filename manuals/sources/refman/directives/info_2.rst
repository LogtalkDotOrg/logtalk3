..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. rst-class:: align-right

**directive**

.. index:: pair: info/2; Directive
.. _directives_info_2:

``info/2``
==========

Description
-----------

::

   info(Name/Arity, [Key is Value, ...])
   info(Name//Arity, [Key is Value, ...])

Documentation directive for predicates and grammar rule non-terminals.
The first argument is either a predicate indicator or a grammar rule
non-terminal indicator. The second argument is a list of pairs using the
format *Key is Value*. See the :ref:`documenting_predicate`
section for a description of the default keys.

Template and modes
------------------

::

   info(+predicate_indicator, +predicate_info_list)
   info(+non_terminal_indicator, +predicate_info_list)

Examples
--------

::

   :- info(empty/1, [
       comment is 'True if the argument is an empty list.',
       argnames is ['List']
   ]).
       
   :- info(sentence//0, [
       comment is 'Rewrites a sentence into a noun phrase and a verb phrase.'
   ]).

.. seealso::

   :ref:`directives_info_1`,
   :ref:`directives_mode_2`,
   :ref:`methods_predicate_property_2`
