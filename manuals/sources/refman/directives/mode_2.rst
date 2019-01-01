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


.. index:: mode/2
.. _directives_mode_2:

mode/2
======

Description
-----------

::

   mode(Mode, NumberOfProofs)

Most predicates can be used with several instantiations modes. This
directive enables the specification of each instantiation mode and the
corresponding number of proofs (not necessarily distinct solutions). You
may also use this directive for documenting grammar rule non-terminals.

Template and modes
------------------

::

   mode(+predicate_mode_term, +number_of_proofs)
   mode(+non_terminal_mode_term, +number_of_proofs)

Examples
--------

::

   :- mode(atom_concat(-atom, -atom, +atom), one_or_more).
   :- mode(atom_concat(+atom, +atom, -atom), one).

   :- mode(var(@term), zero_or_one).

   :- mode(solve(+callable, -list(atom)), zero_or_one).

.. seealso::

   :ref:`directives_info_2`,
   :ref:`methods_predicate_property_2`
