..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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

.. index:: pair: mode_non_terminal/2; Directive
.. _directives_mode_non_terminal_2:

``mode_non_terminal/2``
=======================

Description
-----------

::

   mode_non_terminal(Mode, NumberOfProofs)

Most non-terminals can be used with several instantiations modes. This
directive enables the specification of each
:ref:`instantiation mode <predicates_mode_instantiation>` and the
corresponding :ref:`number of proofs <predicates_mode_number_of_proofs>`
(but not necessarily distinct solutions). The instantiation mode of
each argument can include type information.

Template and modes
------------------

::

   mode_non_terminal(+predicate_mode_term, +number_of_proofs)
   mode_non_terminal(+non_terminal_mode_term, +number_of_proofs)

Examples
--------

::

   :- mode_non_terminal(zero_or_more(-list(atomic)), one).

.. seealso::

   :ref:`directives_mode_2`,
   :ref:`directives_info_2`,
   :ref:`methods_predicate_property_2`
