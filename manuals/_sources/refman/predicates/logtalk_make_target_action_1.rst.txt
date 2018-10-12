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


.. index:: logtalk_make_target_action/1
.. _predicates_logtalk_make_target_action_1:

logtalk_make_target_action/1
============================

Description
-----------

::

   logtalk_make_target_action(Target)

Multifile and dynamic hook predicate that allows defining user actions
for the ``logtalk_make/1`` targets. The user defined actions are run
after the default ones using a failure driven loop.

Template and modes
------------------

::

   logtalk_make_target_action(+atom)

Errors
------

(none)

Examples
--------

::

   % integrate the dead_code_scanner tool with logtalk_make/1

   :- multifile(logtalk_make_target_action/1).
   :- dynamic(logtalk_make_target_action/1).

   logtalk_make_target_action(check) :-
       dead_code_scanner::all.

.. seealso::

   :ref:`predicates_logtalk_make_1`,
   :ref:`predicates_logtalk_make_0`
