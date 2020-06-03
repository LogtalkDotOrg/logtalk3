..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: logtalk_linter_hook/8; Built-in predicate
.. _predicates_logtalk_linter_hook_8:

logtalk_linter_hook/8
=====================

Description
-----------

::

   logtalk_linter_hook(Object, Message, Flag, File, Lines, Type, Entity, Warning)

Multifile user-defined predicate, supporting the definition of linter warnings
for object predicates. Experimental. The ``Flag`` argument must be a supported
linter flag. The ``Warning`` argument must be a valid ``core`` message term.


Modes and number of proofs
--------------------------

::

   logtalk_linter_hook(@object_identifier, @callable, +atom, +atom, +pair(integer), +atom, @object_identifier, --callable) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- multifile(user::logtalk_linter_hook/8).
   user::logtalk_linter_hook(
       list, append(L1,L2,L), suspicious_calls,
       File, Lines, Type, Entity,
       suspicious_call(File, Lines, Type, Entity, list::append(L1,L2,L), [L = [X| L2]])
   ) :-
       nonvar(L1),
       L1 = [X| Tail],
       Tail == [].

