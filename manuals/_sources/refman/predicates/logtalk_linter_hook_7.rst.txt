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


.. index:: pair: logtalk_linter_hook/7; Built-in predicate
.. _predicates_logtalk_linter_hook_7:

``logtalk_linter_hook/7``
=========================

Description
-----------

::

   logtalk_linter_hook(Goal, Flag, File, Lines, Type, Entity, Warning)

Multifile user-defined predicate, supporting the definition of custom linter
warnings. Experimental. The ``Goal`` argument can be a message sending goal,
``Object::Message``, a call to a Prolog built-in predicate, or a call to a
module predicate, ``Module:Predicate``. The ``Flag`` argument must be a
supported linter flag. The ``Warning`` argument must be a valid ``core``
message term.


Modes and number of proofs
--------------------------

::

   logtalk_linter_hook(@callable, +atom, +atom, +pair(integer), +atom, @object_identifier, --callable) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- multifile(user::logtalk_linter_hook/7).
   % warn about using list::append/3 to construct a list from an head and a tail
   user::logtalk_linter_hook(
       list::append(L1,L2,L), suspicious_calls,
       File, Lines, Type, Entity,
       suspicious_call(File, Lines, Type, Entity, list::append(L1,L2,L), [L=[Head|L2]])
   ) :-
       nonvar(L1),
       L1 = [Head].
