%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2016 Barry Evans <barryevans@kyndi.com> and
%  Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- 	set_logtalk_flag(context_switching_calls, allow).


% supporting entities

:- protocol(predicates).

	% predicates with local or inherited scope
	% directives are not dead predicates

	:- public(public_predicate/0).
	:- protected(protected_predicate/0).
	:- private(private_predicate/0).

:- end_protocol.


:- protocol(some_protocol).

	:- public(some_protocol_predicate/0).

:- end_protocol.


:- category(some_category).

	:- public(some_category_predicate/0).

:- end_category.


% testing categories


:- category(stand_alone_category).

	% predicates with local scope directives are not dead predicates

	:- public(public_predicate/0).
	public_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_category.


:- category(category,
	implements(predicates)).

	% predicates with local scope directives are not dead predicates

	:- public(category_public_predicate/0).
	category_public_predicate.

	:- protected(category_protected_predicate/0).
	category_protected_predicate.

	:- private(category_private_predicate/0).
	category_private_predicate.

	% predicates with inherited scope directives are not dead predicates

	public_predicate :-
		non_scoped_predicate_1.

	protected_predicate :-
		non_scoped_predicate_1.

	private_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_category.


% testing prototypes


:- object(stand_alone_prototype).

	% predicates with local scope directives are not dead predicates

	:- public(public_predicate/0).
	public_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.


:- object(parent,
	implements(predicates)).

:- end_object.


:- object(prototype,
	implements(some_protocol),
	imports(some_category),
	extends(parent)).

	% predicates with local scope directives are not dead predicates

	:- public(prototype_public_predicate/0).
	prototype_public_predicate.

	:- protected(prototype_protected_predicate/0).
	prototype_protected_predicate.

	:- private(prototype_private_predicate/0).
	prototype_private_predicate.

	% predicates with inherited scope directives are not dead predicates

	some_protocol_predicate :-
		non_scoped_predicate_1.

	some_category_predicate :-
		non_scoped_predicate_1.

	public_predicate :-
		non_scoped_predicate_1.

	protected_predicate :-
		non_scoped_predicate_1.

	private_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.


% testing classes and instances


:- object(stand_alone_class,
	instantiates(stand_alone_class)).

	% predicates with local scope directives are not dead predicates

	:- public(public_predicate/0).
	public_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.


:- object(metaclass,
	instantiates(metaclass),
	implements(predicates)).

	:- public(metaclass_public_predicate/0).

:- end_object.


:- object(class,
	implements(some_protocol),
	imports(some_category),
	instantiates(metaclass)).

	% predicates with local scope directives are not dead predicates

	:- public(class_public_predicate/0).
	class_public_predicate.

	:- protected(class_protected_predicate/0).
	class_protected_predicate.

	:- private(class_private_predicate/0).
	class_private_predicate.

	% predicates with inherited scope directives are not dead predicates

	some_protocol_predicate :-
		non_scoped_predicate_1.

	some_category_predicate :-
		non_scoped_predicate_1.

	metaclass_public_predicate :-
		non_scoped_predicate_1.

	public_predicate :-
		non_scoped_predicate_1.

	protected_predicate :-
		non_scoped_predicate_1.

	private_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.


:- object(subclass_with_metaclass,
	implements(some_protocol),
	imports(some_category),
	instantiates(metaclass),
	specializes(class)).

	% predicates with local scope directives are not dead predicates

	:- public(subclass_public_predicate/0).
	subclass_public_predicate.

	:- protected(subclass_protected_predicate/0).
	subclass_protected_predicate.

	:- private(subclass_private_predicate/0).
	subclass_private_predicate.

	% predicates with inherited scope directives are not dead predicates

	some_protocol_predicate :-
		non_scoped_predicate_1.

	some_category_predicate :-
		non_scoped_predicate_1.

	metaclass_public_predicate :-
		non_scoped_predicate_1.

	class_public_predicate :-
		non_scoped_predicate_1.

	class_protected_predicate :-
		non_scoped_predicate_1.

	class_private_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.


:- object(subclass,
	implements(some_protocol),
	imports(some_category),
	specializes(class)).

	% predicates with local scope directives are not dead predicates

	:- public(subclass_public_predicate/0).
	subclass_public_predicate.

	:- protected(subclass_protected_predicate/0).
	subclass_protected_predicate.

	:- private(subclass_private_predicate/0).
	subclass_private_predicate.

	% predicates with inherited scope directives are not dead predicates

	some_protocol_predicate :-
		non_scoped_predicate_1.

	some_category_predicate :-
		non_scoped_predicate_1.

	class_public_predicate :-
		non_scoped_predicate_1.

	class_protected_predicate :-
		non_scoped_predicate_1.

	class_private_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.


:- object(instance,
	implements(some_protocol),
	imports(some_category),
	instantiates(subclass)).

	% predicates with inherited scope directives are not dead predicates

	some_protocol_predicate :-
		non_scoped_predicate_1.

	some_category_predicate :-
		non_scoped_predicate_1.

	subclass_public_predicate :-
		non_scoped_predicate_1.

	subclass_protected_predicate :-
		non_scoped_predicate_1.

	subclass_private_predicate :-
		non_scoped_predicate_1.

	% non-scoped predicates called, directly or indirectly, by
	% predicates with local or inherited scoped directives are
	% not dead predicates

	non_scoped_predicate_1 :-
		non_scoped_predicate_2.

	non_scoped_predicate_2.

	% non-scoped predicates not called, directly or indirectly,
	% by predicates with local or inherited scoped directives
	% are dead predicates

	dead_non_terminal --> [].

	dead_predicate.

	dead_predicate_1 :-
		dead_predicate_2.

	dead_predicate_2 :-
		dead_predicate_3.

	dead_predicate_3.

:- end_object.
