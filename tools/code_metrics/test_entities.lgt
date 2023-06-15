%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
%  SPDX-License-Identifier: Apache-2.0
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


:- protocol(prot_a).

:- end_protocol.


:- protocol(prot_b,
	extends(prot_a)).

:- end_protocol.


:- category(cat_a,
	implements(prot_a)).

:- end_category.


:- object(obj_e).

	:- public([foo/0, fact/1]).
	:- dynamic(fact/1).

	foo :-
		true.

:- end_object.


:- object(obj_d).

	:- public([foo/0, bar/0]).

	bar :-
		true.

	foo :-
		obj_e::foo.

:- end_object.


:- object(obj_a,
	imports(cat_a)).

	:- public([foo/0, bar/0]).

	foo :-
		obj_d::bar.

	bar :-
		obj_d::foo,
		obj_e::foo.

	baz(_) :-
		foo.

	baz(_).

:- end_object.


:- object(obj_b,
	extends(obj_a)).

	:- public(foo/0).

	foo :-
		obj_e::assertz(fact(1)).

:- end_object.


:- object(obj_c,
	extends([obj_a, obj_b])).

	:- public(fact/1).
	:- dynamic(fact/1).

	foo :-
		true.

:- end_object.


:- category(cat_b,
	extends(cat_a),
	implements(prot_b)).

	foo :-
		::true.

:- end_category.


:- category(cat_d).

:- end_category.


:- category(cat_c,
	extends(cat_d)).

	foo :-
		obj_b::foo.

:- end_category.


:- object(bird,
	instantiates(bird)).

:- end_object.


:- object(herring,
	specializes(bird)).

:- end_object.


:- object(meta_vehicle,
	instantiates(meta_vehicle)).

:- end_object.


:- object(vehicle,
	instantiates(meta_vehicle)).

:- end_object.


:- object(car,
	instantiates(vehicle)).

:- end_object.


% avoid a warning with the next entity definitions
:- set_logtalk_flag(unknown_entities, silent).


:- object(class,
	instantiates(class),
	specializes(abstract_class)).

:- end_object.


:- object(object,
	instantiates(class)).

:- end_object.


:- object(abstract_class,
	instantiates(class),
	specializes(object)).

:- end_object.


% Halstead metric test entities

:- protocol(h_ptc).

	:- public(foo/1).
	:- public(bar/2).

	% two distinct predicates with a total of three arguments => Pn = 2; PAn = 3
	% no clauses => Cn = 0; CAn = 0

:- end_protocol.


:- category(h_ctg).

	:- public(foo/1).

	foo(X) :-
		bar(X).

	bar(1).
	bar(2).
	bar(3).

	% two distinct predicates, both with a single argument => Pn = 2; PAn = 2
	% one predicate call + four clauses => Cn = 5
	% one predicate call argument + four clause head arguments => CAn = 5

:- end_category.


:- object(h_obj,
	imports(h_ctg)).

	:- public(baz/1).

	baz(X) :-
		^^foo(X).

	local(F) :-
		logtalk::loaded_file(F).

	% four distinct predicates, all with a single argument => Pn = 4; PAn = 4
	% two predicate calls + two clauses => Cn = 4
	% two predicate call arguments + two clause head arguments => CAn = 4

:- end_object.
