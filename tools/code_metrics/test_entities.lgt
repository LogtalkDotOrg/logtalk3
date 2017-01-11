%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
%                 Paulo Moura         <pmoura@logtalk.org>
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

	:- dynamic(fact/1).
	:- public([foo/0, fact/1]).

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
	extends(obj_b)).

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

:- object(bird).

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
