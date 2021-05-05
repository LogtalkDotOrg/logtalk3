%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(parstructs_hook,
	implements(expanding)).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2018-08-16,
		comment is 'Access to an object (or category) first parameter as an ECLiPSe structure.'
	]).

	term_expansion((:- object(Identifier)), [(:- object(Identifier)), export(struct(Struct))]) :-
		Identifier =.. [_, Struct].

	% the goal expansion results are inlined when compiling
	% the source files expanded using this hook object in
	% optimize mode

	goal_expansion(parameter_create(_), true).
	goal_expansion(get_parameter(Key, Value), arg(Key of StructName, Struct, Value)) :-
		logtalk_load_context(entity_identifier, Identifier),
		Identifier =.. [_, Struct],
		functor(Struct, StructName, _).
%	goal_expansion(
%		b_set_parameter(Key, Value),
%		(parameter(1, Struct), b_set_dict(Key, Dict, Value))
%	).
%	goal_expansion(
%		nb_set_parameter(Key, Value),
%		(parameter(1, Struct), nb_set_dict(Key, Dict, Value))
%	).

:- end_object.
