%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(list(_Type),
	extends(list)).

	:- info([
		version is 1.22,
		author is 'Paulo Moura',
		date is 2017/06/29,
		comment is 'List predicates with elements constrained to a single type.',
		parnames is ['Type']
	]).

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		parameter(1, Type),
		Type::valid(Element),
		valid(List).

	check(Term) :-
		context(Context),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, Context))
		;	this(This),
			throw(error(type_error(This, Term), Context))
		).

:- end_object.
