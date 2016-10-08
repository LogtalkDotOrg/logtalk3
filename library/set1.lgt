%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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



:- object(set(_Type),
	extends(set)).

	:- info([
		version is 1.21,
		author is 'Paulo Moura',
		date is 2010/2/10,
		comment is 'Set predicates with elements constrained to a single type.',
		parnames is ['Type']
	]).

	valid((-)) :-				% catch variables
		!,
		fail.
	valid([]) :-
		!.
	valid([Element| Set]) :-
		check_order(Set, Element).

	check_order((-), _) :-	% catch unbound tails
		!,
		fail.
	check_order([], _) :-
		!.
	check_order([Element2| Set], Element1) :-
		parameter(1, Type),
		Type::valid(Element1),
		Type::valid(Element2),
		Element2 @> Element1,
		check_order(Set, Element2).

	check(Term) :-
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
