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



:- object(term,
	implements(termp)).

	:- info([
		version is 1.7,
		author is 'Paulo Moura',
		date is 2014/04/28,
		comment is 'Prolog term utility predicates.'
	]).

	:- alias(termp, [variables/2 as vars/2]).

	depth(Term, Depth) :-
		depth(Term, 0, 0, Depth).

	depth(Var, Acc, MaxSoFar, Depth) :-
		var(Var),
		!,
		(	Acc > MaxSoFar ->
			Depth = Acc
		;	Depth = MaxSoFar
		).
	depth(Atomic, Acc, MaxSoFar, Depth) :-
		atomic(Atomic),
		!,
		(	Acc > MaxSoFar ->
			Depth = Acc
		;	Depth = MaxSoFar
		).
	depth([Arg| Args], Acc, MaxSoFar, Depth) :-
		!,
		depth(Arg, Acc, MaxSoFar, ArgDepth),
		depth(Args, Acc, ArgDepth, Depth).
	depth(Term, Acc, MaxSoFar, Depth) :-
		Acc2 is Acc + 1,
		Term =.. [_| Args],
		depth(Args, Acc2, MaxSoFar, Depth).

	ground(Term) :-
		{ground(Term)}.

	occurs(Var, Term) :-
		(	var(Term) ->
			Var == Term
		;	functor(Term, _, Arity),
			occurs(Arity, Var, Term)
		).

	occurs(N, Var, Term) :-
		compound(Term),
		arg(N, Term, Arg),
		occurs(Var, Arg),
		!.
	occurs(N, Var, Term) :-
		N > 1,
		N2 is N - 1,
		occurs(N2, Var, Term).

	subsumes(General, Specific) :-
		{subsumes_term(General, Specific)}.

	var_member_chk(Var, [Head| Tail]) :-
		(	Var == Head ->
			true
		;	var_member_chk(Var, Tail)
		).

	subterm(Term, Term).
	subterm(Sub, Term) :-
		nonvar(Term),
		functor(Term, _, N),
		subterm(N, Sub, Term).

	subterm(N, Sub, Term) :-
		compound(Term),
		arg(N, Term, Arg),
		subterm(Sub, Arg).
	subterm(N, Sub, Term) :-
		N > 1,
		M is N-1,
		subterm(M, Sub, Term).

	valid(_).

	check(_).

	variant(Term1, Term2) :-
		\+ \+ {subsumes_term(Term1, Term2)},
		\+ \+ {subsumes_term(Term2, Term1)}.

	vars(Term, Vars) :-			% deprecated
		{term_variables(Term, Vars)}.

	variables(Term, Vars) :-
		{term_variables(Term, Vars)}.

	singletons(Term, Singletons) :-
		term_to_vars(Term, [], Vars),
		vars_to_singletons(Vars, [], [], Singletons).

	term_to_vars(Term, Acc, Vars) :-
		(	var(Term) ->
			Vars = [Term| Acc]
		;	Term =.. [_| Args],
			term_to_vars_list(Args, Acc, Vars)
		).

	term_to_vars_list([], Vars, Vars).
	term_to_vars_list([Term| Terms], Acc, Vars) :-
		term_to_vars(Term, Acc, Acc2),
		term_to_vars_list(Terms, Acc2, Vars).

	vars_to_singletons([], _, Singletons, Singletons).
	vars_to_singletons([Var| Vars], Repeated, Acc, Singletons) :-
		(	var_member_chk(Var, Repeated) ->
			Repeated2 = Repeated,
			Acc2 = Acc
		;	var_member_chk(Var, Vars) ->
			Repeated2 = [Var| Repeated],
			Acc2 = Acc
		;	Repeated = Repeated2,
			Acc2 = [Var| Acc]
		),
		vars_to_singletons(Vars, Repeated2, Acc2, Singletons).

:- end_object.
