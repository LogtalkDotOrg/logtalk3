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



:- object(term,
	implements(termp)).

	:- info([
		version is 1:9:0,
		author is 'Paulo Moura',
		date is 2021-03-25,
		comment is 'Term utility predicates.'
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
		subsumes_term(General, Specific).

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
		\+ \+ subsumes_term(Term1, Term2),
		\+ \+ subsumes_term(Term2, Term1).

	vars(Term, Vars) :-			% deprecated
		term_variables(Term, Vars).

	variables(Term, Vars) :-
		term_variables(Term, Vars).

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

	% the backend adapter files ensure that the de facto standard
	% numbervars/3 predicate is avaialble in "user"; the definitions
	% here are inlined by the compiler and thus can be called with
	% no overhead compared with a direct call to the predicate

	numbervars(Term, From, Next) :-
		{numbervars(Term, From, Next)}.

	numbervars(Term) :-
		{numbervars(Term, 0, _)}.

	varnumbers(Term, From, Copy) :-
		varnumbers(Term, From, [], _, Copy).

	varnumbers(Term, _, Pairs, Pairs, Copy) :-
		var(Term),
		!,
		Copy = Term.
	varnumbers('$VAR'(N), From, Pairs0, Pairs, Var) :-
		N >= From,
		!,
		(	member(N-Var, Pairs0) ->
			Pairs = Pairs0
		;	Pairs = [N-Var| Pairs0]
		).
	varnumbers('$VAR'(N), _, Pairs, Pairs, '$VAR'(N)) :-
		!.
	varnumbers(Term, From, Pairs0, Pairs, Copy) :-
		Term =.. [Name| Arguments],
		varnumbers_list(Arguments, From, Pairs0, Pairs, CopyArguments),
		Copy =.. [Name| CopyArguments].

	varnumbers_list([], _, Pairs, Pairs, []).
	varnumbers_list([Argument| Arguments], From, Pairs0, Pairs, [Copy| Copies]) :-
		varnumbers(Argument, From, Pairs0, Pairs1, Copy),
		varnumbers_list(Arguments, From, Pairs1, Pairs, Copies).

	varnumbers(Term, Copy) :-
		varnumbers(Term, 0, Copy).

	% auxiliary predicates (as this is a root object
	% we avoid a dependency on descendant objects)

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

	var_member_chk(Var, [Head| Tail]) :-
		(	Var == Head ->
			true
		;	var_member_chk(Var, Tail)
		).

:- end_object.
