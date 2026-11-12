%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(problog_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'Opt-in expansion of native ProbLog facts, rules, queries, evidence, and directives.'
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	:- uses(type, [
		valid/2
	]).

	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_relation, implements_protocol(_, problog_problem_protocol, _)),
		\+ structural_directive(Term),
		!,
		(	logtalk_load_context(variable_names(Term), Names) ->
			true
		;	Names = []
		),
		(	statement(Term, Names, Statement) ->
			problog_program::serialize([Statement], _),
			origin(Term, Origin),
			Expanded = problog_statement(Statement, Origin)
		;	domain_error(problog_syntax, Term)
		).

	structural_directive((:- end_object)).
	structural_directive((:- info(_))).
	structural_directive((:- set_logtalk_flag(_, _))).

	origin(Term, Origin) :-
		(	logtalk_load_context(file, File),
			logtalk_load_context(term_position(Term), Start-End),
			Start > 0 ->
			Origin = source(File, Start, End)
		;	Origin = unknown
		).

	statement((:- query(Goal)), Names, query(GoalTerm)) :-
		!, goal(Goal, Names, GoalTerm).
	statement((:- evidence(Goal)), Names, evidence(GoalTerm, true)) :-
		!, goal(Goal, Names, GoalTerm).
	statement((:- evidence(Goal, Boolean)), Names, evidence(GoalTerm, Boolean)) :-
		!, boolean(Boolean), goal(Goal, Names, GoalTerm).
	statement((:- raw(Source)), _, raw(Codes)) :-
		!, normalize_text(Source, Codes).
	statement((:- comment(Source)), _, comment(Codes)) :-
		!, normalize_text(Source, Codes).
	statement((:- directive(Goal)), Names, directive(GoalTerm)) :-
		!, goal(Goal, Names, GoalTerm).
	statement((:- Goal), Names, directive(GoalTerm)) :-
		supported_directive(Goal),
		!, goal(Goal, Names, GoalTerm).
	statement((Head :- Body), Names, Statement) :-
		!,
		(	annotated_heads(Head, Names, Alternatives) ->
			goal(Body, Names, BodyTerm),
			Statement = annotated_disjunction(Alternatives, BodyTerm)
		;	probabilistic_head(Head, Probability, HeadTerm) ->
			probability(Probability),
			positive_atom(HeadTerm, Names, ProblogHead),
			goal(Body, Names, BodyTerm),
			Statement = probabilistic_rule(Probability, ProblogHead, BodyTerm)
		;	positive_atom(Head, Names, ProblogHead),
			goal(Body, Names, BodyTerm),
			Statement = rule(ProblogHead, BodyTerm)
		).
	statement((Left ; Right), Names, annotated_disjunction(Alternatives, true)) :-
		!, annotated_heads((Left ; Right), Names, Alternatives).
	statement(Head, Names, probabilistic_fact(Probability, ProblogHead)) :-
		probabilistic_head(Head, Probability, PlainHead),
		!, probability(Probability), positive_atom(PlainHead, Names, ProblogHead).
	statement(Head, Names, fact(ProblogHead)) :-
		positive_atom(Head, Names, ProblogHead).

	annotated_heads((Left ; Right), Names, Alternatives) :-
		!,
		annotated_alternative(Left, Names, First),
		annotated_alternatives(Right, Names, Rest),
		Alternatives = [First| Rest].
	annotated_heads(Alternative, Names, [Pair]) :-
		annotated_alternative(Alternative, Names, Pair).

	annotated_alternatives((Left ; Right), Names, [Pair| Pairs]) :-
		!,
		annotated_alternative(Left, Names, Pair),
		annotated_alternatives(Right, Names, Pairs).
	annotated_alternatives(Alternative, Names, [Pair]) :-
		annotated_alternative(Alternative, Names, Pair).

	annotated_alternative(Alternative, Names, Probability-ProblogHead) :-
		probabilistic_head(Alternative, Probability, Head),
		probability(Probability),
		positive_atom(Head, Names, ProblogHead).

	probabilistic_head(Head, Probability, PlainHead) :-
		nonvar(Head),
		functor(Head, '::', 2),
		arg(1, Head, Probability),
		arg(2, Head, PlainHead).

	supported_directive(use_module(_)).

	probability(Probability) :-
		number(Probability),
		Probability >= 0,
		Probability =< 1.

	boolean(true).
	boolean(false).

	normalize_text(Source, Codes) :-
		(	atom(Source) ->
			atom_codes(Source, Codes)
		;	valid(chars, Source) ->
			atom_chars(Atom, Source),
			atom_codes(Atom, Codes)
		;	{'$lgt_string'(Source)} ->
			{'$lgt_string_codes'(Source, Codes)}
		;	valid(codes, Source) ->
			Codes = Source
		;	domain_error(problog_text, Source)
		).

	goal(true, _, true) :-
		!.
	goal((Left, Right), Names, conjunction(Goals)) :-
		!,
		goal_list((Left, Right), Names, Goals).
	goal((Left ; Right), Names, disjunction([LeftTerm, RightTerm])) :-
		!,
		goal(Left, Names, LeftTerm),
		goal(Right, Names, RightTerm).
	goal(not(Goal), Names, negation(GoalTerm)) :-
		!, goal(Goal, Names, GoalTerm).
	goal(\+(Goal), Names, negation(GoalTerm)) :-
		!, goal(Goal, Names, GoalTerm).
	goal(Goal, Names, comparison(Operator, LeftTerm, RightTerm)) :-
		nonvar(Goal),
		Goal =.. [Functor, Left, Right],
		comparison(Functor, Operator),
		!,
		term(Left, Names, LeftTerm),
		term(Right, Names, RightTerm).
	goal(Goal, Names, ProblogGoal) :-
		positive_atom(Goal, Names, ProblogGoal).

	goal_list((Left, Right), Names, Goals) :-
		!,
		goal_list(Left, Names, LeftGoals),
		goal_list(Right, Names, RightGoals),
		append(LeftGoals, RightGoals, Goals).
	goal_list(Goal, Names, [GoalTerm]) :-
		goal(Goal, Names, GoalTerm).

	comparison('=', '=').
	comparison('==', '==').
	comparison('\\=', '\\=').
	comparison('\\==', '\\==').
	comparison('<', '<').
	comparison('=<', '=<').
	comparison('>', '>').
	comparison('>=', '>=').

	positive_atom(Atom, _, _) :-
		var(Atom),
		instantiation_error.
	positive_atom(Atom, Names, function(Name, Terms)) :-
		callable(Atom),
		Atom =.. [Name| Arguments],
		terms(Arguments, Names, Terms).

	terms([], _, []).
	terms([Value| Values], Names, [Term| Terms]) :-
		term(Value, Names, Term),
		terms(Values, Names, Terms).

	term(Value, Names, Term) :-
		var(Value),
		!,
		(	member(Name=Variable, Names),
			Value == Variable,
			Name \== '_' ->
			Term = variable(Name)
		;	Term = anonymous
		).
	term(Value, _, number(Value)) :-
		number(Value),
		!.
	term(Value, Names, list(Elements)) :-
		valid(list, Value),
		!,
		terms(Value, Names, Elements).
	term(Value, Names, list(Elements, Tail)) :-
		Value = [_|_],
		!,
		split_open_list_tail(Value, Values, Rest),
		terms(Values, Names, Elements),
		term(Rest, Names, Tail).
	term(Value, Names, arithmetic(Operator, Arguments)) :-
		nonvar(Value),
		Value =.. [Operator| Values],
		arithmetic_operator(Operator, Arity),
		length(Values, Arity),
		!,
		terms(Values, Names, Arguments).
	term(Value, Names, function(Name, Arguments)) :-
		nonvar(Value),
		Value =.. [Name| Values],
		Values \== [],
		!,
		terms(Values, Names, Arguments).
	term(Value, _, atom(Value)) :-
		atom(Value),
		!.
	term(Value, _, _) :-
		domain_error(problog_syntax, Value).

	split_open_list_tail([Head| Tail], [Head| Values], Rest) :-
		(	nonvar(Tail),
			Tail = [_|_] ->
			split_open_list_tail(Tail, Values, Rest)
		;	Tail == [] ->
			Values = [],
			Rest = []
		;	Values = [],
			Rest = Tail
		).

	arithmetic_operator('+', 1).
	arithmetic_operator('-', 1).
	arithmetic_operator('+', 2).
	arithmetic_operator('-', 2).
	arithmetic_operator('*', 2).
	arithmetic_operator('/', 2).
	arithmetic_operator('//', 2).
	arithmetic_operator(mod, 2).
	arithmetic_operator('**', 2).

:- end_object.
