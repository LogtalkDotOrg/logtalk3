%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(wmc_reference).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-25,
		comment is 'Reference weighted model counter written in Logtalk (no external solver required): DPLL with unit propagation, connected component decomposition, and free variable handling over DIMACS CNF source with Model Counting Competition weights. Intended for small problems and as an oracle for testing; use an external counter for larger problems.'
	]).

	:- public(count/2).
	:- mode(count(+list(code), -number), one_or_error).
	:- info(count/2, [
		comment is 'Computes the weighted model count of CNF source (optional ``p cnf`` header, ``c p weight`` lines, and clauses). Integer counts are exact when they fit the backend; larger counts are approximated as floats. Weights default to 1.',
		argnames is ['Codes', 'Count'],
		exceptions is [
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is neither a partial list nor a list of character codes' - type_error(codes, 'Codes'),
			'The source is not valid CNF source' - domain_error(wmc_source, 'Line')
		]
	]).

	:- uses(avltree, [
		as_dictionary/2 as dict_from_pairs/2, lookup/3 as dict_lookup/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(type, [
		check/3
	]).

	count(Codes, Count) :-
		context(Context),
		check(codes, Codes, Context),
		split_lines(Codes, Lines),
		parse_lines(Lines, none, Header, [], Weights0, [], [], [], ReversedClauses),
		reverse(ReversedClauses, Clauses),
		max_variable(Clauses, Weights0, 0, MaxVariable),
		validate_header(Header, Clauses, MaxVariable, Variables),
		weights_dictionary(Variables, Weights0, Weights),
		variable_list(Variables, VariableList),
		count(Clauses, VariableList, Weights, Count).

	% parsing

	parse_lines([], Header, Header, Weights, Weights, Pending, PendingLine, Clauses, Clauses) :-
		(	Pending == [] ->
			true
		;	source_error(PendingLine)
		).
	parse_lines([Line| Lines], Header0, Header, Weights0, Weights, Pending0, PendingLine0, Clauses0, Clauses) :-
		words(Line, Words),
		(	Words == [] ->
			Header1 = Header0, Weights1 = Weights0, Pending1 = Pending0,
			PendingLine1 = PendingLine0, Clauses1 = Clauses0
		;	Words = [[0'c], [0'p], [0'w, 0'e, 0'i, 0'g, 0'h, 0't]| _] ->
			(	weight_words(Words, Literal, Weight),
				\+ member(Literal-_, Weights0) ->
				Weights1 = [Literal-Weight| Weights0]
			;	source_error(Line)
			),
			Header1 = Header0, Pending1 = Pending0, PendingLine1 = PendingLine0, Clauses1 = Clauses0
		;	Words = [[0'c]| _] ->
			Header1 = Header0, Weights1 = Weights0, Pending1 = Pending0,
			PendingLine1 = PendingLine0, Clauses1 = Clauses0
		;	Words = [[0'p]| _] ->
			(	Header0 == none, Pending0 == [], Clauses0 == [],
				header_words(Words, Header1) ->
				true
			;	source_error(Line)
			),
			Weights1 = Weights0, Pending1 = Pending0, PendingLine1 = PendingLine0, Clauses1 = Clauses0
		;	clause_words(Words, Pending0, Pending1, Clauses0, Clauses1) ->
			Header1 = Header0, Weights1 = Weights0, PendingLine1 = Line
		;	source_error(Line)
		),
		parse_lines(Lines, Header1, Header, Weights1, Weights, Pending1, PendingLine1, Clauses1, Clauses).

	header_words([[0'p], [0'c, 0'n, 0'f], Variables, Clauses], header(VariableCount, ClauseCount)) :-
		integer_codes(Variables, VariableCount),
		VariableCount >= 0,
		integer_codes(Clauses, ClauseCount),
		ClauseCount >= 0.

	weight_words([[0'c], [0'p], [0'w, 0'e, 0'i, 0'g, 0'h, 0't], LiteralWord, WeightText, [0'0]], Literal, Weight) :-
		integer_codes(LiteralWord, Literal),
		Literal =\= 0,
		wmc_output::parse_number(WeightText, Weight),
		Weight >= 0,
		nonnegative_weight_text(WeightText).

	nonnegative_weight_text([0'-| Codes]) :-
		!,
		zero_mantissa(Codes).
	nonnegative_weight_text(_).

	zero_mantissa([]).
	zero_mantissa([Code| _]) :-
		( Code =:= 0'e ; Code =:= 0'E ),
		!.
	zero_mantissa([Code| Codes]) :-
		( Code =:= 0'0 ; Code =:= 0'. ),
		!,
		zero_mantissa(Codes).

	clause_words([], Pending, Pending, Clauses, Clauses).
	clause_words([Word| Words], Pending0, Pending, Clauses0, Clauses) :-
		integer_codes(Word, Literal),
		(	Literal =:= 0 ->
			reverse(Pending0, Clause),
			Pending1 = [],
			Clauses1 = [Clause| Clauses0]
		;	Pending1 = [Literal| Pending0],
			Clauses1 = Clauses0
		),
		clause_words(Words, Pending1, Pending, Clauses1, Clauses).

	validate_header(none, _, MaxVariable, MaxVariable).
	validate_header(header(Variables, Expected), Clauses, MaxVariable, Variables) :-
		length(Clauses, Actual),
		(	Actual =:= Expected,
			MaxVariable =< Variables ->
			true
		;	domain_error(wmc_source, header(Variables, Expected))
		).

	source_error(Line) :-
		atom_codes(Atom, Line),
		domain_error(wmc_source, Atom).

	% only plain (optionally negative) integers
	integer_codes([0'-| Digits], Integer) :-
		!,
		Digits \== [],
		all_digits(Digits),
		number_codes(Magnitude, Digits),
		Integer is -Magnitude.
	integer_codes(Digits, Integer) :-
		Digits \== [],
		all_digits(Digits),
		number_codes(Integer, Digits).

	all_digits([]).
	all_digits([Code| Codes]) :-
		Code >= 0'0, Code =< 0'9,
		all_digits(Codes).

	words(Codes, Words) :-
		drop_blanks(Codes, Trimmed),
		(	Trimmed == [] ->
			Words = []
		;	take_word(Trimmed, Word, Rest),
			Words = [Word| Words1],
			words(Rest, Words1)
		).

	take_word([], [], []).
	take_word([Code| Codes], Word, Rest) :-
		(	blank_code(Code) ->
			Word = [],
			Rest = Codes
		;	Word = [Code| Word1],
			take_word(Codes, Word1, Rest)
		).

	drop_blanks([Code| Codes0], Codes) :-
		blank_code(Code),
		!,
		drop_blanks(Codes0, Codes).
	drop_blanks(Codes, Codes).

	blank_code(9).
	blank_code(13).
	blank_code(32).

	split_lines(Codes, Lines) :-
		split_lines(Codes, [], Lines).

	split_lines([], Buffer, [Line]) :-
		!,
		reverse(Buffer, Line).
	split_lines([10| Codes], Buffer, [Line| Lines]) :-
		!,
		reverse(Buffer, Line),
		split_lines(Codes, [], Lines).
	split_lines([Code| Codes], Buffer, Lines) :-
		split_lines(Codes, [Code| Buffer], Lines).

	% the number of variables is at least the largest variable that occurs in the source

	max_variable(Clauses, Weights, Declared, Variables) :-
		clauses_max(Clauses, Declared, Max0),
		weights_max(Weights, Max0, Variables).

	clauses_max([], Max, Max).
	clauses_max([Clause| Clauses], Max0, Max) :-
		clause_max(Clause, Max0, Max1),
		clauses_max(Clauses, Max1, Max).

	clause_max([], Max, Max).
	clause_max([Literal| Literals], Max0, Max) :-
		Max1 is max(Max0, abs(Literal)),
		clause_max(Literals, Max1, Max).

	weights_max([], Max, Max).
	weights_max([Literal-_| Weights], Max0, Max) :-
		Max1 is max(Max0, abs(Literal)),
		weights_max(Weights, Max1, Max).

	% weights: Variable-weights(Positive, Negative), defaulting to 1

	weights_dictionary(VariableCount, Weights, Dictionary) :-
		weight_pairs(1, VariableCount, Weights, Pairs),
		dict_from_pairs(Pairs, Dictionary).

	weight_pairs(Variable, VariableCount, Weights, Pairs) :-
		(	Variable > VariableCount ->
			Pairs = []
		;	Negated is -Variable,
			(	member(Variable-Positive, Weights) ->
				true
			;	Positive = 1
			),
			(	member(Negated-Negative, Weights) ->
				true
			;	Negative = 1
			),
			Pairs = [Variable-weights(Positive, Negative)| Pairs1],
			Next is Variable + 1,
			weight_pairs(Next, VariableCount, Weights, Pairs1)
		).

	variable_list(VariableCount, Variables) :-
		variable_list(1, VariableCount, Variables).

	variable_list(Variable, VariableCount, Variables) :-
		(	Variable > VariableCount ->
			Variables = []
		;	Variables = [Variable| Variables1],
			Next is Variable + 1,
			variable_list(Next, VariableCount, Variables1)
		).

	% counting

	count(Clauses, Variables, Weights, Count) :-
		(	member([], Clauses) ->
			Count = 0
		;	Clauses == [] ->
			free_product(Variables, Weights, 1, Count)
		;	unit_clause(Clauses, Literal) ->
			assign(Literal, Clauses, Variables, Weights, Factor, Clauses1, Variables1),
			count(Clauses1, Variables1, Weights, Count1),
			count_product(Factor, Count1, Count)
		;	clauses_variables(Clauses, Used),
			free_variables(Variables, Used, Free, Constrained),
			free_product(Free, Weights, 1, FreeCount),
			components(Clauses, Components),
			components_count(Components, Constrained, Weights, 1, ComponentsCount),
			count_product(FreeCount, ComponentsCount, Count)
		).

	unit_clause([[Literal]| _], Literal) :-
		!.
	unit_clause([_| Clauses], Literal) :-
		unit_clause(Clauses, Literal).

	% a variable set to a literal contributes the weight of that literal

	assign(Literal, Clauses, Variables, Weights, Factor, Clauses1, Variables1) :-
		Variable is abs(Literal),
		dict_lookup(Variable, weights(Positive, Negative), Weights),
		(	Literal > 0 -> Factor = Positive ; Factor = Negative ),
		remove_variable(Variables, Variable, Variables1),
		Negated is -Literal,
		simplify(Clauses, Literal, Negated, Clauses1).

	remove_variable([], _, []).
	remove_variable([Head| Tail], Variable, Rest) :-
		(	Head =:= Variable ->
			Rest = Tail
		;	Rest = [Head| Rest1],
			remove_variable(Tail, Variable, Rest1)
		).

	simplify([], _, _, []).
	simplify([Clause| Clauses], Literal, Negated, Simplified) :-
		(	memberchk(Literal, Clause) ->
			Simplified = Simplified1
		;	remove_literal(Clause, Negated, Clause1),
			Simplified = [Clause1| Simplified1]
		),
		simplify(Clauses, Literal, Negated, Simplified1).

	remove_literal([], _, []).
	remove_literal([Head| Tail], Literal, Rest) :-
		(	Head =:= Literal ->
			Rest = Tail1
		;	Rest = [Head| Tail1]
		),
		remove_literal(Tail, Literal, Tail1).

	% variables that occur in no clause are free

	clauses_variables(Clauses, Variables) :-
		clauses_variables(Clauses, [], Variables0),
		sort(Variables0, Variables).

	clauses_variables([], Variables, Variables).
	clauses_variables([Clause| Clauses], Variables0, Variables) :-
		clause_variables(Clause, Variables0, Variables1),
		clauses_variables(Clauses, Variables1, Variables).

	clause_variables([], Variables, Variables).
	clause_variables([Literal| Literals], Variables0, Variables) :-
		Variable is abs(Literal),
		clause_variables(Literals, [Variable| Variables0], Variables).

	free_variables([], _, [], []).
	free_variables([Variable| Variables], Used, Free, Constrained) :-
		(	memberchk(Variable, Used) ->
			Free = Free1,
			Constrained = [Variable| Constrained1]
		;	Free = [Variable| Free1],
			Constrained = Constrained1
		),
		free_variables(Variables, Used, Free1, Constrained1).

	free_product([], _, Product, Product).
	free_product([Variable| Variables], Weights, Product0, Product) :-
		dict_lookup(Variable, weights(Positive, Negative), Weights),
		count_sum(Positive, Negative, Sum),
		count_product(Sum, Product0, Product1),
		free_product(Variables, Weights, Product1, Product).

	% connected components of the clauses (clauses sharing variables)

	components([], []).
	components([Clause| Clauses], [Component| Components]) :-
		grow([Clause], Clauses, Component, Rest),
		components(Rest, Components).

	grow(Component0, Pool, Component, Rest) :-
		clauses_variables(Component0, Variables),
		split_connected(Pool, Variables, Connected, Others),
		(	Connected == [] ->
			Component = Component0,
			Rest = Others
		;	append(Component0, Connected, Component1),
			grow(Component1, Others, Component, Rest)
		).

	split_connected([], _, [], []).
	split_connected([Clause| Clauses], Variables, Connected, Others) :-
		(	shares_variable(Clause, Variables) ->
			Connected = [Clause| Connected1],
			Others = Others1
		;	Connected = Connected1,
			Others = [Clause| Others1]
		),
		split_connected(Clauses, Variables, Connected1, Others1).

	shares_variable([Literal| Literals], Variables) :-
		Variable is abs(Literal),
		(	member(Variable, Variables) ->
			true
		;	shares_variable(Literals, Variables)
		).

	components_count([], _, _, Count, Count).
	components_count([Component| Components], Constrained, Weights, Count0, Count) :-
		clauses_variables(Component, ComponentVariables),
		branch(Component, ComponentVariables, Weights, BranchCount),
		count_product(Count0, BranchCount, Count1),
		components_count(Components, Constrained, Weights, Count1, Count).

	% branch on the most frequent variable of a component

	branch(Clauses, Variables, Weights, Count) :-
		most_frequent(Variables, Clauses, Variable),
		dict_lookup(Variable, weights(Positive, Negative), Weights),
		branch_value(Variable, Positive, Clauses, Variables, Weights, PositiveCount),
		Negated is -Variable,
		branch_value(Negated, Negative, Clauses, Variables, Weights, NegativeCount),
		count_sum(PositiveCount, NegativeCount, Count).

	branch_value(Literal, Weight, Clauses, Variables, Weights, Value) :-
		Variable is abs(Literal),
		remove_variable(Variables, Variable, Variables1),
		Negated is -Literal,
		simplify(Clauses, Literal, Negated, Clauses1),
		count(Clauses1, Variables1, Weights, Count),
		count_product(Weight, Count, Value).

	count_sum(A, B, Sum) :-
		(	integer(A), integer(B),
			current_prolog_flag(bounded, true),
			current_prolog_flag(max_integer, Maximum),
			A > Maximum - B ->
			Sum is A * 1.0 + B
		;	Sum is A + B
		),
		wmc_output::check_finite(Sum).

	count_product(A, B, Product) :-
		(	integer(A), integer(B), A > 0,
			current_prolog_flag(bounded, true),
			current_prolog_flag(max_integer, Maximum),
			B > Maximum // A ->
			Product is A * 1.0 * B
		;	Product is A * B
		),
		wmc_output::check_finite(Product).

	most_frequent([Variable| Variables], Clauses, Best) :-
		occurrences(Clauses, Variable, 0, Count),
		most_frequent(Variables, Clauses, Variable, Count, Best).

	most_frequent([], _, Best, _, Best).
	most_frequent([Variable| Variables], Clauses, Best0, Count0, Best) :-
		occurrences(Clauses, Variable, 0, Count),
		(	Count > Count0 ->
			most_frequent(Variables, Clauses, Variable, Count, Best)
		;	most_frequent(Variables, Clauses, Best0, Count0, Best)
		).

	occurrences([], _, Count, Count).
	occurrences([Clause| Clauses], Variable, Count0, Count) :-
		(	(	memberchk(Variable, Clause)
			;	Negated is -Variable,
				memberchk(Negated, Clause)
			) ->
			Count1 is Count0 + 1
		;	Count1 is Count0
		),
		occurrences(Clauses, Variable, Count1, Count).

:- end_object.
