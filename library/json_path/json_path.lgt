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


:- object(json_path(_StringRepresentation_),
	implements(json_path_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'Initial JSONPath implementation supporting child and descendant navigation, names, wildcards, indexes, and slices.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used for query member names and normalized paths. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- uses(list, [
		append/3, length/2, nth0/3, valid/1 as proper_list/1
	]).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == xvm; Dialect == swi))).
		:- if(current_logtalk_flag(prolog_dialect, swi)).
			:- use_module(unicode, [unicode_property/2]).
		:- elif(current_logtalk_flag(prolog_dialect, xvm)).
			:- uses(user, [unicode_property/2]).
		:- endif.
	:- endif.

	parse(Source, Query) :-
		parse_source_codes(Source, Codes),
		phrase(json_path_query(Query), Codes),
		query_valid(Query).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(Sink, Query) :-
		query_valid(Query),
		query_codes(Query, Codes),
		!,
		generate_sink_codes(Sink, Codes).
	generate(_, Query) :-
		domain_error(json_path_query, Query).

	evaluate(Query, _, _) :-
		var(Query),
		instantiation_error.
	evaluate(_, JSON, _) :-
		var(JSON),
		instantiation_error.
	evaluate(Query, JSON, Values) :-
		evaluate_query(Query, JSON, InternalNodes),
		node_values(InternalNodes, Values).

	paths(Query, _, _) :-
		var(Query),
		instantiation_error.
	paths(_, JSON, _) :-
		var(JSON),
		instantiation_error.
	paths(Query, JSON, Paths) :-
		evaluate_query(Query, JSON, InternalNodes),
		node_paths(InternalNodes, Paths).

	nodes(Query, _, _) :-
		var(Query),
		instantiation_error.
	nodes(_, JSON, _) :-
		var(JSON),
		instantiation_error.
	nodes(Query, JSON, Nodes) :-
		evaluate_query(Query, JSON, InternalNodes),
		normalized_nodes(InternalNodes, Nodes).

	query(Source, _, _) :-
		var(Source),
		instantiation_error.
	query(_, JSON, _) :-
		var(JSON),
		instantiation_error.
	query(Source, JSON, Values) :-
		parse(Source, Query),
		evaluate(Query, JSON, Values).

	evaluate_query(Query, JSON, Nodes) :-
		query_segments(Query, Segments),
		evaluate_segments(Segments, JSON, [inode([], JSON)], Nodes).

	query_segments(json_path(Segments), Segments) :-
		query_segments_valid(Segments),
		!.
	query_segments(Query, _) :-
		domain_error(json_path_query, Query).

	evaluate_segments([], _, Nodes, Nodes).
	evaluate_segments([Segment| Segments], RootJSON, InputNodes, OutputNodes) :-
		apply_segment(Segment, RootJSON, InputNodes, IntermediateNodes),
		evaluate_segments(Segments, RootJSON, IntermediateNodes, OutputNodes).

	apply_segment(child(Selectors), RootJSON, InputNodes, OutputNodes) :-
		proper_list(Selectors),
		!,
		apply_child_segment(InputNodes, RootJSON, Selectors, OutputNodes).
	apply_segment(descendant(Selectors), RootJSON, InputNodes, OutputNodes) :-
		proper_list(Selectors),
		!,
		apply_descendant_segment(InputNodes, RootJSON, Selectors, OutputNodes).
	apply_segment(Query, _, _, _) :-
		domain_error(json_path_query, Query).

	apply_child_segment([], _, _, []).
	apply_child_segment([Node| Nodes], RootJSON, Selectors, OutputNodes) :-
		selector_results(Selectors, Node, RootJSON, HeadNodes),
		apply_child_segment(Nodes, RootJSON, Selectors, TailNodes),
		append(HeadNodes, TailNodes, OutputNodes).

	apply_descendant_segment([], _, _, []).
	apply_descendant_segment([Node| Nodes], RootJSON, Selectors, OutputNodes) :-
		descendant_nodes(Node, DescendantNodes),
		apply_child_segment(DescendantNodes, RootJSON, Selectors, HeadNodes),
		apply_descendant_segment(Nodes, RootJSON, Selectors, TailNodes),
		append(HeadNodes, TailNodes, OutputNodes).

	selector_results([], _, _, []).
	selector_results([Selector| Selectors], Node, RootJSON, OutputNodes) :-
		selector_nodes(Selector, Node, RootJSON, HeadNodes),
		selector_results(Selectors, Node, RootJSON, TailNodes),
		append(HeadNodes, TailNodes, OutputNodes).

	selector_nodes(name(Token), inode(Location, JSON), _, Nodes) :-
		!,
		text_atom(Token, Name),
		(	object_entries(JSON, Entries),
			first_named_entry(Entries, Name, Value) ->
			extend_location(Location, member(Name), ChildLocation),
			Nodes = [inode(ChildLocation, Value)]
		;	Nodes = []
		).
	selector_nodes(wildcard, inode(Location, JSON), _, Nodes) :-
		!,
		child_nodes(Location, JSON, Nodes).
	selector_nodes(index(Index), inode(Location, JSON), _, Nodes) :-
		integer(Index),
		!,
		(	proper_list(JSON),
			resolve_index(Index, JSON, ResolvedIndex),
			nth0(ResolvedIndex, JSON, Value) ->
			extend_location(Location, index(ResolvedIndex), ChildLocation),
			Nodes = [inode(ChildLocation, Value)]
		;	Nodes = []
		).
	selector_nodes(slice(Start, End, Step), inode(Location, JSON), _, Nodes) :-
		!,
		(	proper_list(JSON) ->
			slice_indices(JSON, Start, End, Step, Indices),
			indexed_nodes(Indices, Location, JSON, Nodes)
		;	Nodes = []
		).
	selector_nodes(filter(Expression), inode(Location, JSON), RootJSON, Nodes) :-
		!,
		child_nodes(Location, JSON, Children),
		filtered_nodes(Children, RootJSON, Expression, Nodes).
	selector_nodes(Query, _, _, _) :-
		domain_error(json_path_query, Query).

	descendant_nodes(Node, [Node| Descendants]) :-
		node_children(Node, Children),
		descendant_lists(Children, Descendants).

	descendant_lists([], []).
	descendant_lists([Child| Children], Descendants) :-
		descendant_nodes(Child, ChildDescendants),
		descendant_lists(Children, RemainingDescendants),
		append(ChildDescendants, RemainingDescendants, Descendants).

	node_children(inode(Location, JSON), Children) :-
		child_nodes(Location, JSON, Children).

	child_nodes(Location, JSON, Nodes) :-
		object_entries(JSON, Entries),
		!,
		object_child_nodes(Entries, Location, Nodes).
	child_nodes(Location, JSON, Nodes) :-
		proper_list(JSON),
		!,
		array_child_nodes(JSON, Location, 0, Nodes).
	child_nodes(_, _, []).

	object_child_nodes([], _, []).
	object_child_nodes([entry(Name, Value)| Entries], Location, [inode(ChildLocation, Value)| Nodes]) :-
		extend_location(Location, member(Name), ChildLocation),
		object_child_nodes(Entries, Location, Nodes).

	array_child_nodes([], _, _, []).
	array_child_nodes([Value| Values], Location, Index, [inode(ChildLocation, Value)| Nodes]) :-
		extend_location(Location, index(Index), ChildLocation),
		NextIndex is Index + 1,
		array_child_nodes(Values, Location, NextIndex, Nodes).

	indexed_nodes([], _, _, []).
	indexed_nodes([Index| Indices], Location, JSON, [inode(ChildLocation, Value)| Nodes]) :-
		nth0(Index, JSON, Value),
		extend_location(Location, index(Index), ChildLocation),
		indexed_nodes(Indices, Location, JSON, Nodes).

	first_named_entry([entry(Name, Value)| _], Name, Value).
	first_named_entry([_| Entries], Name, Value) :-
		first_named_entry(Entries, Name, Value).

	extend_location(Location, Token, ExtendedLocation) :-
		append(Location, [Token], ExtendedLocation).

	resolve_index(Index, Array, ResolvedIndex) :-
		length(Array, Length),
		(	Index >= 0 ->
			ResolvedIndex = Index
		;	ResolvedIndex is Length + Index
		),
		ResolvedIndex >= 0,
		ResolvedIndex < Length.

	slice_indices(Array, Start, End, Step, Indices) :-
		length(Array, Length),
		default_step(Step, NormalizedStep),
		(	NormalizedStep =:= 0 ->
			Indices = []
		;	default_slice_bound(Start, NormalizedStep, Length, NormalizedStart0),
			default_slice_end(End, NormalizedStep, Length, NormalizedEnd0),
			normalize_slice_index(NormalizedStart0, Length, NormalizedStart),
			normalize_slice_index(NormalizedEnd0, Length, NormalizedEnd),
			slice_bounds(NormalizedStart, NormalizedEnd, NormalizedStep, Length, Lower, Upper),
			collect_slice_indices(Lower, Upper, NormalizedStep, Indices)
		).

	default_step(default, 1) :-
		!.
	default_step(Step, Step) :-
		integer(Step).

	default_slice_bound(default, Step, Length, Bound) :-
		!,
		(	Step >= 0 ->
			Bound = 0
		;	Bound is Length - 1
		).
	default_slice_bound(Bound, _, _, Bound).

	default_slice_end(default, Step, Length, End) :-
		!,
		(	Step >= 0 ->
			End = Length
		;	End is -Length - 1
		).
	default_slice_end(End, _, _, End).

	normalize_slice_index(Index, Length, NormalizedIndex) :-
		(	Index >= 0 ->
			NormalizedIndex = Index
		;	NormalizedIndex is Length + Index
		).

	slice_bounds(Start, End, Step, Length, Lower, Upper) :-
		(	Step >= 0 ->
			Lower is min(max(Start, 0), Length),
			Upper is min(max(End, 0), Length)
		;	Upper is min(max(Start, -1), Length - 1),
			Lower is min(max(End, -1), Length - 1)
		).

	collect_slice_indices(Lower, Upper, Step, Indices) :-
		(	Step > 0 ->
			collect_forward_indices(Lower, Upper, Step, Indices)
		;	collect_backward_indices(Upper, Lower, Step, Indices)
		).

	collect_forward_indices(Index, Upper, _, []) :-
		Index >= Upper,
		!.
	collect_forward_indices(Index, Upper, Step, [Index| Indices]) :-
		NextIndex is Index + Step,
		collect_forward_indices(NextIndex, Upper, Step, Indices).

	collect_backward_indices(Index, Lower, _, []) :-
		Lower >= Index,
		!.
	collect_backward_indices(Index, Lower, Step, [Index| Indices]) :-
		NextIndex is Index + Step,
		collect_backward_indices(NextIndex, Lower, Step, Indices).

	query_valid(json_path(Segments)) :-
		query_segments_valid(Segments).

	query_segments_valid(Segments) :-
		proper_list(Segments),
		query_segments_valid_(Segments).

	query_segments_valid_([]).
	query_segments_valid_([Segment| Segments]) :-
		valid_segment(Segment),
		query_segments_valid_(Segments).

	valid_segment(child(Selectors)) :-
		proper_list(Selectors),
		Selectors \== [],
		valid_selectors(Selectors).
	valid_segment(descendant(Selectors)) :-
		proper_list(Selectors),
		Selectors \== [],
		valid_selectors(Selectors).

	valid_selectors([]).
	valid_selectors([Selector| Selectors]) :-
		valid_selector(Selector),
		valid_selectors(Selectors).

	valid_selector(name(Token)) :-
		valid_text_token(Token).
	valid_selector(wildcard).
	valid_selector(index(Index)) :-
		valid_query_integer(Index).
	valid_selector(slice(Start, End, Step)) :-
		valid_optional_integer(Start),
		valid_optional_integer(End),
		valid_optional_step(Step).
	valid_selector(filter(Expression)) :-
		logical_expr_type(Expression, logical).

	valid_optional_integer(default) :-
		!.
	valid_optional_integer(Integer) :-
		valid_query_integer(Integer).

	valid_optional_step(default).
	valid_optional_step(Integer) :-
		valid_query_integer(Integer).

	valid_query_integer(Integer) :-
		integer(Integer),
		ijson_exact_integer_min(Minimum),
		ijson_exact_integer_max(Maximum),
		Integer >= Minimum,
		Integer =< Maximum.

	ijson_exact_integer_min(-9007199254740991).

	ijson_exact_integer_max(9007199254740991).

	valid_text_token(Token) :-
		(	_StringRepresentation_ == atom ->
			atom(Token)
		;	_StringRepresentation_ == chars ->
			Token = chars(Chars),
			valid_char_list(Chars)
		;	_StringRepresentation_ == codes ->
			Token = codes(Codes),
			valid_code_list(Codes)
		;	fail
		).

	valid_char_list([]).
	valid_char_list([Char| Chars]) :-
		char_code(Char, Code),
		unicode_scalar_value(Code),
		valid_char_list(Chars).

	valid_code_list([]).
	valid_code_list([Code| Codes]) :-
		unicode_scalar_value(Code),
		valid_code_list(Codes).

	unicode_scalar_value(Code) :-
		integer(Code),
		Code >= 0,
		Code =< 1114111,
		\+ (Code >= 55296, Code =< 57343).

	logical_expr_type(or(Left, Right), logical) :-
		logical_expr_type(Left, logical),
		logical_expr_type(Right, logical).
	logical_expr_type(and(Left, Right), logical) :-
		logical_expr_type(Left, logical),
		logical_expr_type(Right, logical).
	logical_expr_type(not(Expression), logical) :-
		logical_expr_type(Expression, logical).
	logical_expr_type(exists(Path), logical) :-
		valid_filter_path(Path).
	logical_expr_type(test_function(Function), logical) :-
		function_type(Function, ResultType),
		(	ResultType == logical ->
			true
		;	ResultType == nodes
		).
	logical_expr_type(compare(Operator, Left, Right), logical) :-
		valid_compare_operator(Operator),
		comparable_type(Left, value),
		comparable_type(Right, value).

	comparable_type(literal(Value), value) :-
		valid_literal_value(Value).
	comparable_type(path(Path), value) :-
		path_is_singular(Path),
		!.
	comparable_type(function(Name, Arguments), value) :-
		function_type(function(Name, Arguments), value).

	function_type(function(Name, Arguments), ResultType) :-
		function_signature(Name, ParameterTypes, ResultType),
		function_arguments_valid(Arguments, ParameterTypes).

	function_signature(length, [value], value).
	function_signature(count, [nodes], value).
	function_signature(match, [value, value], logical).
	function_signature(search, [value, value], logical).
	function_signature(value, [nodes], value).

	function_arguments_valid([], []).
	function_arguments_valid([Argument| Arguments], [value| ParameterTypes]) :-
		!,
		valid_value_function_argument(Argument),
		function_arguments_valid(Arguments, ParameterTypes).
	function_arguments_valid([Argument| Arguments], [nodes| ParameterTypes]) :-
		!,
		valid_nodes_function_argument(Argument),
		function_arguments_valid(Arguments, ParameterTypes).
	function_arguments_valid([Argument| Arguments], [logical| ParameterTypes]) :-
		valid_logical_function_argument(Argument),
		function_arguments_valid(Arguments, ParameterTypes).

	valid_value_function_argument(literal(Value)) :-
		valid_literal_value(Value).
	valid_value_function_argument(path(Path)) :-
		path_is_singular(Path),
		!.
	valid_value_function_argument(function(Name, Arguments)) :-
		function_type(function(Name, Arguments), value).

	valid_nodes_function_argument(path(Path)) :-
		valid_filter_path(Path).
	valid_nodes_function_argument(function(Name, Arguments)) :-
		function_type(function(Name, Arguments), nodes).

	valid_logical_function_argument(function(Name, Arguments)) :-
		function_type(function(Name, Arguments), logical),
		!.
	valid_logical_function_argument(logical(Expression)) :-
		logical_expr_type(Expression, logical).

	valid_filter_path(path(Origin, Segments)) :-
		valid_path_origin(Origin),
		query_segments_valid(Segments).

	path_is_singular(path(Origin, Segments)) :-
		valid_path_origin(Origin),
		singular_query_segments_valid(Segments).

	valid_path_origin(root).
	valid_path_origin(current).

	singular_query_segments_valid(Segments) :-
		proper_list(Segments),
		singular_query_segments_valid_(Segments).

	singular_query_segments_valid_([]).
	singular_query_segments_valid_([child([name(Token)])| Segments]) :-
		valid_text_token(Token),
		singular_query_segments_valid_(Segments).
	singular_query_segments_valid_([child([index(Index)])| Segments]) :-
		valid_query_integer(Index),
		singular_query_segments_valid_(Segments).

	valid_literal_value(Value) :-
		number(Value),
		!.
	valid_literal_value(Value) :-
		json_special_literal(Value),
		!.
	valid_literal_value(Value) :-
		atom(Value).

	valid_compare_operator(Operator) :-
		( Operator == eq ->
			true
		; Operator == ne ->
			true
		; Operator == lt ->
			true
		; Operator == le ->
			true
		; Operator == gt ->
			true
		; Operator == ge
		).

	filtered_nodes([], _, _, []).
	filtered_nodes([Node| Nodes], RootJSON, Expression, FilteredNodes) :-
		(	filter_expression_true(Expression, RootJSON, Node) ->
			FilteredNodes = [Node| RemainingFilteredNodes]
		;	FilteredNodes = RemainingFilteredNodes
		),
		filtered_nodes(Nodes, RootJSON, Expression, RemainingFilteredNodes).

	filter_expression_true(Expression, RootJSON, CurrentNode) :-
		evaluate_logical_expression(Expression, RootJSON, CurrentNode, true).

	evaluate_logical_expression(or(Left, Right), RootJSON, CurrentNode, Result) :-
		evaluate_logical_expression(Left, RootJSON, CurrentNode, LeftResult),
		evaluate_logical_expression(Right, RootJSON, CurrentNode, RightResult),
		logical_or(LeftResult, RightResult, Result).
	evaluate_logical_expression(and(Left, Right), RootJSON, CurrentNode, Result) :-
		evaluate_logical_expression(Left, RootJSON, CurrentNode, LeftResult),
		evaluate_logical_expression(Right, RootJSON, CurrentNode, RightResult),
		logical_and(LeftResult, RightResult, Result).
	evaluate_logical_expression(not(Expression), RootJSON, CurrentNode, Result) :-
		evaluate_logical_expression(Expression, RootJSON, CurrentNode, ExpressionResult),
		logical_not(ExpressionResult, Result).
	evaluate_logical_expression(exists(Path), RootJSON, CurrentNode, Result) :-
		evaluate_filter_path(Path, RootJSON, CurrentNode, Nodes),
		(	Nodes == [] ->
			Result = false
		;	Result = true
		).
	evaluate_logical_expression(test_function(function(Name, Arguments)), RootJSON, CurrentNode, Result) :-
		evaluate_logical_function(Name, Arguments, RootJSON, CurrentNode, Result).
	evaluate_logical_expression(compare(Operator, Left, Right), RootJSON, CurrentNode, Result) :-
		evaluate_comparable(Left, RootJSON, CurrentNode, LeftValue),
		evaluate_comparable(Right, RootJSON, CurrentNode, RightValue),
		compare_filter_values(Operator, LeftValue, RightValue, Result).

	logical_or(true, _, true).
	logical_or(false, Right, Right).

	logical_and(true, Right, Right).
	logical_and(false, _, false).

	logical_not(true, false).
	logical_not(false, true).

	evaluate_filter_path(path(root, Segments), RootJSON, _, Nodes) :-
		!,
		evaluate_segments(Segments, RootJSON, [inode([], RootJSON)], Nodes).
	evaluate_filter_path(path(current, Segments), RootJSON, CurrentNode, Nodes) :-
		evaluate_segments(Segments, RootJSON, [CurrentNode], Nodes).

	evaluate_comparable(literal(Value), _, _, Value).
	evaluate_comparable(path(Path), RootJSON, CurrentNode, Value) :-
		evaluate_filter_path(Path, RootJSON, CurrentNode, Nodes),
		nodelist_value(Nodes, Value).
	evaluate_comparable(function(Name, Arguments), RootJSON, CurrentNode, Value) :-
		evaluate_value_function(Name, Arguments, RootJSON, CurrentNode, Value).

	nodelist_value([], nothing).
	nodelist_value([inode(_, Value)], Value) :-
		!.
	nodelist_value([_, _| _], nothing).

	evaluate_value_function(length, [Argument], RootJSON, CurrentNode, Value) :-
		evaluate_value_argument(Argument, RootJSON, CurrentNode, ArgumentValue),
		length_function_value(ArgumentValue, Value).
	evaluate_value_function(count, [Argument], RootJSON, CurrentNode, Value) :-
		evaluate_nodes_argument(Argument, RootJSON, CurrentNode, Nodes),
		length(Nodes, Value).
	evaluate_value_function(value, [Argument], RootJSON, CurrentNode, Value) :-
		evaluate_nodes_argument(Argument, RootJSON, CurrentNode, Nodes),
		nodelist_value(Nodes, Value).

	evaluate_logical_function(match, [ValueArgument, PatternArgument], RootJSON, CurrentNode, Result) :-
		evaluate_value_argument(ValueArgument, RootJSON, CurrentNode, Value),
		evaluate_value_argument(PatternArgument, RootJSON, CurrentNode, Pattern),
		regexp_function_result(match, Value, Pattern, Result).
	evaluate_logical_function(search, [ValueArgument, PatternArgument], RootJSON, CurrentNode, Result) :-
		evaluate_value_argument(ValueArgument, RootJSON, CurrentNode, Value),
		evaluate_value_argument(PatternArgument, RootJSON, CurrentNode, Pattern),
		regexp_function_result(search, Value, Pattern, Result).
	evaluate_logical_function(_, _, _, _, false).

	evaluate_value_argument(literal(Value), _, _, Value).
	evaluate_value_argument(path(Path), RootJSON, CurrentNode, Value) :-
		evaluate_filter_path(Path, RootJSON, CurrentNode, Nodes),
		nodelist_value(Nodes, Value).
	evaluate_value_argument(function(Name, Arguments), RootJSON, CurrentNode, Value) :-
		evaluate_value_function(Name, Arguments, RootJSON, CurrentNode, Value).

	evaluate_nodes_argument(path(Path), RootJSON, CurrentNode, Nodes) :-
		evaluate_filter_path(Path, RootJSON, CurrentNode, Nodes).

	length_function_value(nothing, nothing) :-
		!.
	length_function_value(Value, LengthValue) :-
		atom(Value),
		!,
		atom_codes(Value, Codes),
		length(Codes, LengthValue).
	length_function_value(Value, LengthValue) :-
		proper_list(Value),
		!,
		length(Value, LengthValue).
	length_function_value(Value, LengthValue) :-
		object_entries(Value, Entries),
		!,
		length(Entries, LengthValue).
	length_function_value(_, nothing).

	regexp_function_result(_, Value, _, false) :-
		\+ json_string_codes(Value, _),
		!.
	regexp_function_result(_, _, Pattern, false) :-
		\+ json_string_codes(Pattern, _),
		!.
	regexp_function_result(Mode, Value, Pattern, Result) :-
		json_string_codes(Value, ValueCodes),
		json_string_codes(Pattern, PatternCodes),
		(   parse_iregexp(PatternCodes, Regexp) ->
			(   regexp_match_result(Mode, Regexp, ValueCodes) ->
				Result = true
			;   Result = false
			)
		;   Result = false
		).

	json_string_codes(Value, Codes) :-
		atom(Value),
		\+ json_special_literal(Value),
		atom_codes(Value, Codes).

	regexp_match_result(match, Regexp, Codes) :-
		iregexp_matches(Regexp, Codes).
	regexp_match_result(search, Regexp, Codes) :-
		iregexp_searches(Regexp, Codes).

	parse_iregexp(Codes, Regexp) :-
		parse_iregexp(Codes, [], Regexp),
		iregexp_valid(Regexp).

	parse_iregexp(Codes0, Rest, regexp(Branches)) :-
		parse_iregexp_branch(Codes0, Codes1, Branch),
		parse_iregexp_branches(Codes1, Rest, [Branch], Branches).

	parse_iregexp_branches([124| Codes0], Rest, Branches0, Branches) :-
		!,
		parse_iregexp_branch(Codes0, Codes1, Branch),
		append(Branches0, [Branch], Branches1),
		parse_iregexp_branches(Codes1, Rest, Branches1, Branches).
	parse_iregexp_branches(Codes, Codes, Branches, Branches).

	parse_iregexp_branch(Codes, Codes, []) :-
		iregexp_branch_end(Codes),
		!.
	parse_iregexp_branch(Codes0, Rest, [Piece| Pieces]) :-
		parse_iregexp_piece(Codes0, Codes1, Piece),
		parse_iregexp_branch(Codes1, Rest, Pieces).

	iregexp_branch_end([]).
	iregexp_branch_end([124| _]) :-
		!.
	iregexp_branch_end([41| _]).

	parse_iregexp_piece(Codes0, Rest, piece(Atom, Quantifier)) :-
		parse_iregexp_atom(Codes0, Codes1, Atom),
		parse_iregexp_quantifier(Codes1, Rest, Quantifier).

	parse_iregexp_atom([40| Codes0], Rest, group(Regexp)) :-
		!,
		parse_iregexp(Codes0, [41| Rest], Regexp).
	parse_iregexp_atom([46| Rest], Rest, any) :-
		!.
	parse_iregexp_atom(Codes0, Rest, Class) :-
		Codes0 = [91| _],
		!,
		parse_iregexp_char_class(Codes0, Rest, Class).
	parse_iregexp_atom([92| Codes0], Rest, property(Property)) :-
		parse_iregexp_property_escape(Codes0, Rest, Property),
		!.
	parse_iregexp_atom([92| Codes0], Rest, literal(Code)) :-
		!,
		parse_iregexp_escaped_code(Codes0, Rest, Code).
	parse_iregexp_atom([Code| Rest], Rest, literal(Code)) :-
		iregexp_normal_char(Code).

	parse_iregexp_quantifier([42| Rest], Rest, repeat(0, many)) :-
		!.
	parse_iregexp_quantifier([43| Rest], Rest, repeat(1, many)) :-
		!.
	parse_iregexp_quantifier([63| Rest], Rest, repeat(0, 1)) :-
		!.
	parse_iregexp_quantifier([123| Codes0], Rest, repeat(Min, Max)) :-
		!,
		parse_iregexp_range_quantifier(Codes0, Rest, Min, Max).
	parse_iregexp_quantifier(Codes, Codes, repeat(1, 1)).

	parse_iregexp_range_quantifier(Codes0, Rest, Min, Min) :-
		parse_iregexp_decimal(Codes0, [125| Rest], Min),
		!.
	parse_iregexp_range_quantifier(Codes0, Rest, Min, many) :-
		parse_iregexp_decimal(Codes0, [44, 125| Rest], Min),
		!.
	parse_iregexp_range_quantifier(Codes0, Rest, Min, Max) :-
		parse_iregexp_decimal(Codes0, [44| Codes1], Min),
		parse_iregexp_decimal(Codes1, [125| Rest], Max),
		Max >= Min.

	parse_iregexp_decimal(Codes0, Rest, Integer) :-
		parse_iregexp_digits(Codes0, Rest, Digits),
		number_codes(Integer, Digits).

	parse_iregexp_digits([Digit| Codes0], Rest, [Digit| Digits]) :-
		Digit >= 0'0,
		Digit =< 0'9,
		parse_iregexp_digits_tail(Codes0, Rest, Digits).

	parse_iregexp_digits_tail([Digit| Codes0], Rest, [Digit| Digits]) :-
		Digit >= 0'0,
		Digit =< 0'9,
		!,
		parse_iregexp_digits_tail(Codes0, Rest, Digits).
	parse_iregexp_digits_tail(Codes, Codes, []).

	parse_iregexp_char_class([91| Codes0], Rest, class(Negated, Items)) :-
		parse_iregexp_class_negation(Codes0, Codes1, Negated),
		parse_iregexp_class_leading_hyphen(Codes1, Codes2, LeadingItems),
		parse_iregexp_class_items(Codes2, Codes3, MiddleItems),
		parse_iregexp_class_trailing_hyphen(Codes3, [93| Rest], TrailingItems),
		append(LeadingItems, MiddleItems, Items0),
		append(Items0, TrailingItems, Items),
		iregexp_class_items_valid(Negated, Items).

	parse_iregexp_class_negation([94| Codes], Codes, true) :-
		!.
	parse_iregexp_class_negation(Codes, Codes, false).

	parse_iregexp_class_leading_hyphen([45| Codes], Codes, [code(45)]) :-
		!.
	parse_iregexp_class_leading_hyphen(Codes, Codes, []).

	parse_iregexp_class_items(Codes, Codes, []) :-
		iregexp_class_items_end(Codes),
		!.
	parse_iregexp_class_items(Codes0, Rest, [Item| Items]) :-
		parse_iregexp_class_item(Codes0, Codes1, Item),
		parse_iregexp_class_items(Codes1, Rest, Items).

	iregexp_class_items_end([93| _]) :-
		!.
	iregexp_class_items_end([45, 93| _]).

	parse_iregexp_class_trailing_hyphen([45, 93| Rest], [93| Rest], [code(45)]) :-
		!.
	parse_iregexp_class_trailing_hyphen(Codes, Codes, []).

	parse_iregexp_class_item([92| Codes0], Rest, property(Property)) :-
		parse_iregexp_property_escape(Codes0, Rest, Property),
		!.
	parse_iregexp_class_item(Codes0, Rest, Item) :-
		parse_iregexp_class_char(Codes0, Codes1, First),
		(   Codes1 = [45| Codes2],
			Codes2 \= [93| _],
			parse_iregexp_class_char(Codes2, Rest, Second) ->
			iregexp_class_range(First, Second, Item)
		;   Rest = Codes1,
			Item = code(First)
		).

	iregexp_class_range(First, Second, range(First, Second)) :-
		First =< Second.

	parse_iregexp_class_char([92| Codes0], Rest, Code) :-
		!,
		parse_iregexp_escaped_code(Codes0, Rest, Code).
	parse_iregexp_class_char([Code| Rest], Rest, Code) :-
		iregexp_class_char(Code).

	parse_iregexp_property_escape([112, 123| Codes0], Rest, category(Property)) :-
		iregexp_property_support_available,
		!,
		parse_iregexp_char_property(Codes0, [125| Rest], Property).
	parse_iregexp_property_escape([80, 123| Codes0], Rest, complement_category(Property)) :-
		iregexp_property_support_available,
		!,
		parse_iregexp_char_property(Codes0, [125| Rest], Property).

	parse_iregexp_char_property(Codes0, Rest, Property) :-
		iregexp_char_property(Property, PropertyCodes),
		append(PropertyCodes, Rest, Codes0).

	iregexp_char_property('L', [76]).
	iregexp_char_property('Ll', [76, 108]).
	iregexp_char_property('Lm', [76, 109]).
	iregexp_char_property('Lo', [76, 111]).
	iregexp_char_property('Lt', [76, 116]).
	iregexp_char_property('Lu', [76, 117]).
	iregexp_char_property('M', [77]).
	iregexp_char_property('Mc', [77, 99]).
	iregexp_char_property('Me', [77, 101]).
	iregexp_char_property('Mn', [77, 110]).
	iregexp_char_property('N', [78]).
	iregexp_char_property('Nd', [78, 100]).
	iregexp_char_property('Nl', [78, 108]).
	iregexp_char_property('No', [78, 111]).
	iregexp_char_property('P', [80]).
	iregexp_char_property('Pc', [80, 99]).
	iregexp_char_property('Pd', [80, 100]).
	iregexp_char_property('Pe', [80, 101]).
	iregexp_char_property('Pf', [80, 102]).
	iregexp_char_property('Pi', [80, 105]).
	iregexp_char_property('Po', [80, 111]).
	iregexp_char_property('Ps', [80, 115]).
	iregexp_char_property('S', [83]).
	iregexp_char_property('Sc', [83, 99]).
	iregexp_char_property('Sk', [83, 107]).
	iregexp_char_property('Sm', [83, 109]).
	iregexp_char_property('So', [83, 111]).
	iregexp_char_property('Z', [90]).
	iregexp_char_property('Zl', [90, 108]).
	iregexp_char_property('Zp', [90, 112]).
	iregexp_char_property('Zs', [90, 115]).
	iregexp_char_property('C', [67]).
	iregexp_char_property('Cc', [67, 99]).
	iregexp_char_property('Cf', [67, 102]).
	iregexp_char_property('Cn', [67, 110]).
	iregexp_char_property('Co', [67, 111]).

	iregexp_class_items_valid(true, Items) :-
		Items \== [].
	iregexp_class_items_valid(false, Items) :-
		Items \== [].

	iregexp_normal_char(Code) :-
		integer(Code),
		\+ iregexp_meta_char(Code).

	iregexp_meta_char(40).
	iregexp_meta_char(41).
	iregexp_meta_char(42).
	iregexp_meta_char(43).
	iregexp_meta_char(46).
	iregexp_meta_char(63).
	iregexp_meta_char(91).
	iregexp_meta_char(92).
	iregexp_meta_char(93).
	iregexp_meta_char(123).
	iregexp_meta_char(124).
	iregexp_meta_char(125).

	parse_iregexp_escaped_code([Escape| Rest], Rest, Code) :-
		iregexp_escaped_code(Escape, Code).

	iregexp_escaped_code(40, 40).
	iregexp_escaped_code(41, 41).
	iregexp_escaped_code(42, 42).
	iregexp_escaped_code(43, 43).
	iregexp_escaped_code(45, 45).
	iregexp_escaped_code(46, 46).
	iregexp_escaped_code(63, 63).
	iregexp_escaped_code(91, 91).
	iregexp_escaped_code(92, 92).
	iregexp_escaped_code(93, 93).
	iregexp_escaped_code(94, 94).
	iregexp_escaped_code(0'n, 10).
	iregexp_escaped_code(0'r, 13).
	iregexp_escaped_code(0't, 9).
	iregexp_escaped_code(123, 123).
	iregexp_escaped_code(124, 124).
	iregexp_escaped_code(125, 125).

	iregexp_class_char(Code) :-
		integer(Code),
		Code =\= 45,
		Code =\= 93,
		Code =\= 92.

	iregexp_valid(regexp(Branches)) :-
		iregexp_branches_valid(Branches, false).

	iregexp_valid_in_context(regexp(Branches), InsideExplicitRange) :-
		iregexp_branches_valid(Branches, InsideExplicitRange).

	iregexp_branches_valid([], _).
	iregexp_branches_valid([Branch| Branches], InsideExplicitRange) :-
		iregexp_branch_valid(Branch, InsideExplicitRange),
		iregexp_branches_valid(Branches, InsideExplicitRange).

	iregexp_branch_valid([], _).
	iregexp_branch_valid([Piece| Pieces], InsideExplicitRange) :-
		iregexp_piece_valid(Piece, InsideExplicitRange),
		iregexp_branch_valid(Pieces, InsideExplicitRange).

	iregexp_piece_valid(piece(Atom, Quantifier), InsideExplicitRange) :-
		iregexp_quantifier_valid(Quantifier),
		(   InsideExplicitRange == true,
			iregexp_explicit_range_quantifier(Quantifier) ->
			fail
		;   true
		),
		(   iregexp_explicit_range_quantifier(Quantifier) ->
			NextInsideExplicitRange = true
		;   NextInsideExplicitRange = InsideExplicitRange
		),
		iregexp_atom_valid(Atom, NextInsideExplicitRange).

	iregexp_atom_valid(literal(_), _).
	iregexp_atom_valid(any, _).
	iregexp_atom_valid(property(_), _).
	iregexp_atom_valid(class(Negated, Items), _) :-
		iregexp_class_items_valid(Negated, Items).
	iregexp_atom_valid(group(Regexp), InsideExplicitRange) :-
		iregexp_valid_in_context(Regexp, InsideExplicitRange).

	iregexp_quantifier_valid(repeat(Min, Max)) :-
		integer(Min),
		Min >= 0,
		(   Max == many ->
			true
		;   integer(Max),
			Max >= Min
		),
		(   iregexp_explicit_range_quantifier(repeat(Min, Max)) ->
			iregexp_range_quantifier_safe(Min, Max)
		;   true
		).

	iregexp_explicit_range_quantifier(repeat(Min, Max)) :-
		(   integer(Max) ->
			(Min =\= 1; Max =\= 1)
		;   Max == many,
			Min =\= 0,
			Min =\= 1
		).

	iregexp_range_quantifier_safe(Min, many) :-
		iregexp_range_quantifier_upper_limit(UpperLimit),
		Min =< UpperLimit.
	iregexp_range_quantifier_safe(Min, Max) :-
		iregexp_range_quantifier_upper_limit(UpperLimit),
		iregexp_range_quantifier_span_limit(SpanLimit),
		Max =< UpperLimit,
		Max - Min =< SpanLimit.

	iregexp_range_quantifier_upper_limit(1024).

	iregexp_range_quantifier_span_limit(1024).

	iregexp_matches(Regexp, Codes) :-
		iregexp_matches(Regexp, Codes, []).

	iregexp_matches(Regexp, Codes0, Codes) :-
		Regexp = regexp(Branches),
		iregexp_branch_member(Branch, Branches),
		iregexp_branch_matches(Branch, Codes0, Codes).

	iregexp_searches(Regexp, Codes) :-
		iregexp_suffix(Codes, Suffix),
		iregexp_matches(Regexp, Suffix, _),
		!.

	iregexp_suffix(Codes, Codes).
	iregexp_suffix([_| Codes], Suffix) :-
		iregexp_suffix(Codes, Suffix).

	iregexp_branch_member(Branch, [Branch| _]).
	iregexp_branch_member(Branch, [_| Branches]) :-
		iregexp_branch_member(Branch, Branches).

	iregexp_branch_matches([], Codes, Codes).
	iregexp_branch_matches([Piece| Pieces], Codes0, Codes) :-
		iregexp_piece_matches(Piece, Codes0, Codes1),
		iregexp_branch_matches(Pieces, Codes1, Codes).

	iregexp_piece_matches(piece(Atom, repeat(Min, Max)), Codes0, Codes) :-
		iregexp_repeat_matches(Atom, Min, Max, Codes0, Codes).

	iregexp_repeat_matches(_, 0, 0, Codes, Codes).
	iregexp_repeat_matches(_, Min, _, Codes0, Codes) :-
		Min =< 0,
		Codes = Codes0.
	iregexp_repeat_matches(Atom, Min, Max, Codes0, Codes) :-
		iregexp_repeat_allowed(Max),
		iregexp_atom_matches(Atom, Codes0, Codes1),
		iregexp_next_repeat(Min, Max, NextMin, NextMax),
		(   Codes1 == Codes0 ->
			Min > 0,
			iregexp_repeat_matches(Atom, NextMin, NextMax, Codes1, Codes)
		;   iregexp_repeat_matches(Atom, NextMin, NextMax, Codes1, Codes)
		).

	iregexp_repeat_allowed(many) :-
		!.
	iregexp_repeat_allowed(Max) :-
		integer(Max),
		Max > 0.

	iregexp_next_repeat(Min, many, NextMin, many) :-
		NextMin is max(0, Min - 1).
	iregexp_next_repeat(Min, Max, NextMin, NextMax) :-
		integer(Max),
		NextMax is Max - 1,
		NextMin is max(0, Min - 1).

	iregexp_atom_matches(literal(Code), [Code| Rest], Rest).
	iregexp_atom_matches(any, [Code| Rest], Rest) :-
		Code =\= 10,
		Code =\= 13.
	iregexp_atom_matches(property(Property), [Code| Rest], Rest) :-
		iregexp_property_matches(Property, Code).
	iregexp_atom_matches(group(Regexp), Codes0, Codes) :-
		iregexp_matches(Regexp, Codes0, Codes).
	iregexp_atom_matches(class(Negated, Items), [Code| Rest], Rest) :-
		iregexp_class_match(Negated, Items, Code).

	iregexp_class_match(false, Items, Code) :-
		iregexp_class_contains(Items, Code).
	iregexp_class_match(true, Items, Code) :-
		\+ iregexp_class_contains(Items, Code).

	iregexp_class_contains([Item| _], Code) :-
		iregexp_class_item_contains(Item, Code),
		!.
	iregexp_class_contains([_| Items], Code) :-
		iregexp_class_contains(Items, Code).

	iregexp_class_item_contains(code(Code), Code).
	iregexp_class_item_contains(property(Property), Code) :-
		iregexp_property_matches(Property, Code).
	iregexp_class_item_contains(range(First, Last), Code) :-
		Code >= First,
		Code =< Last.

	iregexp_property_matches(category(Property), Code) :-
		iregexp_unicode_category(Code, Category),
		iregexp_property_category(Property, Category).
	iregexp_property_matches(complement_category(Property), Code) :-
		iregexp_unicode_category(Code, Category),
		\+ iregexp_property_category(Property, Category).

	iregexp_property_category(Property, Category) :-
		atom_codes(Property, PropertyCodes),
		atom_codes(Category, CategoryCodes),
		iregexp_property_category_codes(PropertyCodes, CategoryCodes).

	iregexp_property_category_codes([ClassCode], [ClassCode, _SubcategoryCode]) :-
		!.
	iregexp_property_category_codes([ClassCode, SubcategoryCode], [ClassCode, SubcategoryCode]).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == xvm; Dialect == swi))).
		iregexp_property_support_available.

		iregexp_unicode_category(Code, Category) :-
			unicode_property(Code, category(Category)).
	:- else.
		iregexp_property_support_available :-
			fail.

		iregexp_unicode_category(_, _) :-
			fail.
	:- endif.

	compare_filter_values(eq, Left, Right, Result) :-
		compare_equal_values(Left, Right, Result).
	compare_filter_values(ne, Left, Right, Result) :-
		compare_equal_values(Left, Right, EqualResult),
		logical_not(EqualResult, Result).
	compare_filter_values(lt, Left, Right, Result) :-
		compare_less_values(Left, Right, Result).
	compare_filter_values(le, Left, Right, Result) :-
		compare_less_values(Left, Right, LessResult),
		compare_equal_values(Left, Right, EqualResult),
		logical_or(LessResult, EqualResult, Result).
	compare_filter_values(gt, Left, Right, Result) :-
		compare_less_values(Right, Left, Result).
	compare_filter_values(ge, Left, Right, Result) :-
		compare_less_values(Right, Left, GreaterResult),
		compare_equal_values(Left, Right, EqualResult),
		logical_or(GreaterResult, EqualResult, Result).

	compare_equal_values(nothing, nothing, true) :-
		!.
	compare_equal_values(nothing, _, false) :-
		!.
	compare_equal_values(_, nothing, false) :-
		!.
	compare_equal_values(Left, Right, Result) :-
		(	json_values_equal(Left, Right) ->
			Result = true
		;	Result = false
		).

	compare_less_values(nothing, _, false) :-
		!.
	compare_less_values(_, nothing, false) :-
		!.
	compare_less_values(Left, Right, Result) :-
		(	json_values_less(Left, Right) ->
			Result = true
		;	Result = false
		).

	json_values_equal(Left, Right) :-
		number(Left),
		number(Right),
		!,
		Left =:= Right.
	json_values_equal(Left, Right) :-
		atom(Left),
		atom(Right),
		!,
		Left == Right.
	json_values_equal(Left, Right) :-
		proper_list(Left),
		proper_list(Right),
		!,
		json_arrays_equal(Left, Right).
	json_values_equal(Left, Right) :-
		object_entries(Left, LeftEntries),
		object_entries(Right, RightEntries),
		!,
		json_objects_equal(LeftEntries, RightEntries).

	json_arrays_equal([], []).
	json_arrays_equal([Left| LeftArray], [Right| RightArray]) :-
		json_values_equal(Left, Right),
		json_arrays_equal(LeftArray, RightArray).

	json_objects_equal(LeftEntries, RightEntries) :-
		length(LeftEntries, Length),
		length(RightEntries, Length),
		json_objects_equal_(LeftEntries, RightEntries).

	json_objects_equal_([], []).
	json_objects_equal_([entry(Name, LeftValue)| LeftEntries], RightEntries) :-
		remove_object_entry(Name, RightEntries, RightValue, RemainingRightEntries),
		json_values_equal(LeftValue, RightValue),
		json_objects_equal_(LeftEntries, RemainingRightEntries).

	remove_object_entry(Name, [entry(Name, Value)| Entries], Value, Entries) :-
		!.
	remove_object_entry(Name, [Entry| Entries], Value, [Entry| RemainingEntries]) :-
		remove_object_entry(Name, Entries, Value, RemainingEntries).

	json_values_less(Left, Right) :-
		number(Left),
		number(Right),
		!,
		Left < Right.
	json_values_less(Left, Right) :-
		atom(Left),
		atom(Right),
		\+ json_special_literal(Left),
		\+ json_special_literal(Right),
		!,
		atom_codes(Left, LeftCodes),
		atom_codes(Right, RightCodes),
		codes_less_than(LeftCodes, RightCodes).

	json_special_literal(@true).
	json_special_literal(@false).
	json_special_literal(@null).

	codes_less_than([], [_| _]).
	codes_less_than([LeftCode| LeftCodes], [RightCode| RightCodes]) :-
		(	LeftCode < RightCode ->
			true
		;	LeftCode =:= RightCode,
			codes_less_than(LeftCodes, RightCodes)
		).

	normalized_nodes([], []).
	normalized_nodes([inode(Location, Value)| InternalNodes], [node(Path, Value)| Nodes]) :-
		location_text(Location, Path),
		normalized_nodes(InternalNodes, Nodes).

	node_values([], []).
	node_values([inode(_, Value)| InternalNodes], [Value| Values]) :-
		node_values(InternalNodes, Values).

	node_paths([], []).
	node_paths([inode(Location, _)| InternalNodes], [Path| Paths]) :-
		location_text(Location, Path),
		node_paths(InternalNodes, Paths).

	location_text(Location, Path) :-
		location_codes(Location, Codes),
		codes_text(Codes, Path).

	location_codes(Location, Codes) :-
		location_codes(Location, [0'$], Codes).

	location_codes([], Codes, Codes).
	location_codes([member(Name)| Tokens], Codes0, Codes) :-
		!,
		append(Codes0, [91, 39], Codes1),
		normalized_name_codes(Name, Codes1, Codes2),
		append(Codes2, [39, 93], Codes3),
		location_codes(Tokens, Codes3, Codes).
	location_codes([index(Index)| Tokens], Codes0, Codes) :-
		number_codes(Index, IndexCodes),
		append(Codes0, [91], Codes1),
		append(Codes1, IndexCodes, Codes2),
		append(Codes2, [93], Codes3),
		location_codes(Tokens, Codes3, Codes).

	query_codes(Query, Codes) :-
		query_codes(Query, [], Codes).

	query_codes(json_path(Segments), Codes0, Codes) :-
		proper_list(Segments),
		!,
		append(Codes0, [0'$], Codes1),
		segments_codes(Segments, Codes1, Codes).
	query_codes(_, _, _) :-
		fail.

	segments_codes([], Codes, Codes).
	segments_codes([Segment| Segments], Codes0, Codes) :-
		segment_codes(Segment, Codes0, Codes1),
		segments_codes(Segments, Codes1, Codes).

	segment_codes(child(Selectors), Codes0, Codes) :-
		proper_list(Selectors),
		!,
		append(Codes0, [91], Codes1),
		selectors_codes(Selectors, Codes1, Codes2),
		append(Codes2, [93], Codes3),
		segments_codes([], Codes3, Codes).
	segment_codes(descendant(Selectors), Codes0, Codes) :-
		proper_list(Selectors),
		!,
		append(Codes0, [46, 46, 91], Codes1),
		selectors_codes(Selectors, Codes1, Codes2),
		append(Codes2, [93], Codes).
	segment_codes(_, _, _) :-
		fail.

	selectors_codes([Selector], Codes0, Codes) :-
		!,
		selector_codes(Selector, Codes0, Codes).
	selectors_codes([Selector| Selectors], Codes0, Codes) :-
		selector_codes(Selector, Codes0, Codes1),
		append(Codes1, [44, 32], Codes2),
		selectors_codes(Selectors, Codes2, Codes).

	selector_codes(name(Token), Codes0, Codes) :-
		!,
		append(Codes0, [39], Codes1),
		text_codes(Token, TokenCodes),
		append_escaped_codes(TokenCodes, Codes1, Codes2),
		append(Codes2, [39], Codes).
	selector_codes(wildcard, Codes0, Codes) :-
		!,
		append(Codes0, [0'*], Codes).
	selector_codes(index(Index), Codes0, Codes) :-
		integer(Index),
		!,
		number_codes(Index, IndexCodes),
		append(Codes0, IndexCodes, Codes).
	selector_codes(slice(Start, End, Step), Codes0, Codes) :-
		!,
		slice_codes(Start, End, Step, Codes0, Codes).
	selector_codes(filter(Expression), Codes0, Codes) :-
		!,
		append(Codes0, [63], Codes1),
		logical_expr_codes(Expression, 1, Codes1, Codes).
	selector_codes(_, _, _) :-
		fail.

	logical_expr_codes(Expression, ContextPrecedence, Codes0, Codes) :-
		logical_expr_codes_(Expression, ExpressionPrecedence, Codes0, InnerCodes),
		(	ExpressionPrecedence < ContextPrecedence ->
			append(Codes0, [40], WrappedCodes),
			logical_expr_codes_(Expression, _, WrappedCodes, WrappedInnerCodes),
			append(WrappedInnerCodes, [41], Codes)
		;	Codes = InnerCodes
		).

	logical_expr_codes_(or(Left, Right), 1, Codes0, Codes) :-
		logical_expr_codes(Left, 1, Codes0, Codes1),
		append(Codes1, [32, 124, 124, 32], Codes2),
		logical_expr_codes(Right, 2, Codes2, Codes).
	logical_expr_codes_(and(Left, Right), 2, Codes0, Codes) :-
		logical_expr_codes(Left, 2, Codes0, Codes1),
		append(Codes1, [32, 38, 38, 32], Codes2),
		logical_expr_codes(Right, 3, Codes2, Codes).
	logical_expr_codes_(not(Expression), 4, Codes0, Codes) :-
		append(Codes0, [33], Codes1),
		logical_expr_codes(Expression, 4, Codes1, Codes).
	logical_expr_codes_(exists(Path), 5, Codes0, Codes) :-
		path_codes(Path, Codes0, Codes).
	logical_expr_codes_(test_function(Function), 5, Codes0, Codes) :-
		function_codes(Function, Codes0, Codes).
	logical_expr_codes_(compare(Operator, Left, Right), 3, Codes0, Codes) :-
		comparable_codes(Left, Codes0, Codes1),
		comparison_operator_codes(Operator, OperatorCodes),
		append(Codes1, [32], Codes2),
		append(Codes2, OperatorCodes, Codes3),
		append(Codes3, [32], Codes4),
		comparable_codes(Right, Codes4, Codes).

	comparison_operator_codes(eq, [61, 61]).
	comparison_operator_codes(ne, [33, 61]).
	comparison_operator_codes(lt, [60]).
	comparison_operator_codes(le, [60, 61]).
	comparison_operator_codes(gt, [62]).
	comparison_operator_codes(ge, [62, 61]).

	comparable_codes(literal(Value), Codes0, Codes) :-
		literal_codes(Value, Codes0, Codes).
	comparable_codes(path(Path), Codes0, Codes) :-
		path_codes(Path, Codes0, Codes).
	comparable_codes(function(Name, Arguments), Codes0, Codes) :-
		function_codes(function(Name, Arguments), Codes0, Codes).

	literal_codes(Value, Codes0, Codes) :-
		number(Value),
		!,
		number_codes(Value, ValueCodes),
		append(Codes0, ValueCodes, Codes).
	literal_codes(@true, Codes0, Codes) :-
		!,
		append(Codes0, [116, 114, 117, 101], Codes).
	literal_codes(@false, Codes0, Codes) :-
		!,
		append(Codes0, [102, 97, 108, 115, 101], Codes).
	literal_codes(@null, Codes0, Codes) :-
		!,
		append(Codes0, [110, 117, 108, 108], Codes).
	literal_codes(Value, Codes0, Codes) :-
		atom(Value),
		!,
		append(Codes0, [39], Codes1),
		atom_codes(Value, ValueCodes),
		append_escaped_codes(ValueCodes, Codes1, Codes2),
		append(Codes2, [39], Codes).

	path_codes(path(root, Segments), Codes0, Codes) :-
		!,
		append(Codes0, [36], Codes1),
		segments_codes(Segments, Codes1, Codes).
	path_codes(path(current, Segments), Codes0, Codes) :-
		append(Codes0, [64], Codes1),
		segments_codes(Segments, Codes1, Codes).

	function_codes(function(Name, Arguments), Codes0, Codes) :-
		atom_codes(Name, NameCodes),
		append(Codes0, NameCodes, Codes1),
		append(Codes1, [40], Codes2),
		function_arguments_codes(Arguments, Codes2, Codes3),
		append(Codes3, [41], Codes).

	function_arguments_codes([], Codes, Codes).
	function_arguments_codes([Argument], Codes0, Codes) :-
		!,
		function_argument_codes(Argument, Codes0, Codes).
	function_arguments_codes([Argument| Arguments], Codes0, Codes) :-
		function_argument_codes(Argument, Codes0, Codes1),
		append(Codes1, [44, 32], Codes2),
		function_arguments_codes(Arguments, Codes2, Codes).

	function_argument_codes(literal(Value), Codes0, Codes) :-
		literal_codes(Value, Codes0, Codes).
	function_argument_codes(path(Path), Codes0, Codes) :-
		path_codes(Path, Codes0, Codes).
	function_argument_codes(logical(Expression), Codes0, Codes) :-
		logical_expr_codes(Expression, 1, Codes0, Codes).
	function_argument_codes(function(Name, Arguments), Codes0, Codes) :-
		function_codes(function(Name, Arguments), Codes0, Codes).

	slice_codes(Start, End, default, Codes0, Codes) :-
		!,
		slice_part_codes(Start, Codes0, Codes1),
		append(Codes1, [58], Codes2),
		slice_part_codes(End, Codes2, Codes).
	slice_codes(Start, End, Step, Codes0, Codes) :-
		slice_part_codes(Start, Codes0, Codes1),
		append(Codes1, [58], Codes2),
		slice_part_codes(End, Codes2, Codes3),
		append(Codes3, [58], Codes4),
		slice_part_codes(Step, Codes4, Codes).

	slice_part_codes(default, Codes, Codes) :-
		!.
	slice_part_codes(Value, Codes0, Codes) :-
		integer(Value),
		number_codes(Value, ValueCodes),
		append(Codes0, ValueCodes, Codes).

	parse_source_codes(Source, _) :-
		var(Source),
		instantiation_error.
	parse_source_codes(codes(Codes), Codes) :-
		valid_code_list(Codes),
		!.
	parse_source_codes(codes(_), _) :-
		fail.
	parse_source_codes(chars(Chars), Codes) :-
		valid_char_list(Chars),
		!,
		chars_to_codes(Chars, Codes).
	parse_source_codes(chars(_), _) :-
		fail.
	parse_source_codes(atom(Atom), Codes) :-
		!,
		atom_codes(Atom, Codes).
	parse_source_codes(Source, _) :-
		domain_error(json_path_source, Source).

	generate_sink_codes(codes(Codes), QueryCodes) :-
		!,
		Codes = QueryCodes.
	generate_sink_codes(chars(Chars), QueryCodes) :-
		!,
		codes_to_chars(QueryCodes, Chars).
	generate_sink_codes(atom(Atom), QueryCodes) :-
		!,
		atom_codes(Atom, QueryCodes).
	generate_sink_codes(Sink, _) :-
		domain_error(json_path_sink, Sink).

	text_codes(Token, Codes) :-
		(	_StringRepresentation_ == atom ->
			atom(Token),
			atom_codes(Token, Codes)
		;	_StringRepresentation_ == chars ->
			Token = chars(Chars),
			chars_to_codes(Chars, Codes)
		;	_StringRepresentation_ == codes ->
			Token = codes(Codes)
		;	domain_error(json_path_string_representation, _StringRepresentation_)
		),
		!.
	text_codes(Token, _) :-
		domain_error(json_path_query, Token).

	codes_text(Codes, Token) :-
		(	_StringRepresentation_ == atom ->
			atom_codes(Token, Codes)
		;	_StringRepresentation_ == chars ->
			codes_to_chars(Codes, Chars),
			Token = chars(Chars)
		;	_StringRepresentation_ == codes ->
			Token = codes(Codes)
		;	domain_error(json_path_string_representation, _StringRepresentation_)
		).

	text_atom(Atom, Atom) :-
		atom(Atom),
		!.
	text_atom(chars(Chars), Atom) :-
		!,
		chars_to_codes(Chars, Codes),
		atom_codes(Atom, Codes).
	text_atom(codes(Codes), Atom) :-
		!,
		atom_codes(Atom, Codes).
	text_atom(_, _) :-
		fail.

	object_entries({}, []) :-
		!.
	object_entries({Pairs}, Entries) :-
		!,
		pairs_term_entries(Pairs, Entries).
	object_entries(json(Pairs), Entries) :-
		proper_list(Pairs),
		!,
		pairs_list_entries(Pairs, Entries).

	pairs_term_entries((Pair, Pairs), [Entry| Entries]) :-
		!,
		pair_entry(Pair, Entry),
		pairs_term_entries(Pairs, Entries).
	pairs_term_entries(Pair, [Entry]) :-
		pair_entry(Pair, Entry).

	pairs_list_entries([], []).
	pairs_list_entries([Pair| Pairs], [Entry| Entries]) :-
		pair_entry(Pair, Entry),
		pairs_list_entries(Pairs, Entries).

	pair_entry(Key-Value, entry(Name, Value)) :-
		text_atom(Key, Name),
		!.
	pair_entry(Key=Value, entry(Name, Value)) :-
		text_atom(Key, Name),
		!.
	pair_entry(':'(Key, Value), entry(Name, Value)) :-
		text_atom(Key, Name).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	normalized_name_codes(Name, Codes0, Codes) :-
		atom_codes(Name, NameCodes),
		append_escaped_codes(NameCodes, Codes0, Codes).

	append_escaped_codes([], Codes, Codes).
	append_escaped_codes([Code| Remaining], Codes0, Codes) :-
		escaped_code_codes(Code, EscapedCodeCodes),
		append(Codes0, EscapedCodeCodes, Codes1),
		append_escaped_codes(Remaining, Codes1, Codes).

	escaped_code_codes(92, [92, 92]) :-
		!.
	escaped_code_codes(39, [92, 39]) :-
		!.
	escaped_code_codes(8, [92, 98]) :-
		!.
	escaped_code_codes(9, [92, 116]) :-
		!.
	escaped_code_codes(10, [92, 110]) :-
		!.
	escaped_code_codes(12, [92, 102]) :-
		!.
	escaped_code_codes(13, [92, 114]) :-
		!.
	escaped_code_codes(Code, Codes) :-
		Code >= 32,
		!,
		Codes = [Code].
	escaped_code_codes(Code, [92, 117, 48, 48, High, Low]) :-
		HighValue is Code // 16,
		LowValue is Code mod 16,
		hexadecimal_digit_code(HighValue, High),
		hexadecimal_digit_code(LowValue, Low).

	hexadecimal_digit_code(Value, Code) :-
		Value < 10,
		!,
		Code is 0'0 + Value.
	hexadecimal_digit_code(Value, Code) :-
		Code is 0'a + Value - 10.

	json_path_query(json_path(Segments)) -->
		[0'$],
		segments(Segments),
		end_of_input.

	segments([Segment| Segments]) -->
		blanks,
		segment(Segment),
		!,
		segments(Segments).
	segments([]) -->
		blanks.

	segment(Segment) -->
		descendant_segment(Segment).
	segment(Segment) -->
		child_segment(Segment).

	child_segment(child(Selectors)) -->
		[0'[],
		blanks,
		selectors(Selectors),
		blanks,
		[0']].
	child_segment(child([wildcard])) -->
		[0'., 0'*].
	child_segment(child([name(Name)])) -->
		[0'.],
		member_name_shorthand(Name).

	descendant_segment(descendant(Selectors)) -->
		[0'., 0'., 0'[] ,
		blanks,
		selectors(Selectors),
		blanks,
		[0']].
	descendant_segment(descendant([wildcard])) -->
		[0'., 0'., 0'*].
	descendant_segment(descendant([name(Name)])) -->
		[0'., 0'.],
		member_name_shorthand(Name).

	selectors([Selector| Selectors]) -->
		selector(Selector),
		more_selectors(Selectors).

	more_selectors([Selector| Selectors]) -->
		blanks,
		[0',],
		blanks,
		selector(Selector),
		!,
		more_selectors(Selectors).
	more_selectors([]) -->
		[].

	selector(name(Name)) -->
		string_literal(Name).
	selector(wildcard) -->
		[0'*].
	selector(filter(Expression)) -->
		[0'?],
		blanks,
		logical_expr(Expression).
	selector(slice(Start, End, Step)) -->
		optional_int(Start),
		blanks,
		[0':],
		blanks,
		optional_int(End),
		optional_step(Step).
	selector(index(Index)) -->
		int_value(Index).

	optional_step(Step) -->
		[0':],
		blanks,
		optional_int(Step),
		!.
	optional_step(default) -->
		[].

	optional_int(Integer) -->
		int_value(Integer),
		!.
	optional_int(default) -->
		[].

	logical_expr(Expression) -->
		logical_or_expr(Expression).

	logical_or_expr(Expression) -->
		logical_and_expr(Left),
		logical_or_expr_tail(Left, Expression).

	logical_or_expr_tail(Left, Expression) -->
		blanks,
		[0'|, 0'|],
		blanks,
		logical_and_expr(Right),
		!,
		logical_or_expr_tail(or(Left, Right), Expression).
	logical_or_expr_tail(Expression, Expression) -->
		[].

	logical_and_expr(Expression) -->
		basic_expr(Left),
		logical_and_expr_tail(Left, Expression).

	logical_and_expr_tail(Left, Expression) -->
		blanks,
		[0'&, 0'&],
		blanks,
		basic_expr(Right),
		!,
		logical_and_expr_tail(and(Left, Right), Expression).
	logical_and_expr_tail(Expression, Expression) -->
		[].

	basic_expr(Expression) -->
		paren_expr(Expression).
	basic_expr(Expression) -->
		comparison_expr(Expression).
	basic_expr(Expression) -->
		test_expr(Expression).

	paren_expr(Expression) -->
		optional_not(Present),
		[0'(],
		blanks,
		logical_expr(InnerExpression),
		blanks,
		[0')],
		{apply_optional_not(Present, InnerExpression, Expression)}.

	comparison_expr(compare(Operator, Left, Right)) -->
		comparable(Left),
		blanks,
		comparison_operator(Operator),
		blanks,
		comparable(Right).

	test_expr(Expression) -->
		optional_not(Present),
		filter_query(Path),
		{apply_optional_not(Present, exists(Path), Expression)}.
	test_expr(Expression) -->
		optional_not(Present),
		function_expr(Function),
		{apply_optional_not(Present, test_function(Function), Expression)}.

	optional_not(true) -->
		[0'!],
		blanks,
		!.
	optional_not(false) -->
		[].

	apply_optional_not(true, Expression, not(Expression)).
	apply_optional_not(false, Expression, Expression).

	comparison_operator(eq) -->
		[0'=, 0'=],
		!.
	comparison_operator(ne) -->
		[0'!, 0'=],
		!.
	comparison_operator(le) -->
		[0'<, 0'=],
		!.
	comparison_operator(ge) -->
		[0'>, 0'=],
		!.
	comparison_operator(lt) -->
		[0'<],
		!.
	comparison_operator(gt) -->
		[0'>].

	comparable(literal(Value)) -->
		literal_value(Value),
		!.
	comparable(path(Path)) -->
		singular_query(Path),
		!.
	comparable(function(Name, Arguments)) -->
		function_expr(value, function(Name, Arguments)).

	function_expr(function(Name, Arguments)) -->
		function_expr(_, function(Name, Arguments)).

	function_expr(ResultType, function(Name, Arguments)) -->
		function_name(Name),
		{function_signature(Name, ParameterTypes, ResultType)},
		[0'(],
		blanks,
		function_arguments(ParameterTypes, Arguments),
		blanks,
		[0')].

	function_arguments([], []) -->
		[].
	function_arguments([ParameterType| ParameterTypes], [Argument| Arguments]) -->
		function_argument(ParameterType, Argument),
		more_function_arguments(ParameterTypes, Arguments).

	more_function_arguments([], []) -->
		[].
	more_function_arguments([ParameterType| ParameterTypes], [Argument| Arguments]) -->
		blanks,
		[0',],
		blanks,
		function_argument(ParameterType, Argument),
		!,
		more_function_arguments(ParameterTypes, Arguments).

	function_argument(value, literal(Value)) -->
		literal_value(Value),
		!.
	function_argument(value, path(Path)) -->
		singular_query(Path),
		!.
	function_argument(value, function(Name, Arguments)) -->
		function_expr(value, function(Name, Arguments)).
	function_argument(nodes, path(Path)) -->
		filter_query(Path),
		!.
	function_argument(nodes, function(Name, Arguments)) -->
		function_expr(nodes, function(Name, Arguments)).
	function_argument(logical, function(Name, Arguments)) -->
		function_expr(logical, function(Name, Arguments)),
		!.
	function_argument(logical, logical(Expression)) -->
		logical_expr(Expression).

	filter_query(path(root, Segments)) -->
		[0'$],
		segments(Segments).
	filter_query(path(current, Segments)) -->
		[0'@],
		segments(Segments).

	singular_query(path(root, Segments)) -->
		[0'$],
		singular_query_segments(Segments).
	singular_query(path(current, Segments)) -->
		[0'@],
		singular_query_segments(Segments).

	singular_query_segments([Segment| Segments]) -->
		blanks,
		singular_query_segment(Segment),
		!,
		singular_query_segments(Segments).
	singular_query_segments([]) -->
		blanks.

	singular_query_segment(child([name(Name)])) -->
		[0'[] ,
		blanks,
		string_literal(Name),
		blanks,
		[0']].
	singular_query_segment(child([index(Index)])) -->
		[0'[] ,
		blanks,
		int_value(Index),
		blanks,
		[0']].
	singular_query_segment(child([name(Name)])) -->
		[0'.],
		member_name_shorthand(Name).

	literal_value(Value) -->
		string_literal_atom(Value).
	literal_value(Value) -->
		number_literal(Value).
	literal_value(@true) -->
		[0't, 0'r, 0'u, 0'e].
	literal_value(@false) -->
		[0'f, 0'a, 0'l, 0's, 0'e].
	literal_value(@null) -->
		[0'n, 0'u, 0'l, 0'l].

	string_literal_atom(Atom) -->
		[0'\'],
		quoted_string_codes(0'\', Codes),
		[0'\'],
		{atom_codes(Atom, Codes)}.
	string_literal_atom(Atom) -->
		[0'\"],
		quoted_string_codes(0'\", Codes),
		[0'\"],
		{atom_codes(Atom, Codes)}.

	number_literal(Number) -->
		number_int_part_codes(IntCodes),
		number_fraction_codes(FractionCodes),
		number_exponent_codes(ExponentCodes),
		{	append(IntCodes, FractionCodes, IntFractionCodes),
			append(IntFractionCodes, ExponentCodes, NumberCodes),
			number_codes(Number, NumberCodes)
		}.

	number_int_part_codes([0'-, 0'0]) -->
		[0'-, 0'0],
		!.
	number_int_part_codes([0'0]) -->
		[0'0],
		!.
	number_int_part_codes([0'-| Digits]) -->
		[0'-],
		non_zero_digits(Digits),
		!.
	number_int_part_codes(Digits) -->
		non_zero_digits(Digits).

	number_fraction_codes([0'.| Digits]) -->
		[0'.],
		digits1(Digits),
		!.
	number_fraction_codes([]) -->
		[].

	number_exponent_codes([ExponentMarker, Sign| Digits]) -->
		exponent_marker(ExponentMarker),
		exponent_sign(Sign),
		digits1(Digits),
		!.
	number_exponent_codes([ExponentMarker| Digits]) -->
		exponent_marker(ExponentMarker),
		digits1(Digits),
		!.
	number_exponent_codes([]) -->
		[].

	exponent_marker(0'e) -->
		[0'e].
	exponent_marker(0'E) -->
		[0'E].

	exponent_sign(0'+) -->
		[0'+].
	exponent_sign(0'-) -->
		[0'-].

	digits1([Digit| Digits]) -->
		[Digit],
		{Digit >= 0'0, Digit =< 0'9},
		digit_codes(Digits).

	function_name(Name) -->
		function_name_first(FirstCode),
		function_name_tail(TailCodes),
		{atom_codes(Name, [FirstCode| TailCodes])}.

	function_name_first(Code) -->
		[Code],
		{Code >= 0'a, Code =< 0'z}.

	function_name_tail([Code| Codes]) -->
		[Code],
		{function_name_char(Code)},
		!,
		function_name_tail(Codes).
	function_name_tail([]) -->
		[].

	function_name_char(Code) :-
		Code >= 0'a,
		Code =< 0'z.
	function_name_char(Code) :-
		Code >= 0'0,
		Code =< 0'9.
	function_name_char(0'_).

	member_name_shorthand(Name) -->
		member_name_codes(Codes),
		{codes_text(Codes, Name)}.

	member_name_codes([Code| Codes]) -->
		[Code],
		{is_member_name_first(Code)},
		member_name_tail_codes(Codes).

	member_name_tail_codes([Code| Codes]) -->
		[Code],
		{is_member_name_char(Code)},
		!,
		member_name_tail_codes(Codes).
	member_name_tail_codes([]) -->
		[].

	string_literal(Name) -->
		[0'\'],
		quoted_string_codes(0'\', Codes),
		[0'\'],
		{codes_text(Codes, Name)}.
	string_literal(Name) -->
		[0'\"],
		quoted_string_codes(0'\", Codes),
		[0'\"],
		{codes_text(Codes, Name)}.

	quoted_string_codes(Quote, [Code| Codes]) -->
		escaped_or_unescaped_code(Quote, Code),
		!,
		quoted_string_codes(Quote, Codes).
	quoted_string_codes(Quote, [0'\\, Code| Codes]) -->
		[0'\\, Code],
		{passthrough_escaped_string_code(Code)},
		!,
		quoted_string_codes(Quote, Codes).
	quoted_string_codes(_, []) -->
		[].

	passthrough_escaped_string_code(Code) :-
		Code =\= 0'u,
		Code =\= 0'b,
		Code =\= 0'f,
		Code =\= 0'n,
		Code =\= 0'r,
		Code =\= 0't,
		Code =\= 0'/,
		Code =\= 0'\\,
		Code =\= 0'",
		Code =\= 0'\'.

	escaped_or_unescaped_code(_, Code) -->
		[0'\\, 0'u],
		hexadecimal_quad(Value),
		{Value >= 55296, Value =< 56319},
		[0'\\, 0'u],
		hexadecimal_quad(LowValue),
		{LowValue >= 56320, LowValue =< 57343,
		 Code is 65536 + (Value - 55296) * 1024 + (LowValue - 56320)},
		!.
	escaped_or_unescaped_code(_, _) -->
		[0'\\, 0'u],
		hexadecimal_quad(Value),
		{Value >= 56320, Value =< 57343},
		!,
		{fail}.
	escaped_or_unescaped_code(_, Code) -->
		[0'\\, 0'u],
		hexadecimal_quad(Code),
		{Code < 55296 ; Code > 57343},
		!.
	escaped_or_unescaped_code(_, 8) -->
		[0'\\, 0'b],
		!.
	escaped_or_unescaped_code(_, 12) -->
		[0'\\, 0'f],
		!.
	escaped_or_unescaped_code(_, 10) -->
		[0'\\, 0'n],
		!.
	escaped_or_unescaped_code(_, 13) -->
		[0'\\, 0'r],
		!.
	escaped_or_unescaped_code(_, 9) -->
		[0'\\, 0't],
		!.
	escaped_or_unescaped_code(_, 0'/) -->
		[0'\\, 0'/],
		!.
	escaped_or_unescaped_code(_, 0'\\) -->
		[0'\\, 0'\\],
		!.
	escaped_or_unescaped_code(_, 0'\") -->
		[0'\\, 0'\"],
		!.
	escaped_or_unescaped_code(_, 0'\') -->
		[0'\\, 0'\'],
		!.
	escaped_or_unescaped_code(Quote, Code) -->
		[Code],
		{is_unescaped_string_code(Quote, Code)}.

	hexadecimal_quad(Value) -->
		hexadecimal_digit(Value0),
		hexadecimal_digit(Value1),
		hexadecimal_digit(Value2),
		hexadecimal_digit(Value3),
		{Value is ((Value0 * 16 + Value1) * 16 + Value2) * 16 + Value3}.

	hexadecimal_digit(Value) -->
		[Code],
		{hexadecimal_digit_value(Code, Value)}.

	int_value(Integer) -->
		[0'0],
		{Integer = 0}.
	int_value(Integer) -->
		[0'-],
		non_zero_digits(Digits),
		{number_codes(Value, Digits), Integer is -Value}.
	int_value(Integer) -->
		non_zero_digits(Digits),
		{number_codes(Integer, Digits)}.

	non_zero_digits([Digit| Digits]) -->
		[Digit],
		{Digit >= 0'1, Digit =< 0'9},
		digit_codes(Digits).

	digit_codes([Digit| Digits]) -->
		[Digit],
		{Digit >= 0'0, Digit =< 0'9},
		!,
		digit_codes(Digits).
	digit_codes([]) -->
		[].

	blanks -->
		blank,
		!,
		blanks.
	blanks -->
		[].

	blank -->
		[Code],
		{is_blank(Code)}.

	end_of_input --> [].

	is_blank(9).
	is_blank(10).
	is_blank(13).
	is_blank(32).

	is_member_name_first(Code) :-
		Code >= 0'A,
		Code =< 0'Z,
		!.
	is_member_name_first(Code) :-
		Code >= 0'a,
		Code =< 0'z,
		!.
	is_member_name_first(0'_).
	is_member_name_first(Code) :-
		unicode_scalar_value(Code),
		Code > 127.

	is_member_name_char(Code) :-
		is_member_name_first(Code),
		!.
	is_member_name_char(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	is_unescaped_string_code(Quote, Code) :-
		unicode_scalar_value(Code),
		Code >= 32,
		Code =\= Quote,
		Code =\= 0'\\.

	hexadecimal_digit_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hexadecimal_digit_value(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		!,
		Value is Code - 0'A + 10.
	hexadecimal_digit_value(Code, Value) :-
		Code >= 0'a,
		Code =< 0'f,
		Value is Code - 0'a + 10.

:- end_object.


:- object(json_path,
	extends(json_path(atom))).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'JSONPath implementation using atoms for text representation.'
	]).

:- end_object.
