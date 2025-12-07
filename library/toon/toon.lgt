%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(toon(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_),
	implements(toon_protocol)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2025-12-07,
		comment is 'TOON (Token-Oriented Object Notation) parser and generator. TOON is a compact, human-readable, line-oriented format that encodes the JSON data model.',
		parameters is [
			'ObjectRepresentation' - 'Object representation to be used when decoding TOON objects. Possible values are ``curly`` (default) and ``list``.',
			'PairRepresentation' - 'Pair representation to be used when decoding TOON objects. Possible values are ``dash`` (default), ``equal``, and ``colon``.',
			'StringRepresentation' - 'Text representation to be used when decoding TOON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- uses(list, [
		append/3, last/2, length/2, member/2, msort/2, reverse/2, valid/1 as is_list/1
	]).

	% parse/2 - Main parsing predicate
	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), TOON) :-
		file_to_lines(File, Lines),
		parse_lines(_ObjectRepresentation_, Lines, TOON),
		!.
	parse(stream(Stream), TOON) :-
		stream_to_lines(Stream, Lines),
		parse_lines(_ObjectRepresentation_, Lines, TOON),
		!.
	parse(codes(Codes), TOON) :-
		codes_to_lines(Codes, Lines),
		parse_lines(_ObjectRepresentation_, Lines, TOON),
		!.
	parse(chars(Chars), TOON) :-
		chars_to_lines(Chars, Lines),
		parse_lines(_ObjectRepresentation_, Lines, TOON),
		!.
	parse(atom(Atom), TOON) :-
		atom_codes(Atom, Codes),
		codes_to_lines(Codes, Lines),
		parse_lines(_ObjectRepresentation_, Lines, TOON),
		!.
	parse(Source, _) :-
		domain_error(toon_source, Source).

	% generate/2 - Main generation predicate
	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), Term) :-
		generate_toon(Term, 0, Codes, []),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate(stream(Stream), Term) :-
		generate_toon(Term, 0, Codes, []),
		write_codes(Codes, Stream),
		!.
	generate(codes(Codes), Term) :-
		generate_toon(Term, 0, Codes, []),
		!.
	generate(chars(Chars), Term) :-
		generate_toon(Term, 0, Codes, []),
		codes_to_chars(Codes, Chars),
		!.
	generate(atom(Atom), Term) :-
		generate_toon(Term, 0, Codes, []),
		atom_codes(Atom, Codes),
		!.
	generate(Sink, _) :-
		domain_error(toon_sink, Sink).

	%
	% Parsing implementation
	%

	% Parse lines into TOON term
	parse_lines(curly, [], {}) :-
		!.
	parse_lines(list, [], toon([])) :-
		!.
	parse_lines(_, Lines, Result) :-
		% Filter out empty lines
		filter_non_empty_lines(Lines, NonEmptyLines),
		(	NonEmptyLines == [] ->
			(	_ObjectRepresentation_ == curly ->
				Result = {}
			;	Result = toon([])
			)
		;	% Check if first line is an array header (root array)
			NonEmptyLines = [line(FirstLineCodes)|_],
			(	is_root_array_header(FirstLineCodes) ->
				parse_root_array(NonEmptyLines, Result)
			;	% Check if single primitive
				NonEmptyLines = [line(SingleLine)],
				\+ contains_colon(SingleLine) ->
				parse_primitive_value(SingleLine, Result)
			;	% Otherwise parse as object
				parse_object_lines(NonEmptyLines, 0, [], Pairs),
				build_object(Pairs, Result)
			)
		).

	filter_non_empty_lines([], []).
	filter_non_empty_lines([line([])|Lines], Filtered) :-
		!,
		filter_non_empty_lines(Lines, Filtered).
	filter_non_empty_lines([line(Codes)|Lines], Filtered) :-
		all_whitespace(Codes),
		!,
		filter_non_empty_lines(Lines, Filtered).
	filter_non_empty_lines([Line|Lines], [Line|Filtered]) :-
		filter_non_empty_lines(Lines, Filtered).

	all_whitespace([]).
	all_whitespace([Code|Codes]) :-
		(Code == 32 ; Code == 9),
		all_whitespace(Codes).

	% Check if line is a root array header (starts with '[')
	is_root_array_header(Codes) :-
		skip_whitespace(Codes, [0'[|_]).

	% Check if line contains an unquoted colon
	contains_colon(Codes) :-
		contains_colon(Codes, false).

	contains_colon([0'"| Codes], false) :- !,
		skip_quoted_string(Codes, Rest),
		contains_colon(Rest, false).
	contains_colon([0':| _], false) :- !.
	contains_colon([_| Codes], InQuote) :-
		contains_colon(Codes, InQuote).

	skip_quoted_string([], []).
	skip_quoted_string([0'\\, _| Codes], Rest) :- !,
		skip_quoted_string(Codes, Rest).
	skip_quoted_string([0'"| Codes], Codes) :- !.
	skip_quoted_string([_| Codes], Rest) :-
		skip_quoted_string(Codes, Rest).

	skip_whitespace([], []).
	skip_whitespace([Code| Codes], Result) :-
		(Code == 32 ; Code == 9), !,
		skip_whitespace(Codes, Result).
	skip_whitespace(Codes, Codes).

	% Calculate indentation depth
	get_depth(Codes, Depth) :-
		count_leading_spaces(Codes, Spaces),
		Depth is Spaces // 2.

	count_leading_spaces([], 0).
	count_leading_spaces([32| Codes], N) :-
		!,
		count_leading_spaces(Codes, N1),
		N is N1 + 1.
	count_leading_spaces([_| _], 0).

	% Parse root array (starts with [N]:)
	parse_root_array([line(HeaderCodes)|RestLines], Result) :-
		skip_whitespace(HeaderCodes, Trimmed),
		parse_array_header(Trimmed, none, Length, Delimiter, Fields, ValueCodes),
		(	Fields \== none ->
			% Tabular array
			parse_tabular_rows(RestLines, 1, Delimiter, Fields, Rows, _),
			build_tabular_result(Rows, Fields, Result)
		;	ValueCodes \== [] ->
			% Inline primitive array
			parse_inline_values(ValueCodes, Delimiter, Values),
			Result = Values
		;	% Expanded array
			parse_array_items(RestLines, 1, Length, Delimiter, Items, _),
			Result = Items
		).

	% Parse array header: [N<delim?>]{fields}?: values
	parse_array_header([0'[| Codes], Key, Length, Delimiter, Fields, ValueCodes) :-
		parse_bracket_contents(Codes, Length, Delimiter, AfterBracket),
		(	AfterBracket = [0'{| FieldsCodes] ->
			parse_fields_segment(FieldsCodes, Delimiter, Fields, AfterFields)
		;	Fields = none,
			AfterFields = AfterBracket
		),
		(	AfterFields = [0':| ValueStart] ->
			skip_whitespace(ValueStart, ValueCodes)
		;	AfterFields = [0':],
			ValueCodes = []
		;	ValueCodes = [],
			AfterFields = []
		),
		(	var(Key) -> Key = none
		;	true
		).

	parse_bracket_contents(Codes, Length, Delimiter, Rest) :-
		parse_integer(Codes, Length, AfterInt),
		(	AfterInt = [0'\t, 0']| Rest] ->
			Delimiter = tab
		;	AfterInt = [0'|, 0']| Rest] ->
			Delimiter = pipe
		;	AfterInt = [0']| Rest] ->
			Delimiter = comma
		;	% Handle cases like [0]:
			AfterInt = [0']| Rest],
			Delimiter = comma
		).

	parse_integer(Codes, N, Rest) :-
		parse_digits(Codes, Digits, Rest),
		Digits \== [],
		number_codes(N, Digits).

	parse_digits([Code| Codes], [Code| Digits], Rest) :-
		Code >= 0'0, Code =< 0'9, !,
		parse_digits(Codes, Digits, Rest).
	parse_digits(Codes, [], Codes).

	% Parse fields segment {f1,f2,...}
	parse_fields_segment(Codes, Delimiter, Fields, Rest) :-
		parse_until_brace(Codes, FieldsCodes, AfterBrace),
		split_by_delimiter(FieldsCodes, Delimiter, FieldTokens),
		meta::map(parse_field_name, FieldTokens, Fields),
		Rest = AfterBrace.

	parse_until_brace([0'}| Rest], [], Rest) :-
		!.
	parse_until_brace([Code| Codes], [Code| FieldsCodes], Rest) :-
		parse_until_brace(Codes, FieldsCodes, Rest).

	parse_field_name(Codes, Name) :-
		skip_whitespace(Codes, Trimmed),
		trim_trailing_whitespace(Trimmed, CleanCodes),
		(	CleanCodes = [0'"| _] ->
			parse_quoted_string(CleanCodes, Name, _)
		;	toon_string_to_term(_StringRepresentation_, CleanCodes, Name)
		).

	trim_trailing_whitespace(Codes, Trimmed) :-
		reverse(Codes, Rev),
		skip_whitespace(Rev, RevTrimmed),
		reverse(RevTrimmed, Trimmed).

	% Split by delimiter
	split_by_delimiter(Codes, Delimiter, Tokens) :-
		delimiter_code(Delimiter, DelimCode),
		split_by_code(Codes, DelimCode, Tokens).

	delimiter_code(comma, 0',).
	delimiter_code(tab,   0'\t).
	delimiter_code(pipe,  0'|).

	split_by_code(Codes, DelimCode, Tokens) :-
		split_by_code(Codes, DelimCode, [], Tokens, false).

	split_by_code([], _, Current, [Token], _) :-
		reverse(Current, Token).
	split_by_code([0'"|Codes], DelimCode, Current, Tokens, false) :- !,
		collect_quoted(Codes, Quoted, Rest),
		append([0'"|Quoted], [0'"|Current], NewCurrent),
		split_by_code(Rest, DelimCode, NewCurrent, Tokens, false).
	split_by_code([DelimCode|Codes], DelimCode, Current, [Token|Tokens], false) :- !,
		reverse(Current, Token),
		split_by_code(Codes, DelimCode, [], Tokens, false).
	split_by_code([Code|Codes], DelimCode, Current, Tokens, InQuote) :-
		split_by_code(Codes, DelimCode, [Code|Current], Tokens, InQuote).

	collect_quoted([], [], []).
	collect_quoted([0'\\, Code|Codes], [0'\\, Code|Quoted], Rest) :-
		!,
		collect_quoted(Codes, Quoted, Rest).
	collect_quoted([0'"|Codes], [0'"], Codes) :-
		!.
	collect_quoted([Code|Codes], [Code|Quoted], Rest) :-
		collect_quoted(Codes, Quoted, Rest).

	% Parse inline primitive values
	parse_inline_values([], _, []) :-
		!.
	parse_inline_values(Codes, Delimiter, Values) :-
		split_by_delimiter(Codes, Delimiter, Tokens),
		meta::map(parse_value_token, Tokens, Values).

	parse_value_token(Codes, Value) :-
		skip_whitespace(Codes, Trimmed),
		trim_trailing_whitespace(Trimmed, CleanCodes),
		parse_primitive_value(CleanCodes, Value).

	% Parse a primitive value
	parse_primitive_value([], '') :-
		_StringRepresentation_ == atom, !.
	parse_primitive_value([], chars([])) :-
		_StringRepresentation_ == chars, !.
	parse_primitive_value([], codes([])) :-
		_StringRepresentation_ == codes, !.
	parse_primitive_value([0'"|Codes], Value) :- !,
		parse_quoted_string([0'"|Codes], Value, _).
	parse_primitive_value(Codes, @true) :-
		atom_codes(true, Codes), !.
	parse_primitive_value(Codes, @false) :-
		atom_codes(false, Codes), !.
	parse_primitive_value(Codes, @null) :-
		atom_codes(null, Codes), !.
	parse_primitive_value(Codes, Number) :-
		catch(number_codes(Number, Codes), _, fail),
		number(Number), !.
	parse_primitive_value(Codes, Value) :-
		toon_string_to_term(_StringRepresentation_, Codes, Value).

	toon_string_to_term(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	toon_string_to_term(chars, Codes, chars(Chars)) :-
		codes_to_chars(Codes, Chars).
	toon_string_to_term(codes, Codes, codes(Codes)).

	% Parse quoted string
	parse_quoted_string([0'"|Codes], Value, Rest) :-
		parse_string_contents(Codes, StringCodes, Rest),
		toon_string_to_term(_StringRepresentation_, StringCodes, Value).

	parse_string_contents([], [], []).
	parse_string_contents([0'\\, Code| Codes], [Unescaped| StringCodes], Rest) :-
		!,
		unescape_char(Code, Unescaped),
		parse_string_contents(Codes, StringCodes, Rest).
	parse_string_contents([0'"| Rest], [], Rest) :-
		!.
	parse_string_contents([Code| Codes], [Code| StringCodes], Rest) :-
		parse_string_contents(Codes, StringCodes, Rest).

	unescape_char(0'\\, 0'\\).
	unescape_char(0'", 0'").
	unescape_char(0'n, 0'\n).
	unescape_char(0'r, 0'\r).
	unescape_char(0't, 0'\t).

	% Parse object lines at a given depth
	parse_object_lines([], _, Pairs, Pairs) :- !.
	parse_object_lines([line(Codes)| Lines], Depth, PairsIn, PairsOut) :-
		get_depth(Codes, LineDepth),
		(	LineDepth < Depth ->
			% End of this object
			PairsIn = PairsOut
		;	LineDepth == Depth ->
			skip_n_spaces(Codes, Depth, Content),
			parse_key_value_line(Content, Lines, Depth, Key, Value, RestLines),
			build_pair(_PairRepresentation_, Key, Value, Pair),
			parse_object_lines(RestLines, Depth, [Pair| PairsIn], PairsOut)
		;	% Skip lines with greater depth (handled by nested parsing)
			parse_object_lines(Lines, Depth, PairsIn, PairsOut)
		).

	skip_n_spaces(Codes, 0, Codes) :- !.
	skip_n_spaces([32, 32| Codes], N, Result) :-
		N > 0,
		N1 is N - 1,
		skip_n_spaces(Codes, N1, Result).

	% Parse a key: value line
	parse_key_value_line(Codes, Lines, Depth, Key, Value, RestLines) :-
		parse_key(Codes, Key, AfterKey),
		skip_whitespace(AfterKey, ValueStart),
		(	ValueStart == [] ->
			% Nested object or empty object
			NestedDepth is Depth + 1,
			parse_nested_content(Lines, NestedDepth, Value, RestLines)
		;	ValueStart = [0'[| _] ->
			% Array value
			parse_array_value(ValueStart, Lines, Depth, Value, RestLines)
		;	% Primitive value
			parse_primitive_value(ValueStart, Value),
			RestLines = Lines
		).

	% Parse key from line
	parse_key([0'"| Codes], Key, Rest) :-
		!,
		parse_quoted_string([0'"| Codes], Key, AfterQuote),
		(	AfterQuote = [0':| Rest0] ->
			skip_whitespace(Rest0, Rest)
		;	Rest = AfterQuote
		).
	parse_key(Codes, Key, Rest) :-
		parse_unquoted_key(Codes, KeyCodes, Rest),
		toon_string_to_term(_StringRepresentation_, KeyCodes, Key).

	parse_unquoted_key([], [], []).
	parse_unquoted_key([0':| Rest0], [], Rest) :-
		!,
		skip_whitespace(Rest0, Rest).
	parse_unquoted_key([0'[| Codes], [], [0'[| Codes]) :-
		!.
	parse_unquoted_key([Code| Codes], [Code| KeyCodes], Rest) :-
		parse_unquoted_key(Codes, KeyCodes, Rest).

	% Parse array value
	parse_array_value(Codes, Lines, Depth, Value, RestLines) :-
		parse_array_header(Codes, none, _Length, Delimiter, Fields, ValueCodes),
		(	Fields \== none ->
			% Tabular array
			NestedDepth is Depth + 1,
			parse_tabular_rows(Lines, NestedDepth, Delimiter, Fields, Rows, RestLines),
			build_tabular_result(Rows, Fields, Value)
		;	ValueCodes \== [] ->
			% Inline primitive array
			parse_inline_values(ValueCodes, Delimiter, Value),
			RestLines = Lines
		;	% Expanded array
			NestedDepth is Depth + 1,
			parse_array_items(Lines, NestedDepth, _Length, Delimiter, Value, RestLines)
		).

	% Parse nested content (object or array items)
	parse_nested_content(Lines, Depth, Value, RestLines) :-
		collect_lines_at_depth(Lines, Depth, NestedLines, RestLines),
		(	NestedLines == [] ->
			(	_ObjectRepresentation_ == curly ->
				Value = {}
			;	Value = toon([])
			)
		;	NestedLines = [line(FirstLine)|_],
			get_depth(FirstLine, FirstDepth),
			(	FirstDepth >= Depth,
				skip_n_spaces(FirstLine, FirstDepth, Content),
				Content = [0'-|_] ->
				% Array items
				parse_array_items_from_lines(NestedLines, Depth, Items),
				Value = Items
			;	% Object
				parse_object_lines(NestedLines, Depth, [], Pairs),
				build_object(Pairs, Value)
			)
		).

	collect_lines_at_depth([], _, [], []).
	collect_lines_at_depth([line(Codes)| Lines], Depth, [line(Codes)| Collected], Rest) :-
		get_depth(Codes, LineDepth),
		LineDepth >= Depth, !,
		collect_lines_at_depth(Lines, Depth, Collected, Rest).
	collect_lines_at_depth(Lines, _, [], Lines).

	% Parse tabular rows
	parse_tabular_rows([], _, _, _, [], []) :-
		!.
	parse_tabular_rows([line(Codes)| Lines], Depth, Delimiter, Fields, [Row| Rows], RestLines) :-
		get_depth(Codes, LineDepth),
		LineDepth >= Depth,
		!,
		skip_n_spaces(Codes, LineDepth, Content),
		(	Content = [0'-|_] ->
			% This is a list item, not a tabular row
			Rows = [],
			RestLines = [line(Codes)|Lines]
		;	contains_colon(Content) ->
			% This is a key-value line, end of tabular rows
			Rows = [],
			RestLines = [line(Codes)|Lines]
		;	split_by_delimiter(Content, Delimiter, ValueTokens),
			meta::map(parse_value_token, ValueTokens, Values),
			build_row_object(Fields, Values, Row),
			parse_tabular_rows(Lines, Depth, Delimiter, Fields, Rows, RestLines)
		).
	parse_tabular_rows(Lines, _, _, _, [], Lines).

	build_row_object(Fields, Values, Object) :-
		pairs_from_fields_values(Fields, Values, Pairs),
		build_object(Pairs, Object).

	pairs_from_fields_values([], [], []).
	pairs_from_fields_values([Field| Fields], [Value| Values], [Pair| Pairs]) :-
		build_pair(_PairRepresentation_, Field, Value, Pair),
		pairs_from_fields_values(Fields, Values, Pairs).

	% Build tabular result from rows
	build_tabular_result(Rows, _Fields, Rows).

	% Parse array items
	parse_array_items([], _, _, _, [], []) :-
		!.
	parse_array_items([line(Codes)|Lines], Depth, _Length, Delimiter, Items, RestLines) :-
		get_depth(Codes, LineDepth),
		LineDepth >= Depth,
		!,
		skip_n_spaces(Codes, LineDepth, Content),
		(	Content = [0'-, 32| ItemContent] ->
			skip_whitespace(ItemContent, TrimmedItem),
			parse_array_item(TrimmedItem, [line(Codes)| Lines], Depth, Delimiter, Item, Lines1),
			Items = [Item|RestItems],
			parse_array_items(Lines1, Depth, _Length, Delimiter, RestItems, RestLines)
		;	% Not a list item
			Items = [],
			RestLines = [line(Codes)| Lines]
		).
	parse_array_items(Lines, _, _, _, [], Lines).

	parse_array_items_from_lines(Lines, Depth, Items) :-
		parse_array_items(Lines, Depth, _, comma, Items, _).

	% Parse individual array item
	parse_array_item([], Lines, _Depth, _Delimiter, '', RestLines) :-
		% Empty item
		!,
		skip_current_line(Lines, RestLines).
	parse_array_item([0'[| Codes], Lines, Depth, _Delimiter, Value, RestLines) :-
		% Inline array item
		!,
		parse_array_header([0'[| Codes], none, _Length, ItemDelimiter, Fields, ValueCodes),
		(	Fields \== none ->
			NestedDepth is Depth + 1,
			skip_current_line(Lines, Lines1),
			parse_tabular_rows(Lines1, NestedDepth, ItemDelimiter, Fields, Rows, RestLines),
			build_tabular_result(Rows, Fields, Value)
		;	parse_inline_values(ValueCodes, ItemDelimiter, Value),
			skip_current_line(Lines, RestLines)
		).
	parse_array_item(Codes, Lines, Depth, _Delimiter, Value, RestLines) :-
		% Check if it's an object (has key: pattern)
		(	has_key_value_pattern(Codes) ->
			% Object item
			skip_current_line(Lines, Lines1),
			parse_key(Codes, FirstKey, FirstValueStart),
			(	FirstValueStart = [0'[| ArrayCodes] ->
				% First field is an array
				parse_array_value([0'[| ArrayCodes], Lines1, Depth, FirstValue, Lines2),
				build_pair(_PairRepresentation_, FirstKey, FirstValue, FirstPair),
				NestedDepth is Depth + 1,
				parse_object_lines(Lines2, NestedDepth, [], RestPairs),
				build_object([FirstPair|RestPairs], Value),
				collect_remaining_at_depth(Lines2, NestedDepth, RestLines)
			;	FirstValueStart == [] ->
				% Nested object as first field
				NestedDepth is Depth + 1,
				parse_nested_content(Lines1, NestedDepth, FirstValue, Lines2),
				build_pair(_PairRepresentation_, FirstKey, FirstValue, FirstPair),
				parse_object_lines(Lines2, NestedDepth, [], RestPairs),
				build_object([FirstPair| RestPairs], Value),
				collect_remaining_at_depth(Lines2, NestedDepth, RestLines)
			;	% Simple value as first field
				parse_primitive_value(FirstValueStart, FirstValue),
				build_pair(_PairRepresentation_, FirstKey, FirstValue, FirstPair),
				NestedDepth is Depth + 1,
				parse_object_lines(Lines1, NestedDepth, [], RestPairs),
				build_object([FirstPair| RestPairs], Value),
				collect_remaining_at_depth(Lines1, NestedDepth, RestLines)
			)
		;	% Primitive item
			parse_primitive_value(Codes, Value),
			skip_current_line(Lines, RestLines)
		).

	has_key_value_pattern(Codes) :-
		(	Codes = [0'"| Rest] ->
			skip_quoted_string(Rest, AfterQuote),
			AfterQuote = [0':| _]
		;	append(_, [0':|_], Codes),
			\+ append(_, [0'[| _], Codes)
		).

	skip_current_line([], []).
	skip_current_line([_|Lines], Lines).

	collect_remaining_at_depth([], _, []).
	collect_remaining_at_depth([line(Codes)| Lines], Depth, RestLines) :-
		get_depth(Codes, LineDepth),
		(	LineDepth >= Depth ->
			collect_remaining_at_depth(Lines, Depth, RestLines)
		;	RestLines = [line(Codes)| Lines]
		).

	% Build object from pairs
	build_object(Pairs, Object) :-
		reverse(Pairs, OrderedPairs),
		(	_ObjectRepresentation_ == curly ->
			pairs_to_curly(OrderedPairs, Object)
		;	Object = toon(OrderedPairs)
		).

	pairs_to_curly([], {}) :-
		!.
	pairs_to_curly(Pairs, {CurlyPairs}) :-
		list_to_curly(Pairs, CurlyPairs).

	list_to_curly([Pair], Pair) :-
		!.
	list_to_curly([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly(Pairs, Rest).

	build_pair(dash,  Key, Value, Key-Value).
	build_pair(equal, Key, Value, Key=Value).
	build_pair(colon, Key, Value, ':'(Key, Value)).

	%
	% Generation implementation
	%

	generate_toon(Term, _, _, _) :-
		var(Term),
		instantiation_error.

	% Numbers
	generate_toon(Number, _, Codes, Tail) :-
		number(Number),
		!,
		number_codes(Number, NumCodes),
		append(NumCodes, Tail, Codes).

	% Literals
	generate_toon(@true, _, Codes, Tail) :-
		!,
		atom_codes(true, LitCodes),
		append(LitCodes, Tail, Codes).
	generate_toon(@false, _, Codes, Tail) :-
		!,
		atom_codes(false, LitCodes),
		append(LitCodes, Tail, Codes).
	generate_toon(@null, _, Codes, Tail) :-
		!,
		atom_codes(null, LitCodes),
		append(LitCodes, Tail, Codes).

	% Empty object (curly)
	generate_toon({}, _, Tail, Tail) :-
		_ObjectRepresentation_ == curly,
		!.

	% Object (curly)
	generate_toon({Pairs}, Depth, Codes, Tail) :-
		_ObjectRepresentation_ == curly,
		!,
		curly_to_list(Pairs, PairList),
		generate_object_pairs(PairList, Depth, Codes, Tail).

	% Empty object (list)
	generate_toon(toon([]), _, Tail, Tail) :-
		_ObjectRepresentation_ == list,
		!.

	% Object (list)
	generate_toon(toon(Pairs), Depth, Codes, Tail) :-
		_ObjectRepresentation_ == list,
		!,
		generate_object_pairs(Pairs, Depth, Codes, Tail).

	% Empty array
	generate_toon([], Depth, Codes, Tail) :-
		!,
		generate_indent(Depth, Codes, C1),
		C1 = [0'[, 0'0, 0'], 0':|Tail].

	% Array
	generate_toon(List, Depth, Codes, Tail) :-
		is_list(List),
		!,
		length(List, Len),
		(	is_tabular_array(List, Fields) ->
			generate_tabular_array(List, Fields, Len, Depth, Codes, Tail)
		;	all_primitives(List) ->
			generate_inline_array(List, Len, Depth, Codes, Tail)
		;	generate_expanded_array(List, Len, Depth, Codes, Tail)
		).

	% Strings
	generate_toon(chars(Chars), Depth, Codes, Tail) :-
		!,
		chars_to_codes(Chars, StringCodes),
		generate_string_value(StringCodes, Depth, Codes, Tail).
	generate_toon(codes(StringCodes), Depth, Codes, Tail) :-
		!,
		generate_string_value(StringCodes, Depth, Codes, Tail).
	generate_toon(Atom, Depth, Codes, Tail) :-
		atom(Atom), !,
		atom_codes(Atom, StringCodes),
		generate_string_value(StringCodes, Depth, Codes, Tail).

	% Convert curly pairs to list
	curly_to_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		curly_to_list(Rest, Pairs).
	curly_to_list(Pair, [Pair]).

	% Check if array is tabular (uniform objects with primitive values)
	is_tabular_array([], _) :-
		!,
		fail.
	is_tabular_array([First| Rest], Fields) :-
		is_object_term(_ObjectRepresentation_, First),
		get_object_keys(_ObjectRepresentation_, First, Fields),
		Fields \== [],
		all_primitive_values(_ObjectRepresentation_, First),
		all_same_structure(Rest, Fields).

	is_object_term(curly, {_}).
	is_object_term(list, toon(_)).

	get_object_keys(curly, {Pairs}, Keys) :-
		curly_to_list(Pairs, PairList),
		meta::map(get_pair_key, PairList, Keys).
	get_object_keys(list, toon(Pairs), Keys) :-
		meta::map(get_pair_key, Pairs, Keys).

	get_pair_key(Key-_, Key) :- !.
	get_pair_key(Key=_, Key) :- !.
	get_pair_key(':'(Key, _), Key).

	all_primitive_values(curly, {Pairs}) :-
		curly_to_list(Pairs, PairList),
		meta::map(pair_has_primitive_value, PairList).
	all_primitive_values(list, toon(Pairs)) :-
		meta::map(pair_has_primitive_value, Pairs).

	pair_has_primitive_value(Key-Value) :-
		is_primitive(Value),
		atom(Key).
	pair_has_primitive_value(Key=Value) :-
		is_primitive(Value),
		atom(Key).
	pair_has_primitive_value(':'(Key, Value)) :-
		is_primitive(Value),
		atom(Key).

	is_primitive(Value) :- number(Value), !.
	is_primitive(@true) :- !.
	is_primitive(@false) :- !.
	is_primitive(@null) :- !.
	is_primitive(chars(_)) :- !.
	is_primitive(codes(_)) :- !.
	is_primitive(Value) :- atom(Value).

	all_same_structure([], _).
	all_same_structure([Obj| Rest], Fields) :-
		is_object_term(_ObjectRepresentation_, Obj),
		get_object_keys(_ObjectRepresentation_, Obj, ObjFields),
		same_fields(Fields, ObjFields),
		all_primitive_values(_ObjectRepresentation_, Obj),
		all_same_structure(Rest, Fields).

	same_fields(Fields1, Fields2) :-
		msort(Fields1, Sorted),
		msort(Fields2, Sorted).

	all_primitives([]).
	all_primitives([H| T]) :-
		is_primitive(H),
		all_primitives(T).

	% Generate object pairs
	generate_object_pairs([], _, Tail, Tail).
	generate_object_pairs([Pair| Pairs], Depth, Codes, Tail) :-
		get_pair_key(Pair, Key),
		get_pair_value(Pair, Value),
		generate_indent(Depth, Codes, C1),
		generate_key(Key, C1, C2),
		C2 = [0':, 32| C3],
		(	is_object_term(_ObjectRepresentation_, Value) ->
			% Nested object - key: on its own line, then nested content
			C3 = [0'\n|C4],
			NestedDepth is Depth + 1,
			generate_toon(Value, NestedDepth, C4, C5)
		;	is_list(Value) ->
			% Array value
			generate_array_value(Value, Depth, C3, C5)
		;	% Primitive value
			generate_toon(Value, Depth, C3, C5)
		),
		(	Pairs \== [] ->
			C5 = [0'\n| C6],
			generate_object_pairs(Pairs, Depth, C6, Tail)
		;	C5 = Tail
		).

	get_pair_value(_ - Value,     Value).
	get_pair_value(_ = Value,     Value).
	get_pair_value(':'(_, Value), Value).

	% Generate key
	generate_key(Key, Codes, Tail) :-
		(	atom(Key) ->
			atom_codes(Key, KeyCodes)
		;	Key = chars(Chars) ->
			chars_to_codes(Chars, KeyCodes)
		;	Key = codes(KeyCodes) ->
			true
		;	KeyCodes = []
		),
		(	needs_quoting_key(KeyCodes) ->
			generate_quoted_string(KeyCodes, Codes, Tail)
		;	append(KeyCodes, Tail, Codes)
		).

	needs_quoting_key([]) :-
		!.
	needs_quoting_key([Code| _]) :-
		\+ is_key_start_char(Code),
		!.
	needs_quoting_key(Codes) :-
		member(Code, Codes),
		\+ is_key_char(Code),
		!.

	is_key_start_char(Code) :-
		Code >= 0'A, Code =< 0'Z,
		!.
	is_key_start_char(Code) :-
		Code >= 0'a, Code =< 0'z,
		!.
	is_key_start_char(0'_).

	is_key_char(Code) :-
		is_key_start_char(Code),
		!.
	is_key_char(Code) :-
		Code >= 0'0, Code =< 0'9,
		!.
	is_key_char(0'.).

	% Generate array value (for object fields)
	generate_array_value([], _Depth, Codes, Tail) :-
		!,
		Codes = [0'[, 0'0, 0'], 0':| Tail].
	generate_array_value(List, Depth, Codes, Tail) :-
		length(List, Len),
		number_codes(Len, LenCodes),
		(	is_tabular_array(List, Fields) ->
			generate_tabular_header_inline(Fields, LenCodes, Codes, C1),
			C1 = [0'\n|C2],
			NestedDepth is Depth + 1,
			generate_tabular_rows(List, Fields, NestedDepth, C2, Tail)
		;	all_primitives(List) ->
			append([0'[|LenCodes], [0'], 0':, 32|C2], Codes),
			generate_inline_values(List, C2, Tail)
		;	append([0'[|LenCodes], [0'], 0':, 0'\n|C2], Codes),
			NestedDepth is Depth + 1,
			generate_array_items(List, NestedDepth, C2, Tail)
		).

	generate_tabular_header_inline(Fields, LenCodes, Codes, Tail) :-
		append([0'[|LenCodes], [0'], 0'{|C2], Codes),
		generate_field_list(Fields, C2, C3),
		C3 = [0'}, 0':|Tail].

	generate_field_list([], Tail, Tail).
	generate_field_list([Field], Codes, Tail) :-
		!,
		generate_key(Field, Codes, Tail).
	generate_field_list([Field|Fields], Codes, Tail) :-
		generate_key(Field, Codes, C1),
		C1 = [0',| C2],
		generate_field_list(Fields, C2, Tail).

	% Generate tabular array
	generate_tabular_array(List, Fields, Len, Depth, Codes, Tail) :-
		generate_indent(Depth, Codes, C1),
		number_codes(Len, LenCodes),
		append([0'[| LenCodes], [0'], 0'{| C3], C1),
		generate_field_list(Fields, C3, C4),
		C4 = [0'}, 0':, 0'\n| C5],
		NestedDepth is Depth + 1,
		generate_tabular_rows(List, Fields, NestedDepth, C5, Tail).

	generate_tabular_rows([], _, _, Tail, Tail).
	generate_tabular_rows([Obj], Fields, Depth, Codes, Tail) :-
		!,
		generate_indent(Depth, Codes, C1),
		generate_row_values(Obj, Fields, C1, Tail).
	generate_tabular_rows([Obj| Objs], Fields, Depth, Codes, Tail) :-
		generate_indent(Depth, Codes, C1),
		generate_row_values(Obj, Fields, C1, C2),
		C2 = [0'\n| C3],
		generate_tabular_rows(Objs, Fields, Depth, C3, Tail).

	generate_row_values(Obj, Fields, Codes, Tail) :-
		get_values_in_order(Obj, Fields, Values),
		generate_inline_values(Values, Codes, Tail).

	get_values_in_order(_, [], []) :- !.
	get_values_in_order(Obj, [Field| Fields], [Value| Values]) :-
		get_object_value(_ObjectRepresentation_, Obj, Field, Value),
		get_values_in_order(Obj, Fields, Values).

	get_object_value(curly, {Pairs}, Field, Value) :-
		curly_to_list(Pairs, PairList),
		member(Pair, PairList),
		get_pair_key(Pair, Field),
		get_pair_value(Pair, Value).
	get_object_value(list, toon(Pairs), Field, Value) :-
		member(Pair, Pairs),
		get_pair_key(Pair, Field),
		get_pair_value(Pair, Value).

	% Generate inline array
	generate_inline_array(List, Len, Depth, Codes, Tail) :-
		generate_indent(Depth, Codes, C1),
		number_codes(Len, LenCodes),
		append([0'[|LenCodes], [0'], 0':, 32| C3], C1),
		generate_inline_values(List, C3, Tail).

	generate_inline_values([], Tail, Tail).
	generate_inline_values([Value], Codes, Tail) :-
		!,
		generate_primitive_inline(Value, Codes, Tail).
	generate_inline_values([Value| Values], Codes, Tail) :-
		generate_primitive_inline(Value, Codes, C1),
		C1 = [0',|C2],
		generate_inline_values(Values, C2, Tail).

	generate_primitive_inline(Number, Codes, Tail) :-
		number(Number), !,
		number_codes(Number, NumCodes),
		append(NumCodes, Tail, Codes).
	generate_primitive_inline(@true, Codes, Tail) :-
		!,
		atom_codes(true, LitCodes),
		append(LitCodes, Tail, Codes).
	generate_primitive_inline(@false, Codes, Tail) :-
		!,
		atom_codes(false, LitCodes),
		append(LitCodes, Tail, Codes).
	generate_primitive_inline(@null, Codes, Tail) :-
		!,
		atom_codes(null, LitCodes),
		append(LitCodes, Tail, Codes).
	generate_primitive_inline(chars(Chars), Codes, Tail) :-
		!,
		chars_to_codes(Chars, StringCodes),
		generate_string_inline(StringCodes, Codes, Tail).
	generate_primitive_inline(codes(StringCodes), Codes, Tail) :-
		!,
		generate_string_inline(StringCodes, Codes, Tail).
	generate_primitive_inline(Atom, Codes, Tail) :-
		atom(Atom),
		atom_codes(Atom, StringCodes),
		generate_string_inline(StringCodes, Codes, Tail).

	generate_string_inline(StringCodes, Codes, Tail) :-
		(	needs_quoting(StringCodes, comma) ->
			generate_quoted_string(StringCodes, Codes, Tail)
		;	append(StringCodes, Tail, Codes)
		).

	% Generate expanded array
	generate_expanded_array(List, Len, Depth, Codes, Tail) :-
		generate_indent(Depth, Codes, C1),
		number_codes(Len, LenCodes),
		append([0'[| LenCodes], [0'], 0':, 0'\n| C3], C1),
		NestedDepth is Depth + 1,
		generate_array_items(List, NestedDepth, C3, Tail).

	generate_array_items([], _, Tail, Tail).
	generate_array_items([Item], Depth, Codes, Tail) :-
		!,
		generate_array_item(Item, Depth, Codes, Tail).
	generate_array_items([Item| Items], Depth, Codes, Tail) :-
		generate_array_item(Item, Depth, Codes, C1),
		C1 = [0'\n|C2],
		generate_array_items(Items, Depth, C2, Tail).

	generate_array_item(Item, Depth, Codes, Tail) :-
		generate_indent(Depth, Codes, C1),
		C1 = [0'-, 32| C2],
		(	is_primitive(Item) ->
			generate_toon(Item, 0, C2, Tail)
		;	is_list(Item) ->
			generate_array_value(Item, Depth, C2, Tail)
		;	is_object_term(_ObjectRepresentation_, Item) ->
			generate_object_item(Item, Depth, C2, Tail)
		;	generate_toon(Item, 0, C2, Tail)
		).

	generate_object_item({}, _Depth, Codes, Codes) :- !.
	generate_object_item(toon([]), _Depth, Codes, Codes) :- !.
	generate_object_item(Obj, Depth, Codes, Tail) :-
		get_object_pairs(_ObjectRepresentation_, Obj, Pairs),
		Pairs = [FirstPair|RestPairs],
		get_pair_key(FirstPair, FirstKey),
		get_pair_value(FirstPair, FirstValue),
		generate_key(FirstKey, Codes, C1),
		C1 = [0':, 32|C2],
		(	is_list(FirstValue), is_tabular_array(FirstValue, Fields) ->
			% Tabular array as first field
			length(FirstValue, Len),
			number_codes(Len, LenCodes),
			append([0'[| LenCodes], [0'], 0'{| C4], C2),
			generate_field_list(Fields, C4, C5),
			C5 = [0'}, 0':, 0'\n| C6],
			NestedDepth is Depth + 2,
			generate_tabular_rows(FirstValue, Fields, NestedDepth, C6, C7),
			(	RestPairs \== [] ->
				C7 = [0'\n| C8],
				SiblingDepth is Depth + 1,
				generate_object_pairs(RestPairs, SiblingDepth, C8, Tail)
			;	C7 = Tail
			)
		;	is_object_term(_ObjectRepresentation_, FirstValue) ->
			C2 = [0'\n| C3],
			NestedDepth is Depth + 2,
			generate_toon(FirstValue, NestedDepth, C3, C4),
			(	RestPairs \== [] ->
				C4 = [0'\n| C5],
				SiblingDepth is Depth + 1,
				generate_object_pairs(RestPairs, SiblingDepth, C5, Tail)
			;	C4 = Tail
			)
		;	is_list(FirstValue) ->
			generate_array_value(FirstValue, Depth, C2, C3),
			(	RestPairs \== [] ->
				C3 = [0'\n| C4],
				SiblingDepth is Depth + 1,
				generate_object_pairs(RestPairs, SiblingDepth, C4, Tail)
			;	C3 = Tail
			)
		;	generate_toon(FirstValue, 0, C2, C3),
			(	RestPairs \== [] ->
				C3 = [0'\n| C4],
				SiblingDepth is Depth + 1,
				generate_object_pairs(RestPairs, SiblingDepth, C4, Tail)
			;	C3 = Tail
			)
		).

	get_object_pairs(curly, {Pairs}, PairList) :-
		curly_to_list(Pairs, PairList).
	get_object_pairs(list, toon(Pairs), Pairs).

	% Generate string value
	generate_string_value(StringCodes, _Depth, Codes, Tail) :-
		(	needs_quoting(StringCodes, comma) ->
			generate_quoted_string(StringCodes, Codes, Tail)
		;	append(StringCodes, Tail, Codes)
		).

	% Check if string needs quoting
	needs_quoting([], _) :-
		!.  % Empty string
	needs_quoting([First| _], _) :-
		(First == 32 ; First == 9), !.  % Leading whitespace
	needs_quoting(Codes, _) :-
		last(Codes, Last),
		(Last == 32 ; Last == 9), !.  % Trailing whitespace
	needs_quoting(Codes, _) :-
		atom_codes(true, Codes), !.
	needs_quoting(Codes, _) :-
		atom_codes(false, Codes), !.
	needs_quoting(Codes, _) :-
		atom_codes(null, Codes), !.
	needs_quoting(Codes, _) :-
		catch(number_codes(_, Codes), _, fail), !.
	needs_quoting(Codes, _) :-
		member(Code, Codes),
		(Code == 0': ; Code == 0'" ; Code == 0'\\), !.
	needs_quoting(Codes, _) :-
		member(Code, Codes),
		(Code == 0'[ ; Code == 0'] ; Code == 0'{ ; Code == 0'}), !.
	needs_quoting(Codes, _) :-
		member(Code, Codes),
		(Code == 0'\n ; Code == 0'\r ; Code == 0'\t), !.
	needs_quoting(Codes, Delimiter) :-
		delimiter_code(Delimiter, DelimCode),
		member(DelimCode, Codes), !.
	needs_quoting([0'-|_], _) :- !.  % Starts with hyphen

	% Generate quoted string
	generate_quoted_string(StringCodes, Codes, Tail) :-
		escape_string(StringCodes, EscapedCodes),
		Codes = [0'"|C1],
		append(EscapedCodes, [0'"|Tail], C1).

	escape_string([], []).
	escape_string([0'\\| Codes], [0'\\, 0'\\| Escaped]) :- !,
		escape_string(Codes, Escaped).
	escape_string([0'"| Codes], [0'\\, 0'"| Escaped]) :- !,
		escape_string(Codes, Escaped).
	escape_string([0'\n| Codes], [0'\\, 0'n| Escaped]) :- !,
		escape_string(Codes, Escaped).
	escape_string([0'\r| Codes], [0'\\, 0'r| Escaped]) :- !,
		escape_string(Codes, Escaped).
	escape_string([0'\t| Codes], [0'\\, 0't| Escaped]) :- !,
		escape_string(Codes, Escaped).
	escape_string([Code| Codes], [Code| Escaped]) :-
		escape_string(Codes, Escaped).

	% Generate indentation
	generate_indent(0, Codes, Codes) :-
		!.
	generate_indent(Depth, [32, 32| Codes], Tail) :-
		Depth > 0,
		Depth1 is Depth - 1,
		generate_indent(Depth1, Codes, Tail).

	% Auxiliary predicates

	file_to_lines(File, Lines) :-
		open(File, read, Stream),
		stream_to_lines(Stream, Lines),
		close(Stream).

	stream_to_lines(Stream, Lines) :-
		reader::line_to_codes(Stream, Line),
		stream_to_lines(Line, Stream, Lines).

	stream_to_lines(end_of_file, _, []).
	stream_to_lines([], _, []).
	stream_to_lines([Code| Codes], Stream, [line([Code| Codes])| Lines]) :-
		reader::line_to_codes(Stream, NextLine),
		stream_to_lines(NextLine, Stream, Lines).

	chars_to_lines([], []).
	chars_to_lines([Char| Chars], [line(Line)| Lines]) :-
		chars_to_line([Char| Chars], Line, Rest),
		chars_to_lines(Rest, Lines).

	chars_to_line([], [], []).
	chars_to_line(['\n'| Rest], [], Rest) :-
		!.
	chars_to_line(['\r', '\n'| Rest], [], Rest) :-
		!.
	chars_to_line(['\r'| Rest], [], Rest) :-
		!.
	chars_to_line([Char| Chars], [Code| Codes], Rest) :-
		char_code(Char, Code),
		chars_to_line(Chars, Codes, Rest).

	codes_to_lines([], []) :- !.
	codes_to_lines(Codes, [line(LineCodes)| Lines]) :-
		codes_to_line(Codes, LineCodes, Rest),
		codes_to_lines(Rest, Lines).

	codes_to_line([], [], []).
	codes_to_line([0'\n| Rest], [], Rest) :-
		!.
	codes_to_line([0'\r, 0'\n| Rest], [], Rest) :-
		!.
	codes_to_line([0'\r| Rest], [], Rest) :-
		!.
	codes_to_line([Code| Codes], [Code| LineCodes], Rest) :-
		codes_to_line(Codes, LineCodes, Rest).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

:- end_object.


:- object(toon(StringRepresentation),
	extends(toon(curly, dash, StringRepresentation))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-12-07,
		comment is 'TOON parser and generator. Uses curly terms for parsed TOON objects and dashes for parsed TOON pairs.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding TOON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

:- end_object.


:- object(toon,
	extends(toon(curly, dash, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-12-07,
		comment is 'TOON parser and generator. Uses curly terms for parsed TOON objects, dashes for parsed TOON pairs, and atoms for parsed TOON strings.'
	]).

:- end_object.
