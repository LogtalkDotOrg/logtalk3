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


:- object(toml(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_),
	implements(toml_protocol)).

	:- info([
		version is 1:1:2,
		author is 'Paulo Moura',
		date is 2026-05-18,
		comment is 'TOML parser and generator.',
		parameters is [
			'ObjectRepresentation' - 'Object representation to be used when decoding TOML tables. Possible values are ``compound`` (default) and ``curly``.',
			'PairRepresentation' - 'Pair representation to be used when decoding TOML tables. Possible values are ``dash`` (default), ``equal``, and ``colon``.',
			'StringRepresentation' - 'Text representation to be used when decoding TOML basic and literal string values. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- uses(list, [
		append/2, append/3, length/2, member/2, memberchk/2, reverse/2, valid/1 as is_list/1
	]).

	:- uses(reader, [
		file_to_codes/2, stream_to_codes/2
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), TOML) :-
		!,
		file_to_codes(File, Codes),
		parse_codes(Codes, TOML).
	parse(stream(Stream), TOML) :-
		!,
		stream_to_codes(Stream, Codes),
		parse_codes(Codes, TOML).
	parse(codes(Codes), TOML) :-
		!,
		parse_codes(Codes, TOML).
	parse(chars(Chars), TOML) :-
		!,
		chars_to_codes(Chars, Codes),
		parse_codes(Codes, TOML).
	parse(atom(Atom), TOML) :-
		!,
		atom_codes(Atom, Codes),
		parse_codes(Codes, TOML).
	parse(Source, _) :-
		domain_error(toml_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(_, TOML) :-
		var(TOML),
		instantiation_error.
	generate(file(File), TOML) :-
		check_toml_term(TOML, Internal),
		generate_codes(Internal, Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate(stream(Stream), TOML) :-
		check_toml_term(TOML, Internal),
		generate_codes(Internal, Codes),
		write_codes(Codes, Stream),
		!.
	generate(codes(Codes), TOML) :-
		check_toml_term(TOML, Internal),
		generate_codes(Internal, Codes),
		!.
	generate(chars(Chars), TOML) :-
		check_toml_term(TOML, Internal),
		generate_codes(Internal, Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate(atom(Atom), TOML) :-
		check_toml_term(TOML, Internal),
		generate_codes(Internal, Codes),
		atom_codes(Atom, Codes),
		!.
	generate(Sink, _) :-
		domain_error(toml_sink, Sink).

	parse_codes(Codes, TOML) :-
		phrase(document(Statements), Codes),
		assemble_document(Statements, node(explicit, []), [], Internal),
		internal_to_public_document(Internal, TOML).

	check_toml_term(TOML, Internal) :-
		public_document_to_internal(TOML, Internal),
		!.
	check_toml_term(TOML, _) :-
		domain_error(toml_term, TOML).

	generate_codes(Internal, Codes) :-
		generate_lines(Internal, [], Lines),
		flatten_lines(Lines, Codes).

	% document grammar

	document(Statements) -->
		blank_lines,
		statements(Statements),
		blank_lines,
		eos.

	statements([Statement| Statements]) -->
		statement(Statement),
		!,
		blank_lines,
		statements(Statements).
	statements([]) -->
		[].

	statement(header(Path)) -->
		spaces,
		table_header(Path),
		spaces,
		optional_comment,
		line_end_or_eos.
	statement(array_header(Path)) -->
		spaces,
		array_table_header(Path),
		spaces,
		optional_comment,
		line_end_or_eos.
	statement(kv(Path, Value)) -->
		spaces,
		dotted_key(Path),
		spaces,
		[0'=],
		spaces,
		value(Value),
		spaces,
		optional_comment,
		line_end_or_eos.

	table_header(Path) -->
		[0'[],
		spaces,
		dotted_key(Path),
		spaces,
		[0']].

	array_table_header(Path) -->
		[0'[, 0'[],
		spaces,
		dotted_key(Path),
		spaces,
		[0'], 0']].

	dotted_key([Key| Keys]) -->
		key(Key),
		dotted_key_tail(Keys).

	dotted_key_tail([Key| Keys]) -->
		spaces,
		[0'.],
		spaces,
		key(Key),
		!,
		dotted_key_tail(Keys).
	dotted_key_tail([]) -->
		[].

	key(Key) -->
		[0'"],
		!,
		basic_string_codes(Codes),
		{atom_codes(Key, Codes)}.
	key(Key) -->
		[0'\'],
		!,
		literal_string_codes(Codes),
		{atom_codes(Key, Codes)}.
	key(Key) -->
		bare_key_codes(Codes),
		{Codes \== [], atom_codes(Key, Codes)}.

	value(Value) -->
		[0'", 0'", 0'"],
		!,
		multiline_basic_string_codes(Codes),
		{string_codes_value(Codes, Value)}.
	value(Value) -->
		[0'"],
		!,
		basic_string_codes(Codes),
		{string_codes_value(Codes, Value)}.
	value(Value) -->
		[0'\', 0'\', 0'\'],
		!,
		multiline_literal_string_codes(Codes),
		{string_codes_value(Codes, Value)}.
	value(Value) -->
		[0'\'],
		!,
		literal_string_codes(Codes),
		{string_codes_value(Codes, Value)}.
	value(Value) -->
		[0'[],
		!,
		array(Value).
	value(Value) -->
		[0'{],
		!,
		inline_table(Value).
	value(Value) -->
		temporal_value(Value),
		!.
	value(Value) -->
		bare_value(Value).

	temporal_value(Value) -->
		offset_date_time_value(Value).
	temporal_value(Value) -->
		local_date_time_value(Value).
	temporal_value(Value) -->
		local_date_value(Value).
	temporal_value(Value) -->
		local_time_value(Value).

	array([]) -->
		spaces,
		[0']].
	array([Value| Values]) -->
		spaces,
		value(Value),
		spaces,
		array_tail(Values).

	array_tail([]) -->
		[0']].
	array_tail([]) -->
		[0',],
		spaces,
		[0']].
	array_tail([Value| Values]) -->
		[0',],
		spaces,
		value(Value),
		spaces,
		array_tail(Values).

	inline_table(Value) -->
		spaces,
		[0'}],
		{Value = node(inline, [])}.
	inline_table(Value) -->
		spaces,
		inline_entries(Entries),
		spaces,
		[0'}],
		{assemble_inline_entries(Entries, Value)}.

	inline_entries([kv(Path, Value)| Entries]) -->
		dotted_key(Path),
		spaces,
		[0'=],
		spaces,
		value(Value),
		spaces,
		inline_entries_tail(Entries).

	inline_entries_tail([kv(Path, Value)| Entries]) -->
		[0',],
		spaces,
		dotted_key(Path),
		spaces,
		[0'=],
		spaces,
		value(Value),
		spaces,
		inline_entries_tail(Entries).
	inline_entries_tail([]) -->
		[].

	basic_string_codes(Codes) -->
		basic_string_content(Codes),
		[0'"].

	multiline_basic_string_codes(Codes) -->
		trim_initial_multiline_newline,
		multiline_basic_string_content(Codes).

	basic_string_content([]) -->
		[].
	basic_string_content(Codes) -->
		[0'\\],
		!,
		escape_sequence(Escaped),
		basic_string_content(Rest),
		{append(Escaped, Rest, Codes)}.
	basic_string_content([Code| Codes]) -->
		[Code],
		{Code =\= 0'", Code =\= 0'\\, Code =\= 10, Code =\= 13},
		basic_string_content(Codes).

	multiline_basic_string_content([0'", 0'"]) -->
		[0'", 0'", 0'", 0'", 0'"],
		!.
	multiline_basic_string_content([0'"]) -->
		[0'", 0'", 0'", 0'"],
		!.
	multiline_basic_string_content([]) -->
		[0'", 0'", 0'"],
		!.
	multiline_basic_string_content(Codes) -->
		multiline_trimmed_line_ending,
		!,
		multiline_basic_string_content(Codes).
	multiline_basic_string_content(Codes) -->
		[0'\\],
		!,
		escape_sequence(Escaped),
		multiline_basic_string_content(Rest),
		{append(Escaped, Rest, Codes)}.
	multiline_basic_string_content([0'"| Codes]) -->
		[0'"],
		!,
		multiline_basic_string_content(Codes).
	multiline_basic_string_content([Code| Codes]) -->
		[Code],
		{multiline_basic_string_code(Code)},
		multiline_basic_string_content(Codes).

	escape_sequence([0'"] ) --> [0'"].
	escape_sequence([0'\\]) --> [0'\\].
	escape_sequence([0'/]) --> [0'/].
	escape_sequence([0'\b]) --> [0'b].
	escape_sequence([0'\t]) --> [0't].
	escape_sequence([0'\n]) --> [0'n].
	escape_sequence([0'\f]) --> [0'f].
	escape_sequence([0'\r]) --> [0'r].
	escape_sequence([Code]) -->
		[0'u],
		hex_digits(4, Digits),
		{hex_digits_value(Digits, Code)}.
	escape_sequence([Code]) -->
		[0'U],
		hex_digits(8, Digits),
		{hex_digits_value(Digits, Code)}.

	hex_digits(0, []) -->
		[].
	hex_digits(Count, [Digit| Digits]) -->
		{Count > 0},
		[Digit],
		{hex_digit_code(Digit)},
		{Next is Count - 1},
		hex_digits(Next, Digits).

	literal_string_codes([]) -->
		[0'\'].
	literal_string_codes([Code| Codes]) -->
		[Code],
		{Code =\= 0'\', Code =\= 10, Code =\= 13},
		literal_string_codes(Codes).

	multiline_literal_string_codes(Codes) -->
		trim_initial_multiline_newline,
		multiline_literal_string_content(Codes).

	multiline_literal_string_content([0'\', 0'\']) -->
		[0'\', 0'\', 0'\', 0'\', 0'\'],
		!.
	multiline_literal_string_content([0'\']) -->
		[0'\', 0'\', 0'\', 0'\'],
		!.
	multiline_literal_string_content([]) -->
		[0'\', 0'\', 0'\'],
		!.
	multiline_literal_string_content([0'\'| Codes]) -->
		[0'\'],
		!,
		multiline_literal_string_content(Codes).
	multiline_literal_string_content([Code| Codes]) -->
		[Code],
		{multiline_literal_string_code(Code)},
		multiline_literal_string_content(Codes).

	bare_key_codes([Code| Codes]) -->
		[Code],
		{bare_key_code(Code)},
		bare_key_tail(Codes).

	bare_key_tail([Code| Codes]) -->
		[Code],
		{bare_key_code(Code)},
		!,
		bare_key_tail(Codes).
	bare_key_tail([]) -->
		[].

	bare_key_code(Code) :-
		Code >= 0'a,
		Code =< 0'z.
	bare_key_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z.
	bare_key_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.
	bare_key_code(0'_).
	bare_key_code(0'-).

	bare_value(Value) -->
		bare_value_codes(Codes),
		{Codes \== [], classify_bare_value(Codes, Value)}.

	bare_value_codes([Code| Codes]) -->
		[Code],
		{bare_value_code(Code)},
		bare_value_tail(Codes).

	bare_value_tail([Code| Codes]) -->
		[Code],
		{bare_value_code(Code)},
		!,
		bare_value_tail(Codes).
	bare_value_tail([]) -->
		[].

	bare_value_code(Code) :-
		Code =\= 32,
		Code =\= 9,
		Code =\= 10,
		Code =\= 13,
		Code =\= 0',,
		Code =\= 0'],
		Code =\= 0'},
		Code =\= 0'#.

	blank_lines -->
		blank_line,
		!,
		blank_lines.
	blank_lines -->
		[].

	blank_line -->
		spaces,
		comment,
		line_end.
	blank_line -->
		spaces,
		line_end.

	optional_comment -->
		comment,
		!.
	optional_comment -->
		[].

	comment -->
		[0'#],
		comment_text.

	comment_text -->
		[Code],
		{Code =\= 10, Code =\= 13},
		!,
		comment_text.
	comment_text -->
		[].

	spaces -->
		[Code],
		{member(Code, [32, 9])},
		!,
		spaces.
	spaces -->
		[].

	line_end_or_eos -->
		[10],
		!.
	line_end_or_eos -->
		[13, 10],
		!.
	line_end_or_eos -->
		[13],
		!.
	line_end_or_eos -->
		eos.

	line_end -->
		[10],
		!.
	line_end -->
		[13, 10],
		!.
	line_end -->
		[13].

	local_date_value(date(Year, Month, Day)) -->
		date_codes(Year, Month, Day).

	local_time_value(time(Hours, Minutes, Seconds)) -->
		time_codes(Hours, Minutes, Seconds),
		{valid_time_value(Hours, Minutes, Seconds)}.

	local_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)) -->
		date_codes(Year, Month, Day),
		date_time_separator,
		time_codes(Hours, Minutes, Seconds),
		{valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds))}.

	offset_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds, OffsetSeconds)) -->
		date_codes(Year, Month, Day),
		date_time_separator,
		time_codes(Hours, Minutes, Seconds),
		offset_codes(OffsetSeconds),
		{valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)), valid_offset_value(OffsetSeconds)}.

	date_time_separator -->
		[0'T].
	date_time_separator -->
		[32].

	date_codes(Year, Month, Day) -->
		four_digits(Year),
		[0'-],
		two_digits(Month),
		[0'-],
		two_digits(Day),
		{valid_date_value(Year, Month, Day)}.

	time_codes(Hours, Minutes, Seconds) -->
		two_digits(Hours),
		[0':],
		two_digits(Minutes),
		[0':],
		second_codes(Seconds).

	second_codes(Seconds) -->
		two_digits(WholeSeconds),
		fractional_second_codes(FractionalCodes),
		{second_value(WholeSeconds, FractionalCodes, Seconds)}.

	fractional_second_codes([0'.| Digits]) -->
		[0'.],
		one_or_more_digits(Digits).
	fractional_second_codes([]) -->
		[].

	offset_codes(0) -->
		[0'Z].
	offset_codes(OffsetSeconds) -->
		[Sign],
		{member(Sign, [0'+, 0'-])},
		two_digits(Hours),
		[0':],
		two_digits(Minutes),
		{valid_offset_components(Hours, Minutes),
		Offset0 is (Hours*60 + Minutes)*60,
		(Sign == 0'+ -> OffsetSeconds = Offset0 ; OffsetSeconds is -Offset0)}.

	four_digits(Number) -->
		[Digit1, Digit2, Digit3, Digit4],
		{digit_code(Digit1), digit_code(Digit2), digit_code(Digit3), digit_code(Digit4),
		Number is (Digit1-0'0)*1000 + (Digit2-0'0)*100 + (Digit3-0'0)*10 + (Digit4-0'0)}.

	two_digits(Number) -->
		[Digit1, Digit2],
		{digit_code(Digit1), digit_code(Digit2),
		Number is (Digit1-0'0)*10 + (Digit2-0'0)}.

	one_or_more_digits([Digit| Digits]) -->
		[Digit],
		{digit_code(Digit)},
		digits(Digits).

	digits([Digit| Digits]) -->
		[Digit],
		{digit_code(Digit)},
		!,
		digits(Digits).
	digits([]) -->
		[].

	trim_initial_multiline_newline -->
		line_end,
		!.
	trim_initial_multiline_newline -->
		[].

	multiline_trimmed_line_ending -->
		[0'\\],
		spaces,
		line_end,
		multiline_trimmed_whitespace.

	multiline_trimmed_whitespace -->
		[Code],
		{member(Code, [32, 9, 10, 13])},
		!,
		multiline_trimmed_whitespace.
	multiline_trimmed_whitespace -->
		[].

	% semantic assembly

	assemble_document([], Internal, _, Internal).
	assemble_document([header(Path)| Statements], Internal0, _, Internal) :-
		!,
		ensure_table(Internal0, Path, Internal1),
		assemble_document(Statements, Internal1, Path, Internal).
	assemble_document([array_header(Path)| Statements], Internal0, _, Internal) :-
		!,
		ensure_array_table(Internal0, Path, Internal1),
		assemble_document(Statements, Internal1, Path, Internal).
	assemble_document([kv(Path, Value)| Statements], Internal0, Context, Internal) :-
		append(Context, Path, FullPath),
		insert_value(Internal0, FullPath, Value, Internal1),
		assemble_document(Statements, Internal1, Context, Internal).

	assemble_inline_entries(Entries, Internal) :-
		assemble_document(Entries, node(inline, []), [], Internal).

	ensure_table(Internal, [], Internal) :-
		!.
	ensure_table(node(Status, Pairs0), [Key], node(Status, Pairs)) :-
		!,
		(   pair_lookup(Key, Pairs0, node(implicit, ChildPairs)) ->
			update_pair(Key, node(explicit, ChildPairs), Pairs0, Pairs)
		;   pair_lookup(Key, Pairs0, _) ->
			fail
		;   append(Pairs0, [Key-node(explicit, [])], Pairs)
		).
	ensure_table(node(Status, Pairs0), [Key, NextKey| Keys], node(Status, Pairs)) :-
		(   pair_lookup(Key, Pairs0, node(ChildStatus, ChildPairs)) ->
			ChildStatus \== inline,
			ensure_table(node(ChildStatus, ChildPairs), [NextKey| Keys], UpdatedChild),
			update_pair(Key, UpdatedChild, Pairs0, Pairs)
		;   pair_lookup(Key, Pairs0, Tables0),
			array_of_tables(Tables0) ->
			last_array_table(Tables0, node(ChildStatus, ChildPairs)),
			ChildStatus \== inline,
			ensure_table(node(ChildStatus, ChildPairs), [NextKey| Keys], UpdatedChild),
			update_last_array_table(Tables0, UpdatedChild, Tables),
			update_pair(Key, Tables, Pairs0, Pairs)
		;   ensure_table(node(implicit, []), [NextKey| Keys], Child),
			append(Pairs0, [Key-Child], Pairs)
		).

	ensure_array_table(node(Status, Pairs0), [Key], node(Status, Pairs)) :-
		!,
		(   pair_lookup(Key, Pairs0, Tables0) ->
			array_of_tables(Tables0),
			append(Tables0, [node(explicit, [])], Tables),
			update_pair(Key, Tables, Pairs0, Pairs)
		;   append(Pairs0, [Key-[node(explicit, [])]], Pairs)
		).
	ensure_array_table(node(Status, Pairs0), [Key, NextKey| Keys], node(Status, Pairs)) :-
		(   pair_lookup(Key, Pairs0, node(ChildStatus, ChildPairs)) ->
			ChildStatus \== inline,
			ensure_array_table(node(ChildStatus, ChildPairs), [NextKey| Keys], UpdatedChild),
			update_pair(Key, UpdatedChild, Pairs0, Pairs)
		;   pair_lookup(Key, Pairs0, Tables0),
			array_of_tables(Tables0) ->
			last_array_table(Tables0, node(ChildStatus, ChildPairs)),
			ChildStatus \== inline,
			ensure_array_table(node(ChildStatus, ChildPairs), [NextKey| Keys], UpdatedChild),
			update_last_array_table(Tables0, UpdatedChild, Tables),
			update_pair(Key, Tables, Pairs0, Pairs)
		;   ensure_array_table(node(implicit, []), [NextKey| Keys], Child),
			append(Pairs0, [Key-Child], Pairs)
		).

	insert_value(node(Status, Pairs0), [Key], Value, node(Status, Pairs)) :-
		!,
		\+ pair_lookup(Key, Pairs0, _),
		append(Pairs0, [Key-Value], Pairs).
	insert_value(node(Status, Pairs0), [Key, NextKey| Keys], Value, node(Status, Pairs)) :-
		(   pair_lookup(Key, Pairs0, node(ChildStatus, ChildPairs)) ->
			ChildStatus \== inline,
			insert_value(node(ChildStatus, ChildPairs), [NextKey| Keys], Value, UpdatedChild),
			update_pair(Key, UpdatedChild, Pairs0, Pairs)
		;   pair_lookup(Key, Pairs0, Tables0),
			array_of_tables(Tables0) ->
			last_array_table(Tables0, node(ChildStatus, ChildPairs)),
			ChildStatus \== inline,
			insert_value(node(ChildStatus, ChildPairs), [NextKey| Keys], Value, UpdatedChild),
			update_last_array_table(Tables0, UpdatedChild, Tables),
			update_pair(Key, Tables, Pairs0, Pairs)
		;   \+ pair_lookup(Key, Pairs0, _),
			insert_value(node(implicit, []), [NextKey| Keys], Value, Child),
			append(Pairs0, [Key-Child], Pairs)
		).

	array_of_tables(Tables) :-
		is_list(Tables),
		Tables \== [],
		forall(member(Table, Tables), Table = node(_, _)).

	last_array_table([Table], Table) :-
		!.
	last_array_table([_| Tables], Table) :-
		last_array_table(Tables, Table).

	update_last_array_table([_], UpdatedTable, [UpdatedTable]) :-
		!.
	update_last_array_table([Table| Tables0], UpdatedTable, [Table| Tables]) :-
		update_last_array_table(Tables0, UpdatedTable, Tables).

	pair_lookup(Key, [Key-Value| _], Value).
	pair_lookup(Key, [_| Pairs], Value) :-
		pair_lookup(Key, Pairs, Value).

	update_pair(Key, Value, [Key-_| Pairs], [Key-Value| Pairs]) :-
		!.
	update_pair(Key, Value, [Pair| Pairs0], [Pair| Pairs]) :-
		update_pair(Key, Value, Pairs0, Pairs).

	% token classification

	classify_bare_value(Codes, Value) :-
		atom_codes(Atom, Codes),
		classify_bare_atom(Atom, Value).

	classify_bare_atom(true, '@'(true)) :- !.
	classify_bare_atom(false, '@'(false)) :- !.
	classify_bare_atom(inf, '@'(inf)) :- !.
	classify_bare_atom('+inf', '@'(inf)) :- !.
	classify_bare_atom('-inf', '@'(-inf)) :- !.
	classify_bare_atom(nan, '@'(nan)) :- !.
	classify_bare_atom('+nan', '@'(nan)) :- !.
	classify_bare_atom('-nan', '@'(nan)) :- !.
	classify_bare_atom(Atom, Number) :-
		atom_codes(Atom, Codes),
		number_token_codes(Number, Codes),
		!.

	number_token_codes(Number, [0'0, Prefix| Digits]) :-
		member(Prefix, [0'x, 0'o, 0'b]),
		remove_underscores(Digits, CleanDigits),
		CleanDigits \== [],
		base_from_prefix(Prefix, Base),
		digits_value(CleanDigits, Base, Number).
	number_token_codes(Number, [0'-, 0'0, Prefix| Digits]) :-
		member(Prefix, [0'x, 0'o, 0'b]),
		remove_underscores(Digits, CleanDigits),
		CleanDigits \== [],
		base_from_prefix(Prefix, Base),
		digits_value(CleanDigits, Base, Positive),
		Number is -Positive.
	number_token_codes(Number, [0'+| Digits]) :-
		remove_underscores(Digits, CleanDigits),
		CleanDigits \== [],
		number_codes(Number, CleanDigits).
	number_token_codes(Number, Digits) :-
		remove_underscores(Digits, CleanDigits),
		CleanDigits \== [],
		number_codes(Number, CleanDigits).

	base_from_prefix(0'x, 16).
	base_from_prefix(0'o, 8).
	base_from_prefix(0'b, 2).

	remove_underscores([], []).
	remove_underscores([0'_| Codes], Clean) :-
		!,
		remove_underscores(Codes, Clean).
	remove_underscores([Code| Codes], [Code| Clean]) :-
		remove_underscores(Codes, Clean).

	digits_value(Digits, Base, Value) :-
		digits_value(Digits, Base, 0, Value).

	digits_value([], _, Value, Value).
	digits_value([Digit| Digits], Base, Acc, Value) :-
		digit_value(Digit, DigitValue),
		DigitValue < Base,
		Acc2 is Acc*Base + DigitValue,
		digits_value(Digits, Base, Acc2, Value).

	digit_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		Value is Code - 0'0.
	digit_value(Code, Value) :-
		Code >= 0'a,
		Code =< 0'f,
		Value is Code - 0'a + 10.
	digit_value(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		Value is Code - 0'A + 10.

	hex_digit_code(Code) :-
		digit_value(Code, _).

	hex_digits_value(Digits, Value) :-
		digits_value(Digits, 16, Value).

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	second_value(WholeSeconds, [], WholeSeconds).
	second_value(WholeSeconds, FractionalCodes, Seconds) :-
		Codes = [0'0| FractionalCodes],
		number_codes(Fraction, Codes),
		Seconds is WholeSeconds + Fraction.

	valid_date_value(Year, Month, Day) :-
		integer(Year),
		Year >= 0,
		Year =< 9999,
		integer(Month), Month >= 1, Month =< 12,
		integer(Day), Day >= 1,
		max_day_in_month(Year, Month, MaxDay),
		Day =< MaxDay.

	valid_time_value(Hours, Minutes, Seconds) :-
		integer(Hours), Hours >= 0, Hours =< 23,
		integer(Minutes), Minutes >= 0, Minutes =< 59,
		number(Seconds), Seconds >= 0, Seconds < 60.

	valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		valid_date_value(Year, Month, Day),
		valid_time_value(Hours, Minutes, Seconds).

	valid_offset_value(OffsetSeconds) :-
		integer(OffsetSeconds),
		OffsetSeconds mod 60 =:= 0,
		AbsOffsetSeconds is abs(OffsetSeconds),
		AbsOffsetSeconds =< 86340.

	valid_offset_components(Hours, Minutes) :-
		integer(Hours), Hours >= 0, Hours =< 23,
		integer(Minutes), Minutes >= 0, Minutes =< 59.

	max_day_in_month(Year, 2, 29) :-
		leap_year(Year),
		!.
	max_day_in_month(_, 2, 28) :-
		!.
	max_day_in_month(_, Month, MaxDay) :-
		memberchk(Month-MaxDay, [1-31, 3-31, 4-30, 5-31, 6-30, 7-31, 8-31, 9-30, 10-31, 11-30, 12-31]).

	leap_year(Year) :-
		0 =:= Year mod 400,
		!.
	leap_year(Year) :-
		0 =:= Year mod 4,
		Year mod 100 =\= 0.

	multiline_basic_string_code(Code) :-
		Code =\= 0'",
		Code =\= 0'\\,
		once((\+ control_character_code(Code); member(Code, [9, 10, 13]))).

	multiline_literal_string_code(Code) :-
		Code =\= 0'\',
		once((\+ control_character_code(Code); member(Code, [9, 10, 13]))).

	control_character_code(Code) :-
		Code >= 0,
		Code =< 8,
		!.
	control_character_code(11) :-
		!.
	control_character_code(12) :-
		!.
	control_character_code(Code) :-
		Code >= 14,
		Code =< 31,
		!.
	control_character_code(127).

	string_codes_value(Codes, Value) :-
		string_codes_value(_StringRepresentation_, Codes, Value).

	string_codes_value(atom, Codes, Value) :-
		atom_codes(Value, Codes).
	string_codes_value(chars, Codes, chars(Chars)) :-
		codes_to_chars(Codes, Chars).
	string_codes_value(codes, Codes, codes(Codes)).

	% public representation conversion

	internal_to_public_document(Internal, TOML) :-
		internal_table_to_public(_ObjectRepresentation_, Internal, TOML).

	internal_table_to_public(compound, node(_, Pairs), toml(PublicPairs)) :-
		internal_pairs_to_list(Pairs, PublicPairs).
	internal_table_to_public(curly, node(_, []), {}) :-
		!.
	internal_table_to_public(curly, node(_, Pairs), {PublicPairs}) :-
		internal_pairs_to_list(Pairs, PairList),
		pairs_list_to_comma(PairList, PublicPairs).

	internal_pairs_to_list([], []).
	internal_pairs_to_list([Key-Value| Pairs], [Pair| PublicPairs]) :-
		internal_value_to_public(Value, PublicValue),
		make_pair(_PairRepresentation_, Key, PublicValue, Pair),
		internal_pairs_to_list(Pairs, PublicPairs).

	internal_value_to_public(node(_, Pairs), Public) :-
		!,
		internal_table_to_public(_ObjectRepresentation_, node(explicit, Pairs), Public).
	internal_value_to_public(List, Public) :-
		is_list(List),
		!,
		internal_list_to_public(List, Public).
	internal_value_to_public(Value, Value).

	internal_list_to_public([], []).
	internal_list_to_public([Value| Values], [PublicValue| PublicValues]) :-
		internal_value_to_public(Value, PublicValue),
		internal_list_to_public(Values, PublicValues).

	public_document_to_internal(TOML, Internal) :-
		public_table_to_internal(_ObjectRepresentation_, TOML, Internal).

	public_table_to_internal(compound, toml(Pairs), node(explicit, InternalPairs)) :-
		is_list(Pairs),
		public_pairs_from_list(Pairs, InternalPairs).
	public_table_to_internal(curly, {}, node(explicit, [])) :-
		!.
	public_table_to_internal(curly, {PairsTerm}, node(explicit, InternalPairs)) :-
		comma_to_pairs_list(PairsTerm, Pairs),
		public_pairs_from_list(Pairs, InternalPairs).

	public_pairs_from_list([], []).
	public_pairs_from_list([Pair| Pairs], [Key-Value| InternalPairs]) :-
		pair_key_value(_PairRepresentation_, Pair, Key, PublicValue),
		atom(Key),
		public_value_to_internal(PublicValue, Value),
		public_pairs_from_list(Pairs, InternalPairs).

	public_value_to_internal(Public, node(explicit, Pairs)) :-
		public_table_to_internal(_ObjectRepresentation_, Public, node(explicit, Pairs)),
		!.
	public_value_to_internal(Public, List) :-
		is_list(Public),
		!,
		public_list_to_internal(Public, List).
	public_value_to_internal(chars(Chars), chars(Chars)) :-
		_StringRepresentation_ == chars,
		!.
	public_value_to_internal(codes(Codes), codes(Codes)) :-
		_StringRepresentation_ == codes,
		!.
	public_value_to_internal(Value, Value) :-
		valid_scalar_value(Value).

	public_list_to_internal([], []).
	public_list_to_internal([Public| Publics], [Value| Values]) :-
		public_value_to_internal(Public, Value),
		public_list_to_internal(Publics, Values).

	valid_scalar_value(Value) :-
		integer(Value),
		!.
	valid_scalar_value(Value) :-
		float(Value),
		!.
	valid_scalar_value(Value) :-
		atom(Value),
		!.
	valid_scalar_value('@'(true)).
	valid_scalar_value('@'(false)).
	valid_scalar_value('@'(inf)).
	valid_scalar_value('@'(-inf)).
	valid_scalar_value('@'(nan)).
	valid_scalar_value(chars(Chars)) :-
		_StringRepresentation_ == chars,
		is_list(Chars).
	valid_scalar_value(codes(Codes)) :-
		_StringRepresentation_ == codes,
		is_list(Codes).
	valid_scalar_value(date(Year, Month, Day)) :-
		valid_date_value(Year, Month, Day).
	valid_scalar_value(time(Hours, Minutes, Seconds)) :-
		valid_time_value(Hours, Minutes, Seconds).
	valid_scalar_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)) :-
		valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)).
	valid_scalar_value(date_time(Year, Month, Day, Hours, Minutes, Seconds, OffsetSeconds)) :-
		valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		valid_offset_value(OffsetSeconds).

	make_pair(dash, Key, Value, Key-Value).
	make_pair(equal, Key, Value, Key=Value).
	make_pair(colon, Key, Value, ':'(Key, Value)).

	pair_key_value(dash, Key-Value, Key, Value).
	pair_key_value(equal, Key=Value, Key, Value).
	pair_key_value(colon, ':'(Key, Value), Key, Value).

	pairs_list_to_comma([Pair], Pair) :-
		!.
	pairs_list_to_comma([Pair| Pairs], (Pair, Rest)) :-
		pairs_list_to_comma(Pairs, Rest).

	comma_to_pairs_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		comma_to_pairs_list(Rest, Pairs).
	comma_to_pairs_list(Pair, [Pair]).

	% canonical generation

	generate_lines(node(_, Pairs), Path, Lines) :-
		scalar_assignment_lines(Pairs, ScalarLines),
		table_blocks(Pairs, Path, TableBlocks),
		join_blocks(ScalarLines, TableBlocks, Lines).

	scalar_assignment_lines([], []).
	scalar_assignment_lines([_-Value| Pairs], Lines) :-
		(Value = node(_, _); array_of_tables(Value)),
		!,
		scalar_assignment_lines(Pairs, Lines).
	scalar_assignment_lines([Key-Value| Pairs], [Line| Lines]) :-
		encode_key(Key, KeyCodes),
		encode_scalar_value(Value, ValueCodes),
		append(KeyCodes, [32, 0'=, 32| ValueCodes], Line),
		scalar_assignment_lines(Pairs, Lines).

	table_blocks([], _, []).
	table_blocks([Key-Value| Pairs], Path, Blocks) :-
		Value = node(_, _),
		!,
		append(Path, [Key], NewPath),
		header_codes(NewPath, Header),
		generate_lines(Value, NewPath, Body),
		Block = [Header| Body],
		table_blocks(Pairs, Path, RestBlocks),
		Blocks = [Block| RestBlocks].
	table_blocks([Key-Value| Pairs], Path, Blocks) :-
		array_of_tables(Value),
		!,
		append(Path, [Key], NewPath),
		array_table_blocks(Value, NewPath, ArrayBlocks),
		table_blocks(Pairs, Path, RestBlocks),
		append(ArrayBlocks, RestBlocks, Blocks).
	table_blocks([_| Pairs], Path, Blocks) :-
		table_blocks(Pairs, Path, Blocks).

	array_table_blocks([], _, []).
	array_table_blocks([Table| Tables], Path, [Block| Blocks]) :-
		array_header_codes(Path, Header),
		generate_lines(Table, Path, Body),
		Block = [Header| Body],
		array_table_blocks(Tables, Path, Blocks).

	join_blocks(ScalarLines, [], ScalarLines) :-
		!.
	join_blocks([], [Block| Blocks], Lines) :-
		join_table_blocks([Block| Blocks], Lines).
	join_blocks(ScalarLines, [Block| Blocks], Lines) :-
		append(ScalarLines, [[]| Rest], Lines),
		join_table_blocks([Block| Blocks], Rest).

	join_table_blocks([Block], Block) :-
		!.
	join_table_blocks([Block| Blocks], Lines) :-
		append(Block, [[]| Rest], Lines),
		join_table_blocks(Blocks, Rest).

	header_codes(Path, Codes) :-
		encode_dotted_path(Path, PathCodes),
		append([0'[| PathCodes], [0']], Codes).

	array_header_codes(Path, Codes) :-
		encode_dotted_path(Path, PathCodes),
		append([0'[, 0'[| PathCodes], [0'], 0']], Codes).

	encode_dotted_path([Key], Codes) :-
		!,
		encode_key(Key, Codes).
	encode_dotted_path([Key| Keys], Codes) :-
		encode_key(Key, KeyCodes),
		append(KeyCodes, [0'., 32| RestCodes], Codes),
		encode_dotted_path(Keys, RestCodes).

	encode_scalar_value(Value, Codes) :-
		Value = '@'(true),
		!,
		Codes = [0't, 0'r, 0'u, 0'e].
	encode_scalar_value(Value, Codes) :-
		Value = '@'(false),
		!,
		Codes = [0'f, 0'a, 0'l, 0's, 0'e].
	encode_scalar_value(Value, Codes) :-
		Value = '@'(inf),
		!,
		atom_codes(inf, Codes).
	encode_scalar_value(Value, Codes) :-
		Value = '@'(-inf),
		!,
		atom_codes('-inf', Codes).
	encode_scalar_value(Value, Codes) :-
		Value = '@'(nan),
		!,
		atom_codes(nan, Codes).
	encode_scalar_value(date(Year, Month, Day), Codes) :-
		valid_date_value(Year, Month, Day),
		!,
		encode_date_value(Year, Month, Day, Codes).
	encode_scalar_value(time(Hours, Minutes, Seconds), Codes) :-
		valid_time_value(Hours, Minutes, Seconds),
		!,
		encode_time_value(Hours, Minutes, Seconds, Codes).
	encode_scalar_value(date_time(Year, Month, Day, Hours, Minutes, Seconds, OffsetSeconds), Codes) :-
		valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		valid_offset_value(OffsetSeconds),
		!,
		encode_offset_date_time_value(Year, Month, Day, Hours, Minutes, Seconds, OffsetSeconds, Codes).
	encode_scalar_value(date_time(Year, Month, Day, Hours, Minutes, Seconds), Codes) :-
		valid_date_time_value(date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		!,
		encode_local_date_time_value(Year, Month, Day, Hours, Minutes, Seconds, Codes).
	encode_scalar_value(Value, Codes) :-
		integer(Value),
		!,
		number_codes(Value, Codes).
	encode_scalar_value(Value, Codes) :-
		float(Value),
		!,
		number_codes(Value, Codes).
	encode_scalar_value(Value, Codes) :-
		is_list(Value),
		!,
		encode_array(Value, Codes).
	encode_scalar_value(Value, Codes) :-
		encode_string_value(_StringRepresentation_, Value, Codes).

	encode_array([], [0'[, 0']]).
	encode_array([Value| Values], Codes) :-
		encode_inline_value(Value, ValueCodes),
		encode_array_tail(Values, TailCodes),
		append([0'[| ValueCodes], TailCodes, Codes).

	encode_array_tail([], [0']]).
	encode_array_tail([Value| Values], Codes) :-
		encode_inline_value(Value, ValueCodes),
		append([0',, 32| ValueCodes], TailCodes, Codes),
		encode_array_tail(Values, TailCodes).

	encode_inline_value(node(_, Pairs), Codes) :-
		!,
		encode_inline_table_pairs(Pairs, InnerCodes),
		append([0'{| InnerCodes], [0'}], Codes).
	encode_inline_value(Value, Codes) :-
		encode_scalar_value(Value, Codes).

	encode_inline_table_pairs([], []).
	encode_inline_table_pairs([Key-Value], Codes) :-
		!,
		encode_key(Key, KeyCodes),
		append([KeyCodes, [32, 0'=, 32], ValueCodes], Codes),
		encode_inline_value(Value, ValueCodes).
	encode_inline_table_pairs([Key-Value| Pairs], Codes) :-
		encode_key(Key, KeyCodes),
		encode_inline_value(Value, ValueCodes),
		append(KeyCodes, [32, 0'=, 32], Codes0),
		append(Codes0, ValueCodes, Codes1),
		append(Codes1, [0',, 32| RestCodes], Codes),
		encode_inline_table_pairs(Pairs, RestCodes).

	encode_key(Key, Codes) :-
		atom(Key),
		atom_codes(Key, KeyCodes),
		KeyCodes \== [],
		forall(member(Code, KeyCodes), bare_key_code(Code)),
		!,
		Codes = KeyCodes.
	encode_key(Key, Codes) :-
		atom(Key),
		atom_codes(Key, KeyCodes),
		encode_basic_string(KeyCodes, Codes).

	encode_string_value(chars, chars(Chars), Codes) :-
		chars_to_codes(Chars, StringCodes),
		encode_basic_string(StringCodes, Codes).
	encode_string_value(codes, codes(StringCodes), Codes) :-
		encode_basic_string(StringCodes, Codes).
	encode_string_value(atom, Atom, Codes) :-
		atom(Atom),
		atom_codes(Atom, StringCodes),
		encode_basic_string(StringCodes, Codes).

	encode_basic_string(StringCodes, Codes) :-
		encode_basic_string_content(StringCodes, ContentCodes),
		append([0'"| ContentCodes], [0'"], Codes).

	encode_date_value(Year, Month, Day, Codes) :-
		fixed_width_codes(Year, 4, YearCodes),
		fixed_width_codes(Month, 2, MonthCodes),
		fixed_width_codes(Day, 2, DayCodes),
		append([YearCodes, [0'-], MonthCodes, [0'-], DayCodes], Codes).

	encode_time_value(Hours, Minutes, Seconds, Codes) :-
		fixed_width_codes(Hours, 2, HourCodes),
		fixed_width_codes(Minutes, 2, MinuteCodes),
		second_codes_value(Seconds, SecondCodes),
		append([HourCodes, [0':], MinuteCodes, [0':], SecondCodes], Codes).

	encode_local_date_time_value(Year, Month, Day, Hours, Minutes, Seconds, Codes) :-
		encode_date_value(Year, Month, Day, DateCodes),
		encode_time_value(Hours, Minutes, Seconds, TimeCodes),
		append([DateCodes, [0'T], TimeCodes], Codes).

	encode_offset_date_time_value(Year, Month, Day, Hours, Minutes, Seconds, OffsetSeconds, Codes) :-
		encode_local_date_time_value(Year, Month, Day, Hours, Minutes, Seconds, DateTimeCodes),
		encode_offset_value(OffsetSeconds, OffsetCodes),
		append(DateTimeCodes, OffsetCodes, Codes).

	encode_offset_value(0, [0'Z]) :-
		!.
	encode_offset_value(OffsetSeconds, Codes) :-
		AbsOffsetSeconds is abs(OffsetSeconds),
		OffsetHours is AbsOffsetSeconds // 3600,
		OffsetMinutes is (AbsOffsetSeconds mod 3600) // 60,
		fixed_width_codes(OffsetHours, 2, HourCodes),
		fixed_width_codes(OffsetMinutes, 2, MinuteCodes),
		(OffsetSeconds >= 0 -> SignCode = 0'+ ; SignCode = 0'-),
		append([[SignCode], HourCodes, [0':], MinuteCodes], Codes).

	second_codes_value(Seconds, Codes) :-
		integer(Seconds),
		!,
		fixed_width_codes(Seconds, 2, Codes).
	second_codes_value(Seconds, Codes) :-
		WholeSeconds is truncate(Seconds),
		Fraction is Seconds - WholeSeconds,
		Microseconds0 is round(Fraction*1000000),
		(   Microseconds0 =:= 1000000 ->
			WholeSeconds1 is WholeSeconds + 1,
			Microseconds = 0
		;   WholeSeconds1 = WholeSeconds,
			Microseconds = Microseconds0
		),
		fixed_width_codes(WholeSeconds1, 2, WholeCodes),
		(   Microseconds =:= 0 ->
			Codes = WholeCodes
		;   fixed_width_codes(Microseconds, 6, FractionCodes0),
			trim_trailing_zero_codes(FractionCodes0, FractionCodes),
			append(WholeCodes, [0'.| FractionCodes], Codes)
		).

	fixed_width_codes(Number, Width, Codes) :-
		integer(Number),
		Number >= 0,
		number_codes(Number, RawCodes),
		length(RawCodes, RawLength),
		Padding is Width - RawLength,
		Padding >= 0,
		zero_codes(Padding, PrefixCodes),
		append(PrefixCodes, RawCodes, Codes).

	zero_codes(0, []) :-
		!.
	zero_codes(Count, [0'0| Codes]) :-
		Next is Count - 1,
		zero_codes(Next, Codes).

	trim_trailing_zero_codes(Codes0, Codes) :-
		reverse(Codes0, Reversed0),
		trim_leading_zero_codes(Reversed0, Reversed),
		reverse(Reversed, Codes).

	trim_leading_zero_codes([0'0| Codes0], Codes) :-
		Codes0 \== [],
		!,
		trim_leading_zero_codes(Codes0, Codes).
	trim_leading_zero_codes(Codes, Codes).

	encode_basic_string_content([], []).
	encode_basic_string_content([0'"| StringCodes], [0'\\, 0'"| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([0'\\| StringCodes], [0'\\, 0'\\| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([0'\b| StringCodes], [0'\\, 0'b| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([0'\t| StringCodes], [0'\\, 0't| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([0'\n| StringCodes], [0'\\, 0'n| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([0'\f| StringCodes], [0'\\, 0'f| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([0'\r| StringCodes], [0'\\, 0'r| Codes]) :-
		!,
		encode_basic_string_content(StringCodes, Codes).
	encode_basic_string_content([Code| StringCodes], [Code| Codes]) :-
		encode_basic_string_content(StringCodes, Codes).

	% auxiliary predicates

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

	flatten_lines([], []).
	flatten_lines([Line| Lines], Codes) :-
		append(Line, [10| Rest], Codes),
		flatten_lines(Lines, Rest).

:- end_object.


:- object(toml(StringRepresentation),
	extends(toml(compound, dash, StringRepresentation))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-09,
		comment is 'TOML parser and generator. Uses compound terms for parsed TOML tables and dashes for parsed TOML pairs.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding TOML string values. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

:- end_object.


:- object(toml,
	extends(toml(compound, dash, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-09,
		comment is 'TOML parser and generator. Uses compound terms for parsed TOML tables, dashes for parsed TOML pairs, and atoms for parsed TOML string values.'
	]).

:- end_object.
