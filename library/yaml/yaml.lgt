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


:- object(yaml,
	implements(yaml_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-31,
		comment is 'YAML parser and generator.'
	]).

	:- uses(list, [
		append/3, member/2, reverse/2, valid/1 as is_list/1
	]).

	:- uses(reader, [
		file_to_codes/2, stream_to_codes/2
	]).

	% public predicates

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), Data) :-
		file_to_codes(File, Codes),
		phrase(yaml_document(Data, [], _), Codes),
		!.
	parse(stream(Stream), Data) :-
		stream_to_codes(Stream, Codes),
		phrase(yaml_document(Data, [], _), Codes),
		!.
	parse(codes(Codes), Data) :-
		phrase(yaml_document(Data, [], _), Codes),
		!.
	parse(chars(Chars), Data) :-
		chars_to_codes(Chars, Codes),
		phrase(yaml_document(Data, [], _), Codes),
		!.
	parse(atom(Atom), Data) :-
		atom_codes(Atom, Codes),
		phrase(yaml_document(Data, [], _), Codes),
		!.
	parse(Source, _) :-
		domain_error(yaml_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(_, Data) :-
		var(Data),
		instantiation_error.
	generate(file(File), Data) :-
		phrase(yaml_generate(Data), Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate(stream(Stream), Data) :-
		phrase(yaml_generate(Data), Codes),
		write_codes(Codes, Stream),
		!.
	generate(codes(Codes), Data) :-
		phrase(yaml_generate(Data), Codes),
		!.
	generate(chars(Chars), Data) :-
		phrase(yaml_generate(Data), Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate(atom(Atom), Data) :-
		phrase(yaml_generate(Data), Codes),
		atom_codes(Atom, Codes),
		!.
	generate(Sink, _) :-
		domain_error(yaml_sink, Sink).

	parse_all(Source, _) :-
		var(Source),
		instantiation_error.
	parse_all(file(File), Documents) :-
		file_to_codes(File, Codes),
		phrase(yaml_documents(Documents), Codes),
		!.
	parse_all(stream(Stream), Documents) :-
		stream_to_codes(Stream, Codes),
		phrase(yaml_documents(Documents), Codes),
		!.
	parse_all(codes(Codes), Documents) :-
		phrase(yaml_documents(Documents), Codes),
		!.
	parse_all(chars(Chars), Documents) :-
		chars_to_codes(Chars, Codes),
		phrase(yaml_documents(Documents), Codes),
		!.
	parse_all(atom(Atom), Documents) :-
		atom_codes(Atom, Codes),
		phrase(yaml_documents(Documents), Codes),
		!.
	parse_all(Source, _) :-
		domain_error(yaml_source, Source).

	generate_all(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate_all(_, Documents) :-
		var(Documents),
		instantiation_error.
	generate_all(_, Documents) :-
		\+ is_list(Documents),
		type_error(list, Documents).
	generate_all(file(File), Documents) :-
		phrase(yaml_generate_all(Documents), Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate_all(stream(Stream), Documents) :-
		phrase(yaml_generate_all(Documents), Codes),
		write_codes(Codes, Stream),
		!.
	generate_all(codes(Codes), Documents) :-
		phrase(yaml_generate_all(Documents), Codes),
		!.
	generate_all(chars(Chars), Documents) :-
		phrase(yaml_generate_all(Documents), Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate_all(atom(Atom), Documents) :-
		phrase(yaml_generate_all(Documents), Codes),
		atom_codes(Atom, Codes),
		!.
	generate_all(Sink, _) :-
		domain_error(yaml_sink, Sink).

	% DCG Rules for YAML Parsing

	% Document parsing - entry point (single document) with anchor tracking
	yaml_document(Data, StateIn, StateOut) -->
		skip_whitespace_and_comments,
		yaml_content(0, Data, StateIn, StateOut),
		skip_trailing.

	% Multi-document parsing - entry point (split input by document markers)
	yaml_documents(Documents) -->
		skip_ws_and_comments,
		(   % End of input
		    eos
		->  { Documents = [] }
		;   % Document start or end marker
		    document_marker
		->  skip_rest_of_line,
		    yaml_documents(Documents)
		;   % Parse document content
		    yaml_single_document(Document),
			{ Documents = [Document| Acc] },
		    yaml_documents(Acc)
		).

	% Parse a single document (stops at --- or ... or end)
	yaml_single_document(Document) -->
		yaml_content(0, Document, [], _),
		skip_to_marker_or_end.

	% Skip to next document marker or end of input
	skip_to_marker_or_end -->
		skip_ws_and_comments,
		(   eos
		->  []
		;   document_marker
		->  skip_rest_of_line
		;   []
		).

	% Skip whitespace and comments only
	skip_ws_and_comments -->
		[Code],
		{ member(Code, [32, 9, 10, 13]) },
		!,
		skip_ws_and_comments.
	skip_ws_and_comments -->
		[0'#],
		!,
		skip_rest_of_line,
		skip_ws_and_comments.
	skip_ws_and_comments -->
		[].

	% Skip to end of line
	skip_rest_of_line -->
		[Code],
		{ Code =\= 10, Code =\= 13 },
		!,
		skip_rest_of_line.
	skip_rest_of_line -->
		[10],
		!.
	skip_rest_of_line -->
		[13, 10],
		!.
	skip_rest_of_line -->
		[13],
		!.
	skip_rest_of_line -->
		[].

	% stateful Content Parser (with anchor/alias tracking)
	% state is threaded through as StateIn -> StateOut

	yaml_content(_Indent, Data, S, S) -->
		% Try flow mapping first
		[0'{],
		!,
		flow_mapping(Pairs),
		{ Data = yaml(Pairs) }.
	yaml_content(_Indent, Data, S, S) -->
		% Try flow sequence
		[0'[],
		!,
		flow_sequence(Items),
		{ Data = Items }.
	yaml_content(_Indent, Data, S, S) -->
		% Literal block scalar at document level
		[0'|],
		!,
		block_scalar_header(Chomping),
		skip_to_newline,
		newline,
		literal_block_scalar(Chomping, Codes),
		{ atom_codes(Data, Codes) }.
	yaml_content(_Indent, Data, S, S) -->
		% Folded block scalar at document level
		[0'>],
		!,
		block_scalar_header(Chomping),
		skip_to_newline,
		newline,
		folded_block_scalar(Chomping, Codes),
		{ atom_codes(Data, Codes) }.
	yaml_content(Indent, Data, S0, S) -->
		% Try block sequence (starts with -)
		peek_code(0'-),
		!,
		block_sequence(Indent, Items, S0, S),
		{ Data = Items }.
	yaml_content(Indent, Data, S0, S) -->
		% Try block mapping or scalar
		scalar_key(Key),
		skip_spaces,
		(   [0':]
		->  % This is a block mapping
		    skip_spaces,
		    % Check for merge key as first key
		    (   { Key == '<<' }
		    ->  % Merge key - value should be an alias
		        inline_value(Indent, MergeValue, S0, S1),
		        { merge_key_pairs(MergeValue, MergedPairs) },
		        block_mapping_rest(Indent, RestPairs, S1, S),
		        { append(MergedPairs, RestPairs, AllPairs) },
		        { Data = yaml(AllPairs) }
		    ;   % Normal first key
		        block_mapping_value(Indent, Key, FirstPair, S0, S1),
		        block_mapping_rest(Indent, RestPairs, S1, S),
		        { Data = yaml([FirstPair| RestPairs]) }
		    )
		;   % Just a scalar value
		    { parse_scalar_or_number(Key, Data) },
		    { S = S0 }
		).
	yaml_content(_Indent, Data, S, S) -->
		% Quoted strings
		[0'"],
		!,
		quoted_string_content(Codes),
		[0'"],
		{ atom_codes(Data, Codes) }.
	yaml_content(_Indent, Data, S, S) -->
		[0'\'],
		!,
		single_quoted_content(Codes),
		[0'\'],
		{ atom_codes(Data, Codes) }.
	yaml_content(_Indent, '@'(null), S, S) -->
		% Empty document
		[].

	% Document marker (--- or ...)
	document_marker -->
		[0'-, 0'-, 0'-],
		!.
	document_marker -->
		[0'., 0'., 0'.].

	% Peek for document markers (--- or ...)
	peek_document_marker, [0'-, 0'-, 0'-] -->
		[0'-, 0'-, 0'-],
		!.
	peek_document_marker, [0'., 0'., 0'.] -->
		[0'., 0'., 0'.].

	% stateful Block Mapping/Sequence/Value (with anchor/alias tracking)

	% Block mapping value with state threading
	block_mapping_value(Indent, Key, Key-Value, S0, S) -->
		% Check for anchor with block content on next line
		[0'&],
		!,
		anchor_name(AnchorName),
		skip_spaces,
		block_mapping_value_anchor(Indent, Value, AnchorName, S0, S).
	block_mapping_value(Indent, Key, Key-Value, S0, S) -->
		% Check if value is on the same line
		peek_non_newline_char,
		!,
		inline_value(Indent, Value, S0, S).
	block_mapping_value(Indent, Key, Key-Value, S0, S) -->
		% Value is a sequence starting on next line at same indent level
		skip_to_newline,
		newline,
		measure_indent(ChildIndent),
		{ ChildIndent =:= Indent },
		[0'-],
		!,
		skip_spaces,
		block_sequence_item(Indent, Item, S0, S1),
		block_sequence_rest(Indent, Items, S1, S),
		{ Value = [Item| Items] }.
	block_mapping_value(Indent, Key, Key-Value, S0, S) -->
		% Value is on the next line (nested structure with greater indent)
		skip_to_newline,
		newline,
		measure_indent(ChildIndent),
		{ ChildIndent > Indent },
		!,
		yaml_content(ChildIndent, Value, S0, S).
	block_mapping_value(_Indent, Key, Key-'', S, S) -->
		% Empty value
		[].

	% Helper: parse value after anchor and store anchor
	block_mapping_value_anchor(Indent, Value, AnchorName, S0, S) -->
		% Value is on the same line (after anchor)
		peek_non_newline_char,
		!,
		inline_value(Indent, Value, S0, S1),
		{ store_anchor(AnchorName, Value, S1, S) }.
	block_mapping_value_anchor(Indent, Value, AnchorName, S0, S) -->
		% Value is a sequence starting on next line
		skip_to_newline,
		newline,
		measure_indent(ChildIndent),
		{ ChildIndent =:= Indent },
		[0'-],
		!,
		skip_spaces,
		block_sequence_item(Indent, Item, S0, S1),
		block_sequence_rest(Indent, Items, S1, S2),
		{ Value = [Item| Items] },
		{ store_anchor(AnchorName, Value, S2, S) }.
	block_mapping_value_anchor(Indent, Value, AnchorName, S0, S) -->
		% Value is block content on next line
		skip_to_newline,
		newline,
		measure_indent(ChildIndent),
		{ ChildIndent > Indent },
		!,
		yaml_content(ChildIndent, Value, S0, S1),
		{ store_anchor(AnchorName, Value, S1, S) }.
	block_mapping_value_anchor(_Indent, '', AnchorName, S0, S) -->
		% Empty value - anchor on empty value doesn't make sense but handle it
		{ store_anchor(AnchorName, '', S0, S) }.

	% Rest of block mapping with state threading
	block_mapping_rest(Indent, ResultPairs, S0, S) -->
		skip_to_newline,
		newline,
		skip_empty_lines,
		% Stop at document markers
		\+ peek_document_marker,
		measure_indent(CurrentIndent),
		{ CurrentIndent =:= Indent },
		% Verify there's a valid key (not just whitespace/newline)
		peek_scalar_start,
		!,
		scalar_key(Key),
		skip_spaces,
		[0':],
		skip_spaces,
		% Check for merge key
		(   { Key == '<<' }
		->  % Merge key - value should be an alias or list of aliases
		    inline_value(Indent, MergeValue, S0, S1),
		    { merge_key_pairs(MergeValue, MergedPairs) },
		    block_mapping_rest(Indent, RestPairs, S1, S),
		    { append(MergedPairs, RestPairs, ResultPairs) }
		;   % Normal key-value pair
		    block_mapping_value(Indent, Key, Pair, S0, S1),
		    block_mapping_rest(Indent, RestPairs, S1, S),
		    { ResultPairs = [Pair| RestPairs] }
		).
	block_mapping_rest(_Indent, [], S, S) -->
		[].

	% Block sequence with state threading
	block_sequence(Indent, [Item| Items], S0, S) -->
		[0'-],
		skip_spaces,
		block_sequence_item(Indent, Item, S0, S1),
		block_sequence_rest(Indent, Items, S1, S).

	% Block sequence item with state threading
	block_sequence_item(Indent, Item, S0, S) -->
		% Content on next line (- on its own line)
		peek_newline_char,
		!,
		skip_to_newline,
		newline,
		measure_indent(ChildIndent),
		{ ChildIndent > Indent },
		yaml_content(ChildIndent, Item, S0, S).
	block_sequence_item(Indent, Item, S0, S) -->
		% Check for inline mapping after -
		scalar_key(Key),
		skip_spaces,
		[0':],
		!,
		skip_spaces,
		{ ItemIndent is Indent + 2 },
		block_mapping_value(ItemIndent, Key, Pair, S0, S1),
		block_mapping_rest(ItemIndent, Pairs, S1, S),
		{ Item = yaml([Pair| Pairs]) }.
	block_sequence_item(Indent, Item, S0, S) -->
		% Regular value after -
		inline_value(Indent, Item, S0, S).

	% Block sequence rest with state threading
	block_sequence_rest(Indent, [Item| Items], S0, S) -->
		skip_to_newline,
		newline,
		skip_empty_lines,
		% Stop at document markers
		\+ peek_document_marker,
		measure_indent(CurrentIndent),
		{ CurrentIndent =:= Indent },
		[0'-],
		!,
		skip_spaces,
		block_sequence_item(Indent, Item, S0, S1),
		block_sequence_rest(Indent, Items, S1, S).
	block_sequence_rest(_Indent, [], S, S) -->
		[].

	% Inline value with state threading
	inline_value(_Indent, Value, S, S) -->
		[0'{],
		!,
		flow_mapping(Pairs),
		{ Value = yaml(Pairs) }.
	inline_value(_Indent, Value, S, S) -->
		[0'[],
		!,
		flow_sequence(Value).
	inline_value(_Indent, Value, S, S) -->
		[0'"],
		!,
		quoted_string_content(Codes),
		[0'"],
		{ atom_codes(Value, Codes) }.
	inline_value(_Indent, Value, S, S) -->
		[0'\'],
		!,
		single_quoted_content(Codes),
		[0'\'],
		{ atom_codes(Value, Codes) }.
	inline_value(_Indent, Value, S, S) -->
		% Literal block scalar (|)
		[0'|],
		!,
		block_scalar_header(Chomping),
		skip_to_newline,
		newline,
		literal_block_scalar(Chomping, Codes),
		{ atom_codes(Value, Codes) }.
	inline_value(_Indent, Value, S, S) -->
		% Folded block scalar (>)
		[0'>],
		!,
		block_scalar_header(Chomping),
		skip_to_newline,
		newline,
		folded_block_scalar(Chomping, Codes),
		{ atom_codes(Value, Codes) }.
	inline_value(Indent, Value, S0, S) -->
		% Anchor definition (&name value)
		[0'&],
		!,
		anchor_name(AnchorName),
		skip_spaces,
		% Parse the value that this anchor refers to
		inline_value(Indent, Value, S0, S1),
		% Store the anchor in state
		{ store_anchor(AnchorName, Value, S1, S) }.
	inline_value(_Indent, Value, S0, S0) -->
		% Alias reference (*name)
		[0'*],
		!,
		anchor_name(AliasName),
		% Look up the alias in state
		{ (lookup_anchor(AliasName, S0, Value) -> true ; Value = '@'(null)) }.
	inline_value(_Indent, Value, S, S) -->
		inline_scalar(Scalar),
		{ parse_scalar_or_number(Scalar, Value) }.

	% block Scalar Parsing (| and >)

	% Block scalar header - parse optional chomping indicator
	% - (strip): remove final line breaks
	% + (keep): keep final line breaks
	% (default/clip): single newline at end
	block_scalar_header(strip) -->
		[0'-],
		!.
	block_scalar_header(keep) -->
		[0'+],
		!.
	block_scalar_header(clip) -->
		[].

	% Literal block scalar (|) - preserves newlines
	literal_block_scalar(Chomping, Result) -->
		measure_indent(BaseIndent),
		{ BaseIndent > 0 },
		!,
		literal_block_lines(BaseIndent, Codes),
		{ apply_chomping(Chomping, Codes, Result) }.
	literal_block_scalar(_Chomping, []) -->
		[].

	% Collect lines for literal block scalar - returns flat list of codes
	literal_block_lines(BaseIndent, Codes) -->
		literal_block_line(BaseIndent, FirstLine),
		literal_block_lines_rest(BaseIndent, FirstLine, Codes).

	literal_block_lines_rest(BaseIndent, AccLine, Codes) -->
		% Check if there's a newline followed by content at sufficient indent
		peek_code(Code),
		{ Code =:= 10 ; Code =:= 13 },
		!,
		% Peek at what's after the newline
		(   peek_next_line_indent_literal(BaseIndent)
		->  % More content for this block scalar
		    newline,
		    literal_block_lines_continue(BaseIndent, AccLine, Codes)
		;   % End of block scalar - don't consume newline
		    { Codes = AccLine }
		).
	literal_block_lines_rest(_BaseIndent, AccLine, AccLine) -->
		% End of input
		[].

	% After consuming newline, continue collecting lines
	literal_block_lines_continue(BaseIndent, AccLine, Codes) -->
		% Check for blank line
		peek_code(C),
		{ C =:= 10 ; C =:= 13 },
		!,
		{ append(AccLine, [10], NewAcc) },
		literal_block_lines_rest(BaseIndent, NewAcc, Codes).
	literal_block_lines_continue(BaseIndent, AccLine, Codes) -->
		peek_indent(CurrentIndent),
		{ CurrentIndent >= BaseIndent },
		!,
		% Now actually consume the indentation
		consume_indent(CurrentIndent),
		% Add extra indentation as spaces
		{ ExtraIndent is CurrentIndent - BaseIndent },
		{ n_spaces(ExtraIndent, ExtraSpaces) },
		literal_block_line(CurrentIndent, LineCodes),
		{ append(ExtraSpaces, LineCodes, Line) },
		{ append(AccLine, [10| Line], NewAcc) },
		literal_block_lines_rest(BaseIndent, NewAcc, Codes).
	literal_block_lines_continue(_BaseIndent, AccLine, AccLine) -->
		[].

	% Peek if next line has sufficient indent for literal block (without consuming)
	% Check if next line has sufficient indent for literal block scalar
	% Uses helper predicate to check input list directly without complex pushback
	peek_next_line_indent_literal(MinIndent) -->
		call(current_input(Input)),
		{ has_next_line_with_indent(MinIndent, Input) }.

	% Get the current input without consuming anything
	% Uses DCG state-passing notation
	current_input(Input, Input, Input).

	% Read a single line of content (after indentation consumed)
	literal_block_line(_BaseIndent, Codes) -->
		line_content(Codes).

	line_content([Code| Codes]) -->
		[Code],
		{ Code =\= 10, Code =\= 13 },
		!,
		line_content(Codes).
	line_content([]) -->
		[].

	% Folded block scalar (>) - folds newlines into spaces
	folded_block_scalar(Chomping, Result) -->
		measure_indent(BaseIndent),
		{ BaseIndent > 0 },
		!,
		folded_block_lines(BaseIndent, Lines),
		{ fold_lines(Lines, Folded) },
		{ apply_chomping(Chomping, Folded, Result) }.
	folded_block_scalar(_Chomping, []) -->
		[].

	% Collect lines for folded block scalar
	folded_block_lines(BaseIndent, [line(Line)| Lines]) -->
		folded_block_line(BaseIndent, Line),
		folded_block_lines_rest(BaseIndent, Lines).

	folded_block_lines_rest(BaseIndent, Lines) -->
		% Check if there's a newline followed by content at sufficient indent
		peek_code(Code),
		{ Code =:= 10 ; Code =:= 13 },
		!,
		% Peek at what's after the newline
		(   peek_next_line_indent(BaseIndent)
		->  % More content for this block scalar
		    newline,
		    folded_block_lines_continue(BaseIndent, Lines)
		;   % End of block scalar - don't consume newline
		    { Lines = [] }
		).
	folded_block_lines_rest(_BaseIndent, []) -->
		% End of input
		[].

	% After consuming newline, continue collecting lines
	folded_block_lines_continue(BaseIndent, Lines) -->
		% Check for blank line
		peek_code(C),
		{ C =:= 10 ; C =:= 13 },
		!,
		{ Lines = [blank| RestLines] },
		folded_block_lines_rest(BaseIndent, RestLines).
	folded_block_lines_continue(BaseIndent, Lines) -->
		peek_indent(CurrentIndent),
		{ CurrentIndent >= BaseIndent },
		!,
		% Now actually consume the indentation
		consume_indent(CurrentIndent),
		% Check if more indented (preserve newlines for more indented)
		{ ExtraIndent is CurrentIndent - BaseIndent },
		(   { ExtraIndent > 0 }
		->  { n_spaces(ExtraIndent, ExtraSpaces) },
		    folded_block_line(CurrentIndent, LineCodes),
		    { append(ExtraSpaces, LineCodes, Line) },
		    { Lines = [more(Line)| RestLines] }
		;   folded_block_line(CurrentIndent, Line),
		    { Lines = [line(Line)| RestLines] }
		),
		folded_block_lines_rest(BaseIndent, RestLines).
	folded_block_lines_continue(_BaseIndent, []) -->
		[].

	% Check if next line has sufficient indent for folded block scalar
	% Uses helper predicate to check input list directly
	peek_next_line_indent(MinIndent) -->
		call(current_input(Input)),
		{ has_next_line_with_indent(MinIndent, Input) }.

	folded_block_line(_BaseIndent, Codes) -->
		line_content(Codes).

	% Fold lines according to YAML folding rules
	fold_lines([], []).
	fold_lines([line(L)], L) :- !.
	fold_lines([line(L), blank| Rest], Result) :-
		!,
		count_blanks([blank| Rest], BlankCount, AfterBlanks),
		fold_lines(AfterBlanks, RestResult),
		% Blank lines become newlines
		n_newlines(BlankCount, Newlines),
		append(L, [10| Newlines], Temp),
		append(Temp, RestResult, Result).
	fold_lines([line(L), line(L2)| Rest], Result) :-
		!,
		% Consecutive lines are joined with a space
		append(L, [32], Temp),
		fold_lines([line(L2)| Rest], RestResult),
		append(Temp, RestResult, Result).
	fold_lines([line(L), more(M)| Rest], Result) :-
		!,
		% More indented lines preserve newline
		append(L, [10| M], Temp),
		fold_lines_after_more(Rest, RestResult),
		append(Temp, RestResult, Result).
	fold_lines([more(M)| Rest], Result) :-
		!,
		fold_lines_after_more(Rest, RestResult),
		append(M, RestResult, Result).
	fold_lines([blank| Rest], Result) :-
		fold_lines(Rest, Result).

	fold_lines_after_more([], []).
	fold_lines_after_more([more(M)| Rest], Result) :-
		!,
		fold_lines_after_more(Rest, RestResult),
		append([10| M], RestResult, Result).
	fold_lines_after_more([blank| Rest], [10| Result]) :-
		!,
		fold_lines_after_more(Rest, Result).
	fold_lines_after_more([line(L)| Rest], Result) :-
		!,
		fold_lines([line(L)| Rest], RestResult),
		Result = [10| RestResult].

	count_blanks([blank| Rest], Count, After) :-
		!,
		count_blanks(Rest, Count1, After),
		Count is Count1 + 1.
	count_blanks(List, 0, List).

	% Apply chomping to the result
	apply_chomping(strip, Codes, Result) :-
		!,
		strip_trailing_newlines(Codes, Result).
	apply_chomping(keep, Codes, Result) :-
		!,
		% Keep all trailing newlines, add one if none
		(   Codes == []
		->  Result = []
		;   Result = Codes
		).
	apply_chomping(clip, Codes, Result) :-
		% Default: single trailing newline
		strip_trailing_newlines(Codes, Stripped),
		(   Stripped == []
		->  Result = []
		;   append(Stripped, [10], Result)
		).

	strip_trailing_newlines(Codes, Result) :-
		reverse(Codes, Rev),
		strip_leading_newlines(Rev, RevResult),
		reverse(RevResult, Result).

	strip_leading_newlines([10| Codes], Result) :-
		!,
		strip_leading_newlines(Codes, Result).
	strip_leading_newlines([13| Codes], Result) :-
		!,
		strip_leading_newlines(Codes, Result).
	strip_leading_newlines(Result, Result).

	% Check if input starts with newline followed by line with sufficient indent
	% Used by block scalar parsing to look ahead without DCG pushback
	has_next_line_with_indent(MinIndent, [Code| Codes]) :-
		(Code =:= 10 -> true ; Code =:= 13),
		count_leading_spaces_and_check(Codes, 0, MinIndent).

	% Count leading spaces and check if line has sufficient indent or is blank
	count_leading_spaces_and_check([32| Codes], Acc, MinIndent) :-
		!,
		Acc1 is Acc + 1,
		count_leading_spaces_and_check(Codes, Acc1, MinIndent).
	count_leading_spaces_and_check([9| Codes], Acc, MinIndent) :-
		!,
		Acc1 is Acc + 2,  % Tab counts as 2 spaces
		count_leading_spaces_and_check(Codes, Acc1, MinIndent).
	count_leading_spaces_and_check([Code| _], Acc, MinIndent) :-
		% Non-whitespace character - check indent
		Code =\= 32, Code =\= 9,
		(   (Code =:= 10 ; Code =:= 13)
		->  true  % Blank line is OK (continue to next line)
		;   Acc >= MinIndent  % Content line must have sufficient indent
		).
	count_leading_spaces_and_check([], _, _) :-
		% End of input - OK to continue
		true.

	% Generate N space characters (code 32)
	n_spaces(0, []) :- !.
	n_spaces(N, [32| Codes]) :-
		N > 0,
		N1 is N - 1,
		n_spaces(N1, Codes).

	% Generate N newline characters (code 10)
	n_newlines(0, []) :- !.
	n_newlines(N, [10| Codes]) :-
		N > 0,
		N1 is N - 1,
		n_newlines(N1, Codes).

	% Anchor name parsing
	anchor_name(Name) -->
		anchor_name_codes(Codes),
		{ Codes \= [] },
		{ atom_codes(Name, Codes) }.

	anchor_name_codes([Code| Codes]) -->
		[Code],
		{ Code =\= 32, Code =\= 9, Code =\= 10, Code =\= 13,
		  Code =\= 0':, Code =\= 0',, Code =\= 0'[, Code =\= 0'],
		  Code =\= 0'{, Code =\= 0'} },
		!,
		anchor_name_codes(Codes).
	anchor_name_codes([]) -->
		[].

	% anchor/Alias State Management
	% state is a list of AnchorName-Value pairs

	% Look up an anchor in the state
	lookup_anchor(Name, [Name-Value| _], Value) :-
		!.
	lookup_anchor(Name, [_| Rest], Value) :-
		lookup_anchor(Name, Rest, Value).

	% Store an anchor in the state (prepend to list)
	store_anchor(Name, Value, StateIn, [Name-Value| StateIn]).

	% Extract pairs from merge key value
	% The value can be a yaml(Pairs) or a list of yaml(Pairs)
	merge_key_pairs(yaml(Pairs), Pairs) :- !.
	merge_key_pairs([], []) :- !.
	merge_key_pairs([yaml(Pairs)| Rest], AllPairs) :-
		!,
		merge_key_pairs(Rest, RestPairs),
		append(Pairs, RestPairs, AllPairs).
	merge_key_pairs([_| Rest], Pairs) :-
		% Skip non-mapping values in merge list
		merge_key_pairs(Rest, Pairs).
	merge_key_pairs(_, []).
	% Non-mapping merge value - return empty

	% Flow mapping: {key: value, ...}
	flow_mapping(Pairs) -->
		skip_whitespace,
		(   [0'}]
		->  { Pairs = [] }
		;   flow_mapping_pairs(Pairs),
		    skip_whitespace,
		    [0'}]
		).

	flow_mapping_pairs([Key-Value| Pairs]) -->
		skip_whitespace,
		% Check if we have a closing brace (handles trailing comma)
		peek_code(C),
		{ C =\= 0'} },
		!,
		flow_key(Key),
		skip_whitespace,
		[0':],
		skip_whitespace,
		flow_value(Value),
		skip_whitespace,
		(   [0',]
		->  flow_mapping_pairs(Pairs)
		;   { Pairs = [] }
		).
	flow_mapping_pairs([]) -->
		skip_whitespace.

	% Flow sequence: [item, ...]
	flow_sequence(Items) -->
		skip_whitespace,
		(   [0']]
		->  { Items = [] }
		;   flow_sequence_items(Items),
		    skip_whitespace,
		    [0']]
		).

	flow_sequence_items([Item| Items]) -->
		skip_whitespace,
		flow_value(Item),
		skip_whitespace,
		(   [0',]
		->  flow_sequence_items(Items)
		;   { Items = [] }
		).

	% Flow key (in flow mapping)
	flow_key(Key) -->
		[0'"],
		!,
		quoted_string_content(Codes),
		[0'"],
		{ atom_codes(Key, Codes) }.
	flow_key(Key) -->
		[0'\'],
		!,
		single_quoted_content(Codes),
		[0'\'],
		{ atom_codes(Key, Codes) }.
	flow_key(Key) -->
		flow_scalar(Codes),
		{ atom_codes(Key, Codes) }.

	% Flow value (in flow mapping or sequence)
	flow_value(Value) -->
		[0'{],
		!,
		flow_mapping(Pairs),
		{ Value = yaml(Pairs) }.
	flow_value(Value) -->
		[0'[],
		!,
		flow_sequence(Value).
	flow_value(Value) -->
		[0'"],
		!,
		quoted_string_content(Codes),
		[0'"],
		{ atom_codes(Value, Codes) }.
	flow_value(Value) -->
		[0'\'],
		!,
		single_quoted_content(Codes),
		[0'\'],
		{ atom_codes(Value, Codes) }.
	flow_value(Value) -->
		flow_scalar(Codes),
		{	trim_trailing_spaces(Codes, Trimmed),
			atom_codes(Atom, Trimmed),
			parse_scalar_or_number(Atom, Value)
		}.

	% Scalar key (unquoted, stops at colon followed by space/newline/end)
	scalar_key(Key) -->
		scalar_key_codes(Codes),
		{	Codes \= [],
			trim_trailing_spaces(Codes, Trimmed),
			atom_codes(Key, Trimmed)
		}.

	% Key can contain spaces but ends at ": " or ":\n" or ":end"
	% First character cannot be a quote
	scalar_key_codes([Code| Codes]) -->
		[Code],
		{ \+ member(Code, [0':, 10, 13, 0'", 0'\']) },
		!,
		scalar_key_codes_rest(Codes).
	scalar_key_codes([]) -->
		[].

	% Rest of key can contain colons if followed by non-space
	scalar_key_codes_rest([0':| Codes]) -->
		[0':],
		% Colon followed by non-space is part of the key (e.g., "time:20:03:20")
		peek_code(Code),
		{ Code =\= 32, Code =\= 9, Code =\= 10, Code =\= 13 },
		!,
		scalar_key_codes_rest(Codes).
	scalar_key_codes_rest([Code| Codes]) -->
		[Code],
		{ \+ member(Code, [0':, 10, 13]) },
		!,
		scalar_key_codes_rest(Codes).
	scalar_key_codes_rest([]) -->
		[].

	% Flow scalar (stops at flow indicators)
	flow_scalar([Code| Codes]) -->
		[Code],
		{ \+ member(Code, [0':, 0',, 0'{, 0'}, 0'[, 0'], 10, 13]) },
		!,
		flow_scalar(Codes).
	flow_scalar([]) -->
		[].

	% Inline scalar (to end of line, stops at comment)
	inline_scalar(Scalar) -->
		inline_scalar_codes(Codes),
		{	trim_trailing_spaces(Codes, Trimmed),
			atom_codes(Scalar, Trimmed)
		}.

	inline_scalar_codes([Code| Codes]) -->
		[Code],
		{ \+ member(Code, [10, 13, 0'#]) },
		!,
		inline_scalar_codes(Codes).
	inline_scalar_codes([]) -->
		[].

	% Quoted string content
	% Handle \xXX hex escape (2 hex digits)
	quoted_string_content([Code| Codes]) -->
		[0'\\, 0'x],
		!,
		[H1, H2],
		{ hex_digit(H1, V1), hex_digit(H2, V2), Code is V1 * 16 + V2 },
		quoted_string_content(Codes).
	% Handle \uXXXX unicode escape (4 hex digits)
	quoted_string_content([Code| Codes]) -->
		[0'\\, 0'u],
		!,
		[H1, H2, H3, H4],
		{	hex_digit(H1, V1), hex_digit(H2, V2),
			hex_digit(H3, V3), hex_digit(H4, V4),
			Code is V1 * 4096 + V2 * 256 + V3 * 16 + V4
		},
		quoted_string_content(Codes).
	% Handle other escape sequences
	quoted_string_content([Code| Codes]) -->
		[0'\\],
		!,
		[Escaped],
		{ unescape_char(Escaped, Code) },
		quoted_string_content(Codes).
	% Handle line folding: newline followed by optional whitespace
	% In flow scalars, newlines are folded into spaces
	quoted_string_content(Codes) -->
		flow_newline,
		!,
		% Skip leading whitespace on next line
		skip_flow_line_whitespace,
		% Check for additional blank lines
		quoted_string_fold_lines(Codes).
	quoted_string_content([Code| Codes]) -->
		[Code],
		{ Code =\= 0'" },
		!,
		quoted_string_content(Codes).
	quoted_string_content([]) -->
		[].

	% Match a single line ending (LF, CR, or CRLF as a unit)
	flow_newline --> [13, 10], !.  % CRLF (Windows)
	flow_newline --> [10], !.      % LF (Unix)
	flow_newline --> [13].         % CR (old Mac)

	% Handle blank lines in multi-line flow scalar
	% First newline becomes space, additional blank lines become newlines
	quoted_string_fold_lines(Codes) -->
		% Another newline - this becomes a literal newline
		flow_newline,
		!,
		skip_flow_line_whitespace,
		quoted_string_fold_lines_after_blank(Codes).
	quoted_string_fold_lines([32| Codes]) -->
		% Non-blank line - the original newline becomes a space
		quoted_string_content(Codes).

	% After at least one blank line, add newlines for each blank
	quoted_string_fold_lines_after_blank([10| Codes]) -->
		% Another newline - add another literal newline
		flow_newline,
		!,
		skip_flow_line_whitespace,
		quoted_string_fold_lines_after_blank(Codes).
	quoted_string_fold_lines_after_blank([10| Codes]) -->
		% Non-blank line after blank lines - add one newline and continue
		quoted_string_content(Codes).

	% Skip whitespace at start of continuation line
	skip_flow_line_whitespace -->
		[Code],
		{ Code =:= 32 ; Code =:= 9 },
		!,
		skip_flow_line_whitespace.
	skip_flow_line_whitespace -->
		[].

	% Single quoted string content
	single_quoted_content([0'\'| Codes]) -->
		[0'\'], [0'\'],
		!,
		single_quoted_content(Codes).
	% Handle line folding in single-quoted strings (same as double-quoted)
	single_quoted_content(Codes) -->
		flow_newline,
		!,
		skip_flow_line_whitespace,
		single_quoted_fold_lines(Codes).
	single_quoted_content([Code| Codes]) -->
		[Code],
		{ Code =\= 0'\' },
		!,
		single_quoted_content(Codes).
	single_quoted_content([]) -->
		[].

	% Handle blank lines in multi-line single-quoted scalar
	single_quoted_fold_lines(Codes) -->
		flow_newline,
		!,
		skip_flow_line_whitespace,
		single_quoted_fold_lines_after_blank(Codes).
	single_quoted_fold_lines([32| Codes]) -->
		% Non-blank line - the original newline becomes a space
		single_quoted_content(Codes).

	% After at least one blank line, add newlines for each blank
	single_quoted_fold_lines_after_blank([10| Codes]) -->
		flow_newline,
		!,
		skip_flow_line_whitespace,
		single_quoted_fold_lines_after_blank(Codes).
	single_quoted_fold_lines_after_blank([10| Codes]) -->
		% Non-blank line after blank lines - add one newline and continue
		single_quoted_content(Codes).

	% Unescape characters in double-quoted strings
	unescape_char(0'n, 10) :- !.    % newline
	unescape_char(0't, 9) :- !.     % tab
	unescape_char(0'r, 13) :- !.    % carriage return
	unescape_char(0'b, 8) :- !.     % backspace
	unescape_char(0'f, 12) :- !.    % form feed
	unescape_char(0'0, 0) :- !.     % null character
	unescape_char(0'\\, 0'\\) :- !. % backslash
	unescape_char(0'", 0'") :- !.   % double quote
	unescape_char(0'/, 0'/) :- !.   % forward slash
	unescape_char(C, C).

	% helper DCG rules for parsing

	% Peek at next character without consuming
	peek_code(Code), [Code] --> [Code].

	% Peek for non-newline character
	peek_non_newline_char, [Code] -->
		[Code],
		{ Code =\= 10, Code =\= 13 }.

	% Peek for newline character
	peek_newline_char, [Code] -->
		[Code],
		{ Code =:= 10 ; Code =:= 13 }.

	% Peek for scalar start (not newline, not whitespace, not end of input)
	peek_scalar_start, [Code] -->
		[Code],
		{ \+ member(Code, [10, 13, 32, 9]) }.

	% Skip spaces (not newlines)
	skip_spaces -->
		[Code],
		{ member(Code, [32, 9]) },
		!,
		skip_spaces.
	skip_spaces -->
		[].

	% Skip whitespace (spaces and tabs only)
	skip_whitespace -->
		[Code],
		{ member(Code, [32, 9, 10, 13]) },
		!,
		skip_whitespace.
	skip_whitespace -->
		[].

	% Skip whitespace, comments, and document start markers
	skip_whitespace_and_comments -->
		[0'-, 0'-, 0'-],
		!,
		skip_spaces,
		% Check if there's content after --- on same line
		(   peek_code(Code),
		    { Code =\= 10, Code =\= 13, Code =\= 0'! }
		->  % Real content on same line as --- (e.g., --- |), leave for parser
		    []
		;   % Tag, newline, or spaces after ---, skip it
		    skip_tag_if_present,
		    skip_to_newline,
		    skip_whitespace_and_comments
		).
	skip_whitespace_and_comments -->
		[0'#],
		!,
		skip_to_newline,
		skip_whitespace_and_comments.
	skip_whitespace_and_comments -->
		[Code],
		{ member(Code, [32, 9, 10, 13]) },
		!,
		skip_whitespace_and_comments.
	skip_whitespace_and_comments -->
		[].

	% Skip a YAML tag if present (e.g., !<tag:...> or !!type or !tag)
	skip_tag_if_present -->
		[0'!],
		!,
		skip_tag_content.
	skip_tag_if_present -->
		[].

	% Skip tag content
	skip_tag_content -->
		% Verbatim tag: !<...>
		[0'<],
		!,
		skip_until_char(0'>),
		[0'>],
		skip_spaces.
	skip_tag_content -->
		% Short or named tag: !! or !name
		skip_tag_name,
		skip_spaces.

	% Skip until specific character
	skip_until_char(Target) -->
		[Code],
		{ Code =\= Target },
		!,
		skip_until_char(Target).
	skip_until_char(_) -->
		[].

	% Skip tag name characters
	skip_tag_name -->
		[Code],
		{ Code =\= 32, Code =\= 9, Code =\= 10, Code =\= 13 },
		!,
		skip_tag_name.
	skip_tag_name -->
		[].

	% Skip to end of line
	skip_to_newline -->
		[Code],
		{ \+ member(Code, [10, 13]) },
		!,
		skip_to_newline.
	skip_to_newline -->
		[].

	% Consume newline
	newline --> [10].
	newline --> [13], [10].
	newline --> [13].

	% Skip empty lines (lines with only whitespace)
	skip_empty_lines -->
		peek_empty_line,
		!,
		skip_to_newline,
		newline,
		skip_empty_lines.
	skip_empty_lines -->
		[].

	% Peek if current line is empty (only whitespace until newline)
	peek_empty_line -->
		peek_code(Code),
		{ member(Code, [10, 13]) }.

	% Measure indentation (count spaces at start of line) - consumes the spaces
	measure_indent(Indent) -->
		count_spaces(0, Indent).

	count_spaces(Acc, Indent) -->
		[32],
		!,
		{ Acc1 is Acc + 1 },
		count_spaces(Acc1, Indent).
	count_spaces(Acc, Indent) -->
		[9],
		!,
		{ Acc1 is Acc + 2 },  % Tab counts as 2 spaces
		count_spaces(Acc1, Indent).
	count_spaces(Indent, Indent) -->
		[].

	% Peek indentation without consuming - for block scalar lookahead
	peek_indent(Indent) -->
		peek_count_spaces(0, Indent).

	peek_count_spaces(Acc, Indent), [Code] -->
		[Code],
		{ Code =:= 32 },
		!,
		{ Acc1 is Acc + 1 },
		peek_count_spaces(Acc1, Indent).
	peek_count_spaces(Acc, Indent), [Code] -->
		[Code],
		{ Code =:= 9 },
		!,
		{ Acc1 is Acc + 2 },  % Tab counts as 2 spaces
		peek_count_spaces(Acc1, Indent).
	peek_count_spaces(Indent, Indent) -->
		[].

	% Consume N spaces/tabs worth of indentation
	consume_indent(0) --> !.
	consume_indent(N) -->
		[32],
		!,
		{ N1 is N - 1 },
		consume_indent(N1).
	consume_indent(N) -->
		[9],
		!,
		{ N1 is N - 2 },  % Tab counts as 2 spaces
		consume_indent(N1).
	consume_indent(_) --> [].

	% Skip trailing whitespace, newlines, and comments
	skip_trailing -->
		[0'#],
		!,
		skip_to_newline,
		skip_trailing.
	skip_trailing -->
		[Code],
		{ member(Code, [32, 9, 10, 13]) },
		!,
		skip_trailing.
	skip_trailing -->
		[].

	% scalar value parsing helpers

	parse_scalar_value(true, '@'(true)) :- !.
	parse_scalar_value(false, '@'(false)) :- !.
	parse_scalar_value(Scalar, '@'(null)) :-
		member(Scalar, [null, 'Null', 'NULL', '~', '']),
		!.
	parse_scalar_value(Scalar, Scalar).

	% Parse special float values (.inf, -.inf, .nan)
	parse_scalar_or_number(Atom, Number) :-
		atom(Atom),
		parse_special_float(Atom, Number),
		!.
	% Parse signed positive numbers (+12345)
	parse_scalar_or_number(Atom, Number) :-
		atom(Atom),
		atom_codes(Atom, [0'+| Codes]),
		Codes \= [],
		catch(number_codes(Number, Codes), _, fail),
		!.
	% Parse octal numbers (0o14)
	parse_scalar_or_number(Atom, Number) :-
		atom(Atom),
		sub_atom(Atom, 0, 2, After, '0o'),
		After > 0,
		atom_codes(Atom, Codes),
		number_codes(Number, Codes),
		!.
	% Parse hexadecimal numbers (0x1A)
	parse_scalar_or_number(Atom, Number) :-
		atom(Atom),
		sub_atom(Atom, 0, 2, After, '0x'),
		After > 0,
		atom_codes(Atom, Codes),
		number_codes(Number, Codes),
		!.
	% Standard number parsing
	parse_scalar_or_number(Atom, Number) :-
		atom(Atom),
		catch((atom_codes(Atom, Codes), number_codes(Number, Codes)), _, fail),
		!.
	parse_scalar_or_number(Atom, Value) :-
		parse_scalar_value(Atom, Value).

	% Special float values - represented as @(inf), @(-inf), @(nan)
	parse_special_float(Atom, '@'(inf)) :-
		member(Atom, ['.inf', '.Inf', '.INF']),
		!.
	parse_special_float(Atom, '@'(-inf)) :-
		member(Atom, ['-.inf', '-.Inf', '-.INF']),
		!.
	parse_special_float(Atom, '@'(nan)) :-
		member(Atom, ['.nan', '.NaN', '.NAN']).

	% Trim trailing spaces from a list of codes
	trim_trailing_spaces(Codes, Trimmed) :-
		reverse(Codes, Reversed),
		drop_spaces(Reversed, TrimmedReversed),
		reverse(TrimmedReversed, Trimmed).

	drop_spaces([32| Codes], Trimmed) :- !,
		drop_spaces(Codes, Trimmed).
	drop_spaces([9| Codes], Trimmed) :- !,
		drop_spaces(Codes, Trimmed).
	drop_spaces(Codes, Codes).

	% Hex digit conversion (for \xXX and \uXXXX escapes)
	hex_digit(Code, Value) :- Code >= 0'0, Code =< 0'9, !, Value is Code - 0'0.
	hex_digit(Code, Value) :- Code >= 0'a, Code =< 0'f, !, Value is Code - 0'a + 10.
	hex_digit(Code, Value) :- Code >= 0'A, Code =< 0'F, Value is Code - 0'A + 10.

	% DCG Rules for YAML Generation

	% Main generation entry point
	yaml_generate(Data) -->
		yaml_generate_value(Data).

	% Multi-document generation entry point
	yaml_generate_all([]) -->
		!,
		[].
	yaml_generate_all([Document]) -->
		% Single document - no separator needed
		!,
		yaml_generate_value(Document).
	yaml_generate_all([Document| Documents]) -->
		% Multiple documents - use --- separator
		[0'-, 0'-, 0'-],
		[10],  % newline
		yaml_generate_value(Document),
		[10],  % newline
		yaml_generate_all_rest(Documents).

	% Generate remaining documents with separators
	yaml_generate_all_rest([]) -->
		!,
		[].
	yaml_generate_all_rest([Document| Documents]) -->
		[0'-, 0'-, 0'-],
		[10],  % newline
		yaml_generate_value(Document),
		[10],  % newline
		yaml_generate_all_rest(Documents).

	% Generate special values
	yaml_generate_value('@'(null)) -->
		!,
		[0'n,0'u,0'l,0'l].
	yaml_generate_value('@'(true)) -->
		!,
		[0't,0'r,0'u,0'e].
	yaml_generate_value('@'(false)) -->
		!,
		[0'f,0'a,0'l,0's,0'e].
	% Generate atom
	yaml_generate_value(Atom) -->
		{ atom(Atom) },
		!,
		{ atom_codes(Atom, Codes) },
		codes(Codes).
	% Generate number
	yaml_generate_value(Number) -->
		{ number(Number) },
		!,
		{ number_codes(Number, Codes) },
		codes(Codes).
	% Generate flow mapping
	yaml_generate_value(yaml(Pairs)) -->
		!,
		[0'{],
		yaml_generate_pairs(Pairs),
		[0'}].
	% Generate flow sequence
	yaml_generate_value(List) -->
		{ is_list(List) },
		!,
		[0'[],
		yaml_generate_items(List),
		[0']].
	yaml_generate_value(Data) -->
		{ domain_error(yaml_term, Data) }.

	% Generate key-value pairs for flow mapping
	yaml_generate_pairs([]) -->
		!,
		[].
	yaml_generate_pairs([Key-Value]) -->
		!,
		yaml_generate_value(Key),
		[0':],
		yaml_generate_value(Value).
	yaml_generate_pairs([Key-Value| Pairs]) -->
		yaml_generate_value(Key),
		[0':],
		yaml_generate_value(Value),
		[0',],
		yaml_generate_pairs(Pairs).

	% Generate items for flow sequence
	yaml_generate_items([]) -->
		!,
		[].
	yaml_generate_items([Item]) -->
		!,
		yaml_generate_value(Item).
	yaml_generate_items([Item| Items]) -->
		yaml_generate_value(Item),
		[0',],
		yaml_generate_items(Items).

	% auxiliary predicates

	% Helper to output a list of codes
	codes([]) --> [].
	codes([Code| Codes]) --> [Code], codes(Codes).

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
