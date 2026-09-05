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


:- category(tokenizer_rules,
	implements(tokenizer_language_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Reusable portable rules for tokenization and sentence splitting. Importing objects supply language-specific abbreviation and word punctuation hooks. URL and email address recognition delegates to the ``url`` library.',
		see_also is [tokenizer_language_protocol, english_tokenizer, url(_)]
	]).

	:- protected(abbreviation/1).
	:- mode(abbreviation(?atom), zero_or_more).
	:- info(abbreviation/1, [
		comment is 'Enumerates canonical lowercase abbreviations, including their periods.',
		argnames is ['Abbreviation']
	]).

	:- protected(non_terminal_abbreviation/1).
	:- mode(non_terminal_abbreviation(?atom), zero_or_more).
	:- info(non_terminal_abbreviation/1, [
		comment is 'Enumerates abbreviations that do not terminate a sentence when followed by another token.',
		argnames is ['Abbreviation']
	]).

	:- protected(internal_apostrophe/1).
	:- mode(internal_apostrophe(?character_code), zero_or_more).
	:- info(internal_apostrophe/1, [
		comment is 'Enumerates apostrophe character codes accepted inside words.',
		argnames is ['Code']
	]).

	:- protected(internal_hyphen/1).
	:- mode(internal_hyphen(?character_code), zero_or_more).
	:- info(internal_hyphen/1, [
		comment is 'Enumerates hyphen character codes accepted inside words.',
		argnames is ['Code']
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2, take/3, take/4
	]).

	tokenize_codes(Codes, Tokens, Options) :-
		normalize_codes(Codes, Options, NormalizedCodes),
		^^option(keep_punctuation(KeepPunctuation), Options),
		^^option(lowercase(Lowercase), Options),
		tokens(NormalizedCodes, KeepPunctuation, Lowercase, Tokens).

	split_sentence_codes(Codes, Sentences, Options) :-
		normalize_codes(Codes, Options, NormalizedCodes),
		drop_spaces(NormalizedCodes, TrimmedCodes),
		sentences(TrimmedCodes, [], [], Sentences).

	normalize_codes([], _, []).
	normalize_codes([Code| Codes], Options, [NormalizedCode| NormalizedCodes]) :-
		normalize_code(Code, Options, NormalizedCode),
		normalize_codes(Codes, Options, NormalizedCodes).

	normalize_code(Code, Options, 39) :-
		^^option(normalize_quotes(true), Options),
		member(Code, [8216, 8217]),
		!.
	normalize_code(Code, Options, 34) :-
		^^option(normalize_quotes(true), Options),
		member(Code, [8220, 8221]),
		!.
	normalize_code(Code, Options, 45) :-
		^^option(normalize_dashes(true), Options),
		member(Code, [8208, 8209, 8210, 8211, 8212, 8213]),
		!.
	normalize_code(Code, _, Code).

	tokens([], _, _, []).
	tokens([Code| Codes], KeepPunctuation, Lowercase, Tokens) :-
		(	whitespace(Code) ->
			drop_spaces(Codes, Rest),
			tokens(Rest, KeepPunctuation, Lowercase, Tokens)
		;	web_token([Code| Codes], Token, Rest) ->
			emit_token(Token, Lowercase, Tokens, Tail),
			tokens(Rest, KeepPunctuation, Lowercase, Tail)
		;	abbreviation_token([Code| Codes], Token, Rest) ->
			emit_token(Token, Lowercase, Tokens, Tail),
			tokens(Rest, KeepPunctuation, Lowercase, Tail)
		; number_token([Code| Codes], Token, Rest) ->
			emit_token(Token, Lowercase, Tokens, Tail),
			tokens(Rest, KeepPunctuation, Lowercase, Tail)
		;	word_code(Code) ->
			word_token(Codes, [Code], Token, Rest),
			emit_token(Token, Lowercase, Tokens, Tail),
			tokens(Rest, KeepPunctuation, Lowercase, Tail)
		;	punctuation_token([Code| Codes], Token, Rest),
			emit_punctuation(KeepPunctuation, Token, Tokens, Tail),
			tokens(Rest, KeepPunctuation, Lowercase, Tail)
		).

	emit_token(Token, true, [LowercaseToken| Tokens], Tokens) :-
		!,
		string(codes)::string_lower(Token, LowercaseToken).
	emit_token(Token, false, [Token| Tokens], Tokens).

	emit_punctuation(true, Token, [Token| Tokens], Tokens).
	emit_punctuation(false, _, Tokens, Tokens).

	web_token(Codes, Token, Rest) :-
		take_nonspace(Codes, Candidate, CandidateRest),
		trim_web_punctuation(Candidate, Token, Trailing),
		Token \== [],
		(	url_codes(Token) ->
			true
		;	email_codes(Token)
		),
		append(Trailing, CandidateRest, Rest).

	take_nonspace([], [], []).
	take_nonspace([Code| Codes], [], [Code| Codes]) :-
		whitespace(Code),
		!.
	take_nonspace([Code| Codes], [Code| Token], Rest) :-
		take_nonspace(Codes, Token, Rest).

	trim_web_punctuation(Codes, Token, Trailing) :-
		string(codes)::trim_right(Codes, [33,34,39,41,44,46,58,59,63,93,125], Token),
		append(Token, Trailing, Codes).

	url_codes(Codes) :-
		url(codes)::valid(Codes).
	url_codes([W1,W2,W3,46| Codes]) :-
		lowercase_ascii(W1, 119),
		lowercase_ascii(W2, 119),
		lowercase_ascii(W3, 119),
		Codes \== [],
		url(codes)::valid([104,116,116,112,58,47,47,W1,W2,W3,46| Codes]).

	email_codes(Codes) :-
		Codes \== [],
		url(codes)::valid([109,97,105,108,116,111,58| Codes]).

	abbreviation_token(Codes, Token, Rest) :-
		findall(
			Length,
			(	::abbreviation(Abbreviation),
				atom_codes(Abbreviation, AbbreviationCodes),
				prefix_case_insensitive(AbbreviationCodes, Codes, Rest0),
				token_boundary(Rest0),
				length(AbbreviationCodes, Length)
			),
			Lengths
		),
		longest_length(Lengths, Length),
		take(Length, Codes, Token, Rest).

	longest_length([Length| Lengths], Longest) :-
		longest_length(Lengths, Length, Longest).

	longest_length([], Longest, Longest).
	longest_length([Length| Lengths], Current, Longest) :-
		(	Length > Current ->
			Next = Length
		;	Next = Current
		),
		longest_length(Lengths, Next, Longest).

	prefix_case_insensitive([], Rest, Rest).
	prefix_case_insensitive([Expected| Prefix], [Code| Codes], Rest) :-
		lowercase_ascii(Code, Lowercase),
		Expected =:= Lowercase,
		prefix_case_insensitive(Prefix, Codes, Rest).

	token_boundary([]).
	token_boundary([Code| _]) :-
		\+ word_code(Code).

	number_token(Codes, Token, Rest) :-
		grouped_number_token(Codes, Token, Rest),
		!.
	number_token(Codes, Token, Rest) :-
		phrase(number_grammars(codes)::number(_), Codes, Rest),
		consumed_prefix(Codes, Rest, Token).

	grouped_number_token([Code| Codes], Token, Rest) :-
		digit(Code),
		grouped_number_codes(Codes, [Code], Token, Rest),
		member(44, Token).

	grouped_number_codes([Code| Codes], Reversed, Token, Rest) :-
		digit(Code),
		!,
		grouped_number_codes(Codes, [Code| Reversed], Token, Rest).
	grouped_number_codes([Separator, Next| Codes], Reversed, Token, Rest) :-
		member(Separator, [44, 46]),
		digit(Next),
		!,
		grouped_number_codes(Codes, [Next, Separator| Reversed], Token, Rest).
	grouped_number_codes(Codes, Reversed, Token, Codes) :-
		reverse(Reversed, Token).

	consumed_prefix(Codes, Rest, Token) :-
		length(Codes, CodesLength),
		length(Rest, RestLength),
		TokenLength is CodesLength - RestLength,
		take(TokenLength, Codes, Token).

	word_token([Code, Next| Codes], Reversed, Token, Rest) :-
		internal_word_punctuation(Code),
		word_code(Next),
		!,
		word_token_codes(Codes, [Next, Code| Reversed], Token, Rest).
	word_token(Codes, Reversed, Token, Rest) :-
		word_token_codes(Codes, Reversed, Token, Rest).

	word_token_codes([Code| Codes], Reversed, Token, Rest) :-
		word_code(Code),
		!,
		word_token(Codes, [Code| Reversed], Token, Rest).
	word_token_codes(Codes, Reversed, Token, Codes) :-
		reverse(Reversed, Token).

	internal_word_punctuation(Code) :-
		::internal_apostrophe(Code).
	internal_word_punctuation(Code) :-
		::internal_hyphen(Code).

	punctuation_token([46,46,46| Codes], [46,46,46], Codes) :-
		!.
	punctuation_token([Code| Codes], [Code], Codes).

	sentences([], [], _, []) :-
		!.
	sentences([], Current, _, [Current]) :-
		Current \== [],
		!.
	sentences(Codes, Current, PendingSpaces, Sentences) :-
		take_nonspace(Codes, Chunk, Rest0),
		take_spaces(Rest0, Spaces, Rest),
		append(Current, PendingSpaces, Prefix),
		append(Prefix, Chunk, NextCurrent),
		(	sentence_boundary(Chunk, Rest) ->
			Sentences = [NextCurrent| MoreSentences],
			drop_spaces(Rest, NextCodes),
			sentences(NextCodes, [], [], MoreSentences)
		;	sentences(Rest, NextCurrent, Spaces, Sentences)
		).

	take_spaces([Code| Codes], [Code| Spaces], Rest) :-
		whitespace(Code),
		!,
		take_spaces(Codes, Spaces, Rest).
	take_spaces(Codes, [], Codes).

	drop_spaces([Code| Codes], Rest) :-
		whitespace(Code),
		!,
		drop_spaces(Codes, Rest).
	drop_spaces(Codes, Codes).

	sentence_boundary(Chunk, Rest) :-
		terminal_chunk(Chunk, Terminal, Core),
		!,
		(	member(Terminal, [33, 63]) ->
			true
		;	period_boundary(Chunk, Core, Rest)
		).

	terminal_chunk(Chunk, Terminal, Core) :-
		reverse(Chunk, Reversed),
		drop_closing_codes(Reversed, [Terminal| ReversedCore]),
		member(Terminal, [33, 46, 63]),
		reverse(ReversedCore, Core).

	drop_closing_codes([Code| Codes], Rest) :-
		closing_code(Code),
		!,
		drop_closing_codes(Codes, Rest).
	drop_closing_codes(Codes, Codes).

	closing_code(34).
	closing_code(39).
	closing_code(41).
	closing_code(93).
	closing_code(125).

	period_boundary(Chunk, Core, Rest) :-
		(	ellipsis_ending(Chunk) ->
			true
		;	protected_token_terminal(Chunk) ->
			true
		;	url_codes(Chunk) ->
			fail
		;	email_codes(Chunk) ->
			fail
		;	number_codes_token(Chunk) ->
			fail
		;	lowercase_atom(Chunk, Abbreviation),
			::abbreviation(Abbreviation) ->
			abbreviation_boundary(Abbreviation, Rest)
		;	Core \== [],
			true
		).

	protected_token_terminal(Chunk) :-
		append(Token, [46], Chunk),
		Token \== [],
		(	url_codes(Token) ->
			true
		;	email_codes(Token) ->
			true
		;	number_codes_token(Token)
		).

	abbreviation_boundary(Abbreviation, Rest) :-
		(	::non_terminal_abbreviation(Abbreviation), Rest \== [] ->
			fail
		;	Rest == [] ->
			true
		;	next_sentence_start(Rest)
		).

	next_sentence_start(Codes) :-
		drop_spaces(Codes, [Code| _]),
		(	uppercase_ascii(Code) ->
			true
		;	Code >= 192
		).

	ellipsis_ending(Chunk) :-
		append(_, [46,46,46], Chunk).

	number_codes_token(Codes) :-
		number_token(Codes, _, []).

	lowercase_atom(Codes, Atom) :-
		string(codes)::string_lower(Codes, LowercaseCodes),
		atom_codes(Atom, LowercaseCodes).

	whitespace(Code) :-
		Code =< 32,
		!.
	whitespace(160).

	digit(Code) :-
		Code >= 48,
		Code =< 57.

	word_code(Code) :-
		lowercase_ascii(Code),
		!.
	word_code(Code) :-
		uppercase_ascii(Code),
		!.
	word_code(Code) :-
		Code >= 128,
		\+ unicode_punctuation(Code).

	lowercase_ascii(Code) :-
		Code >= 97,
		Code =< 122.

	uppercase_ascii(Code) :-
		Code >= 65,
		Code =< 90.

	lowercase_ascii(Code, Lowercase) :-
		(	uppercase_ascii(Code) ->
			Lowercase is Code + 32
		;	Lowercase = Code
		).

	unicode_punctuation(Code) :-
		member(Code, [8216, 8217, 8220, 8221, 8208, 8209, 8210, 8211, 8212, 8213]).

:- end_category.
