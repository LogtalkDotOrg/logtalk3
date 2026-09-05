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


:- object(tokenizer(_Representation_, _Language_),
	implements([tokenizer_protocol, sentence_splitter_protocol]),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Tokenizer and sentence splitter parameterized by text representation and language provider.',
		parameters is [
			'Representation' - 'Text representation. Valid values are ``atom``, ``codes``, and ``chars``.',
			'Language' - 'Object implementing the ``tokenizer_language_protocol`` protocol.'
		],
		see_also is [tokenizer_protocol, sentence_splitter_protocol, tokenizer_language_protocol]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(type, [
		check/2, valid/2
	]).

	tokenize(Text, Tokens) :-
		tokenize(Text, Tokens, []).

	tokenize(Text, Tokens, UserOptions) :-
		prepare(Text, UserOptions, Codes, Options),
		_Language_::tokenize_codes(Codes, TokenCodes, Options),
		codes_texts(TokenCodes, Tokens).

	tokenize_sentences(Text, TokenizedSentences) :-
		tokenize_sentences(Text, TokenizedSentences, []).

	tokenize_sentences(Text, TokenizedSentences, UserOptions) :-
		prepare(Text, UserOptions, Codes, Options),
		_Language_::split_sentence_codes(Codes, SentenceCodes, Options),
		tokenize_sentence_codes(SentenceCodes, Options, TokenizedSentences).

	split_sentences(Text, Sentences) :-
		split_sentences(Text, Sentences, []).

	split_sentences(Text, Sentences, UserOptions) :-
		prepare(Text, UserOptions, Codes, Options),
		_Language_::split_sentence_codes(Codes, SentenceCodes, Options),
		codes_texts(SentenceCodes, Sentences).

	default_option(keep_punctuation(true)).
	default_option(lowercase(false)).
	default_option(normalize_quotes(true)).
	default_option(normalize_dashes(true)).

	valid_option(keep_punctuation(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(lowercase(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(normalize_quotes(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(normalize_dashes(Boolean)) :-
		valid(boolean, Boolean).

	prepare(Text, UserOptions, Codes, Options) :-
		check_representation,
		check_text(Text),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		text_codes(_Representation_, Text, Codes).

	check_representation :-
		(	var(_Representation_) ->
			instantiation_error
		;	member(_Representation_, [atom, chars, codes]) ->
			true
		;	domain_error(text_representation, _Representation_)
		).

	check_text(Text) :-
		check_text(_Representation_, Text).

	check_text(atom, Text) :-
		check(atom, Text).
	check_text(chars, Text) :-
		check(chars, Text).
	check_text(codes, Text) :-
		check(codes, Text).

	text_codes(atom, Atom, Codes) :-
		atom_codes(Atom, Codes).
	text_codes(chars, Chars, Codes) :-
		atom_chars(Atom, Chars),
		atom_codes(Atom, Codes).
	text_codes(codes, Codes, Codes).

	codes_texts([], []).
	codes_texts([Codes| CodesList], [Text| Texts]) :-
		codes_text(_Representation_, Codes, Text),
		codes_texts(CodesList, Texts).

	codes_text(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	codes_text(chars, Codes, Chars) :-
		atom_codes(Atom, Codes),
		atom_chars(Atom, Chars).
	codes_text(codes, Codes, Codes).

	tokenize_sentence_codes([], _, []).
	tokenize_sentence_codes([SentenceCodes| SentencesCodes], Options, [Tokens| TokenizedSentences]) :-
		_Language_::tokenize_codes(SentenceCodes, TokenCodes, Options),
		codes_texts(TokenCodes, Tokens),
		tokenize_sentence_codes(SentencesCodes, Options, TokenizedSentences).

:- end_object.
