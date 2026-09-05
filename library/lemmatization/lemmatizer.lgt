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


:- object(lemmatizer(_Representation_, _Language_),
	implements(lemmatizer_protocol),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Lemmatizer parameterized by text representation and language provider.',
		parameters is [
			'Representation' - 'Word representation. Valid values are ``atom``, ``codes``, and ``chars``.',
			'Language' - 'Object implementing the ``lemmatizer_language_protocol`` protocol.'
		],
		see_also is [lemmatizer_protocol, lemmatizer_language_protocol]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(type, [
		check/2, valid/2
	]).

	lemma(Word, Lemma) :-
		lemma(Word, Lemma, []).

	lemma(Word, Lemma, Options) :-
		check_representation,
		check_text(Word),
		^^check_options(Options),
		check_scalar_options(Options),
		^^option(ambiguity(Ambiguity), Options, ambiguity(all)),
		^^option(unknown(Unknown), Options, unknown(normalize)),
		part_of_speech_option(Options, PartOfSpeech),
		normalize(Word, NormalizedWord),
		provider_lemmas(PartOfSpeech, NormalizedWord, ProviderLemmas),
		select_word_lemma(ProviderLemmas, Ambiguity, Unknown, Word, NormalizedWord, Lemma).

	lemmas(Words, Lemmas) :-
		lemmas(Words, Lemmas, []).

	lemmas(Words, Lemmas, Options) :-
		check_representation,
		check_words(Words),
		^^check_options(Options),
		check_list_options(Options),
		^^option(ambiguity(Ambiguity), Options, ambiguity(first)),
		^^option(unknown(Unknown), Options, unknown(normalize)),
		parts_of_speech_option(Options, Words, PartsOfSpeech),
		lemmas(Words, PartsOfSpeech, Ambiguity, Unknown, Lemmas).

	default_option(unknown(normalize)).

	valid_option(part_of_speech(PartOfSpeech)) :-
		atom(PartOfSpeech).
	valid_option(parts_of_speech(PartsOfSpeech)) :-
		valid(list(atom), PartsOfSpeech).
	valid_option(ambiguity(Ambiguity)) :-
		member(Ambiguity, [first, all]).
	valid_option(unknown(Unknown)) :-
		member(Unknown, [normalize, preserve, fail]).

	check_scalar_options(Options) :-
		(	member(parts_of_speech(PsOS), Options) ->
			domain_error(option, parts_of_speech(PsOS))
		;	true
		).

	check_list_options(Options) :-
		(	member(part_of_speech(POS), Options),
			member(parts_of_speech(PsOS), Options) ->
			consistency_error(mutually_exclusive_options, part_of_speech(POS), parts_of_speech(PsOS))
		;	true
		).

	part_of_speech_option(Options, PartOfSpeech) :-
		(	member(part_of_speech(PartOfSpeech0), Options) ->
			PartOfSpeech = some(PartOfSpeech0)
		;	PartOfSpeech = none
		).

	parts_of_speech_option(Options, Words, PartsOfSpeech) :-
		(	member(parts_of_speech(PartsOfSpeech0), Options) ->
			check_same_length(Words, PartsOfSpeech0, Words, PartsOfSpeech0),
			wrap_parts_of_speech(PartsOfSpeech0, PartsOfSpeech)
		;	member(part_of_speech(PartOfSpeech), Options) ->
			shared_parts_of_speech(Words, PartOfSpeech, PartsOfSpeech)
		;	no_parts_of_speech(Words, PartsOfSpeech)
		).

	check_same_length([], [], _, _) :-
		!.
	check_same_length([_| WordsTail], [_| PartsOfSpeechTail], Words, PartsOfSpeech0) :-
		!,
		check_same_length(WordsTail, PartsOfSpeechTail, Words, PartsOfSpeech0).
	check_same_length(_, _, Words, PartsOfSpeech0) :-
		consistency_error(same_length, Words, PartsOfSpeech0).

	wrap_parts_of_speech([], []).
	wrap_parts_of_speech([PartOfSpeech| PartsOfSpeech], [some(PartOfSpeech)| WrappedPartsOfSpeech]) :-
		wrap_parts_of_speech(PartsOfSpeech, WrappedPartsOfSpeech).

	shared_parts_of_speech([], _, []).
	shared_parts_of_speech([_| Words], PartOfSpeech, [some(PartOfSpeech)| PartsOfSpeech]) :-
		shared_parts_of_speech(Words, PartOfSpeech, PartsOfSpeech).

	no_parts_of_speech([], []).
	no_parts_of_speech([_| Words], [none| PartsOfSpeech]) :-
		no_parts_of_speech(Words, PartsOfSpeech).

	lemmas([], [], _, _, []).
	lemmas([Word| Words], [PartOfSpeech| PartsOfSpeech], Ambiguity, Unknown, [Lemma| Lemmas]) :-
		normalize(Word, NormalizedWord),
		provider_lemmas(PartOfSpeech, NormalizedWord, ProviderLemmas),
		select_word_lemma(ProviderLemmas, Ambiguity, Unknown, Word, NormalizedWord, Lemma),
		lemmas(Words, PartsOfSpeech, Ambiguity, Unknown, Lemmas).

	select_word_lemma([], _, preserve, Word, _, Word) :-
		!.
	select_word_lemma(ProviderLemmas, Ambiguity, Unknown, _, NormalizedWord, Lemma) :-
		select_lemma(ProviderLemmas, Ambiguity, Unknown, NormalizedWord, NormalizedLemma),
		atom_to_word(_Representation_, NormalizedLemma, Lemma).

	provider_lemmas(none, Word, Lemmas) :-
		findall(Lemma, _Language_::lemma(Word, Lemma), Lemmas).
	provider_lemmas(some(PartOfSpeech), Word, Lemmas) :-
		findall(Lemma, _Language_::lemma(Word, PartOfSpeech, Lemma), Lemmas).

	select_lemma([Lemma| _], first, _, _, Lemma) :-
		!.
	select_lemma([Lemma], all, _, _, Lemma) :-
		!.
	select_lemma(Lemmas, all, _, _, Lemma) :-
		member(Lemma, Lemmas).
	select_lemma([], _, normalize, Word, Word) :-
		!.

	check_representation :-
		(	var(_Representation_) ->
			instantiation_error
		;	member(_Representation_, [atom, chars, codes]) ->
			true
		;	domain_error(text_representation, _Representation_)
		).

	check_words(Words) :-
		check(list, Words),
		check_word_list(Words).

	check_word_list([]).
	check_word_list([Word| Words]) :-
		check_text(Word),
		check_word_list(Words).

	check_text(Text) :-
		check_text(_Representation_, Text).

	check_text(atom, Text) :-
		check(atom, Text).
	check_text(chars, Text) :-
		check(chars, Text).
	check_text(codes, Text) :-
		check(codes, Text).

	normalize(Word, Normalized) :-
		string(_Representation_)::string_lower(Word, LowercaseWord),
		word_to_atom(_Representation_, LowercaseWord, Normalized).

	atom_to_word(atom, Atom, Atom).
	atom_to_word(chars, Atom, Chars) :-
		atom_chars(Atom, Chars).
	atom_to_word(codes, Atom, Codes) :-
		atom_codes(Atom, Codes).

	word_to_atom(atom, Atom, Atom).
	word_to_atom(chars, Chars, Atom) :-
		atom_chars(Atom, Chars).
	word_to_atom(codes, Codes, Atom) :-
		atom_codes(Atom, Codes).

:- end_object.
