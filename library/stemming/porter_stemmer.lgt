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


:- object(porter_stemmer(_Representation_),
	implements(stemmer_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'Porter stemmer algorithm implementation for English words.',
		parameters is [
			'Representation' - 'Word representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		],
		remarks is [
			'Algorithm' - 'The Porter stemming algorithm (Porter, 1980) is a widely used algorithm for reducing English words to their root form by applying a series of rules that remove common suffixes.',
			'Reference' - 'Porter, M.F. (1980). An algorithm for suffix stripping. Program, 14(3), 130-137.'
		],
		see_also is [stemmer_protocol, lovins_stemmer(_)]
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	% stem/2
	stem(Word, Stem) :-
		word_to_chars(Word, Chars),
		stem_chars(Chars, StemChars),
		chars_to_word(StemChars, Stem).

	% stems/2
	stems([], []).
	stems([Word| Words], [Stem| Stems]) :-
		stem(Word, Stem),
		stems(Words, Stems).

	% -----------------------------------------------------------------
	% Word representation conversion
	% -----------------------------------------------------------------

	word_to_chars(Word, Chars) :-
		word_to_chars(_Representation_, Word, Chars).

	word_to_chars(atom, Word, Chars) :-
		atom_chars(Word, Chars).
	word_to_chars(chars, Chars, Chars).
	word_to_chars(codes, Codes, Chars) :-
		codes_to_chars(Codes, Chars).

	chars_to_word(Chars, Word) :-
		chars_to_word(_Representation_, Chars, Word).

	chars_to_word(atom, Chars, Word) :-
		atom_chars(Word, Chars).
	chars_to_word(chars, Chars, Chars).
	chars_to_word(codes, Chars, Codes) :-
		chars_to_codes(Chars, Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	% -----------------------------------------------------------------
	% Main stemming algorithm
	% -----------------------------------------------------------------

	stem_chars(Word, Stem) :-
		(	Word == [] ->
			Stem = []
		;	Word == [_] ->
			Stem = Word
		;	Word == [_,_] ->
			Stem = Word
		;	step_1a(Word, Word1),
			step_1b(Word1, Word2),
			step_1c(Word2, Word3),
			step_2(Word3, Word4),
			step_3(Word4, Word5),
			step_4(Word5, Word6),
			step_5a(Word6, Word7),
			step_5b(Word7, Stem)
		).

	% -----------------------------------------------------------------
	% Step 1a: Process plurals
	% -----------------------------------------------------------------

	step_1a(Word, Stem) :-
		(	% SSES -> SS
			ends_with(Word, [s,s,e,s], Prefix) ->
			append(Prefix, [s,s], Stem)
		;	% IES -> I
			ends_with(Word, [i,e,s], Prefix) ->
			append(Prefix, [i], Stem)
		;	% SS -> SS
			ends_with(Word, [s,s], _) ->
			Stem = Word
		;	% S -> (delete)
			ends_with(Word, [s], Prefix) ->
			Stem = Prefix
		;	Stem = Word
		).

	% -----------------------------------------------------------------
	% Step 1b: Process past participles and progressive forms
	% -----------------------------------------------------------------

	step_1b(Word, Stem) :-
		(   % (m>0) EED -> EE; if EED matches but m=0, do nothing (longest match rule)
			ends_with(Word, [e,e,d], Prefix) ->
			(   measure(Prefix, M), M > 0 ->
				append(Prefix, [e,e], Stem)
			;   Stem = Word  % EED matched but condition failed, don't try ED
			)
		;	% (*v*) ED -> (delete)
			ends_with(Word, [e,d], Prefix),
			contains_vowel(Prefix) ->
			step_1b_fixup(Prefix, Stem)
		;	% (*v*) ING -> (delete)
			ends_with(Word, [i,n,g], Prefix),
			contains_vowel(Prefix) ->
			step_1b_fixup(Prefix, Stem)
		;	Stem = Word
		).

	step_1b_fixup(Word, Stem) :-
		(	% AT -> ATE
			ends_with(Word, [a,t], Prefix) ->
			append(Prefix, [a,t,e], Stem)
		;	% BL -> BLE
			ends_with(Word, [b,l], Prefix) ->
			append(Prefix, [b,l,e], Stem)
		;	% IZ -> IZE
			ends_with(Word, [i,z], Prefix) ->
			append(Prefix, [i,z,e], Stem)
		;	% Double consonant (not L, S, Z) -> single consonant
			double_consonant(Word),
			ends_with(Word, [C], Prefix),
			\+ member(C, [l, s, z]) ->
			Stem = Prefix
		;	% (m=1 and *o) -> E
			measure(Word, 1),
			ends_cvc(Word) ->
			append(Word, [e], Stem)
		;	Stem = Word
		).

	% -----------------------------------------------------------------
	% Step 1c: Replace Y with I
	% -----------------------------------------------------------------

	step_1c(Word, Stem) :-
		(	% (*v*) Y -> I
			ends_with(Word, [y], Prefix),
			contains_vowel(Prefix) ->
			append(Prefix, [i], Stem)
		;	Stem = Word
		).

	% -----------------------------------------------------------------
	% Step 2: Remove suffixes
	% -----------------------------------------------------------------

	step_2(Word, Stem) :-
		(	step_2_rule(Word, Stem) ->
			true
		;	Stem = Word
		).

	step_2_rule(Word, Stem) :-
		(	ends_with(Word, [a,t,i,o,n,a,l], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,t,e], Stem)
		;	ends_with(Word, [t,i,o,n,a,l], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [t,i,o,n], Stem)
		;	ends_with(Word, [e,n,c,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [e,n,c,e], Stem)
		;	ends_with(Word, [a,n,c,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,n,c,e], Stem)
		;	ends_with(Word, [i,z,e,r], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,z,e], Stem)
		;	ends_with(Word, [a,b,l,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,b,l,e], Stem)
		;	ends_with(Word, [a,l,l,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,l], Stem)
		;	ends_with(Word, [e,n,t,l,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [e,n,t], Stem)
		;	ends_with(Word, [e,l,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [e], Stem)
		;	ends_with(Word, [o,u,s,l,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [o,u,s], Stem)
		;	ends_with(Word, [i,z,a,t,i,o,n], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,z,e], Stem)
		;	ends_with(Word, [a,t,i,o,n], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,t,e], Stem)
		;	ends_with(Word, [a,t,o,r], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,t,e], Stem)
		;	ends_with(Word, [a,l,i,s,m], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,l], Stem)
		;	ends_with(Word, [i,v,e,n,e,s,s], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,v,e], Stem)
		;	ends_with(Word, [f,u,l,n,e,s,s], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [f,u,l], Stem)
		;	ends_with(Word, [o,u,s,n,e,s,s], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [o,u,s], Stem)
		;	ends_with(Word, [a,l,i,t,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,l], Stem)
		;	ends_with(Word, [i,v,i,t,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,v,e], Stem)
		;	ends_with(Word, [b,i,l,i,t,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [b,l,e], Stem)
		;	ends_with(Word, [l,o,g,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [l,o,g], Stem)
		;	fail
		).

	% -----------------------------------------------------------------
	% Step 3: Remove suffixes
	% -----------------------------------------------------------------

	step_3(Word, Stem) :-
		(	step_3_rule(Word, Stem) ->
			true
		;	Stem = Word
		).

	step_3_rule(Word, Stem) :-
		(	ends_with(Word, [i,c,a,t,e], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,c], Stem)
		;	ends_with(Word, [a,t,i,v,e], Prefix), measure(Prefix, M), M > 0 ->
			Stem = Prefix
		;	ends_with(Word, [a,l,i,z,e], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [a,l], Stem)
		;	ends_with(Word, [i,c,i,t,i], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,c], Stem)
		;	ends_with(Word, [i,c,a,l], Prefix), measure(Prefix, M), M > 0 ->
			append(Prefix, [i,c], Stem)
		;	ends_with(Word, [f,u,l], Prefix), measure(Prefix, M), M > 0 ->
			Stem = Prefix
		;	ends_with(Word, [n,e,s,s], Prefix), measure(Prefix, M), M > 0 ->
			Stem = Prefix
		;	fail
		).

	% -----------------------------------------------------------------
	% Step 4: Remove suffixes
	% -----------------------------------------------------------------

	step_4(Word, Stem) :-
		(	step_4_rule(Word, Stem) ->
			true
		;	Stem = Word
		).

	step_4_rule(Word, Stem) :-
		(	ends_with(Word, [a,l], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [a,n,c,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [e,n,c,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [e,r], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,c], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [a,b,l,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,b,l,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [a,n,t], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [e,m,e,n,t], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [m,e,n,t], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	% ENT: Don't apply if longer suffix MENT/EMENT matches (longest match rule)
			ends_with(Word, [e,n,t], Prefix),
			\+ ends_with(Word, [m,e,n,t], _),
			measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,o,n], Prefix), measure(Prefix, M), M > 1,
			(ends_with(Prefix, [s], _) ; ends_with(Prefix, [t], _)) ->
			Stem = Prefix
		;	ends_with(Word, [o,u], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,s,m], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [a,t,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,t,i], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [o,u,s], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,v,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [i,z,e], Prefix), measure(Prefix, M), M > 1 ->
			Stem = Prefix
		;	fail
		).

	% -----------------------------------------------------------------
	% Step 5a: Remove final E
	% -----------------------------------------------------------------

	step_5a(Word, Stem) :-
		(	ends_with(Word, [e], Prefix),
			measure(Prefix, M),
			M > 1 ->
			Stem = Prefix
		;	ends_with(Word, [e], Prefix),
			measure(Prefix, 1),
			\+ ends_cvc(Prefix) ->
			Stem = Prefix
		;	Stem = Word
		).

	% -----------------------------------------------------------------
	% Step 5b: Remove double L
	% -----------------------------------------------------------------

	step_5b(Word, Stem) :-
		(	measure(Word, M), M > 1,
			double_consonant(Word),
			ends_with(Word, [l], Prefix) ->
			Stem = Prefix
		;	Stem = Word
		).

	% -----------------------------------------------------------------
	% Helper predicates
	% -----------------------------------------------------------------

	% Check if a character is a vowel (a, e, i, o, u, or y preceded by consonant)
	vowel(a).
	vowel(e).
	vowel(i).
	vowel(o).
	vowel(u).

	% Check if a character at a given position is a vowel
	is_vowel_at(Chars, Index) :-
		nth1(Index, Chars, Char),
		(	vowel(Char) ->
			true
		;	Char == y,
			Index > 1,
			Index1 is Index - 1,
			nth1(Index1, Chars, Prev),
			\+ vowel(Prev)
		).

	is_consonant_at(Chars, Index) :-
		\+ is_vowel_at(Chars, Index).

	% nth1/3 - get nth element (1-indexed)
	nth1(1, [H|_], H) :- !.
	nth1(N, [_|T], E) :-
		N > 1,
		N1 is N - 1,
		nth1(N1, T, E).

	% Check if word contains a vowel
	contains_vowel(Word) :-
		length(Word, Length),
		between(1, Length, I),
		is_vowel_at(Word, I),
		!.

	% Calculate the measure (m) of a word
	% The measure is the number of VC (vowel-consonant) sequences
	measure(Word, Measure) :-
		word_to_vc_sequence(Word, Sequence),
		count_vc(Sequence, Measure).

	word_to_vc_sequence(Word, Sequence) :-
		length(Word, Length),
		(	Length > 0 ->
			findall(Type, (
				between(1, Length, I),
				(is_vowel_at(Word, I) -> Type = v ; Type = c)
			), Sequence)
		;	Sequence = []
		).

	count_vc(Sequence, Measure) :-
		compress_vc(Sequence, Compressed),
		count_vc_pairs(Compressed, 0, Measure).

	compress_vc([], []).
	compress_vc([H], [H]) :-
		!.
	compress_vc([H,H|T], Compressed) :-
		!,
		compress_vc([H|T], Compressed).
	compress_vc([H1,H2|T], [H1|Compressed]) :-
		H1 \= H2,
		compress_vc([H2|T], Compressed).

	count_vc_pairs([], Measure, Measure).
	count_vc_pairs([c], Measure, Measure) :-
		!.
	count_vc_pairs([v], Measure, Measure) :-
		!.
	count_vc_pairs([c,v|T], Measure0, Measure) :-
		!,
		count_vc_pairs([v|T], Measure0, Measure).
	count_vc_pairs([v,c|T], Measure0, Measure) :-
		!,
		Measure1 is Measure0 + 1,
		count_vc_pairs([c|T], Measure1, Measure).
	count_vc_pairs([v,v|T], Measure0, Measure) :-
		!,
		count_vc_pairs([v|T], Measure0, Measure).
	count_vc_pairs([c,c|T], Measure0, Measure) :-
		count_vc_pairs([c|T], Measure0, Measure).

	% Check if word ends with a double consonant
	double_consonant(Word) :-
		length(Word, Length),
		Length >= 2,
		Pos1 is Length,
		Pos2 is Length - 1,
		nth1(Pos1, Word, Char),
		nth1(Pos2, Word, Char),
		is_consonant_at(Word, Pos1).

	% Check if word ends with CVC pattern where second C is not W, X, or Y
	ends_cvc(Word) :-
		length(Word, Length),
		Length >= 3,
		Pos1 is Length,
		Pos2 is Length - 1,
		Pos3 is Length - 2,
		is_consonant_at(Word, Pos1),
		is_vowel_at(Word, Pos2),
		is_consonant_at(Word, Pos3),
		nth1(Pos1, Word, Char),
		\+ member(Char, [w, x, y]).

	% Check if Word ends with Suffix and return Prefix
	ends_with(Word, Suffix, Prefix) :-
		append(Prefix, Suffix, Word).

:- end_object.
