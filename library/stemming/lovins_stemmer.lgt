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


:- object(lovins_stemmer(_Representation_),
	implements(stemmer_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'Lovins stemmer algorithm implementation for English words.',
		parameters is [
			'Representation' - 'Word representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		],
		remarks is [
			'Algorithm' - 'The Lovins stemming algorithm (Lovins, 1968) removes the longest suffix from a word using a list of 294 endings, each associated with a condition for removal. It then applies transformation rules to fix spelling.',
			'Reference' - 'Lovins, J.B. (1968). Development of a stemming algorithm. Mechanical Translation and Computational Linguistics, 11(1-2), 22-31.'
		],
		see_also is [stemmer_protocol, porter_stemmer(_)]
	]).

	:- uses(list, [
		append/3, last/2, length/2, member/2
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
		;	remove_ending(Word, Word1),
			apply_transformations(Word1, Stem)
		).

	% -----------------------------------------------------------------
	% Remove longest matching ending
	% -----------------------------------------------------------------

	remove_ending(Word, Stem) :-
		(	find_ending(Word, _Suffix, Prefix) ->
			Stem = Prefix
		;	Stem = Word
		).

	% Find the longest ending that matches AND satisfies its condition
	find_ending(Word, Suffix, Prefix) :-
		% Try endings from longest to shortest (order matters!)
		ending(Suffix, Condition),
		append(Prefix, Suffix, Word),
		length(Prefix, PrefixLength),
		PrefixLength >= 2,  % Stem must be at least 2 characters
		check_condition(Condition, Prefix),
		!.

	% -----------------------------------------------------------------
	% Ending rules (suffix -> condition)
	% Sorted by length (longest first) for proper matching
	% -----------------------------------------------------------------

	% 11-letter endings
	ending([a,l,i,s,t,i,c,a,l,l,y], b).
	ending([a,r,i,s,t,i,c,a,l,l,y], none).

	% 10-letter endings
	ending([a,l,l,i,c,a,l,l,y], c).
	ending([a,n,t,i,a,l,l,y], none).
	ending([a,t,i,o,n,a,l,l,y], none).

	% 9-letter endings
	ending([i,z,a,t,i,o,n,a,l], none).
	ending([i,f,i,c,a,t,i,o,n], none).
	ending([e,n,t,i,a,l,l,y], none).
	ending([f,u,l,n,e,s,s,e,s], none).

	% 8-letter endings
	ending([o,u,s,n,e,s,s,e,s], none).
	ending([i,v,e,n,e,s,s,e,s], none).
	ending([a,b,l,e,n,e,s,s,e,s], none).
	ending([a,n,t,n,e,s,s,e,s], none).
	ending([i,n,g,n,e,s,s,e,s], none).
	ending([i,o,n,a,l,i,z,e], none).
	ending([a,t,o,r,i,a,l,l], none).
	ending([a,t,i,n,g,l,y], none).

	% 7-letter endings
	ending([a,t,i,o,n,a,l], b).
	ending([i,z,a,t,i,o,n], none).
	ending([i,n,g,n,e,s,s], none).
	ending([a,n,t,n,e,s,s], none).
	ending([e,n,t,n,e,s,s], none).
	ending([f,u,l,n,e,s,s], none).
	ending([i,v,e,n,e,s,s], none).
	ending([o,u,s,n,e,s,s], none).
	ending([a,b,i,l,i,t,y], none).
	ending([i,b,i,l,i,t,y], none).
	ending([i,f,i,a,b,l,e], none).
	ending([i,s,a,t,i,o,n], none).
	ending([a,r,i,z,i,n,g], none).
	ending([e,r,i,z,i,n,g], none).
	ending([i,o,n,a,l,i,s], none).
	ending([i,o,n,a,l,l,y], none).
	ending([e,n,t,i,a,l,s], none).
	ending([i,o,n,a,l,i,t], none).
	ending([e,n,t,i,o,n,s], none).

	% 6-letter endings
	ending([a,l,i,s,e,d], none).
	ending([a,l,i,s,e,r], none).
	ending([a,l,i,s,e,s], none).
	ending([a,t,i,s,e,d], none).
	ending([a,t,i,v,e,s], none).
	ending([a,t,i,z,e,d], none).
	ending([a,t,i,z,e,r], none).
	ending([a,t,i,z,e,s], none).
	ending([e,d,n,e,s,s], none).
	ending([e,n,c,i,e,s], none).
	ending([e,n,t,i,a,l], none).
	ending([e,n,t,i,e,s], none).
	ending([f,u,l,l,y], none).
	ending([f,u,l,n,e,s], none).
	ending([i,a,l,i,s,t], none).
	ending([i,a,l,i,t,y], none).
	ending([i,a,l,i,z,e], none).
	ending([i,a,l,l,y], none).
	ending([i,b,l,e,l,y], none).
	ending([i,c,a,l,l,y], none).
	ending([i,n,g,l,y], none).
	ending([i,o,n,a,l,l], none).
	ending([i,o,u,s,l,y], none).
	ending([i,s,h,e,s], none).
	ending([i,s,h,l,y], none).
	ending([i,v,e,l,y], none).
	ending([i,v,i,t,y], none).
	ending([l,e,s,s,l,y], none).
	ending([m,e,n,t,a,l], none).
	ending([n,e,s,s,e,s], none).
	ending([o,n,a,l,i,s], none).
	ending([o,u,s,n,e,s], none).

	% 5-letter endings
	ending([a,b,l,e,s], none).
	ending([a,l,i,s,m], b).
	ending([a,l,i,s,t], none).
	ending([a,l,i,t,y], none).
	ending([a,l,i,z,e], none).
	ending([a,n,c,e,s], b).
	ending([a,n,c,i,a], none).
	ending([a,n,c,i,e], none).
	ending([a,r,i,a,n], none).
	ending([a,r,i,a,l], none).
	ending([a,r,i,e,s], none).
	ending([a,r,i,l,y], none).
	ending([a,r,i,z,e], none).
	ending([a,t,e,l,y], none).
	ending([a,t,i,n,g], none).
	ending([a,t,i,o,n], b).
	ending([a,t,i,v,e], none).
	ending([a,t,o,r,s], none).
	ending([a,t,o,r,y], none).
	ending([e,n,c,e,s], none).
	ending([e,n,c,i,a], none).
	ending([e,n,c,i,e], none).
	ending([e,n,c,i,s], none).
	ending([e,n,t,a,l], none).
	ending([e,n,t,e,d], none).
	ending([e,n,t,e,r], none).
	ending([e,n,t,e,s], none).
	ending([e,n,t,i,a], none).
	ending([e,n,t,l,y], none).
	ending([e,r,i,e,s], none).
	ending([e,r,i,n,g], none).
	ending([e,r,i,z,e], none).
	ending([i,b,l,e,s], none).
	ending([i,c,a,l,s], none).
	ending([i,c,a,l,l], none).
	ending([i,c,i,a,n], none).
	ending([i,c,i,s,m], none).
	ending([i,c,i,s,t], none).
	ending([i,c,i,t,y], none).
	ending([i,c,i,z,e], none).
	ending([i,c,s,l,y], none).
	ending([i,f,i,e,d], none).
	ending([i,f,i,e,r], none).
	ending([i,f,i,e,s], none).
	ending([i,l,i,t,y], none).
	ending([i,n,g,l,y], none).
	ending([i,o,u,s,e], none).
	ending([i,s,h,e,d], none).
	ending([i,s,h,e,r], none).
	ending([i,s,h,e,s], none).
	ending([i,t,i,e,s], none).
	ending([i,t,i,n,g], none).
	ending([i,t,o,r,s], none).
	ending([i,t,o,r,y], none).
	ending([i,v,e,l,y], none).
	ending([i,v,i,s,m], none).
	ending([i,v,i,s,t], none).
	ending([i,v,i,t,y], none).
	ending([i,z,e,r,s], none).
	ending([l,e,s,s,l], none).
	ending([m,e,n,t,s], none).
	ending([n,e,s,s,e], none).
	ending([i,o,n,a,l], none).
	ending([o,l,o,g,y], none).
	ending([o,s,i,t,y], none).
	ending([o,u,s,l,y], none).
	ending([t,i,o,n,s], none).

	% 4-letter endings
	ending([a,b,l,e], none).
	ending([a,b,l,y], none).
	ending([a,g,e,s], b).
	ending([a,l,i,a], none).
	ending([a,l,i,c], none).
	ending([a,l,l,y], b).
	ending([a,n,c,e], b).
	ending([a,n,c,y], b).
	ending([a,n,t,s], b).
	ending([a,r,i,c], none).
	ending([a,r,i,s], none).
	ending([a,r,y], none).
	ending([a,t,a], none).
	ending([a,t,e,d], none).
	ending([a,t,e,r], none).
	ending([a,t,e,s], none).
	ending([a,t,i,c], b).
	ending([a,t,o,r], none).
	ending([e,a,l,s], none).
	ending([e,d,l,y], none).
	ending([e,n,c,e], none).
	ending([e,n,c,y], none).
	ending([e,n,c,s], none).
	ending([e,n,d,s], none).
	ending([e,n,t,s], none).
	ending([e,s,t,s], none).
	ending([f,u,l,l], none).
	ending([i,a,l,s], none).
	ending([i,a,l,l], none).
	ending([i,a,n,s], none).
	ending([i,b,l,e], none).
	ending([i,b,l,y], none).
	ending([i,c,a,l], none).
	ending([i,c,e,s], none).
	ending([i,c,s], none).
	ending([i,d,e,s], none).
	ending([i,e,r,s], none).
	ending([i,e,s,t], none).
	ending([i,f,u,l], none).
	ending([i,n,a,l], none).
	ending([i,n,e,s], none).
	ending([i,n,g,s], none).
	ending([i,o,n,s], none).
	ending([i,o,u,s], none).
	ending([i,s,e,d], none).
	ending([i,s,e,r], none).
	ending([i,s,e,s], none).
	ending([i,s,h], none).
	ending([i,s,m,s], b).
	ending([i,s,t,s], none).
	ending([i,t,a,l], none).
	ending([i,t,e,d], none).
	ending([i,t,e,r], none).
	ending([i,t,e,s], none).
	ending([i,t,i,c], none).
	ending([i,v,e,s], none).
	ending([i,z,e,d], none).
	ending([i,z,e,r], none).
	ending([i,z,e,s], none).
	ending([l,e,s,s], none).
	ending([l,i,e,r], none).
	ending([l,i,e,s], none).
	ending([l,i,n,g], none).
	ending([m,e,n,t], none).
	ending([n,e,s,s], none).
	ending([o,i,d,s], none).
	ending([o,u,s,e], none).
	ending([o,u,s,s], none).
	ending([t,i,c,s], none).
	ending([t,o,r,s], none).
	ending([t,o,r,y], none).
	ending([u,o,u,s], none).
	ending([u,r,e,s], none).
	ending([u,r,e,d], none).
	ending([w,a,r,d], none).
	ending([w,i,s,e], none).

	% 3-letter endings
	ending([a,b,l], none).
	ending([a,g,e], b).
	ending([a,i,c], none).
	ending([a,l,s], b).
	ending([a,n,t], b).
	ending([a,r,s], none).
	ending([a,r,y], none).
	ending([a,t,a], none).
	ending([a,t,e], none).
	ending([e,a,l], none).
	ending([e,a,r], none).
	ending([e,e,s], none).
	ending([e,l,y], none).
	ending([e,n,s], none).
	ending([e,n,t], none).
	ending([e,r,s], none).
	ending([e,r,y], none).
	ending([e,s,t], none).
	ending([f,u,l], none).
	ending([i,a,l], none).
	ending([i,a,n], none).
	ending([i,b,l], none).
	ending([i,c,a], none).
	ending([i,c,e], none).
	ending([i,c,s], none).
	ending([i,d,e], none).
	ending([i,e,d], none).
	ending([i,e,r], none).
	ending([i,e,s], none).
	ending([i,f,y], none).
	ending([i,l,e], none).
	ending([i,l,y], none).
	ending([i,n,e], none).
	ending([i,n,g], none).
	ending([i,o,n], q).
	ending([i,s,e], none).
	ending([i,s,h], none).
	ending([i,s,m], b).
	ending([i,s,t], none).
	ending([i,t,e], none).
	ending([i,t,y], none).
	ending([i,u,m], none).
	ending([i,v,e], none).
	ending([i,z,e], none).
	ending([o,i,d], none).
	ending([o,n,e], none).
	ending([o,r,s], none).
	ending([o,r,y], none).
	ending([o,s,e], none).
	ending([o,u,r], none).
	ending([o,u,s], none).
	ending([t,h,s], none).
	ending([t,r,y], none).
	ending([u,a,l], none).
	ending([u,r,e], none).
	ending([u,s,e], none).

	% 2-letter endings
	ending([a,l], b).
	ending([a,n], none).
	ending([a,r], none).
	ending([a,s], b).
	ending([e,d], e).
	ending([e,n], none).
	ending([e,r], none).
	ending([e,s], none).
	ending([e,y], none).
	ending([i,a], none).
	ending([i,c], none).
	ending([i,s], none).
	ending([l,y], b).
	ending([o,n], s).
	ending([o,r], t).
	ending([o,w], none).
	ending([u,m], none).
	ending([u,s], none).

	% 1-letter endings
	ending([a], none).
	ending([e], none).
	ending([i], none).
	ending([o], none).
	ending([s], none).
	ending([y], none).

	% -----------------------------------------------------------------
	% Condition checking
	% -----------------------------------------------------------------

	check_condition(none, _).
	check_condition(b, Stem) :-
		% Minimum stem length of 3
		length(Stem, Length),
		Length >= 3.
	check_condition(c, Stem) :-
		% Minimum stem length of 4
		length(Stem, Lenght),
		Lenght >= 4.
	check_condition(e, Stem) :-
		% Do not remove ending after e
		length(Stem, Lenght),
		Lenght >= 2,
		last(Stem, Last),
		Last \== e.
	check_condition(q, Stem) :-
		% Minimum stem length = 3 and do not remove ending after l or n
		length(Stem, Lenght),
		Lenght >= 3,
		last(Stem, Last),
		Last \== l,
		Last \== n.
	check_condition(s, Stem) :-
		% Remove ending only after dr or t, unless t follows t
		length(Stem, Length),
		Length >= 2,
		(   append(_, [d,r], Stem) ->
		    true
		;   last(Stem, t),
		    \+ append(_, [t,t], Stem)
		).
	check_condition(t, Stem) :-
		% Remove ending only after s or t, unless t follows o
		length(Stem, Lenght),
		Lenght >= 2,
		last(Stem, Last),
		(   Last == s ->
		    true
		;   Last == t,
		    \+ append(_, [o,t], Stem)
		).

	% -----------------------------------------------------------------
	% Transformation rules
	% -----------------------------------------------------------------

	apply_transformations(Word, Stem) :-
		% First apply undouble rule, then other transformations
		undouble(Word, Word1),
		apply_transformation_rules(Word1, Stem).

	% Rule 1: Remove one of double b, d, g, l, m, n, p, r, s, t
	undouble(Word, Stem) :-
		(   append(Prefix, [C, C], Word),
		    member(C, [b, d, g, l, m, n, p, r, s, t]) ->
		    append(Prefix, [C], Stem)
		;   Stem = Word
		).

	apply_transformation_rules(Word, Stem) :-
		(	transformation_rule(Word, Transformed) ->
			apply_transformation_rules(Transformed, Stem)
		;	Stem = Word
		).

	% Spelling fix rules
	transformation_rule(Word, Stem) :-
		% iev -> ief
		append(Prefix, [i,e,v], Word),
		append(Prefix, [i,e,f], Stem).
	transformation_rule(Word, Stem) :-
		% uct -> uc
		append(Prefix, [u,c,t], Word),
		append(Prefix, [u,c], Stem).
	transformation_rule(Word, Stem) :-
		% umpt -> um
		append(Prefix, [u,m,p,t], Word),
		append(Prefix, [u,m], Stem).
	transformation_rule(Word, Stem) :-
		% rpt -> rb
		append(Prefix, [r,p,t], Word),
		append(Prefix, [r,b], Stem).
	transformation_rule(Word, Stem) :-
		% urs -> ur
		append(Prefix, [u,r,s], Word),
		append(Prefix, [u,r], Stem).
	transformation_rule(Word, Stem) :-
		% istr -> ister
		append(Prefix, [i,s,t,r], Word),
		append(Prefix, [i,s,t,e,r], Stem).
	transformation_rule(Word, Stem) :-
		% metr -> meter
		append(Prefix, [m,e,t,r], Word),
		append(Prefix, [m,e,t,e,r], Stem).
	transformation_rule(Word, Stem) :-
		% olv -> olut
		append(Prefix, [o,l,v], Word),
		append(Prefix, [o,l,u,t], Stem).
	transformation_rule(Word, Stem) :-
		% ul -> l when preceded by some letters
		append(Prefix, [u,l], Word),
		Prefix = [_|_],
		append(Prefix, [l], Stem).
	transformation_rule(Word, Stem) :-
		% bex -> bic
		append(Prefix, [b,e,x], Word),
		append(Prefix, [b,i,c], Stem).
	transformation_rule(Word, Stem) :-
		% dex -> dic
		append(Prefix, [d,e,x], Word),
		append(Prefix, [d,i,c], Stem).
	transformation_rule(Word, Stem) :-
		% pex -> pic
		append(Prefix, [p,e,x], Word),
		append(Prefix, [p,i,c], Stem).
	transformation_rule(Word, Stem) :-
		% tex -> tic
		append(Prefix, [t,e,x], Word),
		append(Prefix, [t,i,c], Stem).
	transformation_rule(Word, Stem) :-
		% ax -> ac
		append(Prefix, [a,x], Word),
		append(Prefix, [a,c], Stem).
	transformation_rule(Word, Stem) :-
		% ex -> ec
		append(Prefix, [e,x], Word),
		append(Prefix, [e,c], Stem).
	transformation_rule(Word, Stem) :-
		% ix -> ic
		append(Prefix, [i,x], Word),
		append(Prefix, [i,c], Stem).
	transformation_rule(Word, Stem) :-
		% lux -> luc
		append(Prefix, [l,u,x], Word),
		append(Prefix, [l,u,c], Stem).
	transformation_rule(Word, Stem) :-
		% uad -> uas
		append(Prefix, [u,a,d], Word),
		append(Prefix, [u,a,s], Stem).
	transformation_rule(Word, Stem) :-
		% vad -> vas
		append(Prefix, [v,a,d], Word),
		append(Prefix, [v,a,s], Stem).
	transformation_rule(Word, Stem) :-
		% cid -> cis
		append(Prefix, [c,i,d], Word),
		append(Prefix, [c,i,s], Stem).
	transformation_rule(Word, Stem) :-
		% lid -> lis
		append(Prefix, [l,i,d], Word),
		append(Prefix, [l,i,s], Stem).
	transformation_rule(Word, Stem) :-
		% erid -> eris
		append(Prefix, [e,r,i,d], Word),
		append(Prefix, [e,r,i,s], Stem).
	transformation_rule(Word, Stem) :-
		% pand -> pans
		append(Prefix, [p,a,n,d], Word),
		append(Prefix, [p,a,n,s], Stem).
	transformation_rule(Word, Stem) :-
		% end -> ens when not "s" before
		append(Prefix, [e,n,d], Word),
		Prefix \== [],
		last(Prefix, C),
		C \== s,
		append(Prefix, [e,n,s], Stem).
	transformation_rule(Word, Stem) :-
		% ond -> ons
		append(Prefix, [o,n,d], Word),
		append(Prefix, [o,n,s], Stem).
	transformation_rule(Word, Stem) :-
		% lud -> lus
		append(Prefix, [l,u,d], Word),
		append(Prefix, [l,u,s], Stem).
	transformation_rule(Word, Stem) :-
		% rud -> rus
		append(Prefix, [r,u,d], Word),
		append(Prefix, [r,u,s], Stem).
	transformation_rule(Word, Stem) :-
		% her -> hes when at end
		append(Prefix, [h,e,r], Word),
		append(Prefix, [h,e,s], Stem).
	transformation_rule(Word, Stem) :-
		% mit -> mis
		append(Prefix, [m,i,t], Word),
		append(Prefix, [m,i,s], Stem).
	transformation_rule(Word, Stem) :-
		% ent -> ens when after m
		append(Prefix, [m,e,n,t], Word),
		append(Prefix, [m,e,n,s], Stem).
	transformation_rule(Word, Stem) :-
		% ert -> ers
		append(Prefix, [e,r,t], Word),
		append(Prefix, [e,r,s], Stem).
	transformation_rule(Word, Stem) :-
		% et -> es when not after n
		append(Prefix, [e,t], Word),
		Prefix \== [],
		last(Prefix, Char),
		Char \== n,
		append(Prefix, [e,s], Stem).
	transformation_rule(Word, Stem) :-
		% yt -> ys
		append(Prefix, [y,t], Word),
		append(Prefix, [y,s], Stem).
	transformation_rule(Word, Stem) :-
		% yz -> ys
		append(Prefix, [y,z], Word),
		append(Prefix, [y,s], Stem).

:- end_object.
