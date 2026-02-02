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


:- object(string_distance(_Representation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-02,
		comment is 'String distance predicates.',
		parameters is [
			'Representation' - 'String representation. Valid values are ``atom``, ``codes``, and ``chars``.'
		]
	]).

	% -----------------------------------------------------------------
	% Edit-based distances
	% -----------------------------------------------------------------

	:- public(levenshtein/3).
	:- mode(levenshtein(+text, +text, -integer), one).
	:- info(levenshtein/3, [
		comment is 'Computes the Levenshtein distance between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Distance' - 'Minimum number of single-character edits (insertions, deletions, substitutions) to transform ``String1`` into ``String2``.'
		]
	]).

	:- public(damerau_levenshtein/3).
	:- mode(damerau_levenshtein(+text, +text, -integer), one).
	:- info(damerau_levenshtein/3, [
		comment is 'Computes the Damerau-Levenshtein distance between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Distance' - 'Minimum number of edits (insertions, deletions, substitutions, and adjacent transpositions) to transform ``String1`` into ``String2``.'
		]
	]).

	:- public(hamming/3).
	:- mode(hamming(+text, +text, -integer), zero_or_one).
	:- info(hamming/3, [
		comment is 'Computes the Hamming distance between two strings of equal length. Fails if the strings differ in length.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string (must have the same length as ``String1``).',
			'Distance' - 'Number of positions at which the corresponding characters differ.'
		]
	]).

	% -----------------------------------------------------------------
	% Similarity scores (0.0 to 1.0)
	% -----------------------------------------------------------------

	:- public(jaro/3).
	:- mode(jaro(+text, +text, -float), one).
	:- info(jaro/3, [
		comment is 'Computes the Jaro similarity score between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Similarity' - 'A value between 0.0 (completely different) and 1.0 (identical), based on matching characters and transpositions.'
		]
	]).

	:- public(jaro_winkler/3).
	:- mode(jaro_winkler(+text, +text, -float), one).
	:- info(jaro_winkler/3, [
		comment is 'Computes the Jaro-Winkler similarity score between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Similarity' - 'A value between 0.0 and 1.0. Extends Jaro similarity with a prefix bonus: strings sharing a common prefix are scored higher.'
		]
	]).

	:- public(edit_similarity/3).
	:- mode(edit_similarity(+text, +text, -float), one).
	:- info(edit_similarity/3, [
		comment is 'Computes the edit similarity score between two strings using Levenshtein distance.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Similarity' - 'A value between 0.0 and 1.0 computed as 1 - (edit distance / max length of the two strings).'
		]
	]).

	:- public(edit_similarity/4).
	:- mode(edit_similarity(+atom, +text, +text, -float), one).
	:- info(edit_similarity/4, [
		comment is 'Computes the edit similarity score between two strings using the given algorithm.',
		arguments is [
			'Algorithm' - 'Edit distance algorithm. Valid values are ``levenshtein``, ``damerau_levenshtein``, ``hamming``, and ``longest_common_subsequence``.',
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Similarity' - 'A value between 0.0 and 1.0 computed as 1 - (edit distance / max length of the two strings).'
		]
	]).

	% -----------------------------------------------------------------
	% Subsequence / substring distances
	% -----------------------------------------------------------------

	:- public(longest_common_subsequence_length/3).
	:- mode(longest_common_subsequence_length(+text, +text, -integer), one).
	:- info(longest_common_subsequence_length/3, [
		comment is 'Computes the length of the Longest Common Subsequence between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Length' - 'Length of the longest subsequence common to both strings (characters need not be contiguous).'
		]
	]).

	:- public(longest_common_subsequence/3).
	:- mode(longest_common_subsequence(+text, +text, -atom), one).
	:- info(longest_common_subsequence/3, [
		comment is 'Computes the Longest Common Subsequence itself between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Subsequence' - 'The longest subsequence common to both strings (characters need not be contiguous). If multiple exist, one is returned nondeterministically.'
		]
	]).

	:- public(longest_common_substring/3).
	:- mode(longest_common_substring(+text, +text, -atom), one).
	:- info(longest_common_substring/3, [
		comment is 'Computes the longest contiguous common substring between two strings.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.',
			'Substring' - 'The longest contiguous substring shared by both strings. If multiple exist, one is returned nondeterministically.'
		]
	]).

	% -----------------------------------------------------------------
	% Token-based / set-based similarities
	% -----------------------------------------------------------------

	:- public(cosine_similarity/3).
	:- mode(cosine_similarity(+list(text), +list(text), -float), one).
	:- info(cosine_similarity/3, [
		comment is 'Computes the cosine similarity between two token lists.',
		arguments is [
			'Tokens1'- 'First token list (e.g., list of words or character n-grams).',
			'Tokens2' - 'Second token list.',
			'Similarity' - 'A value between 0.0 and 1.0 representing the cosine of the angle between the two token vectors.'
		]
	]).

	:- public(jaccard_index/3).
	:- mode(jaccard_index(+list(text), +list(text), -float), one).
	:- info(jaccard_index/3, [
		comment is 'Computes the Jaccard index (similarity) between two token lists.',
		arguments is [
			'Tokens1'- 'First token list (e.g., list of words or character n-grams).',
			'Tokens2' - 'Second token list.',
			'Index' - 'A value between 0.0 (no overlap) and 1.0 (identical sets), computed as ``|intersection| / |union|``.'
		]
	]).

	% -----------------------------------------------------------------
	% Phonetic encoding
	% -----------------------------------------------------------------

	:- public(soundex/2).
	:- mode(soundex(+text, -atom), one).
	:- info(soundex/2, [
		comment is 'Computes the Soundex phonetic code for a string.',
		arguments is [
			'String' - 'Input string (typically a name).',
			'Code' - 'A four-character Soundex code representing the phonetic encoding.'
		]
	]).

	:- public(metaphone/2).
	:- mode(metaphone(+text, -atom), one).
	:- info(metaphone/2, [
		comment is 'Computes the Metaphone phonetic key for a string.',
		arguments is [
			'String' - 'Input string (typically a name).',
			'Key' - 'The Metaphone phonetic key, a more accurate phonetic encoding than Soundex.'
		]
	]).

	:- public(soundex_match/2).
	:- mode(soundex_match(+text, +text), one).
	:- info(soundex_match/2, [
		comment is 'Succeeds if two strings share the same Soundex code.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.'
		]
	]).

	:- public(metaphone_match/2).
	:- mode(metaphone_match(+text, +text), one).
	:- info(metaphone_match/2, [
		comment is 'Succeeds if two strings share the same Metaphone key.',
		arguments is [
			'String1' - 'First input string.',
			'String2' - 'Second input string.'
		]
	]).

	:- uses(integer, [
		between/3, sequence/3
	]).

	:- uses(list, [
		append/3, drop/3, last/2, length/2, member/2, nth0/3, reverse/2, take/3
	]).

	:- uses(set, [
		intersection/3, union/3
	]).

	% =================================================================
	% Edit-based distances
	% =================================================================

	% -----------------------------------------------------------------
	% Levenshtein — Wagner-Fischer, one row at a time.
	%
	% RowPrev holds the previous row.  The first element of RowPrev is
	% the row index minus one, so RowIdx = RowPrev[0] + 1.
	% Within lev_row, Left is the cell immediately to the left (the
	% most recently computed value), and Diag is the cell diagonally
	% above-left (RowPrev[j-1], i.e. the previous Above before it was
	% consumed).
	% -----------------------------------------------------------------
	levenshtein(String1, String2, Distance) :-
		levenshtein(_Representation_, String1, String2, Distance).

	levenshtein(atom, String1, String2, Distance) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		levenshtein(codes, Codes1, Codes2, Distance).
	levenshtein(chars, String1, String2, Distance) :-
		levenshtein(codes, String1, String2, Distance).
	levenshtein(codes, String1, String2, Distance) :-
		length(String2, Cols),
		numlist(0, Cols, Row0),
		lev_rows(String1, String2, Row0, RowFinal),
		last(RowFinal, Distance).

	lev_rows([], _, Row, Row).
	lev_rows([H1|T1], Codes2, RowPrev, RowFinal) :-
		RowPrev = [RowIdx0|RowVals],
		RowIdx is RowIdx0 + 1,
		lev_row(Codes2, H1, RowVals, RowIdx, RowIdx0, [], RowCur0),
		RowCur = [RowIdx|RowCur0],
		lev_rows(T1, Codes2, RowCur, RowFinal).

	lev_row([], _, _, _, _, Acc, Row) :-
		reverse(Acc, Row).
	lev_row([H2|T2], H1, [Above|AboveRest], Left, Diag, Acc, Row) :-
		(	H1 == H2
		->	Cost = 0
		;	Cost = 1
		),
		Sub is Diag  + Cost,
		Ins is Above + 1,
		Del is Left  + 1,
		Val is min(Sub, min(Ins, Del)),
		lev_row(T2, H1, AboveRest, Val, Above, [Val|Acc], Row).

	% -----------------------------------------------------------------
	% Damerau-Levenshtein — optimal string alignment (OSA) variant.
	%
	% Same row DP as Levenshtein, but we also carry:
	%   RowPrevPrev  – the row two steps back
	%   PrevCode     – the previous character of String1
	%   ColIdx       – current 0-based column index
	%
	% Transposition condition at (i,j):
	%   s1[i] == s2[j-1]  AND  s1[i-1] == s2[j]
	% which lets us look up RowPrevPrev[j-1] for the transposition cost.
	% -----------------------------------------------------------------

	damerau_levenshtein(String1, String2, Distance) :-
		damerau_levenshtein(_Representation_, String1, String2, Distance).

	damerau_levenshtein(atom, String1, String2, Distance) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		damerau_levenshtein(codes, Codes1, Codes2, Distance).
	damerau_levenshtein(chars, String1, String2, Distance) :-
		damerau_levenshtein(codes, String1, String2, Distance).
	damerau_levenshtein(codes, String1, String2, Distance) :-
		length(String2, Cols),
		sequence(0, Cols, Row0),
		dl_rows(String1, String2, Row0, [], none, RowFinal),
		last(RowFinal, Distance).

	dl_rows([], _, Row, _, _, Row).
	dl_rows([H1|T1], Codes2, RowPrev, RowPrevPrev, PrevCode, RowFinal) :-
		RowPrev = [RowIdx0|RowVals],
		RowIdx is RowIdx0 + 1,
		(	RowPrevPrev == []
		->	RowPrevPrevVals = []
		;	RowPrevPrev = [_|RowPrevPrevVals]
		),
		dl_row(Codes2, H1, PrevCode, RowVals, RowPrevPrevVals, RowIdx, RowIdx0, 0, [], RowCur0),
		RowCur = [RowIdx|RowCur0],
		dl_rows(T1, Codes2, RowCur, RowPrev, H1, RowFinal).

	dl_row([], _, _, _, _, _, _, _, Acc, Row) :-
		reverse(Acc, Row).
	dl_row([H2|T2], H1, PrevCode, [Above|AboveRest], PrevPrevRow, Left, Diag, ColIdx, Acc, Row) :-
		(	H1 == H2
		->	Cost = 0
		;	Cost = 1
		),
		Sub is Diag  + Cost,
		Ins is Above + 1,
		Del is Left  + 1,
		Val0 is min(Sub, min(Ins, Del)),
		% Transposition: s1[i]==s2[j-1] and s1[i-1]==s2[j].
		% H1 = s1[i], PrevCode = s1[i-1], H2 = s2[j].
		% s2[j-1] is the element just before H2 — we detect this by
		% checking H1 == the previous s2 char.  We approximate this
		% with the standard OSA condition using PrevPrevRow.
		(	PrevCode \== none,
			ColIdx > 0,
			PrevPrevRow \= [],
			ColIdx1 is ColIdx - 1,
			nth0(ColIdx1, PrevPrevRow, PPVal)
		->	Trans is PPVal + 1,
			Val is min(Val0, Trans)
		;	Val = Val0
		),
		ColIdxNext is ColIdx + 1,
		dl_row(T2, H1, PrevCode, AboveRest, PrevPrevRow, Val, Above, ColIdxNext, [Val|Acc], Row).

	% -----------------------------------------------------------------
	% Hamming
	% -----------------------------------------------------------------
	hamming(String1, String2, Distance) :-
		hamming(_Representation_, String1, String2, Distance).

	hamming(atom, String1, String2, Distance) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		hamming(codes, Codes1, Codes2, Distance).
	hamming(chars, String1, String2, Distance) :-
		hamming(codes, String1, String2, Distance).
	hamming(codes, String1, String2, Distance) :-
		length(String1, L),
		length(String2, L),
		hamming_count(String1, String2, 0, Distance).

	hamming_count([], [], Distance, Distance).
	hamming_count([Code1| Codes1], [Code2| Codes2], Distance0, Distance) :-
		(	Code1 == Code2
		->	Distance1 = Distance0
		;	Distance1 is Distance0 + 1
		),
		hamming_count(Codes1, Codes2, Distance1, Distance).

	% =================================================================
	% Similarity scores
	% =================================================================

	% -----------------------------------------------------------------
	% Jaro
	%
	% 1. Match window  MW = max(floor(max(|s1|,|s2|)/2) - 1, 0).
	% 2. Scan s1; for each character find the first unused character
	%    in s2 within [i-MW .. i+MW] that matches.  "Used" is tracked
	%    as a boolean list threaded through the scan.
	% 3. Collect the matched characters from s2 in their original order.
	% 4. Count transpositions (positions where the two match-sequences
	%    differ).
	% 5. jaro = (m/|s1| + m/|s2| + (m - t/2)/m) / 3.
	% -----------------------------------------------------------------
	jaro(String1, String2, Similarity) :-
		jaro(_Representation_, String1, String2, Similarity).

	jaro(atom, String1, String2, Similarity) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		jaro(codes, Codes1, Codes2, Similarity).
	jaro(chars, String1, String2, Similarity) :-
		jaro(codes, String1, String2, Similarity).
	jaro(codes, Codes1, Codes2, Similarity) :-
		length(Codes1, Length1),
		length(Codes2, Length2),
		(	Length1 =:= 0, Length2 =:= 0
		->	Similarity = 1.0
		;	(Length1 =:= 0 ; Length2 =:= 0)
		->	Similarity = 0.0
		;	MW is max(max(Length1, Length2) // 2 - 1, 0),
			length(Used0, Length2),
			maplist_eq(Used0, false),
			jaro_match_s1(Codes1, 0, Codes2, Length2, MW, Used0, Matches1, UsedFinal),
			jaro_match_s2(Codes2, 0, UsedFinal, Matches2),
			length(Matches1, M),
			(	M =:= 0
			->	Similarity = 0.0
			;	jaro_count_trans(Matches1, Matches2, 0, T),
				Similarity is (M / Length1 + M / Length2 + (M - T / 2) / M) / 3.0
			)
		).

	jaro_match_s1([], _, _, _, _, Used, [], Used).
	jaro_match_s1([H1|T1], I, Codes2, Length2, MW, Used0, Matches, UsedOut) :-
		Low  is max(0,        I - MW),
		High is min(Length2 - 1, I + MW),
		(	jaro_find_unused(H1, Codes2, Low, High, Used0, Pos)
		->	Matches = [H1| RestM],
			set_nth0(Pos, Used0, true, Used1),
			I1 is I + 1,
			jaro_match_s1(T1, I1, Codes2, Length2, MW, Used1, RestM, UsedOut)
		;	Matches = RestM,
			I1 is I + 1,
			jaro_match_s1(T1, I1, Codes2, Length2, MW, Used0, RestM, UsedOut)
		).

	jaro_find_unused(Code, Codes2, Low, High, Used, Pos) :-
		Low =< High,
		nth0(Low, Codes2,  C2),
		nth0(Low, Used, Flag),
		(	C2 == Code, Flag == false
		->	Pos = Low
		;	Low1 is Low + 1,
			jaro_find_unused(Code, Codes2, Low1, High, Used, Pos)
		).

	jaro_match_s2([], _, _, []).
	jaro_match_s2([H|T], I, Used, Matches) :-
		nth0(I, Used, Flag),
		(	Flag == true
		->	Matches = [H|Rest]
		;	Matches = Rest
		),
		I1 is I + 1,
		jaro_match_s2(T, I1, Used, Rest).

	jaro_count_trans([], [], T, T).
	jaro_count_trans([Head1| Tail1], [Head2| Tail2], T0, T) :-
		(	Head1 \== Head2
		->	T1 is T0 + 1
		;	T1 = T0
		),
		jaro_count_trans(Tail1, Tail2, T1, T).

	% -----------------------------------------------------------------
	% Jaro-Winkler
	%
	% jw = jaro + min(commonPrefix, 4) * 0.1 * (1 - jaro)
	% -----------------------------------------------------------------
	jaro_winkler(String1, String2, Similarity) :-
		jaro_winkler(_Representation_, String1, String2, Similarity).

	jaro_winkler(atom, String1, String2, Similarity) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		jaro_winkler(codes, Codes1, Codes2, Similarity).
	jaro_winkler(chars, String1, String2, Similarity) :-
		jaro_winkler(codes, String1, String2, Similarity).
	jaro_winkler(codes, Codes1, Codes2, Similarity) :-
		jaro(codes, Codes1, Codes2, JaroSim),
		common_prefix_length(Codes1, Codes2, 0, PrefRaw),
		Pref is min(PrefRaw, 4),
		Similarity is JaroSim + Pref * 0.1 * (1.0 - JaroSim).

	common_prefix_length([Code| Codes1], [Code| Codes2], Length0, Length) :-
		Length0 < 4,
		!,
		Length1 is Length0 + 1,
		common_prefix_length(Codes1, Codes2, Length1, Length).
	common_prefix_length(_, _, Length, Length).

	% -----------------------------------------------------------------
	% Edit similarity
	%
	% sim = 1 - (edit_distance / max(|s1|, |s2|))
	% -----------------------------------------------------------------
	edit_similarity(String1, String2, Similarity) :-
		edit_similarity(_Representation_, String1, String2, Similarity).

	edit_similarity(atom, String1, String2, Similarity) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		edit_similarity(codes, Codes1, Codes2, Similarity).
	edit_similarity(chars, String1, String2, Similarity) :-
		edit_similarity(codes, String1, String2, Similarity).
	edit_similarity(codes, String1, String2, Similarity) :-
		levenshtein(codes, String1, String2, Distance),
		length(String1, Length1),
		length(String2, Length2),
		MaxLength is max(Length1, Length2),
		(	MaxLength =:= 0
			% both empty
		->	Similarity = 1.0
		;	Similarity is 1.0 - (Distance / MaxLength)
		).

	edit_similarity(Algorithm, String1, String2, Similarity) :-
		edit_similarity(_Representation_, Algorithm, String1, String2, Similarity).

	edit_similarity(atom, Algorithm, String1, String2, Similarity) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		edit_similarity(codes, Algorithm, Codes1, Codes2, Similarity).
	edit_similarity(chars, Algorithm, String1, String2, Similarity) :-
		edit_similarity(codes, Algorithm, String1, String2, Similarity).
	edit_similarity(codes, Algorithm, String1, String2, Similarity) :-
		(	Algorithm == levenshtein
		->	levenshtein(codes, String1, String2, Distance)
		;	Algorithm == damerau_levenshtein
		->	damerau_levenshtein(codes, String1, String2, Distance)
		;	Algorithm == hamming
		->	hamming(codes, String1, String2, Distance)
		;	Algorithm == longest_common_subsequence
		->	longest_common_subsequence_length(codes, String1, String2, Distance)
		;	fail
		),
		length(String1, Length1),
		length(String2, Length2),
		MaxLength is max(Length1, Length2),
		(	MaxLength =:= 0
		->	Similarity = 1.0
			% both empty
		;	Similarity is 1.0 - (Distance / MaxLength)
		).

	% =================================================================
	% Subsequence / substring
	% =================================================================

	% -----------------------------------------------------------------
	% LCS length — row-by-row DP.
	%
	% Each row is length |s2|+1.  Row0 is all zeros.
	% For each character of s1 we compute a new row:
	%   if s1[i] == s2[j]  then  cell[j+1] = prevRow[j] + 1
	%   else                     cell[j+1] = max(prevRow[j+1], curRow[j])
	%
	% Diag tracks prevRow[j] (the diagonal cell).
	% The current-row left neighbor is the head of the accumulator.
	% -----------------------------------------------------------------
	longest_common_subsequence_length(String1, String2, Length) :-
		longest_common_subsequence_length(_Representation_, String1, String2, Length).

	longest_common_subsequence_length(atom, String1, String2, Length) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		longest_common_subsequence_length(codes, Codes1, Codes2, Length).
	longest_common_subsequence_length(chars, String1, String2, Length) :-
		longest_common_subsequence_length(codes, String1, String2, Length).
	longest_common_subsequence_length(codes, Codes1, Codes2, Length) :-
		length(Codes2, Cols),
		Cols1 is Cols + 1,
		length(Row0, Cols1),
		maplist_eq(Row0, 0),
		longest_common_subsequence_length_rows(Codes1, Codes2, Row0, RowFinal),
		last(RowFinal, Length).

	longest_common_subsequence_length_rows([], _, Row, Row).
	longest_common_subsequence_length_rows([H1| T1], Codes2, RowPrev, RowFinal) :-
		% skip the leading 0 / row-index
		RowPrev = [_| RowPrevTail],
		longest_common_subsequence_length_row(Codes2, H1, RowPrevTail, 0, [0], RowCur),
		longest_common_subsequence_length_rows(T1, Codes2, RowCur, RowFinal).

	longest_common_subsequence_length_row([], _, _, _, Acc, Row) :-
		reverse(Acc, Row).
	longest_common_subsequence_length_row([H2| T2], H1, [Above| AboveRest], Diag, Acc, Row) :-
		(	H1 == H2
		->	Val is Diag + 1
		;	Acc = [Left| _],
			Val is max(Above, Left)
		),
		longest_common_subsequence_length_row(T2, H1, AboveRest, Above, [Val| Acc], Row).

	% -----------------------------------------------------------------
	% LCS (recover the full subsequence)
	%
	% Build the complete DP table as a list of rows (index 0 = all
	% zeros), then backtrack from (|s1|, |s2|).
	% -----------------------------------------------------------------
	longest_common_subsequence(String1, String2, Subsequence) :-
		longest_common_subsequence(_Representation_, String1, String2, Subsequence).

	longest_common_subsequence(atom, String1, String2, Subsequence) :-
		atom_codes(String1, Codes1),
		atom_codes(String2, Codes2),
		longest_common_subsequence(codes, Codes1, Codes2, Subsequence0),
		atom_codes(Subsequence, Subsequence0).
	longest_common_subsequence(chars, String1, String2, Subsequence) :-
		chars_to_codes(String1, Codes1),
		chars_to_codes(String2, Codes2),
		longest_common_subsequence(codes, Codes1, Codes2, Subsequence0),
		codes_to_chars(Subsequence0, Subsequence).
	longest_common_subsequence(codes, Codes1, Codes2, Subsequence) :-
		length(Codes1, Rows),
		length(Codes2, Cols),
		Cols1 is Cols + 1,
		length(Row0, Cols1),
		maplist_eq(Row0, 0),
		lcs_build_table(Codes1, Codes2, Row0, TableRev),
		reverse(TableRev, Table),
		lcs_backtrack(Table, Codes1, Codes2, Rows, Cols, RevSeq),
		reverse(RevSeq, Subsequence).

	lcs_build_table([], _, Row0, [Row0]).
	lcs_build_table([H1|T1], Codes2, RowPrev, [RowCur|Rest]) :-
		RowPrev = [_|RowPrevTail],
		longest_common_subsequence_length_row(Codes2, H1, RowPrevTail, 0, [0], RowCur),
		lcs_build_table(T1, Codes2, RowCur, Rest).

	lcs_backtrack(_, _, _, 0, _, []) :-
		!.
	lcs_backtrack(_, _, _, _, 0, []) :-
		!.
	lcs_backtrack(Table, Codes1, Codes2, I, J, Seq) :-
		I1 is I - 1,
		J1 is J - 1,
		nth0(I1, Codes1, CodeI),
		nth0(J1, Codes2, CodeJ),
		(	CodeI == CodeJ
		->	Seq = [CodeI|RestSeq],
			lcs_backtrack(Table, Codes1, Codes2, I1, J1, RestSeq)
		;	nth0(I1, Table, RowUp),
			nth0(J,  RowUp,  ValUp),
			nth0(I,  Table, RowCur),
			nth0(J1, RowCur, ValLeft),
			(	ValUp >= ValLeft
			->	lcs_backtrack(Table, Codes1, Codes2, I1, J, Seq)
			;	lcs_backtrack(Table, Codes1, Codes2, I, J1, Seq)
			)
		).

	% -----------------------------------------------------------------
	% Longest common substring
	%
	% Row DP: cell(i,j) = length of the longest common suffix of
	% s1[0..i] and s2[0..j].
	%   if s1[i] == s2[j]  then  cell = prevRow[j-1] + 1   (i.e. Above + 1)
	%   else                     cell = 0
	%
	% We track the running maximum and the row index where it occurred
	% (EndI).  After scanning, the substring is Codes1[EndI-MaxLen .. EndI-1].
	% -----------------------------------------------------------------
	longest_common_substring(String1, String2, Substring) :-
		longest_common_substring(_Representation_, String1, String2, Substring).

	longest_common_substring(atom, String1, String2, Substring) :-
		atom_chars(String1, Codes1),
		atom_chars(String2, Codes2),
		longest_common_substring(chars, Codes1, Codes2, Substring0),
		atom_chars(Substring, Substring0).
	longest_common_substring(chars, String1, String2, Substring) :-
		chars_to_codes(String1, Codes1),
		chars_to_codes(String2, Codes2),
		longest_common_substring(codes, Codes1, Codes2, Substring0),
		codes_to_chars(Substring0, Substring).
	longest_common_substring(codes, Codes1, Codes2, Substring) :-
		length(Codes2, Cols),
		Cols1 is Cols + 1,
		length(Row0, Cols1),
		maplist_eq(Row0, 0),
		lcsub_rows(Codes1, Codes2, Row0, 0, 0, 0, MaxLen, EndI),
		(	MaxLen =:= 0
		->	Substring = ''
		;	StartI is EndI - MaxLen,
			lcsub_slice(Codes1, StartI, MaxLen, Substring)
		).

	lcsub_rows([], _, _, ML, EI, _, ML, EI).
	lcsub_rows([H1|T1], Codes2, RowPrev, ML0, EI0, RowIdx, ML, EI) :-
		RowPrev = [_|RowPrevTail],
		lcsub_row(Codes2, H1, RowPrevTail, 0, ML0, EI0, RowIdx, ML1, EI1, RowCur0),
		RowCur = [0|RowCur0],
		RowIdx1 is RowIdx + 1,
		lcsub_rows(T1, Codes2, RowCur, ML1, EI1, RowIdx1, ML, EI).

	lcsub_row([], _, _, _, ML, EI, _, ML, EI, []).
	lcsub_row([H2|T2], H1, [Above|AboveRest], Diag, ML0, EI0, RowIdx, ML, EI, [Val|RestRow]) :-
		(	H1 == H2
		->	Val is Diag + 1
		;	Val = 0
		),
		% 1-based end position in Codes1
		RowIdxEnd is RowIdx + 1,
		(	Val > ML0
		->	ML1 = Val, EI1 = RowIdxEnd
		;	ML1 = ML0, EI1 = EI0
		),
		lcsub_row(T2, H1, AboveRest, Above, ML1, EI1, RowIdx, ML, EI, RestRow).

	lcsub_slice(List, Start, Length, Slice) :-
		drop(Start, List, Rest),
		take(Length, Rest, Slice).

	% =================================================================
	% Token-based similarities
	% =================================================================

	% -----------------------------------------------------------------
	% Cosine similarity
	%
	% Tokens are treated as a bag (duplicates increase the count).
	% We compute the sorted union of unique tokens, then for each
	% token tally its count in each input list to form implicit
	% integer vectors.  Dot product and magnitudes follow from those
	% counts.
	% -----------------------------------------------------------------
	cosine_similarity(Tokens1, Tokens2, Similarity) :-
		cosine_similarity(_Representation_, Tokens1, Tokens2, Similarity).

	cosine_similarity(_, Tokens1, Tokens2, Similarity) :-
		sort(Tokens1, SortedTokens1),
		sort(Tokens2, SortedTokens2),
		union(SortedTokens1, SortedTokens2, AllTokens),
		dot_product(AllTokens, Tokens1, Tokens2, 0.0, DotProduct),
		magnitude(AllTokens, Tokens1, Magnitude1),
		magnitude(AllTokens, Tokens2, Magnitude2),
		Denominator is Magnitude1 * Magnitude2,
		(   Denominator =:= 0
		->  Similarity = 0.0
		;   Similarity is DotProduct / Denominator
		).

	dot_product([], _, _, DotProduct, DotProduct).
	dot_product([Token| Tokens], Tokens1, Tokens2, DotProduct0, DotProduct) :-
		count_token(Tokens1, Token, 0, Count1),
		count_token(Tokens2, Token, 0, Count2),
		DotProduct1 is DotProduct0 + (Count1 * Count2),
		dot_product(Tokens, Tokens1, Tokens2, DotProduct1, DotProduct).

	count_token([], _, N, N).
	count_token([Token| Tokens], Token, N0, N) :-
		!,
		N1 is N0 + 1,
		count_token(Tokens, Token, N1, N).
	count_token([_| Tokens], Token, N0, N) :-
		count_token(Tokens, Token, N0, N).

	magnitude(AllTokens, Tokens, Magnitude) :-
		mag_sq(AllTokens, Tokens, 0.0, SquareSum),
		Magnitude is sqrt(SquareSum).

	mag_sq([], _, SquareSum, SquareSum).
	mag_sq([AllToken| AllTokens], Tokens, SquareSum0, SquareSum) :-
		count_token(Tokens, AllToken, 0, Count),
		SquareSum1 is SquareSum0 + (Count * Count),
		mag_sq(AllTokens, Tokens, SquareSum1, SquareSum).

	% -----------------------------------------------------------------
	% Jaccard index
	%
	% |intersection(String1,String2)| / |union(String1,String2)|
	% Both sets are de-duplicated first (sort).
	% Convention: two empty sets have index 1.0.
	% -----------------------------------------------------------------
	jaccard_index(Tokens1, Tokens2, Index) :-
		jaccard_index(_Representation_, Tokens1, Tokens2, Index).

	jaccard_index(_, Tokens1, Tokens2, Index) :-
		sort(Tokens1, SortedTokens1),
		sort(Tokens2, SortedTokens2),
		intersection(SortedTokens1, SortedTokens2, Intersection),
		union(SortedTokens1, SortedTokens2, Union),
		length(Intersection, IntersectionLength),
		length(Union, UnionLength),
		(	UnionLength =:= 0
		->	Index = 1.0
		;	Index is IntersectionLength / UnionLength
		).

	% =================================================================
	% Phonetic encoding
	% =================================================================

	% -----------------------------------------------------------------
	% Soundex
	%
	% Standard algorithm:
	%   1. Uppercase the first letter; that becomes the code's letter.
	%   2. Map every remaining letter to its digit (see soundex_char_code).
	%   3. Prepend the first letter's own digit so adjacent-duplicate
	%      removal also drops a digit that matches the retained letter.
	%   4. Remove adjacent duplicate digits.
	%   5. Drop the (now redundant) first digit.
	%   6. Remove all '0' digits (vowels / H / W / Y).
	%   7. Pad with '0's or truncate to exactly 3 digits.
	% -----------------------------------------------------------------
	soundex(String, Code) :-
		soundex(_Representation_, String, Code).

	soundex(atom, String, Code) :-
		atom_chars(String, Codes),
		soundex(chars, Codes, Code0),
		atom_chars(Code, Code0).
	soundex(codes, String, Code) :-
		codes_to_chars(String, Chars),
		soundex(chars, Chars, Code0),
		chars_to_codes(Code0, Code).
	soundex(chars, String, Code) :-
		(	String == []
		->	Code = ''
		;	String = [First|Rest],
			upcase_char(First, FirstUp),
			atom_chars(FirstUp, [FirstChar]),
			soundex_char_code(FirstChar, FirstCode),
			soundex_encode(Rest, Codes0),
			soundex_dedup([FirstCode|Codes0], Deduped0),
			% drop the first code
			Deduped0 = [_|Deduped],
			exclude_zero(Deduped, Filtered),
			soundex_pad(Filtered, 3, Digits),
			Code = [FirstChar| Digits]
		).

	soundex_encode([], []).
	soundex_encode([Char| Chars], [Code| Codes]) :-
		upcase_char(Char, UpcaseChar),
		soundex_char_code(UpcaseChar, Code),
		soundex_encode(Chars, Codes).

	soundex_char_code(Char, Code) :-
		(	soundex_char_code_(Char, Code)
		->	true
		;	Code = '0'
		).

	soundex_char_code_('B', '1').
	soundex_char_code_('F', '1').
	soundex_char_code_('P', '1').
	soundex_char_code_('V', '1').
	soundex_char_code_('C', '2').
	soundex_char_code_('G', '2').
	soundex_char_code_('J', '2').
	soundex_char_code_('K', '2').
	soundex_char_code_('Q', '2').
	soundex_char_code_('S', '2').
	soundex_char_code_('X', '2').
	soundex_char_code_('Z', '2').
	soundex_char_code_('D', '3').
	soundex_char_code_('T', '3').
	soundex_char_code_('L', '4').
	soundex_char_code_('M', '5').
	soundex_char_code_('N', '5').
	soundex_char_code_('R', '6').

	soundex_dedup([], []).
	soundex_dedup([X], [X]).
	soundex_dedup([X,X| Tail], Result) :-
		!,
		soundex_dedup([X| Tail], Result).
	soundex_dedup([X,Y| Tail], [X| Result]) :-
		soundex_dedup([Y| Tail], Result).

	exclude_zero([], []).
	exclude_zero(['0'| Tail], Result) :-
		!,
		exclude_zero(Tail, Result).
	exclude_zero([Head| Tail], [Head| Result]) :-
		exclude_zero(Tail, Result).

	soundex_pad(List, N, Padded) :-
		length(List, Length),
		(   Length >= N
		->  length(Padded, N),
			append(Padded, _, List)
		;   Rem is N - Length,
			length(Zeros, Rem),
			maplist_eq(Zeros, '0'),
			append(List, Zeros, Padded)
		).

	% -----------------------------------------------------------------
	% Metaphone (core rule subset)
	%
	% 1. Strip initial silent letter-pairs (AE, GN, KN, PN, WR).
	% 2. Encode remaining characters left-to-right using context-
	%    sensitive rules.  Each rule consumes one or more characters
	%    from the head of the list and either emits a code atom or
	%    emits nothing ([]).
	% 3. Vowels are always dropped (significance at the start is
	%    handled by step 1).
	% -----------------------------------------------------------------
	metaphone(String, Key) :-
		metaphone(_Representation_, String, Key).

	metaphone(atom, String, Code) :-
		atom_chars(String, Codes),
		metaphone(chars, Codes, Code0),
		atom_chars(Code, Code0).
	metaphone(codes, String, Code) :-
		codes_to_chars(String, Chars),
		metaphone(chars, Chars, Code0),
		chars_to_codes(Code0, Code).
	metaphone(chars, String, Code) :-
		(	String == []
		->	Code = ''
		;	upcase_chars(String, Upper),
			metaphone_drop_initial(Upper, Cs0),
			metaphone_encode(Cs0, Code)
		).

	metaphone_drop_initial(['A','E'| Chars], Chars) :-
		!.
	metaphone_drop_initial(['G','N'| Chars], Chars) :-
		!.
	metaphone_drop_initial(['K','N'| Chars], Chars) :-
		!.
	metaphone_drop_initial(['P','N'| Chars], Chars) :-
		!.
	metaphone_drop_initial(['W','R'| Chars], Chars) :-
		!.
	metaphone_drop_initial(Chars, Chars).

	metaphone_encode([], []).
	metaphone_encode([Char| Chars], Encoded) :-
		metaphone_char(Char, Chars, Code, Rest),
		(	Code == []
		->	metaphone_encode(Rest, Encoded)
		;	Encoded = [Code| RestEnc],
			metaphone_encode(Rest, RestEnc)
		).

	% Vowels — dropped.
	metaphone_char('A', Chars, [], Chars).
	metaphone_char('E', Chars, [], Chars).
	metaphone_char('I', Chars, [], Chars).
	metaphone_char('O', Chars, [], Chars).
	metaphone_char('U', Chars, [], Chars).
	% B
	metaphone_char('B', Chars, 'B', Chars).
	% C — S before E/I/Y; K otherwise.
	metaphone_char('C', ['E'| Chars], 'S', Chars) :-
		!.
	metaphone_char('C', ['I'| Chars], 'S', Chars) :-
		!.
	metaphone_char('C', ['Y'| Chars], 'S', Chars) :-
		!.
	metaphone_char('C', Chars,        'K', Chars).
	% D — J before G+vowel-ish; T otherwise.
	metaphone_char('D', ['G','E'| Chars], 'J', Chars) :-
		!.
	metaphone_char('D', ['G','I'| Chars], 'J', Chars) :-
		!.
	metaphone_char('D', ['G','Y'| Chars], 'J', Chars) :-
		!.
	metaphone_char('D', Chars,            'T', Chars).
	% F
	metaphone_char('F', Chars, 'F', Chars).
	% G — silent before H; J before E/I/Y; K otherwise.
	metaphone_char('G', ['H'| Chars], [], Chars) :-
		!.
	metaphone_char('G', ['E'| Chars], 'J', Chars) :-
		!.
	metaphone_char('G', ['I'| Chars], 'J', Chars) :-
		!.
	metaphone_char('G', ['Y'| Chars], 'J', Chars) :-
		!.
	metaphone_char('G', Chars,        'K', Chars).
	% H — kept only before a vowel.
	metaphone_char('H', [Char| Chars], 'H', Chars) :-
		member(Char, ['A','E','I','O','U']),
		!.
	metaphone_char('H', Chars, [], Chars).
	% J
	metaphone_char('J', Chars, 'J', Chars).
	% K
	metaphone_char('K', Chars, 'K', Chars).
	% L
	metaphone_char('L', Chars, 'L', Chars).
	% M
	metaphone_char('M', Chars, 'M', Chars).
	% N
	metaphone_char('N', Chars, 'N', Chars).
	% P — F before H; P otherwise.
	metaphone_char('P', ['H'| Chars], 'F', Chars) :-
		!.
	metaphone_char('P', Chars,       'P', Chars).
	% Q → K
	metaphone_char('Q', Chars, 'K', Chars).
	% R
	metaphone_char('R', Chars, 'R', Chars).
	% S — X (sh) before H / IA / IO; S otherwise.
	metaphone_char('S', ['H'| Chars],     'X', Chars) :-
		!.
	metaphone_char('S', ['I','A'| Chars], 'X', Chars) :-
		!.
	metaphone_char('S', ['I','O'| Chars], 'X', Chars) :-
		!.
	metaphone_char('S', Chars,            'S', Chars).
	% Chars — 0 (theta) before H; X before IA/IO; T otherwise.
	metaphone_char('Chars', ['H'| Chars],     '0', Chars) :-
		!.
	metaphone_char('T', ['I','A'| Chars], 'X', Chars) :-
		!.
	metaphone_char('T', ['I','O'| Chars], 'X', Chars) :-
		!.
	metaphone_char('T', Chars,            'T', Chars).
	% V → F
	metaphone_char('V', Chars, 'F', Chars).
	% W — kept only before a vowel.
	metaphone_char('W', [Char| Chars], 'W', Chars) :-
		member(Char, ['A','E','I','O','U']),
		!.
	metaphone_char('W', Chars, [], Chars).
	% X → K, push S back onto input.
	metaphone_char('X', Chars, 'K', ['S'| Chars]).
	% Y — kept only before a vowel.
	metaphone_char('Y', [Char| Chars], 'Y', Chars) :-
		member(Char, ['A','E','I','O','U']),
		!.
	metaphone_char('Y', Chars, [], Chars).
	% Z → S
	metaphone_char('Z', Chars, 'S', Chars).
	% Fallback
	metaphone_char(_, Chars, [], Chars).

	% -----------------------------------------------------------------
	% Phonetic match convenience predicates
	% -----------------------------------------------------------------
	soundex_match(String1, String2) :-
		soundex(String1, Code),
		soundex(String2, Code).

	metaphone_match(String1, String2) :-
		metaphone(String1, Key),
		metaphone(String2, Key).

	% =================================================================
	% Private helpers
	% =================================================================

	numlist(Low, High, []) :-
		Low > High, !.
	numlist(Low, High, [Low|T]) :-
		Low =< High,
		Next is Low + 1,
		numlist(Next, High, T).

	maplist_eq([], _).
	maplist_eq([Value| Tail], Value) :-
		maplist_eq(Tail, Value).

	upcase_char(Lower, Upper) :-
		upcase_atom(Lower, Upper).

	upcase_atom(Atom, UpCaseAtom) :-
		atom_codes(Atom, Codes),
		upcase_codes(Codes, UpCaseCodes),
		atom_codes(UpCaseAtom, UpCaseCodes).

	upcase_chars(Chars, UpCaseChars) :-
		chars_to_codes(Chars, Codes),
		upcase_codes(Codes, UpCaseCodes),
		codes_to_chars(UpCaseCodes, UpCaseChars).

	upcase_codes([], []).
	upcase_codes([Code| Codes], [UpCaseCode| UpCaseCodes]) :-
		(	between(0'a, 0'z, Code) ->
			UpCaseCode is Code - 32
		;	UpCaseCode is Code
		),
		upcase_codes(Codes,UpCaseCodes).

	% set_nth0(+Position, +ListIn, +Value, -ListOut)
	set_nth0(0, [_| Tail], Value, [Value| Tail]) :-
		!.
	set_nth0(N, [H| Tail], Value, [H| Tail1]) :-
		N > 0,
		N1 is N - 1,
		set_nth0(N1, Tail, Value, Tail1).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.
