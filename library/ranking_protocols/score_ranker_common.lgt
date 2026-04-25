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


:- category(score_ranker_common,
	extends(ranker_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-25,
		comment is 'Shared predicates for score-based rankers that order candidates using precomputed item scores.'
	]).

	:- protected(rank_by_scores/3).
	:- mode(rank_by_scores(+list(pair), +list, -list), one).
	:- info(rank_by_scores/3, [
		comment is 'Ranks candidates using precomputed ``Item-Score`` pairs while validating item existence and duplicate candidates.',
		argnames is ['Scores', 'Candidates', 'Ranking']
	]).

	:- uses(avltree, [
		as_dictionary/2, insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		sort/4
	]).

	:- uses(pairs, [
		values/2 as pairs_values/2
	]).

	rank_by_scores(Scores, Candidates, Ranking) :-
		as_dictionary(Scores, ScoreDictionary),
		dictionary_new(SeenCandidates),
		rank_candidate_pairs(Candidates, ScoreDictionary, SeenCandidates, Candidates, Pairs),
		sort(1, @=<, Pairs, SortedPairs),
		pairs_values(SortedPairs, Ranking).

	rank_candidate_pairs(Candidates, _ScoreDictionary, _SeenCandidates, _Original, _Pairs) :-
		var(Candidates),
		!,
		instantiation_error.
	rank_candidate_pairs([], _ScoreDictionary, _SeenCandidates, _Original, []) :-
		!.
	rank_candidate_pairs([Candidate| Candidates], ScoreDictionary, SeenCandidates0, Original, [pair(NegScore, Candidate)-Candidate| Pairs]) :-
		!,
		(   var(Candidate) ->
			instantiation_error
		;   dictionary_lookup(Candidate, _Seen, SeenCandidates0) ->
			domain_error(unique_candidates, Original)
		;   dictionary_lookup(Candidate, Score, ScoreDictionary) ->
			dictionary_insert(SeenCandidates0, Candidate, true, SeenCandidates),
			NegScore is -Score,
			rank_candidate_pairs(Candidates, ScoreDictionary, SeenCandidates, Original, Pairs)
		;   existence_error(item, Candidate)
		).
	rank_candidate_pairs(Candidates, _ScoreDictionary, _SeenCandidates, _Original, _Pairs) :-
		type_error(list, Candidates).

:- end_category.
