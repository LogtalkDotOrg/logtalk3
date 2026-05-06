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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Performance benchmarks for the "borda_ranker" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	test(large_ranked_ballots_reference_timing, true, [note(metrics(train_seconds-TrainTime, item_count-Count, top_score-TopScore, bottom_score-BottomScore))]) :-
		reference_timing(TrainTime, Count, TopScore, BottomScore).

	reference_timing(TrainTime, Count, TopScore, BottomScore) :-
		benchmark(borda_ranker::learn(large_ranked_ballots, _Ranker), TrainTime),
		borda_ranker::learn(large_ranked_ballots, Ranker),
		borda_ranker::scores(Ranker, Scores),
		length(Scores, Count),
		Count == 60,
		memberchk(item(1)-TopScore, Scores),
		memberchk(item(60)-BottomScore, Scores),
		TopScore =:= 11800,
		BottomScore =:= 0.

:- end_object.
