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


:- object(script_language_detector,
	implements(language_detection_strategy_protocol),
	imports(language_detection_scoring)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-07,
		comment is 'Language detection strategy using Unicode script evidence.',
		see_also is [ngram_language_detector, stopword_language_detector, hybrid_language_detector]
	]).

	:- uses(list, [
		member/2
	]).

	scores(Codes, Candidates, Scores) :-
		language_detection_scripts::script_ratios(Codes, ScriptRatios),
		candidate_scores(Candidates, ScriptRatios, RawScores),
		^^normalize_scores(RawScores, Scores).

	candidate_scores([], _, []).
	candidate_scores([Language| Languages], ScriptRatios, [Language-Score| Scores]) :-
		(	language_detection_scripts::language_scripts(Language, Scripts) ->
			matching_script_score(ScriptRatios, Scripts, 0.0, Score)
		;	Score = 0.0
		),
		candidate_scores(Languages, ScriptRatios, Scores).

	matching_script_score([], _, Score, Score).
	matching_script_score([Script-Ratio| ScriptRatios], Scripts, Score0, Score) :-
		(	member(Script, Scripts) ->
			Score1 is Score0 + Ratio
		;	Score1 = Score0
		),
		matching_script_score(ScriptRatios, Scripts, Score1, Score).

:- end_object.
