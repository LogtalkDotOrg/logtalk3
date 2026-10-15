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


:- object(language_profile_reference(_Profile_),
	imports(language_profile_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Derives profile vectors directly from a source profile stop-word inventory.',
		parameters is ['Profile' - 'Source profile object.']
	]).

	language(Language) :-
		_Profile_::language(Language).

	stop_word(Word) :-
		_Profile_::stop_word(Word).

:- end_object.


:- object(language_profile_checker).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Checks stored built-in language profile vectors and squared norms.'
	]).

	:- public(check/0).
	:- mode(check, zero_or_one).
	:- info(check/0, [
		comment is 'Checks every built-in profile vector and squared norm against direct derivation. Requires the library to be loaded.'
	]).

	check :-
		forall(language_profiles::default_profile(_, Profile), (
			language_profile_reference(Profile)::trigram_counts(Expected),
			Profile::trigram_counts(Actual, SquareSum),
			Actual == Expected,
			square_sum(Expected, 0.0, ExpectedSquareSum),
			SquareSum =:= ExpectedSquareSum
		)).

	square_sum([], Sum, Sum).
	square_sum([_-Count| Counts], Sum0, Sum) :-
		Sum1 is Sum0 + Count * Count,
		square_sum(Counts, Sum1, Sum).

:- end_object.