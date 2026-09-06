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


:- object(language_profiles).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Registry of language detection profiles.',
		see_also is [language_profile_protocol]
	]).

	:- public(profile/2).
	:- mode(profile(?atom, ?object_identifier), zero_or_more).
	:- info(profile/2, [
		comment is 'Enumerates the effective ISO 639-1 language codes and profile objects. Custom profiles override default profiles for the same language code.',
		argnames is ['Language', 'Profile']
	]).

	:- public(default_profile/2).
	:- multifile(default_profile/2).
	:- mode(default_profile(?atom, ?object_identifier), zero_or_more).
	:- info(default_profile/2, [
		comment is 'Enumerates default built-in ISO 639-1 language codes and profile objects.',
		argnames is ['Language', 'Profile']
	]).

	:- public(custom_profile/2).
	:- multifile(custom_profile/2).
	:- mode(custom_profile(?atom, ?object_identifier), zero_or_more).
	:- info(custom_profile/2, [
		comment is 'Enumerates custom ISO 639-1 language codes and profile objects.',
		argnames is ['Language', 'Profile']
	]).

	profile(Language, Profile) :-
		nonvar(Language),
		!,
		(	custom_profile(Language, _) ->
			custom_profile(Language, Profile)
		;	default_profile(Language, Profile)
		).
	profile(Language, Profile) :-
		custom_profile(Language, Profile).
	profile(Language, Profile) :-
		default_profile(Language, Profile),
		\+ custom_profile(Language, _).

:- end_object.
