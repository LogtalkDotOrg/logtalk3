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


:- object(language_detection_scripts).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-07,
		comment is 'Unicode script analysis and language script registry.'
	]).

	:- public(script_ratios/2).
	:- mode(script_ratios(+list(character_code), -list(pair(atom,float))), one).
	:- info(script_ratios/2, [
		comment is 'Returns observed Unicode scripts and their ratios ordered by decreasing ratio and then by script name.',
		argnames is ['Codes', 'ScriptRatios']
	]).

	:- public(language_scripts/2).
	:- mode(language_scripts(?atom, ?list(atom)), zero_or_more).
	:- info(language_scripts/2, [
		comment is 'Enumerates effective ISO 639-1 language codes and their expected Unicode scripts. Custom mappings override default mappings.',
		argnames is ['Language', 'Scripts']
	]).

	:- public(default_language_scripts/2).
	:- mode(default_language_scripts(?atom, ?list(atom)), zero_or_more).
	:- info(default_language_scripts/2, [
		comment is 'Enumerates default ISO 639-1 language codes and their expected Unicode scripts.',
		argnames is ['Language', 'Scripts']
	]).

	:- public(custom_language_scripts/2).
	:- multifile(custom_language_scripts/2).
	:- mode(custom_language_scripts(?atom, ?list(atom)), zero_or_more).
	:- info(custom_language_scripts/2, [
		comment is 'Enumerates custom ISO 639-1 language codes and their expected Unicode scripts.',
		argnames is ['Language', 'Scripts']
	]).

	:- public(scripts_to_languages/2).
	:- mode(scripts_to_languages(+list(atom), -list(atom)), one).
	:- info(scripts_to_languages/2, [
		comment is 'Returns the sorted unique registered languages compatible with any meaningful script in the given list.',
		argnames is ['Scripts', 'Languages']
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(user, [
		unicode_script/2
	]).

	script_ratios([], []) :-
		!.
	script_ratios(Codes, ScriptRatios) :-
		Codes = [_| _],
		count_scripts(Codes, [], Counts),
		length(Codes, Total),
		decorate_ratios(Counts, Total, DecoratedRatios),
		sort(DecoratedRatios, SortedDecoratedRatios),
		undecorate_ratios(SortedDecoratedRatios, ScriptRatios).

	language_scripts(Language, Scripts) :-
		nonvar(Language),
		!,
		(	custom_language_scripts(Language, _) ->
			custom_language_scripts(Language, Scripts)
		;	default_language_scripts(Language, Scripts)
		).
	language_scripts(Language, Scripts) :-
		custom_language_scripts(Language, Scripts).
	language_scripts(Language, Scripts) :-
		default_language_scripts(Language, Scripts),
		\+ custom_language_scripts(Language, _).

	scripts_to_languages(Scripts, Languages) :-
		findall(
			Language,
			(	language_scripts(Language, LanguageScripts),
				member(Script, Scripts),
				meaningful_script(Script),
				member(Script, LanguageScripts)
			),
			Languages0
		),
		sort(Languages0, Languages).

	count_scripts([], Counts, Counts).
	count_scripts([Code| Codes], Counts0, Counts) :-
		unicode_script(Code, Script),
		increment_count(Counts0, Script, Counts1),
		count_scripts(Codes, Counts1, Counts).

	increment_count([], Script, [Script-1]).
	increment_count([Script-Count0| Counts], Script, [Script-Count| Counts]) :-
		!,
		Count is Count0 + 1.
	increment_count([Pair| Counts0], Script, [Pair| Counts]) :-
		increment_count(Counts0, Script, Counts).

	decorate_ratios([], _, []).
	decorate_ratios([Script-Count| Counts], Total, [pair(NegativeRatio, Script)-(Script-Ratio)| Ratios]) :-
		Ratio is Count / Total,
		NegativeRatio is -Ratio,
		decorate_ratios(Counts, Total, Ratios).

	undecorate_ratios([], []).
	undecorate_ratios([_-Ratio| DecoratedRatios], [Ratio| Ratios]) :-
		undecorate_ratios(DecoratedRatios, Ratios).

	meaningful_script(Script) :-
		Script \== 'Common',
		Script \== 'Inherited',
		Script \== 'Unknown',
		Script \== 'Zzzz'.

	default_language_scripts(af, ['Latin']).
	default_language_scripts(ar, ['Arabic']).
	default_language_scripts(bg, ['Cyrillic']).
	default_language_scripts(bn, ['Bengali']).
	default_language_scripts(br, ['Latin']).
	default_language_scripts(ca, ['Latin']).
	default_language_scripts(cs, ['Latin']).
	default_language_scripts(da, ['Latin']).
	default_language_scripts(de, ['Latin']).
	default_language_scripts(el, ['Greek']).
	default_language_scripts(en, ['Latin']).
	default_language_scripts(eo, ['Latin']).
	default_language_scripts(es, ['Latin']).
	default_language_scripts(et, ['Latin']).
	default_language_scripts(eu, ['Latin']).
	default_language_scripts(fa, ['Arabic']).
	default_language_scripts(fi, ['Latin']).
	default_language_scripts(fr, ['Latin']).
	default_language_scripts(ga, ['Latin']).
	default_language_scripts(gl, ['Latin']).
	default_language_scripts(ha, ['Latin']).
	default_language_scripts(he, ['Hebrew']).
	default_language_scripts(hi, ['Devanagari']).
	default_language_scripts(hr, ['Latin']).
	default_language_scripts(hu, ['Latin']).
	default_language_scripts(hy, ['Armenian']).
	default_language_scripts(id, ['Latin']).
	default_language_scripts(it, ['Latin']).
	default_language_scripts(ja, ['Han', 'Hiragana', 'Katakana']).
	default_language_scripts(ko, ['Hangul']).
	default_language_scripts(ku, ['Arabic']).
	default_language_scripts(la, ['Latin']).
	default_language_scripts(lt, ['Latin']).
	default_language_scripts(lv, ['Latin']).
	default_language_scripts(mr, ['Devanagari']).
	default_language_scripts(ms, ['Latin']).
	default_language_scripts(nl, ['Latin']).
	default_language_scripts(no, ['Latin']).
	default_language_scripts(pl, ['Latin']).
	default_language_scripts(pt, ['Latin']).
	default_language_scripts(ro, ['Latin']).
	default_language_scripts(ru, ['Cyrillic']).
	default_language_scripts(sk, ['Latin']).
	default_language_scripts(sl, ['Latin']).
	default_language_scripts(so, ['Latin']).
	default_language_scripts(st, ['Latin']).
	default_language_scripts(sv, ['Latin']).
	default_language_scripts(sw, ['Latin']).
	default_language_scripts(th, ['Thai']).
	default_language_scripts(tl, ['Latin']).
	default_language_scripts(tr, ['Latin']).
	default_language_scripts(uk, ['Cyrillic']).
	default_language_scripts(ur, ['Arabic']).
	default_language_scripts(vi, ['Latin']).
	default_language_scripts(yo, ['Latin']).
	default_language_scripts(zh, ['Han']).
	default_language_scripts(zu, ['Latin']).

:- end_object.
