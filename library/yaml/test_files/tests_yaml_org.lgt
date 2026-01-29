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


:- object(tests_yaml_org,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-29,
		comment is 'Tests for the "yaml" library using examples from yaml.org specification.'
	]).

	:- uses(yaml, [
		parse/2, parse_all/2
	]).

	cover(yaml).

	% Example 2.1: Sequence of Scalars (ball players)
	test(yaml_org_example_2_01, true(YAML == ['Mark McGwire', 'Sammy Sosa', 'Ken Griffey'])) :-
		^^file_path('yaml_org/example_2_01.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.2: Mapping Scalars to Scalars (player statistics)
	test(yaml_org_example_2_02, true(YAML == yaml([hr-65, avg-0.278, rbi-147]))) :-
		^^file_path('yaml_org/example_2_02.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.3: Mapping Scalars to Sequences (ball clubs)
	test(yaml_org_example_2_03, true(YAML == yaml([
		american-['Boston Red Sox', 'Detroit Tigers', 'New York Yankees'],
		national-['New York Mets', 'Chicago Cubs', 'Atlanta Braves']
	]))) :-
		^^file_path('yaml_org/example_2_03.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.4: Sequence of Mappings (players' statistics)
	test(yaml_org_example_2_04, true(YAML == [
		yaml([name-'Mark McGwire', hr-65, avg-0.278]),
		yaml([name-'Sammy Sosa', hr-63, avg-0.288])
	])) :-
		^^file_path('yaml_org/example_2_04.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.5: Sequence of Sequences
	test(yaml_org_example_2_05, true(YAML == [
		[name, hr, avg],
		['Mark McGwire', 65, 0.278],
		['Sammy Sosa', 63, 0.288]
	])) :-
		^^file_path('yaml_org/example_2_05.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.6: Mapping of Mappings
	test(yaml_org_example_2_06, true(YAML == yaml([
		'Mark McGwire'-yaml([hr-65, avg-0.278]),
		'Sammy Sosa'-yaml([hr-63, avg-0.288])
	]))) :-
		^^file_path('yaml_org/example_2_06.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.7: Two Documents in a Stream (using parse_all/2)
	test(yaml_org_example_2_07, true(YAML == Expected)) :-
		Expected = [
			['Mark McGwire', 'Sammy Sosa', 'Ken Griffey'],
			['Chicago Cubs', 'St Louis Cardinals']
		],
		^^file_path('yaml_org/example_2_07.yaml', Path),
		parse_all(file(Path), YAML).

	% Example 2.8: Play by Play Feed from a Game (using parse_all/2)
	test(yaml_org_example_2_08, true(YAML == Expected)) :-
		Expected = [
			yaml([time-'20:03:20', player-'Sammy Sosa', action-'strike (miss)']),
			yaml([time-'20:03:47', player-'Sammy Sosa', action-'grand slam'])
		],
		^^file_path('yaml_org/example_2_08.yaml', Path),
		parse_all(file(Path), YAML).

	% Example 2.9: Single Document with Two Comments
	% Empty file - should parse as null
	test(yaml_org_example_2_09, true(YAML == '@'(null))) :-
		^^file_path('yaml_org/example_2_09.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.10: Node for "Sammy Sosa" appears twice in this document
	% Empty file - should parse as null
	test(yaml_org_example_2_10, true(YAML == '@'(null))) :-
		^^file_path('yaml_org/example_2_10.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.11: Mapping between Sequences
	% Skipped: Complex keys (?) not yet supported
	- test(yaml_org_example_2_11, true) :-
		^^file_path('yaml_org/example_2_11.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.12: Compact Nested Mapping
	test(yaml_org_example_2_12, true(YAML == [
		yaml([item-'Super Hoop', quantity-1]),
		yaml([item-'Basketball', quantity-4]),
		yaml([item-'Big Shoes', quantity-1])
	])) :-
		^^file_path('yaml_org/example_2_12.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.13: In literals, newlines are preserved
	test(yaml_org_example_2_13, true(YAML == '\\//||\\/||\n// ||  ||__\n')) :-
		^^file_path('yaml_org/example_2_13.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.14: In the folded scalars, newlines become spaces
	test(yaml_org_example_2_14, true(YAML == 'Mark McGwire\'s year was crippled by a knee injury.\n')) :-
		^^file_path('yaml_org/example_2_14.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.15: Folded newlines are preserved for "more indented" and blank lines
	test(yaml_org_example_2_15, true(YAML == 'Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!\n')) :-
		^^file_path('yaml_org/example_2_15.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.16: Indentation determines scope
	test(yaml_org_example_2_16, true) :-
		^^file_path('yaml_org/example_2_16.yaml', Path),
		parse(file(Path), yaml([name-'Mark McGwire', accomplishment-_, stats-_])).

	% Example 2.17: Quoted Scalars
	test(yaml_org_example_2_17, true, [condition(\+ current_logtalk_flag(unicode, unsupported))]) :-
		^^file_path('yaml_org/example_2_17.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.18: Multi-line Flow Scalars
	% Skipped: Multi-line plain scalars not yet supported
	- test(yaml_org_example_2_18, true) :-
		^^file_path('yaml_org/example_2_18.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.19: Integers
	test(yaml_org_example_2_19, true) :-
		^^file_path('yaml_org/example_2_19.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.20: Floating Point
	test(yaml_org_example_2_20, true) :-
		^^file_path('yaml_org/example_2_20.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.21: Miscellaneous
	% Empty file - should parse as null
	test(yaml_org_example_2_21, true(YAML == '@'(null))) :-
		^^file_path('yaml_org/example_2_21.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.22: Timestamps
	test(yaml_org_example_2_22, true(YAML == yaml([
		canonical-'2001-12-15T02:59:43.1Z',
		iso8601-'2001-12-14t21:59:43.10-05:00',
		spaced-'2001-12-14 21:59:43.10 -5',
		date-'2002-12-14'
	]))) :-
		^^file_path('yaml_org/example_2_22.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.23: Various Explicit Tags
	% Skipped: Tags (!!str, !!binary, !something) not yet supported
	- test(yaml_org_example_2_23, true) :-
		^^file_path('yaml_org/example_2_23.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.24: Global Tags
	% Empty file - should parse as null
	test(yaml_org_example_2_24, true(YAML == '@'(null))) :-
		^^file_path('yaml_org/example_2_24.yaml', Path),
		parse(file(Path), YAML).

	% Example 2.25: Unordered Sets
	% Skipped: Tags and complex keys not yet supported
	- test(yaml_org_example_2_25, true) :-
		^^file_path('yaml_org/example_2_25.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.26: Ordered Mappings
	% Skipped: Tags not yet supported
	- test(yaml_org_example_2_26, true) :-
		^^file_path('yaml_org/example_2_26.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.27: Invoice
	% Skipped: Tags, anchors, aliases, and literal block scalars not yet supported
	- test(yaml_org_example_2_27, true) :-
		^^file_path('yaml_org/example_2_27.yaml', Path),
		parse(file(Path), _YAML).

	% Example 2.28: Log File
	% Skipped: Multi-document streams and literal block scalars not yet supported
	- test(yaml_org_example_2_28, true) :-
		^^file_path('yaml_org/example_2_28.yaml', Path),
		parse(file(Path), _YAML).

:- end_object.

