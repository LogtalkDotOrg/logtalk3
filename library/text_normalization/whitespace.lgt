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


:- category(text_whitespace,
	extends(unicode_character_data)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Portable Unicode whitespace, line-ending, and control-character normalization rules over code lists.',
		see_also is [text_normalizer(_, _), string(_)]
	]).

	:- protected(normalize_whitespace_codes/6).
	:- mode(normalize_whitespace_codes(+list(integer), +boolean, +atom, +atom, +atom, -list(integer)), one).
	:- info(normalize_whitespace_codes/6, [
		comment is 'Normalizes code-list whitespace using trim, collapse, line-ending, and control-character policies.',
		argnames is ['Codes', 'Trim', 'Collapse', 'LineEndings', 'Controls', 'Normalized']
	]).

	normalize_whitespace_codes(Codes, Trim, Collapse, LineEndings, Controls, Normalized) :-
		remove_controls(Controls, Codes, Controlled),
		normalize_line_endings(LineEndings, Controlled, Lines),
		collapse_whitespace(Collapse, Lines, Collapsed),
		trim_whitespace(Trim, Collapsed, Normalized),
		!.

	remove_controls(preserve, Codes, Codes).
	remove_controls(remove, [], []).
	remove_controls(remove, [Code| Codes], Controlled) :-
		(	control_code(Code), \+ white_space_code(Code) ->
			Controlled = Rest
		;	Controlled = [Code| Rest]
		),
		remove_controls(remove, Codes, Rest).

	normalize_line_endings(preserve, Codes, Codes).
	normalize_line_endings(LineEndings, Codes, Normalized) :-
		LineEndings \== preserve,
		normalize_line_endings_codes(Codes, LineEndings, Normalized).

	normalize_line_endings_codes([], _, []).
	normalize_line_endings_codes([13, 10| Codes], LineEndings, Normalized) :-
		!,
		line_ending_codes(LineEndings, Rest, Normalized),
		normalize_line_endings_codes(Codes, LineEndings, Rest).
	normalize_line_endings_codes([Code| Codes], LineEndings, Normalized) :-
		line_break_code(Code),
		!,
		line_ending_codes(LineEndings, Rest, Normalized),
		normalize_line_endings_codes(Codes, LineEndings, Rest).
	normalize_line_endings_codes([Code| Codes], LineEndings, [Code| Normalized]) :-
		normalize_line_endings_codes(Codes, LineEndings, Normalized).

	line_ending_codes(lf, Rest, [10| Rest]).
	line_ending_codes(cr, Rest, [13| Rest]).
	line_ending_codes(crlf, Rest, [13, 10| Rest]).

	collapse_whitespace(none, Codes, Codes).
	collapse_whitespace(all, [], []).
	collapse_whitespace(all, [Code| Codes], [32| Collapsed]) :-
		white_space_code(Code),
		!,
		drop_white_space(Codes, Rest),
		collapse_whitespace(all, Rest, Collapsed).
	collapse_whitespace(all, [Code| Codes], [Code| Collapsed]) :-
		collapse_whitespace(all, Codes, Collapsed).
	collapse_whitespace(horizontal, [], []).
	collapse_whitespace(horizontal, [Code| Codes], [32| Collapsed]) :-
		horizontal_white_space_code(Code),
		!,
		drop_horizontal_white_space(Codes, Rest),
		collapse_whitespace(horizontal, Rest, Collapsed).
	collapse_whitespace(horizontal, [Code| Codes], [Code| Collapsed]) :-
		collapse_whitespace(horizontal, Codes, Collapsed).

	drop_white_space([Code| Codes], Rest) :-
		white_space_code(Code),
		!,
		drop_white_space(Codes, Rest).
	drop_white_space(Codes, Codes).

	drop_horizontal_white_space([Code| Codes], Rest) :-
		horizontal_white_space_code(Code),
		!,
		drop_horizontal_white_space(Codes, Rest).
	drop_horizontal_white_space(Codes, Codes).

	trim_whitespace(false, Codes, Codes).
	trim_whitespace(true, Codes, Trimmed) :-
		white_space_codes(WhiteSpaceCodes),
		string(codes)::trim(Codes, WhiteSpaceCodes, Trimmed).

	white_space_codes([9, 10, 11, 12, 13, 32, 133, 160, 5760, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201, 8202, 8232, 8233, 8239, 8287, 12288]).

	horizontal_white_space_code(Code) :-
		white_space_code(Code),
		\+ line_break_code(Code).

	white_space_code(Code) :-
		Code >= 9,
		Code =< 13,
		!.
	white_space_code(32).
	white_space_code(133).
	white_space_code(160).
	white_space_code(5760).
	white_space_code(Code) :-
		Code >= 8192,
		Code =< 8202,
		!.
	white_space_code(8232).
	white_space_code(8233).
	white_space_code(8239).
	white_space_code(8287).
	white_space_code(12288).

	line_break_code(10).
	line_break_code(13).
	line_break_code(133).
	line_break_code(8232).
	line_break_code(8233).

	control_code(Code) :-
		^^control_code_range(Start, End),
		Code >= Start,
		Code =< End,
		!.

:- end_category.
