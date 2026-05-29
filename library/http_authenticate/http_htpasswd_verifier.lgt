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


:- object(http_htpasswd_verifier(_Path_),
	implements(http_authenticate_verifier_protocol),
	imports(http_text_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Portable Apache ``.htpasswd`` subset verifier supporting ``{SHA}`` entries and rejecting unsupported hash markers.',
		parameters is [
			'Path' - 'Path of the password file to load on each verification request.'
		]
	]).

	:- uses(list, [
		memberchk/2, reverse/2
	]).

	verify(_Realm, Username, Password) :-
		load_password_entries(_Path_, Entries),
		memberchk(Username-HashSpec, Entries),
		verify_password_hash(HashSpec, Password).

	load_password_entries(Path0, Entries) :-
		check_path(Path0, Path),
		reader::file_to_codes(Path, Codes),
		split_lines(Codes, Lines),
		parse_password_lines(Lines, Path, 1, [], Entries0),
		reverse(Entries0, Entries).

	check_path(Path, _CheckedPath) :-
		var(Path),
		!,
		instantiation_error.
	check_path(Path0, Path) :-
		atom(Path0),
		!,
		os::absolute_file_name(Path0, Path),
		(	os::file_exists(Path) ->
			true
		;	existence_error(http_password_file, Path)
		).
	check_path(Path, _CheckedPath) :-
		type_error(atom, Path).

	parse_password_lines([], _Path, _LineNumber, Entries, Entries).
	parse_password_lines([Line0| Lines], Path, LineNumber, Entries0, Entries) :-
		trim_ows_codes(Line0, Line),
		(	Line == [] ->
			Entries1 = Entries0
		;	Line = [0'#| _] ->
			Entries1 = Entries0
		;	parse_password_line(Path, LineNumber, Line, Entry),
			Entry = Username-_,
			(	memberchk(Username-_, Entries0) ->
				domain_error(http_password_file(Path), duplicate(LineNumber, Username))
			;	Entries1 = [Entry| Entries0]
			)
		),
		NextLineNumber is LineNumber + 1,
		parse_password_lines(Lines, Path, NextLineNumber, Entries1, Entries).

	parse_password_line(Path, LineNumber, Line, Username-HashSpec) :-
		split_line_at_first_colon(Line, UsernameCodes, HashCodes),
		UsernameCodes \== [],
		HashCodes \== [],
		atom_codes(Username, UsernameCodes),
		parse_password_hash(Path, LineNumber, HashCodes, HashSpec),
		!.
	parse_password_line(Path, LineNumber, _Line, _Entry) :-
		domain_error(http_password_file(Path), invalid(LineNumber)).

	split_line_at_first_colon([0':| HashCodes], [], HashCodes) :-
		!.
	split_line_at_first_colon([Code| Codes], [Code| UsernameCodes], HashCodes) :-
		split_line_at_first_colon(Codes, UsernameCodes, HashCodes).

	parse_password_hash(Path, LineNumber, [0'{, 0'S, 0'H, 0'A, 0'}| DigestCodes], sha1(Digest)) :-
		DigestCodes \== [],
		catch(base64::parse(codes(DigestCodes), DigestBytes), _Error, domain_error(http_password_file(Path), invalid(LineNumber))),
		length(DigestBytes, 20),
		atom_codes(Digest, DigestCodes),
		!.
	parse_password_hash(Path, LineNumber, [0'{, 0'S, 0'H, 0'A, 0'}| _DigestCodes], _HashSpec) :-
		domain_error(http_password_file(Path), invalid(LineNumber)).
	parse_password_hash(Path, LineNumber, [0'$| MarkerCodes], _HashSpec) :-
		password_hash_marker(MarkerCodes, Marker),
		domain_error(http_password_file(Path), unsupported(LineNumber, Marker)).
	parse_password_hash(Path, LineNumber, _HashCodes, _HashSpec) :-
		domain_error(http_password_file(Path), unsupported(LineNumber, crypt)).

	password_hash_marker(Codes, Marker) :-
		password_hash_marker(Codes, [], MarkerCodes),
		( MarkerCodes == [] -> Marker = unknown ; atom_codes(Marker, MarkerCodes) ).

	password_hash_marker([], Acc0, MarkerCodes) :-
		reverse(Acc0, MarkerCodes).
	password_hash_marker([0'$| _Codes], Acc0, MarkerCodes) :-
		reverse(Acc0, MarkerCodes),
		!.
	password_hash_marker([Code| Codes], Acc0, MarkerCodes) :-
		password_hash_marker(Codes, [Code| Acc0], MarkerCodes).

	verify_password_hash(sha1(StoredDigest), Password) :-
		atom_codes(Password, PasswordCodes),
		sha1::digest(PasswordCodes, DigestBytes),
		base64::generate(atom(ComputedDigest), DigestBytes),
		ComputedDigest == StoredDigest.

	split_lines(Codes, Lines) :-
		split_lines(Codes, [], [], ReversedLines),
		reverse(ReversedLines, Lines).

	split_lines([], Current0, Lines0, Lines) :-
		(	Current0 == [] ->
			Lines = Lines0
		;	reverse(Current0, Current),
			Lines = [Current| Lines0]
		).
	split_lines([0'\r, 0'\n| Codes], Current0, Lines0, Lines) :-
		!,
		reverse(Current0, Current),
		split_lines(Codes, [], [Current| Lines0], Lines).
	split_lines([0'\n| Codes], Current0, Lines0, Lines) :-
		!,
		reverse(Current0, Current),
		split_lines(Codes, [], [Current| Lines0], Lines).
	split_lines([0'\r| Codes], Current0, Lines0, Lines) :-
		!,
		reverse(Current0, Current),
		split_lines(Codes, [], [Current| Lines0], Lines).
	split_lines([Code| Codes], Current0, Lines0, Lines) :-
		split_lines(Codes, [Code| Current0], Lines0, Lines).

	trim_ows_codes(Codes0, Codes) :-
		^^trim_ows_codes(Codes0, Codes).

:- end_object.
