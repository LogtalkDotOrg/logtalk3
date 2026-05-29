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


:- category(http_docroot_paths).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Shared helpers for validating requests and sandboxing document-root relative paths.'
	]).

	:- protected(validate_relative_path/1).
	:- mode(validate_relative_path(+atom), one_or_error).
	:- info(validate_relative_path/1, [
		comment is 'Validates a document-root relative request path atom.',
		argnames is ['Path']
	]).

	:- protected(validate_request/1).
	:- mode(validate_request(+compound), one_or_error).
	:- info(validate_request/1, [
		comment is 'Validates a normalized HTTP request term.',
		argnames is ['Request']
	]).

	:- protected(validate_document_root/1).
	:- mode(validate_document_root(+atom), one_or_error).
	:- info(validate_document_root/1, [
		comment is 'Validates a document root path atom.',
		argnames is ['DocumentRoot']
	]).

	:- protected(supported_method/1).
	:- mode(supported_method(+compound), zero_or_one).
	:- info(supported_method/1, [
		comment is 'True when the request method is supported for document-root serving, currently ``get`` or ``head``.',
		argnames is ['Request']
	]).

	:- protected(resolved_target_path/3).
	:- mode(resolved_target_path(+atom, +atom, -atom), zero_or_one).
	:- info(resolved_target_path/3, [
		comment is 'Resolves a request path against a document root and succeeds only when the resulting path stays within that root.',
		argnames is ['Path', 'Root', 'Candidate']
	]).

	:- protected(path_within_root/2).
	:- mode(path_within_root(+atom, +atom), zero_or_one).
	:- info(path_within_root/2, [
		comment is 'True when the candidate path is the document root itself or a path nested under it.',
		argnames is ['Root', 'Candidate']
	]).

	validate_relative_path(Path) :-
		(	var(Path) ->
			instantiation_error
		;	atom(Path) ->
			true
		;	type_error(atom, Path)
		).

	validate_request(Request) :-
		(	http::is_request(Request) ->
			true
		;	domain_error(http_request, Request)
		).

	validate_document_root(DocumentRoot) :-
		(	var(DocumentRoot) ->
			instantiation_error
		;	atom(DocumentRoot) ->
			true
		;	type_error(atom, DocumentRoot)
		).

	supported_method(Request) :-
		http::method(Request, Method),
		once((Method == get; Method == head)).

	resolved_target_path(Path, Root, Candidate) :-
		normalized_relative_path(Path, RelativePath),
		os::path_concat(Root, RelativePath, JoinedPath),
		os::absolute_file_name(JoinedPath, Candidate),
		path_within_root(Root, Candidate).

	normalized_relative_path(Path, RelativePath) :-
		strip_leading_slashes(Path, StrippedPath),
		\+ unsafe_request_path(StrippedPath),
		RelativePath = StrippedPath.

	strip_leading_slashes(Path, StrippedPath) :-
		atom_codes(Path, Codes),
		strip_leading_slash_codes(Codes, StrippedCodes),
		atom_codes(StrippedPath, StrippedCodes).

	strip_leading_slash_codes([0'/| Codes], StrippedCodes) :-
		!,
		strip_leading_slash_codes(Codes, StrippedCodes).
	strip_leading_slash_codes(Codes, Codes).

	unsafe_request_path(Path) :-
		Path \== '',
		os::is_absolute_file_name(Path).
	unsafe_request_path(Path) :-
		sub_atom(Path, _, 1, _, '\\').

	path_within_root(Root, Candidate) :-
		Candidate == Root,
		!.
	path_within_root(Root, Candidate) :-
		sub_atom(Candidate, 0, _, _, Root).

:- end_category.
