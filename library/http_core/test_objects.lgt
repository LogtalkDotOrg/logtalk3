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


:- object(http_docroot_paths_test_helper,
	imports(http_docroot_paths)).

	:- public(check_relative_path/1).
	:- mode(check_relative_path(+atom), one_or_error).

	:- public(check_document_root/1).
	:- mode(check_document_root(+atom), one_or_error).

	check_relative_path(Path) :-
		^^validate_relative_path(Path).

	check_document_root(DocumentRoot) :-
		^^validate_document_root(DocumentRoot).

:- end_object.


:- object(http_origin_site_test_helper,
	imports(http_origin_site_helpers)).

	:- public(check_absolute_url_context/2).
	:- mode(check_absolute_url_context(+atom, -compound), one_or_error).

	:- public(check_origin_endpoint/2).
	:- mode(check_origin_endpoint(+atom, -compound), one_or_error).

	:- public(check_request_endpoint/2).
	:- mode(check_request_endpoint(+compound, -compound), one_or_error).

	:- public(check_same_site/2).
	:- mode(check_same_site(+compound, +compound), zero_or_one).

	check_absolute_url_context(URL, Context) :-
		^^absolute_url_context(URL, Context).

	check_origin_endpoint(Origin, Endpoint) :-
		^^origin_endpoint(Origin, Endpoint).

	check_request_endpoint(Request, Endpoint) :-
		^^request_endpoint(Request, Endpoint).

	check_same_site(Left, Right) :-
		^^same_site(Left, Right).

:- end_object.
