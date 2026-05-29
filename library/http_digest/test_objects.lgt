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


:- object(http_digest_test_verifier,
	implements(http_digest_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Local Digest verifier used by the http_digest library tests.'
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	ha1(md5, Realm, Username, HA1) :-
		credential_password(Realm, Username, Password),
		credential_material_codes(Username, Realm, Password, Codes),
		md5::hash(Codes, HA1).

	ha1(sha256, Realm, Username, HA1) :-
		credential_password(Realm, Username, Password),
		credential_material_codes(Username, Realm, Password, Codes),
		sha256::hash(Codes, HA1).

	ha1(sha512_256, Realm, Username, HA1) :-
		credential_password(Realm, Username, Password),
		credential_material_codes(Username, Realm, Password, Codes),
		sha512_256::hash(Codes, HA1).

	credential_password('test-realm', 'Mufasa', 'Circle Of Life').

	credential_material_codes(Username, Realm, Password, Codes) :-
		atomic_list_concat([Username, Realm, Password], ':', Material),
		atom_codes(Material, Codes).

:- end_object.


:- object(http_digest_test_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Local HTTP handler used by the http_digest library tests.'
	]).

	handle(Request, Response) :-
		http::version(Request, Version),
		http::property(Request, digest_username(Username)),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(Username)), [], Response).

:- end_object.


:- object(http_digest_request_echo_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Local HTTP handler used by the http_digest library tests to exercise digest client session request wrappers and request option merging.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	handle(Request, Response) :-
		http::version(Request, Version),
		http::target(Request, Target),
		(	request_info_target_(Target) ->
			request_info_response_(Request, Version, Response)
		;	echo_response_(Request, Version, Response)
		).

	request_info_target_(origin('/request-info')).
	request_info_target_(origin('/request-info', _Query)).

	request_info_response_(Request, Version, Response) :-
		http::property(Request, query_pairs(QueryPairs)),
		memberchk(lang-Lang, QueryPairs),
		memberchk(page-Page, QueryPairs),
		memberchk(item-Item, QueryPairs),
		http::version(Request, http(Major, Minor)),
		http::property(Request, cookies(CookiePairs)),
		memberchk(session-Session, CookiePairs),
		Body = {lang-Lang, page-Page, item-Item, session-Session, major-Major, minor-Minor},
		http::response(Version, status(200, 'OK'), [], content('application/json', json(Body)), [], Response).

	echo_response_(Request, Version, Response) :-
		http::body(Request, Body),
		(	Body == empty ->
			http::method(Request, Method),
			ResponseBody = content('text/plain', text(Method))
		;	ResponseBody = Body
		),
		http::response(Version, status(200, 'OK'), [], ResponseBody, [], Response).

:- end_object.


:- object(http_digest_multipart_summary_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Local HTTP handler used by the http_digest library tests to summarize multipart form-data requests.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	handle(Request, Response) :-
		http::version(Request, Version),
		http::body(Request, Body),
		http_multipart::fields(Body, [title-Title]),
		http::property(Request, content_type('multipart/form-data', Parameters)),
		memberchk(boundary-Boundary, Parameters),
		atom_concat('title=', Title, Prefix),
		atom_concat(Prefix, '; boundary=', Prefix0),
		atom_concat(Prefix0, Boundary, Summary),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(Summary)), [], Response).

:- end_object.
