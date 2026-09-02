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


:- object(http_server_core_oauth_endpoint_handler(
	_ProtectedResource_, _MetadataDescriptors_, _MetadataOptions_, _Verifier_, _Handler_, _ProtectOptions_
),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-01,
		comment is 'OAuth HTTP handler serving public protected-resource metadata and protecting all other requests.',
		parameters is [
			'ProtectedResource' - 'Canonical OAuth protected-resource identifier.',
			'MetadataDescriptors' - 'Protected-resource metadata descriptor list.',
			'MetadataOptions' - 'Options passed to ``http_oauth_metadata::response/4``.',
			'Verifier' - 'Object implementing the ``http_oauth_verifier_protocol`` protocol.',
			'Handler' - 'Wrapped object implementing the ``http_handler_protocol`` protocol.',
			'ProtectOptions' - 'Options passed to ``http_oauth::protect_request/4``.'
		]
	]).

	handle(Request, Response) :-
		metadata_path(MetadataPath),
		http_core::target(Request, origin(MetadataPath)),
		!,
		http_core::method(Request, Method),
		(	Method == get ->
			http_oauth_metadata::response(_ProtectedResource_, _MetadataDescriptors_, Response, _MetadataOptions_)
		;	http_core::version(Request, Version),
			http_core::response(Version, status(405, 'Method Not Allowed'), [allow-'GET'], empty, [], Response)
		).
	handle(Request, Response) :-
		http_server_core_oauth_handler(_Verifier_, _Handler_, _ProtectOptions_)::handle(Request, Response).

	metadata_path(Path) :-
		http_oauth_metadata::well_known_url(_ProtectedResource_, URL),
		url(atom)::parse(URL, Components),
		list::memberchk(path(Path), Components).

:- end_object.
