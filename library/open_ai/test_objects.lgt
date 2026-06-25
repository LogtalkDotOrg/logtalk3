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


:- object(sample_open_ai_backend,
	implements(open_ai_backend_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'Sample backend used by the open_ai tests.'
	]).

	handle_open_ai(OperationId, _Request, ok({operation-OperationId, object-'test'})).

:- end_object.


:- object(open_ai_client_test_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'HTTP handler used by the open_ai client tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::method(Request, Method),
		http_core::target(Request, Target),
		target_path(Target, Path),
		http_core::response(Version, status(200, 'OK'), [], content('application/json', json({method-Method, path-Path})), [], Response).

	target_path(origin(Path), Path).
	target_path(origin(Path, _Query), Path).

:- end_object.
