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


:- object(sample_htmx_router,
	implements(http_handler_protocol),
	imports([http_router, http_router_htmx])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-26,
		comment is 'Sample router object used by the ``http_htmx`` tests.'
	]).

	:- protected(show_panel/2).
	:- info(show_panel/2, [
		comment is 'Route handler used by the sample HTMX router object for the ``/panel`` path.',
		argnames is ['Request', 'Response']
	]).

	middleware(htmx_request, annotate_htmx_request).
	response_middleware(htmx_response_headers, add_htmx_response_headers).

	route(show_panel, get, '/panel', show_panel).

	route_metadata(show_panel, [htmx_response_options([retarget('#panel')])]).

	show_panel(Request, Response) :-
		( 	http::property(Request, htmx_request(true)) ->
			BodyText = '<div>htmx-panel</div>'
		; 	BodyText = '<div>plain-panel</div>'
		),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/html', text(BodyText)), [htmx_response_options([trigger(saved)])], Response).

:- end_object.
