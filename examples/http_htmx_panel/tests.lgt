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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Unit tests for the "http_htmx_panel" example.'
	]).

	:- uses(http_core, [
		body/2, header/3, request/7, status/2
	]).

	cover(htmx_panel_http_handler).
	cover(htmx_panel_server).
	cover(htmx_panel_client).
	cover(http_htmx_panel_demo).

	test(http_htmx_panel_handler_01, deterministic) :-
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		htmx_panel_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'src="https://unpkg.com/htmx.org@2.0.4"')),
		once(sub_atom(HTML, _, _, _, 'hx-get="/panel/metrics"')),
		once(sub_atom(HTML, _, _, _, 'hx-indicator="#panel-loading"')),
		once(sub_atom(HTML, _, _, _, 'hx-push-url="true"')),
		once(sub_atom(HTML, _, _, _, 'hx-get="/activity/metrics"')),
		once(sub_atom(HTML, _, _, _, 'hx-trigger="metrics_loaded from:body"')),
		once(sub_atom(HTML, _, _, _, 'hx-boost="true"')),
		once(sub_atom(HTML, _, _, _, 'id="panel-slot"')).

	test(http_htmx_panel_handler_02, deterministic) :-
		request(get, origin('/panel'), http(1, 1), [], empty, [], Request),
		htmx_panel_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		\+ header(Response, hx_trigger, _),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<!DOCTYPE html>')),
		once(sub_atom(HTML, _, _, _, 'ordinary request')),
		once(sub_atom(HTML, _, _, _, 'Overview panel')).

	test(http_htmx_panel_handler_03, deterministic) :-
		request(get, origin('/panel'), http(1, 1), [hx_request-'true'], empty, [], Request),
		htmx_panel_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, hx_trigger, overview_loaded),
		body(Response, content('text/html', text(HTML))),
		\+ sub_atom(HTML, _, _, _, '<!DOCTYPE html>'),
		once(sub_atom(HTML, _, _, _, 'non-boosted HTMX request')),
		once(sub_atom(HTML, _, _, _, 'Overview panel')).

	test(http_htmx_panel_handler_04, deterministic) :-
		request(get, origin('/panel'), http(1, 1), [hx_request-'true', hx_boosted-'true'], empty, [], Request),
		htmx_panel_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, hx_trigger, overview_loaded),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<!DOCTYPE html>')),
		once(sub_atom(HTML, _, _, _, 'boosted HTMX request')),
		once(sub_atom(HTML, _, _, _, 'Overview panel')).

	test(http_htmx_panel_handler_05, deterministic) :-
		request(get, origin('/panel/metrics'), http(1, 1), [hx_request-'true'], empty, [], Request),
		htmx_panel_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, hx_trigger, metrics_loaded),
		body(Response, content('text/html', text(HTML))),
		\+ sub_atom(HTML, _, _, _, '<!DOCTYPE html>'),
		once(sub_atom(HTML, _, _, _, 'Metrics panel')),
		once(sub_atom(HTML, _, _, _, 'HX-Trigger relays refresh the side rail without inline scripting.')).

	test(http_htmx_panel_handler_06, deterministic) :-
		request(get, origin('/activity/metrics'), http(1, 1), [hx_request-'true'], empty, [], Request),
		htmx_panel_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		\+ header(Response, hx_trigger, _),
		body(Response, content('text/html', text(HTML))),
		\+ sub_atom(HTML, _, _, _, '<!DOCTYPE html>'),
		once(sub_atom(HTML, _, _, _, 'Activity rail updated')),
		once(sub_atom(HTML, _, _, _, 'Metrics panel')).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_htmx_panel_client_01, deterministic) :-
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(htmx_panel_server::serve_listener(Listener, 4), Tag),
			catch(
				htmx_panel_client::run(Port, result(HomeResponse, PanelPageResponse, PanelFragmentResponse, PanelBoostedResponse)),
				Error,
				(  cleanup_server_thread(Listener, Tag),
					throw(Error)
				)
			),
			http_socket_transport::request_listener_shutdown(Listener),
			threaded_exit(htmx_panel_server::serve_listener(Listener, 4), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			status(HomeResponse, status(200, 'OK')),
			body(HomeResponse, content('text/html', text(HomeHTML))),
			once(sub_atom(HomeHTML, _, _, _, 'Show metrics panel')),
			status(PanelPageResponse, status(200, 'OK')),
			body(PanelPageResponse, content('text/html', text(PanelPageHTML))),
			once(sub_atom(PanelPageHTML, _, _, _, '<!DOCTYPE html>')),
			status(PanelFragmentResponse, status(200, 'OK')),
			header(PanelFragmentResponse, hx_trigger, overview_loaded),
			body(PanelFragmentResponse, content('text/html', text(PanelFragmentHTML))),
			\+ sub_atom(PanelFragmentHTML, _, _, _, '<!DOCTYPE html>'),
			status(PanelBoostedResponse, status(200, 'OK')),
			header(PanelBoostedResponse, hx_trigger, overview_loaded),
			body(PanelBoostedResponse, content('text/html', text(PanelBoostedHTML))),
			once(sub_atom(PanelBoostedHTML, _, _, _, '<!DOCTYPE html>')).

		test(http_htmx_panel_demo_01, deterministic) :-
			http_htmx_panel_demo::run(result(HomeResponse, PanelPageResponse, PanelFragmentResponse, PanelBoostedResponse)),
			status(HomeResponse, status(200, 'OK')),
			status(PanelPageResponse, status(200, 'OK')),
			status(PanelFragmentResponse, status(200, 'OK')),
			header(PanelFragmentResponse, hx_trigger, overview_loaded),
			status(PanelBoostedResponse, status(200, 'OK')),
			header(PanelBoostedResponse, hx_trigger, overview_loaded).

		cleanup_server_thread(Listener, Tag) :-
			http_socket_transport::request_listener_shutdown(Listener),
			catch(threaded_exit(htmx_panel_server::serve_listener(Listener, 4), Tag), _, true),
			catch(http_socket_transport::close_listener(Listener), _, true).

	:- endif.

:- end_object.
