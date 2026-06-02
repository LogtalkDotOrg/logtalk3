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
		date is 2026-05-26,
		comment is 'Unit tests for the ``http_htmx`` library.'
	]).

	:- uses(http_core, [
		body/2, header/3, request/7, response/6, status/2
	]).

	cover(http_htmx).

	test(http_htmx_is_htmx_request_1_01, true) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, []),
		http_htmx::is_htmx_request(Request).

	test(http_htmx_is_fragment_request_1_01, true) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, []),
		http_htmx::is_fragment_request(Request).

	test(http_htmx_is_fragment_request_1_02, fail) :-
		Request = request(get, origin('/items/1'), http(1, 1), [], empty, []),
		http_htmx::is_fragment_request(Request).

	test(http_htmx_is_fragment_request_1_03, fail) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_boosted-'true'], empty, []),
		http_htmx::is_fragment_request(Request).

	test(http_htmx_is_fragment_request_1_04, fail) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_history_restore_request-'true'], empty, []),
		http_htmx::is_fragment_request(Request).

	test(http_htmx_request_kind_2_01, deterministic(Kind == ordinary)) :-
		Request = request(get, origin('/items/1'), http(1, 1), [], empty, []),
		http_htmx::request_kind(Request, Kind).

	test(http_htmx_request_kind_2_02, deterministic(Kind == fragment)) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, []),
		http_htmx::request_kind(Request, Kind).

	test(http_htmx_request_kind_2_03, deterministic(Kind == boosted)) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_boosted-'true'], empty, []),
		http_htmx::request_kind(Request, Kind).

	test(http_htmx_request_kind_2_04, deterministic(Kind == history_restore)) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_history_restore_request-'true'], empty, []),
		http_htmx::request_kind(Request, Kind).

	test(http_htmx_request_kind_2_05, deterministic(Kind == history_restore)) :-
		Request = request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_boosted-'true', hx_history_restore_request-'true'], empty, []),
		http_htmx::request_kind(Request, Kind).

	test(http_htmx_current_url_abs_path_2_01, deterministic(AbsolutePath == '/items/1?draft=true')) :-
		request(
			get,
			origin('/items/1'),
			http(1, 1),
			[host-host('example.com'), hx_current_url-'https://example.com:443/items/1?draft=true#editor'],
			empty,
			[scheme(https)],
			Request
		),
		http_htmx::current_url_abs_path(Request, AbsolutePath).

	test(http_htmx_current_url_abs_path_2_02, fail) :-
		request(
			get,
			origin('/items/1'),
			http(1, 1),
			[host-host('example.com'), hx_current_url-'https://other.example.com/items/1?draft=true'],
			empty,
			[scheme(https)],
			Request
		),
		http_htmx::current_url_abs_path(Request, _AbsolutePath).

	test(http_htmx_current_url_abs_path_2_03, fail) :-
		request(
			get,
			origin('/items/1'),
			http(1, 1),
			[host-host('example.com'), hx_current_url-'https://example.com/items/1?draft=true'],
			empty,
			[],
			Request
		),
		http_htmx::current_url_abs_path(Request, _AbsolutePath).

	test(http_htmx_request_properties_2_01, deterministic(Properties == [htmx_request(true), htmx_request_kind(history_restore), htmx_boosted(true), htmx_target(panel), htmx_trigger('save-button'), htmx_trigger_name(save), htmx_current_url('https://example.com/items/1'), htmx_current_url_abs_path('/items/1'), htmx_prompt('Rename?'), htmx_history_restore_request(true)])) :-
		request(
			get,
			origin('/items/1'),
			http(1, 1),
			[
				host-host('example.com'),
				hx_request-'true',
				hx_boosted-'true',
				hx_target-panel,
				hx_trigger-'save-button',
				hx_trigger_name-save,
				hx_current_url-'https://example.com/items/1',
				hx_prompt-'Rename?',
				hx_history_restore_request-'true'
			],
			empty,
			[scheme(https)],
			Request
		),
		http_htmx::request_properties(Request, Properties).

	test(http_htmx_request_properties_2_02, deterministic(Properties == [htmx_request_kind(ordinary)])) :-
		Request = request(get, origin('/items/1'), http(1, 1), [], empty, []),
		http_htmx::request_properties(Request, Properties).

	test(http_htmx_request_property_2_01, deterministic) :-
		Request = request(
			get,
			origin('/items/1'),
			http(1, 1),
			[
				hx_request-'true',
				hx_target-panel,
				hx_history_restore_request-'true'
			],
			empty,
			[]
		),
		http_htmx::request_property(Request, htmx_request(true)),
		http_htmx::request_property(Request, htmx_request_kind(history_restore)),
		http_htmx::request_property(Request, htmx_target(panel)),
		http_htmx::request_property(Request, htmx_history_restore_request(true)),
		\+ http_htmx::request_property(Request, htmx_boosted(true)).

	test(http_htmx_request_property_2_02, deterministic) :-
		Request = request(get, origin('/items/1'), http(1, 1), [], empty, []),
		http_htmx::request_property(Request, htmx_request_kind(ordinary)).

	test(http_htmx_request_accessors_2_02, deterministic) :-
		atom_codes('true', BoostedCodes),
		atom_chars('true', HistoryChars),
		atom_codes('https://example.com/items/1', URLCodes),
		atom_chars('Rename?', PromptChars),
		request(
			get,
			origin('/items/1'),
			http(1, 1),
			[
				host-host('example.com'),
				hx_boosted-BoostedCodes,
				hx_history_restore_request-HistoryChars,
				hx_current_url-URLCodes,
				hx_prompt-PromptChars,
				hx_target-panel,
				hx_trigger-'save-button',
				hx_trigger_name-save
			],
			empty,
			[scheme(https)],
			Request
		),
		http_htmx::is_boosted_request(Request),
		http_htmx::is_history_restore_request(Request),
		http_htmx::current_url(Request, 'https://example.com/items/1'),
		http_htmx::current_url_abs_path(Request, '/items/1'),
		http_htmx::prompt(Request, 'Rename?'),
		http_htmx::target(Request, panel),
		http_htmx::trigger(Request, 'save-button'),
		http_htmx::trigger_name(Request, save).

	test(http_htmx_reply_3_01, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', Response),
		status(Response, status(200, 'OK')),
		\+ header(Response, hx_retarget, _),
		body(Response, content('text/html', text('<div>item</div>'))).

	test(http_htmx_reply_3_02, error(domain_error(http_htmx_content, foo(bar)))) :-
		request(get, origin('/items/1'), http(1, 1), [], empty, [], Request),
		http_htmx::reply(Request, foo(bar), _Response).

	test(http_htmx_reply_4_01, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, div('ok'), Response, [retarget('#panel'), trigger(saved)]),
		status(Response, status(200, 'OK')),
		header(Response, hx_retarget, '#panel'),
		header(Response, hx_trigger, saved),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<div>')),
		once(sub_atom(HTML, _, _, _, 'ok')).

	test(http_htmx_reply_4_02, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', Response, [trigger([saved, refreshed])]),
		status(Response, status(200, 'OK')),
		header(Response, hx_trigger, 'saved, refreshed').

	test(http_htmx_page_fragment_reply_4_01, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [], empty, [], Request),
		http_htmx::page_fragment_reply(Request, html([head(title('Item')), body(div('page'))]), div('fragment'), Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<!DOCTYPE html>')),
		once(sub_atom(HTML, _, _, _, '<html>')).

	test(http_htmx_page_fragment_reply_4_02, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::page_fragment_reply(Request, html([head(title('Item')), body(div('page'))]), div('fragment'), Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		\+ sub_atom(HTML, _, _, _, '<!DOCTYPE html>'),
		once(sub_atom(HTML, _, _, _, 'fragment')).

	test(http_htmx_page_fragment_reply_4_03, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_boosted-'true'], empty, [], Request),
		http_htmx::page_fragment_reply(Request, html([head(title('Item')), body(div('page'))]), div('fragment'), Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<!DOCTYPE html>')),
		once(sub_atom(HTML, _, _, _, '<html>')).

	test(http_htmx_page_fragment_reply_4_04, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true', hx_history_restore_request-'true'], empty, [], Request),
		http_htmx::page_fragment_reply(Request, html([head(title('Item')), body(div('page'))]), div('fragment'), Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<!DOCTYPE html>')),
		once(sub_atom(HTML, _, _, _, '<html>')).

	test(http_htmx_add_response_headers_4_01, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		response(http(1, 1), status(200, 'OK'), [], content('text/html', text('<div>ok</div>')), [], Response0),
		http_htmx::add_response_headers(Request, Response0, Response, [location({path-'/items/1', target-'#panel'}), trigger_after_swap({saved-{id-1}})]),
		once(header(Response, hx_location, Location)),
		once(sub_atom(Location, _, _, _, '"path":"/items/1"')),
		once(sub_atom(Location, _, _, _, '"target":"#panel"')),
		once(header(Response, hx_trigger_after_swap, TriggerAfterSwap)),
		once(sub_atom(TriggerAfterSwap, _, _, _, '"saved"')),
		once(sub_atom(TriggerAfterSwap, _, _, _, '"id":1')).

	test(http_htmx_add_response_headers_4_02, error(domain_error(htmx_response_headers_status, status(302, 'Found')))) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		response(http(1, 1), status(302, 'Found'), [], content('text/html', text('<div>ok</div>')), [], Response0),
		http_htmx::add_response_headers(Request, Response0, _Response, [trigger(saved)]).

	test(http_htmx_add_response_headers_4_03, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		response(http(1, 1), status(200, 'OK'), [], content('text/html', text('<div>ok</div>')), [], Response0),
		http_htmx::add_response_headers(Request, Response0, Response, [location({path-'/items/2', target-'#panel', swap-'innerHTML', select-'#item', push-false, replace-'/items/2', source-'link-1', event-click, handler-'swap-handler', values-{page-1}, headers-{accept-'text/html'}})]),
		once(header(Response, hx_location, Location)),
		once(sub_atom(Location, _, _, _, '"path":"/items/2"')),
		once(sub_atom(Location, _, _, _, '"swap":"innerHTML"')),
		once(sub_atom(Location, _, _, _, '"push":false')),
		once(sub_atom(Location, _, _, _, '"values":{"page":1}')),
		once(sub_atom(Location, _, _, _, '"headers":{"accept":"text/html"}')).

	test(http_htmx_add_response_headers_4_04, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		response(http(1, 1), status(200, 'OK'), [etag-'keep', hx_push_url-'old-push', hx_replace_url-'old-replace'], content('text/html', text('<div>ok</div>')), [], Response0),
		http_htmx::add_response_headers(Request, Response0, Response, [push_url(false), replace_url('/items/2')]),
		header(Response, etag, 'keep'),
		header(Response, hx_push_url, 'false'),
		header(Response, hx_replace_url, '/items/2'),
		\+ header(Response, hx_push_url, 'old-push'),
		\+ header(Response, hx_replace_url, 'old-replace').

	test(http_htmx_add_response_headers_4_05, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [], empty, [], Request),
		response(http(1, 1), status(200, 'OK'), [cache_control-'public'], content('text/html', text('<div>ok</div>')), [], Response0),
		http_htmx::add_response_headers(Request, Response0, Response, [vary_hx_request(true)]),
		header(Response, vary, 'HX-Request'),
		header(Response, cache_control, 'public'),
		\+ header(Response, hx_trigger, _).

	test(http_htmx_reply_4_09, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', Response, [headers([vary-'Accept-Encoding']), vary_hx_request(true), push_url(false)]),
		header(Response, vary, 'Accept-Encoding, HX-Request'),
		header(Response, hx_push_url, 'false').

	test(http_htmx_reply_4_03, error(domain_error(option, retarget(_)))) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', _Response, [retarget(_)]).

	test(http_htmx_reply_4_04, error(domain_error(option, location({target-'#panel'})))) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', _Response, [location({target-'#panel'})]).

	test(http_htmx_reply_4_05, error(domain_error(option, location({path-'/items/1', taget-'#panel'})))) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', _Response, [location({path-'/items/1', taget-'#panel'})]).

	test(http_htmx_reply_4_06, error(domain_error(option, trigger(1)))) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', _Response, [trigger(1)]).

	test(http_htmx_reply_4_07, error(domain_error(htmx_response_headers_status, status(302, 'Found')))) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', _Response, [status(status(302, 'Found')), redirect('/next')]).

	test(http_htmx_reply_4_08, deterministic) :-
		request(get, origin('/items/1'), http(1, 1), [hx_request-'true'], empty, [], Request),
		http_htmx::reply(Request, '<div>item</div>', Response, [headers([etag-'abc', cache_control-'no-store']), refresh(false)]),
		header(Response, etag, 'abc'),
		header(Response, cache_control, 'no-store'),
		\+ header(Response, hx_refresh, _).

	test(http_router_htmx_handle_2_01, deterministic) :-
		request(get, origin('/panel'), http(1, 1), [hx_request-'true'], empty, [], Request),
		sample_htmx_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, hx_retarget, '#panel'),
		header(Response, hx_trigger, saved),
		body(Response, content('text/html', text('<div>htmx-panel</div>'))).

	test(http_router_htmx_handle_2_02, deterministic) :-
		request(get, origin('/panel'), http(1, 1), [], empty, [], Request),
		sample_htmx_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		\+ header(Response, hx_retarget, _),
		\+ header(Response, hx_trigger, _),
		body(Response, content('text/html', text('<div>plain-panel</div>'))).

:- end_object.
