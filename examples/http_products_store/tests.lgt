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
		date is 2026-07-14,
		comment is 'Unit tests for the "products_api" HTTP example.'
	]).

	:- uses(http_core, [
		body/2, header/3, status/2
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(http_products_api).
	cover(http_products_store).

	% these tests dispatch synthetic request terms directly to
	% products_api::handle/2, so they require no sockets and run
	% on every backend

	% GET /products

	test(http_products_api_list_products_defaults, deterministic(Page-PerPage == 1-5)) :-
		Request = request(get, origin('/products'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({page-Page, per_page-PerPage, results-_Results}))).

	test(http_products_api_list_products_filtered, deterministic(Names == ['Mechanical Keyboard', 'Wireless Mouse'])) :-
		Request = request(get, origin('/products', 'category=peripherals&status=active'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({page-1, per_page-5, results-Results}))),
		findall(Name, member({id-_, name-Name, category-peripherals, status-active, price-_}, Results), Names).

	test(http_products_api_list_products_query, deterministic(Names == ['Wireless Mouse'])) :-
		Request = request(get, origin('/products', 'q=Wireless'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({page-1, per_page-5, results-Results}))),
		findall(Name, member({id-_, name-Name, category-_, status-_, price-_}, Results), Names).

	test(http_products_api_list_products_pagination, deterministic(Names == ['USB-C Hub'])) :-
		Request = request(get, origin('/products', 'per_page=1&page=3'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({page-3, per_page-1, results-Results}))),
		findall(Name, member({id-_, name-Name, category-_, status-_, price-_}, Results), Names).

	test(http_products_api_list_products_invalid_status, deterministic) :-
		Request = request(get, origin('/products', 'status=bogus'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(400, 'Bad Request')).

	test(http_products_api_list_products_page_out_of_range, deterministic) :-
		Request = request(get, origin('/products', 'page=0'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(400, 'Bad Request')).

	% GET /products/{id}

	test(http_products_api_show_product_found, deterministic(Name-Price == 'Notebook Sleeve'-19.90)) :-
		Request = request(get, origin('/products/1'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-1, name-Name, category-accessories, status-active, price-Price}))).

	test(http_products_api_show_product_missing, deterministic) :-
		Request = request(get, origin('/products/999'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(404, 'Not Found')).

	test(http_products_api_show_product_not_routed, deterministic) :-
		% "abc" does not satisfy the "{id:integer}" typed placeholder, so
		% the route itself does not match: 404, not a 400 validation error
		Request = request(get, origin('/products/abc'), http(1, 1), [], empty, []),
		http_products_api::handle(Request, Response),
		status(Response, status(404, 'Not Found')).

	% POST /products

	test(http_products_api_create_product, deterministic(Name-Category == 'Desk Lamp'-accessories)) :-
		Request = request(
			post, origin('/products'), http(1, 1), [],
			content('application/x-www-form-urlencoded', form([name-'Desk Lamp', category-accessories, price-'39.90'])),
			[]
		),
		http_products_api::handle(Request, Response),
		status(Response, status(201, 'Created')),
		once(header(Response, location, _Location)),
		body(Response, content('application/json', json({id-_Id, name-Name, category-Category, status-active, price-39.90}))).

	test(http_products_api_create_product_missing_field, deterministic) :-
		Request = request(
			post, origin('/products'), http(1, 1), [],
			content('application/x-www-form-urlencoded', form([name-'Desk Lamp', category-accessories])),
			[]
		),
		http_products_api::handle(Request, Response),
		status(Response, status(400, 'Bad Request')).

	% end-to-end round trip through real "http_server" and "http_client"
	% sockets; skipped on backends without threading support

	test(http_products_api_http_round_trip, deterministic(Names == ['Mechanical Keyboard']), [condition(current_logtalk_flag(threads, supported))]) :-
		http_server::start('127.0.0.1', Port, http_products_api, Server, []),
		atomic_list_concat(['http://127.0.0.1:', Port, '/products'], URL),
		http_client::get(
			URL, Response,
			[query([category-peripherals, status-active, per_page-'1'])]
		),
		http_server::stop(Server),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({page-1, per_page-1, results-Results}))),
		findall(Name, member({id-_, name-Name, category-peripherals, status-active, price-_}, Results), Names).

:- end_object.
