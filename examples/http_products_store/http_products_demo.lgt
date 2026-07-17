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


:- object(http_products_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-14,
		comment is 'Starts a local HTTP server and drives it with a few client requests to illustrate the full request/response round trip. Requires threads support.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Starts a local server, performs a handful of client requests against it, prints the results, and stops the server.'
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	run :-
		write('Starting the http_products_api demo server...'), nl, nl,
		http_server::start('127.0.0.1', Port, http_products_api, Server, []),
		atomic_concat('http://127.0.0.1:', Port, BaseURL),
		atom_concat(BaseURL, '/products', ListURL),
		atom_concat(BaseURL, '/products/1', ShowURL),
		atom_concat(BaseURL, '/products/999', MissingURL),

		write('GET /products (default filters, page 1):'), nl,
		http_client::get(ListURL, ListResponse, []),
		print_response(ListResponse), nl,

		write('GET /products?category=peripherals&status=active&per_page=2:'), nl,
		http_client::get(
			ListURL, FilteredResponse,
			[query([category-peripherals, status-active, per_page-'2'])]
		),
		print_response(FilteredResponse), nl,

		write('POST /products (create a new product):'), nl,
		http_client::post(
			ListURL,
			content('application/x-www-form-urlencoded', form([name-'Desk Lamp', category-accessories, price-'39.90'])),
			CreateResponse,
			[]
		),
		print_response(CreateResponse), nl,

		write('GET /products/1 (existing product):'), nl,
		http_client::get(ShowURL, ShowResponse, []),
		print_response(ShowResponse), nl,

		write('GET /products/999 (missing product):'), nl,
		http_client::get(MissingURL, MissingResponse, []),
		print_response(MissingResponse), nl,

		write('GET /products?status=bogus (fails parameter validation):'), nl,
		http_client::get(ListURL, InvalidResponse, [query([status-bogus])]),
		print_response(InvalidResponse), nl,

		http_server::stop(Server),
		write('Server stopped.'), nl.

	print_response(Response) :-
		status(Response, status(Code, Reason)),
		body(Response, Body),
		write('  status: '), writeq(Code-Reason), nl,
		write('  body:   '), writeq(Body), nl.

:- end_object.
