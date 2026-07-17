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


:- object(http_products_api,
	implements(http_handler_protocol),
	imports(http_router_parameters)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-17,
		comment is 'Sample REST API illustrating the "http_router", "http_parameters", "http_server", and "http_client" libraries working together.'
	]).

	:- protected(list_products/2).
	:- info(list_products/2, [
		comment is 'Route handler for ``GET /products``.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_product/2).
	:- info(show_product/2, [
		comment is 'Route handler for ``GET /products/{id}``.',
		argnames is ['Request', 'Response']
	]).

	:- protected(create_product/2).
	:- info(create_product/2, [
		comment is 'Route handler for ``POST /products``.',
		argnames is ['Request', 'Response']
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	% route(Id, Method, PathTemplate, Handler)

	route(list_products,  get,  '/products',              list_products).
	route(show_product,   get,  '/products/{id:integer}', show_product).
	route(create_product, post, '/products',              create_product).

	% parameter declarations, reused for extraction and OpenAPI generation

	route_parameter_declarations(list_products, [
		parameter(q, query, string, [
			optional, description('Case-sensitive substring match on the product name')
		]),
		parameter(category, query, list(atom), [
			optional, description('Restrict results to one or more categories')
		]),
		parameter(status, query, atom, [
			optional, enum([active, discontinued]), description('Restrict results to a product status')
		]),
		parameter(page, query, integer, [
			default(1), minimum(1), description('Page number, starting at 1')
		]),
		parameter(per_page, query, integer, [
			default(5), minimum(1), maximum(50), description('Number of results per page')
		])
	]).
	route_parameter_declarations(show_product, [
		parameter(id, path, integer, [description('Product identifier')])
	]).
	route_parameter_declarations(create_product, [
		parameter(name, form, string, [description('Product name')]),
		parameter(category, form, atom, [description('Product category')]),
		parameter(price, form, number, [minimum(0), description('Product price')])
	]).

	% route handlers

	list_products(Request, Response) :-
		::route_parameters(Request, Parameters),
		list_filter(q, Parameters, _, Query),
		list_filter(category, Parameters, [], Categories),
		list_filter(status, Parameters, _, Status),
		memberchk(page-Page, Parameters),
		memberchk(per_page-PerPage, Parameters),
		http_products_store::list_products(Query, Categories, Status, Page, PerPage, Products),
		http_core::version(Request, Version),
		http_core::response(
			Version, status(200, 'OK'), [],
			content('application/json', json({page-Page, per_page-PerPage, results-Products})),
			[], Response
		).

	show_product(Request, Response) :-
		::route_parameters(Request, Parameters),
		memberchk(id-Id, Parameters),
		http_core::version(Request, Version),
		(	http_products_store::find_product(Id, Product) ->
			http_core::response(
				Version, status(200, 'OK'), [],
				content('application/json', json(Product)),
				[], Response
			)
		;	http_core::response(
				Version, status(404, 'Not Found'), [],
				content('application/json', json({error-'product not found'})),
				[], Response
			)
		).

	create_product(Request, Response) :-
		::route_parameters(Request, Parameters),
		memberchk(name-Name, Parameters),
		memberchk(category-Category, Parameters),
		memberchk(price-Price, Parameters),
		http_products_store::create_product(Name, Category, Price, Id),
		atomic_concat('/products/', Id, Location),
		http_core::version(Request, Version),
		http_core::response(
			Version, status(201, 'Created'), [location-Location],
			content('application/json', json({id-Id, name-Name, category-Category, status-active, price-Price})),
			[], Response
		).

	% extraction helper: Value unifies with the declared parameter when
	% present in Parameters, or with Default (typically an unbound
	% variable, meaning "no filter") when the optional parameter is absent

	list_filter(Key, Parameters, Default, Value) :-
		(	member(Key-Value0, Parameters) ->
			Value = Value0
		;	Value = Default
		).

:- end_object.
