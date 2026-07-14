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


:- object(http_products_store).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-14,
		comment is 'In-memory sample products catalog.'
	]).

	:- public(list_products/6).
	:- mode(list_products(?atom, +list(atom), ?atom, +integer, +integer, -list(compound)), one).
	:- info(list_products/6, [
		comment is 'Returns the page of catalog products matching the given filters. A variable ``Query``, an empty ``Categories`` list, or a variable ``Status`` skips that particular filter.',
		argnames is ['Query', 'Categories', 'Status', 'Page', 'PerPage', 'Products']
	]).

	:- public(find_product/2).
	:- mode(find_product(+integer, -compound), zero_or_one).
	:- info(find_product/2, [
		comment is 'Looks up a single catalog product by identifier.',
		argnames is ['Id', 'Product']
	]).

	:- public(create_product/4).
	:- mode(create_product(+atom, +atom, +number, -integer), one).
	:- info(create_product/4, [
		comment is 'Adds a new product to the catalog, returning its generated identifier.',
		argnames is ['Name', 'Category', 'Price', 'Id']
	]).

	:- uses(list, [
		drop/3, memberchk/2
	]).

	:- private(product_/5).
	:- dynamic(product_/5).
	:- mode(product_(?integer, ?atom, ?atom, ?atom, ?number), zero_or_more).
	:- info(product_/5, [
		comment is 'Product table.',
		argnames is ['Id', 'Name', 'Category', 'Status', 'Price']
	]).

	:- private(next_id_/1).
	:- dynamic(next_id_/1).
	:- mode(next_id_(?integer), zero_or_one).
	:- info(next_id_/1, [
		comment is 'Next id to be used when creating a new product.',
		argnames is ['Id']
	]).

	% seed catalog: product_(Id, Name, Category, Status, Price)
	product_(1, 'Notebook Sleeve', accessories, active, 19.90).
	product_(2, 'Mechanical Keyboard', peripherals, active, 79.00).
	product_(3, 'USB-C Hub', peripherals, discontinued, 24.50).
	product_(4, 'Standing Desk Mat', accessories, active, 34.90).
	product_(5, 'Wireless Mouse', peripherals, active, 29.90).

	next_id_(6).

	list_products(Query, Categories, Status, Page, PerPage, Products) :-
		findall(
			{id-Id, name-Name, category-Category, status-Status0, price-Price},
			(	product_(Id, Name, Category, Status0, Price),
				matches_query(Query, Name),
				matches_category(Categories, Category),
				matches_status(Status, Status0)
			),
			Matches
		),
		paginate(Matches, Page, PerPage, Products).

	find_product(Id, {id-Id, name-Name, category-Category, status-Status, price-Price}) :-
		product_(Id, Name, Category, Status, Price).

	create_product(Name, Category, Price, Id) :-
		retract(next_id_(Id)),
		NextId is Id + 1,
		assertz(next_id_(NextId)),
		assertz(product_(Id, Name, Category, active, Price)).

	% filtering helpers; an unbound filter argument matches everything

	matches_query(Query, _Name) :-
		var(Query),
		!.
	matches_query(Query, Name) :-
		sub_atom(Name, _, _, _, Query).

	matches_category([], _Category) :-
		!.
	matches_category(Categories, Category) :-
		memberchk(Category, Categories).

	matches_status(Status, _Status0) :-
		var(Status),
		!.
	matches_status(Status, Status).

	% pagination helpers

	paginate(Matches, Page, PerPage, Products) :-
		Skip is (Page - 1) * PerPage,
		drop(Skip, Matches, Remaining),
		take_up_to(PerPage, Remaining, Products).

	take_up_to(0, _Remaining, []) :-
		!.
	take_up_to(_Count, [], []) :-
		!.
	take_up_to(Count, [Product| Remaining], [Product| Products]) :-
		NextCount is Count - 1,
		take_up_to(NextCount, Remaining, Products).

:- end_object.
