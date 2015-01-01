%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- protocol(numberlistp).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2011/12/15,
		comment is 'List of numbers protocol.'
	]).

	:- public(product/2).
	:- mode(product(+list(number), -number), zero_or_one).
	:- info(product/2, [
		comment is 'Calculates the product of all list numbers. Fails if the list is empty.',
		argnames is ['List', 'Product']
	]).

	:- public(sum/2).
	:- mode(sum(+list(number), -number), one).
	:- info(sum/2, [
		comment is 'Calculates the sum of all list numbers. Returns the integer zero if the list is empty.',
		argnames is ['List', 'Sum']
	]).

	:- public(average/2).
	:- mode(average(+list(number), -float), zero_or_one).
	:- info(average/2, [
		comment is 'Calculates the average (i.e. arithmetic mean) of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Average']
	]).

	:- public(euclidean_norm/2).
	:- mode(euclidean_norm(+list(number), -float), zero_or_one).
	:- info(euclidean_norm/2, [
		comment is 'Calculates the Euclidean norm of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Norm']
	]).

	:- public(chebyshev_norm/2).
	:- mode(chebyshev_norm(+list(integer), -integer), zero_or_one).
	:- mode(chebyshev_norm(+list(float), -float), zero_or_one).
	:- info(chebyshev_norm/2, [
		comment is 'Calculates the Chebyshev norm of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Norm']
	]).

	:- public(manhattan_norm/2).
	:- mode(manhattan_norm(+list(integer), -integer), zero_or_one).
	:- mode(manhattan_norm(+list(float), -float), zero_or_one).
	:- info(manhattan_norm/2, [
		comment is 'Calculates the Manhattan norm of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Norm']
	]).

	:- public(euclidean_distance/3).
	:- mode(euclidean_distance(+list(number), +list(number), -float), zero_or_one).
	:- info(euclidean_distance/3, [
		comment is 'Calculates the Euclidean distance between two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(chebyshev_distance/3).
	:- mode(chebyshev_distance(+list(integer), +list(integer), -integer), zero_or_one).
	:- mode(chebyshev_distance(+list(float), +list(float), -float), zero_or_one).
	:- info(chebyshev_distance/3, [
		comment is 'Calculates the Chebyshev distance between two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(manhattan_distance/3).
	:- mode(manhattan_distance(+list(integer), +list(integer), -integer), zero_or_one).
	:- mode(manhattan_distance(+list(float), +list(float), -float), zero_or_one).
	:- info(manhattan_distance/3, [
		comment is 'Calculates the Manhattan distance between two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(scalar_product/3).
	:- mode(scalar_product(+list(integer), +list(integer), -integer), zero_or_one).
	:- mode(scalar_product(+list(float), +list(float), -float), zero_or_one).
	:- info(scalar_product/3, [
		comment is 'Calculates the scalar product of two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Product']
	]).

:- end_protocol.
