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


:- object(iris,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-14,
		comment is 'Classic Iris flower dataset with 150 examples and 4 continuous attributes for classifying iris species. Originally from Fisher (1936).'
	]).

	attribute_values(sepal_length, continuous).
	attribute_values(sepal_width, continuous).
	attribute_values(petal_length, continuous).
	attribute_values(petal_width, continuous).

	class(species).

	class_values([setosa, versicolor, virginica]).

	example(  1, setosa,     [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.2]).
	example(  2, setosa,     [sepal_length-4.9, sepal_width-3.0, petal_length-1.4, petal_width-0.2]).
	example(  3, setosa,     [sepal_length-4.7, sepal_width-3.2, petal_length-1.3, petal_width-0.2]).
	example(  4, setosa,     [sepal_length-4.6, sepal_width-3.1, petal_length-1.5, petal_width-0.2]).
	example(  5, setosa,     [sepal_length-5.0, sepal_width-3.6, petal_length-1.4, petal_width-0.2]).
	example(  6, setosa,     [sepal_length-5.4, sepal_width-3.9, petal_length-1.7, petal_width-0.4]).
	example(  7, setosa,     [sepal_length-4.6, sepal_width-3.4, petal_length-1.4, petal_width-0.3]).
	example(  8, setosa,     [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2]).
	example(  9, setosa,     [sepal_length-4.4, sepal_width-2.9, petal_length-1.4, petal_width-0.2]).
	example( 10, setosa,     [sepal_length-4.9, sepal_width-3.1, petal_length-1.5, petal_width-0.1]).
	example( 11, setosa,     [sepal_length-5.4, sepal_width-3.7, petal_length-1.5, petal_width-0.2]).
	example( 12, setosa,     [sepal_length-4.8, sepal_width-3.4, petal_length-1.6, petal_width-0.2]).
	example( 13, setosa,     [sepal_length-4.8, sepal_width-3.0, petal_length-1.4, petal_width-0.1]).
	example( 14, setosa,     [sepal_length-4.3, sepal_width-3.0, petal_length-1.1, petal_width-0.1]).
	example( 15, setosa,     [sepal_length-5.8, sepal_width-4.0, petal_length-1.2, petal_width-0.2]).
	example( 16, setosa,     [sepal_length-5.7, sepal_width-4.4, petal_length-1.5, petal_width-0.4]).
	example( 17, setosa,     [sepal_length-5.4, sepal_width-3.9, petal_length-1.3, petal_width-0.4]).
	example( 18, setosa,     [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.3]).
	example( 19, setosa,     [sepal_length-5.7, sepal_width-3.8, petal_length-1.7, petal_width-0.3]).
	example( 20, setosa,     [sepal_length-5.1, sepal_width-3.8, petal_length-1.5, petal_width-0.3]).
	example( 21, setosa,     [sepal_length-5.4, sepal_width-3.4, petal_length-1.7, petal_width-0.2]).
	example( 22, setosa,     [sepal_length-5.1, sepal_width-3.7, petal_length-1.5, petal_width-0.4]).
	example( 23, setosa,     [sepal_length-4.6, sepal_width-3.6, petal_length-1.0, petal_width-0.2]).
	example( 24, setosa,     [sepal_length-5.1, sepal_width-3.3, petal_length-1.7, petal_width-0.5]).
	example( 25, setosa,     [sepal_length-4.8, sepal_width-3.4, petal_length-1.9, petal_width-0.2]).
	example( 26, setosa,     [sepal_length-5.0, sepal_width-3.0, petal_length-1.6, petal_width-0.2]).
	example( 27, setosa,     [sepal_length-5.0, sepal_width-3.4, petal_length-1.6, petal_width-0.4]).
	example( 28, setosa,     [sepal_length-5.2, sepal_width-3.5, petal_length-1.5, petal_width-0.2]).
	example( 29, setosa,     [sepal_length-5.2, sepal_width-3.4, petal_length-1.4, petal_width-0.2]).
	example( 30, setosa,     [sepal_length-4.7, sepal_width-3.2, petal_length-1.6, petal_width-0.2]).
	example( 31, setosa,     [sepal_length-4.8, sepal_width-3.1, petal_length-1.6, petal_width-0.2]).
	example( 32, setosa,     [sepal_length-5.4, sepal_width-3.4, petal_length-1.5, petal_width-0.4]).
	example( 33, setosa,     [sepal_length-5.2, sepal_width-4.1, petal_length-1.5, petal_width-0.1]).
	example( 34, setosa,     [sepal_length-5.5, sepal_width-4.2, petal_length-1.4, petal_width-0.2]).
	example( 35, setosa,     [sepal_length-4.9, sepal_width-3.1, petal_length-1.5, petal_width-0.1]).
	example( 36, setosa,     [sepal_length-5.0, sepal_width-3.2, petal_length-1.2, petal_width-0.2]).
	example( 37, setosa,     [sepal_length-5.5, sepal_width-3.5, petal_length-1.3, petal_width-0.2]).
	example( 38, setosa,     [sepal_length-4.9, sepal_width-3.1, petal_length-1.5, petal_width-0.1]).
	example( 39, setosa,     [sepal_length-4.4, sepal_width-3.0, petal_length-1.3, petal_width-0.2]).
	example( 40, setosa,     [sepal_length-5.1, sepal_width-3.4, petal_length-1.5, petal_width-0.2]).
	example( 41, setosa,     [sepal_length-5.0, sepal_width-3.5, petal_length-1.3, petal_width-0.3]).
	example( 42, setosa,     [sepal_length-4.5, sepal_width-2.3, petal_length-1.3, petal_width-0.3]).
	example( 43, setosa,     [sepal_length-4.4, sepal_width-3.2, petal_length-1.3, petal_width-0.2]).
	example( 44, setosa,     [sepal_length-5.0, sepal_width-3.5, petal_length-1.6, petal_width-0.6]).
	example( 45, setosa,     [sepal_length-5.1, sepal_width-3.8, petal_length-1.9, petal_width-0.4]).
	example( 46, setosa,     [sepal_length-4.8, sepal_width-3.0, petal_length-1.4, petal_width-0.3]).
	example( 47, setosa,     [sepal_length-5.1, sepal_width-3.8, petal_length-1.6, petal_width-0.2]).
	example( 48, setosa,     [sepal_length-4.6, sepal_width-3.2, petal_length-1.4, petal_width-0.2]).
	example( 49, setosa,     [sepal_length-5.3, sepal_width-3.7, petal_length-1.5, petal_width-0.2]).
	example( 50, setosa,     [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2]).

	example( 51, versicolor, [sepal_length-7.0, sepal_width-3.2, petal_length-4.7, petal_width-1.4]).
	example( 52, versicolor, [sepal_length-6.4, sepal_width-3.2, petal_length-4.5, petal_width-1.5]).
	example( 53, versicolor, [sepal_length-6.9, sepal_width-3.1, petal_length-4.9, petal_width-1.5]).
	example( 54, versicolor, [sepal_length-5.5, sepal_width-2.3, petal_length-4.0, petal_width-1.3]).
	example( 55, versicolor, [sepal_length-6.5, sepal_width-2.8, petal_length-4.6, petal_width-1.5]).
	example( 56, versicolor, [sepal_length-5.7, sepal_width-2.8, petal_length-4.5, petal_width-1.3]).
	example( 57, versicolor, [sepal_length-6.3, sepal_width-3.3, petal_length-4.7, petal_width-1.6]).
	example( 58, versicolor, [sepal_length-4.9, sepal_width-2.4, petal_length-3.3, petal_width-1.0]).
	example( 59, versicolor, [sepal_length-6.6, sepal_width-2.9, petal_length-4.6, petal_width-1.3]).
	example( 60, versicolor, [sepal_length-5.2, sepal_width-2.7, petal_length-3.9, petal_width-1.4]).
	example( 61, versicolor, [sepal_length-5.0, sepal_width-2.0, petal_length-3.5, petal_width-1.0]).
	example( 62, versicolor, [sepal_length-5.9, sepal_width-3.0, petal_length-4.2, petal_width-1.5]).
	example( 63, versicolor, [sepal_length-6.0, sepal_width-2.2, petal_length-4.0, petal_width-1.0]).
	example( 64, versicolor, [sepal_length-6.1, sepal_width-2.9, petal_length-4.7, petal_width-1.4]).
	example( 65, versicolor, [sepal_length-5.6, sepal_width-2.9, petal_length-3.6, petal_width-1.3]).
	example( 66, versicolor, [sepal_length-6.7, sepal_width-3.1, petal_length-4.4, petal_width-1.4]).
	example( 67, versicolor, [sepal_length-5.6, sepal_width-3.0, petal_length-4.5, petal_width-1.5]).
	example( 68, versicolor, [sepal_length-5.8, sepal_width-2.7, petal_length-4.1, petal_width-1.0]).
	example( 69, versicolor, [sepal_length-6.2, sepal_width-2.2, petal_length-4.5, petal_width-1.5]).
	example( 70, versicolor, [sepal_length-5.6, sepal_width-2.5, petal_length-3.9, petal_width-1.1]).
	example( 71, versicolor, [sepal_length-5.9, sepal_width-3.2, petal_length-4.8, petal_width-1.8]).
	example( 72, versicolor, [sepal_length-6.1, sepal_width-2.8, petal_length-4.0, petal_width-1.3]).
	example( 73, versicolor, [sepal_length-6.3, sepal_width-2.5, petal_length-4.9, petal_width-1.5]).
	example( 74, versicolor, [sepal_length-6.1, sepal_width-2.8, petal_length-4.7, petal_width-1.2]).
	example( 75, versicolor, [sepal_length-6.4, sepal_width-2.9, petal_length-4.3, petal_width-1.3]).
	example( 76, versicolor, [sepal_length-6.6, sepal_width-3.0, petal_length-4.4, petal_width-1.4]).
	example( 77, versicolor, [sepal_length-6.8, sepal_width-2.8, petal_length-4.8, petal_width-1.4]).
	example( 78, versicolor, [sepal_length-6.7, sepal_width-3.0, petal_length-5.0, petal_width-1.7]).
	example( 79, versicolor, [sepal_length-6.0, sepal_width-2.9, petal_length-4.5, petal_width-1.5]).
	example( 80, versicolor, [sepal_length-5.7, sepal_width-2.6, petal_length-3.5, petal_width-1.0]).
	example( 81, versicolor, [sepal_length-5.5, sepal_width-2.4, petal_length-3.8, petal_width-1.1]).
	example( 82, versicolor, [sepal_length-5.5, sepal_width-2.4, petal_length-3.7, petal_width-1.0]).
	example( 83, versicolor, [sepal_length-5.8, sepal_width-2.7, petal_length-3.9, petal_width-1.2]).
	example( 84, versicolor, [sepal_length-6.0, sepal_width-2.7, petal_length-5.1, petal_width-1.6]).
	example( 85, versicolor, [sepal_length-5.4, sepal_width-3.0, petal_length-4.5, petal_width-1.5]).
	example( 86, versicolor, [sepal_length-6.0, sepal_width-3.4, petal_length-4.5, petal_width-1.6]).
	example( 87, versicolor, [sepal_length-6.7, sepal_width-3.1, petal_length-4.7, petal_width-1.5]).
	example( 88, versicolor, [sepal_length-6.3, sepal_width-2.3, petal_length-4.4, petal_width-1.3]).
	example( 89, versicolor, [sepal_length-5.6, sepal_width-3.0, petal_length-4.1, petal_width-1.3]).
	example( 90, versicolor, [sepal_length-5.5, sepal_width-2.5, petal_length-4.0, petal_width-1.3]).
	example( 91, versicolor, [sepal_length-5.5, sepal_width-2.6, petal_length-4.4, petal_width-1.2]).
	example( 92, versicolor, [sepal_length-6.1, sepal_width-3.0, petal_length-4.6, petal_width-1.4]).
	example( 93, versicolor, [sepal_length-5.8, sepal_width-2.6, petal_length-4.0, petal_width-1.2]).
	example( 94, versicolor, [sepal_length-5.0, sepal_width-2.3, petal_length-3.3, petal_width-1.0]).
	example( 95, versicolor, [sepal_length-5.6, sepal_width-2.7, petal_length-4.2, petal_width-1.3]).
	example( 96, versicolor, [sepal_length-5.7, sepal_width-3.0, petal_length-4.2, petal_width-1.2]).
	example( 97, versicolor, [sepal_length-5.7, sepal_width-2.9, petal_length-4.2, petal_width-1.3]).
	example( 98, versicolor, [sepal_length-6.2, sepal_width-2.9, petal_length-4.3, petal_width-1.3]).
	example( 99, versicolor, [sepal_length-5.1, sepal_width-2.5, petal_length-3.0, petal_width-1.1]).
	example(100, versicolor, [sepal_length-5.7, sepal_width-2.8, petal_length-4.1, petal_width-1.3]).

	example(101, virginica,  [sepal_length-6.3, sepal_width-3.3, petal_length-6.0, petal_width-2.5]).
	example(102, virginica,  [sepal_length-5.8, sepal_width-2.7, petal_length-5.1, petal_width-1.9]).
	example(103, virginica,  [sepal_length-7.1, sepal_width-3.0, petal_length-5.9, petal_width-2.1]).
	example(104, virginica,  [sepal_length-6.3, sepal_width-2.9, petal_length-5.6, petal_width-1.8]).
	example(105, virginica,  [sepal_length-6.5, sepal_width-3.0, petal_length-5.8, petal_width-2.2]).
	example(106, virginica,  [sepal_length-7.6, sepal_width-3.0, petal_length-6.6, petal_width-2.1]).
	example(107, virginica,  [sepal_length-4.9, sepal_width-2.5, petal_length-4.5, petal_width-1.7]).
	example(108, virginica,  [sepal_length-7.3, sepal_width-2.9, petal_length-6.3, petal_width-1.8]).
	example(109, virginica,  [sepal_length-6.7, sepal_width-2.5, petal_length-5.8, petal_width-1.8]).
	example(110, virginica,  [sepal_length-7.2, sepal_width-3.6, petal_length-6.1, petal_width-2.5]).
	example(111, virginica,  [sepal_length-6.5, sepal_width-3.2, petal_length-5.1, petal_width-2.0]).
	example(112, virginica,  [sepal_length-6.4, sepal_width-2.7, petal_length-5.3, petal_width-1.9]).
	example(113, virginica,  [sepal_length-6.8, sepal_width-3.0, petal_length-5.5, petal_width-2.1]).
	example(114, virginica,  [sepal_length-5.7, sepal_width-2.5, petal_length-5.0, petal_width-2.0]).
	example(115, virginica,  [sepal_length-5.8, sepal_width-2.8, petal_length-5.1, petal_width-2.4]).
	example(116, virginica,  [sepal_length-6.4, sepal_width-3.2, petal_length-5.3, petal_width-2.3]).
	example(117, virginica,  [sepal_length-6.5, sepal_width-3.0, petal_length-5.5, petal_width-1.8]).
	example(118, virginica,  [sepal_length-7.7, sepal_width-3.8, petal_length-6.7, petal_width-2.2]).
	example(119, virginica,  [sepal_length-7.7, sepal_width-2.6, petal_length-6.9, petal_width-2.3]).
	example(120, virginica,  [sepal_length-6.0, sepal_width-2.2, petal_length-5.0, petal_width-1.5]).
	example(121, virginica,  [sepal_length-6.9, sepal_width-3.2, petal_length-5.7, petal_width-2.3]).
	example(122, virginica,  [sepal_length-5.6, sepal_width-2.8, petal_length-4.9, petal_width-2.0]).
	example(123, virginica,  [sepal_length-7.7, sepal_width-2.8, petal_length-6.7, petal_width-2.0]).
	example(124, virginica,  [sepal_length-6.3, sepal_width-2.7, petal_length-4.9, petal_width-1.8]).
	example(125, virginica,  [sepal_length-6.7, sepal_width-3.3, petal_length-5.7, petal_width-2.1]).
	example(126, virginica,  [sepal_length-7.2, sepal_width-3.2, petal_length-6.0, petal_width-1.8]).
	example(127, virginica,  [sepal_length-6.2, sepal_width-2.8, petal_length-4.8, petal_width-1.8]).
	example(128, virginica,  [sepal_length-6.1, sepal_width-3.0, petal_length-4.9, petal_width-1.8]).
	example(129, virginica,  [sepal_length-6.4, sepal_width-2.8, petal_length-5.6, petal_width-2.1]).
	example(130, virginica,  [sepal_length-7.2, sepal_width-3.0, petal_length-5.8, petal_width-1.6]).
	example(131, virginica,  [sepal_length-7.4, sepal_width-2.8, petal_length-6.1, petal_width-1.9]).
	example(132, virginica,  [sepal_length-7.9, sepal_width-3.8, petal_length-6.4, petal_width-2.0]).
	example(133, virginica,  [sepal_length-6.4, sepal_width-2.8, petal_length-5.6, petal_width-2.2]).
	example(134, virginica,  [sepal_length-6.3, sepal_width-2.8, petal_length-5.1, petal_width-1.5]).
	example(135, virginica,  [sepal_length-6.1, sepal_width-2.6, petal_length-5.6, petal_width-1.4]).
	example(136, virginica,  [sepal_length-7.7, sepal_width-3.0, petal_length-6.1, petal_width-2.3]).
	example(137, virginica,  [sepal_length-6.3, sepal_width-3.4, petal_length-5.6, petal_width-2.4]).
	example(138, virginica,  [sepal_length-6.4, sepal_width-3.1, petal_length-5.5, petal_width-1.8]).
	example(139, virginica,  [sepal_length-6.0, sepal_width-3.0, petal_length-4.8, petal_width-1.8]).
	example(140, virginica,  [sepal_length-6.9, sepal_width-3.1, petal_length-5.4, petal_width-2.1]).
	example(141, virginica,  [sepal_length-6.7, sepal_width-3.1, petal_length-5.6, petal_width-2.4]).
	example(142, virginica,  [sepal_length-6.9, sepal_width-3.1, petal_length-5.1, petal_width-2.3]).
	example(143, virginica,  [sepal_length-5.8, sepal_width-2.7, petal_length-5.1, petal_width-1.9]).
	example(144, virginica,  [sepal_length-6.8, sepal_width-3.2, petal_length-5.9, petal_width-2.3]).
	example(145, virginica,  [sepal_length-6.7, sepal_width-3.3, petal_length-5.7, petal_width-2.5]).
	example(146, virginica,  [sepal_length-6.7, sepal_width-3.0, petal_length-5.2, petal_width-2.3]).
	example(147, virginica,  [sepal_length-6.3, sepal_width-2.5, petal_length-5.0, petal_width-1.9]).
	example(148, virginica,  [sepal_length-6.5, sepal_width-3.0, petal_length-5.2, petal_width-2.0]).
	example(149, virginica,  [sepal_length-6.2, sepal_width-3.4, petal_length-5.4, petal_width-2.3]).
	example(150, virginica,  [sepal_length-5.9, sepal_width-3.0, petal_length-5.1, petal_width-1.8]).

:- end_object.
