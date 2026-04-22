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
		date is 2026-02-18,
		comment is 'Classic Iris flower dataset with 150 examples and 4 continuous attributes for classifying iris species. Originally from Fisher (1936).'
	]).

	attribute_values(sepal_length, continuous).
	attribute_values(sepal_width, continuous).
	attribute_values(petal_length, continuous).
	attribute_values(petal_width, continuous).

	class(species).

	class_values([setosa, versicolor, virginica]).

	example(Id, Class, [sepal_length-SepalLength, sepal_width-SepalWidth, petal_length-PetalLength, petal_width-PetalWidth]) :-
		example_(Id, Class, [SepalLength, SepalWidth, PetalLength, PetalWidth]).

	example_(  1, setosa,     [5.1, 3.5, 1.4,0.2]).
	example_(  2, setosa,     [4.9, 3.0, 1.4,0.2]).
	example_(  3, setosa,     [4.7, 3.2, 1.3,0.2]).
	example_(  4, setosa,     [4.6, 3.1, 1.5,0.2]).
	example_(  5, setosa,     [5.0, 3.6, 1.4,0.2]).
	example_(  6, setosa,     [5.4, 3.9, 1.7,0.4]).
	example_(  7, setosa,     [4.6, 3.4, 1.4,0.3]).
	example_(  8, setosa,     [5.0, 3.4, 1.5,0.2]).
	example_(  9, setosa,     [4.4, 2.9, 1.4,0.2]).
	example_( 10, setosa,     [4.9, 3.1, 1.5,0.1]).
	example_( 11, setosa,     [5.4, 3.7, 1.5,0.2]).
	example_( 12, setosa,     [4.8, 3.4, 1.6,0.2]).
	example_( 13, setosa,     [4.8, 3.0, 1.4,0.1]).
	example_( 14, setosa,     [4.3, 3.0, 1.1,0.1]).
	example_( 15, setosa,     [5.8, 4.0, 1.2,0.2]).
	example_( 16, setosa,     [5.7, 4.4, 1.5,0.4]).
	example_( 17, setosa,     [5.4, 3.9, 1.3,0.4]).
	example_( 18, setosa,     [5.1, 3.5, 1.4,0.3]).
	example_( 19, setosa,     [5.7, 3.8, 1.7,0.3]).
	example_( 20, setosa,     [5.1, 3.8, 1.5,0.3]).
	example_( 21, setosa,     [5.4, 3.4, 1.7,0.2]).
	example_( 22, setosa,     [5.1, 3.7, 1.5,0.4]).
	example_( 23, setosa,     [4.6, 3.6, 1.0,0.2]).
	example_( 24, setosa,     [5.1, 3.3, 1.7,0.5]).
	example_( 25, setosa,     [4.8, 3.4, 1.9,0.2]).
	example_( 26, setosa,     [5.0, 3.0, 1.6,0.2]).
	example_( 27, setosa,     [5.0, 3.4, 1.6,0.4]).
	example_( 28, setosa,     [5.2, 3.5, 1.5,0.2]).
	example_( 29, setosa,     [5.2, 3.4, 1.4,0.2]).
	example_( 30, setosa,     [4.7, 3.2, 1.6,0.2]).
	example_( 31, setosa,     [4.8, 3.1, 1.6,0.2]).
	example_( 32, setosa,     [5.4, 3.4, 1.5,0.4]).
	example_( 33, setosa,     [5.2, 4.1, 1.5,0.1]).
	example_( 34, setosa,     [5.5, 4.2, 1.4,0.2]).
	example_( 35, setosa,     [4.9, 3.1, 1.5,0.1]).
	example_( 36, setosa,     [5.0, 3.2, 1.2,0.2]).
	example_( 37, setosa,     [5.5, 3.5, 1.3,0.2]).
	example_( 38, setosa,     [4.9, 3.1, 1.5,0.1]).
	example_( 39, setosa,     [4.4, 3.0, 1.3,0.2]).
	example_( 40, setosa,     [5.1, 3.4, 1.5,0.2]).
	example_( 41, setosa,     [5.0, 3.5, 1.3,0.3]).
	example_( 42, setosa,     [4.5, 2.3, 1.3,0.3]).
	example_( 43, setosa,     [4.4, 3.2, 1.3,0.2]).
	example_( 44, setosa,     [5.0, 3.5, 1.6,0.6]).
	example_( 45, setosa,     [5.1, 3.8, 1.9,0.4]).
	example_( 46, setosa,     [4.8, 3.0, 1.4,0.3]).
	example_( 47, setosa,     [5.1, 3.8, 1.6,0.2]).
	example_( 48, setosa,     [4.6, 3.2, 1.4,0.2]).
	example_( 49, setosa,     [5.3, 3.7, 1.5,0.2]).
	example_( 50, setosa,     [5.0, 3.3, 1.4,0.2]).
	example_( 51, versicolor, [7.0, 3.2, 4.7,1.4]).
	example_( 52, versicolor, [6.4, 3.2, 4.5,1.5]).
	example_( 53, versicolor, [6.9, 3.1, 4.9,1.5]).
	example_( 54, versicolor, [5.5, 2.3, 4.0,1.3]).
	example_( 55, versicolor, [6.5, 2.8, 4.6,1.5]).
	example_( 56, versicolor, [5.7, 2.8, 4.5,1.3]).
	example_( 57, versicolor, [6.3, 3.3, 4.7,1.6]).
	example_( 58, versicolor, [4.9, 2.4, 3.3,1.0]).
	example_( 59, versicolor, [6.6, 2.9, 4.6,1.3]).
	example_( 60, versicolor, [5.2, 2.7, 3.9,1.4]).
	example_( 61, versicolor, [5.0, 2.0, 3.5,1.0]).
	example_( 62, versicolor, [5.9, 3.0, 4.2,1.5]).
	example_( 63, versicolor, [6.0, 2.2, 4.0,1.0]).
	example_( 64, versicolor, [6.1, 2.9, 4.7,1.4]).
	example_( 65, versicolor, [5.6, 2.9, 3.6,1.3]).
	example_( 66, versicolor, [6.7, 3.1, 4.4,1.4]).
	example_( 67, versicolor, [5.6, 3.0, 4.5,1.5]).
	example_( 68, versicolor, [5.8, 2.7, 4.1,1.0]).
	example_( 69, versicolor, [6.2, 2.2, 4.5,1.5]).
	example_( 70, versicolor, [5.6, 2.5, 3.9,1.1]).
	example_( 71, versicolor, [5.9, 3.2, 4.8,1.8]).
	example_( 72, versicolor, [6.1, 2.8, 4.0,1.3]).
	example_( 73, versicolor, [6.3, 2.5, 4.9,1.5]).
	example_( 74, versicolor, [6.1, 2.8, 4.7,1.2]).
	example_( 75, versicolor, [6.4, 2.9, 4.3,1.3]).
	example_( 76, versicolor, [6.6, 3.0, 4.4,1.4]).
	example_( 77, versicolor, [6.8, 2.8, 4.8,1.4]).
	example_( 78, versicolor, [6.7, 3.0, 5.0,1.7]).
	example_( 79, versicolor, [6.0, 2.9, 4.5,1.5]).
	example_( 80, versicolor, [5.7, 2.6, 3.5,1.0]).
	example_( 81, versicolor, [5.5, 2.4, 3.8,1.1]).
	example_( 82, versicolor, [5.5, 2.4, 3.7,1.0]).
	example_( 83, versicolor, [5.8, 2.7, 3.9,1.2]).
	example_( 84, versicolor, [6.0, 2.7, 5.1,1.6]).
	example_( 85, versicolor, [5.4, 3.0, 4.5,1.5]).
	example_( 86, versicolor, [6.0, 3.4, 4.5,1.6]).
	example_( 87, versicolor, [6.7, 3.1, 4.7,1.5]).
	example_( 88, versicolor, [6.3, 2.3, 4.4,1.3]).
	example_( 89, versicolor, [5.6, 3.0, 4.1,1.3]).
	example_( 90, versicolor, [5.5, 2.5, 4.0,1.3]).
	example_( 91, versicolor, [5.5, 2.6, 4.4,1.2]).
	example_( 92, versicolor, [6.1, 3.0, 4.6,1.4]).
	example_( 93, versicolor, [5.8, 2.6, 4.0,1.2]).
	example_( 94, versicolor, [5.0, 2.3, 3.3,1.0]).
	example_( 95, versicolor, [5.6, 2.7, 4.2,1.3]).
	example_( 96, versicolor, [5.7, 3.0, 4.2,1.2]).
	example_( 97, versicolor, [5.7, 2.9, 4.2,1.3]).
	example_( 98, versicolor, [6.2, 2.9, 4.3,1.3]).
	example_( 99, versicolor, [5.1, 2.5, 3.0,1.1]).
	example_(100, versicolor, [5.7, 2.8, 4.1,1.3]).
	example_(101, virginica,  [6.3, 3.3, 6.0,2.5]).
	example_(102, virginica,  [5.8, 2.7, 5.1,1.9]).
	example_(103, virginica,  [7.1, 3.0, 5.9,2.1]).
	example_(104, virginica,  [6.3, 2.9, 5.6,1.8]).
	example_(105, virginica,  [6.5, 3.0, 5.8,2.2]).
	example_(106, virginica,  [7.6, 3.0, 6.6,2.1]).
	example_(107, virginica,  [4.9, 2.5, 4.5,1.7]).
	example_(108, virginica,  [7.3, 2.9, 6.3,1.8]).
	example_(109, virginica,  [6.7, 2.5, 5.8,1.8]).
	example_(110, virginica,  [7.2, 3.6, 6.1,2.5]).
	example_(111, virginica,  [6.5, 3.2, 5.1,2.0]).
	example_(112, virginica,  [6.4, 2.7, 5.3,1.9]).
	example_(113, virginica,  [6.8, 3.0, 5.5,2.1]).
	example_(114, virginica,  [5.7, 2.5, 5.0,2.0]).
	example_(115, virginica,  [5.8, 2.8, 5.1,2.4]).
	example_(116, virginica,  [6.4, 3.2, 5.3,2.3]).
	example_(117, virginica,  [6.5, 3.0, 5.5,1.8]).
	example_(118, virginica,  [7.7, 3.8, 6.7,2.2]).
	example_(119, virginica,  [7.7, 2.6, 6.9,2.3]).
	example_(120, virginica,  [6.0, 2.2, 5.0,1.5]).
	example_(121, virginica,  [6.9, 3.2, 5.7,2.3]).
	example_(122, virginica,  [5.6, 2.8, 4.9,2.0]).
	example_(123, virginica,  [7.7, 2.8, 6.7,2.0]).
	example_(124, virginica,  [6.3, 2.7, 4.9,1.8]).
	example_(125, virginica,  [6.7, 3.3, 5.7,2.1]).
	example_(126, virginica,  [7.2, 3.2, 6.0,1.8]).
	example_(127, virginica,  [6.2, 2.8, 4.8,1.8]).
	example_(128, virginica,  [6.1, 3.0, 4.9,1.8]).
	example_(129, virginica,  [6.4, 2.8, 5.6,2.1]).
	example_(130, virginica,  [7.2, 3.0, 5.8,1.6]).
	example_(131, virginica,  [7.4, 2.8, 6.1,1.9]).
	example_(132, virginica,  [7.9, 3.8, 6.4,2.0]).
	example_(133, virginica,  [6.4, 2.8, 5.6,2.2]).
	example_(134, virginica,  [6.3, 2.8, 5.1,1.5]).
	example_(135, virginica,  [6.1, 2.6, 5.6,1.4]).
	example_(136, virginica,  [7.7, 3.0, 6.1,2.3]).
	example_(137, virginica,  [6.3, 3.4, 5.6,2.4]).
	example_(138, virginica,  [6.4, 3.1, 5.5,1.8]).
	example_(139, virginica,  [6.0, 3.0, 4.8,1.8]).
	example_(140, virginica,  [6.9, 3.1, 5.4,2.1]).
	example_(141, virginica,  [6.7, 3.1, 5.6,2.4]).
	example_(142, virginica,  [6.9, 3.1, 5.1,2.3]).
	example_(143, virginica,  [5.8, 2.7, 5.1,1.9]).
	example_(144, virginica,  [6.8, 3.2, 5.9,2.3]).
	example_(145, virginica,  [6.7, 3.3, 5.7,2.5]).
	example_(146, virginica,  [6.7, 3.0, 5.2,2.3]).
	example_(147, virginica,  [6.3, 2.5, 5.0,1.9]).
	example_(148, virginica,  [6.5, 3.0, 5.2,2.0]).
	example_(149, virginica,  [6.2, 3.4, 5.4,2.3]).
	example_(150, virginica,  [5.9, 3.0, 5.1,1.8]).

:- end_object.
