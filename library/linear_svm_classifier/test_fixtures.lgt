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


:- object(svm_bias_only(_Classes_, _Labels_),
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Attribute-free datasets for linear SVM classifier regression tests.'
	]).

	:- uses(list, [
		nth1/3
	]).

	attribute_values(_, _) :-
		fail.

	class(outcome).

	class_values(_Classes_).

	example(Id, Class, []) :-
		nth1(Id, _Labels_, Class).

:- end_object.


:- object(svm_high_cardinality(_Width_, _Count_),
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Deterministic categorical datasets for linear SVM performance measurements.'
	]).

	attribute_values(color, Values) :-
		category_values(1, _Width_, Values).
	attribute_values(shape, Values) :-
		category_values(1, _Width_, Values).

	class(outcome).
	class_values([yes, no]).

	example(Id, Class, [color-Color, shape-Shape]) :-
		example_index(1, _Count_, Id),
		Color is (Id - 1) mod _Width_ + 1,
		Shape is (Id - 1 + _Width_ // 2) mod _Width_ + 1,
		(	Color =< _Width_ // 2 ->
			Class = yes
		;	Class = no
		).

	category_values(Index, Width, Values) :-
		(	Index > Width ->
			Values = []
		;	Values = [Index| Rest],
			Next is Index + 1,
			category_values(Next, Width, Rest)
		).

	example_index(Index, Count, Id) :-
		Index =< Count,
		(	Id = Index
		;	Next is Index + 1,
			example_index(Next, Count, Id)
		).

:- end_object.


:- object(svm_continuous(_Values_),
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Continuous datasets for linear SVM scaling and optimizer regression tests.'
	]).

	:- uses(list, [
		nth1/3
	]).

	attribute_values(x, continuous).

	class(outcome).

	class_values([yes, no]).

	example(Id, Class, [x-Value]) :-
		nth1(Id, _Values_, Value),
		(	Id mod 2 =:= 1 ->
			Class = yes
		;	Class = no
		).

:- end_object.
