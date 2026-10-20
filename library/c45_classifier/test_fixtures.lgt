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


:- object(c45_fixture(_Kind_),
	implements(dataset_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-20,
		comment is 'Fixed edge-case and generated datasets for the "c45_classifier" library unit tests.'
	]).

	:- set_logtalk_flag(duplicated_clauses, silent).

	class(label).

	class_values(Classes) :-
		(	_Kind_ == singleton ->
			Classes = [yes]
		;	Classes = [yes, no]
		).

	attribute_values(Attribute, Values) :-
		fixture_attribute(_Kind_, Attribute, Values).

	example(Id, Class, Values) :-
		fixture_example(_Kind_, Id, Class, Values).

	fixture_attribute(integer_numeric, x, continuous).
	fixture_attribute(extreme_numeric, x, continuous).
	fixture_attribute(constant_xor, x, continuous).
	fixture_attribute(constant_xor, a, [0, 1]).
	fixture_attribute(constant_xor, b, [0, 1]).
	fixture_attribute(continuous_xor, x, continuous).
	fixture_attribute(continuous_xor, b, [0, 1]).
	fixture_attribute(constant, x, continuous).
	fixture_attribute(missing_discrete, a, [ordinary, other]).
	fixture_attribute(missing_categorical, a, [first, second]).
	fixture_attribute(unused_branch, a, [first, second, unused]).
	fixture_attribute(missing, x, continuous).
	fixture_attribute(all_missing, x, continuous).
	fixture_attribute(categorical, a, [=<(2), >(2), ordinary]).
	fixture_attribute(singleton, a, [ordinary]).
	fixture_attribute(duplicate_attributes, x, continuous).
	fixture_attribute(duplicate_attributes, x, continuous).
	fixture_attribute(incomplete, x, continuous).
	fixture_attribute(generated(_), x, continuous).
	fixture_attribute(generated(_), a, [0, 1]).
	fixture_attribute(pruning_skew, x, continuous).

	fixture_example(integer_numeric, 1, yes, [x-1]).
	fixture_example(integer_numeric, 2, no, [x-2]).
	fixture_example(integer_numeric, 3, no, [x-3]).
	fixture_example(integer_numeric, 4, no, [x-4]).
	fixture_example(extreme_numeric, 1, yes, [x-1.0e308]).
	fixture_example(extreme_numeric, 2, no, [x-1.1e308]).
	fixture_example(constant_xor, 1, yes, [x-1.0, a-0, b-0]).
	fixture_example(constant_xor, 2, no, [x-1.0, a-0, b-1]).
	fixture_example(constant_xor, 3, no, [x-1.0, a-1, b-0]).
	fixture_example(constant_xor, 4, yes, [x-1.0, a-1, b-1]).
	fixture_example(continuous_xor, 1, yes, [x-0.0, b-0]).
	fixture_example(continuous_xor, 2, no, [x-0.0, b-1]).
	fixture_example(continuous_xor, 3, no, [x-1.0, b-0]).
	fixture_example(continuous_xor, 4, yes, [x-1.0, b-1]).
	fixture_example(constant, 1, yes, [x-1.0]).
	fixture_example(constant, 2, no, [x-1.0]).
	fixture_example(missing_discrete, 1, yes, [a-_]).
	fixture_example(missing_discrete, 2, no, [a-_]).
	fixture_example(missing_categorical, 1, yes, [a-first]).
	fixture_example(missing_categorical, 2, no, [a-second]).
	fixture_example(missing_categorical, 3, yes, [a-_]).
	fixture_example(missing_categorical, 4, no, [a-_]).
	fixture_example(unused_branch, 1, yes, [a-first]).
	fixture_example(unused_branch, 2, no, [a-second]).
	fixture_example(missing, 1, yes, [x-0.0]).
	fixture_example(missing, 2, no, [x-1.0]).
	fixture_example(missing, 3, yes, [x-_]).
	fixture_example(missing, 4, no, [x-_]).
	fixture_example(all_missing, 1, yes, [x-_]).
	fixture_example(all_missing, 2, no, [x-_]).
	fixture_example(categorical, 1, yes, [a- =<(2)]).
	fixture_example(categorical, 2, no, [a- >(2)]).
	fixture_example(categorical, 3, yes, [a-ordinary]).
	fixture_example(singleton, 1, yes, [a-ordinary]).
	fixture_example(attribute_free, 1, yes, []).
	fixture_example(attribute_free, 2, yes, []).
	fixture_example(attribute_free, 3, no, []).
	fixture_example(duplicate_attributes, 1, yes, [x-1.0]).
	fixture_example(incomplete, 1, yes, []).
	fixture_example(generated(Rows), Id, Class, Values) :-
		list::member(example(Id, Class, Values), Rows).
	fixture_example(pruning_skew, 1, yes, [x-0]).
	fixture_example(pruning_skew, 2, no, [x-1]).
	fixture_example(pruning_skew, 3, yes, [x-2]).
	fixture_example(pruning_skew, 4, no, [x-3]).
	fixture_example(pruning_skew, 5, yes, [x-4]).
	fixture_example(pruning_skew, 6, no, [x-5]).
	fixture_example(pruning_skew, 7, yes, [x-6]).
	fixture_example(pruning_skew, 8, no, [x-7]).

:- end_object.
