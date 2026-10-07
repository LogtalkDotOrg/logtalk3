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


:- object(lasso_sparse_dataset,
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Orthogonal sparse regression fixture for the Lasso feature selector.'
	]).

	attribute_values(signal, continuous).
	attribute_values(noise, continuous).
	attribute_values(constant, continuous).

	example_count(4).

	example(1, [signal- -1, noise- -1, constant-1], -2).
	example(2, [signal- -1, noise-1, constant-1], -2).
	example(3, [signal-1, noise- -1, constant-1], 2).
	example(4, [signal-1, noise-1, constant-1], 2).

:- end_object.


:- object(lasso_fixture(_Declarations_, _Examples_),
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Parametric feature dataset fixture for the Lasso feature selector.',
		parameters is [
			'Declarations' - 'Feature-domain pairs.',
			'Examples' - 'Feature dataset example terms.'
		]
	]).

	:- uses(list, [
		length/2, member/2
	]).

	attribute_values(Feature, Domain) :-
		member(Feature-Domain, _Declarations_).

	example_count(Count) :-
		length(_Examples_, Count).

	example(Id, Pairs, Target) :-
		member(example(Id, Pairs, Target), _Examples_).

:- end_object.
