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


:- object(iris_unlabeled,
	implements(clustering_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Compact unlabeled Iris-derived dataset for clustering experiments.'
	]).

	attribute_values(sepal_length, continuous).
	attribute_values(sepal_width, continuous).
	attribute_values(petal_length, continuous).
	attribute_values(petal_width, continuous).

	example( 1, [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.2]).
	example( 2, [sepal_length-4.9, sepal_width-3.0, petal_length-1.4, petal_width-0.2]).
	example( 3, [sepal_length-4.7, sepal_width-3.2, petal_length-1.3, petal_width-0.2]).
	example( 4, [sepal_length-7.0, sepal_width-3.2, petal_length-4.7, petal_width-1.4]).
	example( 5, [sepal_length-6.4, sepal_width-3.2, petal_length-4.5, petal_width-1.5]).
	example( 6, [sepal_length-6.9, sepal_width-3.1, petal_length-4.9, petal_width-1.5]).
	example( 7, [sepal_length-6.3, sepal_width-3.3, petal_length-6.0, petal_width-2.5]).
	example( 8, [sepal_length-5.8, sepal_width-2.7, petal_length-5.1, petal_width-1.9]).
	example( 9, [sepal_length-7.1, sepal_width-3.0, petal_length-5.9, petal_width-2.1]).

:- end_object.
