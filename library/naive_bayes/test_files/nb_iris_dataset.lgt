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


:- object(nb_iris_dataset,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-15,
		comment is 'Simplified Iris dataset for Naive Bayes testing (continuous features).'
	]).

	attribute_values(sepal_length, continuous).
	attribute_values(sepal_width, continuous).
	attribute_values(petal_length, continuous).
	attribute_values(petal_width, continuous).

	class(species).

	class_values([setosa, versicolor, virginica]).

	example( 1, setosa,     [sepal_length-5.1, sepal_width-3.5, petal_length-1.4, petal_width-0.2]).
	example( 2, setosa,     [sepal_length-4.9, sepal_width-3.0, petal_length-1.4, petal_width-0.2]).
	example( 3, setosa,     [sepal_length-4.7, sepal_width-3.2, petal_length-1.3, petal_width-0.2]).
	example( 4, versicolor, [sepal_length-7.0, sepal_width-3.2, petal_length-4.7, petal_width-1.4]).
	example( 5, versicolor, [sepal_length-6.4, sepal_width-3.2, petal_length-4.5, petal_width-1.5]).
	example( 6, versicolor, [sepal_length-6.9, sepal_width-3.1, petal_length-4.9, petal_width-1.5]).
	example( 7, virginica,  [sepal_length-6.3, sepal_width-3.3, petal_length-6.0, petal_width-2.5]).
	example( 8, virginica,  [sepal_length-5.8, sepal_width-2.7, petal_length-5.1, petal_width-1.9]).
	example( 9, virginica,  [sepal_length-7.1, sepal_width-3.0, petal_length-5.9, petal_width-2.1]).

:- end_object.
