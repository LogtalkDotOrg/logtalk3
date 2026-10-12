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


:- object(svm_small,
	implements(anomaly_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Small continuous dataset for one-class SVM unit tests.'
	]).

	attribute_values(x, continuous).

	class_values([normal]).

	example(1, normal, [x-1.0]).
	example(2, normal, [x-2.0]).

:- end_object.


:- object(svm_large,
	implements(anomaly_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Large identical values for one-class SVM numerical tests.'
	]).

	attribute_values(x, continuous).

	class_values([normal]).

	example(1, normal, [x-1.0e10]).
	example(2, normal, [x-1.0e10]).

:- end_object.


:- object(svm_centered,
	implements(anomaly_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Symmetric continuous dataset for centered linear SVM tests.'
	]).

	attribute_values(x, continuous).

	class_values([normal]).

	example(1, normal, [x-(-1.0)]).
	example(2, normal, [x-1.0]).

:- end_object.
