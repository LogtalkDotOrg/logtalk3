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


:- object(labeled_measurements,
	implements(supervised_dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-18,
		comment is 'Compact labeled continuous dataset with three classes suitable for LDA projection tests.'
	]).

	attribute_values(length, continuous).
	attribute_values(width, continuous).
	attribute_values(height, continuous).
	attribute_values(weight, continuous).

	class(label).
	class_values([alpha, beta, gamma]).

	example(1, [length-5.1, width-3.5, height-1.4, weight-0.2]).
	example(2, [length-4.9, width-3.0, height-1.4, weight-0.2]).
	example(3, [length-5.0, width-3.4, height-1.5, weight-0.2]).
	example(4, [length-6.4, width-3.2, height-4.5, weight-1.5]).
	example(5, [length-6.9, width-3.1, height-4.9, weight-1.5]).
	example(6, [length-6.5, width-2.8, height-4.6, weight-1.5]).
	example(7, [length-6.5, width-3.0, height-5.2, weight-2.0]).
	example(8, [length-6.2, width-3.4, height-5.4, weight-2.3]).
	example(9, [length-5.9, width-3.0, height-5.1, weight-1.8]).

	example(1, alpha, [length-5.1, width-3.5, height-1.4, weight-0.2]).
	example(2, alpha, [length-4.9, width-3.0, height-1.4, weight-0.2]).
	example(3, alpha, [length-5.0, width-3.4, height-1.5, weight-0.2]).
	example(4, beta,  [length-6.4, width-3.2, height-4.5, weight-1.5]).
	example(5, beta,  [length-6.9, width-3.1, height-4.9, weight-1.5]).
	example(6, beta,  [length-6.5, width-2.8, height-4.6, weight-1.5]).
	example(7, gamma, [length-6.5, width-3.0, height-5.2, weight-2.0]).
	example(8, gamma, [length-6.2, width-3.4, height-5.4, weight-2.3]).
	example(9, gamma, [length-5.9, width-3.0, height-5.1, weight-1.8]).

:- end_object.
