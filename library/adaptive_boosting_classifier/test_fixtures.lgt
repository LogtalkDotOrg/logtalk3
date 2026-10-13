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


:- object(adaptive_boosting_singleton,
	implements(dataset_protocol)).

	class(label).

	class_values([yes]).

	attribute_values(feature, [a]).

	example(1, yes, [feature-a]).

:- end_object.


:- object(adaptive_boosting_constant,
	implements(dataset_protocol)).

	class(label).

	class_values([yes, no]).

	example(1, yes, []).

:- end_object.


:- object(adaptive_boosting_conflicting,
	implements(dataset_protocol)).

	class(label).

	class_values([yes, no]).

	attribute_values(feature, [a]).

	example(1, yes, [feature-a]).
	example(2, no, [feature-a]).

:- end_object.


:- object(adaptive_boosting_perfect,
	implements(dataset_protocol)).

	class(label).

	class_values([yes, no]).

	attribute_values(feature, [a, b]).

	example(1, yes, [feature-a]).
	example(2, no, [feature-b]).

:- end_object.
