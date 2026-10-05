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


:- object(xor_dataset,
	implements(dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	class(label).

	class_values([negative, positive]).

	example(1, positive, [x1-0.0, x2-0.0]).
	example(2, negative, [x1-0.0, x2-1.0]).
	example(3, negative, [x1-1.0, x2-0.0]).
	example(4, positive, [x1-1.0, x2-1.0]).

:- end_object.


:- object(featureless_dataset,
	implements(dataset_protocol)).

	class(label).

	class_values([negative, positive]).

	example(1, negative, []).
	example(2, positive, []).
	example(3, negative, []).
	example(4, positive, []).

:- end_object.
