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


:- object(concentric_rings,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(i1, [x-1.0, y-0.0]).
	example(i2, [x-0.707, y-0.707]).
	example(i3, [x-0.0, y-1.0]).
	example(i4, [x- -0.707, y-0.707]).
	example(i5, [x- -1.0, y-0.0]).
	example(i6, [x- -0.707, y- -0.707]).
	example(i7, [x-0.0, y- -1.0]).
	example(i8, [x-0.707, y- -0.707]).
	example(o1, [x-3.0, y-0.0]).
	example(o2, [x-2.772, y-1.148]).
	example(o3, [x-2.121, y-2.121]).
	example(o4, [x-1.148, y-2.772]).
	example(o5, [x-0.0, y-3.0]).
	example(o6, [x- -1.148, y-2.772]).
	example(o7, [x- -2.121, y-2.121]).
	example(o8, [x- -2.772, y-1.148]).
	example(o9, [x- -3.0, y-0.0]).
	example(o10, [x- -2.772, y- -1.148]).
	example(o11, [x- -2.121, y- -2.121]).
	example(o12, [x- -1.148, y- -2.772]).
	example(o13, [x-0.0, y- -3.0]).
	example(o14, [x-1.148, y- -2.772]).
	example(o15, [x-2.121, y- -2.121]).
	example(o16, [x-2.772, y- -1.148]).

:- end_object.


:- object(identical_points,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-1.0]).
	example(2, [x-1.0, y-1.0]).
	example(3, [x-1.0, y-1.0]).

:- end_object.


:- object(invalid_spectral_dataset,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, x-1.1, y-1.0]).
	example(2, [x-5.0, y-5.0]).

:- end_object.
