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


:- object(plane,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-13,
		comment is 'Two-feature regression dataset following the plane z = 3x1 - 2x2 + 5.'
	]).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	target(z).

	example(1,  6,  [x1-1, x2-1]).
	example(2,  8,  [x1-2, x2-1.5]).
	example(3,  2,  [x1-1, x2-3]).
	example(4,  9,  [x1-3, x2-2.5]).
	example(5,  1,  [x1-0, x2-2]).
	example(6, 11,  [x1-4, x2-3]).

:- end_object.
