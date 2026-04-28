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


:- object(mixed_independent_sources,
	implements(dimension_reduction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Continuous dataset obtained by linearly mixing two non-Gaussian independent sources into three observed features, intended for ICA separation tests.'
	]).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).

	example(1, [x1-(-5.0), x2-(-4.0), x3-(-4.0)]).
	example(2, [x1-(-1.0), x2-(-8.0), x3-(-2.0)]).
	example(3, [x1-(-3.0), x2-0.0, x3-(-2.0)]).
	example(4, [x1-1.0, x2-(-4.0), x3-0.0]).
	example(5, [x1-(-1.0), x2-4.0, x3-0.0]).
	example(6, [x1-3.0, x2-0.0, x3-2.0]).
	example(7, [x1-1.0, x2-8.0, x3-2.0]).
	example(8, [x1-5.0, x2-4.0, x3-4.0]).

:- end_object.
