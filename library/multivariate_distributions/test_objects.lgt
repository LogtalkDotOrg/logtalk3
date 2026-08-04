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


:- object(fixed_multivariate_sampler).

	:- public(standard_normal/1).
	:- mode(standard_normal(-float), one).
	:- info(standard_normal/1, [
		comment is 'Returns the fixed value 1.0.',
		argnames is ['Value']
	]).

	standard_normal(1.0).

	:- public(standard_gamma/2).
	:- mode(standard_gamma(+positive_float, -float), one).
	:- info(standard_gamma/2, [
		comment is 'Returns the fixed value 1.0.',
		argnames is ['Shape', 'Value']
	]).

	standard_gamma(_Shape, 1.0).

:- end_object.