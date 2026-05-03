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


:- object(sparse_signal,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Two-feature regression dataset where only the signal attribute contributes to the target and the noise attribute is orthogonal to it.'
	]).

	attribute_values(signal, continuous).
	attribute_values(noise, continuous).

	target(y).

	example(1, -4, [signal-(-3), noise-1]).
	example(2, -1, [signal-(-2), noise-(-2)]).
	example(3,  2, [signal-(-1), noise-1]).
	example(4,  8, [signal-1,    noise-1]).
	example(5, 11, [signal-2,    noise-(-2)]).
	example(6, 14, [signal-3,    noise-1]).

:- end_object.