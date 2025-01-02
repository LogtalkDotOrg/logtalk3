%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-11-21,
		comment is 'Unit tests for the "self_vs_this" example.'
	]).

	test(self_vs_this_01, true(ctx(Self,This,Sender) == ctx(aircraft,thing,tests))) :-
		aircraft::context(Self, This, Sender).

	test(self_vs_this_02, true(ctx(Self,This,Sender) == ctx(transport,thing,tests))) :-
		transport::context(Self, This, Sender).

	test(self_vs_this_03, true(ctx(Self,This,Sender) == ctx(thing,thing,tests))) :-
		thing::context(Self, This, Sender).

:- end_object.
