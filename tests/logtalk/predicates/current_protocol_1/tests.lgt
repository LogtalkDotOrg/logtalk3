%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2018-03-26,
		comment is 'Unit tests for the current_protocol/1 built-in predicate.'
	]).

	throws(current_protocol_1_1, error(type_error(protocol_identifier, 1), logtalk(current_protocol(1), _))) :-
		% delay the error to runtime
		{current_protocol(1)}.

	succeeds(current_protocol_1_2) :-
		% delay the error to runtime
		{current_protocol(monitoring)}.

	fails(current_protocol_1_3) :-
		% delay the error to runtime
		{current_object(non_exisiting_protocol)}.

:- end_object.
