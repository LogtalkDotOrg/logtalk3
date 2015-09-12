%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/04,
		comment is 'Unit tests for the current_protocol/1 built-in predicate.'
	]).

	throws(current_protocol_1_1, error(type_error(protocol_identifier, 1), logtalk(current_protocol(1), _))) :-
		current_protocol(1).

	succeeds(current_protocol_1_2) :-
		current_protocol(monitoring).

	fails(current_protocol_1_3) :-
		current_object(non_exisiting_protocol).

	succeeds(current_protocol_1_4) :-
		current_protocol(expanding),
		protocol_property(expanding, built_in),
		protocol_property(expanding, static).

	succeeds(current_protocol_1_5) :-
		current_protocol(monitoring),
		protocol_property(monitoring, built_in),
		protocol_property(monitoring, static).

	succeeds(current_protocol_1_6) :-
		current_protocol(forwarding),
		protocol_property(forwarding, built_in),
		protocol_property(forwarding, static).

:- end_object.
