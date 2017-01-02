%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		date is 2012/12/12,
		comment is 'Unit tests for the abolish_protocol/1 built-in predicate.'
	]).

	throws(abolish_protocol_1_1, error(instantiation_error, logtalk(abolish_protocol(_), _))) :-
		abolish_protocol(_).

	throws(abolish_protocol_1_2, error(type_error(protocol_identifier, 1), logtalk(abolish_protocol(1), _))) :-
		abolish_protocol(1).

	throws(abolish_protocol_1_3, error(existence_error(protocol, non_exisiting_protocol), logtalk(abolish_protocol(non_exisiting_protocol), _))) :-
		abolish_protocol(non_exisiting_protocol).

	throws(abolish_protocol_1_4, error(permission_error(modify, static_protocol, monitoring), logtalk(abolish_protocol(monitoring), _))) :-
		abolish_protocol(monitoring).

	succeeds(abolish_protocol_1_5) :-
		create_protocol(Protocol, [], []),
		current_protocol(Protocol),
		abolish_protocol(Protocol),
		\+ current_protocol(Protocol).

	succeeds(abolish_protocol_1_6) :-
		create_protocol(a_protocol, [], []),
		current_protocol(a_protocol),
		abolish_protocol(a_protocol),
		\+ current_protocol(a_protocol).

:- end_object.
