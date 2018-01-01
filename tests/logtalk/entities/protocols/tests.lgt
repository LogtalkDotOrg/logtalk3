%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		date is 2014/12/08,
		comment is 'Unit tests for the protocol/2 opening directive.'
	]).

	% test all possible syntaxes for protocol relations

	test(protocol_0) :-
		logtalk_load(protocols, [unknown_entities(silent)]).

	test(protocol_1) :-
		extends_protocol(protocol_1, parent1).

	test(protocol_2) :-
		extends_protocol(protocol_2, parent1),
		extends_protocol(protocol_2, parent2).

	test(protocol_3) :-
		extends_protocol(protocol_3, parent1),
		extends_protocol(protocol_3, parent2).

:- end_object.
