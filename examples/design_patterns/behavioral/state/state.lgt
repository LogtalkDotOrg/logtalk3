%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the state design pattern:
%
% https://en.wikipedia.org/wiki/State_pattern


% in this example, we assume that clients can send to the "context"
% object any message accepted by the concrete state objects; thus,
% we forward all the *client* messages to the current concrete
% state object

:- object(context,
	implements(forwarding)).

	% set initial state upon loading
	:- initialization(set_state(state_pretty)).

	% predicate for setting the current concrete state
	% object that should handle the client requests
	:- public(set_state/1).

	:- private(state_/1).
	:- dynamic(state_/1).

	set_state(State) :-
		retractall(state_(_)),
		assertz(state_(State)).

	% forward client messages to the current
	% concrete state object
	forward(Message) :-
		state_(State),
		[State::Message].

:- end_object.


% we now define a simple protocol for the concrete state objects
% declaring a predicate that outputs a term; however it could also
% be the case that each concrete state object would provide a
% specific protocol

:- protocol(state_protocol).

	:- public(output/1).

:- end_protocol.


% similar to the original Wikipedia example, for illustrating purposes,
% we simply switch state after handling a request

:- object(state_pretty,
	implements(state_protocol)).

	output(Term) :-
		writeq(Term), nl,
		context::set_state(state_canonical).

:- end_object.


:- object(state_canonical,
	implements(state_protocol)).

	output(Term) :-
		write_canonical(Term), nl,
		context::set_state(state_pretty).

:- end_object.
