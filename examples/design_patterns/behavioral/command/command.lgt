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
% page on the Memento design pattern:
%
% https://en.wikipedia.org/wiki/Command_pattern


% first, we define a simple command protocol

:- protocol(command).

	:- public(execute/0).

:- end_protocol.


% a client/invoker object that can save and execute commands

:- object(switch).

	:- public(store_and_execute/1).
	store_and_execute(Command) :-
		Command::execute,
		assertz(history_(Command)).

	:- public(history/0).
	history :-
		history_(Command),
			writeq(Command), nl,
		fail.
	history.

	:- public(reply_history/0).
	reply_history :-
		forall(history_(Command), Command::execute).

	:- private(history_/1).
	:- dynamic(history_/1).

:- end_object.


% a simple category for devices that can be turned on and off

:- category(device).

	:- public(turn_on/0).
	turn_on :-
		self(Self),
		write('The '), write(Self), write(' is on'), nl.

	:- public(turn_off/0).
	turn_off :-
		self(Self),
		write('The '), write(Self), write(' is off'), nl.

:- end_category.


% a receiver object representing a light

:- object(light(_Light_),
	imports(device)).

:- end_object.

% a receiver object representing a coffee maker

:- object(coffee_maker,
	imports(device)).

:- end_object.


% a command for turning on a device

:- object(flip_up_command(_Device_),
	implements(command)).

	execute :-
		_Device_::turn_on.

:- end_object.


% a command for turning off a device

:- object(flip_down_command(_Device_),
	implements(command)).

	execute :-
		_Device_::turn_off.

:- end_object.
