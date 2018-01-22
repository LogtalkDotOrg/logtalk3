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


:- object(stop_watch,
	implements(monitoring),
	imports(monitor)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/10/20,
		comment is 'Message executing time monitor.'
	]).

	:- uses(time, [cpu_time/1]).

	before(Object, Message, Sender) :-
		write(Object), write(' <-- '), writeq(Message),
		write(' from '), write(Sender), nl, write('STARTING at '),
		cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

	after(Object, Message, Sender) :-
		write(Object), write(' <-- '), writeq(Message),
		write(' from '), write(Sender), nl, write('ENDING at '),
		cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

:- end_object.
