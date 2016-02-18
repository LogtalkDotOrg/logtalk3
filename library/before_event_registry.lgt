%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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



:- object(before_event_registry,
	implements(event_registryp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/10/8,
		comment is 'Before events registry predicates.'
	]).

	monitor(Monitor) :-
		current_event(before, _, _, _, Monitor).

	monitors(Monitors) :-
		(	setof(Monitor, monitor(Monitor), Monitors) ->
			true
		;	Monitors = []
		).

	monitored(Objects) :-
		(	setof(Object, Message^Sender^Monitor^current_event(before, Object, Message, Sender, Monitor), Objects) ->
			true
		;	Objects = []
		).

	monitor(Object, Message, Sender, Monitor) :-
		current_event(before, Object, Message, Sender, Monitor).

	set_monitor(Object, Message, Sender, Monitor) :-
		define_events(before, Object, Message, Sender, Monitor).

	del_monitors(Object, Message, Sender, Monitor) :-
		abolish_events(before, Object, Message, Sender, Monitor).

	del_monitors :-
		abolish_events(before, _, _, _, _).

:- end_object.
