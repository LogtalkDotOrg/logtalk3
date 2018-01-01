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


:- category(hacker,
	% built-in protocol for the event handler methods
	implements(monitoring),
	% patch (or attempt to patch) the "my_vault" object
	complements(my_vault)).

	% attempt to override the "my_vault" password:
	password('1234567890').

	% print a hacked message every time a message
	% is sent to the "my_vault" object:
	% define a "before" event handler for the complemented object:
	before(_, _, _) :-
		write('You have been hacked by SmartPants!'), nl.

:- end_category.


% setup the object "my_vault" as a monitor for any message sent to itself:

:- initialization((
	define_events(before, my_vault, _, _, my_vault),
	set_logtalk_flag(events, allow)
)).
