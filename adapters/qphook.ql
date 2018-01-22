%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration code for Qu-Prolog 7.1 and later versions, used when
%  generating a new interpreter that embeds Logtalk.
%  Last updated on August 2, 2012
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



main(Args) :-
	current_prolog_flag(version, Version),
	(	process_symbol(_), member('gui', Args) ->
		global_state_set('$gui_state', gui),
		start_thread_gui
	;	global_state_set('$gui_state', dumb)
	),
	write_term_list([wa('Qu-Prolog '), w(Version), nl]),
	interpreter.
