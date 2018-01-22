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


:- object(wrapper,
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/09/02,
		comment is 'Hook object for compiling plain Prolog files, wrapping their code in an object named after the file name.'
	]).

	term_expansion(begin_of_file, [(:- object(Object))]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(Object, '.pl', Basename).

	term_expansion(end_of_file, [(:- end_object), end_of_file]) :-
		logtalk_load_context(basename, Basename),
		atom_concat(_, '.pl', Basename).

:- end_object.
