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


:- object(misspell).

	% call to an undefined but declared predicate
	:- public(foo/0).

	output :-
		foo.

	% call to an undefined local predicate
	output(A) :-
		bar(A).

	% misspelt call to Prolog built-in predicate
	output(A) :-
		writr(A).

:- end_object.



% singleton variables in opening object directive
:- object(singletons(L)).

	% singleton variables in predicate clause
	predicate(A) :-
		write(C).

:- end_object.



:- object(redefinitions).

	% redefinition of Logtalk built-in predicate
	current_object(_).

	% redefinition of a Prolog built-in predicate
	write(_).

:- end_object.



% references to unknown entities in object opening directive

:- object(unknownrefs,
	implements(some_protocol),
	imports(some_category),
	extends(some_object)).

	:- if(current_logtalk_flag(modules, supported)).
		:- use_module(some_module, [predicate/0]).
	:- endif.

:- end_object.



:- object(portability).

	:- public(predicate/0).

	% clause with calls to non-ISO Prolog standard predicates
	predicate :-
		compare(Result, first, second),
		retractall(result(Result, _)),
		sort([], []),
		consult(file).

:- end_object.



:- object(unused_predicate).

	:- uses(logtalk, [
		expand_library_path/2
	]).

:- end_object.



:- object(unused_non_terminal).

	:- uses(logtalk, [
		message_tokens//2
	]).

:- end_object.
