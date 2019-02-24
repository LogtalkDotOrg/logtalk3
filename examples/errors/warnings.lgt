%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		tell(file).

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



:- object(mode_directive_typo).

	:- public(a/1).
	:- mode(a(+atom, -integer), one).

:- end_object.



:- object(missing_public_directive).

	:- multifile(m/2).

:- end_object.



:- object(missing_protocol_reference).

	before(_, _, _).

:- end_object.



:- object(tautology).

	oops :- x \== y.

	really :- \+ x == y.

:- end_object.



:- object(falsewood).

	damn :- x == y.

	rats :- \+ x \== y.

:- end_object.



:- object(trivial_fails).

	foo :-
		bar(1).

	bar([]).
	bar([_| _]).

:- end_object.



:- object(duplicated_scope_directive).

	:- public(foo/1).

	foo(1).
	foo(2).
	foo(3).

	:- public(foo/1).

:- end_object.



:- object(duplicated_multifile_directive).

	:- public(foo/1).
	:- multifile(foo/1).

	foo(1).
	foo(2).
	foo(3).

	:- multifile(foo/1).

:- end_object.



:- object(duplicated_dynamic_directive).

	:- dynamic(foo/1).

	foo(1).
	foo(2).
	foo(3).

	:- dynamic(foo/1).

:- end_object.



:- object(duplicated_discontiguous_directive).

	:- discontiguous(foo/1).

	foo(1).
	foo(2).
	foo(3).

	:- discontiguous(foo/1).

:- end_object.



:- object(duplicated_meta_predicte_directive).

	:- meta_predicate(foo(0)).

	foo(X) :-
		call(X).

	:- meta_predicate(foo(0)).

:- end_object.



:- object(duplicated_meta_non_terminal_directive).

	:- meta_non_terminal(foo(0)).

	foo(X) --> call(X).

	:- meta_non_terminal(foo(0)).

:- end_object.



:- object(suspicious_calls).

	recursive([]).
	recursive([H| T]) :-
		::single(H),
		::recursive(T).

	foo :-
		self(Self),
		Self::bar.

	bar :-
		this(This),
		This::baz.

	baz.

:- end_object.
