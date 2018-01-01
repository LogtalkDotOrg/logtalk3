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


% no term_expansion/2 hook predicate defined
:- object(obj_ol_01).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

:- end_object.


% missing implements(expanding) but with a
% term_expansion/2 hook predicate definition
:- object(obj_ol_02).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- object(obj_ol_03,
	implements(expanding)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- object(obj_ol_04,
	implements(protected::expanding)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- object(obj_ol_05,
	implements(private::expanding)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- object(obj_ol_06_root,
	implements(private::expanding)).

:- end_object.


:- object(obj_ol_06,
	extends(obj_ol_06_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_object.


% within scope term_expansion/2 hook predicate definitions
% are not used by the expand_term/2 method
:- object(obj_ol_07_root,
	implements(expanding)).

	term_expansion(term, 'TERM').

:- end_object.


:- object(obj_ol_07,
	extends(obj_ol_07_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

:- end_object.


% within scope term_expansion/2 hook predicate definitions
% are not used by the expand_term/2 method
:- object(obj_ol_08_root,
	implements(protected::expanding)).

	term_expansion(term, 'TERM').

:- end_object.


:- object(obj_ol_08,
	extends(obj_ol_08_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

:- end_object.


% out-of-scope term_expansion/2 hook predicate definitions
% are not used by the expand_term/2 method
:- object(obj_ol_09_root,
	implements(private::expanding)).

	term_expansion(term, 'TERM').

:- end_object.


:- object(obj_ol_09,
	extends(obj_ol_09_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

:- end_object.


% overriding an inherited term_expansion/2 hook predicate
% works the same as for any other predicate
:- object(obj_ol_10_root,
	implements(expanding)).

	term_expansion(term, 'TERM').

:- end_object.


:- object(obj_ol_10,
	extends(obj_ol_10_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(foo, 'FOO').

:- end_object.
