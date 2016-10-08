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


% no term_expansion/2 hook predicate defined
:- category(ctg_cl_01).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

:- end_category.


:- object(obj_cl_01,
	imports(ctg_cl_01)).

:- end_object.


% missing implements(expanding) but with a
% term_expansion/2 hook predicate definition
:- category(ctg_cl_02).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_category.


:- object(obj_cl_02,
	imports(ctg_cl_02)).

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- category(ctg_cl_03,
	implements(expanding)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_category.


:- object(obj_cl_03,
	imports(ctg_cl_03)).

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- category(ctg_cl_04,
	implements(protected::expanding)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_category.


:- object(obj_cl_04,
	imports(ctg_cl_04)).

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- category(ctg_cl_05,
	implements(private::expanding)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_category.


:- object(obj_cl_05,
	imports(ctg_cl_05)).

:- end_object.


% scope of the term_expansion/2 hook predicate is irrelevant
% for local calls of the expand_term/2 method when the hook
% predicate is defined locally
:- category(ctg_cl_06_root,
	implements(private::expanding)).

:- end_category.


:- category(ctg_cl_06,
	extends(ctg_cl_06_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(term, 'TERM').

:- end_category.


:- object(obj_cl_06,
	imports(ctg_cl_06)).

:- end_object.


% within scope term_expansion/2 hook predicate definitions
% are not used by the expand_term/2 method
:- category(ctg_cl_07_root,
	implements(expanding)).

	term_expansion(term, 'TERM').

:- end_category.


:- category(ctg_cl_07,
	extends(ctg_cl_07_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

:- end_category.


:- object(obj_cl_07,
	imports(ctg_cl_07)).

:- end_object.


% within scope term_expansion/2 hook predicate definitions
% are not used by the expand_term/2 method
:- category(ctg_cl_08_root,
	implements(protected::expanding)).

	term_expansion(term, 'TERM').

:- end_category.


:- category(ctg_cl_08,
	extends(ctg_cl_08_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

:- end_category.


:- object(obj_cl_08,
	imports(ctg_cl_08)).

:- end_object.


% out-of-scope term_expansion/2 hook predicate definitions
% are not used by the expand_term/2 method
:- category(ctg_cl_09_root,
	implements(private::expanding)).

	term_expansion(term, 'TERM').

:- end_category.


:- category(ctg_cl_09,
	extends(ctg_cl_09_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

:- end_category.


:- object(obj_cl_09,
	imports(ctg_cl_09)).

:- end_object.


% overriding an inherited term_expansion/2 hook predicate
% works the same as for any other predicate
:- category(ctg_cl_10_root,
	implements(expanding)).

	term_expansion(term, 'TERM').

:- end_category.


:- category(ctg_cl_10,
	extends(ctg_cl_10_root)).

	:- public(p/1).
	p(Term) :-
		expand_term(term, Term).

	:- public(q/1).
	q(Term) :-
		expand_term(foo, Term).

	term_expansion(foo, 'FOO').

:- end_category.


:- object(obj_cl_10,
	imports(ctg_cl_10)).

:- end_object.
