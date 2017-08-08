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


:- category(os_types).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2017/03/15,
		comment is 'A set of operating-system related types.',
		remarks is [
			'Provided types' - 'This category adds "file", "file(Extensions)", "directory", and "environment_variable" types for type-checking when using the "type" library object.',
			'Type file' - 'For checking if a term is an atom and an existing file.',
			'Type file(Extensions)' - 'For checking if a term is an atom and an existing file with one of the listed extensions (specified as \'.ext\').',
			'Type directory' - 'For checking if a term is an atom and an existing directory.',
			'Type environment_variable' - 'For checking if a term is an atom and an existing environment variable.'
		],
		see_also is [osp, os]
	]).

	:- multifile(type::type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::type/1).
	:- endif.

	% clauses for the type::type/1 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::type(file).
	type::type(file(_Extensions)).
	type::type(directory).
	type::type(environment_variable).

	:- multifile(type::check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::check/2).
	:- endif.

	% clauses for the type::check/2 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::check(file, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	os::file_exists(Term) ->
			true
		;	throw(existence_error(file, Term))
		).
	type::check(file(Extensions), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	\+ os::file_exists(Term) ->
			throw(existence_error(file, Term))
		;	os::decompose_file_name(Term, _, _, Extension),
			list::member(Extension, Extensions) ->
			true
		;	throw(domain_error(file(Extensions), Term))
		).
	type::check(directory, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	os::directory_exists(Term) ->
			true
		;	throw(existence_error(file, Term))
		).
	type::check(environment_variable, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	os::environment_variable(Term, _) ->
			true
		;	throw(existence_error(environment_variable, Term))
		).

:- end_category.
