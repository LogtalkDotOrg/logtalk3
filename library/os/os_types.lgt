%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2021-02-12,
		comment is 'A set of operating-system related types.',
		remarks is [
			'Provided types' - 'This category adds ``file``, ``file(Extensions)``, ``file(Extensions,Permissions)``, ``directory``, ``directory(Permissions)``, and ``environment_variable`` types for type-checking when using the ``type`` library object.',
			'Type ``file``' - 'For checking if a term is an atom and an existing file.',
			'Type ``file(Extensions)``' - 'For checking if a term is an atom and an existing file with one of the listed extensions (specified as ``''.ext''``).',
			'Type ``file(Extensions,Permissions)``' - 'For checking if a term is an atom and an existing file with one of the listed extensions (specified as ``''.ext''``) and listed permissions ({``read``, ``write``, ``execute``}).',
			'Type ``directory``' - 'For checking if a term is an atom and an existing directory.',
			'Type ``directory(Permissions)``' - 'For checking if a term is an atom and an existing directory with the listed permissions ({``read``, ``write``, ``execute``}).',
			'Type ``environment_variable``' - 'For checking if a term is an atom and an existing environment variable.'
		],
		see_also is [osp, os, type]
	]).

	:- multifile(type::type/1).
	% clauses for the type::type/1 predicate must always be defined with
	% an instantiated first argument to keep calls deterministic by taking
	% advantage of first argument indexing
	type::type(file).
	type::type(file(_Extensions)).
	type::type(file(_Extensions, _Permissions)).
	type::type(directory).
	type::type(directory(_Permissions)).
	type::type(environment_variable).

	:- multifile(type::check/2).
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
		;	Extensions == [] ->
			true
		;	os::decompose_file_name(Term, _, _, Extension),
			list::member(Extension, Extensions) ->
			true
		;	throw(domain_error(file(Extensions), Term))
		).
	type::check(file(Extensions, Permissions), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	\+ os::file_exists(Term) ->
			throw(existence_error(file, Term))
		;	Extensions == [] ->
			check_permissions(Permissions, Extensions, Term)
		;	os::decompose_file_name(Term, _, _, Extension),
			list::member(Extension, Extensions) ->
			check_permissions(Permissions, Extensions, Term)
		;	throw(domain_error(file(Extensions, Permissions), Term))
		).
	type::check(directory, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	os::directory_exists(Term) ->
			true
		;	throw(existence_error(directory, Term))
		).
	type::check(directory(Permissions), Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	\+ atom(Term) ->
			throw(type_error(atom, Term))
		;	\+ os::directory_exists(Term) ->
			throw(existence_error(directory, Term))
		;	check_permissions(Permissions, Term)
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

	check_permissions([], _, _).
	check_permissions([Permission| Permissions], Extensions, File) :-
		(	os::file_permission(File, Permission) ->
			check_permissions(Permissions, Extensions, File)
		;	throw(domain_error(file(Extensions, Permissions), File))
		).

	check_permissions([], _).
	check_permissions([Permission| Permissions], Directory) :-
		(	os::file_permission(Directory, Permission) ->
			check_permissions(Permissions, Directory)
		;	throw(domain_error(directory(Permissions), Directory))
		).

:- end_category.
