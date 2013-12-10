%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(diagram).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/09,
		comment is 'Predicates for generating diagrams.'
	]).

	:- public(all/1).
	:- mode(all(+atom), one).
	:- info(all/1, [
		comment is 'Creates a diagram for all loaded files using the specified options.',
		argnames is ['Options']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Creates a diagram for all loaded files using default options.'
	]).

	all :-
		::all([]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates a diagram for a library and its sub-libraries using default options.',
		argnames is ['Library']
	]).

	rlibrary(Library) :-
		::rlibrary(Library, []).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates a diagram for a library using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for a library using default options.',
		argnames is ['Library']
	]).

	library(Library) :-
		::library(Library, []).

	:- public(default_options/1).
	:- mode(default_options(-list), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOptions']
	]).

	default_options(DefaultOptions) :-
		::merge_options([], DefaultOptions).

	:- protected(merge_options/2).
	:- mode(merge_options(+list, -list), one).
	:- info(merge_options/2, [
		comment is 'Returns the list of options used when generating a diagram.',
		argnames is ['UserOptions', 'DefaultOptions']
	]).

	:- protected(not_excluded_file/3).
	:- mode(not_excluded_file(+list(atom), +atom, +atom), zero_or_one).
	:- info(not_excluded_file/3, [
		comment is 'Returns the list of options used when generating a diagram.',
		argnames is ['ExcludedFiles', 'Path', 'Basename']
	]).

	not_excluded_file([], _, _).
	not_excluded_file([ExcludedFile| ExcludedFiles], Path, Basename) :-
		% files in the exclusion list may be given by full path or by basename
		\+ member(Path, [ExcludedFile| ExcludedFiles]),
		\+ member(Basename, [ExcludedFile| ExcludedFiles]),
		% files in the exclusion list may be given with or without extension
		atom_concat(Source1, '.lgt', Path),
		\+ member(Source1, [ExcludedFile| ExcludedFiles]),
		atom_concat(Source2, '.logtalk', Path),
		\+ member(Source2, [ExcludedFile| ExcludedFiles]),
		atom_concat(Source3, '.lgt', Basename),
		\+ member(Source3, [ExcludedFile| ExcludedFiles]),
		atom_concat(Source4, '.logtalk', Basename),
		\+ member(Source4, [ExcludedFile| ExcludedFiles]).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
