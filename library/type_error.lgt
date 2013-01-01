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



:- category(type_checking).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2009/02/18,
		comment is 'Description']).

	:- public(type_check/2).
	:- mode(type_check(@object_identifier, @term), one).
	:- info(type_check/2, [
		comment is 'Description',
		argnames is ['Type', 'Term']]).

	type_check(Type, Term) :-	% default definition
		(	Type::valid(Term) ->
			true
		;	var(Term) ->	
			this(This),
			throw(error(instantiation_error, Term), :type_check(Type, Term), This))
		;	this(This),
			throw(error(type_error(Type, Term), :type_check(Type, Term), This))
		).

:- end_category.



:- object(type_error_hook).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2009/02/19,
		comment is 'Description']).

	% uses mode/2 predicate directives to add type an mode checking code to all predicate clauses

:- end_object.
