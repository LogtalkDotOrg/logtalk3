%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
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


:- object(inheritance_diagram(Format),
	extends(entity_diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/03,
		comment is 'Predicates for generating entity inheritance diagrams.',
		parnames is ['Format']
	]).

	% by default, print current date:
	default_option(date(true)).
	% by default, print entity public predicates:
	default_option(interface(false)).
	% by default, write inheritance links:
	default_option(inheritance_relations(true)).
	% by default, write cross-referencing links:
	default_option(cross_reference_relations(false)).
	% by default, print entity relation labels:
	default_option(relation_labels(true)).
	% by default, write cross-referencing calls:
	default_option(cross_reference_calls(false)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any library sub-directories:
	default_option(exclude_libraries([])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).

	diagram_name_suffix('_inheritance_diagram').

:- end_object.



:- object(inheritance_diagram,
	extends(inheritance_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/01,
		comment is 'Predicates for generating entity inheritance diagrams in DOT format.'
	]).

:- end_object.
