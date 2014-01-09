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


:- object(file_uses_diagram(Format),
	imports(file_diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/09,
		comment is 'Predicates for generating file contents dependency diagrams.',
		parnames is ['Format']
	]).

	output_file(Path, Basename, Directory, Options) :-
		^^linking_options(Path, Options, LinkingOptions),
		(	member(directory_paths(true), Options) ->
			member(omit_path_prefix(Prefix), Options),
			(	atom_concat(Prefix, Relative, Directory) ->
				^^output_node(Path, Basename, [Relative], file, LinkingOptions)
			;	^^output_node(Path, Basename, [Directory], file, LinkingOptions)
			)
		;	^^output_node(Path, Basename, [], file, LinkingOptions)
		),
		^^remember_included_file(Path),
		fail.
	output_file(Path, Basename, Directory, Options) :-
		depends_file(Basename, Directory, OtherPath, Kind),
		\+ ^^edge(Path, OtherPath, _, _, _),
		(	Kind == module ->
			^^remember_referenced_prolog_file(OtherPath),
			^^save_edge(Path, OtherPath, [use_module], calls_predicate, [tooltip(use_module)| Options])
		;	^^remember_referenced_logtalk_file(OtherPath),
			^^save_edge(Path, OtherPath, [uses], calls_predicate, [tooltip(uses)| Options])
		),
		fail.		
	output_file(_, _, _, _).

	depends_file(Basename, Directory, OtherPath, Kind) :-
		(	object_property(Object, file(Basename, Directory)),
			depends_object(Object, Kind, Other)
		;	protocol_property(Protocol, file(Basename, Directory)),
			depends_protocol(Protocol, Kind, Other)
		;	category_property(Category, file(Basename, Directory)),
			depends_category(Category, Kind, Other)
		),
		entity_basename_directory(Kind, Other, OtherBasename, OtherDirectory),
		(	OtherBasename \== Basename ->
			true
		;	OtherDirectory \== Directory
		),
		atom_concat(OtherDirectory, OtherBasename, OtherPath).

	depends_object(Object, object, Other) :-
		object_property(Object, calls(Other::_,_)), nonvar(Other).
	depends_object(Object, module, Other) :-
		object_property(Object, calls(':'(Other,_),_)), nonvar(Other).
	depends_object(Object, object, Other) :-
		extends_object(Object, Other).
	depends_object(Object, object, Other) :-
		instantiates_class(Object, Other).
	depends_object(Object, object, Other) :-
		specializes_class(Object, Other).
	depends_object(Object, protocol, Other) :-
		implements_protocol(Object, Other).
	depends_object(Object, category, Other) :-
		imports_category(Object, Other).
	depends_object(Object, category, Other) :-
		complements_object(Other, Object).

	depends_protocol(Protocol, object, Other) :-
		protocol_property(Protocol, calls(Other::_,_)), nonvar(Other).
	depends_protocol(Protocol, module, Other) :-
		protocol_property(Protocol, calls(':'(Other,_),_)), nonvar(Other).
	depends_protocol(Protocol, object, Other) :-
		extends_protocol(Protocol, Other).

	depends_category(Category, object, Other) :-
		category_property(Category, calls(Other::_,_)), nonvar(Other).
	depends_category(Category, module, Other) :-
		category_property(Category, calls(':'(Other,_),_)), nonvar(Other).
	depends_category(Category, category, Other) :-
		extends_category(Category, Other).
	depends_category(Category, protocol, Other) :-
		implements_protocol(Category, Other).

	entity_basename_directory(object, Entity, Basename, Directory) :-
		object_property(Entity, file(Basename, Directory)).
	entity_basename_directory(protocol, Entity, Basename, Directory) :-
		protocol_property(Entity, file(Basename, Directory)).
	entity_basename_directory(category, Entity, Basename, Directory) :-
		category_property(Entity, file(Basename, Directory)).
	entity_basename_directory(module, Entity, Basename, Directory) :-
		prolog_modules_diagram_support::module_property(Entity, file(Path)),
		prolog_modules_diagram_support::source_file_property(Path, basename(Basename)),
		prolog_modules_diagram_support::source_file_property(Path, directory(Directory)).

	% by default, don't generate cluster URLs:
	default_option(url_protocol('')).
	% by default, don't omit a path prefix when printing paths:
	default_option(omit_path_prefix('')).
	% by default, don't print directory paths:
	default_option(directory_paths(false)).
	% by default, print current date:
	default_option(date(true)).
	% by default, print relation labels:
	default_option(relation_labels(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any library sub-directories:
	default_option(exclude_libraries([])).

	diagram_name_suffix('_file_uses_diagram').

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.



:- object(file_uses_diagram,
	extends(file_uses_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/08,
		comment is 'Predicates for generating file dependency diagrams in DOT format.'
	]).

:- end_object.
