%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(file_dependency_diagram(Format),
	imports(file_diagram(Format))).

	:- info([
		version is 2:27:0,
		author is 'Paulo Moura',
		date is 2023-12-25,
		comment is 'Predicates for generating file contents dependency diagrams. A dependency exists when an entity in one file makes a reference to an entity in another file.',
		parameters is ['Format' - 'Graph language file format.'],
		see_also is [file_load_diagram(_), directory_load_diagram(_), library_load_diagram(_)]
	]).

	:- uses(list, [
		member/2
	]).

	% first, output the file node
	output_file(Path, Basename, _Directory, Options) :-
		^^filter_file_extension(Basename, Options, Name),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	member(directory_paths(true), Options) ->
			^^output_node(Path, Name, file, [Relative], file, LinkingOptions)
		;	^^output_node(Path, Name, file, [], file, LinkingOptions)
		),
		^^remember_included_file(Path),
		fail.
	% second, output edges for all files that this file refers to
	output_file(Path, Basename, Directory, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		depends_file(Basename, Directory, OtherPath, Kind),
		os::decompose_file_name(OtherPath, _, OtherBasename),
		::not_excluded_file(ExcludedFiles, OtherPath, OtherBasename),
		% ensure that this dependency is not already recorded
		\+ ^^edge(Path, OtherPath, _, _, _),
			(	Kind == module ->
				^^remember_referenced_prolog_file(OtherPath),
				^^save_edge(Path, OtherPath, [depends], depends_on_file, [tooltip(depends)| Options])
			;	^^remember_referenced_logtalk_file(OtherPath),
				^^save_edge(Path, OtherPath, [depends], depends_on_file, [tooltip(depends)| Options])
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
		;	modules_diagram_support::module_property(Module, file(Basename, Directory)),
			depends_module(Module, Kind, Other)
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
		extends_protocol(Protocol, Other).

	depends_category(Category, object, Other) :-
		category_property(Category, calls(Other::_,_)), nonvar(Other).
	depends_category(Category, module, Other) :-
		category_property(Category, calls(':'(Other,_),_)), nonvar(Other).
	depends_category(Category, category, Other) :-
		extends_category(Category, Other).
	depends_category(Category, protocol, Other) :-
		implements_protocol(Category, Other).

	depends_module(Module, object, Other) :-
		modules_diagram_support::module_property(Module, calls(Other::_,_)), nonvar(Other).
	depends_module(Module, module, Other) :-
		modules_diagram_support::module_property(Module, calls(':'(Other,_),_)), nonvar(Other).

	entity_basename_directory(object, Entity, Basename, Directory) :-
		object_property(Entity, file(Basename, Directory)).
	entity_basename_directory(protocol, Entity, Basename, Directory) :-
		protocol_property(Entity, file(Basename, Directory)).
	entity_basename_directory(category, Entity, Basename, Directory) :-
		category_property(Entity, file(Basename, Directory)).
	entity_basename_directory(module, Entity, Basename, Directory) :-
		modules_diagram_support::module_property(Entity, file(Basename, Directory)).

	% by default, diagram layout is top to bottom:
	default_option(layout(top_to_bottom)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, don't omit any prefix when printing paths:
	default_option(omit_path_prefixes(Prefixes)) :-
		(	logtalk::expand_library_path(home, Home) ->
			Prefixes = [Home]
		;	Prefixes = []
		).
	% by default, don't print directory paths:
	default_option(directory_paths(false)).
	% by default, print file name extensions:
	default_option(file_extensions(true)).
	% by default, print relation labels:
	default_option(relation_labels(true)).
	% by default, print external nodes:
	default_option(externals(true)).
	% by default, print node type captions:
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./dot_dias')).
	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_description('File dependency diagram').

	diagram_name_suffix('_file_dependency_diagram').

	message_diagram_description('file dependency').

:- end_object.



:- object(file_dependency_diagram,
	extends(file_dependency_diagram(dot))).

	:- info([
		version is 2:01:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Predicates for generating file contents dependency diagrams in DOT format. A dependency exists when an entity in one file makes a reference to an entity in another file.',
		see_also is [file_load_diagram, directory_load_diagram, library_load_diagram]
	]).

:- end_object.
