%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(directory_dependency_diagram(Format),
	imports(directory_diagram(Format))).

	:- info([
		version is 1:19:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Predicates for generating directory dependency diagrams. A dependency exists when an entity in one directory makes a reference to an entity in another directory.',
		parameters is ['Format' - 'Graph language file format'],
		see_also is [directory_load_diagram(_), file_load_diagram(_), library_load_diagram(_)]
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- private(sub_diagram_/2).
	:- dynamic(sub_diagram_/2).

	% first, output the directory node
	output_library(Project, Directory, Options) :-
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		(	(	logtalk::loaded_file_property(_, directory(Directory))
			;	modules_diagram_support::loaded_file_property(_, directory(Directory))
			) ->
			parameter(1, Format),
			file_dependency_diagram(Format)::diagram_name_suffix(Suffix),
			^^add_node_zoom_option(Project, Suffix, LinkingOptions, NodeOptions),
			assertz((sub_diagram_(Project, Directory)))
		;	% no files for this directory
			NodeOptions = LinkingOptions
		),
		^^output_node(Directory, Relative, directory, [], directory, NodeOptions),
		^^remember_included_directory(Directory),
		fail.
	% second, output edges for all directories that this directory refers to
	output_library(_, Directory, Options) :-
		depends_directory(Directory, OtherDirectory, Kind),
		memberchk(exclude_directories(ExcludedDirectories), Options),
		\+ member(OtherDirectory, ExcludedDirectories),
		% ensure that this dependency is not already recorded
		\+ ^^edge(Directory, OtherDirectory, _, _, _),
		^^save_edge(Directory, OtherDirectory, [depends], depends_on_directory, [tooltip(depends)| Options]),
		(	Kind == module ->
			^^remember_referenced_prolog_directory(OtherDirectory)
		;	^^remember_referenced_logtalk_directory(OtherDirectory)
		),
		fail.
	output_library(_, _, _).

	depends_directory(Directory, OtherDirectory, Kind) :-
		(	object_property(Object, file(Basename, Directory)),
			depends_object(Object, Kind, Other)
		;	protocol_property(Protocol, file(Basename, Directory)),
			depends_protocol(Protocol, Kind, Other)
		;	category_property(Category, file(Basename, Directory)),
			depends_category(Category, Kind, Other)
		;	modules_diagram_support::module_property(Module, file(Basename, Directory)),
			depends_module(Module, Kind, Other)
		),
		entity_directory(Kind, Other, OtherDirectory),
		OtherDirectory \== Directory.

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

	entity_directory(object, Entity, Directory) :-
		object_property(Entity, file(_, Directory)).
	entity_directory(protocol, Entity, Directory) :-
		protocol_property(Entity, file(_, Directory)).
	entity_directory(category, Entity, Directory) :-
		category_property(Entity, file(_, Directory)).
	entity_directory(module, Entity, Directory) :-
		modules_diagram_support::module_property(Entity, file(_, Directory)).

	output_sub_diagrams(Options) :-
		parameter(1, Format),
		memberchk(zoom(true), Options),
		sub_diagram_(Project, Directory),
		file_dependency_diagram(Format)::directory(Project, Directory, Options),
		fail.
	output_sub_diagrams(_).

	reset :-
		^^reset,
		retractall(sub_diagram_(_, _)).

	% by default, diagram layout is top to bottom:
	default_option(layout(top_to_bottom)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, don't omit any prefix when printing paths:
	default_option(omit_path_prefixes([])).
	% by default, don't print directory paths:
	default_option(directory_paths(false)).
	% by default, print relation labels:
	default_option(relation_labels(true)).
	% by default, print external nodes:
	default_option(externals(true)).
	% by default, print node type captions:
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_description('Directory dependency diagram').

	diagram_name_suffix('_directory_dependency_diagram').

	message_diagram_description('directory dependency').

:- end_object.



:- object(directory_dependency_diagram,
	extends(directory_dependency_diagram(dot))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-04-07,
		comment is 'Predicates for generating directory dependency diagrams in DOT format.',
		see_also is [directory_load_diagram, file_load_diagram]
	]).

:- end_object.
