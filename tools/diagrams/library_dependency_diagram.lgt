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


:- object(library_dependency_diagram(Format),
	imports(library_diagram(Format))).

	:- info([
		version is 2:29:0,
		author is 'Paulo Moura',
		date is 2022-05-18,
		comment is 'Predicates for generating library dependency diagrams. A dependency exists when an entity in one library makes a reference to an entity in another library.',
		parameters is ['Format' - 'Graph language file format'],
		see_also is [library_load_diagram(_), directory_load_diagram(_), file_load_diagram(_), entity_diagram(_)]
	]).

	:- uses(list, [
		member/2
	]).

	:- private(sub_diagrams_/1).
	:- dynamic(sub_diagrams_/1).

	% first, output the library node
	output_library(Library, Directory, Options) :-
		parameter(1, Format),
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		^^add_library_documentation_url(logtalk, LinkingOptions, Library, NodeOptions0),
		(	logtalk::loaded_file_property(File, library(Library)),
			(	logtalk::loaded_file_property(File, object(_))
			;	logtalk::loaded_file_property(File, protocol(_))
			;	logtalk::loaded_file_property(File, category(_))
			) ->
			entity_diagram(Format)::diagram_name_suffix(Suffix),
			^^add_node_zoom_option(Library, Suffix, NodeOptions0, NodeOptions),
			assertz(sub_diagrams_(Library))
		;	% no entities for this library; entity diagram empty
			NodeOptions = NodeOptions0
		),
		(	member(directory_paths(true), Options) ->
			^^output_node(Directory, Library, library, [Relative], library, NodeOptions)
		;	^^output_node(Directory, Library, library, [], library, NodeOptions)
		),
		^^remember_included_library(Library, Directory),
		fail.
	% second, output edges for all libraries that this library refers to
	output_library(Library, Directory, Options) :-
		depends_library(Library, Directory, OtherLibrary, OtherDirectory, Kind),
		^^option(exclude_libraries(ExcludedLibraries), Options),
		\+ member(OtherLibrary, ExcludedLibraries),
		% ensure that this dependency is not already recorded
		\+ ^^edge(Directory, OtherDirectory, _, _, _),
		^^save_edge(Directory, OtherDirectory, [depends], depends_on_library, [tooltip(depends)| Options]),
		(	Kind == module ->
			^^remember_referenced_prolog_library(OtherLibrary, OtherDirectory)
		;	^^remember_referenced_logtalk_library(OtherLibrary, OtherDirectory)
		),
		fail.
	output_library(_, _, _).

	depends_library(Library, Directory, OtherLibrary, OtherDirectory, Kind) :-
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
		atom_concat(OtherDirectory, OtherBasename, OtherPath),
		(	Kind == module ->
			modules_diagram_support::module_property(Other, file(OtherPath)),
			OtherLibrary = Other
		;	logtalk::loaded_file_property(OtherPath, library(OtherLibrary))
		),
		OtherLibrary \== Library.

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

	output_sub_diagrams(Options) :-
		parameter(1, Format),
		^^option(zoom(true), Options),
		sub_diagrams_(Library),
		entity_diagram(Format)::library(Library, Options),
		fail.
	output_sub_diagrams(_).

	reset :-
		^^reset,
		::retractall(sub_diagrams_(_)).

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
	% by default, print relation labels:
	default_option(relation_labels(true)).
	% by default, print external nodes:
	default_option(externals(true)).
	% by default, print node type captions:
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).
	% by default, use a 'library_index.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('library_index.html', '#')).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_description('Library dependency diagram').

	diagram_name_suffix('_library_dependency_diagram').

	message_diagram_description('library dependency').

:- end_object.



:- object(library_dependency_diagram,
	extends(library_dependency_diagram(dot))).

	:- info([
		version is 2:01:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Predicates for generating library dependency diagrams in DOT format.',
		see_also is [library_load_diagram, file_load_diagram, entity_diagram]
	]).

:- end_object.
