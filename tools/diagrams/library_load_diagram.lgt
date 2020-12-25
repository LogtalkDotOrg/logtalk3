%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(library_load_diagram(Format),
	imports(library_diagram(Format))).

	:- info([
		version is 2:27:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Predicates for generating library loading dependency diagrams.',
		parameters is ['Format' - 'Graph language file format'],
		see_also is [library_dependency_diagram(_), directory_dependency_diagram(_), file_dependency_diagram(_), entity_diagram(_)]
	]).

	:- uses(list, [
		member/2, memberchk/2
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
	% second, output edges for all libraries loaded by files in this library
	output_library(Library, Directory, Options) :-
		% any Logtalk or Prolog library file may load other files
		(	logtalk::loaded_file_property(File, directory(Directory))
		;	modules_diagram_support::loaded_file_property(File, directory(Directory))
		),
		% look for a file in another library that have this file as parent
		(	memberchk(exclude_libraries(ExcludedLibraries), Options),
			logtalk::loaded_file_property(Other, parent(File)),
			logtalk::loaded_file_property(Other, library(OtherLibrary)),
			Library \== OtherLibrary,
			\+ member(OtherLibrary, ExcludedLibraries),
			logtalk::loaded_file_property(Other, directory(OtherDirectory))
		;	modules_diagram_support::loaded_file_property(Other, parent(File)),
			modules_diagram_support::loaded_file_property(Other, directory(OtherDirectory)),
			OtherDirectory \== Directory,
			% not a Logtalk generated intermediate Prolog file
			\+ logtalk::loaded_file_property(_, target(Other))
		),
		% edge not previously recorded
		\+ ^^edge(Directory, OtherDirectory, _, _, _),
		(	logtalk::loaded_file_property(Other, library(OtherLibrary)) ->
			^^remember_referenced_logtalk_library(OtherLibrary, OtherDirectory)
		;	modules_diagram_support::loaded_file_property(Other, directory(OtherDirectory)),
			logtalk_library_path(OtherLibrary, _),
			logtalk::expand_library_path(OtherLibrary, OtherDirectory) ->
			% file found in a directory corresponding to a Logtalk library
			memberchk(exclude_libraries(ExcludedLibraries), Options),
			\+ member(OtherLibrary, ExcludedLibraries),
			^^remember_referenced_logtalk_library(OtherLibrary, OtherDirectory)
		;	modules_diagram_support::module_property(OtherLibrary, file(Other)) ->
			% Prolog library module
			^^remember_referenced_prolog_library(OtherLibrary, OtherDirectory)
		;	% as last resort, use the basename of the file
			modules_diagram_support::loaded_file_property(Other, directory(OtherDirectory)),
			modules_diagram_support::loaded_file_property(Other, basename(OtherLibrary))
		),
		^^save_edge(Directory, OtherDirectory, [loads], loads_library, [tooltip(loads)| Options]),
		fail.
	output_library(_, _, _).

	output_sub_diagrams(Options) :-
		parameter(1, Format),
		memberchk(zoom(true), Options),
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
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).
	% by default, use a 'library_index.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('library_index.html', '#')).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_description('Library load diagram').

	diagram_name_suffix('_library_load_diagram').

	message_diagram_description('library load').

:- end_object.



:- object(library_load_diagram,
	extends(library_load_diagram(dot))).

	:- info([
		version is 2:01:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Predicates for generating library loading dependency diagrams in DOT format.',
		see_also is [library_dependency_diagram, file_dependency_diagram, entity_diagram]
	]).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
:- endif.
