%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(file_load_diagram(Format),
	imports(file_diagram(Format))).

	:- info([
		version is 2:32:0,
		author is 'Paulo Moura',
		date is 2024-12-04,
		comment is 'Predicates for generating file loading dependency diagrams. A dependency exists when a file loads or includes another file.',
		parameters is ['Format' - 'Graph language file format.'],
		see_also is [file_dependency_diagram(_), directory_dependency_diagram(_), library_dependency_diagram(_)]
	]).

	:- private(sub_diagram_/1).
	:- dynamic(sub_diagram_/1).
	:- mode(sub_diagram_(?atom), zero_or_more).
	:- info(sub_diagram_/1, [
		comment is 'Table of file sub-diagrams to support their generation.',
		argnames is ['File']
	]).

	:- uses(list, [
		member/2
	]).

	% output the file node
	output_file(Path, Basename, _Directory, Options) :-
		^^filter_file_extension(Basename, Options, Name),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	(	logtalk::loaded_file_property(Path, object(_))
			;	logtalk::loaded_file_property(Path, protocol(_))
			;	logtalk::loaded_file_property(Path, category(_))
			;	modules_diagram_support::module_property(_, file(Path))
			) ->
			parameter(1, Format),
			entity_diagram(Format)::diagram_name_suffix(Suffix),
			os::decompose_file_name(Path, _, File, _),
			^^add_node_zoom_option(File, Suffix, LinkingOptions, NodeOptions)
		;	% file doesn't define any entity
			NodeOptions = LinkingOptions
		),
		assertz(sub_diagram_(Path)),
		(	member(directory_paths(true), Options) ->
			^^output_node(Path, Name, file, [Relative], file, NodeOptions)
		;	^^output_node(Path, Name, file, [], file, NodeOptions)
		),
		^^remember_included_file(Path),
		fail.
	% output nodes for all included files
	output_file(Path, _, _, Options) :-
		logtalk::loaded_file_property(Path, includes(IncludePath)),
		os::decompose_file_name(IncludePath, _, IncludeBasename),
		^^not_excluded_file(IncludePath, IncludeBasename, Options),
		^^filter_file_extension(IncludeBasename, Options, Name),
		^^add_link_options(IncludePath, Options, LinkingOptions),
		^^omit_path_prefix(IncludePath, Options, Relative),
		(	member(directory_paths(true), Options) ->
			^^output_node(IncludePath, Name, file, [Relative], file, LinkingOptions)
		;	^^output_node(IncludePath, Name, file, [], file, LinkingOptions)
		),
		^^remember_included_file(IncludePath),
		fail.
	% output edges for all files loaded by this file
	output_file(Path, _, _, Options) :-
		logtalk::loaded_file_property(OtherPath, parent(Path)),
		logtalk::loaded_file_property(OtherPath, basename(OtherBasename)),
			^^not_excluded_file(OtherPath, OtherBasename, Options),
			^^remember_referenced_logtalk_file(OtherPath),
			^^save_edge(Path, OtherPath, [loads], loads_file, [tooltip(loads)| Options]),
		fail.
	% output edges for all files included by this file
	output_file(Path, _, _, Options) :-
		logtalk::loaded_file_property(Path, includes(IncludePath)),
		logtalk::loaded_file_property(IncludePath, basename(IncludeBasename)),
			^^not_excluded_file(IncludePath, IncludeBasename, Options),
			^^remember_referenced_logtalk_file(IncludePath),
			^^save_edge(Path, IncludePath, [includes], includes_file, [tooltip(includes)| Options]),
		fail.
	% output edges for loaded Prolog module files
	output_file(Path, _, _, Options) :-
		modules_diagram_support::loaded_file_property(OtherPath, parent(Path)),
		modules_diagram_support::loaded_file_property(OtherPath, basename(OtherBasename)),
			^^not_excluded_file(OtherPath, OtherBasename, Options),
			(	logtalk::loaded_file_property(OriginalPath, target(OtherPath)) ->
				(	% make sure we don't get circular references as Path can be a Logtalk
					% file and the generated intermediate Prolog file may have a link to
					% it depending on the backend Prolog compiler
					OriginalPath \== Path ->
					% Prolog file loading a Logtalk generated intermediate Prolog file
					^^remember_referenced_logtalk_file(OriginalPath),
					^^save_edge(Path, OtherPath, [loads], loads_file, [tooltip(loads)| Options])
				;	true
				)
			;	% Prolog file loading a non-Logtalk generated Prolog file
				^^remember_referenced_prolog_file(OtherPath),
				^^save_edge(Path, OtherPath, [loads], loads_file, [tooltip(loads)| Options])
			),
		fail.
	output_file(_, _, _, _).

	output_sub_diagrams(Options) :-
		parameter(1, Format),
		^^option(zoom(true), Options),
		entity_diagram(Format)::default_option(layout(Layout)),
		sub_diagram_(File),
		entity_diagram(Format)::file(File, [layout(Layout)| Options]),
		fail.
	output_sub_diagrams(_).

	reset :-
		^^reset,
		retractall(sub_diagram_(_)).

	% by default, diagram layout is top to bottom:
	default_option(layout(top_to_bottom)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, don't print Logtalk and backend version data:
	default_option(versions(false)).
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
	% by default, exclude only the "scratch_directory" library:
	default_option(exclude_libraries([scratch_directory])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_description('File load diagram').

	diagram_name_suffix('_file_load_diagram').

	message_diagram_description('file load').

:- end_object.



:- object(file_load_diagram,
	extends(file_load_diagram(dot))).

	:- info([
		version is 2:01:0,
		author is 'Paulo Moura',
		date is 2019-06-13,
		comment is 'Predicates for generating file loading dependency diagrams in DOT format. A dependency exists when a file loads or includes another file.',
		see_also is [file_dependency_diagram, directory_dependency_diagram, library_dependency_diagram]
	]).

:- end_object.
