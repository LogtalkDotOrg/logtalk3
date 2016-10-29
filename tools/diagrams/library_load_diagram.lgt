%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.2,
		author is 'Paulo Moura',
		date is 2016/10/29,
		comment is 'Predicates for generating library loading dependency diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [
		member/2
	]).

	% first, output the library node
	output_library(Library, Directory, Options) :-
		^^add_link_options(Directory, Options, LinkingOptions),
		^^omit_path_prefix(Directory, Options, Relative),
		^^add_library_documentation_url(logtalk, LinkingOptions, Relative, NodeOptions0),
		(	logtalk::loaded_file_property(File, library(Library)),
			(	logtalk::loaded_file_property(File, object(_))
			;	logtalk::loaded_file_property(File, protocol(_))
			;	logtalk::loaded_file_property(File, category(_))
			) ->
			entity_diagram::diagram_name_suffix(Suffix),
			^^add_node_zoom_option(Library, Suffix, Options, NodeOptions0, NodeOptions)
		;	NodeOptions = NodeOptions0
		),
		(	member(directory_paths(true), Options) ->
			^^output_node(Relative, Library, library, [Relative], library, NodeOptions)
		;	^^output_node(Relative, Library, library, [], library, NodeOptions)
		),
		^^remember_included_library(Library, Directory),
		fail.
	% second, output edges for all libraries loaded by files in this library
	output_library(_Library, Directory, Options) :-
		% any library Logtalk or Prolog file may load other files
		(	logtalk::loaded_file_property(File, directory(Directory))
		;	modules_diagram_support::loaded_file_property(File, directory(Directory))
		),
		% look for a file in another library that have this file as parent
		(	logtalk::loaded_file_property(Other, parent(File)),
			logtalk::loaded_file_property(Other, directory(OtherDirectory)),
			OtherDirectory \== Directory
		;	modules_diagram_support::loaded_file_property(Other, parent(File)),
			modules_diagram_support::loaded_file_property(Other, directory(OtherDirectory)),
			OtherDirectory \== Directory,
			% not a Logtalk generated intermediate Prolog file
			\+ logtalk::loaded_file_property(_, target(Other))
		),
		^^omit_path_prefix(Directory, Options, Relative),
		^^omit_path_prefix(OtherDirectory, Options, OtherRelative),
		% edge not previously recorded
		\+ ^^edge(Relative, OtherRelative, _, _, _),
		(	logtalk::loaded_file_property(Other, library(OtherLibrary)) ->
			^^remember_referenced_logtalk_library(OtherLibrary, OtherDirectory)
		;	modules_diagram_support::loaded_file_property(Other, directory(OtherDirectory)),
			logtalk_library_path(OtherLibrary, _),
			logtalk::expand_library_path(OtherLibrary, OtherDirectory) ->
			% file found in a directory corresponding to a Logtalk library
			^^remember_referenced_logtalk_library(OtherLibrary, OtherDirectory)
		;	modules_diagram_support::module_property(OtherLibrary, file(Other)) ->
			% Prolog library module 
			^^remember_referenced_prolog_library(OtherLibrary, OtherDirectory)
		;	% as last resort, use the basename of the file
			modules_diagram_support::loaded_file_property(Other, directory(OtherDirectory)),
			modules_diagram_support::loaded_file_property(Other, basename(OtherLibrary))
		),
		^^save_edge(Relative, OtherRelative, [loads], loads_library, [tooltip(loads)| Options]),
		fail.
	output_library(_, _, _).

	% by default, diagram layout is top to bottom:
	default_option(layout(top_to_bottom)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, don't generate cluster, file, and entity URLs:
	default_option(url_prefixes('', '')).
	% by default, don't omit any path prefixes when printing paths:
	default_option(omit_path_prefixes([])).
	% by default, don't print directory paths:
	default_option(directory_paths(false)).
	% by default, print relation labels:
	default_option(relation_labels(true)).
	% by default, don't print node type captions
	default_option(node_type_captions(false)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any library sub-directories:
	default_option(exclude_libraries([])).
	% by default, use a 'directory_index.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('directory_index.html', '#')).
	% by default, don't zooming into libraries and entities:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for zoom linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_name_suffix('_library_load_diagram').

:- end_object.



:- object(library_load_diagram,
	extends(library_load_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2016/02/29,
		comment is 'Predicates for generating library loading dependency diagrams in DOT format.'
	]).

:- end_object.
