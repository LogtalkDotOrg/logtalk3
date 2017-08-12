%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.8,
		author is 'Paulo Moura',
		date is 2017/08/10,
		comment is 'Predicates for generating file loading dependency diagrams. A dependency exists when a file loads or includes another file.',
		parnames is ['Format'],
		see_also is [file_dependency_diagram(_), library_dependency_diagram(_)]
	]).

	:- uses(list, [
		member/2
	]).

	% output the file node
	output_file(Path, Basename, _, Options) :-
		^^filter_file_extension(Basename, Options, Name),
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	member(directory_paths(true), Options) ->
			^^output_node(Relative, Name, file, [Relative], file, LinkingOptions)
		;	^^output_node(Relative, Name, file, [], file, LinkingOptions)
		),
		^^remember_included_file(Path),
		fail.
	% output nodes for all included files
	output_file(Path, _, _, Options) :-
		logtalk::loaded_file_property(Path, includes(IncludePath)),
		os::decompose_file_name(IncludePath, _, Basename),
		^^filter_file_extension(Basename, Options, Name),
		^^add_link_options(IncludePath, Options, LinkingOptions),
		^^omit_path_prefix(IncludePath, Options, Relative),
		(	member(directory_paths(true), Options) ->
			^^output_node(Relative, Name, file, [Relative], file, LinkingOptions)
		;	^^output_node(Relative, Name, file, [], file, LinkingOptions)
		),
		^^remember_included_file(IncludePath),
		fail.
	% output edges for all files loaded by this file
	output_file(Path, _, _, Options) :-
		^^omit_path_prefix(Path, Options, Relative),
		logtalk::loaded_file_property(OtherPath, parent(Path)),
			^^remember_referenced_logtalk_file(OtherPath),
			^^omit_path_prefix(OtherPath, Options, OtherRelative),
			^^save_edge(Relative, OtherRelative, [loads], loads_file, [tooltip(loads)| Options]),
		fail.
	% output edges for all files included by this file
	output_file(Path, _, _, Options) :-
		^^omit_path_prefix(Path, Options, Relative),
		logtalk::loaded_file_property(Path, includes(IncludePath)),
			^^remember_referenced_logtalk_file(IncludePath),
			^^omit_path_prefix(IncludePath, Options, IncludeRelative),
			^^save_edge(Relative, IncludeRelative, [includes], includes_file, [tooltip(includes)| Options]),
		fail.
	% output edges for loaded Prolog module files
	output_file(Path, _, _, Options) :-
		^^omit_path_prefix(Path, Options, Relative),
		modules_diagram_support::loaded_file_property(OtherPath, parent(Path)),
			(	logtalk::loaded_file_property(OriginalPath, target(OtherPath)) ->
				(	% make sure we don't get circular references as Path can be a Logtalk
					% file and the generated intermediate Prolog file may have a link to
					% it depending on the backend Prolog compiler
					OriginalPath \== Path ->
					% Prolog file loading a Logtalk generated intermediate Prolog file
					^^remember_referenced_logtalk_file(OriginalPath),
					^^omit_path_prefix(OriginalPath, Options, OriginalRelative),
					^^save_edge(Relative, OriginalRelative, [loads], loads_file, [tooltip(loads)| Options])
				;	true
				)
			;	% Prolog file loading a non-Logtalk generated Prolog file
				^^remember_referenced_prolog_file(OtherPath),
				^^omit_path_prefix(OtherPath, Options, OtherRelative),
				^^save_edge(Relative, OtherRelative, [loads], loads_file, [tooltip(loads)| Options])
			),
		fail.
	output_file(_, _, _, _).

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
	% by default, print file name extensions:
	default_option(file_extensions(true)).
	% by default, print relation labels:
	default_option(relation_labels(true)).
	% by default, print node type captions
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, exclude only the "startup" library:
	default_option(exclude_libraries([startup])).
	% by default, don't zooming into libraries and entities:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for zoom linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_name_suffix('_file_load_diagram').

:- end_object.



:- object(file_load_diagram,
	extends(file_load_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/08,
		comment is 'Predicates for generating file loading dependency diagrams in DOT format. A dependency exists when a file loads or includes another file.',
		see_also is [file_dependency_diagram, library_dependency_diagram]
	]).

:- end_object.
