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


:- object(file_load_diagram(Format),
	imports(file_diagram(Format))).

	:- info([
		version is 2.1,
		author is 'Paulo Moura',
		date is 2016/02/26,
		comment is 'Predicates for generating file loading dependency diagrams.',
		parnames is ['Format']
	]).

	:- uses(list, [member/2, memberchk/2]).

	% first, output the file node
	output_file(Path, Basename, _Directory, Options) :-
		^^add_link_options(Path, Options, LinkingOptions),
		^^omit_path_prefix(Path, Options, Relative),
		(	memberchk(directory_paths(true), Options) ->
			^^output_node(Relative, Basename, file, [Relative], file, LinkingOptions)
		;	^^output_node(Relative, Basename, file, [], file, LinkingOptions)
		),
		^^remember_included_file(Path),
		fail.
	% second, output edges for all files loaded by this file
	output_file(Path, _, _, Options) :-
		^^omit_path_prefix(Path, Options, Relative),
		logtalk::loaded_file_property(OtherPath, parent(Path)),
			^^remember_referenced_logtalk_file(OtherPath),
			^^omit_path_prefix(OtherPath, Options, OtherRelative),
			^^save_edge(Relative, OtherRelative, [loads], loads_file, [tooltip(loads)| Options]),
		fail.
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

	diagram_name_suffix('_file_load_diagram').

:- end_object.



:- object(file_load_diagram,
	extends(file_load_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/08,
		comment is 'Predicates for generating file loading dependency diagrams in DOT format.'
	]).

:- end_object.
