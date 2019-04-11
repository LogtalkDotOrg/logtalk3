%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(uses_diagram(Format),
	extends(entity_diagram(Format))).

	:- info([
		version is 2.8,
		author is 'Paulo Moura',
		date is 2019/04/11,
		comment is 'Predicates for generating entity diagrams with only uses and use_module relation edges.',
		parnames is ['Format'],
		see_also is [entity_diagram(_), inheritance_diagram(_), xref_diagram(_)]
	]).

	% by default, diagram layout is bottom to top:
	default_option(layout(bottom_to_top)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, print entity public predicates:
	default_option(interface(true)).
	% by default, print file labels:
	default_option(file_labels(true)).
	% by default, don't write inheritance links:
	default_option(inheritance_relations(false)).
	% by default, don't write provide links:
	default_option(provide_relations(false)).
	% by default, write cross-referencing links:
	default_option(xref_relations(true)).
	% by default, print file name extensions:
	default_option(file_extensions(true)).
	% by default, print entity relation labels:
	default_option(relation_labels(true)).
	% by default, don't write cross-referencing calls:
	default_option(xref_calls(false)).
	% by default, print node type captions
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, use a home directory URL for the source code:
	default_option(url_prefixes(URL, '')) :-
		logtalk::expand_library_path(home, HOME),
		atom_concat('file://', HOME, URL).
	% by default, omit the home directory path prefix when printing paths:
	default_option(omit_path_prefixes([HOME])) :-
		logtalk::expand_library_path(home, HOME).
	% by default, use a '.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('.html', '#')).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_name_suffix('_uses_diagram').

	message_diagram_description('uses').

:- end_object.



:- object(uses_diagram,
	extends(uses_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/15,
		comment is 'Predicates for generating entity diagrams in DOT format with only uses and use_module relation edges.',
		see_also is [entity_diagram, inheritance_diagram, xref_diagram]
	]).

:- end_object.
