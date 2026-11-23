%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests(_Format_),
	extends(lgtunit)).

	:- info([
		version is 0:13:0,
		author is 'Paulo Moura',
		date is 2026-11-23,
		comment is 'Unit tests for the "diagrams" tool.'
	]).

	cover(diagram(_)).
	cover(diagrams(_)).
	cover(entity_diagram(_)).
	cover(directory_dependency_diagram(_)).
	cover(directory_diagram(_)).
	cover(directory_load_diagram(_)).
	cover(file_dependency_diagram(_)).
	cover(file_diagram(_)).
	cover(file_load_diagram(_)).
	cover(cytoscapejs_graph_language).
	cover(graph_language_registry).
	cover(inheritance_diagram(_)).
	cover(library_dependency_diagram(_)).
	cover(library_diagram(_)).
	cover(library_load_diagram(_)).
	:- if(current_logtalk_flag(modules, supported)).
		cover(modules_diagram_support).
	:- endif.
	cover(plantuml_graph_language).
	cover(uses_diagram(_)).
	cover(xref_diagram(_)).
	cover(caller_diagram(_)).

	setup :-
		logtalk::expand_library_path(logtalk_user(scratch), Directory),
		os::change_directory(Directory).

	cleanup :-
		logtalk::expand_library_path(logtalk_user(scratch), ScratchDirectory),
		os::path_concat(ScratchDirectory, 'dot_dias', OutputDirectory),
		os::delete_directory_and_contents(OutputDirectory).

	test(d2_graph_link_tooltip, true) :-
		d2_graph_output('Library "name"\\path', [url('https://example.com'), tooltip('Other')], Output),
		sub_atom(Output, _, _, _, 'link: "https://example.com"\n'),
		sub_atom(Output, _, _, _, 'tooltip: "Library \\"name\\"\\\\path"\n').

	test(d2_graph_unlinked_tooltip, true) :-
		d2_graph_output('Library', [url(''), tooltip('Path')], Output),
		\+ sub_atom(Output, _, _, _, 'link:'),
		sub_atom(Output, _, _, _, 'tooltip: Path\n').

	test(d2_node_comment_tooltip, true) :-
		d2_node_output(
			[comment('He said "hi": ${value}\\path\nnext'), tooltip(object), url('https://example.com'), zoom_url('entity.svg')],
			Output
		),
		sub_atom(Output, _, _, _, 'tooltip: "He said \\"hi\\": \\${value}\\\\path\\nnext"\n'),
		sub_atom(Output, _, _, _, '[&#128269;](entity.svg "Zoom")\n'),
		sub_atom(Output, _, _, _, '## [entity](https://example.com "https://example.com")\n').

	test(d2_node_no_comment_tooltip, true) :-
		d2_node_output([url('https://example.com')], Output),
		\+ sub_atom(Output, _, _, _, 'tooltip:'),
		sub_atom(Output, _, _, _, '## [entity](https://example.com "https://example.com")\n').

	test(d2_node_link_tooltip_escaping, true) :-
		d2_node_output([url('https://example.com/a(b)?q="x"&v=1'), zoom_url('some (entity).svg')], Output),
		sub_atom(Output, _, _, _, '[&#128269;](some%20\\(entity\\).svg "Zoom")\n'),
		sub_atom(Output, _, _, _, '## [entity](https://example.com/a\\(b\\)?q=\\"x\\"&amp;v=1 "https://example.com/a(b)?q=\\"x\\"&amp;v=1")\n').

	test(d2_node_unlinked, true) :-
		d2_node_output([comment('Comment'), tooltip(object), url('')], Output),
		sub_atom(Output, _, _, _, 'tooltip: "Comment"\n'),
		sub_atom(Output, _, _, _, '## entity\n'),
		sub_atom(Output, _, _, _, '"entity_".text: {\ntooltip: "object"\n}\n').

	test(d2_node_tooltip_without_comment_or_url, true) :-
		d2_node_output([tooltip('Object path'), url('')], Output),
		sub_atom(Output, _, _, _, 'style.stroke-dash: 0\nlabel: ""\n'),
		sub_atom(Output, _, _, _, '## entity\n'),
		sub_atom(Output, _, _, _, '"entity_".text: {\ntooltip: "Object path"\n}\n').

	test(d2_node_empty_comment_tooltip, true) :-
		d2_node_output([comment('')], Output),
		\+ sub_atom(Output, _, _, _, 'tooltip:').

	test(cx2_node_comment_tooltip, true) :-
		cx2_node_output(
			[comment('A "comment"\\path\nnext'), tooltip(object), url('https://example.com')],
			Output
		),
		sub_atom(Output, _, _, _, '"comment": "A \\"comment\\"\\\\path\\nnext"'),
		sub_atom(Output, _, _, _, '"url": "https://example.com"'),
		sub_atom(Output, _, _, _, '"tooltip": "object"').

	test(cx2_node_no_comment_tooltip, true) :-
		cx2_node_output([tooltip(object)], Output),
		\+ sub_atom(Output, _, _, _, '"comment": "'),
		sub_atom(Output, _, _, _, '"tooltip": "object"').

	test(cx2_node_empty_comment_tooltip, true) :-
		cx2_node_output([comment(''), tooltip(object)], Output),
		\+ sub_atom(Output, _, _, _, '"comment": "'),
		sub_atom(Output, _, _, _, '"tooltip": "object"').

	test(mermaid_node_comment_tooltips, true) :-
		mermaid_node_output(
			[comment('O\'Reilly "A" <x>\nnext'), tooltip(object), url('https://example.com')],
			Output
		),
		sub_atom(Output, _, _, _, '<b title=\'https://example.com\'>'),
		sub_atom(Output, _, _, _, 'click \'entity\' "https://example.com" "O#39;Reilly #quot;A#quot; #amp;lt;x#amp;gt;#10;next"').

	test(mermaid_node_comment_without_url, true) :-
		mermaid_node_output([comment('Comment'), tooltip('An "object"\nnext')], Output),
		sub_atom(Output, _, _, _, '<b title=\'An #quot;object#quot;#10;next\'>'),
		sub_atom(Output, _, _, _, 'click \'entity\' noop "Comment"').

	test(mermaid_node_comment_only, true) :-
		mermaid_node_output([comment('Comment')], Output),
		\+ sub_atom(Output, _, _, _, '<b title='),
		sub_atom(Output, _, _, _, 'click \'entity\' noop "Comment"').

	test(mermaid_node_empty_comment, true) :-
		mermaid_node_output([comment(''), tooltip(object)], Output),
		\+ sub_atom(Output, _, _, _, '<b title='),
		sub_atom(Output, _, _, _, 'click \'entity\' noop "object"').

	test(mermaid_node_url_tooltip_line_number, true) :-
		mermaid_node_output(
			[url('vscode://file/path/file.lgt:58:0'), tooltip('vscode://file/path/file.lgt')],
			Output
		),
		sub_atom(Output, _, _, _, 'click \'entity\' "vscode://file/path/file.lgt:58:0" "vscode://file/path/file.lgt:58:0"').

	test(mermaid_node_zoom_tooltips, true) :-
		mermaid_node_output(
			[zoom_url('entity\'s.html?x=1&y=2'), comment('Comment'), url('https://example.com')],
			Output
		),
		sub_atom(Output, _, _, _, '<a href=\'entity#39;s.html?x=1#amp;y=2\' title=\'Zoom\'>&#128269;</a><br/>'),
		sub_atom(Output, _, _, _, '<b title=\'https://example.com\'>'),
		sub_atom(Output, _, _, _, 'click \'entity\' "https://example.com" "Comment"').

	test(mermaid_node_zoom_without_url, true) :-
		mermaid_node_output([zoom_url('entity.svg'), comment('Comment')], Output),
		sub_atom(Output, _, _, _, '<a href=\'entity.html\' title=\'Zoom\'>&#128269;</a>'),
		sub_atom(Output, _, _, _, 'click \'entity\' noop "Comment"').

	test(mermaid_node_without_zoom, true) :-
		mermaid_node_output([comment('Comment'), url('https://example.com')], Output),
		\+ sub_atom(Output, _, _, _, 'title=\'Zoom\''),
		sub_atom(Output, _, _, _, 'click \'entity\' "https://example.com" "Comment"').

	test(mermaid_zoom_generated_subdiagram, true) :-
		object_property(d2_graph_language, file(File)),
		entity_diagram(mermaid)::file(File, [zoom(true)]),
		os::file_exists('./dot_dias/d2_graph_language_object_xref_diagram.html'),
		open('./dot_dias/d2_graph_language_entity_diagram.html', read, Stream),
		read_node_output_chars(Stream, Chars),
		close(Stream),
		atom_chars(Output, Chars),
		sub_atom(Output, _, _, _, 'href=\'d2_graph_language_object_xref_diagram.html\' title=\'Zoom\'').

	test(mermaid_zoom_explicit_suffix, true) :-
		object_property(d2_graph_language, file(File)),
		entity_diagram(mermaid)::file(File, [zoom(true), zoom_url_suffix('.custom')]),
		open('./dot_dias/d2_graph_language_entity_diagram.html', read, Stream),
		read_node_output_chars(Stream, Chars),
		close(Stream),
		atom_chars(Output, Chars),
		sub_atom(Output, _, _, _, 'href=\'d2_graph_language_object_xref_diagram.custom\' title=\'Zoom\'').

	test(mermaid_zoom_library_subdiagram, true) :-
		library_dependency_diagram(mermaid)::libraries(test, [options], [zoom(true)]),
		os::file_exists('./dot_dias/options_entity_diagram.html'),
		open('./dot_dias/test_library_dependency_diagram.html', read, Stream),
		read_node_output_chars(Stream, Chars),
		close(Stream),
		atom_chars(Output, Chars),
		sub_atom(Output, _, _, _, 'href=\'options_entity_diagram.html\' title=\'Zoom\'').

	% the following tests only check (for now) that the called
	% predicates succeed as expected and are deterministic

	% entity_diagram tests

	test(entity_diagram_libraries_2_01, deterministic) :-
		entity_diagram(_Format_)::libraries(test, [assertions,options]).

	test(entity_diagram_libraries_1_01, deterministic) :-
		entity_diagram(_Format_)::libraries([assertions,options]).

	test(entity_diagram_all_libraries_0_01, deterministic) :-
		entity_diagram(_Format_)::all_libraries.

	test(entity_diagram_rlibrary_1_01, deterministic) :-
		entity_diagram(_Format_)::rlibrary(options).

	test(entity_diagram_library_1_01, deterministic) :-
		entity_diagram(_Format_)::library(options).

	test(entity_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		entity_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(entity_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		entity_diagram(_Format_)::directory(test, Directory).

	test(entity_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		entity_diagram(_Format_)::directory(Directory).

	test(entity_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		entity_diagram(_Format_)::files(test, Files).

	test(entity_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		entity_diagram(_Format_)::files(Files).

	test(entity_diagram_all_files_0_01, deterministic) :-
		entity_diagram(_Format_)::all_files.

	test(entity_diagram_file_1_01, deterministic) :-
		object_property(entity_diagram, file(File)),
		entity_diagram(_Format_)::file(File).

	% inheritance_diagram tests

	test(inheritance_diagram_libraries_2_01, deterministic) :-
		inheritance_diagram(_Format_)::libraries(test, [assertions,options]).

	test(inheritance_diagram_libraries_1_01, deterministic) :-
		inheritance_diagram(_Format_)::libraries([assertions,options]).

	test(inheritance_diagram_all_libraries_0_01, deterministic) :-
		inheritance_diagram(_Format_)::all_libraries.

	test(inheritance_diagram_rlibrary_1_01, deterministic) :-
		inheritance_diagram(_Format_)::rlibrary(options).

	test(inheritance_diagram_library_1_01, deterministic) :-
		inheritance_diagram(_Format_)::library(options).

	test(inheritance_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		inheritance_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(inheritance_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		inheritance_diagram(_Format_)::directory(test, Directory).

	test(inheritance_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		inheritance_diagram(_Format_)::directory(Directory).

	test(inheritance_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		inheritance_diagram(_Format_)::files(test, Files).

	test(inheritance_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		inheritance_diagram(_Format_)::files(Files).

	test(inheritance_diagram_all_files_0_01, deterministic) :-
		inheritance_diagram(_Format_)::all_files.

	test(inheritance_diagram_file_1_01, deterministic) :-
		object_property(inheritance_diagram, file(File)),
		inheritance_diagram(_Format_)::file(File).

	% directory_dependency_diagram tests

	test(directory_dependency_diagram_rdirectory_2_01, deterministic) :-
		logtalk::expand_library_path(options, LgtunitDirectory),
		directory_dependency_diagram(_Format_)::rdirectory(test, LgtunitDirectory).

	test(directory_dependency_diagram_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(options, LgtunitDirectory),
		directory_dependency_diagram(_Format_)::rdirectory(LgtunitDirectory).

	test(directory_dependency_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		directory_dependency_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(directory_dependency_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		directory_dependency_diagram(_Format_)::directory(test, Directory).

	test(directory_dependency_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		directory_dependency_diagram(_Format_)::directory(Directory).

	test(directory_dependency_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_dependency_diagram(_Format_)::files(test, Files).

	test(directory_dependency_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_dependency_diagram(_Format_)::files(Files).

	test(directory_dependency_diagram_all_files_0_01, deterministic) :-
		directory_dependency_diagram(_Format_)::all_files.

	% directory_load_diagram tests

	test(directory_load_diagram_rdirectory_2_01, deterministic) :-
		logtalk::expand_library_path(options, LgtunitDirectory),
		directory_load_diagram(_Format_)::rdirectory(test, LgtunitDirectory).

	test(directory_load_diagram_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(options, LgtunitDirectory),
		directory_load_diagram(_Format_)::rdirectory(LgtunitDirectory).

	test(directory_load_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		directory_load_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(directory_load_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		directory_load_diagram(_Format_)::directory(test, Directory).

	test(directory_load_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		directory_load_diagram(_Format_)::directory(Directory).

	test(directory_load_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_load_diagram(_Format_)::files(test, Files).

	test(directory_load_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_load_diagram(_Format_)::files(Files).

	test(directory_load_diagram_all_files_0_01, deterministic) :-
		directory_load_diagram(_Format_)::all_files.

	% file_dependency_diagram tests

	test(file_dependency_diagram_libraries_2_01, deterministic) :-
		file_dependency_diagram(_Format_)::libraries(test, [assertions,options]).

	test(file_dependency_diagram_libraries_1_01, deterministic) :-
		file_dependency_diagram(_Format_)::libraries([assertions,options]).

	test(file_dependency_diagram_all_libraries_0_01, deterministic) :-
		file_dependency_diagram(_Format_)::all_libraries.

	test(file_dependency_diagram_rlibrary_1_01, deterministic) :-
		file_dependency_diagram(_Format_)::rlibrary(options).

	test(file_dependency_diagram_library_1_01, deterministic) :-
		file_dependency_diagram(_Format_)::library(options).

	test(file_dependency_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		file_dependency_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(file_dependency_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		file_dependency_diagram(_Format_)::directory(test, Directory).

	test(file_dependency_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		file_dependency_diagram(_Format_)::directory(Directory).

	test(file_dependency_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_dependency_diagram(_Format_)::files(test, Files).

	test(file_dependency_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_dependency_diagram(_Format_)::files(Files).

	test(file_dependency_diagram_all_files_0_01, deterministic) :-
		file_dependency_diagram(_Format_)::all_files.

	% file_load_diagram tests

	test(file_load_diagram_libraries_2_01, deterministic) :-
		file_load_diagram(_Format_)::libraries(test, [assertions,options]).

	test(file_load_diagram_libraries_1_01, deterministic) :-
		file_load_diagram(_Format_)::libraries([assertions,options]).

	test(file_load_diagram_all_libraries_0_01, deterministic) :-
		file_load_diagram(_Format_)::all_libraries.

	test(file_load_diagram_rlibrary_1_01, deterministic) :-
		file_load_diagram(_Format_)::rlibrary(options).

	test(file_load_diagram_library_1_01, deterministic) :-
		file_load_diagram(_Format_)::library(options).

	test(file_load_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		file_load_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(file_load_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		file_load_diagram(_Format_)::directory(test, Directory).

	test(file_load_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		file_load_diagram(_Format_)::directory(Directory).

	test(file_load_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_load_diagram(_Format_)::files(test, Files).

	test(file_load_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_load_diagram(_Format_)::files(Files).

	test(file_load_diagram_all_files_0_01, deterministic) :-
		file_load_diagram(_Format_)::all_files.

	% library_dependency_diagram tests

	test(library_dependency_diagram_libraries_2_01, deterministic) :-
		library_dependency_diagram(_Format_)::libraries(test, [assertions,options]).

	test(library_dependency_diagram_libraries_1_01, deterministic) :-
		library_dependency_diagram(_Format_)::libraries([assertions,options]).

	test(library_dependency_diagram_all_libraries_0_01, deterministic) :-
		library_dependency_diagram(_Format_)::all_libraries.

	test(library_dependency_diagram_rlibrary_1_01, deterministic) :-
		library_dependency_diagram(_Format_)::rlibrary(options).

	test(library_dependency_diagram_library_1_01, deterministic) :-
		library_dependency_diagram(_Format_)::library(options).

	test(library_dependency_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		library_dependency_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(library_dependency_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		library_dependency_diagram(_Format_)::directory(test, Directory).

	test(library_dependency_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		library_dependency_diagram(_Format_)::directory(Directory).

	test(library_dependency_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_dependency_diagram(_Format_)::files(test, Files).

	test(library_dependency_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_dependency_diagram(_Format_)::files(Files).

	test(library_dependency_diagram_all_files_0_01, deterministic) :-
		library_dependency_diagram(_Format_)::all_files.

	% library_load_diagram tests

	test(library_load_diagram_libraries_2_01, deterministic) :-
		library_load_diagram(_Format_)::libraries(test, [assertions,options]).

	test(library_load_diagram_libraries_1_01, deterministic) :-
		library_load_diagram(_Format_)::libraries([assertions,options]).

	test(library_load_diagram_all_libraries_0_01, deterministic) :-
		library_load_diagram(_Format_)::all_libraries.

	test(library_load_diagram_rlibrary_1_01, deterministic) :-
		library_load_diagram(_Format_)::rlibrary(options).

	test(library_load_diagram_library_1_01, deterministic) :-
		library_load_diagram(_Format_)::library(options).

	test(library_load_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		library_load_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(library_load_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		library_load_diagram(_Format_)::directory(test, Directory).

	test(library_load_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		library_load_diagram(_Format_)::directory(Directory).

	test(library_load_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_load_diagram(_Format_)::files(test, Files).

	test(library_load_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_load_diagram(_Format_)::files(Files).

	test(library_load_diagram_all_files_0_01, deterministic) :-
		library_load_diagram(_Format_)::all_files.

	% uses_diagram tests

	test(uses_diagram_libraries_2_01, deterministic) :-
		uses_diagram(_Format_)::libraries(test, [assertions,options]).

	test(uses_diagram_libraries_1_01, deterministic) :-
		uses_diagram(_Format_)::libraries([assertions,options]).

	test(uses_diagram_all_libraries_0_01, deterministic) :-
		uses_diagram(_Format_)::all_libraries.

	test(uses_diagram_rlibrary_1_01, deterministic) :-
		uses_diagram(_Format_)::rlibrary(options).

	test(uses_diagram_library_1_01, deterministic) :-
		uses_diagram(_Format_)::library(options).

	test(uses_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		uses_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(uses_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		uses_diagram(_Format_)::directory(test, Directory).

	test(uses_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		uses_diagram(_Format_)::directory(Directory).

	test(uses_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		uses_diagram(_Format_)::files(test, Files).

	test(uses_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		uses_diagram(_Format_)::files(Files).

	test(uses_diagram_all_files_0_01, deterministic) :-
		uses_diagram(_Format_)::all_files.

	test(uses_diagram_file_1_01, deterministic) :-
		object_property(uses_diagram, file(File)),
		uses_diagram(_Format_)::file(File).

	% xref_diagram tests

	test(xref_diagram_libraries_2_01, deterministic) :-
		xref_diagram(_Format_)::libraries(test, [assertions,options]).

	test(xref_diagram_libraries_1_01, deterministic) :-
		xref_diagram(_Format_)::libraries([assertions,options]).

	test(xref_diagram_all_libraries_0_01, deterministic) :-
		xref_diagram(_Format_)::all_libraries.

	test(xref_diagram_rlibrary_1_01, deterministic) :-
		xref_diagram(_Format_)::rlibrary(options).

	test(xref_diagram_library_1_01, deterministic) :-
		xref_diagram(_Format_)::library(options).

	test(xref_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, CodeMetricsDirectory),
		logtalk::expand_library_path(options, LgtunitDirectory),
		xref_diagram(_Format_)::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(xref_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		xref_diagram(_Format_)::directory(test, Directory).

	test(xref_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		xref_diagram(_Format_)::directory(Directory).

	test(xref_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		xref_diagram(_Format_)::files(test, Files).

	test(xref_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(assertions, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		xref_diagram(_Format_)::files(Files).

	test(xref_diagram_all_files_0_01, deterministic) :-
		xref_diagram(_Format_)::all_files.

	test(xref_diagram_file_1_01, deterministic) :-
		object_property(xref_diagram, file(File)),
		xref_diagram(_Format_)::file(File).

	% caller_diagram tests

	test(caller_diagram_predicate_1_01, deterministic) :-
		caller_diagram(_Format_)::predicate(list::member/2).

	% auxiliary predicates

	d2_graph_output(Label, Options, Output) :-
		File = 'd2_graph_tooltip_test.tmp',
		\+ os::file_exists(File),
		open(File, write, Stream),
		d2_graph_language::graph_header(Stream, graph, Label, library, Options),
		d2_graph_language::graph_footer(Stream, graph, Label, library, Options),
		close(Stream),
		open(File, read, Input),
		read_node_output_chars(Input, Chars),
		close(Input),
		os::delete_file(File),
		atom_chars(Output, Chars).

	d2_node_output(Options, Output) :-
		File = 'd2_node_tooltip_test.tmp',
		\+ os::file_exists(File),
		open(File, write, Stream),
		d2_graph_language::node(Stream, entity, entity, object, [predicate/1], prototype, Options),
		close(Stream),
		open(File, read, Input),
		read_node_output_chars(Input, Chars),
		close(Input),
		os::delete_file(File),
		atom_chars(Output, Chars).

	cx2_node_output(Options, Output) :-
		File = 'cx2_node_tooltip_test.tmp',
		\+ os::file_exists(File),
		open(File, write, Stream),
		cytoscapejs_graph_language::file_header(Stream, test, [title(''), description('')]),
		cytoscapejs_graph_language::node(Stream, entity, entity, object, [predicate/1], prototype, Options),
		cytoscapejs_graph_language::file_footer(Stream, test, []),
		close(Stream),
		open(File, read, Input),
		read_node_output_chars(Input, Chars),
		close(Input),
		os::delete_file(File),
		atom_chars(Output, Chars).

	mermaid_node_output(Options, Output) :-
		File = 'mermaid_node_tooltip_test.tmp',
		\+ os::file_exists(File),
		open(File, write, Stream),
		mermaid_graph_language::node(Stream, entity, entity, object, [predicate/1], prototype, Options),
		close(Stream),
		open(File, read, Input),
		read_node_output_chars(Input, Chars),
		close(Input),
		os::delete_file(File),
		atom_chars(Output, Chars).

	read_node_output_chars(Stream, Chars) :-
		get_char(Stream, Char),
		(	Char == end_of_file ->
			Chars = []
		;	Chars = [Char| Rest],
			read_node_output_chars(Stream, Rest)
		).

	% suppress all messages from the "diagrams" tool
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, diagrams, _Tokens).

:- end_object.
