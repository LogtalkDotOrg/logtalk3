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


:- object(plantuml_graph_language,
	implements(graph_language_protocol),
	imports(options)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Predicates for generating diagrams in PlantUML format.'
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(term_io, [
		write_term_to_chars/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- multifile(graph_language_registry::language_object/2).
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(puml, plantuml_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.puml', File).

	file_header(Stream, _Identifier, Options) :-
		write(Stream, '@startuml\n'),
		write(Stream, 'set separator none\n'),
		write_preamble(Stream),
		^^option(layout(Layout), Options),
		convert_layout(Layout, Direction),
		write(Stream, Direction),
		nl(Stream),
		diagram_label(Options, Label),
		(	Label \== '' ->
			write(Stream, 'title\n'),
			write(Stream, Label),
			write(Stream, 'end title\n')
		;	true
		),
		nl(Stream).

	convert_layout(top_to_bottom, 'top to bottom direction').
	convert_layout(bottom_to_top, 'top to bottom direction').
	convert_layout(left_to_right, 'left to right direction').
	convert_layout(right_to_left, 'left to right direction').

	diagram_label(Options, Label) :-
		^^option(title(Title), Options),
		^^option(description(Description), Options),
		(	Title \== '' ->
			atomic_list_concat([Title, '\n', Description, '\n'], Label0)
		;	Description \== '' ->
			atomic_list_concat([Description, '\n'], Label0)
		;	Label0 = ''
		),
		(	Label0 \== '',
			^^option(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),
			atomic_list_concat([Label0, 'Generated on ', Year, '-', PaddedMonth, '-', PaddedDay, ', ', PaddedHours, ':', PaddedMinutes, '\n'], Label1)
		;	Label1 = Label0
		),
		(	Label1 \== '',
			^^option(versions(true), Options) ->
			current_logtalk_flag(version_data, logtalk(LogtalkMajor, LogtalkMinor, LogtalkPatch, LogtalkStatus)),
			current_logtalk_flag(prolog_dialect, BackendId),
			backend(BackendId, BackendName),
			current_logtalk_flag(prolog_version, v(BackendMajor, BackendMinor, BackendPatch)),
			atomic_list_concat([
				Label1,
				'Generated with Logtalk ', LogtalkMajor, '.', LogtalkMinor, '.', LogtalkPatch, '-', LogtalkStatus,
				' running on ', BackendName, ' ', BackendMajor, '.', BackendMinor, '.', BackendPatch, '\n'
			], Label)
		;	Label = Label1
		).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			atom_codes(Atom, [0'0| Codes])
		;	atom_codes(Atom, Codes)
		).

	% Prolog backend identifier table
	backend(b,       'B-Prolog').
	backend(ciao,    'Ciao Prolog').
	backend(cx,      'CxProlog').
	backend(eclipse, 'ECLiPSe').
	backend(gnu,     'GNU Prolog').
	backend(ji,      'JIProlog').
	backend(quintus, 'Quintus Prolog').
	backend(sicstus, 'SICStus Prolog').
	backend(swi,     'SWI-Prolog').
	backend(tau,     'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb,     'XSB').
	backend(xvm,     'XVM').
	backend(yap,     'YAP').

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '@enduml\n').

	write_preamble(Stream) :-
		% class skinparams
		write(Stream, 'skinparam classBackgroundColor<<object>> #D6EAF8\n'),
		write(Stream, 'skinparam classBorderColor<<object>> #2980B9\n'),
		write(Stream, 'skinparam classBackgroundColor<<protocol>> #D5F5E3\n'),
		write(Stream, 'skinparam classBorderColor<<protocol>> #1E8449\n'),
		write(Stream, 'skinparam classBackgroundColor<<category>> #FDEBD0\n'),
		write(Stream, 'skinparam classBorderColor<<category>> #CA6F1E\n'),
		write(Stream, 'skinparam classBackgroundColor<<module>> #E8DAEF\n'),
		write(Stream, 'skinparam classBorderColor<<module>> #7D3C98\n'),
		write(Stream, 'skinparam classBackgroundColor<<file>> #D4E6F1\n'),
		write(Stream, 'skinparam classBorderColor<<file>> #2471A3\n'),
		write(Stream, 'skinparam classBackgroundColor<<directory>> #FADBD8\n'),
		write(Stream, 'skinparam classBorderColor<<directory>> #C0392B\n'),
		write(Stream, 'skinparam classBackgroundColor<<library>> #FADBD8\n'),
		write(Stream, 'skinparam classBorderColor<<library>> #C0392B\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_object>> #EBF5FB\n'),
		write(Stream, 'skinparam classBorderColor<<external_object>> #85C1E9\n'),
		write(Stream, 'skinparam classFontColor<<external_object>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_protocol>> #EAFAF1\n'),
		write(Stream, 'skinparam classBorderColor<<external_protocol>> #82E0AA\n'),
		write(Stream, 'skinparam classFontColor<<external_protocol>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_category>> #FEF5E7\n'),
		write(Stream, 'skinparam classBorderColor<<external_category>> #F0B27A\n'),
		write(Stream, 'skinparam classFontColor<<external_category>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_module>> #F4ECF7\n'),
		write(Stream, 'skinparam classBorderColor<<external_module>> #BB8FCE\n'),
		write(Stream, 'skinparam classFontColor<<external_module>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_file>> #EBF5FB\n'),
		write(Stream, 'skinparam classBorderColor<<external_file>> #85C1E9\n'),
		write(Stream, 'skinparam classFontColor<<external_file>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_directory>> #FDEDEC\n'),
		write(Stream, 'skinparam classBorderColor<<external_directory>> #F1948A\n'),
		write(Stream, 'skinparam classFontColor<<external_directory>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_library>> #FDEDEC\n'),
		write(Stream, 'skinparam classBorderColor<<external_library>> #F1948A\n'),
		write(Stream, 'skinparam classFontColor<<external_library>> #5D6D7E\n'),
		write(Stream, 'skinparam classBackgroundColor<<directive>> #FDEBD0\n'),
		write(Stream, 'skinparam classBorderColor<<directive>> #E67E22\n'),
		write(Stream, 'skinparam classBackgroundColor<<predicate>> #F2F3F4\n'),
		write(Stream, 'skinparam classBorderColor<<predicate>> #BDC3C7\n'),
		write(Stream, 'skinparam classBackgroundColor<<public_predicate>> #D5F5E3\n'),
		write(Stream, 'skinparam classBorderColor<<public_predicate>> #27AE60\n'),
		write(Stream, 'skinparam classBackgroundColor<<protected_predicate>> #FCF3CF\n'),
		write(Stream, 'skinparam classBorderColor<<protected_predicate>> #F1C40F\n'),
		write(Stream, 'skinparam classBackgroundColor<<private_predicate>> #FADBD8\n'),
		write(Stream, 'skinparam classBorderColor<<private_predicate>> #E74C3C\n'),
		write(Stream, 'skinparam classBackgroundColor<<local_predicate>> #F2F3F4\n'),
		write(Stream, 'skinparam classBorderColor<<local_predicate>> #BDC3C7\n'),
		write(Stream, 'skinparam classBackgroundColor<<multifile_predicate>> #D4E6F1\n'),
		write(Stream, 'skinparam classBorderColor<<multifile_predicate>> #2980B9\n'),
		write(Stream, 'skinparam classBackgroundColor<<exported_predicate>> #D5F5E3\n'),
		write(Stream, 'skinparam classBorderColor<<exported_predicate>> #27AE60\n'),
		write(Stream, 'skinparam classBackgroundColor<<external_predicate>> #F2F4F4\n'),
		write(Stream, 'skinparam classBorderColor<<external_predicate>> #D5DBDB\n'),
		write(Stream, 'skinparam classFontColor<<external_predicate>> #5D6D7E\n'),
		write(Stream, 'skinparam classFontName Monospace\n'),
		write(Stream, 'skinparam classFontSize 10\n'),
		% rounded corners for categories
		write(Stream, 'skinparam RoundCorner<<category>> 15\n'),
		write(Stream, 'skinparam RoundCorner<<external_category>> 15\n'),
		% package skinparams
		write(Stream, 'skinparam packageBackgroundColor<<rlibrary>> #D3D3D3\n'),
		write(Stream, 'skinparam packageBorderColor<<rlibrary>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<libraries>> #D3D3D3\n'),
		write(Stream, 'skinparam packageBorderColor<<libraries>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<library>> #F5F5F5\n'),
		write(Stream, 'skinparam packageBorderColor<<library>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<rdirectory>> #D3D3D3\n'),
		write(Stream, 'skinparam packageBorderColor<<rdirectory>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<directories>> #D3D3D3\n'),
		write(Stream, 'skinparam packageBorderColor<<directories>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<directory>> #F5F5F5\n'),
		write(Stream, 'skinparam packageBorderColor<<directory>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<files>> #F5F5F5\n'),
		write(Stream, 'skinparam packageBorderColor<<files>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<file>> #FFFAFA\n'),
		write(Stream, 'skinparam packageBorderColor<<file>> #A9A9A9\n'),
		write(Stream, 'skinparam packageBackgroundColor<<external>> #FFFFFF\n'),
		write(Stream, 'skinparam packageBorderColor<<external>> #D3D3D3\n'),
		write(Stream, 'skinparam packageBackgroundColor<<entity>> #FFFAFA\n'),
		write(Stream, 'skinparam packageBorderColor<<entity>> #A9A9A9\n'),
		write(Stream, 'skinparam packageFontName Monospace\n'),
		write(Stream, 'skinparam packageFontSize 10\n'),
		write(Stream, 'skinparam packageStyle rectangle\n'),
		% general settings
		write(Stream, 'skinparam defaultFontName Monospace\n'),
		write(Stream, 'skinparam defaultFontSize 10\n'),
		write(Stream, 'hide stereotype\n'),
		nl(Stream).

	graph_header(Stream, _Identifier, Label, Kind, _Options) :-
		graph_style(Kind, Style),
		write(Stream, 'package "'),
		write(Stream, Label),
		write(Stream, '" '),
		write(Stream, Style),
		write(Stream, ' {\n').

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '}\n\n').

	graph_style(rlibrary,    '<<rlibrary>>').
	graph_style(libraries,   '<<libraries>>').
	graph_style(library,     '<<library>>').
	graph_style(rdirectory,  '<<rdirectory>>').
	graph_style(directories, '<<directories>>').
	graph_style(directory,   '<<directory>>').
	graph_style(files,       '<<files>>').
	graph_style(file,        '<<file>>').
	graph_style(external,    '<<external>>').
	graph_style(entity,      '<<entity>>').

	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_stereotype_quoted(Kind, Stereotype, Quoted),
		write(Stream, 'class "'),
		write_escaped_term(Stream, Label, [quoted(Quoted)]),
		write(Stream, '" as '),
		write_identifier(Stream, Identifier),
		write(Stream, ' '),
		write(Stream, Stereotype),
		(	Contents == [] ->
			(	^^option(node_type_captions(true), Options),
				Caption \== '' ->
				write(Stream, ' {\n'),
				write(Stream, Caption),
				write(Stream, '\n'),
				write_metrics_overlay(Stream, Options),
				write(Stream, '}\n')
			;	member(metrics_overlay(_, _, _, _), Options) ->
				write(Stream, ' {\n'),
				write_metrics_overlay(Stream, Options),
				write(Stream, '}\n')
			;	nl(Stream)
			)
		;	write(Stream, ' {\n'),
			(	^^option(node_type_captions(true), Options),
				Caption \== '' ->
				write(Stream, Caption),
				write(Stream, '\n--\n')
			;	true
			),
			write_node_lines(Contents, Stream, [quoted(Quoted)]),
			write_metrics_overlay(Stream, Options),
			write(Stream, '}\n')
		),
		(	member(zoom_url(Diagram), Options) ->
			write(Stream, 'url of '),
			write_identifier(Stream, Identifier),
			write(Stream, ' is [['),
			write(Stream, Diagram),
			write(Stream, ']]\n')
		;	^^option(url(URL), Options),
			URL \== '' ->
			write(Stream, 'url of '),
			write_identifier(Stream, Identifier),
			write(Stream, ' is [['),
			write(Stream, URL),
			write(Stream, ']]\n')
		;	true
		).

	write_metrics_overlay(Stream, Options) :-
		(	member(metrics_overlay(Ce,Ca,I,A), Options) ->
			write(Stream, '--\n'),
			write(Stream, 'Ca:'),
			write(Stream, Ca),
			write(Stream, ' Ce:'),
			write(Stream, Ce),
			write(Stream, ' I:'),
			write(Stream, I),
			write(Stream, ' A:'),
			write(Stream, A),
			nl(Stream)
		;	true
		).

	% entities belonging to the file or library being documented
	node_stereotype_quoted(prototype,                   '<< (O,#4A90D9) object >>',               false).
	node_stereotype_quoted(class,                       '<< (O,#4A90D9) object >>',               false).
	node_stereotype_quoted(instance,                    '<< (O,#4A90D9) object >>',               false).
	node_stereotype_quoted(instance_and_class,          '<< (O,#4A90D9) object >>',               false).
	node_stereotype_quoted(protocol,                    '<< (P,#27AE60) protocol >>',             false).
	node_stereotype_quoted(category,                    '<< (C,#E67E22) category >>',             false).
	node_stereotype_quoted(module,                      '<< (M,#8E44AD) module >>',               false).
	node_stereotype_quoted(file,                        '<< (F,#3498DB) file >>',                 false).
	node_stereotype_quoted(directory,                   '<< (D,#E74C3C) directory >>',            false).
	node_stereotype_quoted(library,                     '<< (L,#E74C3C) library >>',              false).
	% external entities to the file or library being documented
	node_stereotype_quoted(external_prototype,          '<< (O,#85C1EC) external_object >>',      false).
	node_stereotype_quoted(external_class,              '<< (O,#85C1EC) external_object >>',      false).
	node_stereotype_quoted(external_instance,           '<< (O,#85C1EC) external_object >>',      false).
	node_stereotype_quoted(external_instance_and_class, '<< (O,#85C1EC) external_object >>',      false).
	node_stereotype_quoted(external_protocol,           '<< (P,#82E0AA) external_protocol >>',    false).
	node_stereotype_quoted(external_category,           '<< (C,#F0B27A) external_category >>',    false).
	node_stereotype_quoted(external_module,             '<< (M,#BB8FCE) external_module >>',      false).
	node_stereotype_quoted(external_file,               '<< (F,#85C1EC) external_file >>',        false).
	node_stereotype_quoted(external_directory,          '<< (D,#F1948A) external_directory >>',    false).
	node_stereotype_quoted(external_library,            '<< (L,#F1948A) external_library >>',     false).
	% predicates of the entities being documented
	node_stereotype_quoted(directive,                   '<< (d,#F39C12) directive >>',            true).
	node_stereotype_quoted(predicate,                   '<< (p,#BDC3C7) predicate >>',            true).
	node_stereotype_quoted(public_predicate,            '<< (p,#2ECC71) public_predicate >>',     true).
	node_stereotype_quoted(protected_predicate,         '<< (p,#F1C40F) protected_predicate >>',  true).
	node_stereotype_quoted(private_predicate,           '<< (p,#E74C3C) private_predicate >>',    true).
	node_stereotype_quoted(local_predicate,             '<< (p,#BDC3C7) local_predicate >>',      true).
	node_stereotype_quoted(multifile_predicate,         '<< (p,#3498DB) multifile_predicate >>',  true).
	node_stereotype_quoted(exported_predicate,          '<< (p,#2ECC71) exported_predicate >>',   true).
	% external predicates to the entities being documented
	node_stereotype_quoted(external_predicate,          '<< (p,#D5DBDB) external_predicate >>',   true).

	edge(Stream, _-Start, _-End, Labels, Kind, Options) :-
		!,
		edge(Stream, Start, End, Labels, Kind, Options).
	edge(Stream, Start, End, Labels, Kind, Options) :-
		edge_arrow(Kind, Arrow, RelLabel),
		write_identifier(Stream, Start),
		write(Stream, ' '),
		(	member(color(Color), Options) ->
			write(Stream, '[#'),
			write(Stream, Color),
			write(Stream, ']')
		;	true
		),
		write(Stream, Arrow),
		write(Stream, ' '),
		write_identifier(Stream, End),
		write(Stream, ' : '),
		(	Labels == [] ->
			write(Stream, RelLabel)
		;	write_edge_lines(Labels, Stream)
		),
		nl(Stream).

	% entity relations
	edge_arrow(extends_object,         '<|--',  '<<extends>>').
	edge_arrow(extends_protocol,       '<|--',  '<<extends>>').
	edge_arrow(extends_category,       '<|--',  '<<extends>>').
	edge_arrow(instantiates_class,     '<|..',  '<<instantiates>>').
	edge_arrow(specializes_class,      '<|--',  '<<specializes>>').
	edge_arrow(implements_protocol,    '..|>',  '<<implements>>').
	edge_arrow(imports_category,       '..>',   '<<imports>>').
	edge_arrow(complements_object,     '..>',   '<<complements>>').
	% multifile predicates
	edge_arrow(provides_clauses,       '..>',   '<<provides>>').
	% cross-referencing predicate calls
	edge_arrow(calls_predicate,        '-->',   '<<calls>>').
	edge_arrow(calls_super_predicate,  '-->',   '<<calls>>').
	edge_arrow(calls_self_predicate,   '-->',   '<<calls>>').
	% dynamic predicate updates
	edge_arrow(updates_predicate,      '-->>',  '<<updates>>').
	edge_arrow(updates_this_predicate, '-->>',  '<<updates>>').
	edge_arrow(updates_self_predicate, '-->>',  '<<updates>>').
	% file relations
	edge_arrow(depends_on_file,        '-->',   '<<depends on>>').
	edge_arrow(loads_file,             '-->',   '<<loads>>').
	edge_arrow(includes_file,          '-->',   '<<includes>>').
	% directory relations
	edge_arrow(depends_on_directory,   '-->',   '<<depends on>>').
	edge_arrow(loads_directory,        '-->',   '<<loads>>').
	% library relations
	edge_arrow(depends_on_library,     '-->',   '<<depends on>>').
	edge_arrow(loads_library,          '-->',   '<<loads>>').

	write_identifier(Stream, Identifier) :-
		write_term_to_chars(Identifier, Chars, []),
		write_identifier_chars(Chars, Stream).

	write_identifier_chars([], _).
	write_identifier_chars([Char| Chars], Stream) :-
		(	identifier_safe_char(Char) ->
			put_char(Stream, Char)
		;	put_char(Stream, '_')
		),
		write_identifier_chars(Chars, Stream).

	identifier_safe_char(Char) :-
		(	Char @>= 'a', Char @=< 'z' -> true
		;	Char @>= 'A', Char @=< 'Z' -> true
		;	Char @>= '0', Char @=< '9' -> true
		;	Char == '_'
		).

	write_node_lines([], _, _).
	write_node_lines([Line| Lines], Stream, Options) :-
		write_escaped_term(Stream, Line, Options),
		nl(Stream),
		write_node_lines(Lines, Stream, Options).

	write_edge_lines([], _).
	write_edge_lines([Line| Lines], Stream) :-
		write_edge_lines(Lines, Line, Stream).

	write_edge_lines([], Line, Stream) :-
		write(Stream, Line).
	write_edge_lines([Next| Lines], Line, Stream) :-
		write(Stream, Line),
		write(Stream, '\\n'),
		write_edge_lines(Lines, Next, Stream).

	write_escaped_term(Stream, Term, Options) :-
		write_term_to_chars(Term, Chars, Options),
		write_escaped_chars(Chars, Stream).

	write_escaped_chars([], _).
	write_escaped_chars([Char| Chars], Stream) :-
		(	Char == '"' ->
			write(Stream, '~"')
		;	put_char(Stream, Char)
		),
		write_escaped_chars(Chars, Stream).

:- end_object.
