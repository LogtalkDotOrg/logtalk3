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


:- object(dot_graph_language,
	implements(graph_language_protocol)).

	:- info([
		version is 3:3:0,
		author is 'Paulo Moura',
		date is 2020-10-02,
		comment is 'Predicates for generating graph files in the DOT language (version 2.36.0 or later).'
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- multifile(graph_language_registry::language_object/2).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		:- dynamic(graph_language_registry::language_object/2).
	:- endif.
	graph_language_registry::language_object(dot, dot_graph_language).

	output_file_name(Name, File) :-
		atom_concat(Name, '.dot', File).

	file_header(Stream, Identifier, Options) :-
		write(Stream, 'digraph "'),
		write(Stream, Identifier),
		write(Stream, '" {'), nl(Stream),
		memberchk(layout(Layout), Options),
		convert_layout(Layout, RankDir),
		write_key_value_nl(Stream, rankdir, RankDir),
		write_key_value_nl(Stream, ranksep, '1.0'),
		write_key_value_nl(Stream, compound, true),
		write_key_value_nl(Stream, splines, true),
		write_key_value_nl(Stream, pack, true),
		write_key_value_nl(Stream, clusterrank, local),
		write_key_value_nl(Stream, labeljust, l),
		write_key_value_nl(Stream, margin, '1.0'),
		write_key_value_nl(Stream, fontname, 'Courier'),
		write_key_value_nl(Stream, fontsize, 10),
		write_key_value_nl(Stream, fontcolor, dimgray),
		write_key_value_nl(Stream, pencolor, dimgray),
		write_key_value_nl(Stream, stylesheet, 'diagrams.css'),
		write(Stream, 'node [shape="ellipse",style="filled",fillcolor="white",fontname="Courier",fontsize="9"]'), nl(Stream),
		write(Stream, 'edge [fontname="Courier",fontsize="9"]'), nl(Stream),
		diagram_label(Options, Label),
		write_key_value_nl(Stream, label, Label),
		nl(Stream).

	convert_layout(top_to_bottom, 'TB').
	convert_layout(bottom_to_top, 'BT').
	convert_layout(left_to_right, 'LR').
	convert_layout(right_to_left, 'RL').

	diagram_label(Options, Label) :-
		memberchk(title(Title), Options),
		(	Title \== '' ->
			atom_concat(Title, '\\l', Label0)
		;	Label0 = ''
		),
		memberchk(description(Description), Options),
		atom_concat(Label0, Description, Label1),
		atom_concat(Label1, '\\l', Label2),
		(	memberchk(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Minutes, _, _), _, fail) ->
			number_codes(Year, YearCodes),
			atom_codes(YearAtom, YearCodes),
			integer_to_padded_atom(Month, PaddedMonth),
			integer_to_padded_atom(Day, PaddedDay),
			integer_to_padded_atom(Hours, PaddedHours),
			integer_to_padded_atom(Minutes, PaddedMinutes),
			atom_concat(Label2, 'Generated on ', Label3),
			atom_concat(Label3, YearAtom, Label4),
			atom_concat(Label4, '-', Label5),
			atom_concat(Label5, PaddedMonth, Label6),
			atom_concat(Label6, '-', Label7),
			atom_concat(Label7, PaddedDay, Label8),
			atom_concat(Label8, ', ', Label9),
			atom_concat(Label9, PaddedHours, Label10),
			atom_concat(Label10, ':', Label11),
			atom_concat(Label11, PaddedMinutes, Label12),
			atom_concat(Label12, '\\l', Label)
		;	Label = Label2
		).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			char_code('0', ZeroCode),
			atom_codes(Atom, [ZeroCode| Codes])
		;	atom_codes(Atom, Codes)
		).

	file_footer(Stream, _Identifier, _Options) :-
		write(Stream, '}'), nl(Stream).

	graph_header(Stream, Identifier, Label, Kind, Options) :-
		graph_style_margin_color(Kind, Style, Margin, Color),
		write(Stream, 'subgraph "cluster_'),
		write(Stream, Identifier),
		write(Stream, '" {'), nl(Stream),
		write_key_value_nl(Stream, bgcolor, Color),
		write_key_value_nl(Stream, style, Style),
		write_key_value_nl(Stream, margin, Margin),
		(	member(url(URL), Options) ->
			(	URL \== '' ->
				write(Stream, 'label=<<TABLE border="0" cellborder="0"><TR><TD tooltip="'),
				write(Stream, URL),
				write(Stream, '" href="'),
				write(Stream, URL),
				write(Stream, '">'),
				write(Stream, Label),
				write(Stream, '</TD></TR></TABLE>>'), nl(Stream),
				write_key_value_nl(Stream, tooltip, Label)
			;	member(tooltip(Tooltip), Options) ->
				write(Stream, 'label=<<TABLE border="0" cellborder="0"><TR><TD tooltip="'),
				write(Stream, Tooltip),
				write(Stream, '">'),
				write(Stream, Label),
				write(Stream, '</TD></TR></TABLE>>'), nl(Stream),
				write_key_value_nl(Stream, tooltip, Tooltip)
			;	write_key_value_nl(Stream, tooltip, Label)
			)
		;	member(tooltip(Tooltip), Options) ->
			write(Stream, 'label=<<TABLE border="0" cellborder="0"><TR><TD tooltip="'),
			write(Stream, Tooltip),
			write(Stream, '">'),
			write(Stream, Label),
			write(Stream, '</TD></TR></TABLE>>'), nl(Stream),
			write_key_value_nl(Stream, tooltip, Tooltip)
		;	write_key_value_nl(Stream, tooltip, Label)
		).

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		write(Stream, '}'), nl(Stream), nl(Stream).

	graph_style_margin_color(rlibrary, rounded, 10, lightgray).
	graph_style_margin_color(libraries, rounded, 10, lightgray).
	graph_style_margin_color(library, rounded, 10, whitesmoke).
	graph_style_margin_color(rdirectory, rounded, 10, lightgray).
	graph_style_margin_color(directories, rounded, 10, lightgray).
	graph_style_margin_color(directory, rounded, 10, whitesmoke).
	graph_style_margin_color(files, rounded, 10, whitesmoke).
	graph_style_margin_color(file, rounded, 10, snow).
	graph_style_margin_color(external, rounded, 10, white).
	graph_style_margin_color(entity, rounded, 10, snow).

	node(Stream, Identifier, Label, Caption, Contents, Kind, Options) :-
		node_shape_style_color(Kind, Shape, Style, Color),
		write(Stream, '"'),
		write(Stream, Identifier),
		write(Stream, '" ['),
		write_key_value_comma(Stream, shape, Shape),
		(	memberchk(url(URL), Options),
			URL \== '' ->
			write_key_value_comma(Stream, 'URL', URL),
			write_key_value_comma(Stream, tooltip, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value_comma(Stream, tooltip, Tooltip)
		;	true
		),
		write_key_value_comma(Stream, style, Style),
		write_key_value_comma(Stream, fillcolor, Color),
		write(Stream, 'label=<<TABLE border="0" cellborder="0" cellspacing="0" cellpadding="0">'),
		(	member(zoom_url(Diagram), Options) ->
			write(Stream, '<TR><TD width="11" height="11" fixedsize="true" align="left" tooltip="Zoom" href="'),
			write(Stream, Diagram),
			write(Stream, '"><IMG SRC="zoom.png"/></TD></TR>')
		;	true
		),
		write(Stream, '<TR><TD> </TD><TD><FONT POINT-SIZE="11">'),
		write_escaped_term(Stream, Label),
		write(Stream, '</FONT></TD><TD> </TD></TR>'),
		(	memberchk(node_type_captions(true), Options),
			Caption \== '' ->
			write(Stream, '<TR><TD> </TD><TD><FONT POINT-SIZE="7">'),
			write_escaped_term(Stream, Caption),
			write(Stream, '</FONT></TD><TD> </TD></TR>')
		;	true
		),
		(	Contents == [] ->
			true
		;	write(Stream, '<TR><TD> </TD></TR>'),
			write_node_lines(Contents, Stream)
		),
		write(Stream, '</TABLE>>]'), nl(Stream).

	% entities belonging to the file or library being documented
	node_shape_style_color(prototype, box, filled, cornsilk).
	node_shape_style_color(class, box, filled, yellow).
	node_shape_style_color(instance, box, filled, yellow).
	node_shape_style_color(instance_and_class, box, filled, yellow).
	node_shape_style_color(protocol, note, filled, aquamarine).
	node_shape_style_color(category, component, filled, lightcyan).
	node_shape_style_color(module, tab, filled, plum).
	node_shape_style_color(file, box, filled, paleturquoise).
	node_shape_style_color(directory, tab, filled, lightsalmon).
	node_shape_style_color(library, tab, filled, lightsalmon).
	% external entities to the file or library being documented
	node_shape_style_color(external_prototype, box, 'filled,dashed', beige).
	node_shape_style_color(external_class, box, 'filled,dashed', lightgoldenrodyellow).
	node_shape_style_color(external_instance, box, 'filled,dashed', lightgoldenrodyellow).
	node_shape_style_color(external_instance_and_class, box, 'filled,dashed', lightgoldenrodyellow).
	node_shape_style_color(external_protocol, note, 'filled,dashed', mediumaquamarine).
	node_shape_style_color(external_category, component, 'filled,dashed', cyan).
	node_shape_style_color(external_module, tab, 'filled,dashed', thistle).
	node_shape_style_color(external_file, box, 'filled,dashed', powderblue).
	node_shape_style_color(external_directory, tab, 'filled,dashed', salmon).
	node_shape_style_color(external_library, tab, 'filled,dashed', salmon).
	% predicates of the entities being documented
	node_shape_style_color(directive, box, filled, bisque).
	node_shape_style_color(predicate, box, filled, cornsilk).
	node_shape_style_color(public_predicate, box, filled, springgreen).
	node_shape_style_color(protected_predicate, box, filled, yellow).
	node_shape_style_color(private_predicate, box, filled, indianred1).
	node_shape_style_color(local_predicate, box, filled, cornsilk).
	node_shape_style_color(multifile_predicate, box, filled, skyblue).
	node_shape_style_color(exported_predicate, box, filled, springgreen).
	% external predicates to the entities being documented
	node_shape_style_color(external_predicate, box, 'filled,dashed', beige).

	edge(Stream, Start, End, Labels, Kind, Options) :-
		edge_arrow(Kind, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" ['),
		write_key_value_comma(Stream, arrowhead, ArrowHead),
		(	memberchk(url(URL), Options),
			URL \== '' ->
			write_key_value_comma(Stream, 'URL', URL),
			write_key_value_comma(Stream, labeltooltip, URL)
		;	member(tooltip(Tooltip), Options) ->
			write_key_value_comma(Stream, labeltooltip, Tooltip)
		;	true
		),
		write(Stream, 'label=<'),
		write_edge_lines(Labels, Stream),
		write(Stream, '>]'), nl(Stream).

	% entity relations
	edge_arrow(extends_object, vee).
	edge_arrow(extends_protocol, vee).
	edge_arrow(extends_category, vee).
	edge_arrow(instantiates_class, normal).
	edge_arrow(specializes_class, onormal).
	edge_arrow(implements_protocol, dot).
	edge_arrow(imports_category, box).
	edge_arrow(complements_object, obox).
	% multifile predicates
	edge_arrow(provides_clauses, inv).
	% cross-referencing predicate calls
	edge_arrow(calls_predicate, normal).
	edge_arrow(calls_super_predicate, normal).
	edge_arrow(calls_self_predicate, normal).
	% dynamic predicate updates
	edge_arrow(updates_predicate, diamond).
	edge_arrow(updates_this_predicate, diamond).
	edge_arrow(updates_self_predicate, diamond).
	% file relations
	edge_arrow(depends_on_file, normal).
	edge_arrow(loads_file, normal).
	edge_arrow(includes_file, normal).
	% directory relations
	edge_arrow(depends_on_directory, normal).
	edge_arrow(loads_directory, normal).
	% library relations
	edge_arrow(depends_on_library, normal).
	edge_arrow(loads_library, normal).

	write_key_value_nl(Stream, Key, Value) :-
		write_key_value(Stream, Key, Value),
		nl(Stream).

	write_key_value_comma(Stream, Key, Value) :-
		write_key_value(Stream, Key, Value),
		write(Stream, ',').

	write_key_value(Stream, Key, Value) :-
		write(Stream, Key),
		write(Stream, '="'),
		write_escaped_term(Stream, Value),
		write(Stream, '"').

	write_node_lines([], _).
	write_node_lines([Line| Lines], Stream) :-
		write(Stream, '<TR><TD> </TD><TD>'),
		write_escaped_term(Stream, Line),
		write(Stream, '</TD><TD> </TD></TR>'),
		write_node_lines(Lines, Stream).

	write_edge_lines([], _).
	write_edge_lines([Line| Lines], Stream) :-
		write_edge_lines(Lines, Line, Stream).

	write_edge_lines([], Line, Stream) :-
		write_escaped_term(Stream, Line).
	write_edge_lines([Next| Lines], Line, Stream) :-
		write_escaped_term(Stream, Line),
		write(Stream, '<BR/>'),
		write_edge_lines(Lines, Next, Stream).

	% CDATA tags are not officially supported in dot as of version 2.38
	% and are broken as they don't escape problematic characters; we try
	% to workaround the problem by manually escaping characters but the
	% chosen solution requires writing a term to a list of characters,
	% which is a non-standard functionality that only some backend systems
	% provide in a usable form
	write_escaped_term(Stream, Term) :-
		(	term_to_html_atom(Term, Atom) ->
			write(Stream, Atom)
		;	write(Stream, Term)
		).

	term_to_html_atom(Term, Atom) :-
		write_term_to_chars(Term, Chars),
		escape_chars(Chars, EscapedChars),
		atom_chars(Atom, EscapedChars).

	escape_chars([], []).
	escape_chars([Char| Chars], EscapedChars) :-
		(	Char == ('>') ->
			EscapedChars = ['&','g','t',';'| RestEscapedChars]
		;	Char == ('<') ->
			EscapedChars = ['&','l','t',';'| RestEscapedChars]
		;	Char == ('&') ->
			EscapedChars = ['&','a','m','p',';'| RestEscapedChars]
		;	EscapedChars = [Char| RestEscapedChars]
		),
		escape_chars(Chars, RestEscapedChars).

	:- if(current_logtalk_flag(prolog_dialect, cx)).

		write_term_to_chars(Term, Chars) :-
			{atom_term(Atom, Term), atom_chars(Atom, Chars)}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		write_term_to_chars(Term, Chars) :-
			{term_string(Term, String), atom_string(Atom, String), atom_chars(Atom, Chars)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		write_term_to_chars(Term, Chars) :-
			{write_to_chars(Chars, Term)}.

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

		write_term_to_chars(Term, Chars) :-
			{open_string(write, Stream), write(Stream, Term), stream_to_atom(Stream, Atom), atom_chars(Atom, Chars)}.

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		write_term_to_chars(Term, Chars) :-
			{format(chars(Chars), '~w', [Term])}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		{:- use_module(library(codesio), [])}.
		write_term_to_chars(Term, Chars) :-
			{codesio:format_to_codes('~w', Term, Codes), atom_codes(Atom, Codes), atom_chars(Atom, Chars)}.

	:- elif(current_logtalk_flag(prolog_dialect, tau)).

		{:- use_module(library(charsio))}.
		write_term_to_chars(Term, Chars) :-
			{write_term_to_chars(Term, [], Chars)}.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		write_term_to_chars(Term, Chars) :-
			{term_to_atom(Term, Atom), atom_chars(Atom, Chars)}.

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		write_term_to_chars(Term, Chars) :-
			{format(chars(Chars), '~w', [Term])}.

	:- elif(current_logtalk_flag(prolog_dialect, lvm)).

		write_term_to_chars(Term, Chars) :-
			{open(atom(Atom), write, Stream), write(Stream, Term), close(Stream), atom_chars(Atom, Chars)}.

	:- else.

		write_term_to_chars(_, _) :-
			fail.

	:- endif.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(term_to_atom,2), string)).
:- endif.
