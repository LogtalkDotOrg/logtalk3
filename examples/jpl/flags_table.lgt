%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% this example is an adaptation of one of the examples distributed with JPL
%
% the original code was converted to use the minimal abstraction of the JPL
% API for calling Java from Logtalk using familiar message sending syntax

:- object(flags_table).

	:- info([
		version is 1.0,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2015/01/21,
		comment is 'JTable example from the JPL distribution.'
	]).

	:- public(display/0).
	:- mode(display, one).
	:- info(display/0, [
		comment is 'Displays the names and values of all current Logtalk flags in a new JTable (within a new JScrollPane, within a new JFrame).'
	]).

	display :-
		findall(
			Array,
			(	current_logtalk_flag(Flag, Value),
				term_to_atom(Value, ValueAtom),
				java('[Ljava.lang.String;')::new([Flag,ValueAtom], Array)
			),
			Arrays
		),
		java('[[Ljava.lang.String;')::new(Arrays, RowData),
		java('[Ljava.lang.String;')::new([name,value], ColumnNames),
		java('javax.swing.JFrame')::new(['current_logtalk_flag/2'], Frame),
		java(Frame, ContentPane)::getContentPane,
		java('javax.swing.JTable')::new([RowData,ColumnNames], Table),
		java('javax.swing.JScrollPane')::new([Table], ScrollPane),
		java(ContentPane)::add(ScrollPane, 'Center'),
		java(Frame)::setSize(600, 400),
		java(Frame)::setVisible(@true).

:- end_object.
