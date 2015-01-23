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

:- object(jlist).

	:- info([
		version is 1.0,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2015/01/23,
		comment is 'JList dialog example from the JPL distribution.'
	]).

	:- public(display/0).
	:- mode(display, one).
	:- info(display/0, [
		comment is 'Displays the names of all current loaded protocols in a JList.'
	]).

	display :-
		java('javax.swing.JFrame')::new(['protocols'], Frame),
		java('javax.swing.DefaultListModel')::new(DefaultListModel),
		java('javax.swing.JList')::new([DefaultListModel], List),
		java(Frame, ContentPane)::getContentPane,
		java(ContentPane)::add(List),
		(	current_protocol(Protocol),
			java(DefaultListModel)::addElement(Protocol),
			fail
		;	true
		),
		java(Frame)::pack,
		java(Frame, Height)::getHeight,
		java(Frame)::setSize(150, Height),
		java(Frame)::setVisible(@(true)).

:- end_object.
