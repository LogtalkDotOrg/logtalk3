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

:- object(color_chooser).

	:- info([
		version is 1.0,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2015/01/21,
		comment is 'JColorChooser dialog example from the JPL distribution.'
	]).

	:- public(get_color/1).
	get_color(Color) :-
		java('javax.swing.JFrame')::new(['frame with dialog'], Frame),
		java(Frame)::setLocation(400,300),
		java(Frame)::setSize(400,300),
		java(Frame)::setVisible(@(true)),
		java(Frame)::toFront,
		java(Frame, ContentPane)::getContentPane,
		java('java.awt.Color')::get_field(pink, Pink),
		java('javax.swing.JColorChooser', Color)::showDialog(ContentPane,'pick a colo(u)r',Pink),
		java(Frame)::dispose,
		Color \== @(null).

:- end_object.
