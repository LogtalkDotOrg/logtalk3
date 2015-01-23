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

:- object(text_entry).

	:- info([
		version is 1.0,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2015/01/21,
		comment is 'JOptionPane dialog example from the JPL distribution.'
	]).

	:- public(text/1).
	:- mode(text(-atom), zero_or_one).
	:- info(text/1, [
		comment is 'Shows a JOptionPane dialog, on top of a (necessary) new JFrame, and awaits text entry and OK/Cancel button click.'
	]).

	text(Text) :-
		java('javax.swing.JFrame')::new(['frame with dialog'], Frame),
		java(Frame)::setLocation(400, 300),
		java(Frame)::setSize(400, 300),
		java(Frame)::setVisible(@true),
		java(Frame)::toFront,
		java('javax.swing.JOptionPane', Text)::showInputDialog(Frame, 'type your name'),
		java(Frame)::dispose,
		Text \== @null.

:- end_object.
