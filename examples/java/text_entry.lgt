%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of one of the examples distributed with JPL
%
% the original code was converted to use the minimal abstraction of the JPL
% API for calling Java from Logtalk using familiar message sending syntax

:- object(text_entry).

	:- info([
		version is 1.1,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2016/09/21,
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
		java::true(True),
		java(Frame)::setVisible(True),
		java(Frame)::toFront,
		java('javax.swing.JOptionPane', Text)::showInputDialog(Frame, 'type your name'),
		java(Frame)::dispose,
		\+ java::is_null(Text).

:- end_object.
