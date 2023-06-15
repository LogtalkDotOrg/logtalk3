%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of one of the examples distributed with JPL
%
% the original code was converted to use the minimal abstraction of the JPL
% API for calling Java from Logtalk using familiar message sending syntax

:- object(jlist).

	:- info([
		version is 1:2:0,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2018-05-23,
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
		forall(
			current_protocol(Protocol),
			java(DefaultListModel)::addElement(Protocol)
		),
		java(Frame)::pack,
		java(Frame, Height)::getHeight,
		java(Frame)::setSize(150, Height),
		java::true(True),
		java(Frame)::setVisible(True).

:- end_object.
