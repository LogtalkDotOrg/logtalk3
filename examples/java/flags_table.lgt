%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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

:- object(flags_table).

	:- info([
		version is 1.1,
		author is 'Paul Singleton; adapted to Logtalk by Paulo Moura.',
		date is 2016/09/19,
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
		java::true(True),
		java(Frame)::setVisible(True).

:- end_object.
