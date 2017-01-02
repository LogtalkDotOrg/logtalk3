%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(graph_language_protocol).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/12/30,
		comment is 'Predicates for generating graph files.'
	]).

	:- public(output_file_name/2).
	:- mode(output_file_name(+atom, -atom), one).
	:- info(output_file_name/2, [
		comment is 'Constructs the diagram file basename by adding a graph language dependent extension to the given name.',
		argnames is ['Name', 'Basename']
	]).

	:- public(file_header/3).
	:- mode(file_header(+stream_or_alias, +atom, +list(compound)), one).
	:- info(file_header/3, [
		comment is 'Writes the output file header using the specified options.',
		argnames is ['Stream', 'Identifier', 'Options']
	]).

	:- public(file_footer/3).
	:- mode(file_footer(+stream_or_alias, +atom, +list(compound)), one).
	:- info(file_footer/3, [
		comment is 'Writes the output file footer using the specified options.',
		argnames is ['Stream', 'Identifier', 'Options']
	]).

	:- public(graph_header/5).
	:- mode(graph_header(+stream_or_alias, +atom, +atom, +atom, +list(compound)), one).
	:- info(graph_header/5, [
		comment is 'Writes a graph header using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Kind', 'Options']
	]).

	:- public(graph_footer/5).
	:- mode(graph_footer(+stream_or_alias, +atom, +atom, +atom, +list(compound)), one).
	:- info(graph_footer/5, [
		comment is 'Writes a graph footer using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Kind', 'Options']
	]).

	:- public(node/7).
	:- mode(node(+stream_or_alias, +nonvar, +nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(node/7, [
		comment is 'Writes a node using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Caption', 'Lines', 'Kind', 'Options']
	]).

	:- public(edge/6).
	:- mode(edge(+stream_or_alias, +nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(edge/6, [
		comment is 'Writes an edge between two nodes using the specified options.',
		argnames is ['Stream', 'Start', 'End', 'Labels', 'Kind', 'Options']
	]).

:- end_protocol.
