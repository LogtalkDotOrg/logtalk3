%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- protocol(graphp).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/01,
		comment is 'Description']).

	:- public(output_file_name/2).
	:- mode(output_file_name(+atom, -atom), one).
	:- info(output_file_name/2, [
		comment is 'Constructs the the diagram file name.',
		argnames is ['Name', 'File']
	]).

	:- public(output_file_header/2).
	:- mode(output_file_header(+stream_or_alias, +list(compound)), one).
	:- info(output_file_header/2, [
		comment is 'Writes the output file header using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- public(output_file_footer/2).
	:- mode(output_file_footer(+stream_or_alias, +list(compound)), one).
	:- info(output_file_footer/2, [
		comment is 'Writes the output file footer using the specified options.',
		argnames is ['Stream', 'Options']
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

	:- public(node/6).
	:- mode(node(+stream_or_alias, +nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(node/6, [
		comment is 'Writes a node using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Lines', 'Kind', 'Options']
	]).

	:- public(edge/6).
	:- mode(edge(+stream_or_alias, +nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(edge/6, [
		comment is 'Writes an edge between two nodes using the specified options.',
		argnames is ['Stream', 'Start', 'End', 'Labels', 'Kind', 'Options']
	]).

:- end_protocol.
