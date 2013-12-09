%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/12/06,
		comment is 'Description']).

	:- public(output_file_name/2).
	:- mode(output_file_name(+atom, -atom), one).
	:- info(output_file_name/2, [
		comment is 'Constructs the the diagram file name.',
		argnames is ['Name', 'File']
	]).

	:- public(output_file_header/2).
	:- mode(output_file_header(+stream_or_alias, +list), one).
	:- info(output_file_header/2, [
		comment is 'Writes the output file header using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- public(output_file_footer/2).
	:- mode(output_file_footer(+stream_or_alias, +list), one).
	:- info(output_file_footer/2, [
		comment is 'Writes the output file footer using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- public(graph_header/4).
	:- mode(graph_header(+stream_or_alias, +atom, +atom, +list), one).
	:- info(graph_header/4, [
		comment is 'Writes a graph header using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Options']
	]).

	:- public(graph_footer/4).
	:- mode(graph_footer(+stream_or_alias, +atom, +atom, +list), one).
	:- info(graph_footer/4, [
		comment is 'Writes a graph footer using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Options']
	]).

	:- public(node/4).
	:- mode(node(+stream_or_alias, +atom, +list(predicate_indicator), +atom), one).
	:- info(node/4, [
		comment is 'Writes an arrow between two nodes using the specified options.',
		argnames is ['Stream', 'Label', 'Predicates', 'Kind']
	]).

	:- public(arrow/5).
	:- mode(arrow(+stream_or_alias, +atom, +atom, +atom, +list), one).
	:- info(arrow/5, [
		comment is 'Writes an arrow between two nodes using the specified options.',
		argnames is ['Stream', 'Start', 'End', 'Label', 'Options']
	]).

:- end_protocol.
