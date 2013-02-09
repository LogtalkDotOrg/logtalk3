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



:- object(logger,
	implements(loggingp)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/01/06,
		comment is 'Global logger object for logging events to files.'
	]).

	:- private(log_file_/2).
	:- dynamic(log_file_/2).
	:- mode(log_file_(?atom, ?nonvar), zero_or_more).
	:- info(log_file_/2, [
		comment is 'Table of log files.',
		argnames is ['Alias', 'File']
	]).

	:- private(logging_to_file_/2).
	:- dynamic(logging_to_file_/2).
	:- mode(logging_to_file_(?atom, ?atom), zero_or_more).
	:- info(logging_to_file_/2, [
		comment is 'Table of logging file status for log files.',
		argnames is ['Alias', 'Status']
	]).

	log_file(Alias, File) :-
		log_file_(Alias, File).

	define_log_file(Alias, File) :-
		retractall(log_file_(Alias, _)),
		asserta(log_file_(Alias, File)),
		retractall(logging_to_file_(Alias, _)),
		asserta(logging_to_file_(Alias, on)).

	init_log_file(Alias, File) :-
		open(File, write, Stream),
		close(Stream),
		define_log_file(Alias, File).

	log_event(Alias, Event) :-
		(	logging_to_file_(Alias, on) ->
			log_file_(Alias, File),
			open(File, append, Stream),
			write(Stream, Event),
			nl(Stream),
			close(Stream)
		;	true
		).

	logging(Alias) :-
		logging_to_file_(Alias, on).

	enable_logging(Alias) :-
		log_file_(Alias, _),
		retractall(logging_to_file_(Alias, _)),
		asserta(logging_to_file_(Alias, on)).

	disable_logging(Alias) :-
		log_file_(Alias, _),
		retractall(logging_to_file_(Alias, _)),
		asserta(logging_to_file_(Alias, off)).

:- end_object.
