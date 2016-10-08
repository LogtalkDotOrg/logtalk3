%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
