%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 2016 Barry Evans <barryevans@kyndi.com>  
%                 Paulo Moura <pmoura@logtalk.org>
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


:- category(dead_code_scanner_messages).

	:- info([
		version is 0.3,
		author is 'Barry Evans and Paulo Moura',
		date is 2016/10/12,
		comment is 'Logtalk "dead_code_scanner" tool default message translations.'
	]).
	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, dead_code_scanner, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, dead_code_scanner) -->
		message_tokens(Message).

	message_tokens(scan_started) -->
		[].

	message_tokens(scan_ended) -->
		[].

	message_tokens(scan_start_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)) -->
		['~w scan started at ~w/~w/~w, ~w:~w:~w'-[Type, Year, Month, Day, Hours, Minutes, Seconds], nl].

	message_tokens(scan_end_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)) -->
		['~w scan ended at ~w/~w/~w, ~w:~w:~w'-[Type, Year, Month, Day, Hours, Minutes, Seconds], nl].

	message_tokens(scanning_directory(Directory)) -->
		['Scanning directory ~w ...'-[Directory], nl].

	message_tokens(scanning_file(File)) -->
		['Scanning file ~w ...'-[File], nl].

	message_tokens(scanning_entity(Kind, Entity)) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Scanning ~q ~w ...'-[GroundEntity, Kind], nl].

	message_tokens(dead_predicate(_Entity, Predicate, File, Line)) -->
		['Likely dead predicate: ~q'-[Predicate], nl, '  in file ~w at or above line ~d'-[File, Line], nl].

	% auxiliary predicates

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

:- end_category.
