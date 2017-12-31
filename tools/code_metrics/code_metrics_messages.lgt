%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- category(code_metrics_messages).

	:- info([
		version is 0.5,
		author is 'Ebrahim Azarisooreh',
		date is 2017/12/31,
		comment is 'Message translations for the code_metrics tool.'
	]).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, code_metrics, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, code_metrics) -->
		{ground_term_copy(Message, GroundMessage)},
		message_tokens(GroundMessage).

	message_tokens(starting_code_analysis) -->
		[].

	message_tokens(finished_code_analysis) -->
		[].

	message_tokens(scan_start_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)) -->
		{ Args = [Type, Year, Month, Day, Hours, Minutes, Seconds] },
		['~w analysis started at ~w/~w/~w, ~w:~w:~w'-Args, nl].

	message_tokens(scan_end_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)) -->
		{ Args = [Type, Year, Month, Day, Hours, Minutes, Seconds] },
		[nl, '~w analysis finished at ~w/~w/~w, ~w:~w:~w'-Args, nl].

	message_tokens(scanning_directory(Directory)) -->
		['Scanning directory ~w ...'-[Directory], nl].

	message_tokens(scanning_file(File)) -->
		['Scanning file ~w ...'-[File], nl].

	message_tokens(scanning_entity(Kind, Entity)) -->
		[nl, 'Scanning ~q ~w ...'-[Entity, Kind], nl].

	message_tokens(unknown(library, Library)) -->
		['Library not defined: ~q'-[Library], nl].
	message_tokens(unknown(directory, Directory)) -->
		['Directory does not exist: ~q'-[Directory], nl].
	message_tokens(unknown(file, File)) -->
		['File does not exist: ~q'-[File], nl].
	message_tokens(unknown(entity, Entity)) -->
		['Entity not loaded: ~q'-[Entity], nl].

	message_tokens(entity_score(Entity, Metric, Score)) -->
		Metric::entity_score(Entity, Score).

	% auxiliary predicates

	metric_label(Metric, Label) :-
		(	conforms_to_protocol(Metric, code_metric_protocol),
			Metric::metric_label(Label) ->
			true
		;	Label = Metric
		).

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

:- end_category.
