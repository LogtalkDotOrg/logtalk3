%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(crontab(_Representation_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-09,
		comment is 'Parser and generator for user and system crontab files.',
		parameters is [
			'Representation' - 'Text representation. Valid values are ``atom``, ``chars``, and ``codes``.'
		]
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a user crontab from the given source using the default options.',
		argnames is ['Source', 'Entries']
	]).

	:- public(parse/3).
	:- mode(parse(++compound, --list, +list), one_or_error).
	:- info(parse/3, [
		comment is 'Parses a crontab from the given source. The supported option is ``format(user)`` (default) or ``format(system)``.',
		argnames is ['Source', 'Entries', 'Options']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +list), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a user crontab to the given sink using the default options.',
		argnames is ['Sink', 'Entries']
	]).

	:- public(generate/3).
	:- mode(generate(+compound, +list, +list), one_or_error).
	:- info(generate/3, [
		comment is 'Generates a crontab to the given sink. The supported option is ``format(user)`` (default) or ``format(system)``.',
		argnames is ['Sink', 'Entries', 'Options']
	]).

	:- public(entry/1).
	:- mode(entry(@nonvar), zero_or_one).
	:- info(entry/1, [
		comment is 'True if the argument is a valid crontab entry using the current text representation.',
		argnames is ['Entry']
	]).

	:- uses(list, [
		append/3, member/2, nth0/3, nth1/3, reverse/2
	]).

	:- uses(type, [
		check/3
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(Source, Entries) :-
		context(Context),
		check_representation,
		^^default_options(Options),
		parse_source(Source, Entries, Context, Options),
		!.
	parse(Source, _) :-
		domain_error(crontab_source, Source).

	parse(Source, _, _) :-
		var(Source),
		instantiation_error.
	parse(Source, Entries, UserOptions) :-
		context(Context),
		check_representation,
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		parse_source(Source, Entries, Context, Options),
		!.
	parse(Source, _, _) :-
		domain_error(crontab_source, Source).

	parse_source(file(File), Entries, Context, Options) :-
		check(file([], [read]), File, Context),
		reader::file_to_codes(File, Codes),
		parse_codes(Codes, Entries, Context, Options).
	parse_source(stream(Stream), Entries, Context, Options) :-
		reader::stream_to_codes(Stream, Codes),
		parse_codes(Codes, Entries, Context, Options).
	parse_source(atom(Atom), Entries, Context, Options) :-
		check(atom, Atom, Context),
		atom_codes(Atom, Codes),
		parse_codes(Codes, Entries, Context, Options).
	parse_source(chars(Chars), Entries, Context, Options) :-
		check(chars, Chars, Context),
		chars_to_codes(Chars, Codes),
		parse_codes(Codes, Entries, Context, Options).
	parse_source(codes(Codes), Entries, Context, Options) :-
		check(codes, Codes, Context),
		parse_codes(Codes, Entries, Context, Options).

	parse_codes(Codes, Entries, Context, Options) :-
		^^option(format(Format), Options),
		lines(Codes, Context, Lines),
		parse_lines(Lines, 1, Format, Entries, Context, Options).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(Sink, Entries) :-
		context(Context),
		check_representation,
		^^default_options(Options),
		^^option(format(Format), Options),
		entries_codes(Entries, Format, Codes),
		generate_sink(Sink, Codes, Context, Options),
		!.
	generate(Sink, _) :-
		domain_error(crontab_sink, Sink).

	generate(Sink, _, _) :-
		var(Sink),
		instantiation_error.
	generate(Sink, Entries, UserOptions) :-
		context(Context),
		check_representation,
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(format(Format), Options),
		entries_codes(Entries, Format, Codes),
		generate_sink(Sink, Codes, Context, Options),
		!.
	generate(Sink, _, _) :-
		domain_error(crontab_sink, Sink).

	generate_sink(file(File), Codes, Context, _Options) :-
		check(atom, File, Context),
		(	open(File, write, Stream),
			catch(
				write_codes(Codes, Stream),
				Error,
				(catch(close(Stream), _, true), throw(Error))
			) ->
			close(Stream)
		;	catch(close(Stream), _, true),
			fail
		).
	generate_sink(stream(Stream), Codes, _Context, _Options) :-
		write_codes(Codes, Stream).
	generate_sink(atom(Atom), Codes, _Context, _Options) :-
		atom_codes(Atom, Codes).
	generate_sink(chars(Chars), Codes, _Context, _Options) :-
		codes_to_chars(Codes, Chars).
	generate_sink(codes(Codes), Codes, _Context, _Options).

	entry(Entry) :-
		check_representation,
		valid_entry(Entry).

	check_representation :-
		(	var(_Representation_) ->
			instantiation_error
		;	member(_Representation_, [atom, chars, codes]) ->
			true
		;	domain_error(crontab_representation, _Representation_)
		).

	lines(Codes, Context, Lines) :-
		lines(Codes, 1, Context, Lines).

	lines([], _, _, []).
	lines(Codes, Number, Context, [Line| Lines]) :-
		take_line(Codes, RawLine, Rest, Terminated),
		(	valid_line_ending(RawLine, Line) ->
			true
		;	throw(error(syntax_error(crontab(Number, invalid_line_ending)), Context))
		),
		(	Terminated == true, Rest == [] ->
			Lines = []
		;	Next is Number + 1,
			lines(Rest, Next, Context, Lines)
		).

	take_line([], [], [], false).
	take_line([10| Codes], [], Codes, true) :-
		!.
	take_line([Code| Codes], [Code| Line], Rest, Terminated) :-
		take_line(Codes, Line, Rest, Terminated).

	valid_line_ending(RawLine, Line) :-
		(	append(Line, [13], RawLine) ->
			\+ member(13, Line)
		;	Line = RawLine,
			\+ member(13, Line)
		).

	parse_lines([], _, _, [], _, _).
	parse_lines([Line| Lines], Number, Format, [Entry| Entries], Context, Options) :-
		(	parse_line(Line, Format, Entry) ->
			true
		;	throw(error(syntax_error(crontab(Number, invalid_line)), Context))
		),
		Next is Number + 1,
		parse_lines(Lines, Next, Format, Entries, Context, Options).

	parse_line(Line, _, blank) :-
		horizontal_white_space(Line),
		!.
	parse_line(Line, _, comment(Text)) :-
		trim_left(Line, [0'#| Codes]),
		codes_text(Codes, Text),
		!.
	parse_line(Line, _, env(Name, Value)) :-
		parse_environment(Line, NameCodes, ValueCodes),
		codes_text(NameCodes, Name),
		codes_text(ValueCodes, Value),
		!.
	parse_line(Line, Format, Entry) :-
		phrase(special_job_tokens(Format, NameCodes, UserCodes, CommandCodes), Line),
		special_name(NameCodes, Name),
		parse_command(CommandCodes, CommandText, Input),
		codes_text(CommandText, Command),
		job_entry(Format, UserCodes, special(Name), Command, Input, Entry),
		!.
	parse_line(Line, Format, Entry) :-
		phrase(job_tokens(Format, FieldCodes, UserCodes, CommandCodes), Line),
		parse_fields(FieldCodes, Schedule),
		parse_command(CommandCodes, CommandText, Input),
		codes_text(CommandText, Command),
		job_entry(Format, UserCodes, Schedule, Command, Input, Entry).

	parse_environment(Line, Name, Value) :-
		trim_left(Line, Trimmed),
		append(BeforeEquals, [0'=| AfterEquals], Trimmed),
		trim_right(BeforeEquals, Name),
		valid_environment_name(Name),
		trim_left(AfterEquals, RawValue),
		environment_value(RawValue, Value).

	valid_environment_name([Code| Codes]) :-
		identifier_start(Code),
		identifier_codes(Codes).

	identifier_start(0'_).
	identifier_start(Code) :-
		Code >= 0'A, Code =< 0'Z.
	identifier_start(Code) :-
		Code >= 0'a, Code =< 0'z.

	identifier_codes([]).
	identifier_codes([Code| Codes]) :-
		(	identifier_start(Code)
		;	Code >= 0'0, Code =< 0'9
		),
		identifier_codes(Codes).

	environment_value([Quote| Codes], Value) :-
		member(Quote, [0'", 0'\']),
		append(Value, [Quote], Codes),
		!.
	environment_value(Value, Value).

	special_job_tokens(user, Name, [], Command) -->
		[0'@], token(Name), horizontal_white_space1, remaining(Command),
		{Command \== []}.
	special_job_tokens(system, Name, User, Command) -->
		[0'@], token(Name), horizontal_white_space1, token(User),
		horizontal_white_space1, remaining(Command),
		{Command \== []}.

	special_name(Codes, Name) :-
		lowercase_codes(Codes, Lower),
		atom_codes(Alias, Lower),
		special_alias(Alias, Name).

	special_alias(reboot, reboot).
	special_alias(yearly, yearly).
	special_alias(annually, yearly).
	special_alias(monthly, monthly).
	special_alias(weekly, weekly).
	special_alias(daily, daily).
	special_alias(midnight, daily).
	special_alias(hourly, hourly).

	job_tokens(user, Fields, [], Command) -->
		five_tokens(Fields), horizontal_white_space1, remaining(Command),
		{Command \== []}.
	job_tokens(system, Fields, User, Command) -->
		five_tokens(Fields), horizontal_white_space1, token(User),
		horizontal_white_space1, remaining(Command),
		{Command \== []}.

	five_tokens([A, B, C, D, E]) -->
		token(A), horizontal_white_space1,
		token(B), horizontal_white_space1,
		token(C), horizontal_white_space1,
		token(D), horizontal_white_space1,
		token(E).

	token([Code| Codes]) -->
		[Code], {\+ horizontal_code(Code)}, token_rest(Codes).

	token_rest([Code| Codes]) -->
		[Code], {\+ horizontal_code(Code)},
		!,
		token_rest(Codes).
	token_rest([]) -->
		[].

	horizontal_white_space1 -->
		[Code], {horizontal_code(Code)}, horizontal_white_space.

	horizontal_white_space -->
		[Code], {horizontal_code(Code)}, !, horizontal_white_space.
	horizontal_white_space -->
		[].

	remaining([]) -->
		[].
	remaining([Code| Codes]) -->
		[Code], remaining(Codes).

	parse_fields([Minute, Hour, Day, Month, Weekday], time(MinuteField, HourField, DayField, MonthField, WeekdayField)) :-
		parse_field(Minute, minute, MinuteField),
		parse_field(Hour, hour, HourField),
		parse_field(Day, day_of_month, DayField),
		parse_field(Month, month, MonthField),
		parse_field(Weekday, day_of_week, WeekdayField).

	parse_field(Codes, Kind, Field) :-
		split_on(0',, Codes, Parts),
		(	Parts = [Part] ->
			parse_field_item(Part, Kind, Field)
		;	Parts = [_, _| _],
			parse_field_items(Parts, Kind, Items),
			Field = list(Items)
		).

	parse_field_items([], _, []).
	parse_field_items([Part| Parts], Kind, [Item| Items]) :-
		Part \== [],
		parse_field_item(Part, Kind, Item),
		parse_field_items(Parts, Kind, Items).

	parse_field_item(Codes, Kind, Field) :-
		(	split_once(0'/, Codes, BaseCodes, StepCodes) ->
			StepCodes \== [],
			catch(number_codes(Step, StepCodes), _, fail),
			integer(Step), Step > 0,
			parse_step_base(BaseCodes, Kind, Base),
			Field = step(Base, Step)
		;	parse_field_base(Codes, Kind, Field)
		).

	parse_step_base([0'*], _, '*').
	parse_step_base(Codes, Kind, Range) :-
		parse_range(Codes, Kind, Range).

	parse_field_base([0'*], _, '*').
	parse_field_base(Codes, Kind, Field) :-
		(	parse_range(Codes, Kind, Field) ->
			true
		;	parse_scalar(Codes, Kind, Field)
		).

	parse_range(Codes, Kind, range(Low, High)) :-
		split_once(0'-, Codes, LowCodes, HighCodes),
		parse_scalar(LowCodes, Kind, Low),
		parse_scalar(HighCodes, Kind, High),
		scalar_index(Kind, Low, LowIndex),
		scalar_index(Kind, High, HighIndex),
		LowIndex =< HighIndex.

	parse_scalar(Codes, Kind, Value) :-
		(	catch(number_codes(Number, Codes), _, fail), integer(Number) ->
			valid_number(Kind, Number),
			Value = Number
		;	lowercase_codes(Codes, Lower),
			atom_codes(Name, Lower),
			valid_name(Kind, Name),
			Value = Name
		).

	valid_number(minute, Number) :-
		Number >= 0, Number =< 59.
	valid_number(hour, Number) :-
		Number >= 0, Number =< 23.
	valid_number(day_of_month, Number) :-
		Number >= 1, Number =< 31.
	valid_number(month, Number) :-
		Number >= 1, Number =< 12.
	% crontab convention permits both 0 and 7 to represent Sunday
	valid_number(day_of_week, Number) :-
		Number >= 0, Number =< 7.

	valid_name(month, Name) :-
		member(Name, [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]).
	valid_name(day_of_week, Name) :-
		member(Name, [sun, mon, tue, wed, thu, fri, sat]).

	scalar_index(_, Value, Value) :-
		integer(Value),
		!.
	scalar_index(month, Name, Index) :-
		nth1(Index, [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec], Name).
	scalar_index(day_of_week, Name, Index) :-
		nth0(Index, [sun, mon, tue, wed, thu, fri, sat], Name).

	parse_command(Codes, Command, Input) :-
		split_command(Codes, Command, InputCodes),
		Command \== [],
		(	InputCodes == none ->
			Input = none
		;	codes_text(InputCodes, InputText),
			Input = stdin(InputText)
		).

	split_command([], [], none).
	split_command([92, 0'%| Codes],  [0'%| Command], Input) :-
		!,
		split_command(Codes, Command, Input).
	split_command([0'%| Codes], [], Input) :-
		!,
		stdin_codes(Codes, Input).
	split_command([Code| Codes], [Code| Command], Input) :-
		split_command(Codes, Command, Input).

	stdin_codes([], []).
	stdin_codes([92, 0'%| Codes], [0'%| Input]) :-
		!,
		stdin_codes(Codes, Input).
	stdin_codes([0'%| Codes], [10| Input]) :-
		!,
		stdin_codes(Codes, Input).
	stdin_codes([Code| Codes], [Code| Input]) :-
		stdin_codes(Codes, Input).

	job_entry(user, [], Schedule, Command, Input, entry(Schedule, Command, Input)).
	job_entry(system, UserCodes, Schedule, Command, Input, user_entry(User, Schedule, Command, Input)) :-
		valid_user_name(UserCodes),
		codes_text(UserCodes, User).

	valid_user_name([Code| Codes]) :-
		\+ horizontal_code(Code),
		Code >= 33,
		valid_user_name_rest(Codes).

	valid_user_name_rest([]).
	valid_user_name_rest([Code| Codes]) :-
		\+ horizontal_code(Code),
		Code >= 33,
		valid_user_name_rest(Codes).

	entries_codes([], _, []).
	entries_codes([Entry| Entries], Format, Codes) :-
		entry_codes(Entry, Format, EntryCodes),
		entries_codes(Entries, Format, Rest),
		append(EntryCodes, [10| Rest], Codes).

	entry_codes(blank, _, []).
	entry_codes(comment(Text), _, [0'#| Codes]) :-
		text_codes(Text, Codes).
	entry_codes(env(Name, Value), _, Codes) :-
		text_codes(Name, NameCodes),
		valid_environment_name(NameCodes),
		text_codes(Value, ValueCodes),
		environment_output_codes(ValueCodes, OutputValueCodes),
		append(NameCodes, [0'=| OutputValueCodes], Codes).
	entry_codes(entry(Schedule, Command, Input), user, Codes) :-
		schedule_codes(Schedule, ScheduleCodes),
		text_codes(Command, CommandCodes), CommandCodes \== [],
		command_codes(CommandCodes, Input, EncodedCommand),
		append(ScheduleCodes, [32| EncodedCommand], Codes).
	entry_codes(user_entry(User, Schedule, Command, Input), system, Codes) :-
		text_codes(User, UserCodes), valid_user_name(UserCodes),
		schedule_codes(Schedule, ScheduleCodes),
		text_codes(Command, CommandCodes), CommandCodes \== [],
		command_codes(CommandCodes, Input, EncodedCommand),
		append(ScheduleCodes, [32| UserCodes], Prefix),
		append(Prefix, [32| EncodedCommand], Codes).

	schedule_codes(time(Minute, Hour, Day, Month, Weekday), Codes) :-
		field_codes(Minute, minute, MinuteCodes),
		field_codes(Hour, hour, HourCodes),
		field_codes(Day, day_of_month, DayCodes),
		field_codes(Month, month, MonthCodes),
		field_codes(Weekday, day_of_week, WeekdayCodes),
		join_fields([MinuteCodes, HourCodes, DayCodes, MonthCodes, WeekdayCodes], Codes).
	schedule_codes(special(Name), [0'@| Codes]) :-
		special_alias(Name, Name),
		atom_codes(Name, Codes).

	environment_output_codes([], []).
	environment_output_codes(Codes, Output) :-
		(	boundary_white_space(Codes) ->
			quote_environment_value(Codes, Output)
		;	Output = Codes
		).

	boundary_white_space([First| Codes]) :-
		(	horizontal_code(First) ->
			true
		;	reverse(Codes, [Last| _]),
			horizontal_code(Last)
		).

	quote_environment_value(Codes, [0'"| Quoted]) :-
		append(Codes, [0'"], Quoted).

	field_codes('*', _, [0'*]).
	field_codes(Value, Kind, Codes) :-
		(	integer(Value) ->
			valid_number(Kind, Value),
			number_codes(Value, Codes)
		;	atom(Value),
			valid_name(Kind, Value),
			atom_codes(Value, Codes)
		).
	field_codes(range(Low, High), Kind, Codes) :-
		field_codes(Low, Kind, LowCodes), field_codes(High, Kind, HighCodes),
		scalar_index(Kind, Low, LowIndex), scalar_index(Kind, High, HighIndex), LowIndex =< HighIndex,
		append(LowCodes, [0'-| HighCodes], Codes).
	field_codes(step(Base, Step), Kind, Codes) :-
		integer(Step), Step > 0,
		(	Base == '*' ->
			BaseCodes = [0'*]
		;	Base = range(_, _),
			field_codes(Base, Kind, BaseCodes)
		),
		number_codes(Step, StepCodes),
		append(BaseCodes, [0'/| StepCodes], Codes).
	field_codes(list(Items), Kind, Codes) :-
		Items = [_, _| _],
		field_items_codes(Items, Kind, Codes).

	field_items_codes([Item], Kind, Codes) :-
		Item \= list(_),
		field_codes(Item, Kind, Codes).
	field_items_codes([Item| Items], Kind, Codes) :-
		Item \= list(_),
		field_codes(Item, Kind, ItemCodes),
		field_items_codes(Items, Kind, Rest),
		append(ItemCodes, [0',| Rest], Codes).

	join_fields([Codes], Codes).
	join_fields([Codes| Fields], Joined) :-
		join_fields(Fields, Rest),
		append(Codes, [32| Rest], Joined).

	command_codes(Command, none, Codes) :-
		escape_percent(Command, Codes).
	command_codes(Command, stdin(Input), Codes) :-
		escape_percent(Command, CommandCodes),
		text_codes(Input, InputCodes),
		stdin_output_codes(InputCodes, StdinCodes),
		append(CommandCodes, [0'%| StdinCodes], Codes).

	escape_percent([], []).
	escape_percent([0'%| Codes], [92, 0'%| Escaped]) :-
		!,
		escape_percent(Codes, Escaped).
	escape_percent([Code| Codes], [Code| Escaped]) :-
		escape_percent(Codes, Escaped).

	stdin_output_codes([], []).
	stdin_output_codes([10| Codes], [0'%| Output]) :-
		!,
		stdin_output_codes(Codes, Output).
	stdin_output_codes([0'%| Codes], [92, 0'%| Output]) :-
		!,
		stdin_output_codes(Codes, Output).
	stdin_output_codes([Code| Codes], [Code| Output]) :-
		stdin_output_codes(Codes, Output).

	valid_entry(blank).
	valid_entry(comment(Text)) :-
		text_codes(Text, _).
	valid_entry(env(Name, Value)) :-
		text_codes(Name, NameCodes),
		valid_environment_name(NameCodes),
		text_codes(Value, _).
	valid_entry(entry(Schedule, Command, Input)) :-
		schedule_codes(Schedule, _),
		text_codes(Command, Codes),
		Codes \== [],
		valid_input(Input).
	valid_entry(user_entry(User, Schedule, Command, Input)) :-
		text_codes(User, UserCodes),
		valid_user_name(UserCodes),
		schedule_codes(Schedule, _),
		text_codes(Command, Codes),
		Codes \== [],
		valid_input(Input).

	valid_input(none).
	valid_input(stdin(Text)) :-
		text_codes(Text, _).

	codes_text(Codes, Text) :-
		(	_Representation_ == atom -> atom_codes(Text, Codes)
		;	_Representation_ == chars -> codes_to_chars(Codes, Text)
		;	_Representation_ == codes -> Text = Codes
		;	fail
		).

	text_codes(Text, Codes) :-
		(	_Representation_ == atom -> atom(Text), atom_codes(Text, Codes)
		;	_Representation_ == chars -> proper_chars(Text), chars_to_codes(Text, Codes)
		;	_Representation_ == codes -> proper_codes(Text), Codes = Text
		;	fail
		).

	horizontal_white_space([]).
	horizontal_white_space([Code| Codes]) :-
		horizontal_code(Code),
		horizontal_white_space(Codes).

	horizontal_code(9).
	horizontal_code(32).

	trim_left([Code| Codes], Trimmed) :-
		horizontal_code(Code),
		!,
		trim_left(Codes, Trimmed).
	trim_left(Codes, Codes).

	trim_right(Codes, Trimmed) :-
		reverse(Codes, Reversed),
		trim_left(Reversed, TrimmedReversed),
		reverse(TrimmedReversed, Trimmed).

	split_once(Separator, Codes, Before, After) :-
		append(Before, [Separator| After], Codes), !.

	split_on(Separator, Codes, Parts) :-
		(	split_once(Separator, Codes, Before, After) ->
			Parts = [Before| Rest],
			split_on(Separator, After, Rest)
		;	Parts = [Codes]
		).

	lowercase_codes([], []).
	lowercase_codes([Code| Codes], [Lower| LowerCodes]) :-
		(	Code >= 0'A, Code =< 0'Z ->
			Lower is Code + 32
		;	Lower = Code
		),
		lowercase_codes(Codes, LowerCodes).

	proper_codes([]).
	proper_codes([Code| Codes]) :-
		integer(Code), Code >= 0,
		proper_codes(Codes).

	proper_chars([]).
	proper_chars([Char| Chars]) :-
		atom(Char), atom_length(Char, 1),
		proper_chars(Chars).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

	valid_option(format(Format)) :-
		once((Format == user; Format == system)).

	default_option(format(user)).

:- end_object.


:- object(crontab,
	extends(crontab(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-09,
		comment is 'Parser and generator for user and system crontab files using atoms for text representation.'
	]).

:- end_object.
