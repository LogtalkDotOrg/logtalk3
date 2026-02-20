%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2011-2015 Marcus Uneson <marcus.uneson@ling.lu.se>
%  SPDX-License-Identifier: BSD-2-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions
%  are met:
%
%  1. Redistributions of source code must retain the above copyright
%     notice, this list of conditions and the following disclaimer.
%
%  2. Redistributions in binary form must reproduce the above copyright
%     notice, this list of conditions and the following disclaimer in
%     the documentation and/or other materials provided with the
%     distribution.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(command_line_options,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Marcus Uneson and Paulo Moura',
		date is 2026-02-20,
		comment is 'Command line options parsing predicates. Uses object-based option specifications with the ``command_line_option`` category.',
		see_also is [command_line_option]
	]).

	:- public(parse/4).
	:- mode(parse(+list(object), +list(atom), -list, -list(atom)), one_or_error).
	:- info(parse/4, [
		comment is 'Parses the arguments ``ApplArguments`` according to the option objects ``OptionObjects`` using default parsing options.',
		argnames is ['OptionObjects', 'ApplArguments', 'Options', 'PositionalArguments']
	]).

	:- public(parse/5).
	:- mode(parse(+list(object), +list(atom), -list, -list(atom), +list), one_or_error).
	:- info(parse/5, [
		comment is 'Parses the arguments ``ApplArguments`` according to the option objects ``OptionObjects`` and the parsing options ``ParseOptions``. ``Options`` is a list of parsed options as ``Name(Value)`` terms by default (or ``Func(Name,Value)`` when the ``output_functor(Func)`` parse option is used). ``PositionalArguments`` are the remaining non-dashed arguments. ``ParseOptions`` include ``output_functor(Func)``, ``duplicated_flags(Keep)`` (one of ``keepfirst``, ``keeplast``, ``keepall``; default ``keeplast``), and ``allow_empty_flag_spec(Bool)`` (default ``true``).',
		argnames is ['OptionObjects', 'ApplArguments', 'Options', 'PositionalArguments', 'ParseOptions']
	]).

	:- public(help/2).
	:- mode(help(+list(object), -atom), one_or_error).
	:- info(help/2, [
		comment is 'Synthesizes a help text ``Help`` as an atom from the option objects ``OptionObjects`` using default help options.',
		argnames is ['OptionObjects', 'Help']
	]).

	:- public(help/3).
	:- mode(help(+list(object), -atom, +list), one_or_error).
	:- info(help/3, [
		comment is 'Synthesizes a help text ``Help`` as an atom from the option objects ``OptionObjects`` using the given ``HelpOptions``. ``HelpOptions`` include ``line_width(Width)`` (default 80), ``min_help_width(Width)`` (default 40), ``break_long_flags(Boolean)`` (default ``false``), and ``suppress_empty_meta(Boolean)`` (default ``true``).',
		argnames is ['OptionObjects', 'Help', 'HelpOptions']
	]).

	:- uses(atom, [
		split/3
	]).

	:- uses(list, [
		length/2, member/2, reverse/2, select/3
	]).

	:- uses(numberlist, [
		max/2 as max_list/2
	]).

	:- uses(meta, [
		map/2, map/3
	]).

	:- uses(term_io, [
		read_from_atom/2
	]).

	:- uses(type, [
		check/2 as type_check/2, valid/2 as type_valid/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	parse(OptionObjects, ApplArguments, Options, PositionalArguments, UserParseOptions) :-
		^^check_options(UserParseOptions),
		^^merge_options(UserParseOptions, ParseOptions),
		check_option_objects(OptionObjects, SortedOptionObjects, ParseOptions),
		objects_to_specs(SortedOptionObjects, Specs),
		parse_(Specs, ApplArguments, Options, PositionalArguments, ParseOptions).

	parse(OptionObjects, ApplArguments, Options, PositionalArguments) :-
		parse(OptionObjects, ApplArguments, Options, PositionalArguments, []).

	help(OptionObjects, Help, UserHelpOptions) :-
		^^check_options(UserHelpOptions),
		^^merge_options(UserHelpOptions, HelpOptions),
		check_option_objects(OptionObjects, SortedOptionObjects, HelpOptions),
		objects_to_specs(SortedOptionObjects, Specs),
		help_(Specs, Help, HelpOptions).

	help(OptionObjects, Help) :-
		help(OptionObjects, Help, []).

	check_option_objects(OptionObjects, SortedOptionObjects, Options) :-
		% no instantiation errors and all objects must exist
		type_check(list(object), OptionObjects),
		% ensure no repeated option objects
		sort(OptionObjects, SortedOptionObjects),
		% validate all option objects first
		forall(member(OptionObject, SortedOptionObjects), OptionObject::check),
		% validate that the set of options is consistent
		consistent_set_of_options(SortedOptionObjects, Options).

	% Convert option objects to specification format
	objects_to_specs([], []).
	objects_to_specs([Object| Objects], [Spec| Specs]) :-
		object_to_spec(Object, Spec),
		objects_to_specs(Objects, Specs).

	object_to_spec(Object, spec(Name, DashedShortFlags, DashedLongFlags, Type, Default, Meta, Help)) :-
		Object::name(Name),
		Object::short_flags(ShortFlags),
		map([ShortFlag, Dashed]>>atom_concat('-', ShortFlag, Dashed), ShortFlags, DashedShortFlags),
		Object::long_flags(LongFlags),
		map([LongFlag, DoubleDashed]>>atom_concat('--', LongFlag, DoubleDashed), LongFlags, DashedLongFlags),
		Object::type(Type),
		(	Object::default(Default) ->
			true
		;	Default = '_'
		),
		Object::meta(Meta),
		Object::help(Help).

	% Check that the set of option objects is consistent
	% (no duplicate keys, no conflicting flags, no empty flag specs when disallowed)
	consistent_set_of_options(OptionObjects, Options) :-
		% Check for duplicate names
		findall(Name, (member(Object, OptionObjects), Object::name(Name)), Names),
		(	select(Name, Names, OtherNames),
			member(Name, OtherNames) ->
			domain_error(unique_key, Name)
		;	true
		),
		% Check for conflicting short flags
		findall(Flag, (member(Object, OptionObjects), Object::short_flags(Flags), member(Flag, Flags)), ShortFlags),
		(	select(ShortFlag, ShortFlags, OtherShortFlags),
			member(ShortFlag, OtherShortFlags) ->
			domain_error(unique_short_flag, ShortFlag)
		;	true
		),
		% Check for conflicting long flags
		findall(Flag, (member(Object, OptionObjects), Object::long_flags(Flags), member(Flag, Flags)), LongFlags),
		(	select(LongFlag, LongFlags, OtherLongFlags),
			member(LongFlag, OtherLongFlags) ->
			domain_error(unique_long_flag, LongFlag)
		;	true
		),
		% Check for empty flag specs when not allowed
		(	^^option(allow_empty_flag_spec(true), Options) ->
			true
		;	forall(
				member(Object, OptionObjects),
				(	Object::short_flags([_| _])
				;	Object::long_flags([_| _])
				;	Object::name(Name),
					domain_error(non_empty_flag_spec, Name)
				)
			)
		).

	parse_(Specs, Arguments, Options, PositionalArguments, ParseOptions) :-
		type_check(list(atom), Arguments),
		parse_options(Specs, Arguments, Options0, PositionalArguments),
		add_default_options(Specs, Options0, Options1),
		^^option(duplicated_flags(Keep), ParseOptions),
		remove_duplicates(Keep, Options1, Options2),
		(	^^option(output_functor(Functor), ParseOptions) ->
			refunctor_options(Functor, Options2, Options)
		;	refunctor_options(Options2, Options)
		).

	help_(Specs, Help, HelpOptions) :-
		include_in_help(Specs, Specs1),
		^^option(suppress_empty_meta(SuppressEmptyMeta), HelpOptions),
		map(spec_to_help_spec(SuppressEmptyMeta), Specs1, HelpSpecs),
		short_flags_col_width(HelpSpecs, SFlagsCW),
		metatypedef_col_width(HelpSpecs, MTDCW),
		long_flag_col_width(HelpSpecs, LongestFlagWidth),
		map(format_option(LongestFlagWidth, [SFlagsCW, MTDCW], HelpOptions), HelpSpecs, Lines),
		atomic_list_concat(Lines, Help).

	include_in_help([], []).
	include_in_help([Spec| Specs], Result) :-
		Spec = spec(_, ShortFlags, LongFlags, _, _, _, _),
		(	(ShortFlags = [_| _] ; LongFlags = [_| _]) ->
			Result = [Spec| Rest]
		;	Result = Rest
		),
		include_in_help(Specs, Rest).

	spec_to_help_spec(SuppressEmptyMeta,
			spec(_, ShortFlags, LongFlags, Type, Default, Meta, Help),
			help_spec(ShortFlagsStr, LongFlags, MTD, Help)
	) :-
		atomic_list_concat(ShortFlags, ',', ShortFlagsStr),
		compute_meta_type_default(SuppressEmptyMeta, Meta, Type, Default, MTD).

	compute_meta_type_default(SuppressEmptyMeta, Meta0, Type, Default, MTD) :-
		(	var(Default) ->
			DefAtom = '_'
		;	DefAtom = Default
		),
		atom_length(Meta0, N),
		(	N > 0 ->
			Meta = Meta0
		;	SuppressEmptyMeta == true ->
			Meta = ''
		;	meta_placeholder(Type, Meta)
		),
		(	Meta == '' ->
			atomic_list_concat([Type, '=', DefAtom], MTD)
		;	atomic_list_concat([Meta, ':', Type], MetaType),
			atomic_list_concat([MetaType, '=', DefAtom], MTD)
		).

	meta_placeholder(boolean, 'B').
	meta_placeholder(atom,    'A').
	meta_placeholder(float,   'F').
	meta_placeholder(integer, 'I').
	meta_placeholder(term,    'T').

	short_flags_col_width(HelpSpecs, ColWidth) :-
		findall(
			N,
			(	member(help_spec(SFlags, _, _, _), HelpSpecs),
				atom_length(SFlags, N0),
				N is N0 + 2
			),
			Ns
		),
		max_list([0| Ns], ColWidth).

	metatypedef_col_width(HelpSpecs, ColWidth) :-
		findall(
			N,
			(	member(help_spec(_, _, MTD, _), HelpSpecs),
				atom_length(MTD, N0),
				N is N0 + 2
			),
			Ns
		),
		max_list([0| Ns], ColWidth).

	long_flag_col_width(HelpSpecs, ColWidth) :-
		findall(
			FlagLength,
			(	member(help_spec(_, LongFlags, _, _), HelpSpecs),
				member(Flag, LongFlags),
				atom_length(Flag, FlagLength)
			),
			FlagLengths
		),
		max_list([0| FlagLengths], ColWidth).

	format_option(LongestFlagWidth, [SFlagsCW, MTDCW], HelpOptions,
			help_spec(SFlags, LFlags0, MetaTypeDef, Help), Line) :-
		(	LFlags0 == [] ->
			LFlags1 = [[]]
		;	group_length(LongestFlagWidth, LFlags0, LFlags1)
		),
		LFlagsCW is LongestFlagWidth + 2,
		(	^^option(break_long_flags(true), HelpOptions) ->
			LFlagSep = ',\n'
		;	LFlagSep = ', '
		),
		map({LFlagSep}/[LFlag1, LFlag2]>>atomic_list_concat(LFlag1, LFlagSep, LFlag2), LFlags1, LFlags2),
		atomic_list_concat(LFlags2, ',\n', LFlags),
		HelpIndent is LFlagsCW + SFlagsCW + MTDCW + 2,
		^^option(line_width(LW), HelpOptions),
		^^option(min_help_width(MHW), HelpOptions),
		HelpWidth is max(MHW, LW - HelpIndent),
		(	atom(Help) ->
			line_breaks(Help, HelpWidth, HelpIndent, BrokenHelp)
		;	Help = [_| _] ->
			indent_lines(Help, HelpIndent, BrokenHelp)
		;	line_breaks(Help, HelpWidth, HelpIndent, BrokenHelp)
		),
		pad_atom(LFlags, LFlagsCW, PaddedLFlags),
		pad_atom(SFlags, SFlagsCW, PaddedSFlags),
		pad_atom(MetaTypeDef, MTDCW, PaddedMetaTypeDef),
		atomic_list_concat([PaddedLFlags, PaddedSFlags, PaddedMetaTypeDef, BrokenHelp, '\n'], Line).

	% Pad an atom to a specific width with trailing spaces
	pad_atom(Atom, Width, Padded) :-
		atom_length(Atom, Lenght),
		(	Lenght >= Width ->
			Padded = Atom
		;	SpaceCount is Width - Lenght,
			n_spaces(SpaceCount, Spaces),
			atom_concat(Atom, Spaces, Padded)
		).

	n_spaces(N, Atom) :-
		length(Codes, N),
		map(=(32), Codes),
		atom_codes(Atom, Codes).

	line_breaks(TextLine, LineLength, Indent, TextLines) :-
		split(TextLine, ' ', Words),
		group_length(LineLength, Words, Groups0),
		map([Group0, Group]>>atomic_list_concat(Group0, ' ', Group), Groups0, Groups),
		indent_lines(Groups, Indent, TextLines).

	indent_lines(Lines, Indent, TextLines) :-
		n_spaces(Indent, IndentSpaces),
		atom_concat('\n', IndentSpaces, Separator),
		atomic_list_concat(Lines, Separator, TextLines).

	group_length(LineLength, Words, Groups) :-
		group_length(Words, LineLength, LineLength, [], [], Groups).

	group_length([], _, _, ThisLine, GroupsAcc, Groups) :-
		map(reverse, [ThisLine| GroupsAcc], GroupsAcc1),
		reverse(GroupsAcc1, Groups).
	group_length([Word| Words], LineLength, Remains, ThisLine, Groups, GroupsAcc) :-
		atom_length(Word, K),
		(	(Remains >= K ; ThisLine = []) ->
			Remains1 is Remains - K - 1,
			group_length(Words, LineLength, Remains1, [Word| ThisLine], Groups, GroupsAcc)
		;	group_length([Word| Words], LineLength, LineLength, [], [ThisLine| Groups], GroupsAcc)
		).

	parse_options(Specs, Arguments, Options, PositionalArguments) :-
		parse_arguments(Arguments, Specs, ParsedArguments),
		partition_arguments(ParsedArguments, Options, PositionalArguments).

	% Base case: empty list
	parse_arguments([], _, []).

	% Boolean flag given as --no-my-arg: set to false
	parse_arguments([Argument| Arguments], Specs, [option(Name, false)| Result]) :-
		atom_concat('--no-', BaseName, Argument),
		atom_concat('--', BaseName, DashedFlag),
		flag_id_type(Specs, DashedFlag, Name, boolean),
		!,
		parse_arguments(Arguments, Specs, Result).

	% Long flag with attached value: --flag=value
	parse_arguments([Argument| Arguments], Specs, [option(Name, Value)| Result]) :-
		match_long_flag_value(Argument, Specs, Name, Type, ValueAtom),
		!,
		parse_value(Type, ValueAtom, Value),
		parse_arguments(Arguments, Specs, Result).

	% Short flag with attached value: -fvalue
	parse_arguments([Argument| Arguments], Specs, [option(Name, Value)| Result]) :-
		match_short_flag_value(Argument, Specs, Name, Type, ValueAtom),
		!,
		parse_value(Type, ValueAtom, Value),
		parse_arguments(Arguments, Specs, Result).

	% Boolean flag (long or short) as last argument with implicit true
	parse_arguments([Argument], Specs, [option(Name, true)| Result]) :-
		flag_id_type(Specs, Argument, Name, boolean),
		!,
		parse_arguments([], Specs, Result).

	% Boolean flag (long or short) with implicit true
	parse_arguments([Argument, Argument2| Arguments], Specs, Result) :-
		flag_id_type(Specs, Argument, _Name, boolean),
		\+ member(Argument2, [true, false]),
		!,
		parse_arguments([Argument, true, Argument2| Arguments], Specs, Result).

	% Normal flag-value pair (separate arguments)
	parse_arguments([Argument1, Argument2| Arguments], Specs, [option(Name, Value)| Result]) :-
		flag_id_type(Specs, Argument1, Name, Type),
		!,
		parse_value(Type, Argument2, Value),
		parse_arguments(Arguments, Specs, Result).

	% Positional argument (doesn't start with -)
	parse_arguments([Argument| Arguments], Specs, [pos(Argument)| Result]) :-
		\+ sub_atom(Argument, 0, 1, _, '-'),
		!,
		parse_arguments(Arguments, Specs, Result).

	% Unknown flag (starts with - but doesn't match any spec)
	parse_arguments([Argument| _], _, _) :-
		sub_atom(Argument, 0, 1, _, '-'),
		existence_error(command_line_option, Argument).

	% Match --flag=value format
	match_long_flag_value(Argument, Specs, Name, Type, Value) :-
		sub_atom(Argument, Before, 1, After, '='),
		Before > 2,  % At least "--x"
		sub_atom(Argument, 0, 2, _, '--'),
		!,
		sub_atom(Argument, 0, Before, _, Flag),
		flag_id_type(Specs, Flag, Name, Type),
		sub_atom(Argument, _, After, 0, Value).

	% Match -fvalue format (short flag with attached value)
	match_short_flag_value(Argument, Specs, Name, Type, Value) :-
		sub_atom(Argument, 0, 1, _, '-'),
		\+ sub_atom(Argument, 1, 1, _, '-'),
		atom_length(Argument, Length),
		Length > 2,
		sub_atom(Argument, 0, 2, _, Flag),
		% Disallow -f=value format
		(	sub_atom(Argument, 2, 1, _, '=') ->
			syntax_error('disallowed: <shortflag>=<value>')
		;	true
		),
		flag_id_type(Specs, Flag, Name, Type),
		!,
		ValueLen is Length - 2,
		sub_atom(Argument, 2, ValueLen, 0, Value).

	flag_id_type(Specs, Flag, Name, Type) :-
		member(spec(Name, ShortFlags, LongFlags, Type, _, _, _), Specs),
		(	member(Flag, ShortFlags) ->
			true
		;	member(Flag, LongFlags) ->
			true
		;	fail
		).

	parse_value(boolean, true, true) :-
		!.
	parse_value(boolean, false, false) :-
		!.
	parse_value(atom, Atom, Atom) :-
		!.
	parse_value(integer, Atom, Result) :-
		atom_codes(Atom, Codes),
		number_codes(Result, Codes),
		integer(Result),
		!.
	parse_value(float, Atom, Result) :-
		atom_codes(Atom, Codes),
		number_codes(Result, Codes),
		float(Result),
		!.
	parse_value(term, Atom, Result) :-
		catch(read_from_atom(Atom, Result), _, fail),
		!.
	parse_value(Type, _, _) :-
		type_error(flag_value, Type).

	partition_arguments([], [], []).
	partition_arguments([option(Name, Value)| Arguments], [option(Name, Value)| Options], PositionalArguments) :-
		!,
		partition_arguments(Arguments, Options, PositionalArguments).
	partition_arguments([pos(Argument)| Arguments], Options, [Argument| PositionalArguments]) :-
		!,
		partition_arguments(Arguments, Options, PositionalArguments).

	add_default_options([], Options, Options).
	add_default_options([spec(Name, _, _, _, Default, _, _)| Specs], OptsIn, Result) :-
		(	member(option(Name, _Value), OptsIn) ->
			Result = OptsOut
		;	Default == '_' ->
			Result = [option(Name, _)| OptsOut]
		;	Result = [option(Name, Default)| OptsOut]
		),
		add_default_options(Specs, OptsIn, OptsOut).

	remove_duplicates(_, [], []) :-
		!.
	remove_duplicates(keeplast, [option(Name, Value)| Options], Result) :-
		!,
		(	member(option(Name, _), Options) ->
			Result = RestOpts
		;	Result = [option(Name, Value)| RestOpts]
		),
		remove_duplicates(keeplast, Options, RestOpts).
	remove_duplicates(keepfirst, Options, Result) :-
		!,
		remove_duplicates_keepfirst(Options, [], Result).
	remove_duplicates(keepall, OptsIn, OptsIn).

	remove_duplicates_keepfirst([], Result, Result).
	remove_duplicates_keepfirst([option(Name, Value)| Options], Acc, Result) :-
		(	member(option(Name, _), Acc) ->
			remove_duplicates_keepfirst(Options, Acc, Result)
		;	remove_duplicates_keepfirst(Options, [option(Name, Value)| Acc], Result)
		).

	refunctor_options(Functor, OptsIn, OptsOut) :-
		map(refunctor_option(Functor), OptsIn, OptsOut).

	refunctor_option(Functor, option(Name, Value), Option) :-
		Option =.. [Functor, Name, Value].

	refunctor_options(OptsIn, OptsOut) :-
		map(refunctor_option, OptsIn, OptsOut).

	refunctor_option(option(Name, Value), Option) :-
		Option =.. [Name, Value].

	% default options for parse/5
	default_option(duplicated_flags(keeplast)).
	default_option(allow_empty_flag_spec(true)).
	% default options for help/3
	default_option(line_width(80)).
	default_option(min_help_width(40)).
	default_option(break_long_flags(false)).
	default_option(suppress_empty_meta(true)).

	% options validation for parse/5
	valid_option(output_functor(Functor)) :-
		type_valid(atom, Functor).
	valid_option(duplicated_flags(Keep)) :-
		type_valid(one_of(atom, [keepfirst, keeplast, keepall]), Keep).
	valid_option(allow_empty_flag_spec(Boolean)) :-
		type_valid(boolean, Boolean).
	% options validation for help/3
	valid_option(line_width(Width)) :-
		type_valid(positive_integer, Width).
	valid_option(min_help_width(Width)) :-
		type_valid(positive_integer, Width).
	valid_option(break_long_flags(Boolean)) :-
		type_valid(boolean, Boolean).
	valid_option(suppress_empty_meta(Boolean)) :-
		type_valid(boolean, Boolean).

:- end_object.
