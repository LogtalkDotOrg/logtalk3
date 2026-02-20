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


% Example option objects for testing

:- object(verbose_option,
	imports(command_line_option)).

	name(verbose).

	short_flags([v]).

	long_flags([verbosity]).

	type(integer).

	default(2).

	meta('V').

	help('verbosity level, 1 <= V <= 3').

:- end_object.


:- object(mode_option,
	imports(command_line_option)).

	name(mode).

	short_flags([m]).

	long_flags([mode]).

	type(atom).

	default('SCAN').

	help('data gathering mode').

:- end_object.


:- object(cache_option,
	imports(command_line_option)).

	name(cache).

	short_flags([r]).

	long_flags(['rebuild-cache']).

	type(boolean).

	default(true).

	help('rebuild cache in each iteration').

:- end_object.


:- object(threshold_option,
	imports(command_line_option)).

	name(threshold).

	short_flags([t, h]).

	long_flags(['heisenberg-threshold']).

	type(float).

	default(0.1).

	help('heisenberg threshold').

:- end_object.


:- object(depth_option,
	imports(command_line_option)).

	name(depth).

	short_flags([i, d]).

	long_flags([depths, iters]).

	type(integer).

	default(3).

	meta('K').

	help('stop after K iterations').

:- end_object.


:- object(outfile_option,
	imports(command_line_option)).

	name(outfile).

	short_flags([o]).

	long_flags(['output-file']).

	type(atom).

	meta('FILE').

	help('write output to FILE').

:- end_object.


:- object(goal_option,
	imports(command_line_option)).

	name(goal).

	short_flags([g]).

	long_flags([goal]).

	type(term).

	meta('GOAL').

	help('initialization goal').

:- end_object.


:- object(path_option,
	imports(command_line_option)).

	% Option without flags - configuration parameter only
	name(path).

	default('/some/file/path/').

:- end_object.


% Invalid option objects for testing validation

:- object(invalid_short_flag_option,
	imports(command_line_option)).

	name(invalid).

	short_flags([vv]).  % Invalid: more than one character

	type(boolean).

	default(true).

:- end_object.


:- object(invalid_type_option,
	imports(command_line_option)).

	name(invalid).

	type(unknown_type).  % Invalid: unknown type

	default(foo).

:- end_object.


:- object(invalid_default_option,
	imports(command_line_option)).

	name(invalid).

	type(integer).

	default(not_an_integer).  % Invalid: default doesn't match type

:- end_object.


:- object(no_key_option,
	imports(command_line_option)).

	% Invalid: missing key/1 definition
	type(atom).

	default(foo).

:- end_object.


% Option objects for testing consistency checks
% (individually valid, but can form invalid sets)

:- object(duplicate_key_option,
	imports(command_line_option)).

	% Same key as verbose_option
	name(verbose).

	short_flags([x]).

	type(boolean).

	default(false).

:- end_object.


:- object(duplicate_short_flag_option,
	imports(command_line_option)).

	name(unique_key).

	short_flags([v]).  % Same short flag as verbose_option

	type(atom).

	default(foo).

:- end_object.


:- object(duplicate_long_flag_option,
	imports(command_line_option)).

	name(another_unique_key).

	long_flags([verbosity]).  % Same long flag as verbose_option

	type(atom).

	default(bar).

:- end_object.


:- object(invalid_option_object,
	imports(command_line_option)).

	short_flags([iii]).

	long_flags([i]).

	type(foo).

	default([a, b, c]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura and Marcus Uneson',
		date is 2026-02-20,
		comment is 'Unit tests for the "command_line_options" library.'
	]).

	:- uses(list, [
		member/2, msort/2
	]).

	:- uses(command_line_options, [
		help/2, parse/4, parse/5
	]).

	cover(command_line_options).
	cover(command_line_option).

	% Tests for command_line_option category defaults

	test(command_line_option_short_flags_default, true(Flags == [])) :-
		path_option::short_flags(Flags).

	test(command_line_option_long_flags_default, true(Flags == [])) :-
		path_option::long_flags(Flags).

	test(command_line_option_type_default, true(Type == term)) :-
		path_option::type(Type).

	test(command_line_option_meta_default, true(Meta == '')) :-
		path_option::meta(Meta).

	test(command_line_option_help_default, true(Help == '')) :-
		path_option::help(Help).

	% Tests for command_line_option check/0 and valid/0 predicates

	test(command_line_option_check_valid_option, true) :-
		verbose_option::check.

	test(command_line_option_valid_valid_option, true) :-
		verbose_option::valid.

	test(command_line_option_check_invalid_short_flag, error(_)) :-
		invalid_short_flag_option::check.

	test(command_line_option_valid_invalid_short_flag, fail) :-
		invalid_short_flag_option::valid.

	test(command_line_option_check_invalid_type, error(_)) :-
		invalid_type_option::check.

	test(command_line_option_valid_invalid_type, fail) :-
		invalid_type_option::valid.

	test(command_line_option_check_invalid_default, error(_)) :-
		invalid_default_option::check.

	test(command_line_option_valid_invalid_default, fail) :-
		invalid_default_option::valid.

	test(command_line_option_check_no_key, error(_)) :-
		no_key_option::check.

	test(command_line_option_valid_no_key, fail) :-
		no_key_option::valid.

	% Tests for parse/4 and help/2 validation integration

	test(parse_4_invalid_option_object, error(_)) :-
		parse([invalid_option_object], ['-v', '5'],	_Options, _PositionalArguments).

	test(help_2_invalid_option_object, error(_)) :-
		help([invalid_option_object], _Help).

	% Tests for consistency checks on option sets

	test(parse_4_duplicate_key_error, error(domain_error(unique_key,verbose))) :-
		parse(
			[verbose_option, duplicate_key_option],
			['-v', '5'],
			_Options,
			_PositionalArguments
		).

	test(parse_4_duplicate_short_flag_error, error(domain_error(unique_short_flag,v))) :-
		parse(
			[verbose_option, duplicate_short_flag_option],
			['-v', '5'],
			_Options,
			_PositionalArguments
		).

	test(parse_4_duplicate_long_flag_error, error(domain_error(unique_long_flag,verbosity))) :-
		parse(
			[verbose_option, duplicate_long_flag_option],
			['--verbosity', '5'],
			_Options,
			_PositionalArguments
		).

	% Tests for object-based parse/4

	test(parse_4_simple_short_flag, true(Options-PositionalArguments == [verbose(5)]-[])) :-
		parse(
			[verbose_option],
			['-v', '5'],
			Options,
			PositionalArguments
		).

	test(parse_4_simple_long_flag, true(Options-PositionalArguments == [verbose(5)]-[])) :-
		parse(
			[verbose_option],
			['--verbosity', '5'],
			Options,
			PositionalArguments
		).

	test(parse_4_long_flag_with_equals, true(Options-PositionalArguments == [verbose(7)]-[])) :-
		parse(
			[verbose_option],
			['--verbosity=7'],
			Options,
			PositionalArguments
		).

	test(parse_4_short_flag_concatenated, true(Options-PositionalArguments == [depth(5)]-[])) :-
		parse(
			[depth_option],
			['-d5'],
			Options,
			PositionalArguments
		).

	test(parse_4_boolean_implicit_true, true(Options-PositionalArguments == [cache(true)]-[])) :-
		parse(
			[cache_option],
			['--rebuild-cache'],
			Options,
			PositionalArguments
		).

	test(parse_4_boolean_explicit_false, true(Options-PositionalArguments == [cache(false)]-[])) :-
		parse(
			[cache_option],
			['--rebuild-cache', 'false'],
			Options,
			PositionalArguments
		).

	test(parse_4_boolean_negated, true(Options-PositionalArguments == [cache(false)]-[])) :-
		parse(
			[cache_option],
			['--no-rebuild-cache'],
			Options,
			PositionalArguments
		).

	test(parse_4_float_type, true(Options-PositionalArguments == [threshold(0.14)]-[])) :-
		parse(
			[threshold_option],
			['--heisenberg-threshold', '0.14'],
			Options,
			PositionalArguments
		).

	test(parse_4_term_type, true(Options-PositionalArguments == [goal((write(foo),nl))]-[])) :-
		parse(
			[goal_option],
			['--goal', 'write(foo),nl'],
			Options,
			PositionalArguments
		).

	test(parse_4_positional_args, true(Options-PositionalArguments == [verbose(3)]-['input.txt', 'output.txt'])) :-
		parse(
			[verbose_option],
			['-v', '3', 'input.txt', 'output.txt'],
			Options,
			PositionalArguments
		).

	test(parse_4_default_values, true((member(verbose(2), Options), member(mode('SCAN'), Options)))) :-
		parse(
			[verbose_option, mode_option],
			[],
			Options,
			_PositionalArguments
		).

	test(parse_4_multiple_options, true((
			member(verbose(5), Options),
			member(mode('READ'), Options),
			member(cache(true), Options)
		))) :-
		parse(
			[verbose_option, mode_option, cache_option],
			['-v', '5', '-m', 'READ', '-r'],
			Options,
			_PositionalArguments
		).

	% Tests for object-based parse/5 with parse options

	test(parse_5_output_functor, true(Options-PositionalArguments == [config(verbose,5)]-[])) :-
		parse(
			[verbose_option],
			['-v', '5'],
			Options,
			PositionalArguments,
			[output_functor(config)]
		).

	test(parse_5_duplicated_flags_keeplast, true(Options-PositionalArguments == [verbose(7)]-[])) :-
		parse(
			[verbose_option],
			['-v', '5', '-v', '7'],
			Options,
			PositionalArguments,
			[duplicated_flags(keeplast)]
		).

	test(parse_5_duplicated_flags_keepfirst, true(Options-PositionalArguments == [verbose(5)]-[])) :-
		parse(
			[verbose_option],
			['-v', '5', '-v', '7'],
			Options,
			PositionalArguments,
			[duplicated_flags(keepfirst)]
		).

	test(parse_5_duplicated_flags_keepall, true(Options-PositionalArguments == [verbose(5), verbose(7)]-[])) :-
		parse(
			[verbose_option],
			['-v', '5', '-v', '7'],
			Options,
			PositionalArguments,
			[duplicated_flags(keepall)]
		).

	% Tests for object-based help/2

	test(help_2_contains_flag, true(sub_atom(Help, _, _, _, '--verbosity'))) :-
		help([verbose_option], Help).

	test(help_2_contains_help_text, true(sub_atom(Help, _, _, _, 'verbosity level'))) :-
		help([verbose_option], Help).

	test(help_2_multiple_options, true((
			sub_atom(Help, _, _, _, '--verbosity'),
			sub_atom(Help, _, _, _, '--mode')
		))) :-
		help([verbose_option, mode_option], Help).

	% Tests for flagless options (config parameters only)

	test(parse_4_flagless_option_default, true(Options-PositionalArguments == [path('/some/file/path/')]-[])) :-
		parse(
			[path_option],
			[],
			Options,
			PositionalArguments
		).

	% Complex integration test

	test(parse_4_complex_commandline, true) :-
		Options = [depth_option, threshold_option, outfile_option, cache_option, verbose_option],
		Arguments = [
			'-d5',
			'--heisenberg-threshold', '0.14',
			'--iters', '7',
			'-ooutput.txt',
			'--rebuild-cache', 'true',
			'input.txt',
			'--verbosity=2'
		],
		parse(Options, Arguments, OptionsAll, PositionalArguments, [duplicated_flags(keepall)]),
		msort(OptionsAll, OptionsAllSorted),
		parse(Options, Arguments, OptionsLast, _, [duplicated_flags(keeplast)]),
		msort(OptionsLast, OptionsLastSorted),
		^^assertion(keepall, OptionsAllSorted == [cache(true), depth(5), depth(7), outfile('output.txt'), threshold(0.14), verbose(2)]),
		^^assertion(keeplast, OptionsLastSorted == [cache(true), depth(7), outfile('output.txt'), threshold(0.14), verbose(2)]),
		^^assertion(positional, PositionalArguments == ['input.txt']).

	% Tests for help/3 with custom options

	test(help_3_custom_options, true(sub_atom(Help, _, _, _, '--verbosity'))) :-
		command_line_options::help([verbose_option], Help, [line_width(100)]).

	% Tests for invalid parse/help options

	test(parse_5_invalid_option_error, error(type_error(compound,unknown_option))) :-
		parse(
			[verbose_option],
			['-v', '5'],
			_Options,
			_PositionalArguments,
			[unknown_option]
		).

	test(parse_5_invalid_duplicated_flags_error, error(domain_error(option,duplicated_flags(invalid)))) :-
		parse(
			[verbose_option],
			['-v', '5'],
			_Options,
			_PositionalArguments,
			[duplicated_flags(invalid)]
		).

	test(parse_5_invalid_flag_error, error(existence_error(command_line_option,'--unknown_flag'))) :-
		parse(
			[verbose_option],
			['-v', '5', '--unknown_flag', 'foo'],
			_Options,
			_PositionalArguments
		).

	% Tests for allow_empty_flag_spec option

	test(parse_5_allow_empty_flag_spec_true, true(member(path('/some/file/path/'), Options))) :-
		% path_option has no flags, should succeed with default allow_empty_flag_spec(true)
		parse(
			[path_option],
			[],
			Options,
			_PositionalArguments,
			[allow_empty_flag_spec(true)]
		).

	test(parse_5_allow_empty_flag_spec_false, error(domain_error(non_empty_flag_spec,path))) :-
		% path_option has no flags, should fail with allow_empty_flag_spec(false)
		parse(
			[path_option],
			[],
			_Options,
			_PositionalArguments,
			[allow_empty_flag_spec(false)]
		).

	test(help_3_allow_empty_flag_spec_false, error(domain_error(non_empty_flag_spec,path))) :-
		% path_option has no flags, should fail with allow_empty_flag_spec(false)
		command_line_options::help([path_option], _Help, [allow_empty_flag_spec(false)]).

	% Tests for min_help_width option

	test(help_3_min_help_width, true(sub_atom(Help, _, _, _, 'verbosity level'))) :-
		% Test that min_help_width affects output (help text should still be present)
		command_line_options::help([verbose_option], Help, [min_help_width(60)]).

	test(help_3_min_help_width_small, true(sub_atom(Help, _, _, _, 'verbosity level'))) :-
		% Test with smaller min_help_width
		command_line_options::help([verbose_option], Help, [min_help_width(20)]).

	% Tests for break_long_flags option

	test(help_3_break_long_flags_false, true(sub_atom(Help, _, _, _, '--heisenberg-threshold'))) :-
		% Default behavior - long flags on same line
		command_line_options::help([threshold_option], Help, [break_long_flags(false)]).

	test(help_3_break_long_flags_true, true(sub_atom(Help, _, _, _, '--heisenberg-threshold'))) :-
		% With break_long_flags(true), flags should be broken onto separate lines
		command_line_options::help([threshold_option], Help, [break_long_flags(true)]).

	% Tests for suppress_empty_meta option

	test(help_3_suppress_empty_meta_true, true(sub_atom(Help, _, _, _, '=SCAN'))) :-
		% mode_option has empty meta - with suppress_empty_meta(true) (default),
		% the type:default should appear without meta placeholder
		command_line_options::help([mode_option], Help, [suppress_empty_meta(true)]).

	test(help_3_suppress_empty_meta_false, true(sub_atom(Help, _, _, _, 'A:atom=SCAN'))) :-
		% With suppress_empty_meta(false), should show generated meta placeholder
		% mode_option has type(atom), so meta placeholder should be 'A'
		command_line_options::help([mode_option], Help, [suppress_empty_meta(false)]).

	test(help_3_suppress_empty_meta_with_user_meta, true(sub_atom(Help, _, _, _, 'V:integer=2'))) :-
		% verbose_option has meta('V') - user-supplied meta should always be shown
		% regardless of suppress_empty_meta setting
		command_line_options::help([verbose_option], Help, [suppress_empty_meta(true)]).

:- end_object.
