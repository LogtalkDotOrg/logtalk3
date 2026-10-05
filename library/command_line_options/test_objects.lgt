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

	% Invalid: missing name/1 definition
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
