%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- category(command_line_option).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Category for defining command-line options. Import this category into objects that represent individual command-line options and override the predicates as needed.',
		see_also is [command_line_options]
	]).

	:- public(check/0).
	:- mode(check, one_or_error).
	:- info(check/0, [
		comment is 'Checks if the command-line option definition is valid. Throws an error if the definition is invalid.'
	]).

	:- public(valid/0).
	:- mode(valid, zero_or_one).
	:- info(valid/0, [
		comment is 'Succeeds if the command-line option definition is valid. Fails otherwise.'
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_one).
	:- info(name/1, [
		comment is 'Name used to identify this option in the parsed results. This predicate must be overridden. No default.',
		argnames is ['Name']
	]).

	:- public(short_flags/1).
	:- mode(short_flags(-list(character)), one).
	:- info(short_flags/1, [
		comment is 'List of single-character short flags for this option (e.g., ``[v]`` for ``-v``). Default is an empty list.',
		argnames is ['Flags']
	]).

	:- public(long_flags/1).
	:- mode(long_flags(-list(atom)), one).
	:- info(long_flags/1, [
		comment is 'List of long flags for this option (e.g., ``[verbose]`` for ``--verbose``). Default is an empty list.',
		argnames is ['Flags']
	]).

	:- public(type/1).
	:- mode(type(-atom), one).
	:- info(type/1, [
		comment is 'Option value type. One of ``boolean``, ``atom``, ``integer``, ``float``, or ``term``. Default is ``term``.',
		argnames is ['Type']
	]).

	:- public(default/1).
	:- mode(default(-term), zero_or_one).
	:- info(default/1, [
		comment is 'Default value for this option if any.',
		argnames is ['Default']
	]).

	:- public(meta/1).
	:- mode(meta(-atom), one).
	:- info(meta/1, [
		comment is 'Metasyntactic variable name for the help text (e.g., ``\'FILE\'``). Default is an empty atom.',
		argnames is ['Meta']
	]).

	:- public(help/1).
	:- mode(help(-atom), one).
	:- mode(help(-list(atom)), one).
	:- info(help/1, [
		comment is 'Help text for this option. Can be an atom or a list of atoms for pre-broken lines. Default is an empty atom.',
		argnames is ['Help']
	]).

	:- uses(type, [
		check/3
	]).

	% check/0 - type-checks the command-line option definition
	check :-
		context(Context),
		% key/1 must succeed with a ground atom
		(	::name(Name) ->
			check(atom, Name, Context)
		;	existence_error(predicate, name/1)
		),
		% short flags must be single-character atoms
		::short_flags(ShortFlags),
		check(list(character), ShortFlags, Context),
		% long flags must be atoms
		::long_flags(LongFlags),
		check(list(atom), LongFlags, Context),
		% type must be known
		::type(Type),
		check(one_of(atom, [boolean, atom, integer, float, term]), Type, Context),
		% default must match type if provided
		(	::default(Default) ->
			check(Type, Default, Context)
		;	true
		),
		::help(Help),
		check(types([atom, list(atom)]), Help, Context).

	% valid/0 - succeeds if the definition is valid, fails otherwise
	valid :-
		catch(check, _, fail).

	% Default implementations - override these in importing objects

	short_flags([]).

	long_flags([]).

	type(term).

	meta('').

	help('').

:- end_category.
