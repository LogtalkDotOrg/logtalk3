%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(options_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-02-01,
		comment is 'Options protocol.',
		see_also is [options]
	]).

	:- public(check_option/1).
	:- mode(check_option(@term), one_or_error).
	:- info(check_option/1, [
		comment is 'Succeeds if the option is valid. Throws an error otherwise.',
		argnames is ['Option'],
		exceptions is [
			'``Option`` is a variable' - instantiation_error,
			'``Option`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'``Option`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(check_options/1).
	:- mode(check_options(@term), one_or_error).
	:- info(check_options/1, [
		comment is 'Succeeds if all the options in a list are valid. Throws an error otherwise.',
		argnames is ['Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(valid_option/1).
	:- mode(valid_option(@term), zero_or_one).
	:- info(valid_option/1, [
		comment is 'Succeeds if the option is valid.',
		argnames is ['Option']
	]).

	:- public(valid_options/1).
	:- mode(valid_options(@term), one).
	:- info(valid_options/1, [
		comment is 'Succeeds if all the options in a list are valid.',
		argnames is ['Options']
	]).

	:- public(default_option/1).
	:- mode(default_option(?compound), zero_or_more).
	:- info(default_option/1, [
		comment is 'Enumerates, by backtracking, the default options.',
		argnames is ['Option']
	]).

	:- public(default_options/1).
	:- mode(default_options(-list(compound)), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options.',
		argnames is ['Options']
	]).

	:- protected(merge_options/2).
	:- mode(merge_options(+list(compound), -list(compound)), one).
	:- info(merge_options/2, [
		comment is 'Merges the user options with the default options, returning the final list of options. Calls the ``fix_options/2`` predicate to preprocess the options after merging. Callers must ensure, if required, that the user options are valid.',
		argnames is ['UserOptions', 'Options']
	]).

	:- protected(fix_options/2).
	:- mode(fix_options(+list(compound), -list(compound)), one).
	:- info(fix_options/2, [
		comment is 'Fixes a list of options, returning the list of options.',
		argnames is ['Options', 'FixedOptions']
	]).

	:- protected(fix_option/2).
	:- mode(fix_option(+compound, -compound), zero_or_one).
	:- info(fix_option/2, [
		comment is 'Fixes an option.',
		argnames is ['Option', 'FixedOption']
	]).

:- end_protocol.
