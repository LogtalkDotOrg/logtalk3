%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- category(options,
	implements(options_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-02-01,
		comment is 'Options processing predicates.'
	]).

	:- uses(list, [
		append/3, member/2
	]).

	check_options(Options) :-
		check_options(Options, Options).

	check_options(Options, _) :-
		var(Options),
		instantiation_error.
	check_options([Option| Options], Options0) :-
		!,
		check_option(Option),
		check_options(Options, Options0).
	check_options([], _) :-
		!.
	check_options(_, Options0) :-
		type_error(list, Options0).

	check_option(Option) :-
		(	var(Option) ->
			instantiation_error
		;	\+ compound(Option) ->
			type_error(compound, Option)
		;	\+ ::valid_option(Option) ->
			domain_error(option, Option)
		;	true
		).

	valid_options(Options) :-
		var(Options),
		!,
		fail.
	valid_options([Option| Options]) :-
		nonvar(Option),
		::valid_option(Option),
		valid_options(Options).
	valid_options([]).

	default_options(DefaultOptions) :-
		findall(DefaultOption, ::default_option(DefaultOption), DefaultOptions).

	merge_options(UserOptions, Options) :-
		findall(
			DefaultOption,
			(	::default_option(DefaultOption),
				functor(DefaultOption, OptionName, Arity),
				functor(UserOption, OptionName, Arity),
				\+ member(UserOption, UserOptions)
			),
			DefaultOptions
		),
		append(UserOptions, DefaultOptions, Options0),
		fix_options(Options0, Options).

	fix_options([], []).
	fix_options([Option| Options], [FixedOption| FixedOptions]) :-
		(	::fix_option(Option, FixedOption) ->
			true
		;	FixedOption = Option
		),
		fix_options(Options, FixedOptions).

:- end_category.
