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


:- object(tutor,
	imports(tutor_explanations)).

	:- info([
		version is 0:86:0,
		author is 'Paulo Moura',
		date is 2026-03-28,
		comment is 'This object adds explanations and suggestions to selected compiler and developer tool warning and error messages.',
		remarks is [
			'Usage' - 'Simply load this object at startup using the goal ``logtalk_load(tutor(loader))``.'
		]
	]).

	% intercept all compiler warning and error messages

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, Kind, core, Tokens) :-
		message_hook(Message, Kind, core, Tokens).
	logtalk::message_hook(Message, Kind, lgtunit, Tokens) :-
		message_hook(Message, Kind, lgtunit, Tokens).
	logtalk::message_hook(Message, Kind, packs, Tokens) :-
		message_hook(Message, Kind, packs, Tokens).
	logtalk::message_hook(Message, Kind, dead_code_scanner, Tokens) :-
		message_hook(Message, Kind, dead_code_scanner, Tokens).

	message_hook(Message, Kind, Component, Tokens) :-
		phrase(^^explain(Message), ExplanationTokens),
		% avoid empty line between the compiler message and its explanation
		(	list::append(Tokens0, [nl, nl], Tokens) ->
			list::append([begin(Kind,Ctx)| Tokens0], [nl| ExplanationTokens], ExtendedTokens0)
		;	list::append([begin(Kind,Ctx)| Tokens], ExplanationTokens, ExtendedTokens0)
		),
		% add begin/2 and end/1 tokens to enable message coloring
		% if supported by the backend Prolog compiler
		list::append(ExtendedTokens0, [end(Ctx)], ExtendedTokens),
		logtalk::message_prefix_stream(Kind, Component, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, ExtendedTokens),
		% take into account tools such as VSCode that require copying messages to a file for parsing
		(	logtalk::message_prefix_file(Kind, Component, Prefix, File, Mode, Options) ->
			open(File, Mode, FileStream, Options),
			logtalk::print_message_tokens(FileStream, Prefix, ExtendedTokens),
			close(FileStream)
		;	true
		).

:- end_object.
