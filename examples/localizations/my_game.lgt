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


:- object(my_game(_CountryCode_)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-02-07,
		comment is 'A simple example of supporting application localization in multiple languages.',
		parnames is ['CountryCode']
	]).

	% we use an object parameter to pass the country code
	% of the language to be used when printing messages

	:- public(banner/0).

	banner :-
		logtalk::print_message(comment, my_game(_CountryCode_), banner).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).
	logtalk::message_prefix_stream(comment, my_game(_), '>>> ', Stream) :-
		current_output(Stream).

:- end_object.
